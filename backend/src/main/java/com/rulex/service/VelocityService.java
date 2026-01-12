package com.rulex.service;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.rulex.dto.TransactionRequest;
import com.rulex.entity.VelocityTransactionLog;
import com.rulex.repository.VelocityCounterRepository;
import com.rulex.repository.VelocityTransactionLogRepository;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import lombok.Builder;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * Serviço de velocidade para detecção de fraude.
 *
 * <p>Calcula agregações temporais (COUNT, SUM, AVG) sem alterar o payload de entrada. As agregações
 * são baseadas em: - PAN (hash para privacidade) - customerId - merchantId
 *
 * <p>Janelas temporais suportadas: 5min, 15min, 30min, 1h, 6h, 12h, 24h, 7d, 30d
 */
@Service
@Slf4j
public class VelocityService {

  private final VelocityCounterRepository counterRepository;
  private final VelocityTransactionLogRepository logRepository;

  // Cache Caffeine com TTL de 30 segundos e máximo de 10.000 entradas
  // Resolve o problema de memory leak do ConcurrentHashMap sem eviction
  private final Cache<String, VelocityStats> statsCache;

  public VelocityService(
      VelocityCounterRepository counterRepository,
      VelocityTransactionLogRepository logRepository) {
    this.counterRepository = counterRepository;
    this.logRepository = logRepository;
    this.statsCache = Caffeine.newBuilder()
        .expireAfterWrite(30, TimeUnit.SECONDS)
        .maximumSize(10_000)
        .recordStats()
        .build();
  }

  /** Estatísticas de velocidade. */
  @Data
  @Builder
  public static class VelocityStats {
    private final long transactionCount;
    private final BigDecimal totalAmount;
    private final BigDecimal avgAmount;
    private final BigDecimal minAmount;
    private final BigDecimal maxAmount;
    private final long distinctMerchants;
    private final long distinctMccs;
    private final long distinctCountries;
    private final long fraudCount;
    private final boolean found;
    private final String source; // CACHE, DB, COMPUTED

    public static VelocityStats empty() {
      return VelocityStats.builder()
          .transactionCount(0)
          .totalAmount(BigDecimal.ZERO)
          .avgAmount(BigDecimal.ZERO)
          .distinctMerchants(0)
          .distinctMccs(0)
          .distinctCountries(0)
          .fraudCount(0)
          .found(false)
          .source("EMPTY")
          .build();
    }
  }

  /** Tipo de agregação. */
  public enum AggregationType {
    COUNT,
    SUM,
    AVG,
    MIN,
    MAX,
    DISTINCT_MERCHANTS,
    DISTINCT_MCCS,
    DISTINCT_COUNTRIES,
    FRAUD_COUNT
  }

  /** Tipo de chave. */
  public enum KeyType {
    PAN,
    CUSTOMER_ID,
    MERCHANT_ID
  }

  /** Janela temporal em minutos. */
  public enum TimeWindow {
    MINUTE_5(5),
    MINUTE_15(15),
    MINUTE_30(30),
    HOUR_1(60),
    HOUR_6(360),
    HOUR_12(720),
    HOUR_24(1440),
    DAY_7(10080),
    DAY_30(43200);

    private final int minutes;

    TimeWindow(int minutes) {
      this.minutes = minutes;
    }

    public int getMinutes() {
      return minutes;
    }
  }

  /**
   * Obtém estatísticas de velocidade para uma transação.
   *
   * @param request Transação
   * @param keyType Tipo de chave (PAN, CUSTOMER_ID, MERCHANT_ID)
   * @param window Janela temporal
   * @return Estatísticas de velocidade
   */
  public VelocityStats getStats(TransactionRequest request, KeyType keyType, TimeWindow window) {
    if (request == null) {
      return VelocityStats.empty();
    }

    String keyValue = getKeyValue(request, keyType);
    if (keyValue == null || keyValue.isBlank()) {
      log.debug("Chave {} não disponível na transação", keyType);
      return VelocityStats.empty();
    }

    // Verificar cache
    String cacheKey = String.format("%s|%s|%d", keyType, keyValue, window.getMinutes());
    VelocityStats cached = statsCache.getIfPresent(cacheKey);
    if (cached != null) {
      return cached;
    }

    // Calcular estatísticas
    VelocityStats stats = computeStats(keyType, keyValue, window);

    // Cachear resultado
    statsCache.put(cacheKey, stats);

    return stats;
  }

  /** Obtém um valor específico de agregação. */
  public BigDecimal getAggregation(
      TransactionRequest request, KeyType keyType, TimeWindow window, AggregationType aggregation) {

    VelocityStats stats = getStats(request, keyType, window);

    return switch (aggregation) {
      case COUNT -> BigDecimal.valueOf(stats.getTransactionCount());
      case SUM -> stats.getTotalAmount();
      case AVG -> stats.getAvgAmount();
      case MIN -> stats.getMinAmount() != null ? stats.getMinAmount() : BigDecimal.ZERO;
      case MAX -> stats.getMaxAmount() != null ? stats.getMaxAmount() : BigDecimal.ZERO;
      case DISTINCT_MERCHANTS -> BigDecimal.valueOf(stats.getDistinctMerchants());
      case DISTINCT_MCCS -> BigDecimal.valueOf(stats.getDistinctMccs());
      case DISTINCT_COUNTRIES -> BigDecimal.valueOf(stats.getDistinctCountries());
      case FRAUD_COUNT -> BigDecimal.valueOf(stats.getFraudCount());
    };
  }

  /**
   * Registra uma transação no log de velocidade. Deve ser chamado após o processamento da
   * transação.
   *
   * <p>Usa REPEATABLE_READ para evitar phantom reads durante verificação de duplicatas e inserção.
   */
  @Transactional(isolation = org.springframework.transaction.annotation.Isolation.REPEATABLE_READ)
  public void logTransaction(TransactionRequest request, String decision, Integer riskScore) {
    if (request == null || request.getExternalTransactionId() == null) {
      return;
    }

    // Verificar se já existe
    if (logRepository.findByExternalTransactionId(request.getExternalTransactionId()).isPresent()) {
      log.debug(
          "Transação {} já registrada no log de velocidade", request.getExternalTransactionId());
      return;
    }

    try {
      VelocityTransactionLog logEntry =
          VelocityTransactionLog.builder()
              .externalTransactionId(request.getExternalTransactionId())
              .panHash(hashPan(request.getPan()))
              .customerId(request.getCustomerIdFromHeader())
              .merchantId(request.getMerchantId())
              .amount(request.getTransactionAmount())
              .currencyCode(request.getTransactionCurrencyCode())
              .mcc(request.getMcc())
              .merchantCountry(request.getMerchantCountryCode())
              .decision(decision)
              .riskScore(riskScore)
              .transactionAt(OffsetDateTime.now(ZoneOffset.UTC))
              .build();

      logRepository.save(logEntry);

      // Limpar cache para esta transação
      clearCacheForTransaction(request);

      log.debug("Transação {} registrada no log de velocidade", request.getExternalTransactionId());
    } catch (Exception e) {
      log.warn("Erro ao registrar transação no log de velocidade: {}", e.getMessage());
    }
  }

  /** Calcula estatísticas a partir do log de transações. */
  private VelocityStats computeStats(KeyType keyType, String keyValue, TimeWindow window) {
    OffsetDateTime since = OffsetDateTime.now(ZoneOffset.UTC).minusMinutes(window.getMinutes());

    try {
      long count;
      BigDecimal sum;
      BigDecimal avg;
      long distinctMerchants = 0;
      long distinctMccs = 0;
      long distinctCountries = 0;
      long fraudCount = 0;

      switch (keyType) {
        case PAN -> {
          count = logRepository.countByPanHashSince(keyValue, since);
          sum = logRepository.sumAmountByPanHashSince(keyValue, since);
          avg = logRepository.avgAmountByPanHashSince(keyValue, since);
          distinctMerchants = logRepository.countDistinctMerchantsByPanHashSince(keyValue, since);
          distinctMccs = logRepository.countDistinctMccsByPanHashSince(keyValue, since);
          distinctCountries = logRepository.countDistinctCountriesByPanHashSince(keyValue, since);
          fraudCount = logRepository.countFraudByPanHashSince(keyValue, since);
        }
        case CUSTOMER_ID -> {
          count = logRepository.countByCustomerIdSince(keyValue, since);
          sum = logRepository.sumAmountByCustomerIdSince(keyValue, since);
          avg =
              count > 0
                  ? sum.divide(BigDecimal.valueOf(count), 2, RoundingMode.HALF_UP)
                  : BigDecimal.ZERO;
        }
        case MERCHANT_ID -> {
          // Para merchant, usar contagem simples por enquanto
          count = 0;
          sum = BigDecimal.ZERO;
          avg = BigDecimal.ZERO;
        }
        default -> {
          return VelocityStats.empty();
        }
      }

      return VelocityStats.builder()
          .transactionCount(count)
          .totalAmount(sum != null ? sum : BigDecimal.ZERO)
          .avgAmount(avg != null ? avg : BigDecimal.ZERO)
          .distinctMerchants(distinctMerchants)
          .distinctMccs(distinctMccs)
          .distinctCountries(distinctCountries)
          .fraudCount(fraudCount)
          .found(count > 0)
          .source("DB")
          .build();

    } catch (Exception e) {
      log.warn("Erro ao calcular estatísticas de velocidade: {}", e.getMessage());
      return VelocityStats.empty();
    }
  }

  /** Obtém o valor da chave a partir da transação. */
  private String getKeyValue(TransactionRequest request, KeyType keyType) {
    return switch (keyType) {
      case PAN -> hashPan(request.getPan());
      case CUSTOMER_ID -> request.getCustomerIdFromHeader();
      case MERCHANT_ID -> request.getMerchantId();
    };
  }

  /** Gera hash SHA-256 do PAN para privacidade. */
  private String hashPan(String pan) {
    if (pan == null || pan.isBlank()) {
      return null;
    }

    try {
      MessageDigest digest = MessageDigest.getInstance("SHA-256");
      byte[] hash = digest.digest(pan.getBytes(StandardCharsets.UTF_8));
      StringBuilder hexString = new StringBuilder();
      for (byte b : hash) {
        String hex = Integer.toHexString(0xff & b);
        if (hex.length() == 1) hexString.append('0');
        hexString.append(hex);
      }
      return hexString.toString();
    } catch (Exception e) {
      log.error("Erro ao gerar hash do PAN: {}", e.getMessage());
      return null;
    }
  }

  /** Limpa o cache para uma transação específica. */
  private void clearCacheForTransaction(TransactionRequest request) {
    String panHash = hashPan(request.getPan());
    String customerId = request.getCustomerIdFromHeader();
    String merchantId = request.getMerchantId();

    statsCache
        .asMap()
        .entrySet()
        .removeIf(
            entry -> {
              String key = entry.getKey();
              return (panHash != null && key.contains(panHash))
                  || (customerId != null && key.contains(customerId))
                  || (merchantId != null && key.contains(merchantId));
            });
  }

  /** Limpa todo o cache. */
  public void clearCache() {
    statsCache.invalidateAll();
    log.info("Cache de velocidade limpo");
  }

  /** Retorna estatisticas do cache. */
  public Map<String, Long> getCacheStats() {
    return Map.of("statsCache", statsCache.estimatedSize());
  }

  // ========== Métodos de conveniência para regras ==========

  /** Conta transações do PAN nas últimas N horas. */
  public long countPanTransactionsInHours(TransactionRequest request, int hours) {
    TimeWindow window =
        switch (hours) {
          case 1 -> TimeWindow.HOUR_1;
          case 6 -> TimeWindow.HOUR_6;
          case 12 -> TimeWindow.HOUR_12;
          case 24 -> TimeWindow.HOUR_24;
          default -> TimeWindow.HOUR_24;
        };
    return getStats(request, KeyType.PAN, window).getTransactionCount();
  }

  /** Soma valores do PAN nas últimas N horas. */
  public BigDecimal sumPanAmountInHours(TransactionRequest request, int hours) {
    TimeWindow window =
        switch (hours) {
          case 1 -> TimeWindow.HOUR_1;
          case 6 -> TimeWindow.HOUR_6;
          case 12 -> TimeWindow.HOUR_12;
          case 24 -> TimeWindow.HOUR_24;
          default -> TimeWindow.HOUR_24;
        };
    return getStats(request, KeyType.PAN, window).getTotalAmount();
  }

  /** Conta merchants distintos do PAN nas últimas 24h. */
  public long countDistinctMerchantsIn24h(TransactionRequest request) {
    return getStats(request, KeyType.PAN, TimeWindow.HOUR_24).getDistinctMerchants();
  }

  /** Verifica se há burst de transações (muitas em pouco tempo). */
  public boolean isBurst(TransactionRequest request, int maxTransactions, int minutes) {
    TimeWindow window =
        switch (minutes) {
          case 5 -> TimeWindow.MINUTE_5;
          case 15 -> TimeWindow.MINUTE_15;
          case 30 -> TimeWindow.MINUTE_30;
          default -> TimeWindow.MINUTE_5;
        };
    long count = getStats(request, KeyType.PAN, window).getTransactionCount();
    return count >= maxTransactions;
  }
}
