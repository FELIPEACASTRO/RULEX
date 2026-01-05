package com.rulex.service;

import com.rulex.dto.TransactionRequest;
import java.math.BigDecimal;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

/**
 * Facade para serviços de velocidade que escolhe automaticamente entre cache em memória (Redis) e
 * banco de dados.
 *
 * <p>GAP-FIX #2: Resolve o gargalo de performance do VelocityService que executava queries de
 * agregação pesadas (COUNT, SUM, AVG, COUNT DISTINCT) diretamente no PostgreSQL para cada
 * transação.
 *
 * <p>Quando habilitado, usa o RedisVelocityService que implementa:
 *
 * <ul>
 *   <li>Sliding windows com buckets de 1 minuto
 *   <li>HyperLogLog para contagem de valores distintos
 *   <li>Cache em memória com latência sub-milissegundo
 * </ul>
 *
 * <p>Quando desabilitado, faz fallback para o VelocityService original (queries no banco).
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class VelocityServiceFacade {

  private final VelocityService velocityService;
  private final RedisVelocityService redisVelocityService;

  @Value("${rulex.engine.velocity.redis.enabled:false}")
  private boolean redisEnabled;

  /**
   * Obtém estatísticas de velocidade usando o serviço apropriado.
   *
   * @param request Transação
   * @param keyType Tipo de chave (PAN, CUSTOMER_ID, MERCHANT_ID)
   * @param window Janela temporal
   * @return Estatísticas de velocidade
   */
  public VelocityService.VelocityStats getStats(
      TransactionRequest request,
      VelocityService.KeyType keyType,
      VelocityService.TimeWindow window) {

    if (redisEnabled) {
      try {
        // Converter tipos entre os dois serviços
        RedisVelocityService.KeyType redisKeyType = convertKeyType(keyType);
        RedisVelocityService.TimeWindow redisWindow = convertTimeWindow(window);

        RedisVelocityService.VelocityStats redisStats =
            redisVelocityService.getStats(request, redisKeyType, redisWindow);

        // Converter resultado de volta para o formato do VelocityService
        return VelocityService.VelocityStats.builder()
            .transactionCount(redisStats.getTransactionCount())
            .totalAmount(redisStats.getTotalAmount())
            .avgAmount(redisStats.getAvgAmount())
            .distinctMerchants(redisStats.getDistinctMerchants())
            .distinctMccs(redisStats.getDistinctMccs())
            .distinctCountries(redisStats.getDistinctCountries())
            .found(redisStats.getTransactionCount() > 0)
            .source("REDIS_CACHE")
            .build();
      } catch (Exception e) {
        log.warn(
            "Erro ao usar RedisVelocityService, fazendo fallback para banco: {}", e.getMessage());
        // Fallback para banco em caso de erro
        return velocityService.getStats(request, keyType, window);
      }
    }

    return velocityService.getStats(request, keyType, window);
  }

  /** Obtém um valor específico de agregação. */
  public BigDecimal getAggregation(
      TransactionRequest request,
      VelocityService.KeyType keyType,
      VelocityService.TimeWindow window,
      VelocityService.AggregationType aggregation) {

    VelocityService.VelocityStats stats = getStats(request, keyType, window);

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

  /** Registra uma transação para tracking de velocidade. */
  public void recordTransaction(TransactionRequest request, String decision, Integer riskScore) {
    if (redisEnabled) {
      try {
        redisVelocityService.recordTransaction(request);
      } catch (Exception e) {
        log.warn("Erro ao registrar no RedisVelocityService: {}", e.getMessage());
      }
    }
    // Sempre registra no banco para persistência
    velocityService.logTransaction(request, decision, riskScore);
  }

  /** Verifica se o cache Redis está habilitado. */
  public boolean isRedisEnabled() {
    return redisEnabled;
  }

  /** Retorna estatísticas do cache. */
  public String getCacheStatus() {
    if (redisEnabled) {
      return "REDIS_CACHE_ENABLED";
    }
    return "DATABASE_ONLY";
  }

  // ========== Métodos de conveniência ==========

  /** Conta transações do PAN nas últimas N horas. */
  public long countPanTransactionsInHours(TransactionRequest request, int hours) {
    VelocityService.TimeWindow window =
        switch (hours) {
          case 1 -> VelocityService.TimeWindow.HOUR_1;
          case 6 -> VelocityService.TimeWindow.HOUR_6;
          case 12 -> VelocityService.TimeWindow.HOUR_12;
          case 24 -> VelocityService.TimeWindow.HOUR_24;
          default -> VelocityService.TimeWindow.HOUR_24;
        };
    return getStats(request, VelocityService.KeyType.PAN, window).getTransactionCount();
  }

  /** Soma valores do PAN nas últimas N horas. */
  public BigDecimal sumPanAmountInHours(TransactionRequest request, int hours) {
    VelocityService.TimeWindow window =
        switch (hours) {
          case 1 -> VelocityService.TimeWindow.HOUR_1;
          case 6 -> VelocityService.TimeWindow.HOUR_6;
          case 12 -> VelocityService.TimeWindow.HOUR_12;
          case 24 -> VelocityService.TimeWindow.HOUR_24;
          default -> VelocityService.TimeWindow.HOUR_24;
        };
    return getStats(request, VelocityService.KeyType.PAN, window).getTotalAmount();
  }

  /** Conta merchants distintos do PAN nas últimas 24h. */
  public long countDistinctMerchantsIn24h(TransactionRequest request) {
    return getStats(request, VelocityService.KeyType.PAN, VelocityService.TimeWindow.HOUR_24)
        .getDistinctMerchants();
  }

  /** Verifica se há burst de transações. */
  public boolean isBurst(TransactionRequest request, int maxTransactions, int minutes) {
    VelocityService.TimeWindow window =
        switch (minutes) {
          case 5 -> VelocityService.TimeWindow.MINUTE_5;
          case 15 -> VelocityService.TimeWindow.MINUTE_15;
          case 30 -> VelocityService.TimeWindow.MINUTE_30;
          default -> VelocityService.TimeWindow.MINUTE_5;
        };
    long count = getStats(request, VelocityService.KeyType.PAN, window).getTransactionCount();
    return count >= maxTransactions;
  }

  // ========== Conversores de tipos ==========

  private RedisVelocityService.KeyType convertKeyType(VelocityService.KeyType keyType) {
    return switch (keyType) {
      case PAN -> RedisVelocityService.KeyType.PAN;
      case CUSTOMER_ID -> RedisVelocityService.KeyType.CUSTOMER_ID;
      case MERCHANT_ID -> RedisVelocityService.KeyType.MERCHANT_ID;
    };
  }

  private RedisVelocityService.TimeWindow convertTimeWindow(VelocityService.TimeWindow window) {
    return switch (window) {
      case MINUTE_5 -> RedisVelocityService.TimeWindow.MINUTE_5;
      case MINUTE_15 -> RedisVelocityService.TimeWindow.MINUTE_15;
      case MINUTE_30 -> RedisVelocityService.TimeWindow.MINUTE_30;
      case HOUR_1 -> RedisVelocityService.TimeWindow.HOUR_1;
      case HOUR_6 -> RedisVelocityService.TimeWindow.HOUR_6;
      case HOUR_12 -> RedisVelocityService.TimeWindow.HOUR_12;
      case HOUR_24 -> RedisVelocityService.TimeWindow.HOUR_24;
      case DAY_7 -> RedisVelocityService.TimeWindow.DAY_7;
      case DAY_30 -> RedisVelocityService.TimeWindow.DAY_7; // Fallback para 7 dias
    };
  }
}
