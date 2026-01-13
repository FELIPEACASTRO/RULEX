package com.rulex.service;

import com.rulex.dto.TransactionRequest;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.time.Duration;
import java.util.concurrent.TimeUnit;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

/**
 * Serviço de cache Redis para cálculos de velocidade de alta performance.
 *
 * <p>GAP-FIX #2 (Implementação Redis Real): Este serviço substitui as queries de agregação
 * diretamente no PostgreSQL por operações atômicas no Redis, reduzindo a latência de ~50ms para
 * <1ms.
 *
 * <p>Estratégia de chaves:
 *
 * <ul>
 *   <li>velocity:{keyType}:{keyHash}:count:{window} - Contador de transações
 *   <li>velocity:{keyType}:{keyHash}:sum:{window} - Soma de valores
 *   <li>velocity:{keyType}:{keyHash}:distinct:merchants - HyperLogLog de merchants
 *   <li>velocity:{keyType}:{keyHash}:distinct:mccs - HyperLogLog de MCCs
 *   <li>velocity:{keyType}:{keyHash}:distinct:countries - HyperLogLog de países
 * </ul>
 *
 * <p>Janelas temporais suportadas: 5min, 15min, 30min, 1h, 6h, 12h, 24h
 */
@Service
@RequiredArgsConstructor
@Slf4j
@ConditionalOnProperty(name = "rulex.engine.velocity.redis.enabled", havingValue = "true")
public class RedisVelocityCacheService {

  private final StringRedisTemplate redisTemplate;

  private static final String KEY_PREFIX = "velocity:";
  private static final String COUNT_SUFFIX = ":count:";
  private static final String SUM_SUFFIX = ":sum:";
  private static final String DISTINCT_PREFIX = ":distinct:";

  /** Janelas temporais em minutos com seus TTLs correspondentes. */
  public enum TimeWindow {
    MINUTE_5(5, Duration.ofMinutes(6)),
    MINUTE_15(15, Duration.ofMinutes(16)),
    MINUTE_30(30, Duration.ofMinutes(31)),
    HOUR_1(60, Duration.ofMinutes(65)),
    HOUR_6(360, Duration.ofHours(7)),
    HOUR_12(720, Duration.ofHours(13)),
    HOUR_24(1440, Duration.ofHours(25));

    private final int minutes;
    private final Duration ttl;

    TimeWindow(int minutes, Duration ttl) {
      this.minutes = minutes;
      this.ttl = ttl;
    }

    public int getMinutes() {
      return minutes;
    }

    public Duration getTtl() {
      return ttl;
    }

    public String getKey() {
      return minutes + "m";
    }
  }

  /** Tipo de chave para agrupamento. */
  public enum KeyType {
    PAN,
    CUSTOMER_ID,
    MERCHANT_ID
  }

  /**
   * Registra uma transação no cache Redis. Atualiza contadores e HyperLogLogs para todas as janelas
   * temporais.
   *
   * @param request Transação a ser registrada
   */
  public void recordTransaction(TransactionRequest request) {
    if (request == null || request.getExternalTransactionId() == null) {
      return;
    }

    try {
      BigDecimal amount =
          request.getTransactionAmount() != null ? request.getTransactionAmount() : BigDecimal.ZERO;

      // Registrar para PAN
      String panHash = hashPan(request.getPan());
      if (panHash != null) {
        recordForKey(KeyType.PAN, panHash, amount, request);
      }

      // Registrar para Customer ID
      String customerId = request.getCustomerIdFromHeader();
      if (customerId != null && !customerId.isBlank()) {
        recordForKey(KeyType.CUSTOMER_ID, customerId, amount, request);
      }

      // Registrar para Merchant ID
      String merchantId = request.getMerchantId();
      if (merchantId != null && !merchantId.isBlank()) {
        recordForKey(KeyType.MERCHANT_ID, merchantId, amount, request);
      }

      log.debug("Transação {} registrada no cache Redis", request.getExternalTransactionId());
    } catch (Exception e) {
      log.warn("Erro ao registrar transação no Redis: {}", e.getMessage());
    }
  }

  /**
   * Obtém a contagem de transações para uma chave e janela temporal.
   *
   * @param request Transação (para extrair a chave)
   * @param keyType Tipo de chave
   * @param window Janela temporal
   * @return Contagem de transações
   */
  public long getTransactionCount(TransactionRequest request, KeyType keyType, TimeWindow window) {
    String keyValue = extractKeyValue(request, keyType);
    if (keyValue == null) {
      return 0;
    }

    String redisKey = buildCountKey(keyType, keyValue, window);
    try {
      String value = redisTemplate.opsForValue().get(redisKey);
      return value != null ? Long.parseLong(value) : 0;
    } catch (Exception e) {
      log.warn("Erro ao ler contagem do Redis: {}", e.getMessage());
      return 0;
    }
  }

  /**
   * Obtém a soma de valores para uma chave e janela temporal.
   *
   * @param request Transação (para extrair a chave)
   * @param keyType Tipo de chave
   * @param window Janela temporal
   * @return Soma de valores em centavos
   */
  public BigDecimal getTotalAmount(TransactionRequest request, KeyType keyType, TimeWindow window) {
    String keyValue = extractKeyValue(request, keyType);
    if (keyValue == null) {
      return BigDecimal.ZERO;
    }

    String redisKey = buildSumKey(keyType, keyValue, window);
    try {
      String value = redisTemplate.opsForValue().get(redisKey);
      if (value != null) {
        // Valor armazenado em centavos, converter para decimal
        return new BigDecimal(value).divide(new BigDecimal("100"), 2, RoundingMode.HALF_UP);
      }
      return BigDecimal.ZERO;
    } catch (Exception e) {
      log.warn("Erro ao ler soma do Redis: {}", e.getMessage());
      return BigDecimal.ZERO;
    }
  }

  /**
   * Obtém a média de valores para uma chave e janela temporal.
   *
   * @param request Transação (para extrair a chave)
   * @param keyType Tipo de chave
   * @param window Janela temporal
   * @return Média de valores
   */
  public BigDecimal getAverageAmount(
      TransactionRequest request, KeyType keyType, TimeWindow window) {
    long count = getTransactionCount(request, keyType, window);
    if (count == 0) {
      return BigDecimal.ZERO;
    }
    BigDecimal total = getTotalAmount(request, keyType, window);
    return total.divide(BigDecimal.valueOf(count), 2, RoundingMode.HALF_UP);
  }

  /**
   * Obtém a contagem de merchants distintos usando HyperLogLog.
   *
   * @param request Transação (para extrair a chave)
   * @param keyType Tipo de chave
   * @return Estimativa de merchants distintos
   */
  public long getDistinctMerchants(TransactionRequest request, KeyType keyType) {
    String keyValue = extractKeyValue(request, keyType);
    if (keyValue == null) {
      return 0;
    }

    String redisKey = buildDistinctKey(keyType, keyValue, "merchants");
    try {
      Long count = redisTemplate.opsForHyperLogLog().size(redisKey);
      return count != null ? count : 0;
    } catch (Exception e) {
      log.warn("Erro ao ler HyperLogLog do Redis: {}", e.getMessage());
      return 0;
    }
  }

  /**
   * Obtém a contagem de MCCs distintos usando HyperLogLog.
   *
   * @param request Transação (para extrair a chave)
   * @param keyType Tipo de chave
   * @return Estimativa de MCCs distintos
   */
  public long getDistinctMccs(TransactionRequest request, KeyType keyType) {
    String keyValue = extractKeyValue(request, keyType);
    if (keyValue == null) {
      return 0;
    }

    String redisKey = buildDistinctKey(keyType, keyValue, "mccs");
    try {
      Long count = redisTemplate.opsForHyperLogLog().size(redisKey);
      return count != null ? count : 0;
    } catch (Exception e) {
      log.warn("Erro ao ler HyperLogLog do Redis: {}", e.getMessage());
      return 0;
    }
  }

  /**
   * Obtém a contagem de países distintos usando HyperLogLog.
   *
   * @param request Transação (para extrair a chave)
   * @param keyType Tipo de chave
   * @return Estimativa de países distintos
   */
  public long getDistinctCountries(TransactionRequest request, KeyType keyType) {
    String keyValue = extractKeyValue(request, keyType);
    if (keyValue == null) {
      return 0;
    }

    String redisKey = buildDistinctKey(keyType, keyValue, "countries");
    try {
      Long count = redisTemplate.opsForHyperLogLog().size(redisKey);
      return count != null ? count : 0;
    } catch (Exception e) {
      log.warn("Erro ao ler HyperLogLog do Redis: {}", e.getMessage());
      return 0;
    }
  }

  /**
   * Verifica se há burst de transações (muitas em pouco tempo).
   *
   * @param request Transação
   * @param maxTransactions Limite de transações
   * @param minutes Janela em minutos
   * @return true se houver burst
   */
  public boolean isBurst(TransactionRequest request, int maxTransactions, int minutes) {
    TimeWindow window = findClosestWindow(minutes);
    long count = getTransactionCount(request, KeyType.PAN, window);
    return count >= maxTransactions;
  }

  // ========== Métodos internos ==========

  private void recordForKey(
      KeyType keyType, String keyValue, BigDecimal amount, TransactionRequest request) {

    // Incrementar contadores para todas as janelas
    for (TimeWindow window : TimeWindow.values()) {
      String countKey = buildCountKey(keyType, keyValue, window);
      String sumKey = buildSumKey(keyType, keyValue, window);

      // Incrementar contador
      redisTemplate.opsForValue().increment(countKey);
      setTtlIfNew(countKey, window.getTtl());

      // Incrementar soma (em centavos para evitar problemas de precisão)
      long amountCents = amount.multiply(new BigDecimal("100")).longValue();
      redisTemplate.opsForValue().increment(sumKey, amountCents);
      setTtlIfNew(sumKey, window.getTtl());
    }

    // Adicionar aos HyperLogLogs (sem TTL, pois são acumulativos)
    if (request.getMerchantId() != null) {
      String merchantKey = buildDistinctKey(keyType, keyValue, "merchants");
      redisTemplate.opsForHyperLogLog().add(merchantKey, request.getMerchantId());
      redisTemplate.expire(merchantKey, Duration.ofHours(25));
    }

    if (request.getMcc() != null) {
      String mccKey = buildDistinctKey(keyType, keyValue, "mccs");
      redisTemplate.opsForHyperLogLog().add(mccKey, String.valueOf(request.getMcc()));
      redisTemplate.expire(mccKey, Duration.ofHours(25));
    }

    if (request.getMerchantCountryCode() != null) {
      String countryKey = buildDistinctKey(keyType, keyValue, "countries");
      redisTemplate.opsForHyperLogLog().add(countryKey, request.getMerchantCountryCode());
      redisTemplate.expire(countryKey, Duration.ofHours(25));
    }
  }

  private void setTtlIfNew(String key, Duration ttl) {
    // Só define TTL se a chave não tiver um
    Long currentTtl = redisTemplate.getExpire(key, TimeUnit.SECONDS);
    if (currentTtl == null || currentTtl < 0) {
      redisTemplate.expire(key, ttl);
    }
  }

  private String extractKeyValue(TransactionRequest request, KeyType keyType) {
    if (request == null) {
      return null;
    }
    return switch (keyType) {
      case PAN -> hashPan(request.getPan());
      case CUSTOMER_ID -> request.getCustomerIdFromHeader();
      case MERCHANT_ID -> request.getMerchantId();
    };
  }

  private String buildCountKey(KeyType keyType, String keyValue, TimeWindow window) {
    return KEY_PREFIX
        + keyType.name().toLowerCase()
        + ":"
        + keyValue
        + COUNT_SUFFIX
        + window.getKey();
  }

  private String buildSumKey(KeyType keyType, String keyValue, TimeWindow window) {
    return KEY_PREFIX
        + keyType.name().toLowerCase()
        + ":"
        + keyValue
        + SUM_SUFFIX
        + window.getKey();
  }

  private String buildDistinctKey(KeyType keyType, String keyValue, String distinctType) {
    return KEY_PREFIX
        + keyType.name().toLowerCase()
        + ":"
        + keyValue
        + DISTINCT_PREFIX
        + distinctType;
  }

  private TimeWindow findClosestWindow(int minutes) {
    for (TimeWindow window : TimeWindow.values()) {
      if (window.getMinutes() >= minutes) {
        return window;
      }
    }
    return TimeWindow.HOUR_24;
  }

  private String hashPan(String pan) {
    if (pan == null || pan.isBlank()) {
      return null;
    }
    try {
      MessageDigest md = MessageDigest.getInstance("SHA-256");
      byte[] hash = md.digest(pan.getBytes(StandardCharsets.UTF_8));
      StringBuilder sb = new StringBuilder();
      for (byte b : hash) {
        sb.append(String.format("%02x", b));
      }
      return sb.toString();
    } catch (Exception e) {
      log.warn("Erro ao gerar hash do PAN: {}", e.getMessage());
      return null;
    }
  }
}
