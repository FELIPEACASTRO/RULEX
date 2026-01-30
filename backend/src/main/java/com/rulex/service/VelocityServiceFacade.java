package com.rulex.service;

import com.rulex.dto.TransactionRequest;
import jakarta.annotation.PostConstruct;
import java.math.BigDecimal;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.stereotype.Service;

/**
 * Facade para serviços de velocidade que escolhe automaticamente entre cache Redis real, cache em
 * memória ou banco de dados.
 *
 * <p>GAP-FIX #2: Resolve o gargalo de performance do VelocityService que executava queries de
 * agregação pesadas (COUNT, SUM, AVG, COUNT DISTINCT) diretamente no PostgreSQL para cada
 * transação.
 *
 * <p>Hierarquia de fallback:
 *
 * <ol>
 *   <li>RedisVelocityCacheService (Redis real) - quando disponível
 *   <li>RedisVelocityService (cache em memória) - fallback
 *   <li>VelocityService (banco de dados) - último recurso
 * </ol>
 */
@Service
@Slf4j
public class VelocityServiceFacade {

  private final VelocityService velocityService;
  private final RedisVelocityService redisVelocityService;
  private final RedisVelocityCacheService redisVelocityCacheService;

  @Value("${rulex.engine.velocity.redis.enabled:false}")
  private boolean redisEnabled;

  public VelocityServiceFacade(
      VelocityService velocityService,
      RedisVelocityService redisVelocityService,
      ObjectProvider<RedisVelocityCacheService> redisVelocityCacheServiceProvider) {
    this.velocityService = velocityService;
    this.redisVelocityService = redisVelocityService;
    this.redisVelocityCacheService =
        redisVelocityCacheServiceProvider != null
            ? redisVelocityCacheServiceProvider.getIfAvailable()
            : null;
  }

  @PostConstruct
  public void init() {
    log.info("============================================================");
    log.info("VelocityServiceFacade INICIALIZADO");
    log.info("Redis habilitado: {}", redisEnabled);
    log.info("RedisVelocityCacheService disponível: {}", redisVelocityCacheService != null);
    log.info("Status atual: {}", getCacheStatus());
    log.info("============================================================");
  }

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
      // Tentar usar Redis real primeiro
      if (redisVelocityCacheService != null) {
        try {
          return getStatsFromRedisCacheService(request, keyType, window);
        } catch (Exception e) {
          log.warn("Erro ao usar RedisVelocityCacheService: {}", e.getMessage());
          // Fallback para cache em memória
        }
      }

      // Fallback para cache em memória
      try {
        RedisVelocityService.KeyType redisKeyType = convertKeyType(keyType);
        RedisVelocityService.TimeWindow redisWindow = convertTimeWindow(window);

        if (redisWindow == null) {
          return velocityService.getStats(request, keyType, window);
        }

        RedisVelocityService.VelocityStats redisStats =
            redisVelocityService.getStats(request, redisKeyType, redisWindow);

        return VelocityService.VelocityStats.builder()
            .transactionCount(redisStats.getTransactionCount())
            .totalAmount(redisStats.getTotalAmount())
            .avgAmount(redisStats.getAvgAmount())
            .distinctMerchants(redisStats.getDistinctMerchants())
            .distinctMccs(redisStats.getDistinctMccs())
            .distinctCountries(redisStats.getDistinctCountries())
            .found(redisStats.getTransactionCount() > 0)
            .source("MEMORY_CACHE")
            .build();
      } catch (Exception e) {
        log.warn(
            "Erro ao usar RedisVelocityService, fazendo fallback para banco: {}", e.getMessage());
      }
    }

    return velocityService.getStats(request, keyType, window);
  }

  private VelocityService.VelocityStats getStatsFromRedisCacheService(
      TransactionRequest request,
      VelocityService.KeyType keyType,
      VelocityService.TimeWindow window) {

    RedisVelocityCacheService.KeyType cacheKeyType = convertToCacheKeyType(keyType);
    RedisVelocityCacheService.TimeWindow cacheWindow = convertToCacheTimeWindow(window);

    long count = redisVelocityCacheService.getTransactionCount(request, cacheKeyType, cacheWindow);
    BigDecimal total = redisVelocityCacheService.getTotalAmount(request, cacheKeyType, cacheWindow);
    BigDecimal avg = redisVelocityCacheService.getAverageAmount(request, cacheKeyType, cacheWindow);
    long distinctMerchants =
        redisVelocityCacheService.getDistinctMerchants(request, cacheKeyType, cacheWindow);
    long distinctMccs =
        redisVelocityCacheService.getDistinctMccs(request, cacheKeyType, cacheWindow);
    long distinctCountries =
        redisVelocityCacheService.getDistinctCountries(request, cacheKeyType, cacheWindow);

    return VelocityService.VelocityStats.builder()
        .transactionCount(count)
        .totalAmount(total)
        .avgAmount(avg)
        .distinctMerchants(distinctMerchants)
        .distinctMccs(distinctMccs)
        .distinctCountries(distinctCountries)
        .found(count > 0)
        .source("REDIS_REAL")
        .build();
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
      // Registrar no Redis real se disponível
      if (redisVelocityCacheService != null) {
        try {
          redisVelocityCacheService.recordTransaction(request);
        } catch (Exception e) {
          log.warn("Erro ao registrar no RedisVelocityCacheService: {}", e.getMessage());
        }
      }

      // Também registrar no cache em memória
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
      if (redisVelocityCacheService != null) {
        return "REDIS_REAL_ENABLED";
      }
      return "MEMORY_CACHE_ENABLED";
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
      case IP_ADDRESS, DEVICE_ID, CARD_ID, BENEFICIARY_ID ->
          RedisVelocityService.KeyType.PAN; // Fallback to PAN for new types
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
      case DAY_30 -> null; // Cache em memória não suporta 30 dias
    };
  }

  // ========== Conversores para RedisVelocityCacheService ==========

  private RedisVelocityCacheService.KeyType convertToCacheKeyType(VelocityService.KeyType keyType) {
    return switch (keyType) {
      case PAN -> RedisVelocityCacheService.KeyType.PAN;
      case CUSTOMER_ID -> RedisVelocityCacheService.KeyType.CUSTOMER_ID;
      case MERCHANT_ID -> RedisVelocityCacheService.KeyType.MERCHANT_ID;
      case IP_ADDRESS, DEVICE_ID, CARD_ID, BENEFICIARY_ID ->
          RedisVelocityCacheService.KeyType.PAN; // Fallback to PAN for new types
    };
  }

  private RedisVelocityCacheService.TimeWindow convertToCacheTimeWindow(
      VelocityService.TimeWindow window) {
    return switch (window) {
      case MINUTE_5 -> RedisVelocityCacheService.TimeWindow.MINUTE_5;
      case MINUTE_15 -> RedisVelocityCacheService.TimeWindow.MINUTE_15;
      case MINUTE_30 -> RedisVelocityCacheService.TimeWindow.MINUTE_30;
      case HOUR_1 -> RedisVelocityCacheService.TimeWindow.HOUR_1;
      case HOUR_6 -> RedisVelocityCacheService.TimeWindow.HOUR_6;
      case HOUR_12 -> RedisVelocityCacheService.TimeWindow.HOUR_12;
      case HOUR_24 -> RedisVelocityCacheService.TimeWindow.HOUR_24;
      case DAY_7 -> RedisVelocityCacheService.TimeWindow.DAY_7;
      case DAY_30 -> RedisVelocityCacheService.TimeWindow.DAY_30;
    };
  }
}
