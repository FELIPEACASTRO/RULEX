package com.rulex.service.enrichment;

import com.rulex.dto.TransactionRequest;
import com.rulex.service.VelocityService;
import com.rulex.service.VelocityService.KeyType;
import com.rulex.service.VelocityService.TimeWindow;
import com.rulex.service.VelocityService.VelocityStats;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.HashMap;
import java.util.Map;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Enriquecimento de velocidade para regras de fraude.
 *
 * <p>Consolida todos os campos de velocidade em um formato fácil de usar pelo motor de regras.
 * Campos disponíveis:
 *
 * <ul>
 *   <li>velocity.transactions_5min - Transações nos últimos 5 minutos
 *   <li>velocity.transactions_1h - Transações na última hora
 *   <li>velocity.transactions_24h - Transações nas últimas 24 horas
 *   <li>velocity.amount_1h - Soma de valores na última hora
 *   <li>velocity.amount_24h - Soma de valores nas últimas 24 horas
 *   <li>velocity.avg_amount_24h - Média de valores nas últimas 24 horas
 *   <li>velocity.avg_amount_ratio - Razão entre valor atual e média 24h
 *   <li>velocity.distinct_merchants_1h - Merchants distintos na última hora
 *   <li>velocity.distinct_merchants_24h - Merchants distintos nas últimas 24h
 *   <li>velocity.max_amount_24h - Maior valor nas últimas 24h
 *   <li>velocity.min_amount_24h - Menor valor nas últimas 24h
 *   <li>velocity.fraud_count_24h - Transações fraudulentas nas últimas 24h
 *   <li>velocity.is_burst - Flag de burst (muitas transações em pouco tempo)
 *   <li>velocity.is_anomaly - Flag de anomalia de velocidade
 *   <li>velocity.score - Score de velocidade (0-100)
 * </ul>
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class VelocityEnrichment {

  private final VelocityService velocityService;

  // Thresholds configuráveis
  private static final int BURST_THRESHOLD_5MIN = 5;
  private static final int BURST_THRESHOLD_1H = 20;
  private static final BigDecimal AVG_RATIO_ANOMALY_THRESHOLD = new BigDecimal("3.0");
  private static final int DISTINCT_MERCHANTS_ANOMALY_THRESHOLD = 5;

  /** Resultado do enriquecimento de velocidade. */
  @Data
  @Builder
  public static class VelocityContext {
    // Contagens por janela temporal
    private final long transactions5min;
    private final long transactions15min;
    private final long transactions1h;
    private final long transactions6h;
    private final long transactions24h;
    private final long transactions7d;
    private final long transactions30d;

    // Somas por janela temporal
    private final BigDecimal amount1h;
    private final BigDecimal amount24h;
    private final BigDecimal amount7d;
    private final BigDecimal amount30d;

    // Médias
    private final BigDecimal avgAmount24h;
    private final BigDecimal avgAmount7d;
    private final BigDecimal avgAmount30d;

    // Min/Max
    private final BigDecimal minAmount24h;
    private final BigDecimal maxAmount24h;

    // Distintos
    private final long distinctMerchants1h;
    private final long distinctMerchants24h;
    private final long distinctMccs24h;
    private final long distinctCountries24h;

    // Fraude
    private final long fraudCount24h;
    private final long fraudCount7d;

    // Razões calculadas
    private final BigDecimal avgAmountRatio;
    private final BigDecimal maxAmountRatio;

    // Flags
    private final boolean isBurst5min;
    private final boolean isBurst1h;
    private final boolean isAnomaly;

    // Score
    private final int velocityScore;

    /** Converte para Map para uso no evaluator. */
    public Map<String, Object> toMap() {
      Map<String, Object> map = new HashMap<>();

      // Contagens
      map.put("velocity.transactions_5min", transactions5min);
      map.put("velocity.transactions_15min", transactions15min);
      map.put("velocity.transactions_1h", transactions1h);
      map.put("velocity.transactions_6h", transactions6h);
      map.put("velocity.transactions_24h", transactions24h);
      map.put("velocity.transactions_7d", transactions7d);
      map.put("velocity.transactions_30d", transactions30d);

      // Aliases para compatibilidade
      map.put("transactionsLast5min", transactions5min);
      map.put("transactionsLast1h", transactions1h);
      map.put("transactionsLast24h", transactions24h);

      // Somas
      map.put("velocity.amount_1h", amount1h);
      map.put("velocity.amount_24h", amount24h);
      map.put("velocity.amount_7d", amount7d);
      map.put("velocity.amount_30d", amount30d);

      // Aliases
      map.put("amountLast1h", amount1h);
      map.put("amountLast24h", amount24h);

      // Médias
      map.put("velocity.avg_amount_24h", avgAmount24h);
      map.put("velocity.avg_amount_7d", avgAmount7d);
      map.put("velocity.avg_amount_30d", avgAmount30d);

      // Aliases
      map.put("avgAmountLast24h", avgAmount24h);

      // Min/Max
      map.put("velocity.min_amount_24h", minAmount24h);
      map.put("velocity.max_amount_24h", maxAmount24h);

      // Distintos
      map.put("velocity.distinct_merchants_1h", distinctMerchants1h);
      map.put("velocity.distinct_merchants_24h", distinctMerchants24h);
      map.put("velocity.distinct_mccs_24h", distinctMccs24h);
      map.put("velocity.distinct_countries_24h", distinctCountries24h);

      // Aliases
      map.put("distinctMerchantsLast1h", distinctMerchants1h);
      map.put("distinctMerchantsLast24h", distinctMerchants24h);

      // Fraude
      map.put("velocity.fraud_count_24h", fraudCount24h);
      map.put("velocity.fraud_count_7d", fraudCount7d);

      // Razões
      map.put("velocity.avg_amount_ratio", avgAmountRatio);
      map.put("velocity.max_amount_ratio", maxAmountRatio);

      // Aliases
      map.put("avgAmountRatio24h", avgAmountRatio);

      // Flags
      map.put("velocity.is_burst_5min", isBurst5min);
      map.put("velocity.is_burst_1h", isBurst1h);
      map.put("velocity.is_anomaly", isAnomaly);

      // Score
      map.put("velocity.score", velocityScore);
      map.put("velocityScore", velocityScore);

      return map;
    }

    public static VelocityContext empty() {
      return VelocityContext.builder()
          .transactions5min(0)
          .transactions15min(0)
          .transactions1h(0)
          .transactions6h(0)
          .transactions24h(0)
          .transactions7d(0)
          .transactions30d(0)
          .amount1h(BigDecimal.ZERO)
          .amount24h(BigDecimal.ZERO)
          .amount7d(BigDecimal.ZERO)
          .amount30d(BigDecimal.ZERO)
          .avgAmount24h(BigDecimal.ZERO)
          .avgAmount7d(BigDecimal.ZERO)
          .avgAmount30d(BigDecimal.ZERO)
          .minAmount24h(BigDecimal.ZERO)
          .maxAmount24h(BigDecimal.ZERO)
          .distinctMerchants1h(0)
          .distinctMerchants24h(0)
          .distinctMccs24h(0)
          .distinctCountries24h(0)
          .fraudCount24h(0)
          .fraudCount7d(0)
          .avgAmountRatio(BigDecimal.ZERO)
          .maxAmountRatio(BigDecimal.ZERO)
          .isBurst5min(false)
          .isBurst1h(false)
          .isAnomaly(false)
          .velocityScore(0)
          .build();
    }
  }

  /**
   * Enriquece uma transação com dados de velocidade.
   *
   * @param request A transação a ser enriquecida
   * @return Contexto de velocidade com todos os campos calculados
   */
  public VelocityContext enrich(TransactionRequest request) {
    if (request == null) {
      log.debug("TransactionRequest é null, retornando contexto vazio");
      return VelocityContext.empty();
    }

    try {
      // Obter estatísticas para cada janela temporal
      VelocityStats stats5min = velocityService.getStats(request, KeyType.PAN, TimeWindow.MINUTE_5);
      VelocityStats stats15min =
          velocityService.getStats(request, KeyType.PAN, TimeWindow.MINUTE_15);
      VelocityStats stats1h = velocityService.getStats(request, KeyType.PAN, TimeWindow.HOUR_1);
      VelocityStats stats6h = velocityService.getStats(request, KeyType.PAN, TimeWindow.HOUR_6);
      VelocityStats stats24h = velocityService.getStats(request, KeyType.PAN, TimeWindow.HOUR_24);
      VelocityStats stats7d = velocityService.getStats(request, KeyType.PAN, TimeWindow.DAY_7);
      VelocityStats stats30d = velocityService.getStats(request, KeyType.PAN, TimeWindow.DAY_30);

      // Calcular razões
      BigDecimal currentAmount =
          request.getTransactionAmount() != null ? request.getTransactionAmount() : BigDecimal.ZERO;

      BigDecimal avgAmountRatio = calculateRatio(currentAmount, stats24h.getAvgAmount());
      BigDecimal maxAmountRatio = calculateRatio(currentAmount, stats24h.getMaxAmount());

      // Detectar burst
      boolean isBurst5min = stats5min.getTransactionCount() >= BURST_THRESHOLD_5MIN;
      boolean isBurst1h = stats1h.getTransactionCount() >= BURST_THRESHOLD_1H;

      // Detectar anomalia
      boolean isAnomaly =
          isBurst5min
              || isBurst1h
              || avgAmountRatio.compareTo(AVG_RATIO_ANOMALY_THRESHOLD) > 0
              || stats1h.getDistinctMerchants() >= DISTINCT_MERCHANTS_ANOMALY_THRESHOLD;

      // Calcular score de velocidade (0-100)
      int velocityScore = calculateVelocityScore(stats5min, stats1h, stats24h, avgAmountRatio);

      return VelocityContext.builder()
          .transactions5min(stats5min.getTransactionCount())
          .transactions15min(stats15min.getTransactionCount())
          .transactions1h(stats1h.getTransactionCount())
          .transactions6h(stats6h.getTransactionCount())
          .transactions24h(stats24h.getTransactionCount())
          .transactions7d(stats7d.getTransactionCount())
          .transactions30d(stats30d.getTransactionCount())
          .amount1h(stats1h.getTotalAmount())
          .amount24h(stats24h.getTotalAmount())
          .amount7d(stats7d.getTotalAmount())
          .amount30d(stats30d.getTotalAmount())
          .avgAmount24h(stats24h.getAvgAmount())
          .avgAmount7d(stats7d.getAvgAmount())
          .avgAmount30d(stats30d.getAvgAmount())
          .minAmount24h(stats24h.getMinAmount())
          .maxAmount24h(stats24h.getMaxAmount())
          .distinctMerchants1h(stats1h.getDistinctMerchants())
          .distinctMerchants24h(stats24h.getDistinctMerchants())
          .distinctMccs24h(stats24h.getDistinctMccs())
          .distinctCountries24h(stats24h.getDistinctCountries())
          .fraudCount24h(stats24h.getFraudCount())
          .fraudCount7d(stats7d.getFraudCount())
          .avgAmountRatio(avgAmountRatio)
          .maxAmountRatio(maxAmountRatio)
          .isBurst5min(isBurst5min)
          .isBurst1h(isBurst1h)
          .isAnomaly(isAnomaly)
          .velocityScore(velocityScore)
          .build();

    } catch (Exception e) {
      log.warn("Erro ao enriquecer velocidade: {}", e.getMessage());
      return VelocityContext.empty();
    }
  }

  /** Calcula a razão entre dois valores, evitando divisão por zero. */
  private BigDecimal calculateRatio(BigDecimal numerator, BigDecimal denominator) {
    if (denominator == null || denominator.compareTo(BigDecimal.ZERO) == 0) {
      return BigDecimal.ZERO;
    }
    return numerator.divide(denominator, 2, RoundingMode.HALF_UP);
  }

  /**
   * Calcula um score de velocidade baseado em múltiplas métricas.
   *
   * @return Score de 0 a 100 (maior = mais risco)
   */
  private int calculateVelocityScore(
      VelocityStats stats5min,
      VelocityStats stats1h,
      VelocityStats stats24h,
      BigDecimal avgAmountRatio) {

    int score = 0;

    // Pontuação por transações em 5 minutos (0-30 pontos)
    if (stats5min.getTransactionCount() >= 10) {
      score += 30;
    } else if (stats5min.getTransactionCount() >= 5) {
      score += 20;
    } else if (stats5min.getTransactionCount() >= 3) {
      score += 10;
    }

    // Pontuação por transações em 1 hora (0-25 pontos)
    if (stats1h.getTransactionCount() >= 50) {
      score += 25;
    } else if (stats1h.getTransactionCount() >= 20) {
      score += 15;
    } else if (stats1h.getTransactionCount() >= 10) {
      score += 10;
    }

    // Pontuação por merchants distintos (0-20 pontos)
    if (stats1h.getDistinctMerchants() >= 10) {
      score += 20;
    } else if (stats1h.getDistinctMerchants() >= 5) {
      score += 15;
    } else if (stats1h.getDistinctMerchants() >= 3) {
      score += 10;
    }

    // Pontuação por razão de valor (0-25 pontos)
    if (avgAmountRatio.compareTo(new BigDecimal("10")) > 0) {
      score += 25;
    } else if (avgAmountRatio.compareTo(new BigDecimal("5")) > 0) {
      score += 20;
    } else if (avgAmountRatio.compareTo(new BigDecimal("3")) > 0) {
      score += 15;
    } else if (avgAmountRatio.compareTo(new BigDecimal("2")) > 0) {
      score += 10;
    }

    return Math.min(100, score);
  }
}
