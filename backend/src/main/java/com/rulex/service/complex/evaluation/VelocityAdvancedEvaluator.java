package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.VelocityService;
import com.rulex.service.VelocityServiceFacade;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Map;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class VelocityAdvancedEvaluator {

  private VelocityAdvancedEvaluator() {}

  // --- CATEGORIA L: Transaction Count Velocity Avançado (12) ---

  public static boolean evaluateTransactionCountPerCardHour(
      RuleCondition condition, EvaluationContext context, VelocityServiceFacade velocityServiceFacade) {
    return evaluateVelocityOperator(
        condition,
        context,
        velocityServiceFacade,
        VelocityService.KeyType.PAN,
        VelocityService.TimeWindow.HOUR_1,
        VelocityService.AggregationType.COUNT);
  }

  public static boolean evaluateTransactionCountPerIpHour(
      RuleCondition condition, EvaluationContext context, VelocityServiceFacade velocityServiceFacade) {
    return evaluateVelocityOperator(
        condition,
        context,
        velocityServiceFacade,
        VelocityService.KeyType.IP_ADDRESS,
        VelocityService.TimeWindow.HOUR_1,
        VelocityService.AggregationType.COUNT);
  }

  public static boolean evaluateTransactionCountPerDeviceDay(
      RuleCondition condition, EvaluationContext context, VelocityServiceFacade velocityServiceFacade) {
    return evaluateVelocityOperator(
        condition,
        context,
        velocityServiceFacade,
        VelocityService.KeyType.DEVICE_ID,
        VelocityService.TimeWindow.HOUR_24,
        VelocityService.AggregationType.COUNT);
  }

  public static boolean evaluateTransactionCountPerMerchantHour(
      RuleCondition condition, EvaluationContext context, VelocityServiceFacade velocityServiceFacade) {
    return evaluateVelocityOperator(
        condition,
        context,
        velocityServiceFacade,
        VelocityService.KeyType.MERCHANT_ID,
        VelocityService.TimeWindow.HOUR_1,
        VelocityService.AggregationType.COUNT);
  }

  public static boolean evaluateTransactionCountPerCustomerHour(
      RuleCondition condition, EvaluationContext context, VelocityServiceFacade velocityServiceFacade) {
    return evaluateVelocityOperator(
        condition,
        context,
        velocityServiceFacade,
        VelocityService.KeyType.CUSTOMER_ID,
        VelocityService.TimeWindow.HOUR_1,
        VelocityService.AggregationType.COUNT);
  }

  public static boolean evaluateUniqueCardCountPerIpHour(
      RuleCondition condition, EvaluationContext context, VelocityServiceFacade velocityServiceFacade) {
    try {
      if (context.getTransactionRequest() == null) return false;
      long threshold = Long.parseLong(condition.getValueSingle().trim());

      VelocityService.VelocityStats stats =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(),
              VelocityService.KeyType.IP_ADDRESS,
              VelocityService.TimeWindow.HOUR_1);

      return stats.getDistinctPans() > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar UNIQUE_CARD_COUNT_PER_IP_HOUR: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateUniqueMerchantCountPerCardDay(
      RuleCondition condition, EvaluationContext context, VelocityServiceFacade velocityServiceFacade) {
    try {
      if (context.getTransactionRequest() == null) return false;
      long threshold = Long.parseLong(condition.getValueSingle().trim());

      VelocityService.VelocityStats stats =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(),
              VelocityService.KeyType.PAN,
              VelocityService.TimeWindow.HOUR_24);

      return stats.getDistinctMerchants() > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar UNIQUE_MERCHANT_COUNT_PER_CARD_DAY: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateTransactionAttemptCountPerCard(
      RuleCondition condition, EvaluationContext context, VelocityServiceFacade velocityServiceFacade) {
    try {
      if (context.getTransactionRequest() == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      long threshold = Long.parseLong(parts[0].trim());
      int minutes = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : 15;

      VelocityService.TimeWindow window = parseTimeWindow(minutes);
      VelocityService.VelocityStats stats =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(), VelocityService.KeyType.PAN, window);

      // Inclui tentativas com falha
      return stats.getTransactionCount() + stats.getFailedCount() > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar TRANSACTION_ATTEMPT_COUNT_PER_CARD: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateCvvFailureVelocity(
      RuleCondition condition, EvaluationContext context, VelocityServiceFacade velocityServiceFacade) {
    try {
      if (context.getTransactionRequest() == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      long threshold = Long.parseLong(parts[0].trim());
      int minutes = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : 10;

      VelocityService.TimeWindow window = parseTimeWindow(minutes);
      VelocityService.VelocityStats stats =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(), VelocityService.KeyType.PAN, window);

      return stats.getCvvFailures() > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar CVV_FAILURE_VELOCITY: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateAddressChangeVelocity(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      int threshold = Integer.parseInt(parts[0].trim());
      int days = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : 30;

      Object changesObj = payload.get("address_changes_" + days + "d");
      if (changesObj == null) {
        changesObj = payload.get("addressChangesCount");
      }

      int changes = changesObj instanceof Number ? ((Number) changesObj).intValue() : 0;
      return changes > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar ADDRESS_CHANGE_VELOCITY: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateBeneficiaryAddVelocity(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      int threshold = Integer.parseInt(parts[0].trim());
      int days = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : 7;

      Object addedObj = payload.get("beneficiaries_added_" + days + "d");
      if (addedObj == null) {
        addedObj = payload.get("newBeneficiariesCount");
      }

      int added = addedObj instanceof Number ? ((Number) addedObj).intValue() : 0;
      return added > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar BENEFICIARY_ADD_VELOCITY: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateCardAddVelocity(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      int threshold = Integer.parseInt(parts[0].trim());
      int days = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : 7;

      Object addedObj = payload.get("cards_added_" + days + "d");
      if (addedObj == null) {
        addedObj = payload.get("newCardsCount");
      }

      int added = addedObj instanceof Number ? ((Number) addedObj).intValue() : 0;
      return added > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar CARD_ADD_VELOCITY: {}", e.getMessage());
      return false;
    }
  }

  // --- CATEGORIA A: Velocity Avançado (10) ---

  /**
   * VELOCITY_CROSS_CHANNEL: Detecta velocidade entre diferentes canais. Formato valueSingle:
   * "threshold|channelField" (ex: "5|channel_type")
   */
  public static boolean evaluateVelocityCrossChannel(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getTransactionRequest() == null) return false;
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      int threshold = Integer.parseInt(parts[0].trim());

      Object channelsObj = payload.get("cross_channel_count");
      if (channelsObj == null) channelsObj = payload.get("distinctChannels");

      int channelCount = channelsObj instanceof Number ? ((Number) channelsObj).intValue() : 1;
      return channelCount > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar VELOCITY_CROSS_CHANNEL: {}", e.getMessage());
      return false;
    }
  }

  /**
   * VELOCITY_ROLLING_WINDOW: Velocidade com janela móvel customizada. Formato valueSingle:
   * "threshold|minutes" (ex: "10|30")
   */
  public static boolean evaluateVelocityRollingWindow(
      RuleCondition condition, EvaluationContext context, VelocityServiceFacade velocityServiceFacade) {
    try {
      if (context.getTransactionRequest() == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      long threshold = Long.parseLong(parts[0].trim());
      int minutes = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : 60;

      VelocityService.TimeWindow window = parseTimeWindow(minutes);
      VelocityService.VelocityStats stats =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(), VelocityService.KeyType.PAN, window);

      return stats.getTransactionCount() > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar VELOCITY_ROLLING_WINDOW: {}", e.getMessage());
      return false;
    }
  }

  /**
   * VELOCITY_PERCENTILE: Verifica se transação está acima de determinado percentil. Formato
   * valueSingle: "percentile" (ex: "95" = acima do P95)
   */
  public static boolean evaluateVelocityPercentile(
      RuleCondition condition, EvaluationContext context, VelocityServiceFacade velocityServiceFacade) {
    try {
      if (context.getTransactionRequest() == null) return false;

      double percentileThreshold = Double.parseDouble(condition.getValueSingle().trim());

      VelocityService.VelocityStats stats =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(),
              VelocityService.KeyType.PAN,
              VelocityService.TimeWindow.HOUR_24);

      BigDecimal avg = stats.getAvgAmount();
      BigDecimal stdDev = stats.getStdDevAmount();
      BigDecimal txAmount = context.getTransactionRequest().getTransactionAmount();

      if (avg == null || stdDev == null || stdDev.compareTo(BigDecimal.ZERO) == 0) return false;

      // Usar Z-score para aproximar percentil
      double zScore = txAmount.subtract(avg).divide(stdDev, 4, RoundingMode.HALF_UP).doubleValue();
      double percentile = 50 + (50 * Math.tanh(zScore / 2)); // Aproximação

      return percentile > percentileThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar VELOCITY_PERCENTILE: {}", e.getMessage());
      return false;
    }
  }

  /**
   * VELOCITY_RATIO_GT: Razão entre velocidade atual e histórica. Formato valueSingle: "ratio" (ex:
   * "2.0" = 2x a velocidade normal)
   */
  public static boolean evaluateVelocityRatioGt(
      RuleCondition condition, EvaluationContext context, VelocityServiceFacade velocityServiceFacade) {
    try {
      if (context.getTransactionRequest() == null) return false;

      double ratioThreshold = Double.parseDouble(condition.getValueSingle().trim());

      VelocityService.VelocityStats statsHour =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(),
              VelocityService.KeyType.PAN,
              VelocityService.TimeWindow.HOUR_1);
      VelocityService.VelocityStats stats24h =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(),
              VelocityService.KeyType.PAN,
              VelocityService.TimeWindow.HOUR_24);

      long countHour = statsHour.getTransactionCount();
      long count24h = stats24h.getTransactionCount();

      if (count24h == 0) return false;

      double avgHourly = count24h / 24.0;
      double ratio = avgHourly > 0 ? countHour / avgHourly : 0;

      return ratio > ratioThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar VELOCITY_RATIO_GT: {}", e.getMessage());
      return false;
    }
  }

  /**
   * VELOCITY_TREND: Detecta tendência de aumento na velocidade. Formato valueSingle:
   * "direction|threshold" (ex: "UP|1.5")
   */
  public static boolean evaluateVelocityTrend(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      String direction = parts[0].trim().toUpperCase();
      double threshold = parts.length > 1 ? Double.parseDouble(parts[1].trim()) : 1.5;

      Object trendObj = payload.get("velocity_trend_ratio");
      if (trendObj == null) trendObj = payload.get("velocityTrendRatio");

      double trendRatio = trendObj instanceof Number ? ((Number) trendObj).doubleValue() : 1.0;

      if ("UP".equals(direction)) {
        return trendRatio > threshold;
      } else if ("DOWN".equals(direction)) {
        return trendRatio < (1.0 / threshold);
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar VELOCITY_TREND: {}", e.getMessage());
      return false;
    }
  }

  /**
   * COUNT_UNIQUE_BENEFICIARIES_LAST_N_DAYS: Conta beneficiários únicos em N dias. Formato
   * valueSingle: "threshold|days" (ex: "10|7")
   */
  public static boolean evaluateCountUniqueBeneficiariesLastNDays(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      int threshold = Integer.parseInt(parts[0].trim());

      Object countObj = payload.get("unique_beneficiaries_count");
      if (countObj == null) countObj = payload.get("uniqueBeneficiariesCount");

      int count = countObj instanceof Number ? ((Number) countObj).intValue() : 0;
      return count > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_UNIQUE_BENEFICIARIES_LAST_N_DAYS: {}", e.getMessage());
      return false;
    }
  }

  /**
   * COUNT_UNIQUE_IPS_LAST_N_HOURS: Conta IPs únicos em N horas. Formato valueSingle:
   * "threshold|hours" (ex: "5|24")
   */
  public static boolean evaluateCountUniqueIpsLastNHours(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      int threshold = Integer.parseInt(parts[0].trim());

      Object countObj = payload.get("unique_ips_count");
      if (countObj == null) countObj = payload.get("uniqueIpsCount");

      int count = countObj instanceof Number ? ((Number) countObj).intValue() : 0;
      return count > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_UNIQUE_IPS_LAST_N_HOURS: {}", e.getMessage());
      return false;
    }
  }

  /**
   * SUM_BY_CHANNEL_LAST_N_DAYS: Soma valores por canal em N dias. Formato valueSingle:
   * "threshold|channel|days" (ex: "50000|PIX|7")
   */
  public static boolean evaluateSumByChannelLastNDays(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      double threshold = Double.parseDouble(parts[0].trim());
      String channel = parts.length > 1 ? parts[1].trim().toUpperCase() : "ALL";

      String key = "sum_by_channel_" + channel.toLowerCase();
      Object sumObj = payload.get(key);
      if (sumObj == null) sumObj = payload.get("sumByChannel" + channel);

      double sum = sumObj instanceof Number ? ((Number) sumObj).doubleValue() : 0.0;
      return sum > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar SUM_BY_CHANNEL_LAST_N_DAYS: {}", e.getMessage());
      return false;
    }
  }

  /**
   * AVG_INTERVAL_BETWEEN_TXN: Intervalo médio entre transações. Formato valueSingle: "minSeconds"
   * (ex: "30" = mínimo 30s entre txns)
   */
  public static boolean evaluateAvgIntervalBetweenTxn(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double minSeconds = Double.parseDouble(condition.getValueSingle().trim());

      Object intervalObj = payload.get("avg_interval_seconds");
      if (intervalObj == null) intervalObj = payload.get("avgIntervalSeconds");

      double avgInterval =
          intervalObj instanceof Number ? ((Number) intervalObj).doubleValue() : Double.MAX_VALUE;
      return avgInterval < minSeconds;
    } catch (Exception e) {
      log.error("Erro ao avaliar AVG_INTERVAL_BETWEEN_TXN: {}", e.getMessage());
      return false;
    }
  }

  /**
   * VELOCITY_ACCELERATION: Detecta aceleração na frequência de transações. Formato valueSingle:
   * "accelerationThreshold" (ex: "2.0" = aceleração 2x)
   */
  public static boolean evaluateVelocityAcceleration(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double threshold = Double.parseDouble(condition.getValueSingle().trim());

      Object accelObj = payload.get("velocity_acceleration");
      if (accelObj == null) accelObj = payload.get("velocityAcceleration");

      double acceleration = accelObj instanceof Number ? ((Number) accelObj).doubleValue() : 1.0;
      return acceleration > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar VELOCITY_ACCELERATION: {}", e.getMessage());
      return false;
    }
  }

  // --- CATEGORIA M: Amount Velocity Avançado (10) ---

  public static boolean evaluateAmountSumPerCardHour(
      RuleCondition condition, EvaluationContext context, VelocityServiceFacade velocityServiceFacade) {
    return evaluateVelocityOperator(
        condition,
        context,
        velocityServiceFacade,
        VelocityService.KeyType.PAN,
        VelocityService.TimeWindow.HOUR_1,
        VelocityService.AggregationType.SUM);
  }

  public static boolean evaluateAmountSumPerCustomerDay(
      RuleCondition condition, EvaluationContext context, VelocityServiceFacade velocityServiceFacade) {
    return evaluateVelocityOperator(
        condition,
        context,
        velocityServiceFacade,
        VelocityService.KeyType.CUSTOMER_ID,
        VelocityService.TimeWindow.HOUR_24,
        VelocityService.AggregationType.SUM);
  }

  public static boolean evaluateAvgTransactionSpike(
      RuleCondition condition, EvaluationContext context, VelocityServiceFacade velocityServiceFacade) {
    try {
      if (context.getTransactionRequest() == null) return false;

      double multiplier = Double.parseDouble(condition.getValueSingle().trim());

      VelocityService.VelocityStats stats =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(),
              VelocityService.KeyType.PAN,
              VelocityService.TimeWindow.DAY_30);

      BigDecimal currentAmount = context.getTransactionRequest().getTransactionAmount();
      BigDecimal avgAmount = stats.getAvgAmount();

      if (avgAmount == null || avgAmount.compareTo(BigDecimal.ZERO) == 0) {
        return false;
      }

      BigDecimal spikeThreshold = avgAmount.multiply(BigDecimal.valueOf(multiplier));
      return currentAmount.compareTo(spikeThreshold) > 0;
    } catch (Exception e) {
      log.error("Erro ao avaliar AVG_TRANSACTION_SPIKE: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateLargeAmountFrequency(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      double amountThreshold = Double.parseDouble(parts[0].trim());
      int countThreshold = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : 3;
      int days = parts.length > 2 ? Integer.parseInt(parts[2].trim()) : 7;

      Object countObj =
          payload.get("large_amount_count_" + days + "d_over_" + (int) amountThreshold);
      if (countObj == null) {
        countObj = payload.get("largeAmountTransactionCount");
      }

      int count = countObj instanceof Number ? ((Number) countObj).intValue() : 0;
      return count >= countThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar LARGE_AMOUNT_FREQUENCY: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateSmallAmountVelocity(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      double maxAmount = Double.parseDouble(parts[0].trim());
      int countThreshold = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : 10;
      int hours = parts.length > 2 ? Integer.parseInt(parts[2].trim()) : 1;

      Object countObj = payload.get("small_amount_count_" + hours + "h_under_" + (int) maxAmount);
      if (countObj == null) {
        countObj = payload.get("microTransactionCount");
      }

      int count = countObj instanceof Number ? ((Number) countObj).intValue() : 0;
      return count >= countThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar SMALL_AMOUNT_VELOCITY: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateRoundAmountFrequency(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      int roundThreshold = Integer.parseInt(parts[0].trim());
      double percentageThreshold = parts.length > 1 ? Double.parseDouble(parts[1].trim()) : 50.0;

      Object percentageObj = payload.get("round_amount_percentage_divisible_" + roundThreshold);
      if (percentageObj == null) {
        percentageObj = payload.get("roundAmountPercentage");
      }

      double percentage =
          percentageObj instanceof Number ? ((Number) percentageObj).doubleValue() : 0.0;
      return percentage >= percentageThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar ROUND_AMOUNT_FREQUENCY: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateSequentialAmountPattern(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String patternType =
          condition.getValueSingle() != null
              ? condition.getValueSingle().trim().toLowerCase()
              : "linear";

      Object patternObj = payload.get("sequential_pattern_" + patternType);
      if (patternObj == null) {
        patternObj = payload.get("hasSequentialAmountPattern");
      }

      return Boolean.TRUE.equals(patternObj) || "true".equalsIgnoreCase(String.valueOf(patternObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar SEQUENTIAL_AMOUNT_PATTERN: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateAmountVarianceAnomaly(
      RuleCondition condition, EvaluationContext context, VelocityServiceFacade velocityServiceFacade) {
    try {
      if (context.getTransactionRequest() == null) return false;

      double zScoreThreshold = Double.parseDouble(condition.getValueSingle().trim());

      VelocityService.VelocityStats stats =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(),
              VelocityService.KeyType.PAN,
              VelocityService.TimeWindow.DAY_30);

      BigDecimal currentAmount = context.getTransactionRequest().getTransactionAmount();
      BigDecimal avgAmount = stats.getAvgAmount();
      BigDecimal stdDev = stats.getStdDevAmount();

      if (stdDev == null || stdDev.compareTo(BigDecimal.ZERO) == 0) {
        return false;
      }

      double zScore =
          currentAmount
              .subtract(avgAmount)
              .divide(stdDev, 4, java.math.RoundingMode.HALF_UP)
              .doubleValue();
      return Math.abs(zScore) > zScoreThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar AMOUNT_VARIANCE_ANOMALY: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateDailyLimitProximity(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double percentageThreshold = Double.parseDouble(condition.getValueSingle().trim());

      Object usedObj = payload.get("daily_limit_used_percentage");
      if (usedObj == null) {
        usedObj = payload.get("dailyLimitUsagePercentage");
      }

      double usedPercentage = usedObj instanceof Number ? ((Number) usedObj).doubleValue() : 0.0;
      return usedPercentage >= percentageThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar DAILY_LIMIT_PROXIMITY: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateWeeklyLimitProximity(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double percentageThreshold = Double.parseDouble(condition.getValueSingle().trim());

      Object usedObj = payload.get("weekly_limit_used_percentage");
      if (usedObj == null) {
        usedObj = payload.get("weeklyLimitUsagePercentage");
      }

      double usedPercentage = usedObj instanceof Number ? ((Number) usedObj).doubleValue() : 0.0;
      return usedPercentage >= percentageThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar WEEKLY_LIMIT_PROXIMITY: {}", e.getMessage());
      return false;
    }
  }

  private static VelocityService.TimeWindow parseTimeWindow(int minutes) {
    if (minutes <= 5) return VelocityService.TimeWindow.MINUTE_5;
    if (minutes <= 15) return VelocityService.TimeWindow.MINUTE_15;
    if (minutes <= 30) return VelocityService.TimeWindow.MINUTE_30;
    if (minutes <= 60) return VelocityService.TimeWindow.HOUR_1;
    if (minutes <= 360) return VelocityService.TimeWindow.HOUR_6;
    if (minutes <= 720) return VelocityService.TimeWindow.HOUR_12;
    if (minutes <= 1440) return VelocityService.TimeWindow.HOUR_24;
    if (minutes <= 10080) return VelocityService.TimeWindow.DAY_7;
    return VelocityService.TimeWindow.DAY_30;
  }

  private static boolean evaluateVelocityOperator(
      RuleCondition condition,
      EvaluationContext context,
      VelocityServiceFacade velocityServiceFacade,
      VelocityService.KeyType keyType,
      VelocityService.TimeWindow window,
      VelocityService.AggregationType aggregationType) {
    try {
      if (context.getTransactionRequest() == null) return false;

      BigDecimal threshold = new BigDecimal(condition.getValueSingle().trim());

      VelocityService.VelocityStats stats =
          velocityServiceFacade.getStats(context.getTransactionRequest(), keyType, window);

      BigDecimal value =
          switch (aggregationType) {
            case COUNT -> BigDecimal.valueOf(stats.getTransactionCount());
            case SUM -> stats.getTotalAmount();
            case AVG -> stats.getAvgAmount();
            case MAX -> stats.getMaxAmount();
            case MIN -> stats.getMinAmount();
            default -> BigDecimal.ZERO;
          };

      return value.compareTo(threshold) > 0;
    } catch (Exception e) {
      log.error("Erro ao avaliar operador de velocidade: {}", e.getMessage());
      return false;
    }
  }
}
