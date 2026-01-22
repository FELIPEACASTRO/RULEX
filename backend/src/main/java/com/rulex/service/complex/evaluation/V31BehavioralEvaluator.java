package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.VelocityService;
import com.rulex.service.VelocityServiceFacade;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.math.BigDecimal;
import java.util.Map;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class V31BehavioralEvaluator {

  private V31BehavioralEvaluator() {}

  public static boolean evaluateDormancyRevival(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      int dormancyDays = Integer.parseInt(parts[0].trim());
      int txThreshold = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : 3;

      Object lastActivityObj = payload.get("days_since_last_activity");
      if (lastActivityObj == null) lastActivityObj = payload.get("daysSinceLastActivity");

      Object recentTxObj = payload.get("recent_tx_count");
      if (recentTxObj == null) recentTxObj = payload.get("recentTxCount");

      int daysSinceActivity =
          lastActivityObj instanceof Number ? ((Number) lastActivityObj).intValue() : 0;
      int recentTxCount = recentTxObj instanceof Number ? ((Number) recentTxObj).intValue() : 0;

      return daysSinceActivity > dormancyDays && recentTxCount >= txThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar DORMANCY_REVIVAL: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateAmountDeviationFromAvg(
      RuleCondition condition,
      EvaluationContext context,
      VelocityServiceFacade velocityServiceFacade) {
    try {
      if (context.getTransactionRequest() == null) return false;

      double multiplier = Double.parseDouble(condition.getValueSingle().trim());

      VelocityService.VelocityStats stats =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(),
              VelocityService.KeyType.PAN,
              VelocityService.TimeWindow.HOUR_24);

      BigDecimal avg = stats.getAvgAmount();
      BigDecimal stdDev = stats.getStdDevAmount();
      BigDecimal txAmount = context.getTransactionRequest().getTransactionAmount();

      if (avg == null || stdDev == null || stdDev.compareTo(BigDecimal.ZERO) == 0) return false;

      BigDecimal deviation = txAmount.subtract(avg).abs();
      BigDecimal threshold = stdDev.multiply(BigDecimal.valueOf(multiplier));

      return deviation.compareTo(threshold) > 0;
    } catch (Exception e) {
      log.error("Erro ao avaliar AMOUNT_DEVIATION_FROM_AVG: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateTimeDeviationFromUsual(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      int hoursThreshold = Integer.parseInt(condition.getValueSingle().trim());

      Object deviationObj = payload.get("time_deviation_hours");
      if (deviationObj == null) deviationObj = payload.get("timeDeviationHours");

      double deviation =
          deviationObj instanceof Number ? ((Number) deviationObj).doubleValue() : 0.0;
      return Math.abs(deviation) > hoursThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar TIME_DEVIATION_FROM_USUAL: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateMerchantDeviation(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object deviationObj = payload.get("merchant_is_unusual");
      if (deviationObj == null) deviationObj = payload.get("merchantIsUnusual");
      if (deviationObj == null) deviationObj = payload.get("new_merchant");

      return Boolean.TRUE.equals(deviationObj)
          || "true".equalsIgnoreCase(String.valueOf(deviationObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_DEVIATION: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateMicroTransactionTest(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getTransactionRequest() == null) return false;
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      double maxAmount = Double.parseDouble(parts[0].trim());
      int countThreshold = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : 2;

      BigDecimal txAmount = context.getTransactionRequest().getTransactionAmount();

      Object microCountObj = payload.get("micro_tx_count");
      if (microCountObj == null) microCountObj = payload.get("microTxCount");

      int microCount = microCountObj instanceof Number ? ((Number) microCountObj).intValue() : 0;

      boolean isMicro = txAmount.doubleValue() <= maxAmount;
      return isMicro && microCount >= countThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar MICRO_TRANSACTION_TEST: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateLocationDeviation(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double kmThreshold = Double.parseDouble(condition.getValueSingle().trim());

      Object distanceObj = payload.get("distance_from_usual_km");
      if (distanceObj == null) distanceObj = payload.get("distanceFromUsualKm");

      double distance = distanceObj instanceof Number ? ((Number) distanceObj).doubleValue() : 0.0;
      return distance > kmThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar LOCATION_DEVIATION: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateChannelSwitchPattern(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      int switchThreshold = Integer.parseInt(parts[0].trim());

      Object switchCountObj = payload.get("channel_switch_count");
      if (switchCountObj == null) switchCountObj = payload.get("channelSwitchCount");

      int switchCount = switchCountObj instanceof Number ? ((Number) switchCountObj).intValue() : 0;
      return switchCount >= switchThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar CHANNEL_SWITCH_PATTERN: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateBeneficiaryReusePattern(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      int reuseThreshold = Integer.parseInt(condition.getValueSingle().trim());

      Object reuseCountObj = payload.get("beneficiary_reuse_count");
      if (reuseCountObj == null) reuseCountObj = payload.get("beneficiaryReuseCount");

      int reuseCount = reuseCountObj instanceof Number ? ((Number) reuseCountObj).intValue() : 0;
      return reuseCount >= reuseThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar BENEFICIARY_REUSE_PATTERN: {}", e.getMessage());
      return false;
    }
  }
}
