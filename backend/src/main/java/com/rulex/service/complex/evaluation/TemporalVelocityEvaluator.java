package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.Map;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class TemporalVelocityEvaluator {

  private TemporalVelocityEvaluator() {}

  public static boolean evaluateTimeBetweenConsecutiveTx(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      int minSeconds = Integer.parseInt(condition.getValueSingle().trim());

      Object secondsObj = payload.get("seconds_since_last_transaction");
      if (secondsObj == null) {
        secondsObj = payload.get("timeSinceLastTxSeconds");
      }

      int seconds =
          secondsObj instanceof Number ? ((Number) secondsObj).intValue() : Integer.MAX_VALUE;
      return seconds < minSeconds;
    } catch (Exception e) {
      log.error("Erro ao avaliar TIME_BETWEEN_CONSECUTIVE_TX: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateTransactionFrequencyAnomaly(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double multiplier = Double.parseDouble(condition.getValueSingle().trim());

      Object freqObj = payload.get("transaction_frequency_ratio");
      if (freqObj == null) {
        freqObj = payload.get("transactionFrequencyRatio");
      }

      double ratio = freqObj instanceof Number ? ((Number) freqObj).doubleValue() : 0.0;
      return ratio > multiplier;
    } catch (Exception e) {
      log.error("Erro ao avaliar TRANSACTION_FREQUENCY_ANOMALY: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateTimeOfDayAnomaly(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double threshold = Double.parseDouble(condition.getValueSingle().trim());

      Object scoreObj = payload.get("time_of_day_anomaly_score");
      if (scoreObj == null) {
        scoreObj = payload.get("timeOfDayAnomalyScore");
      }

      double score = scoreObj instanceof Number ? ((Number) scoreObj).doubleValue() : 0.0;
      return score > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar TIME_OF_DAY_ANOMALY: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateDormancyAlertVelocity(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      int maxDormancyDays = Integer.parseInt(condition.getValueSingle().trim());

      Object dormancyObj = payload.get("account_dormant_days");
      if (dormancyObj == null) {
        dormancyObj = payload.get("accountDormantDays");
      }

      int days =
          dormancyObj instanceof Number ? ((Number) dormancyObj).intValue() : Integer.MAX_VALUE;
      return days >= maxDormancyDays;
    } catch (Exception e) {
      log.error("Erro ao avaliar DORMANCY_ALERT_VELOCITY: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateWeekendVsWeekdayPattern(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double ratioThreshold = Double.parseDouble(condition.getValueSingle().trim());

      Object ratioObj = payload.get("weekend_weekday_ratio");
      if (ratioObj == null) {
        ratioObj = payload.get("weekendWeekdayRatio");
      }

      double ratio = ratioObj instanceof Number ? ((Number) ratioObj).doubleValue() : 0.0;
      return ratio > ratioThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar WEEKEND_VS_WEEKDAY_PATTERN: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateHolidayTransactionSpike(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double multiplier = Double.parseDouble(condition.getValueSingle().trim());

      Object spikeObj = payload.get("holiday_transaction_multiplier");
      if (spikeObj == null) {
        spikeObj = payload.get("holidayTransactionMultiplier");
      }

      double spike = spikeObj instanceof Number ? ((Number) spikeObj).doubleValue() : 0.0;
      return spike > multiplier;
    } catch (Exception e) {
      log.error("Erro ao avaliar HOLIDAY_TRANSACTION_SPIKE: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateNighttimeTransactionRatio(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double threshold = Double.parseDouble(condition.getValueSingle().trim());

      Object ratioObj = payload.get("nighttime_transaction_ratio");
      if (ratioObj == null) {
        ratioObj = payload.get("nighttimeTransactionRatio");
      }

      double ratio = ratioObj instanceof Number ? ((Number) ratioObj).doubleValue() : 0.0;
      return ratio > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar NIGHTTIME_TRANSACTION_RATIO: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateBusinessHoursDeviation(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double threshold = Double.parseDouble(condition.getValueSingle().trim());

      Object devObj = payload.get("business_hours_deviation_score");
      if (devObj == null) {
        devObj = payload.get("businessHoursDeviationScore");
      }

      double deviation = devObj instanceof Number ? ((Number) devObj).doubleValue() : 0.0;
      return deviation > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar BUSINESS_HOURS_DEVIATION: {}", e.getMessage());
      return false;
    }
  }
}
