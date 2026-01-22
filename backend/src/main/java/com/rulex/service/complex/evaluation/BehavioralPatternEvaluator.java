package com.rulex.service.complex.evaluation;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.math.BigDecimal;
import java.util.Map;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class BehavioralPatternEvaluator {

  private BehavioralPatternEvaluator() {}

  public static boolean evaluateBehavioralBaselineDeviation(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double threshold = Double.parseDouble(condition.getValueSingle().trim());

      Object deviationObj = payload.get("behavioral_deviation_score");
      if (deviationObj == null) {
        deviationObj = payload.get("behavioralDeviationScore");
      }

      if (deviationObj == null) return false;
      double deviation =
          deviationObj instanceof Number ? ((Number) deviationObj).doubleValue() : 0.0;
      return deviation > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar BEHAVIORAL_BASELINE_DEVIATION: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateSpendingCategoryShift(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object shiftObj = payload.get("spending_category_shift");
      if (shiftObj == null) {
        shiftObj = payload.get("spendingCategoryShift");
      }

      return Boolean.TRUE.equals(shiftObj) || "true".equalsIgnoreCase(String.valueOf(shiftObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar SPENDING_CATEGORY_SHIFT: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateTransactionSizeEscalation(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double factor = Double.parseDouble(condition.getValueSingle().trim());

      Object escalationObj = payload.get("transaction_escalation_factor");
      if (escalationObj == null) {
        escalationObj = payload.get("transactionEscalationFactor");
      }

      if (escalationObj == null) return false;
      double escalation =
          escalationObj instanceof Number ? ((Number) escalationObj).doubleValue() : 0.0;
      return escalation > factor;
    } catch (Exception e) {
      log.error("Erro ao avaliar TRANSACTION_SIZE_ESCALATION: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateFrequencyPatternChange(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object changeObj = payload.get("frequency_pattern_changed");
      if (changeObj == null) {
        changeObj = payload.get("frequencyPatternChanged");
      }

      return Boolean.TRUE.equals(changeObj) || "true".equalsIgnoreCase(String.valueOf(changeObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar FREQUENCY_PATTERN_CHANGE: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateTimePreferenceShift(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object shiftObj = payload.get("time_preference_shift");
      if (shiftObj == null) {
        shiftObj = payload.get("timePreferenceShift");
      }

      return Boolean.TRUE.equals(shiftObj) || "true".equalsIgnoreCase(String.valueOf(shiftObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar TIME_PREFERENCE_SHIFT: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateChannelUsageAnomaly(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object anomalyObj = payload.get("channel_usage_anomaly");
      if (anomalyObj == null) {
        anomalyObj = payload.get("channelUsageAnomaly");
      }

      return Boolean.TRUE.equals(anomalyObj) || "true".equalsIgnoreCase(String.valueOf(anomalyObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar CHANNEL_USAGE_ANOMALY: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluatePaymentMethodSwitch(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object switchObj = payload.get("payment_method_switched");
      if (switchObj == null) {
        switchObj = payload.get("paymentMethodSwitched");
      }

      return Boolean.TRUE.equals(switchObj) || "true".equalsIgnoreCase(String.valueOf(switchObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar PAYMENT_METHOD_SWITCH: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateRecipientDiversityChange(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      int threshold = Integer.parseInt(condition.getValueSingle().trim());

      Object countObj = payload.get("new_recipient_count");
      if (countObj == null) {
        countObj = payload.get("newRecipientCount");
      }

      if (countObj == null) return false;
      int count = countObj instanceof Number ? ((Number) countObj).intValue() : 0;
      return count > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar RECIPIENT_DIVERSITY_CHANGE: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateGeographicBehaviorShift(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object shiftObj = payload.get("geographic_behavior_shift");
      if (shiftObj == null) {
        shiftObj = payload.get("geographicBehaviorShift");
      }

      return Boolean.TRUE.equals(shiftObj) || "true".equalsIgnoreCase(String.valueOf(shiftObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar GEOGRAPHIC_BEHAVIOR_SHIFT: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateSessionBehaviorAnomaly(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object anomalyObj = payload.get("session_behavior_anomaly");
      if (anomalyObj == null) {
        anomalyObj = payload.get("sessionBehaviorAnomaly");
      }

      return Boolean.TRUE.equals(anomalyObj) || "true".equalsIgnoreCase(String.valueOf(anomalyObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar SESSION_BEHAVIOR_ANOMALY: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateLoginPatternDeviation(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object deviationObj = payload.get("login_pattern_deviation");
      if (deviationObj == null) {
        deviationObj = payload.get("loginPatternDeviation");
      }

      return Boolean.TRUE.equals(deviationObj)
          || "true".equalsIgnoreCase(String.valueOf(deviationObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar LOGIN_PATTERN_DEVIATION: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateNavigationPatternAnomaly(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object anomalyObj = payload.get("navigation_pattern_anomaly");
      if (anomalyObj == null) {
        anomalyObj = payload.get("navigationPatternAnomaly");
      }

      return Boolean.TRUE.equals(anomalyObj) || "true".equalsIgnoreCase(String.valueOf(anomalyObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar NAVIGATION_PATTERN_ANOMALY: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateTransactionTimingCluster(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      int threshold = Integer.parseInt(condition.getValueSingle().trim());

      Object clusterObj = payload.get("transaction_cluster_size");
      if (clusterObj == null) {
        clusterObj = payload.get("transactionClusterSize");
      }

      if (clusterObj == null) return false;
      int clusterSize = clusterObj instanceof Number ? ((Number) clusterObj).intValue() : 0;
      return clusterSize >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar TRANSACTION_TIMING_CLUSTER: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateAmountRoundingBehavior(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double threshold = Double.parseDouble(condition.getValueSingle().trim());

      Object percentObj = payload.get("rounding_percentage");
      if (percentObj == null) {
        percentObj = payload.get("roundingPercentage");
      }

      if (percentObj == null) {
        // Fallback: check current transaction
        TransactionRequest tx = context.getTransactionRequest();
        if (tx != null && tx.getTransactionAmount() != null) {
          BigDecimal amount = tx.getTransactionAmount();
          boolean isRound =
              amount.remainder(BigDecimal.valueOf(100)).compareTo(BigDecimal.ZERO) == 0;
          return isRound && threshold <= 50; // Simple heuristic
        }
        return false;
      }

      double percentage = percentObj instanceof Number ? ((Number) percentObj).doubleValue() : 0.0;
      return percentage >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar AMOUNT_ROUNDING_BEHAVIOR: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateSplitPaymentPattern(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      int threshold = Integer.parseInt(condition.getValueSingle().trim());

      Object countObj = payload.get("split_payment_count");
      if (countObj == null) {
        countObj = payload.get("splitPaymentCount");
      }

      if (countObj == null) return false;
      int count = countObj instanceof Number ? ((Number) countObj).intValue() : 0;
      return count >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar SPLIT_PAYMENT_PATTERN: {}", e.getMessage());
      return false;
    }
  }
}
