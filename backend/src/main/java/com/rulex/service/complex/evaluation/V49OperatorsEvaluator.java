package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import com.rulex.service.complex.context.FieldValueExtractor;
import java.util.Map;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class V49OperatorsEvaluator {

  private V49OperatorsEvaluator() {}

  public static boolean evaluateLogicalAnd(RuleCondition condition, EvaluationContext context) {
    Object fieldValue =
        FieldValueExtractor.getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
    boolean a = Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue));
    boolean b = Boolean.parseBoolean(condition.getValueSingle());
    return a && b;
  }

  public static boolean evaluateLogicalOr(RuleCondition condition, EvaluationContext context) {
    Object fieldValue =
        FieldValueExtractor.getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
    boolean a = Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue));
    boolean b = Boolean.parseBoolean(condition.getValueSingle());
    return a || b;
  }

  public static boolean evaluateLogicalNot(RuleCondition condition, EvaluationContext context) {
    Object fieldValue =
        FieldValueExtractor.getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
    return !Boolean.TRUE.equals(fieldValue) && !"true".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  public static boolean evaluateLogicalXor(RuleCondition condition, EvaluationContext context) {
    Object fieldValue =
        FieldValueExtractor.getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
    boolean a = Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue));
    boolean b = Boolean.parseBoolean(condition.getValueSingle());
    return a ^ b;
  }

  public static boolean evaluateLogicalNand(RuleCondition condition, EvaluationContext context) {
    return !evaluateLogicalAnd(condition, context);
  }

  public static boolean evaluateLogicalNor(RuleCondition condition, EvaluationContext context) {
    return !evaluateLogicalOr(condition, context);
  }

  public static boolean evaluateAmountAnomalyOp(RuleCondition condition, EvaluationContext context) {
    try {
      Object fieldValue =
          FieldValueExtractor.getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
      double amount = Double.parseDouble(String.valueOf(fieldValue));
      Map<String, Object> payload = context.getPayload();
      double avgAmount =
          payload != null && payload.containsKey("avgAmount")
              ? Double.parseDouble(payload.get("avgAmount").toString())
              : 0;
      double stdDev =
          payload != null && payload.containsKey("amountStdDev")
              ? Double.parseDouble(payload.get("amountStdDev").toString())
              : 1;
      double threshold =
          condition.getValueSingle() != null
              ? Double.parseDouble(condition.getValueSingle())
              : 3.0;
      return Math.abs(amount - avgAmount) > threshold * stdDev;
    } catch (Exception e) {
      return false;
    }
  }

  public static boolean evaluateTimeAnomalyOp(RuleCondition condition, EvaluationContext context) {
    try {
      Object fieldValue =
          FieldValueExtractor.getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
      int hour = Integer.parseInt(String.valueOf(fieldValue));
      return hour >= 2 && hour <= 5;
    } catch (Exception e) {
      return false;
    }
  }

  public static boolean evaluateVelocityAnomalyOp(RuleCondition condition, EvaluationContext context) {
    try {
      Object fieldValue =
          FieldValueExtractor.getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
      double velocity = Double.parseDouble(String.valueOf(fieldValue));
      Map<String, Object> payload = context.getPayload();
      double avgVelocity =
          payload != null && payload.containsKey("avgVelocity")
              ? Double.parseDouble(payload.get("avgVelocity").toString())
              : 0;
      double stdDev =
          payload != null && payload.containsKey("velocityStdDev")
              ? Double.parseDouble(payload.get("velocityStdDev").toString())
              : 1;
      double threshold =
          condition.getValueSingle() != null
              ? Double.parseDouble(condition.getValueSingle())
              : 2.0;
      return Math.abs(velocity - avgVelocity) > threshold * stdDev;
    } catch (Exception e) {
      return false;
    }
  }

  public static boolean evaluateMccAnomalyOp(RuleCondition condition, EvaluationContext context) {
    Object fieldValue =
        FieldValueExtractor.getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
    return Boolean.TRUE.equals(fieldValue)
        || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "ANOMALY".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  public static boolean evaluateMerchantAnomalyOp(RuleCondition condition, EvaluationContext context) {
    Object fieldValue =
        FieldValueExtractor.getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
    return Boolean.TRUE.equals(fieldValue)
        || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "ANOMALY".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  public static boolean evaluateIsNewDeviceOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue)
        || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "NEW".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  public static boolean evaluateIsNewLocationOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue)
        || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "NEW".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  public static boolean evaluateDeviceFingerprintMismatchOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue)
        || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "MISMATCH".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  public static boolean evaluateSessionDurationLtOp(Object fieldValue, RuleCondition condition) {
    try {
      double duration = Double.parseDouble(String.valueOf(fieldValue));
      double threshold = Double.parseDouble(condition.getValueSingle());
      return duration < threshold;
    } catch (Exception e) {
      return false;
    }
  }

  public static boolean evaluateClickVelocityGtOp(Object fieldValue, RuleCondition condition) {
    try {
      double velocity = Double.parseDouble(String.valueOf(fieldValue));
      double threshold = Double.parseDouble(condition.getValueSingle());
      return velocity > threshold;
    } catch (Exception e) {
      return false;
    }
  }

  public static boolean evaluateMouseMovementAnomalyOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue)
        || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "ANOMALY".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  public static boolean evaluateTypingSpeedAnomalyOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue)
        || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "ANOMALY".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  public static boolean evaluateUserAgentSuspiciousOp(Object fieldValue) {
    if (fieldValue == null) return false;
    String userAgent = String.valueOf(fieldValue).toLowerCase();
    return userAgent.contains("bot")
        || userAgent.contains("crawler")
        || userAgent.contains("spider")
        || userAgent.contains("headless")
        || userAgent.contains("phantom");
  }

  public static boolean evaluateExpiredCardOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue)
        || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "EXPIRED".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  public static boolean evaluateCardCaptureFraudOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  public static boolean evaluatePinCvvLimitExceededOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue)
        || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "EXCEEDED".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  public static boolean evaluateOfflinePinFailedOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue)
        || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "FAILED".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  public static boolean evaluateEmvSecurityCheckOp(Object fieldValue) {
    return Boolean.FALSE.equals(fieldValue)
        || "false".equalsIgnoreCase(String.valueOf(fieldValue))
        || "FAILED".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  public static boolean evaluateEcommerceNoAvsOp(Object fieldValue) {
    return fieldValue == null
        || Boolean.TRUE.equals(fieldValue)
        || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "NO_AVS".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  public static boolean evaluatePosSecurityMissingOp(Object fieldValue) {
    return fieldValue == null
        || Boolean.TRUE.equals(fieldValue)
        || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "MISSING".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  public static boolean evaluateTerminalVerificationFailedOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue)
        || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "FAILED".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  public static boolean evaluateSuspiciousTerminalOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue)
        || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "SUSPICIOUS".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  public static boolean evaluateUnusualCardMediaOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue)
        || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "UNUSUAL".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  public static boolean evaluateTransferAmountGtOp(Object fieldValue, RuleCondition condition) {
    try {
      double amount = Double.parseDouble(String.valueOf(fieldValue));
      double threshold = Double.parseDouble(condition.getValueSingle());
      return amount > threshold;
    } catch (Exception e) {
      return false;
    }
  }

  public static boolean evaluateTransferVelocityGtOp(Object fieldValue, RuleCondition condition) {
    try {
      double velocity = Double.parseDouble(String.valueOf(fieldValue));
      double threshold = Double.parseDouble(condition.getValueSingle());
      return velocity > threshold;
    } catch (Exception e) {
      return false;
    }
  }

  public static boolean evaluateRecipientInWatchlistOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue)
        || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "WATCHLIST".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  public static boolean evaluateRecipientIsNewOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue)
        || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "NEW".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  public static boolean evaluateAddressMismatchOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue)
        || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "MISMATCH".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  public static boolean evaluatePhoneCountryMismatchOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue)
        || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "MISMATCH".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  public static boolean evaluateEmailDomainAgeLtDaysOp(Object fieldValue, RuleCondition condition) {
    try {
      double domainAgeDays = Double.parseDouble(String.valueOf(fieldValue));
      double threshold = Double.parseDouble(condition.getValueSingle());
      return domainAgeDays < threshold;
    } catch (Exception e) {
      return false;
    }
  }

  public static boolean evaluateAccountAgeLtDaysOp(Object fieldValue, RuleCondition condition) {
    try {
      double accountAgeDays = Double.parseDouble(String.valueOf(fieldValue));
      double threshold = Double.parseDouble(condition.getValueSingle());
      return accountAgeDays < threshold;
    } catch (Exception e) {
      return false;
    }
  }

  public static boolean evaluateContextOp(
      Object fieldValue, RuleCondition condition, EvaluationContext context) {
    if (condition.getValueSingle() == null || context.getPayload() == null) return false;
    Object contextValue = context.getPayload().get(condition.getValueSingle());
    return fieldValue != null && fieldValue.equals(contextValue);
  }

  public static boolean evaluateFraudOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue)
        || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "FRAUD".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  public static boolean evaluateSecurityOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  public static boolean evaluateSuspiciousOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue)
        || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "SUSPICIOUS".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  public static boolean evaluateSuspiciousTransactionTypeOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue)
        || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "SUSPICIOUS".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  public static boolean evaluateVelocityOp(Object fieldValue, RuleCondition condition) {
    try {
      double velocity = Double.parseDouble(String.valueOf(fieldValue));
      double threshold = Double.parseDouble(condition.getValueSingle());
      return velocity > threshold;
    } catch (Exception e) {
      return false;
    }
  }

  public static boolean evaluateRoundAmountOp(Object fieldValue) {
    try {
      double amount = Double.parseDouble(String.valueOf(fieldValue));
      return amount == Math.round(amount) && amount % 100 == 0;
    } catch (Exception e) {
      return false;
    }
  }

  public static boolean evaluateImpossibleTravelOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue)
        || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "IMPOSSIBLE".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  public static boolean evaluateNotInListOp(Object fieldValue, RuleCondition condition) {
    if (fieldValue == null) return true;
    if (condition.getValueArray() == null || condition.getValueArray().isEmpty()) return true;
    String fieldStr = String.valueOf(fieldValue);
    return !condition.getValueArray().contains(fieldStr);
  }

  public static boolean evaluateCaptchaFailedOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue)
        || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "FAILED".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  public static boolean evaluateCountDistinctCountriesLastNDaysOp(
      RuleCondition condition, EvaluationContext context) {
    try {
      Object fieldValue =
          FieldValueExtractor.getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
      int count = Integer.parseInt(String.valueOf(fieldValue));
      int threshold = Integer.parseInt(condition.getValueSingle());
      return count > threshold;
    } catch (Exception e) {
      return false;
    }
  }
}
