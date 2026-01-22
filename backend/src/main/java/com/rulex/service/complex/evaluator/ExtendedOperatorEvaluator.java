package com.rulex.service.complex.evaluator;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Map;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Avaliador estendido para operadores diversos. Cobre velocidade, tempo, campos, tipos de dados e
 * outros.
 */
@Component
@Slf4j
public class ExtendedOperatorEvaluator implements OperatorEvaluator {

  private static final Set<ConditionOperator> SUPPORTED =
      Set.of(
          // Velocidade estendida
          ConditionOperator.ADDRESS_CHANGE_VELOCITY,
          ConditionOperator.BENEFICIARY_ADD_VELOCITY,
          ConditionOperator.CARD_ADD_VELOCITY,
          ConditionOperator.CROSS_BORDER_VELOCITY,
          ConditionOperator.CVV_FAILURE_VELOCITY,
          ConditionOperator.DORMANCY_ALERT_VELOCITY,
          ConditionOperator.MCC_CATEGORY_VELOCITY,
          ConditionOperator.MERCHANT_VELOCITY_SPIKE,
          ConditionOperator.VELOCITY_ACCELERATION,
          ConditionOperator.VELOCITY_CROSS_CHANNEL,
          ConditionOperator.VELOCITY_PERCENTILE,
          ConditionOperator.VELOCITY_RATIO_GT,
          ConditionOperator.VELOCITY_ROLLING_WINDOW,
          ConditionOperator.VELOCITY_SPIKE,
          ConditionOperator.VELOCITY_TREND,

          // Tempo
          ConditionOperator.AVG_INTERVAL_BETWEEN_TXN,
          ConditionOperator.EXPIRES_WITHIN_DAYS,
          ConditionOperator.HOLIDAY_TRANSACTION_SPIKE,
          ConditionOperator.NIGHTTIME_TRANSACTION_RATIO,
          ConditionOperator.TIME_BETWEEN_CONSECUTIVE_TX,
          ConditionOperator.TIME_DEVIATION_FROM_USUAL,
          ConditionOperator.TIME_OF_DAY_ANOMALY,
          ConditionOperator.TIME_SINCE_LAST_LT,
          ConditionOperator.TIMEZONE_MISMATCH,
          ConditionOperator.WEEKEND_VS_WEEKDAY_PATTERN,
          ConditionOperator.GT_CURRENT_DATE,
          ConditionOperator.LT_CURRENT_DATE,

          // Compliance
          ConditionOperator.PACS008_FIELD_VALIDATION,

          // Campos
          ConditionOperator.FIELD_EQ,
          ConditionOperator.FIELD_GT,
          ConditionOperator.FIELD_GTE,
          ConditionOperator.FIELD_LT,
          ConditionOperator.FIELD_LTE,
          ConditionOperator.FIELD_NEQ,
          ConditionOperator.GT_FIELD_MULTIPLIER,
          ConditionOperator.PERCENTAGE_OF_FIELD,

          // Beneficiário
          ConditionOperator.BENEFICIARY_CONCENTRATION,
          ConditionOperator.BENEFICIARY_REUSE_PATTERN,
          ConditionOperator.RECIPIENT_DIVERSITY_CHANGE,

          // Limites
          ConditionOperator.DAILY_LIMIT_PROXIMITY,
          ConditionOperator.WEEKLY_LIMIT_PROXIMITY,
          ConditionOperator.MAX_AMOUNT_LAST_N_DAYS,
          ConditionOperator.MIN_AMOUNT_LAST_N_DAYS,
          ConditionOperator.SUM_BY_CHANNEL_LAST_N_DAYS,
          ConditionOperator.SUM_LAST_N_HOURS,

          // MCC
          ConditionOperator.MCC_CROSS_CATEGORY_PATTERN,
          ConditionOperator.MCC_CRYPTO,
          ConditionOperator.MCC_GAMBLING,
          ConditionOperator.MCC_HIGH_RISK,
          ConditionOperator.MCC_SPENDING_LIMIT_CHECK,

          // Outros
          ConditionOperator.AVG_TRANSACTION_SPIKE,
          ConditionOperator.CRYPTO_PUMP_DUMP_DETECTION,
          ConditionOperator.DISTANCE_FROM_LAST_GT,
          ConditionOperator.DORMANCY_REVIVAL,
          ConditionOperator.EMAIL_DOMAIN_AGE,
          ConditionOperator.EMAIL_PHONE_MISMATCH,
          ConditionOperator.ENTROPY_SCORE_ANOMALY,
          ConditionOperator.FAN_IN_COUNT,
          ConditionOperator.FAN_OUT_COUNT,
          ConditionOperator.INJECTION_ATTACK_DETECTION,
          ConditionOperator.INTEGRATION_PATTERN,
          ConditionOperator.IN_CUSTOMER_CHARGEBACK_MERCHANTS,
          ConditionOperator.IN_CUSTOMER_HISTORY,
          ConditionOperator.IN_CUSTOMER_USUAL_HOURS,
          ConditionOperator.LANGUAGE_MISMATCH,
          ConditionOperator.LOCATION_DEVIATION,
          ConditionOperator.MERCHANT_CATEGORY_CHANGE,
          ConditionOperator.MERCHANT_COUNTRY_MISMATCH,
          ConditionOperator.PAYMENT_METHOD_SWITCH,
          ConditionOperator.PEER_GROUP_DEVIATION_SCORE,
          ConditionOperator.PHONE_CARRIER_CHECK,
          ConditionOperator.RAPID_SUCCESSION_PATTERN,
          ConditionOperator.REAL_TIME_RISK_SCORING,
          ConditionOperator.ROUND_TRIP_DETECTION,
          ConditionOperator.SEGMENT_OF_ONE_PROFILING,
          ConditionOperator.SHARED_IP_COUNT,
          ConditionOperator.UNIQUE_CARD_COUNT_PER_IP_HOUR,
          ConditionOperator.UNIQUE_MERCHANT_COUNT_PER_CARD_DAY);

  @Override
  public Set<ConditionOperator> getSupportedOperators() {
    return SUPPORTED;
  }

  @Override
  public boolean evaluate(RuleCondition condition, EvaluationContext context) {
    ConditionOperator op = condition.getOperator();
    log.debug("ExtendedOperatorEvaluator: op={}", op);

    // Velocidade
    if (isVelocityOperator(op)) {
      return evaluateVelocityOperator(op, condition, context);
    }

    // Tempo
    if (isTimeOperator(op)) {
      return evaluateTimeOperator(op, condition, context);
    }

    // Campos
    if (isFieldOperator(op)) {
      return evaluateFieldOperator(op, condition, context);
    }

    // Outros operadores específicos
    return evaluateSpecificOperator(op, condition, context);
  }

  private boolean isVelocityOperator(ConditionOperator op) {
    return op.name().contains("VELOCITY")
        || op == ConditionOperator.ADDRESS_CHANGE_VELOCITY
        || op == ConditionOperator.BENEFICIARY_ADD_VELOCITY
        || op == ConditionOperator.CARD_ADD_VELOCITY
        || op == ConditionOperator.CVV_FAILURE_VELOCITY
        || op == ConditionOperator.DORMANCY_ALERT_VELOCITY;
  }

  private boolean isTimeOperator(ConditionOperator op) {
    return op.name().startsWith("TIME_")
        || op == ConditionOperator.AVG_INTERVAL_BETWEEN_TXN
        || op == ConditionOperator.EXPIRES_WITHIN_DAYS
        || op == ConditionOperator.HOLIDAY_TRANSACTION_SPIKE
        || op == ConditionOperator.NIGHTTIME_TRANSACTION_RATIO
        || op == ConditionOperator.WEEKEND_VS_WEEKDAY_PATTERN
        || op == ConditionOperator.TIMEZONE_MISMATCH;
  }

  private boolean isFieldOperator(ConditionOperator op) {
    return op.name().startsWith("FIELD_")
        || op == ConditionOperator.GT_FIELD_MULTIPLIER
        || op == ConditionOperator.PERCENTAGE_OF_FIELD;
  }

  private boolean evaluateVelocityOperator(
      ConditionOperator op, RuleCondition condition, EvaluationContext context) {
    Map<String, Object> payload = context.getPayload();
    if (payload == null) return false;

    String key = camelCase(op.name());
    Object value = payload.get(key);
    if (value == null) {
      value = payload.get(op.name().toLowerCase());
    }

    if (value == null) return false;

    if (value instanceof Boolean) {
      return (Boolean) value;
    }

    BigDecimal v = parseBigDecimal(String.valueOf(value), BigDecimal.ZERO);
    BigDecimal threshold = parseBigDecimal(condition.getValueSingle(), BigDecimal.ONE);
    return v.compareTo(threshold) > 0;
  }

  private boolean evaluateTimeOperator(
      ConditionOperator op, RuleCondition condition, EvaluationContext context) {
    Map<String, Object> payload = context.getPayload();
    if (payload == null) return false;

    return switch (op) {
      case EXPIRES_WITHIN_DAYS -> {
        Object expiry = payload.get("expiryDate");
        if (expiry == null) yield false;
        LocalDate exp = parseDate(expiry);
        int days = parseIntSafe(condition.getValueSingle(), 30);
        yield exp != null && exp.isBefore(LocalDate.now().plusDays(days));
      }
      case TIME_SINCE_LAST_LT -> {
        Object minutes = payload.get("timeSinceLastTx");
        if (minutes == null) yield false;
        int m = parseIntSafe(String.valueOf(minutes), Integer.MAX_VALUE);
        int threshold = parseIntSafe(condition.getValueSingle(), 5);
        yield m < threshold;
      }
      case NIGHTTIME_TRANSACTION_RATIO -> {
        Object ratio = payload.get("nighttimeRatio");
        if (ratio == null) yield false;
        BigDecimal r = parseBigDecimal(String.valueOf(ratio), BigDecimal.ZERO);
        BigDecimal threshold = parseBigDecimal(condition.getValueSingle(), new BigDecimal("30"));
        yield r.compareTo(threshold) > 0;
      }
      case WEEKEND_VS_WEEKDAY_PATTERN -> evaluateBoolean(payload, "weekendWeekdayAnomaly");
      case TIMEZONE_MISMATCH -> evaluateBoolean(payload, "timezoneMismatch");
      case TIME_BETWEEN_CONSECUTIVE_TX -> {
        Object time = payload.get("timeBetweenConsecutiveTx");
        if (time == null) yield false;
        int t = parseIntSafe(String.valueOf(time), Integer.MAX_VALUE);
        int threshold = parseIntSafe(condition.getValueSingle(), 60);
        yield t < threshold;
      }
      case TIME_DEVIATION_FROM_USUAL -> evaluateBoolean(payload, "timeDeviationFromUsual");
      case TIME_OF_DAY_ANOMALY -> evaluateBoolean(payload, "timeOfDayAnomaly");
      case AVG_INTERVAL_BETWEEN_TXN -> {
        Object avg = payload.get("avgIntervalBetweenTxn");
        if (avg == null) yield false;
        int a = parseIntSafe(String.valueOf(avg), Integer.MAX_VALUE);
        int threshold = parseIntSafe(condition.getValueSingle(), 60);
        yield a < threshold;
      }
      case HOLIDAY_TRANSACTION_SPIKE -> evaluateBoolean(payload, "holidayTransactionSpike");
      case GT_CURRENT_DATE -> {
        Object date = payload.get(condition.getFieldName());
        if (date == null) yield false;
        LocalDate d = parseDate(date);
        yield d != null && d.isAfter(LocalDate.now());
      }
      case LT_CURRENT_DATE -> {
        Object date = payload.get(condition.getFieldName());
        if (date == null) yield false;
        LocalDate d = parseDate(date);
        yield d != null && d.isBefore(LocalDate.now());
      }
      case PACS008_FIELD_VALIDATION -> evaluateBoolean(payload, "pacs008FieldValid");
      default -> evaluateBoolean(payload, camelCase(op.name()));
    };
  }

  private boolean evaluateFieldOperator(
      ConditionOperator op, RuleCondition condition, EvaluationContext context) {
    Map<String, Object> payload = context.getPayload();
    if (payload == null) return false;

    String field1 = condition.getFieldName();
    String field2 = condition.getValueSingle();

    Object value1 = payload.get(field1);
    Object value2 = payload.get(field2);

    if (value1 == null || value2 == null) return false;

    BigDecimal v1 = parseBigDecimal(String.valueOf(value1), BigDecimal.ZERO);
    BigDecimal v2 = parseBigDecimal(String.valueOf(value2), BigDecimal.ZERO);

    return switch (op) {
      case FIELD_EQ -> v1.compareTo(v2) == 0;
      case FIELD_NEQ -> v1.compareTo(v2) != 0;
      case FIELD_GT -> v1.compareTo(v2) > 0;
      case FIELD_GTE -> v1.compareTo(v2) >= 0;
      case FIELD_LT -> v1.compareTo(v2) < 0;
      case FIELD_LTE -> v1.compareTo(v2) <= 0;
      case GT_FIELD_MULTIPLIER -> {
        BigDecimal multiplier = parseBigDecimal(condition.getValueMax(), BigDecimal.ONE);
        yield v1.compareTo(v2.multiply(multiplier)) > 0;
      }
      case PERCENTAGE_OF_FIELD -> {
        if (v2.compareTo(BigDecimal.ZERO) == 0) yield false;
        BigDecimal pct =
            v1.divide(v2, 4, java.math.RoundingMode.HALF_UP).multiply(new BigDecimal("100"));
        BigDecimal threshold = parseBigDecimal(condition.getValueMax(), new BigDecimal("50"));
        yield pct.compareTo(threshold) > 0;
      }
      default -> false;
    };
  }

  private boolean evaluateSpecificOperator(
      ConditionOperator op, RuleCondition condition, EvaluationContext context) {
    Map<String, Object> payload = context.getPayload();
    if (payload == null) return false;

    return switch (op) {
      case BENEFICIARY_CONCENTRATION ->
          evaluateThreshold(payload, "beneficiaryConcentration", condition, 80);
      case BENEFICIARY_REUSE_PATTERN -> evaluateBoolean(payload, "beneficiaryReusePattern");
      case RECIPIENT_DIVERSITY_CHANGE -> evaluateBoolean(payload, "recipientDiversityChange");
      case DAILY_LIMIT_PROXIMITY ->
          evaluateThreshold(payload, "dailyLimitProximity", condition, 90);
      case WEEKLY_LIMIT_PROXIMITY ->
          evaluateThreshold(payload, "weeklyLimitProximity", condition, 90);
      case MAX_AMOUNT_LAST_N_DAYS -> evaluateThreshold(payload, "maxAmountLastNDays", condition, 0);
      case MIN_AMOUNT_LAST_N_DAYS -> evaluateThreshold(payload, "minAmountLastNDays", condition, 0);
      case SUM_BY_CHANNEL_LAST_N_DAYS ->
          evaluateThreshold(payload, "sumByChannelLastNDays", condition, 0);
      case SUM_LAST_N_HOURS -> evaluateThreshold(payload, "sumLastNHours", condition, 0);
      case MCC_CRYPTO -> evaluateBoolean(payload, "mccCrypto");
      case MCC_GAMBLING -> evaluateBoolean(payload, "mccGambling");
      case MCC_HIGH_RISK -> evaluateBoolean(payload, "mccHighRisk");
      case MCC_CROSS_CATEGORY_PATTERN -> evaluateBoolean(payload, "mccCrossCategoryPattern");
      case MCC_SPENDING_LIMIT_CHECK -> evaluateBoolean(payload, "mccSpendingLimitExceeded");
      case CRYPTO_PUMP_DUMP_DETECTION -> evaluateBoolean(payload, "cryptoPumpDumpDetected");
      case DISTANCE_FROM_LAST_GT -> evaluateThreshold(payload, "distanceFromLast", condition, 100);
      case DORMANCY_REVIVAL -> evaluateBoolean(payload, "dormancyRevival");
      case EMAIL_DOMAIN_AGE -> evaluateThresholdLessThan(payload, "emailDomainAge", condition, 30);
      case EMAIL_PHONE_MISMATCH -> evaluateBoolean(payload, "emailPhoneMismatch");
      case ENTROPY_SCORE_ANOMALY -> evaluateBoolean(payload, "entropyScoreAnomaly");
      case FAN_IN_COUNT -> evaluateCountThreshold(payload, "fanInCount", condition, 10);
      case FAN_OUT_COUNT -> evaluateCountThreshold(payload, "fanOutCount", condition, 10);
      case INJECTION_ATTACK_DETECTION -> evaluateBoolean(payload, "injectionAttackDetected");
      case INTEGRATION_PATTERN -> evaluateBoolean(payload, "integrationPatternDetected");
      case IN_CUSTOMER_CHARGEBACK_MERCHANTS ->
          evaluateBoolean(payload, "inCustomerChargebackMerchants");
      case IN_CUSTOMER_HISTORY -> evaluateBoolean(payload, "inCustomerHistory");
      case IN_CUSTOMER_USUAL_HOURS -> evaluateBoolean(payload, "inCustomerUsualHours");
      case LANGUAGE_MISMATCH -> evaluateBoolean(payload, "languageMismatch");
      case LOCATION_DEVIATION -> evaluateBoolean(payload, "locationDeviation");
      case MERCHANT_CATEGORY_CHANGE -> evaluateBoolean(payload, "merchantCategoryChange");
      case MERCHANT_COUNTRY_MISMATCH -> evaluateBoolean(payload, "merchantCountryMismatch");
      case PAYMENT_METHOD_SWITCH -> evaluateBoolean(payload, "paymentMethodSwitch");
      case PEER_GROUP_DEVIATION_SCORE ->
          evaluateThreshold(payload, "peerGroupDeviationScore", condition, 2);
      case PHONE_CARRIER_CHECK -> evaluateBoolean(payload, "phoneCarrierSuspicious");
      case RAPID_SUCCESSION_PATTERN -> evaluateBoolean(payload, "rapidSuccessionPattern");
      case REAL_TIME_RISK_SCORING -> evaluateThreshold(payload, "realTimeRiskScore", condition, 70);
      case ROUND_TRIP_DETECTION -> evaluateBoolean(payload, "roundTripDetected");
      case SEGMENT_OF_ONE_PROFILING -> evaluateBoolean(payload, "segmentOfOneAnomaly");
      case SHARED_IP_COUNT -> evaluateCountThreshold(payload, "sharedIpCount", condition, 5);
      case UNIQUE_CARD_COUNT_PER_IP_HOUR ->
          evaluateCountThreshold(payload, "uniqueCardCountPerIpHour", condition, 10);
      case UNIQUE_MERCHANT_COUNT_PER_CARD_DAY ->
          evaluateCountThreshold(payload, "uniqueMerchantCountPerCardDay", condition, 20);
      case AVG_TRANSACTION_SPIKE -> evaluateBoolean(payload, "avgTransactionSpike");
      default -> evaluateBoolean(payload, camelCase(op.name()));
    };
  }

  private boolean evaluateBoolean(Map<String, Object> payload, String key) {
    Object value = payload.get(key);
    if (value == null) return false;
    return Boolean.parseBoolean(String.valueOf(value));
  }

  private boolean evaluateThreshold(
      Map<String, Object> payload, String key, RuleCondition condition, double defaultThreshold) {
    Object value = payload.get(key);
    if (value == null) return false;
    BigDecimal v = parseBigDecimal(String.valueOf(value), BigDecimal.ZERO);
    BigDecimal threshold =
        parseBigDecimal(condition.getValueSingle(), new BigDecimal(defaultThreshold));
    return v.compareTo(threshold) > 0;
  }

  private boolean evaluateThresholdLessThan(
      Map<String, Object> payload, String key, RuleCondition condition, int defaultThreshold) {
    Object value = payload.get(key);
    if (value == null) return false;
    int v = parseIntSafe(String.valueOf(value), Integer.MAX_VALUE);
    int threshold = parseIntSafe(condition.getValueSingle(), defaultThreshold);
    return v < threshold;
  }

  private boolean evaluateCountThreshold(
      Map<String, Object> payload, String key, RuleCondition condition, int defaultThreshold) {
    Object value = payload.get(key);
    if (value == null) return false;
    int count = parseIntSafe(String.valueOf(value), 0);
    int threshold = parseIntSafe(condition.getValueSingle(), defaultThreshold);
    return count > threshold;
  }

  private String camelCase(String snakeCase) {
    StringBuilder result = new StringBuilder();
    boolean capitalizeNext = false;
    for (char c : snakeCase.toLowerCase().toCharArray()) {
      if (c == '_') {
        capitalizeNext = true;
      } else if (capitalizeNext) {
        result.append(Character.toUpperCase(c));
        capitalizeNext = false;
      } else {
        result.append(c);
      }
    }
    return result.toString();
  }

  private LocalDate parseDate(Object value) {
    if (value instanceof LocalDate) return (LocalDate) value;
    if (value instanceof LocalDateTime) return ((LocalDateTime) value).toLocalDate();
    try {
      return LocalDate.parse(String.valueOf(value));
    } catch (Exception e) {
      return null;
    }
  }

  private int parseIntSafe(String value, int defaultValue) {
    try {
      return Integer.parseInt(value);
    } catch (Exception e) {
      return defaultValue;
    }
  }

  private BigDecimal parseBigDecimal(String value, BigDecimal defaultValue) {
    try {
      return new BigDecimal(value);
    } catch (Exception e) {
      return defaultValue;
    }
  }

  @Override
  public String getCategory() {
    return "EXTENDED";
  }
}
