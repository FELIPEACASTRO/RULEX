package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import com.rulex.service.complex.parsing.NumericParser;
import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class V28V30Evaluator {

  private V28V30Evaluator() {}

  public static boolean evaluateHasFailed3dsLastNMinutes(
      RuleCondition condition, EvaluationContext context) {
    try {
      int minutes = parseIntSafe(condition.getValueSingle(), 30);
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Boolean failed3ds = (Boolean) payload.get("auth_has_failed_3ds_recently");
      Integer lastFailureMinutes = (Integer) payload.get("auth_last_3ds_failure_minutes_ago");

      if (Boolean.TRUE.equals(failed3ds) && lastFailureMinutes != null) {
        return lastFailureMinutes <= minutes;
      }

      Boolean hasRecentFailure = (Boolean) payload.get("has_3ds_failure_" + minutes + "min");
      return Boolean.TRUE.equals(hasRecentFailure);
    } catch (Exception e) {
      log.error("Erro ao avaliar HAS_FAILED_3DS_LAST_N_MINUTES: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateCountMfaAbandonments(
      RuleCondition condition, EvaluationContext context) {
    try {
      String valueSingle = condition.getValueSingle();
      if (valueSingle == null || valueSingle.isBlank()) {
        return false;
      }

      String[] parts = valueSingle.split(":");
      int threshold = parseIntSafe(parts[0], 3);
      int hours = parts.length > 1 ? parseIntSafe(parts[1], 24) : 24;

      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Integer count = (Integer) payload.get("auth_mfa_abandonment_count_" + hours + "h");
      if (count == null) {
        count = (Integer) payload.get("mfa_abandonment_count");
      }

      return count != null && count >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_MFA_ABANDONMENTS: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateHasIncomingTransferLastNHours(
      RuleCondition condition, EvaluationContext context) {
    try {
      int hours = parseIntSafe(condition.getValueSingle(), 24);
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Boolean hasIncoming = (Boolean) payload.get("velocity_has_incoming_transfer_" + hours + "h");
      if (hasIncoming == null) {
        hasIncoming = (Boolean) payload.get("has_incoming_transfer_last_" + hours + "h");
      }

      return Boolean.TRUE.equals(hasIncoming);
    } catch (Exception e) {
      log.error("Erro ao avaliar HAS_INCOMING_TRANSFER_LAST_N_HOURS: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateIsImpossibleCombination(
      RuleCondition condition, EvaluationContext context) {
    try {
      String combinationType = condition.getValueSingle();
      if (combinationType == null || combinationType.isBlank()) {
        return false;
      }

      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      return switch (combinationType.toLowerCase().trim()) {
        case "age_corporate" -> {
          Object ageObj = payload.get("customer_age");
          String accountType = (String) payload.get("account_type");
          int age = ageObj instanceof Number ? ((Number) ageObj).intValue() : -1;
          yield age >= 0 && age < 18 && "CORPORATE".equalsIgnoreCase(accountType);
        }
        case "country_currency" -> {
          String country = (String) payload.get("merchantCountryCode");
          String currency = (String) payload.get("transactionCurrencyCode");
          yield !isValidCurrencyForCountry(country, currency);
        }
        case "deceased_active" -> {
          Boolean isDeceased = (Boolean) payload.get("customer_is_deceased");
          yield Boolean.TRUE.equals(isDeceased);
        }
        case "minor_high_value" -> {
          Object ageObj = payload.get("customer_age");
          Object amountObj = payload.get("transactionAmount");
          int age = ageObj instanceof Number ? ((Number) ageObj).intValue() : 99;
          BigDecimal amount = getBigDecimal(amountObj);
          yield age < 18 && amount != null && amount.compareTo(new BigDecimal("5000")) > 0;
        }
        default -> false;
      };
    } catch (Exception e) {
      log.error("Erro ao avaliar IS_IMPOSSIBLE_COMBINATION: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluatePixKeyChangedLastNDays(
      RuleCondition condition, EvaluationContext context) {
    try {
      int days = parseIntSafe(condition.getValueSingle(), 7);
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Integer daysSinceChange = (Integer) payload.get("customer_pix_key_changed_days_ago");
      if (daysSinceChange == null) {
        daysSinceChange = (Integer) payload.get("pix_key_age_days");
      }

      return daysSinceChange != null && daysSinceChange <= days;
    } catch (Exception e) {
      log.error("Erro ao avaliar PIX_KEY_CHANGED_LAST_N_DAYS: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateCountCryptoTxnLastNDays(
      RuleCondition condition, EvaluationContext context) {
    try {
      String valueSingle = condition.getValueSingle();
      if (valueSingle == null || valueSingle.isBlank()) {
        return false;
      }

      String[] parts = valueSingle.split("\\|");
      int threshold = parseIntSafe(parts[0], 5);
      int days = parts.length > 1 ? parseIntSafe(parts[1], 30) : 30;

      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object countObj = payload.get("velocity_crypto_txn_count_" + days + "d");
      if (countObj == null) {
        countObj = payload.get("crypto_transaction_count");
      }

      long count = countObj instanceof Number ? ((Number) countObj).longValue() : 0;
      return count >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_CRYPTO_TXN_LAST_N_DAYS: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateCountDistinctInstrumentsLastNDays(
      RuleCondition condition, EvaluationContext context) {
    try {
      String valueSingle = condition.getValueSingle();
      if (valueSingle == null || valueSingle.isBlank()) {
        return false;
      }

      String[] parts = valueSingle.split("\\|");
      int threshold = parseIntSafe(parts[0], 10);
      int days = parts.length > 1 ? parseIntSafe(parts[1], 30) : 30;

      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object countObj = payload.get("velocity_distinct_instruments_" + days + "d");
      if (countObj == null) {
        countObj = payload.get("distinct_payment_instruments");
      }

      long count = countObj instanceof Number ? ((Number) countObj).longValue() : 0;
      return count >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_DISTINCT_INSTRUMENTS_LAST_N_DAYS: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateCountDistinctPayersLastNDays(
      RuleCondition condition, EvaluationContext context) {
    try {
      String valueSingle = condition.getValueSingle();
      if (valueSingle == null || valueSingle.isBlank()) {
        return false;
      }

      String[] parts = valueSingle.split("\\|");
      int threshold = parseIntSafe(parts[0], 5);
      int days = parts.length > 1 ? parseIntSafe(parts[1], 7) : 7;

      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object countObj = payload.get("velocity_distinct_payers_" + days + "d");
      if (countObj == null) {
        countObj = payload.get("distinct_payers_count");
      }

      long count = countObj instanceof Number ? ((Number) countObj).longValue() : 0;
      return count >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_DISTINCT_PAYERS_LAST_N_DAYS: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateCountDistinctUserAgentsLastNHours(
      RuleCondition condition, EvaluationContext context) {
    try {
      String valueSingle = condition.getValueSingle();
      if (valueSingle == null || valueSingle.isBlank()) {
        return false;
      }

      String[] parts = valueSingle.split("\\|");
      int threshold = parseIntSafe(parts[0], 5);
      int hours = parts.length > 1 ? parseIntSafe(parts[1], 24) : 24;

      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object countObj = payload.get("device_distinct_user_agents_" + hours + "h");
      if (countObj == null) {
        countObj = payload.get("distinct_user_agents");
      }

      long count = countObj instanceof Number ? ((Number) countObj).longValue() : 0;
      return count >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_DISTINCT_USER_AGENTS_LAST_N_HOURS: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateCountMfaDenialsLastNHours(
      RuleCondition condition, EvaluationContext context) {
    try {
      String valueSingle = condition.getValueSingle();
      if (valueSingle == null || valueSingle.isBlank()) {
        return false;
      }

      String[] parts = valueSingle.split(":");
      int threshold = parseIntSafe(parts[0], 3);
      int hours = parts.length > 1 ? parseIntSafe(parts[1], 24) : 24;

      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Integer count = (Integer) payload.get("auth_mfa_denial_count_" + hours + "h");
      if (count == null) {
        count = (Integer) payload.get("mfa_denial_count");
      }

      return count != null && count >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_MFA_DENIALS_LAST_N_HOURS: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateDaysSinceLastActivity(
      RuleCondition condition, EvaluationContext context) {
    try {
      String valueSingle = condition.getValueSingle();
      if (valueSingle == null || valueSingle.isBlank()) {
        return false;
      }

      String[] parts = valueSingle.split("\\|");
      int threshold = parseIntSafe(parts[0], 30);
      String operator = parts.length > 1 ? parts[1].toUpperCase() : "GT";

      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object daysObj = payload.get("customer_days_since_last_activity");
      if (daysObj == null) {
        daysObj = payload.get("days_since_last_transaction");
      }

      if (daysObj == null) return false;
      int days = daysObj instanceof Number ? ((Number) daysObj).intValue() : -1;
      if (days < 0) return false;

      return switch (operator) {
        case "GT" -> days > threshold;
        case "GTE" -> days >= threshold;
        case "LT" -> days < threshold;
        case "LTE" -> days <= threshold;
        case "EQ" -> days == threshold;
        default -> days > threshold;
      };
    } catch (Exception e) {
      log.error("Erro ao avaliar DAYS_SINCE_LAST_ACTIVITY: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateDeviceChangedInSession(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Boolean changed = (Boolean) payload.get("device_changed_in_session");
      if (changed == null) {
        changed = (Boolean) payload.get("session_device_changed");
      }

      return Boolean.TRUE.equals(changed);
    } catch (Exception e) {
      log.error("Erro ao avaliar DEVICE_CHANGED_IN_SESSION: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateIsCryptoRansomAmount(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object amountObj = payload.get("transactionAmount");
      if (amountObj == null) {
        amountObj = payload.get("amount");
      }

      BigDecimal amount = getBigDecimal(amountObj);
      if (amount == null || amount.compareTo(BigDecimal.ZERO) <= 0) return false;

      List<BigDecimal> typicalRansomAmounts =
          List.of(
              new BigDecimal("500"),
              new BigDecimal("1000"),
              new BigDecimal("2500"),
              new BigDecimal("5000"),
              new BigDecimal("10000"),
              new BigDecimal("15000"),
              new BigDecimal("25000"),
              new BigDecimal("35000"),
              new BigDecimal("50000"),
              new BigDecimal("75000"),
              new BigDecimal("100000"),
              new BigDecimal("150000"),
              new BigDecimal("175000"),
              new BigDecimal("200000"),
              new BigDecimal("350000"),
              new BigDecimal("500000"));

      for (BigDecimal typical : typicalRansomAmounts) {
        BigDecimal lower = typical.multiply(new BigDecimal("0.9"));
        BigDecimal upper = typical.multiply(new BigDecimal("1.1"));
        if (amount.compareTo(lower) >= 0 && amount.compareTo(upper) <= 0) {
          return true;
        }
      }

      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar IS_CRYPTO_RANSOM_AMOUNT: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateOutflowRateLastNDays(
      RuleCondition condition, EvaluationContext context) {
    try {
      String valueSingle = condition.getValueSingle();
      if (valueSingle == null || valueSingle.isBlank()) {
        return false;
      }

      String[] parts = valueSingle.split("\\|");
      double threshold = parseDoubleSafe(parts[0], 80.0);
      int days = parts.length > 1 ? parseIntSafe(parts[1], 30) : 30;

      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object rateObj = payload.get("velocity_outflow_rate_" + days + "d");
      if (rateObj == null) {
        rateObj = payload.get("outflow_rate_percentage");
      }

      double rate = rateObj instanceof Number ? ((Number) rateObj).doubleValue() : 0.0;
      return rate >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar OUTFLOW_RATE_LAST_N_DAYS: {}", e.getMessage());
      return false;
    }
  }

  private static boolean isValidCurrencyForCountry(String country, String currency) {
    if (country == null || currency == null) return true;

    Map<String, List<String>> validCurrencies =
        Map.of(
            "BR", List.of("BRL"),
            "US", List.of("USD"),
            "GB", List.of("GBP"),
            "DE", List.of("EUR"),
            "FR", List.of("EUR"),
            "IT", List.of("EUR"),
            "ES", List.of("EUR"),
            "PT", List.of("EUR"),
            "JP", List.of("JPY"),
            "CN", List.of("CNY", "CNH"));

    List<String> valid = validCurrencies.get(country.toUpperCase());
    return valid == null || valid.contains(currency.toUpperCase());
  }

  private static BigDecimal getBigDecimal(Object value) {
    if (value == null) return null;
    if (value instanceof BigDecimal) return (BigDecimal) value;
    if (value instanceof Number) return BigDecimal.valueOf(((Number) value).doubleValue());
    try {
      return new BigDecimal(String.valueOf(value));
    } catch (NumberFormatException e) {
      return null;
    }
  }

  private static int parseIntSafe(String value, int defaultValue) {
    return NumericParser.parseIntSafe(value, defaultValue);
  }

  private static double parseDoubleSafe(String value, double defaultValue) {
    return NumericParser.parseDoubleSafe(value, defaultValue);
  }
}
