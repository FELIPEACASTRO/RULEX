package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.OperatorDataService;
import com.rulex.service.VelocityService;
import com.rulex.service.VelocityServiceFacade;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import com.rulex.service.complex.context.FieldValueExtractor;
import com.rulex.service.complex.parsing.NumericParser;
import com.rulex.service.complex.parsing.TimeWindowParser;
import java.math.BigDecimal;
import java.util.Map;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class VelocityAggregationEvaluator {

  private VelocityAggregationEvaluator() {}

  public static boolean evaluateSumLastNDays(
      RuleCondition condition, EvaluationContext context, VelocityServiceFacade velocityServiceFacade) {
    try {
      String[] parts = condition.getValueSingle().split("\\|");
      if (parts.length != 4) {
        log.error("Formato inválido para SUM_LAST_N_DAYS: {}", condition.getValueSingle());
        return false;
      }

      String fieldName = parts[0];
      int nDays = Integer.parseInt(parts[1]);
      BigDecimal threshold = new BigDecimal(parts[2]);
      String operator = parts[3];

      VelocityService.TimeWindow window = parseTimeWindowFromDays(nDays);
      VelocityService.KeyType keyType = VelocityService.KeyType.PAN;

      var stats = velocityServiceFacade.getStats(context.getTransactionRequest(), keyType, window);
      BigDecimal sum = stats.getTotalAmount();

      return switch (operator) {
        case "GT" -> sum.compareTo(threshold) > 0;
        case "GTE" -> sum.compareTo(threshold) >= 0;
        case "LT" -> sum.compareTo(threshold) < 0;
        case "LTE" -> sum.compareTo(threshold) <= 0;
        case "EQ" -> sum.compareTo(threshold) == 0;
        default -> false;
      };

    } catch (Exception e) {
      log.error("Erro ao avaliar SUM_LAST_N_DAYS: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateCountLastNHours(
      RuleCondition condition, EvaluationContext context, VelocityServiceFacade velocityServiceFacade) {
    try {
      String[] parts = condition.getValueSingle().split("\\|");
      if (parts.length != 3) {
        log.error("Formato inválido para COUNT_LAST_N_HOURS: {}", condition.getValueSingle());
        return false;
      }

      int nHours = Integer.parseInt(parts[0]);
      long threshold = Long.parseLong(parts[1]);
      String operator = parts[2];

      VelocityService.TimeWindow window = parseTimeWindowFromHours(nHours);
      VelocityService.KeyType keyType = VelocityService.KeyType.PAN;

      var stats = velocityServiceFacade.getStats(context.getTransactionRequest(), keyType, window);
      long count = stats.getTransactionCount();

      return switch (operator) {
        case "GT" -> count > threshold;
        case "GTE" -> count >= threshold;
        case "LT" -> count < threshold;
        case "LTE" -> count <= threshold;
        case "EQ" -> count == threshold;
        default -> false;
      };

    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_LAST_N_HOURS: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateAvgLastNDays(
      RuleCondition condition, EvaluationContext context, VelocityServiceFacade velocityServiceFacade) {
    try {
      String[] parts = condition.getValueSingle().split("\\|");
      if (parts.length != 4) {
        log.error("Formato inválido para AVG_LAST_N_DAYS: {}", condition.getValueSingle());
        return false;
      }

      String fieldName = parts[0];
      int nDays = Integer.parseInt(parts[1]);
      BigDecimal threshold = new BigDecimal(parts[2]);
      String operator = parts[3];

      VelocityService.TimeWindow window = parseTimeWindowFromDays(nDays);
      VelocityService.KeyType keyType = VelocityService.KeyType.PAN;

      var stats = velocityServiceFacade.getStats(context.getTransactionRequest(), keyType, window);
      BigDecimal avg = stats.getAvgAmount();

      return switch (operator) {
        case "GT" -> avg.compareTo(threshold) > 0;
        case "GTE" -> avg.compareTo(threshold) >= 0;
        case "LT" -> avg.compareTo(threshold) < 0;
        case "LTE" -> avg.compareTo(threshold) <= 0;
        case "EQ" -> avg.compareTo(threshold) == 0;
        default -> false;
      };

    } catch (Exception e) {
      log.error("Erro ao avaliar AVG_LAST_N_DAYS: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateCountDistinctMerchantsLastNDays(
      RuleCondition condition, EvaluationContext context, VelocityServiceFacade velocityServiceFacade) {
    try {
      String[] parts = condition.getValueSingle().split("\\|");
      if (parts.length != 3) {
        log.error(
            "Formato inválido para COUNT_DISTINCT_MERCHANTS_LAST_N_DAYS: {}",
            condition.getValueSingle());
        return false;
      }

      int nDays = Integer.parseInt(parts[0]);
      int threshold = Integer.parseInt(parts[1]);
      String operator = parts[2];

      VelocityService.TimeWindow window = parseTimeWindowFromDays(nDays);
      VelocityService.KeyType keyType = VelocityService.KeyType.PAN;

      var stats = velocityServiceFacade.getStats(context.getTransactionRequest(), keyType, window);
      long distinctMerchants = stats.getDistinctMerchants();

      return switch (operator) {
        case "GT" -> distinctMerchants > threshold;
        case "GTE" -> distinctMerchants >= threshold;
        case "LT" -> distinctMerchants < threshold;
        case "LTE" -> distinctMerchants <= threshold;
        case "EQ" -> distinctMerchants == threshold;
        default -> false;
      };

    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_DISTINCT_MERCHANTS_LAST_N_DAYS: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateCountDistinctCountriesLastNHours(
      RuleCondition condition, EvaluationContext context, VelocityServiceFacade velocityServiceFacade) {
    try {
      String[] parts = condition.getValueSingle().split("\\|");
      if (parts.length != 3) {
        log.error(
            "Formato inválido para COUNT_DISTINCT_COUNTRIES_LAST_N_HOURS: {}",
            condition.getValueSingle());
        return false;
      }

      int nHours = Integer.parseInt(parts[0]);
      int threshold = Integer.parseInt(parts[1]);
      String operator = parts[2];

      VelocityService.TimeWindow window = parseTimeWindowFromHours(nHours);
      VelocityService.KeyType keyType = VelocityService.KeyType.PAN;

      var stats = velocityServiceFacade.getStats(context.getTransactionRequest(), keyType, window);
      long distinctCountries = stats.getDistinctCountries();

      return switch (operator) {
        case "GT" -> distinctCountries > threshold;
        case "GTE" -> distinctCountries >= threshold;
        case "LT" -> distinctCountries < threshold;
        case "LTE" -> distinctCountries <= threshold;
        case "EQ" -> distinctCountries == threshold;
        default -> false;
      };

    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_DISTINCT_COUNTRIES_LAST_N_HOURS: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateMaxAmountLastNDays(
      RuleCondition condition, EvaluationContext context, VelocityServiceFacade velocityServiceFacade) {
    try {
      String[] parts = condition.getValueSingle().split("\\|");
      if (parts.length != 3) {
        log.error("Formato inválido para MAX_AMOUNT_LAST_N_DAYS: {}", condition.getValueSingle());
        return false;
      }

      int nDays = Integer.parseInt(parts[0]);
      BigDecimal threshold = new BigDecimal(parts[1]);
      String operator = parts[2];

      VelocityService.TimeWindow window = parseTimeWindowFromDays(nDays);
      VelocityService.KeyType keyType = VelocityService.KeyType.PAN;

      var stats = velocityServiceFacade.getStats(context.getTransactionRequest(), keyType, window);
      BigDecimal maxAmount = stats.getMaxAmount();

      return switch (operator) {
        case "GT" -> maxAmount.compareTo(threshold) > 0;
        case "GTE" -> maxAmount.compareTo(threshold) >= 0;
        case "LT" -> maxAmount.compareTo(threshold) < 0;
        case "LTE" -> maxAmount.compareTo(threshold) <= 0;
        case "EQ" -> maxAmount.compareTo(threshold) == 0;
        default -> false;
      };

    } catch (Exception e) {
      log.error("Erro ao avaliar MAX_AMOUNT_LAST_N_DAYS: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateMinAmountLastNDays(
      RuleCondition condition, EvaluationContext context, VelocityServiceFacade velocityServiceFacade) {
    try {
      String[] parts = condition.getValueSingle().split("\\|");
      if (parts.length != 3) {
        log.error("Formato inválido para MIN_AMOUNT_LAST_N_DAYS: {}", condition.getValueSingle());
        return false;
      }

      int nDays = Integer.parseInt(parts[0]);
      BigDecimal threshold = new BigDecimal(parts[1]);
      String operator = parts[2];

      VelocityService.TimeWindow window = parseTimeWindowFromDays(nDays);
      VelocityService.KeyType keyType = VelocityService.KeyType.PAN;

      var stats = velocityServiceFacade.getStats(context.getTransactionRequest(), keyType, window);
      BigDecimal minAmount = stats.getMinAmount();

      return switch (operator) {
        case "GT" -> minAmount.compareTo(threshold) > 0;
        case "GTE" -> minAmount.compareTo(threshold) >= 0;
        case "LT" -> minAmount.compareTo(threshold) < 0;
        case "LTE" -> minAmount.compareTo(threshold) <= 0;
        case "EQ" -> minAmount.compareTo(threshold) == 0;
        default -> false;
      };

    } catch (Exception e) {
      log.error("Erro ao avaliar MIN_AMOUNT_LAST_N_DAYS: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateSumLastNHours(
      RuleCondition condition, EvaluationContext context, VelocityServiceFacade velocityServiceFacade) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      if (parts.length < 2) {
        log.error("Formato inválido para SUM_LAST_N_HOURS: {}", condition.getValueSingle());
        return false;
      }

      int hours = Integer.parseInt(parts[0].trim());
      BigDecimal threshold = new BigDecimal(parts[1].trim());
      String operator = parts.length > 2 ? parts[2].trim() : "GT";

      VelocityService.TimeWindow window = parseTimeWindowFromHours(hours);

      if (context.getTransactionRequest() != null) {
        VelocityService.VelocityStats stats =
            velocityServiceFacade.getStats(
                context.getTransactionRequest(), VelocityService.KeyType.PAN, window);

        return switch (operator) {
          case "GT" -> stats.getTotalAmount().compareTo(threshold) > 0;
          case "GTE" -> stats.getTotalAmount().compareTo(threshold) >= 0;
          case "LT" -> stats.getTotalAmount().compareTo(threshold) < 0;
          case "LTE" -> stats.getTotalAmount().compareTo(threshold) <= 0;
          default -> false;
        };
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar SUM_LAST_N_HOURS: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateCountFailuresLastNHours(
      RuleCondition condition,
      EvaluationContext context,
      OperatorDataService operatorDataService,
      VelocityServiceFacade velocityServiceFacade) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      if (parts.length < 2) {
        return false;
      }

      int hours = Integer.parseInt(parts[0].trim());
      int threshold = Integer.parseInt(parts[1].trim());

      Object customerIdObj = FieldValueExtractor.getFieldValue("customerIdFromHeader", null, context);
      if (customerIdObj == null) {
        customerIdObj = FieldValueExtractor.getFieldValue("customerId", null, context);
      }

      if (customerIdObj != null) {
        String customerId = customerIdObj.toString();
        long failureCount = operatorDataService.countAuthFailuresLastNHours(customerId, hours);
        boolean result = failureCount > threshold;
        log.debug(
            "COUNT_FAILURES_LAST_N_HOURS: customerId={}, hours={}, failures={}, threshold={}, result={}",
            customerId,
            hours,
            failureCount,
            threshold,
            result);
        return result;
      }

      VelocityService.TimeWindow window = parseTimeWindowFromHours(hours);
      if (context.getTransactionRequest() != null) {
        VelocityService.VelocityStats stats =
            velocityServiceFacade.getStats(
                context.getTransactionRequest(), VelocityService.KeyType.PAN, window);
        return stats.getFraudCount() > threshold;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_FAILURES_LAST_N_HOURS: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateCountDistinctMerchantsLastNHours(
      RuleCondition condition, EvaluationContext context, VelocityServiceFacade velocityServiceFacade) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      if (parts.length < 2) {
        return false;
      }

      int hours = Integer.parseInt(parts[0].trim());
      int threshold = Integer.parseInt(parts[1].trim());

      VelocityService.TimeWindow window = parseTimeWindowFromHours(hours);

      if (context.getTransactionRequest() != null) {
        VelocityService.VelocityStats stats =
            velocityServiceFacade.getStats(
                context.getTransactionRequest(), VelocityService.KeyType.PAN, window);
        return stats.getDistinctMerchants() > threshold;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_DISTINCT_MERCHANTS_LAST_N_HOURS: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateTimeSinceLastLt(
      RuleCondition condition,
      EvaluationContext context,
      OperatorDataService operatorDataService,
      VelocityServiceFacade velocityServiceFacade) {
    try {
      int thresholdMinutes = Integer.parseInt(condition.getValueSingle().trim());

      Object customerIdObj = FieldValueExtractor.getFieldValue("customerIdFromHeader", null, context);
      if (customerIdObj == null) {
        customerIdObj = FieldValueExtractor.getFieldValue("customerId", null, context);
      }

      if (customerIdObj != null) {
        String customerId = customerIdObj.toString();
        long minutesSinceLast = operatorDataService.getMinutesSinceLastTransaction(customerId);

        if (minutesSinceLast >= 0) {
          boolean result = minutesSinceLast < thresholdMinutes;
          log.debug(
              "TIME_SINCE_LAST_LT: customerId={}, minutesSinceLast={}, threshold={}, result={}",
              customerId,
              minutesSinceLast,
              thresholdMinutes,
              result);
          return result;
        }
      }

      VelocityService.TimeWindow window = parseTimeWindowFromHours(1);
      if (context.getTransactionRequest() != null) {
        VelocityService.VelocityStats stats =
            velocityServiceFacade.getStats(
                context.getTransactionRequest(), VelocityService.KeyType.PAN, window);
        return stats.getTransactionCount() > 1;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar TIME_SINCE_LAST_LT: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateVelocitySpike(
      RuleCondition condition, EvaluationContext context, VelocityServiceFacade velocityServiceFacade) {
    try {
      double multiplier = Double.parseDouble(condition.getValueSingle().trim());

      if (context.getTransactionRequest() != null) {
        VelocityService.VelocityStats statsHour =
            velocityServiceFacade.getStats(
                context.getTransactionRequest(),
                VelocityService.KeyType.PAN,
                VelocityService.TimeWindow.HOUR_1);

        VelocityService.VelocityStats statsDay =
            velocityServiceFacade.getStats(
                context.getTransactionRequest(),
                VelocityService.KeyType.PAN,
                VelocityService.TimeWindow.HOUR_24);

        double avgPerHour = statsDay.getTransactionCount() / 24.0;
        double currentHour = statsHour.getTransactionCount();

        return avgPerHour > 0 && currentHour > (avgPerHour * multiplier);
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar VELOCITY_SPIKE: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateAmountSpike(
      RuleCondition condition, EvaluationContext context, VelocityServiceFacade velocityServiceFacade) {
    try {
      double multiplier = Double.parseDouble(condition.getValueSingle().trim());

      Object amountValue = FieldValueExtractor.getFieldValue("transactionAmount", null, context);
      if (amountValue == null) {
        return false;
      }

      BigDecimal currentAmount = new BigDecimal(String.valueOf(amountValue));

      if (context.getTransactionRequest() != null) {
        VelocityService.VelocityStats stats =
            velocityServiceFacade.getStats(
                context.getTransactionRequest(),
                VelocityService.KeyType.PAN,
                VelocityService.TimeWindow.DAY_30);

        BigDecimal avgAmount = stats.getAvgAmount();
        if (avgAmount.compareTo(BigDecimal.ZERO) > 0) {
          BigDecimal threshold = avgAmount.multiply(BigDecimal.valueOf(multiplier));
          return currentAmount.compareTo(threshold) > 0;
        }
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar AMOUNT_SPIKE: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluatePatternEscalation(
      RuleCondition condition, EvaluationContext context, VelocityServiceFacade velocityServiceFacade) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      int minTransactions = parts.length > 0 ? Integer.parseInt(parts[0].trim()) : 3;
      double escalationFactor = parts.length > 1 ? Double.parseDouble(parts[1].trim()) : 1.5;

      if (context.getTransactionRequest() == null) {
        return false;
      }

      VelocityService.TimeWindow window = VelocityService.TimeWindow.HOUR_1;
      VelocityService.VelocityStats stats =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(), VelocityService.KeyType.PAN, window);

      if (stats.getTransactionCount() < minTransactions) {
        return false;
      }

      BigDecimal currentAmount = context.getTransactionRequest().getTransactionAmount();
      BigDecimal avgAmount = stats.getAvgAmount();

      if (avgAmount != null && avgAmount.compareTo(BigDecimal.ZERO) > 0) {
        double ratio = currentAmount.doubleValue() / avgAmount.doubleValue();
        boolean isEscalating = ratio >= escalationFactor;

        log.debug(
            "PATTERN_ESCALATION: txCount={}, currentAmount={}, avgAmount={}, ratio={}, factor={}, result={}",
            stats.getTransactionCount(),
            currentAmount,
            avgAmount,
            ratio,
            escalationFactor,
            isEscalating);
        return isEscalating;
      }

      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar PATTERN_ESCALATION: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluatePatternSplitTransaction(
      RuleCondition condition, EvaluationContext context, VelocityServiceFacade velocityServiceFacade) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      if (parts.length < 3) {
        log.warn(
            "Formato inválido para PATTERN_SPLIT_TRANSACTION. Esperado: maxMinutes:minTransactions:totalThreshold");
        return false;
      }

      int maxMinutes = Integer.parseInt(parts[0].trim());
      int minTransactions = Integer.parseInt(parts[1].trim());
      BigDecimal totalThreshold = new BigDecimal(parts[2].trim());

      if (context.getTransactionRequest() != null) {
        VelocityService.TimeWindow window = parseTimeWindowFromHours(maxMinutes / 60 + 1);
        VelocityService.VelocityStats stats =
            velocityServiceFacade.getStats(
                context.getTransactionRequest(), VelocityService.KeyType.PAN, window);

        return stats.getTransactionCount() >= minTransactions
            && stats.getTotalAmount().compareTo(totalThreshold) >= 0;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar PATTERN_SPLIT_TRANSACTION: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateCountLastNDays(
      RuleCondition condition, EvaluationContext context, VelocityServiceFacade velocityServiceFacade) {
    try {
      String valueSingle = condition.getValueSingle();
      if (valueSingle == null || valueSingle.isBlank()) {
        return false;
      }

      String[] parts = valueSingle.split("\\|");

      String keyType = "PAN";
      int threshold;
      int days;

      if (parts.length == 3) {
        keyType = parts[0].toUpperCase();
        threshold = parseIntSafe(parts[1], 10);
        days = parseIntSafe(parts[2], 30);
      } else if (parts.length == 2) {
        threshold = parseIntSafe(parts[0], 10);
        days = parseIntSafe(parts[1], 30);
      } else {
        threshold = parseIntSafe(parts[0], 10);
        days = 30;
      }

      if (context.getTransactionRequest() != null) {
        VelocityService.TimeWindow window = parseTimeWindowFromDays(days);
        VelocityService.KeyType kt = parseKeyType(keyType);
        VelocityService.VelocityStats stats =
            velocityServiceFacade.getStats(context.getTransactionRequest(), kt, window);
        return stats.getTransactionCount() >= threshold;
      }

      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object countObj = payload.get("velocity_count_" + days + "d");
      long count = countObj instanceof Number ? ((Number) countObj).longValue() : 0;
      return count >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_LAST_N_DAYS: {}", e.getMessage());
      return false;
    }
  }

  private static VelocityService.TimeWindow parseTimeWindowFromDays(int days) {
    return TimeWindowParser.fromDays(days);
  }

  private static VelocityService.TimeWindow parseTimeWindowFromHours(int hours) {
    return TimeWindowParser.fromHours(hours);
  }

  private static VelocityService.KeyType parseKeyType(String keyType) {
    try {
      return VelocityService.KeyType.valueOf(keyType.toUpperCase());
    } catch (IllegalArgumentException e) {
      return VelocityService.KeyType.PAN;
    }
  }

  private static int parseIntSafe(String value, int defaultValue) {
    return NumericParser.parseIntSafe(value, defaultValue);
  }
}
