package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.OperatorDataService;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import com.rulex.service.complex.context.FieldValueExtractor;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.OffsetDateTime;
import java.util.Optional;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class CriticalOperatorEvaluator {

  private CriticalOperatorEvaluator() {}

  public static boolean evaluateGtePercentOfLastIncoming(
      RuleCondition condition,
      EvaluationContext context,
      OperatorDataService operatorDataService) {
    try {
      int percentage = Integer.parseInt(condition.getValueSingle().trim());
      Object fieldValue = FieldValueExtractor.getFieldValue(condition.getFieldName(), null, context);

      if (fieldValue == null) {
        return false;
      }

      BigDecimal currentAmount = new BigDecimal(String.valueOf(fieldValue));

      Object customerIdObj = FieldValueExtractor.getFieldValue("customerIdFromHeader", null, context);
      if (customerIdObj == null) {
        customerIdObj = FieldValueExtractor.getFieldValue("customerId", null, context);
      }

      BigDecimal lastIncoming = BigDecimal.valueOf(1000);

      if (customerIdObj != null) {
        String customerId = customerIdObj.toString();
        Optional<BigDecimal> lastIncomingOpt = operatorDataService.getLastIncomingAmount(customerId);
        if (lastIncomingOpt.isPresent()) {
          lastIncoming = lastIncomingOpt.get();
        }
      }

      if (lastIncoming.compareTo(BigDecimal.ZERO) == 0) {
        return false;
      }

      BigDecimal threshold =
          lastIncoming
              .multiply(BigDecimal.valueOf(percentage))
              .divide(BigDecimal.valueOf(100), RoundingMode.HALF_UP);
      boolean result = currentAmount.compareTo(threshold) >= 0;

      log.debug(
          "GTE_PERCENT_OF_LAST_INCOMING: current={}, lastIncoming={}, percentage={}%, threshold={}, result={}",
          currentAmount,
          lastIncoming,
          percentage,
          threshold,
          result);
      return result;
    } catch (Exception e) {
      log.error("Erro ao avaliar GTE_PERCENT_OF_LAST_INCOMING: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateDomainInList(Object fieldValue, RuleCondition condition) {
    if (fieldValue == null || condition.getValueSingle() == null) {
      return false;
    }

    String email = String.valueOf(fieldValue).toLowerCase().trim();
    int atIndex = email.indexOf('@');
    if (atIndex < 0 || atIndex >= email.length() - 1) {
      return false;
    }

    String domain = email.substring(atIndex + 1);
    String[] blockedDomains = condition.getValueSingle().toLowerCase().split(",");

    for (String blocked : blockedDomains) {
      if (domain.equals(blocked.trim())) {
        return true;
      }
    }
    return false;
  }

  public static boolean evaluateChargebackRateGt(
      RuleCondition condition,
      EvaluationContext context,
      OperatorDataService operatorDataService) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      if (parts.length < 2) {
        log.warn("Formato inválido para CHARGEBACK_RATE_GT. Esperado: rate:days");
        return false;
      }

      double rateThreshold = Double.parseDouble(parts[0].trim());
      int days = Integer.parseInt(parts[1].trim());

      Object merchantIdObj = FieldValueExtractor.getFieldValue(condition.getFieldName(), null, context);
      if (merchantIdObj == null) {
        return false;
      }
      String merchantId = merchantIdObj.toString();

      BigDecimal threshold = BigDecimal.valueOf(rateThreshold / 100.0);
      boolean result = operatorDataService.hasHighChargebackRate(merchantId, threshold);

      log.debug(
          "CHARGEBACK_RATE_GT: merchantId={}, threshold={}%, days={}, result={}",
          merchantId,
          rateThreshold,
          days,
          result);
      return result;
    } catch (Exception e) {
      log.error("Erro ao avaliar CHARGEBACK_RATE_GT: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateAccountAgeLtMinutes(
      RuleCondition condition,
      EvaluationContext context,
      OperatorDataService operatorDataService) {
    try {
      int thresholdMinutes = Integer.parseInt(condition.getValueSingle().trim());

      Object customerIdObj = FieldValueExtractor.getFieldValue("customerIdFromHeader", null, context);
      if (customerIdObj == null) {
        customerIdObj = FieldValueExtractor.getFieldValue("customerId", null, context);
      }

      if (customerIdObj != null) {
        String customerId = customerIdObj.toString();
        long accountAgeMinutes = operatorDataService.getAccountAgeInMinutes(customerId);

        if (accountAgeMinutes >= 0) {
          boolean result = accountAgeMinutes < thresholdMinutes;
          log.debug(
              "ACCOUNT_AGE_LT_MINUTES: customerId={}, age={}min, threshold={}min, result={}",
              customerId,
              accountAgeMinutes,
              thresholdMinutes,
              result);
          return result;
        }
      }

      Object createdAt = FieldValueExtractor.getFieldValue("accountCreatedAt", null, context);
      if (createdAt == null) {
        createdAt = FieldValueExtractor.getFieldValue("customerCreatedAt", null, context);
      }

      if (createdAt != null) {
        OffsetDateTime createdDateTime;
        if (createdAt instanceof OffsetDateTime) {
          createdDateTime = (OffsetDateTime) createdAt;
        } else if (createdAt instanceof String) {
          createdDateTime = OffsetDateTime.parse((String) createdAt);
        } else {
          return false;
        }

        long ageMinutes =
            java.time.Duration.between(createdDateTime, OffsetDateTime.now()).toMinutes();
        boolean result = ageMinutes < thresholdMinutes;
        log.debug(
            "ACCOUNT_AGE_LT_MINUTES: age={}min, threshold={}min, result={}",
            ageMinutes,
            thresholdMinutes,
            result);
        return result;
      }

      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar ACCOUNT_AGE_LT_MINUTES: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateIsVoip(Object fieldValue, OperatorDataService operatorDataService) {
    if (fieldValue == null) {
      return false;
    }

    String phone = String.valueOf(fieldValue).replaceAll("[^0-9]", "");

    String countryCode = "55";
    if (phone.startsWith("55")) {
      phone = phone.substring(2);
    }

    if (operatorDataService.isVoipNumber(phone, countryCode)) {
      return true;
    }

    String[] voipPrefixes = {
      "0800", "0300", "0303", "0500", "0900", "4000", "4003", "4004", "4020", "4062"
    };
    for (String prefix : voipPrefixes) {
      if (phone.startsWith(prefix)) {
        return true;
      }
    }

    return false;
  }
}
