package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.OperatorDataService;
import com.rulex.service.VelocityService;
import com.rulex.service.VelocityServiceFacade;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import com.rulex.service.complex.context.FieldValueExtractor;
import com.rulex.service.complex.parsing.DateTimeParser;
import com.rulex.service.complex.parsing.TimeWindowParser;
import java.time.LocalTime;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class CustomerHistoryEvaluator {

  private CustomerHistoryEvaluator() {}

  public static boolean evaluateInCustomerHistory(
      RuleCondition condition, EvaluationContext context, VelocityServiceFacade velocityServiceFacade) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      String fieldToCheck = parts.length > 0 ? parts[0].trim() : condition.getFieldName();
      int days = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : 90;

      Object fieldValue = FieldValueExtractor.getFieldValue(fieldToCheck, null, context);
      if (fieldValue == null) {
        return false;
      }

      if (context.getTransactionRequest() != null) {
        VelocityService.TimeWindow window = TimeWindowParser.fromDays(days);
        VelocityService.VelocityStats stats =
            velocityServiceFacade.getStats(
                context.getTransactionRequest(), VelocityService.KeyType.PAN, window);
        return stats.getTransactionCount() > 0;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar IN_CUSTOMER_HISTORY: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateInCustomerUsualHours(
      RuleCondition condition, EvaluationContext context) {
    try {
      Object timeValue = FieldValueExtractor.getFieldValue("transactionTime", null, context);
      if (timeValue == null) {
        return true;
      }

      int currentHour;
      if (timeValue instanceof Integer) {
        currentHour = ((Integer) timeValue) / 10000;
      } else {
        LocalTime time = DateTimeParser.parseTime(timeValue);
        if (time == null) return true;
        currentHour = time.getHour();
      }

      boolean isNormalHour = currentHour >= 6 && currentHour <= 23;
      return isNormalHour;
    } catch (Exception e) {
      log.error("Erro ao avaliar IN_CUSTOMER_USUAL_HOURS: {}", e.getMessage());
      return true;
    }
  }

  public static boolean evaluateInCustomerChargebackMerchants(
      RuleCondition condition,
      EvaluationContext context,
      OperatorDataService operatorDataService) {
    try {
      Object merchantIdObj = FieldValueExtractor.getFieldValue("merchantId", null, context);
      if (merchantIdObj == null) {
        return false;
      }
      String merchantId = merchantIdObj.toString();

      Object customerIdObj = FieldValueExtractor.getFieldValue("customerIdFromHeader", null, context);
      if (customerIdObj == null) {
        customerIdObj = FieldValueExtractor.getFieldValue("customerId", null, context);
      }
      if (customerIdObj == null) {
        return false;
      }
      String customerId = customerIdObj.toString();

      boolean result = operatorDataService.hasChargebackWithMerchant(customerId, merchantId);
      log.debug(
          "IN_CUSTOMER_CHARGEBACK_MERCHANTS: customerId={}, merchantId={}, result={}",
          customerId,
          merchantId,
          result);
      return result;
    } catch (Exception e) {
      log.error("Erro ao avaliar IN_CUSTOMER_CHARGEBACK_MERCHANTS: {}", e.getMessage());
      return false;
    }
  }
}
