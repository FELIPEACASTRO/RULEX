package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.VelocityService;
import com.rulex.service.VelocityServiceFacade;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import com.rulex.service.complex.context.FieldValueExtractor;
import com.rulex.service.complex.parsing.TimeWindowParser;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class FirstOccurrenceEvaluator {

  private FirstOccurrenceEvaluator() {}

  public static boolean evaluateIsFirst(
      RuleCondition condition,
      EvaluationContext context,
      VelocityServiceFacade velocityServiceFacade) {
    try {
      String fieldToCheck =
          condition.getValueSingle() != null
              ? condition.getValueSingle().trim()
              : condition.getFieldName();

      Object fieldValue = FieldValueExtractor.getFieldValue(fieldToCheck, null, context);
      if (fieldValue == null) {
        return false;
      }

      if (context.getTransactionRequest() != null) {
        VelocityService.VelocityStats stats =
            velocityServiceFacade.getStats(
                context.getTransactionRequest(),
                VelocityService.KeyType.PAN,
                VelocityService.TimeWindow.DAY_30);
        return stats.getTransactionCount() == 0;
      }
      return true;
    } catch (Exception e) {
      log.error("Erro ao avaliar IS_FIRST: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateIsNew(
      RuleCondition condition,
      EvaluationContext context,
      VelocityServiceFacade velocityServiceFacade) {
    try {
      int maxDays =
          condition.getValueSingle() != null
              ? Integer.parseInt(condition.getValueSingle().trim())
              : 7;

      Object fieldValue =
          FieldValueExtractor.getFieldValue(condition.getFieldName(), null, context);
      if (fieldValue == null) {
        return false;
      }

      if (context.getTransactionRequest() != null) {
        VelocityService.VelocityStats stats30 =
            velocityServiceFacade.getStats(
                context.getTransactionRequest(),
                VelocityService.KeyType.PAN,
                VelocityService.TimeWindow.DAY_30);

        VelocityService.TimeWindow window = TimeWindowParser.fromDays(maxDays);
        VelocityService.VelocityStats statsRecent =
            velocityServiceFacade.getStats(
                context.getTransactionRequest(), VelocityService.KeyType.PAN, window);

        return statsRecent.getTransactionCount() > 0
            && stats30.getTransactionCount() <= statsRecent.getTransactionCount();
      }
      return true;
    } catch (Exception e) {
      log.error("Erro ao avaliar IS_NEW: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateDistanceFromLastGt(
      RuleCondition condition,
      EvaluationContext context,
      VelocityServiceFacade velocityServiceFacade) {
    try {
      double thresholdKm = Double.parseDouble(condition.getValueSingle().trim());

      if (context.getTransactionRequest() == null) {
        return false;
      }

      String currentCountry = context.getTransactionRequest().getMerchantCountryCode();
      String currentCity = context.getTransactionRequest().getMerchantCity();

      if (currentCountry == null && currentCity == null) {
        return false;
      }

      if (context.getTransactionRequest() != null) {
        VelocityService.VelocityStats stats =
            velocityServiceFacade.getStats(
                context.getTransactionRequest(),
                VelocityService.KeyType.PAN,
                VelocityService.TimeWindow.HOUR_24);
        return stats.getDistinctCountries() > 1;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar DISTANCE_FROM_LAST_GT: {}", e.getMessage());
      return false;
    }
  }
}
