package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.OperatorDataService;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import com.rulex.service.complex.context.FieldValueExtractor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class HistoricalEvaluator {

  private HistoricalEvaluator() {}

  public static boolean evaluateNotInHistorical(
      RuleCondition condition, EvaluationContext context, OperatorDataService operatorDataService) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      if (parts.length < 3) {
        log.warn("Formato inválido para NOT_IN_HISTORICAL. Esperado: sourceField:targetField:days");
        return false;
      }

      String sourceField = parts[0].trim();
      String targetField = parts[1].trim();
      int days = Integer.parseInt(parts[2].trim());

      Object sourceValue = FieldValueExtractor.getFieldValue(sourceField, null, context);
      Object targetValue = FieldValueExtractor.getFieldValue(targetField, null, context);

      if (sourceValue == null || targetValue == null) {
        return false;
      }

      String customerId = sourceValue.toString();
      String beneficiaryId = targetValue.toString();

      boolean isNew = operatorDataService.isNewBeneficiary(customerId, beneficiaryId);
      log.debug(
          "NOT_IN_HISTORICAL: customerId={}, beneficiaryId={}, days={}, isNew={}",
          customerId,
          beneficiaryId,
          days,
          isNew);
      return isNew;
    } catch (Exception e) {
      log.error("Erro ao avaliar NOT_IN_HISTORICAL: {}", e.getMessage());
      return false;
    }
  }
}
