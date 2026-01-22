package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.RuleExecutionDetail;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import com.rulex.service.complex.context.FieldValueExtractor;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class ConditionEvaluationService {

  private ConditionEvaluationService() {}

  public static boolean evaluateCondition(
      RuleCondition condition,
      EvaluationContext context,
      List<RuleExecutionDetail> details,
      BiFunction<RuleCondition, Object, Boolean> operatorEvaluator,
      Function<RuleCondition, String> expectedValueProvider) {
    long startTime = System.currentTimeMillis();
    String errorMessage = null;
    boolean result = false;

    try {
      Object fieldValue =
          FieldValueExtractor.getFieldValue(
              condition.getFieldName(), condition.getFieldPath(), context);

      result = operatorEvaluator.apply(condition, fieldValue);

      if (Boolean.TRUE.equals(condition.getNegate())) {
        result = !result;
      }
    } catch (Exception e) {
      log.warn("Erro ao avaliar condição {}: {}", condition.getFieldName(), e.getMessage());
      errorMessage = e.getMessage();
      result = false;
    }

    details.add(
        RuleExecutionDetail.builder()
            .decisionLogId(context.getDecisionLogId())
            .ruleVersionId(context.getRuleVersionId())
            .conditionId(condition.getId())
            .groupId(condition.getGroup() != null ? condition.getGroup().getId() : null)
            .fieldName(condition.getFieldName())
            .fieldValue(
                String.valueOf(
                    FieldValueExtractor.getFieldValue(
                        condition.getFieldName(), condition.getFieldPath(), context)))
            .operator(condition.getOperator().name())
            .expectedValue(expectedValueProvider.apply(condition))
            .result(result)
            .executionTimeMs((int) (System.currentTimeMillis() - startTime))
            .errorMessage(errorMessage)
            .build());

    return result;
  }
}
