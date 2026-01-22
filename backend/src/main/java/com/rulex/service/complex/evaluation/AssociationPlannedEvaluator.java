package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.exception.UnsupportedOperatorException;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;

public final class AssociationPlannedEvaluator {

  private AssociationPlannedEvaluator() {}

  public static boolean evaluateAprioriAssociation(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.APRIORI_ASSOCIATION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  public static boolean evaluateEclatItemset(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.ECLAT_ITEMSET,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  public static boolean evaluateFpgrowthFrequentPatterns(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FPGROWTH_FREQUENT_PATTERNS,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }
}
