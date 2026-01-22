package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.exception.UnsupportedOperatorException;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;

public final class FuzzyPlannedEvaluator {

  private FuzzyPlannedEvaluator() {}

  public static boolean evaluateFuzzyMembership(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FUZZY_MEMBERSHIP,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  public static boolean evaluateFuzzyAdaptiveThreshold(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FUZZY_ADAPTIVE_THRESHOLD,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }
}
