package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.RuleCondition;

public final class ExpectedValueFormatter {

  private ExpectedValueFormatter() {}

  public static String format(RuleCondition condition) {
    if (condition.getValueSingle() != null) {
      return condition.getValueSingle();
    }
    if (condition.getValueArray() != null) {
      return condition.getValueArray().toString();
    }
    if (condition.getValueMin() != null && condition.getValueMax() != null) {
      return condition.getValueMin() + " - " + condition.getValueMax();
    }
    if (condition.getValueFieldRef() != null) {
      return "field:" + condition.getValueFieldRef();
    }
    if (condition.getValueExpression() != null) {
      return "expr:" + condition.getValueExpression();
    }
    return "";
  }
}
