package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.RuleConditionGroup;
import com.rulex.entity.complex.RuleExecutionDetail;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.ArrayList;
import java.util.List;

public final class GroupEvaluationService {

  private GroupEvaluationService() {}

  @FunctionalInterface
  public interface ConditionEvaluator {
    boolean evaluate(RuleCondition condition, EvaluationContext context, List<RuleExecutionDetail> details);
  }

  public static boolean evaluateGroup(
      RuleConditionGroup group,
      EvaluationContext context,
      List<RuleExecutionDetail> details,
      ConditionEvaluator evaluator) {
    if (group == null || !Boolean.TRUE.equals(group.getEnabled())) {
      return true;
    }

    List<Boolean> results = new ArrayList<>();

    if (group.getConditions() != null) {
      for (RuleCondition condition : group.getConditions()) {
        if (Boolean.TRUE.equals(condition.getEnabled())) {
          boolean conditionResult = evaluator.evaluate(condition, context, details);
          results.add(conditionResult);
        }
      }
    }

    if (group.getChildren() != null) {
      for (RuleConditionGroup child : group.getChildren()) {
        boolean childResult = evaluateGroup(child, context, details, evaluator);
        results.add(childResult);
      }
    }

    if (results.isEmpty()) {
      return true;
    }

    return applyLogicOperator(group.getLogicOperator(), results);
  }

  private static boolean applyLogicOperator(
      RuleConditionGroup.GroupLogicOperator operator, List<Boolean> results) {
    if (results.isEmpty()) return true;

    return switch (operator) {
      case AND -> results.stream().allMatch(r -> r);
      case OR -> results.stream().anyMatch(r -> r);
      case NOT -> !results.get(0);
      case XOR -> results.stream().filter(r -> r).count() == 1;
      case NAND -> !results.stream().allMatch(r -> r);
      case NOR -> !results.stream().anyMatch(r -> r);
    };
  }
}
