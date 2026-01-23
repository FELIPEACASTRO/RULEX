package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.RuleConditionGroup;
import com.rulex.entity.complex.RuleExecutionDetail;
import com.rulex.service.complex.ComplexRuleEvaluator;
import java.util.ArrayList;
import java.util.List;
import org.springframework.stereotype.Component;

@Component
public class ConditionGroupEvaluator {

  @FunctionalInterface
  public interface ConditionEvaluator {
    boolean evaluate(
        RuleCondition condition,
        ComplexRuleEvaluator.EvaluationContext context,
        List<RuleExecutionDetail> details);
  }

  public boolean evaluateGroup(
      RuleConditionGroup group,
      ComplexRuleEvaluator.EvaluationContext context,
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

  private boolean applyLogicOperator(
      RuleConditionGroup.GroupLogicOperator operator, List<Boolean> results) {
    if (results.isEmpty()) return true;

    return switch (operator) {
      case AND -> results.stream().allMatch(Boolean::booleanValue);
      case OR -> results.stream().anyMatch(Boolean::booleanValue);
      case NOT -> !results.get(0);
      case XOR -> results.stream().filter(Boolean::booleanValue).count() == 1;
      case NAND -> !results.stream().allMatch(Boolean::booleanValue);
      case NOR -> !results.stream().anyMatch(Boolean::booleanValue);
    };
  }
}
