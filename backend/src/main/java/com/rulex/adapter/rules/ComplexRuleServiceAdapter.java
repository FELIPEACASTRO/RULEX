package com.rulex.adapter.rules;

import com.rulex.core.rules.port.ComplexRulePort;
import com.rulex.dto.complex.ConditionGroupDTO;
import com.rulex.dto.complex.ContextVariableDTO;
import com.rulex.dto.complex.ExpressionDTO;
import com.rulex.dto.complex.RuleActionDTO;
import com.rulex.service.complex.ComplexRuleService;
import java.util.List;
import java.util.UUID;
import org.springframework.stereotype.Component;

@Component
public class ComplexRuleServiceAdapter implements ComplexRulePort {

  private final ComplexRuleService complexRuleService;

  public ComplexRuleServiceAdapter(ComplexRuleService complexRuleService) {
    this.complexRuleService = complexRuleService;
  }

  @Override
  public ConditionGroupDTO getConditionGroup(UUID ruleVersionId) {
    return complexRuleService.getConditionGroup(ruleVersionId);
  }

  @Override
  public List<ExpressionDTO> getExpressions(UUID ruleVersionId) {
    return complexRuleService.getExpressions(ruleVersionId);
  }

  @Override
  public List<ContextVariableDTO> getContextVariables(UUID ruleVersionId) {
    return complexRuleService.getContextVariables(ruleVersionId);
  }

  @Override
  public List<RuleActionDTO> getActions(UUID ruleVersionId) {
    return complexRuleService.getActions(ruleVersionId);
  }

  @Override
  public List<String> getFieldsUsed(UUID ruleVersionId) {
    return complexRuleService.getFieldsUsed(ruleVersionId);
  }

  @Override
  public void saveConditionGroup(UUID ruleVersionId, ConditionGroupDTO conditionGroup) {
    complexRuleService.saveConditionGroup(ruleVersionId, conditionGroup);
  }

  @Override
  public void saveExpressions(UUID ruleVersionId, List<ExpressionDTO> expressions) {
    complexRuleService.saveExpressions(ruleVersionId, expressions);
  }

  @Override
  public void saveContextVariables(UUID ruleVersionId, List<ContextVariableDTO> variables) {
    complexRuleService.saveContextVariables(ruleVersionId, variables);
  }

  @Override
  public void saveActions(UUID ruleVersionId, List<RuleActionDTO> actions) {
    complexRuleService.saveActions(ruleVersionId, actions);
  }
}
