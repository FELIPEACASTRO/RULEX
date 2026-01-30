package com.rulex.core.rules.port;

import com.rulex.dto.complex.ConditionGroupDTO;
import com.rulex.dto.complex.ContextVariableDTO;
import com.rulex.dto.complex.ExpressionDTO;
import com.rulex.dto.complex.RuleActionDTO;
import java.util.List;
import java.util.UUID;

public interface ComplexRulePort {

  ConditionGroupDTO getConditionGroup(UUID ruleVersionId);

  List<ExpressionDTO> getExpressions(UUID ruleVersionId);

  List<ContextVariableDTO> getContextVariables(UUID ruleVersionId);

  List<RuleActionDTO> getActions(UUID ruleVersionId);

  List<String> getFieldsUsed(UUID ruleVersionId);

  void saveConditionGroup(UUID ruleVersionId, ConditionGroupDTO conditionGroup);

  void saveExpressions(UUID ruleVersionId, List<ExpressionDTO> expressions);

  void saveContextVariables(UUID ruleVersionId, List<ContextVariableDTO> variables);

  void saveActions(UUID ruleVersionId, List<RuleActionDTO> actions);
}
