package com.rulex.core.approval.port;

import com.rulex.dto.RuleConfigurationDTO;

public interface RuleConfigurationPort {

  RuleConfigurationDTO getRuleById(Long ruleId);

  RuleConfigurationDTO createRule(RuleConfigurationDTO ruleDto);

  RuleConfigurationDTO updateRule(Long ruleId, RuleConfigurationDTO ruleDto);

  void deleteRule(Long ruleId);

  RuleConfigurationDTO toggleRule(Long ruleId);
}
