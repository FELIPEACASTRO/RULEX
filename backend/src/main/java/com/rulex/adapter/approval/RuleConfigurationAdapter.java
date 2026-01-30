package com.rulex.adapter.approval;

import com.rulex.core.approval.port.RuleConfigurationPort;
import com.rulex.dto.RuleConfigurationDTO;
import com.rulex.service.RuleConfigurationService;
import org.springframework.stereotype.Component;

@Component
public class RuleConfigurationAdapter implements RuleConfigurationPort {

  private final RuleConfigurationService ruleConfigurationService;

  public RuleConfigurationAdapter(RuleConfigurationService ruleConfigurationService) {
    this.ruleConfigurationService = ruleConfigurationService;
  }

  @Override
  public RuleConfigurationDTO getRuleById(Long ruleId) {
    return ruleConfigurationService.getRuleById(ruleId);
  }

  @Override
  public RuleConfigurationDTO createRule(RuleConfigurationDTO ruleDto) {
    return ruleConfigurationService.createRule(ruleDto);
  }

  @Override
  public RuleConfigurationDTO updateRule(Long ruleId, RuleConfigurationDTO ruleDto) {
    return ruleConfigurationService.updateRule(ruleId, ruleDto);
  }

  @Override
  public void deleteRule(Long ruleId) {
    ruleConfigurationService.deleteRule(ruleId);
  }

  @Override
  public RuleConfigurationDTO toggleRule(Long ruleId) {
    return ruleConfigurationService.toggleRule(ruleId);
  }
}
