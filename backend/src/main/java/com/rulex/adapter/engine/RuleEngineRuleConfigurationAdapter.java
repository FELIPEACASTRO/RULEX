package com.rulex.adapter.engine;

import com.rulex.core.engine.port.RuleEngineRuleConfigurationPort;
import com.rulex.entity.RuleConfiguration;
import com.rulex.repository.RuleConfigurationRepository;
import java.util.List;
import org.springframework.stereotype.Component;

@Component
public class RuleEngineRuleConfigurationAdapter implements RuleEngineRuleConfigurationPort {

  private final RuleConfigurationRepository ruleConfigRepository;

  public RuleEngineRuleConfigurationAdapter(RuleConfigurationRepository ruleConfigRepository) {
    this.ruleConfigRepository = ruleConfigRepository;
  }

  @Override
  public List<RuleConfiguration> findByEnabled(boolean enabled) {
    return ruleConfigRepository.findByEnabled(enabled);
  }
}
