package com.rulex.core.engine.port;

import com.rulex.entity.RuleConfiguration;
import java.util.List;

public interface RuleEngineRuleConfigurationPort {

  List<RuleConfiguration> findByEnabled(boolean enabled);
}
