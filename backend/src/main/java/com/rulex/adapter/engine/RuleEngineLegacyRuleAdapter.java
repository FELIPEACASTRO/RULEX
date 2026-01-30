package com.rulex.adapter.engine;

import com.rulex.core.engine.port.RuleEngineLegacyRulePort;
import com.rulex.dto.TransactionRequest;
import com.rulex.entity.RuleConfiguration;
import com.rulex.service.engine.RuleEngineLegacyRuleHelper;
import org.springframework.stereotype.Component;

@Component
public class RuleEngineLegacyRuleAdapter implements RuleEngineLegacyRulePort {

  private final RuleEngineLegacyRuleHelper helper;

  public RuleEngineLegacyRuleAdapter(RuleEngineLegacyRuleHelper helper) {
    this.helper = helper;
  }

  @Override
  public RuleEngineLegacyRuleHelper.LegacyRuleResult evaluateLegacyRule(
      TransactionRequest request, RuleConfiguration rule) {
    return helper.evaluateLegacyRule(request, rule);
  }
}
