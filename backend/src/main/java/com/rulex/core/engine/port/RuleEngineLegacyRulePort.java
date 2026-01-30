package com.rulex.core.engine.port;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.RuleConfiguration;
import com.rulex.service.engine.RuleEngineLegacyRuleHelper;

public interface RuleEngineLegacyRulePort {

  RuleEngineLegacyRuleHelper.LegacyRuleResult evaluateLegacyRule(
      TransactionRequest request, RuleConfiguration rule);
}
