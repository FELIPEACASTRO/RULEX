package com.rulex.core.engine.port;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.RuleConfiguration;
import com.rulex.service.ShadowModeService;
import java.util.List;

public interface RuleEngineShadowPort {

  void executeShadowRules(
      List<RuleConfiguration> shadowRules,
      TransactionRequest request,
      String actualDecision,
      ShadowModeService.RuleEvaluator evaluator);
}
