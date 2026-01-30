package com.rulex.adapter.engine;

import com.rulex.core.engine.port.RuleEngineShadowPort;
import com.rulex.dto.TransactionRequest;
import com.rulex.entity.RuleConfiguration;
import com.rulex.service.ShadowModeService;
import com.rulex.service.engine.ShadowRuleExecutionHelper;
import java.util.List;
import org.springframework.stereotype.Component;

@Component
public class RuleEngineShadowAdapter implements RuleEngineShadowPort {

  private final ShadowRuleExecutionHelper helper;

  public RuleEngineShadowAdapter(ShadowRuleExecutionHelper helper) {
    this.helper = helper;
  }

  @Override
  public void executeShadowRules(
      List<RuleConfiguration> shadowRules,
      TransactionRequest request,
      String actualDecision,
      ShadowModeService.RuleEvaluator evaluator) {
    helper.executeShadowRules(shadowRules, request, actualDecision, evaluator);
  }
}
