package com.rulex.service.engine;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.RuleConfiguration;
import com.rulex.service.ShadowModeService;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class ShadowRuleExecutionHelper {

  private final ShadowModeService shadowModeService;

  public void executeShadowRules(
      List<RuleConfiguration> shadowRules,
      TransactionRequest request,
      String actualDecision,
      ShadowModeService.RuleEvaluator evaluator) {
    for (RuleConfiguration shadowRule : shadowRules) {
      shadowModeService.executeShadow(shadowRule, request, evaluator, actualDecision);
    }
  }
}
