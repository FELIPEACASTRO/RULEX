package com.rulex.adapter.engine;

import com.rulex.core.engine.port.RuleEnginePrecheckPort;
import com.rulex.dto.TransactionRequest;
import com.rulex.dto.TriggeredRuleDTO;
import com.rulex.entity.TransactionDecision;
import com.rulex.service.DerivedContext;
import com.rulex.service.engine.RuleEnginePrecheckHelper;
import java.util.List;
import java.util.Map;
import org.springframework.stereotype.Component;

@Component
public class RuleEnginePrecheckAdapter implements RuleEnginePrecheckPort {

  private final RuleEnginePrecheckHelper helper;

  public RuleEnginePrecheckAdapter(RuleEnginePrecheckHelper helper) {
    this.helper = helper;
  }

  @Override
  public TransactionDecision.TransactionClassification runPreChecks(
      TransactionRequest request,
      DerivedContext derivedContext,
      List<TriggeredRuleDTO> triggeredRules,
      Map<String, Object> scoreDetails,
      boolean bloomFilterEnabled,
      boolean impossibleTravelEnabled) {
    return helper.runPreChecks(
        request,
        derivedContext,
        triggeredRules,
        scoreDetails,
        bloomFilterEnabled,
        impossibleTravelEnabled);
  }
}
