package com.rulex.core.engine.port;

import com.rulex.dto.TransactionRequest;
import com.rulex.dto.TriggeredRuleDTO;
import com.rulex.entity.TransactionDecision;
import com.rulex.service.DerivedContext;
import java.util.List;
import java.util.Map;

public interface RuleEnginePrecheckPort {

  TransactionDecision.TransactionClassification runPreChecks(
      TransactionRequest request,
      DerivedContext derivedContext,
      List<TriggeredRuleDTO> triggeredRules,
      Map<String, Object> scoreDetails,
      boolean bloomFilterEnabled,
      boolean impossibleTravelEnabled);
}
