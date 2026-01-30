package com.rulex.core.engine.port;

import com.rulex.dto.RuleConditionDTO;
import com.rulex.dto.TransactionRequest;

public interface RuleEngineConditionPort {

  boolean evaluateCondition(TransactionRequest request, RuleConditionDTO condition);

  String explainCondition(TransactionRequest request, RuleConditionDTO condition, boolean result);
}
