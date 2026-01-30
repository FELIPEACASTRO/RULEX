package com.rulex.adapter.engine;

import com.rulex.core.engine.port.RuleEngineConditionPort;
import com.rulex.dto.RuleConditionDTO;
import com.rulex.dto.TransactionRequest;
import com.rulex.service.engine.RuleEngineConditionHelper;
import org.springframework.stereotype.Component;

@Component
public class RuleEngineConditionAdapter implements RuleEngineConditionPort {

  private final RuleEngineConditionHelper helper;

  public RuleEngineConditionAdapter(RuleEngineConditionHelper helper) {
    this.helper = helper;
  }

  @Override
  public boolean evaluateCondition(TransactionRequest request, RuleConditionDTO condition) {
    return helper.evaluateCondition(request, condition);
  }

  @Override
  public String explainCondition(TransactionRequest request, RuleConditionDTO condition, boolean result) {
    return helper.explainCondition(request, condition, result);
  }
}
