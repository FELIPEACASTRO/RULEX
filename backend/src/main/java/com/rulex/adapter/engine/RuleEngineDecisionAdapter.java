package com.rulex.adapter.engine;

import com.rulex.core.engine.model.RuleEvaluationResult;
import com.rulex.core.engine.port.RuleEngineDecisionPort;
import com.rulex.entity.Transaction;
import com.rulex.entity.TransactionDecision;
import com.rulex.service.engine.RuleEngineDecisionHelper;
import org.springframework.stereotype.Component;

@Component
public class RuleEngineDecisionAdapter implements RuleEngineDecisionPort {

  private final RuleEngineDecisionHelper helper;

  public RuleEngineDecisionAdapter(RuleEngineDecisionHelper helper) {
    this.helper = helper;
  }

  @Override
  public TransactionDecision buildTamperDecision(String externalTransactionId, String newPayloadHash) {
    return helper.buildTamperDecision(externalTransactionId, newPayloadHash);
  }

  @Override
  public TransactionDecision createDecision(Transaction transaction, RuleEvaluationResult result) {
    return helper.createDecision(transaction, result);
  }
}
