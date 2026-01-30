package com.rulex.core.engine.port;

import com.rulex.core.engine.model.RuleEvaluationResult;
import com.rulex.entity.Transaction;
import com.rulex.entity.TransactionDecision;

public interface RuleEngineDecisionPort {

  TransactionDecision buildTamperDecision(String externalTransactionId, String newPayloadHash);

  TransactionDecision createDecision(Transaction transaction, RuleEvaluationResult result);
}
