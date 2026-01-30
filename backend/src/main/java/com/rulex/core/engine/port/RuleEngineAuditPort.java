package com.rulex.core.engine.port;

import com.rulex.core.engine.model.RuleEvaluationResult;
import com.rulex.entity.Transaction;
import com.rulex.entity.TransactionDecision;

public interface RuleEngineAuditPort {

  void logTransactionProcessed(
      Transaction transaction, TransactionDecision decision, RuleEvaluationResult result);

  void logError(String transactionId, Exception exception);

  void logTamperAttempt(String transactionId, String expectedHash, String actualHash);
}
