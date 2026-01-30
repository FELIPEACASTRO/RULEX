package com.rulex.adapter.engine;

import com.rulex.core.engine.model.RuleEvaluationResult;
import com.rulex.core.engine.port.RuleEngineAuditPort;
import com.rulex.entity.Transaction;
import com.rulex.entity.TransactionDecision;
import com.rulex.service.AuditService;
import org.springframework.stereotype.Component;

@Component
public class RuleEngineAuditAdapter implements RuleEngineAuditPort {

  private final AuditService auditService;

  public RuleEngineAuditAdapter(AuditService auditService) {
    this.auditService = auditService;
  }

  @Override
  public void logTransactionProcessed(
      Transaction transaction, TransactionDecision decision, RuleEvaluationResult result) {
    auditService.logTransactionProcessed(transaction, decision, result);
  }

  @Override
  public void logError(String transactionId, Exception exception) {
    auditService.logError(transactionId, exception);
  }

  @Override
  public void logTamperAttempt(String transactionId, String expectedHash, String actualHash) {
    auditService.logTamperAttempt(transactionId, expectedHash, actualHash);
  }
}
