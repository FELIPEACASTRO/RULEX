package com.rulex.adapter.engine;

import com.rulex.core.engine.model.RuleEvaluationResult;
import com.rulex.core.engine.port.RuleEngineResponsePort;
import com.rulex.dto.EvaluateResponse;
import com.rulex.dto.TransactionResponse;
import com.rulex.dto.TriggeredRuleDTO;
import com.rulex.entity.Transaction;
import com.rulex.entity.TransactionDecision;
import com.rulex.service.engine.RuleEngineResponseBuilder;
import com.rulex.v31.execlog.ExecutionEventType;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.List;
import org.springframework.stereotype.Component;

@Component
public class RuleEngineResponseAdapter implements RuleEngineResponsePort {

  private final RuleEngineResponseBuilder builder;

  public RuleEngineResponseAdapter(RuleEngineResponseBuilder builder) {
    this.builder = builder;
  }

  @Override
  public TransactionResponse buildResponse(
      Transaction transaction,
      TransactionDecision decision,
      RuleEvaluationResult result,
      long processingTime) {
    return builder.buildResponse(transaction, decision, result, processingTime);
  }

  @Override
  public TransactionResponse buildResponseFromExisting(Transaction transaction, long processingTime) {
    return builder.buildResponseFromExisting(transaction, processingTime);
  }

  @Override
  public TransactionResponse buildResponseFromTamperDecision(
      TransactionDecision decision, long processingTime) {
    return builder.buildResponseFromTamperDecision(decision, processingTime);
  }

  @Override
  public EvaluateResponse buildEvaluateResponse(
      Transaction transaction,
      TransactionDecision decision,
      RuleEvaluationResult result,
      long processingTime) {
    return builder.buildEvaluateResponse(transaction, decision, result, processingTime);
  }

  @Override
  public EvaluateResponse buildEvaluateResponseFromExisting(Transaction transaction, long processingTime) {
    return builder.buildEvaluateResponseFromExisting(transaction, processingTime);
  }

  @Override
  public EvaluateResponse buildEvaluateResponseFromTamperDecision(
      TransactionDecision decision, long processingTime) {
    return builder.buildEvaluateResponseFromTamperDecision(decision, processingTime);
  }

  @Override
  public void safeLogEvaluate(
      ExecutionEventType eventType,
      String externalTransactionId,
      String payloadHash,
      TransactionDecision decision,
      List<TriggeredRuleDTO> triggeredRules,
      JsonNode errorJson) {
    builder.safeLogEvaluate(
        eventType, externalTransactionId, payloadHash, decision, triggeredRules, errorJson);
  }
}
