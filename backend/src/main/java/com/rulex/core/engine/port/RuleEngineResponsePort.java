package com.rulex.core.engine.port;

import com.rulex.core.engine.model.RuleEvaluationResult;
import com.rulex.dto.EvaluateResponse;
import com.rulex.dto.TransactionResponse;
import com.rulex.dto.TriggeredRuleDTO;
import com.rulex.entity.Transaction;
import com.rulex.entity.TransactionDecision;
import com.rulex.v31.execlog.ExecutionEventType;
import java.util.List;
import com.fasterxml.jackson.databind.JsonNode;

public interface RuleEngineResponsePort {

  TransactionResponse buildResponse(
      Transaction transaction,
      TransactionDecision decision,
      RuleEvaluationResult result,
      long processingTime);

  TransactionResponse buildResponseFromExisting(Transaction transaction, long processingTime);

  TransactionResponse buildResponseFromTamperDecision(
      TransactionDecision decision, long processingTime);

  EvaluateResponse buildEvaluateResponse(
      Transaction transaction,
      TransactionDecision decision,
      RuleEvaluationResult result,
      long processingTime);

  EvaluateResponse buildEvaluateResponseFromExisting(Transaction transaction, long processingTime);

  EvaluateResponse buildEvaluateResponseFromTamperDecision(
      TransactionDecision decision, long processingTime);

  void safeLogEvaluate(
      ExecutionEventType eventType,
      String externalTransactionId,
      String payloadHash,
      TransactionDecision decision,
      List<TriggeredRuleDTO> triggeredRules,
      JsonNode errorJson);
}
