package com.rulex.core.engine.port;

import com.rulex.dto.EvaluateResponse;
import com.rulex.dto.TransactionRequest;
import com.rulex.entity.TransactionDecision;
import java.util.List;

public interface RuleEngineContractValidationPort {

  List<String> missingRequiredForPersistence(TransactionRequest request);

  void safeLogContractError(
      String transactionId, String payloadHash, String errorCode, String message);

  EvaluateResponse buildContractErrorEvaluateResponse(
      String errorCode,
      String message,
      TransactionDecision.TransactionClassification classification,
      String payloadHash,
      long startTime);
}
