package com.rulex.adapter.engine;

import com.rulex.core.engine.port.RuleEngineContractValidationPort;
import com.rulex.dto.EvaluateResponse;
import com.rulex.dto.TransactionRequest;
import com.rulex.entity.TransactionDecision;
import com.rulex.service.engine.ContractValidationHelper;
import java.util.List;
import org.springframework.stereotype.Component;

@Component
public class RuleEngineContractValidationAdapter implements RuleEngineContractValidationPort {

  private final ContractValidationHelper helper;

  public RuleEngineContractValidationAdapter(ContractValidationHelper helper) {
    this.helper = helper;
  }

  @Override
  public List<String> missingRequiredForPersistence(TransactionRequest request) {
    return helper.missingRequiredForPersistence(request);
  }

  @Override
  public void safeLogContractError(
      String transactionId, String payloadHash, String errorCode, String message) {
    helper.safeLogContractError(transactionId, payloadHash, errorCode, message);
  }

  @Override
  public EvaluateResponse buildContractErrorEvaluateResponse(
      String errorCode,
      String message,
      TransactionDecision.TransactionClassification classification,
      String payloadHash,
      long startTime) {
    return helper.buildContractErrorEvaluateResponse(
        errorCode, message, classification, payloadHash, startTime);
  }
}
