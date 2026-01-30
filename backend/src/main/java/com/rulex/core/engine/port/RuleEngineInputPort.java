package com.rulex.core.engine.port;

import com.rulex.dto.EvaluateResponse;
import com.rulex.dto.TransactionRequest;
import com.rulex.dto.TransactionResponse;

public interface RuleEngineInputPort {

  TransactionResponse analyzeTransaction(TransactionRequest request);

  TransactionResponse analyzeTransaction(
      TransactionRequest request, byte[] rawBytes, String contentType);

  EvaluateResponse evaluate(TransactionRequest request);

  EvaluateResponse evaluate(TransactionRequest request, byte[] rawBytes, String contentType);

  EvaluateResponse evaluateRaw(String rawBody, byte[] rawBytes, String contentType);
}
