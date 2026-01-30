package com.rulex.core.engine.port;

import com.rulex.dto.TransactionRequest;

public interface RuleEngineVelocityPort {

  void recordTransaction(TransactionRequest request, String classification, int riskScore);
}
