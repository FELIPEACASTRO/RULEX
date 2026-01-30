package com.rulex.adapter.engine;

import com.rulex.core.engine.port.RuleEngineVelocityPort;
import com.rulex.dto.TransactionRequest;
import com.rulex.service.VelocityServiceFacade;
import org.springframework.stereotype.Component;

@Component
public class RuleEngineVelocityAdapter implements RuleEngineVelocityPort {

  private final VelocityServiceFacade velocityServiceFacade;

  public RuleEngineVelocityAdapter(VelocityServiceFacade velocityServiceFacade) {
    this.velocityServiceFacade = velocityServiceFacade;
  }

  @Override
  public void recordTransaction(TransactionRequest request, String classification, int riskScore) {
    velocityServiceFacade.recordTransaction(request, classification, riskScore);
  }
}
