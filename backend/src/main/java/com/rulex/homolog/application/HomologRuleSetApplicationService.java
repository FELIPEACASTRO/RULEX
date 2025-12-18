package com.rulex.homolog.application;

import com.rulex.dto.TransactionRequest;
import com.rulex.dto.homolog.CreateRuleSetRequest;
import com.rulex.dto.homolog.RuleSetVersionResponse;
import com.rulex.dto.homolog.SimulationResponse;
import com.rulex.homolog.usecase.HomologRuleSetUseCase;
import java.util.UUID;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class HomologRuleSetApplicationService {

  private final HomologRuleSetUseCase useCase;

  public HomologRuleSetApplicationService(HomologRuleSetUseCase useCase) {
    this.useCase = useCase;
  }

  @Transactional
  public RuleSetVersionResponse createDraft(String actorEmail, CreateRuleSetRequest req) {
    return useCase.createDraft(actorEmail, req);
  }

  @Transactional
  public RuleSetVersionResponse publish(String actorEmail, UUID ruleSetVersionId) {
    return useCase.publish(actorEmail, ruleSetVersionId);
  }

  @Transactional
  public void activate(String actorEmail, UUID ruleSetVersionId) {
    useCase.activate(actorEmail, ruleSetVersionId);
  }

  @Transactional
  public SimulationResponse simulate(
      String actorEmail, UUID maybeRuleSetVersionId, TransactionRequest payload) {
    return useCase.simulate(actorEmail, maybeRuleSetVersionId, payload);
  }
}
