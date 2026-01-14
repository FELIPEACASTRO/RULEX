package com.rulex.homolog.application;

import com.rulex.dto.homolog.CreateRuleRequest;
import com.rulex.dto.homolog.RuleVersionResponse;
import com.rulex.homolog.usecase.HomologRuleUseCase;
import java.util.Optional;
import java.util.UUID;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class HomologRuleApplicationService {

  private final HomologRuleUseCase useCase;

  public HomologRuleApplicationService(HomologRuleUseCase useCase) {
    this.useCase = useCase;
  }

  @Transactional
  public RuleVersionResponse create(String actorEmail, CreateRuleRequest req) {
    return useCase.create(actorEmail, req);
  }

  @Transactional(readOnly = true)
  public Optional<RuleVersionResponse> getLatest(UUID ruleId) {
    return useCase.getLatest(ruleId);
  }

  @Transactional
  public RuleVersionResponse publish(String actorEmail, UUID ruleVersionId) {
    return useCase.publish(actorEmail, ruleVersionId);
  }

  @Transactional
  public RuleVersionResponse rollbackToVersion(String actorEmail, UUID ruleId, int version) {
    return useCase.rollbackToVersion(actorEmail, ruleId, version);
  }
}
