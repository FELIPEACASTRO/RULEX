package com.rulex.adapter.engine;

import com.rulex.core.engine.port.RuleEngineDecisionRepositoryPort;
import com.rulex.entity.TransactionDecision;
import com.rulex.repository.TransactionDecisionRepository;
import org.springframework.stereotype.Component;

@Component
public class RuleEngineDecisionRepositoryAdapter implements RuleEngineDecisionRepositoryPort {

  private final TransactionDecisionRepository decisionRepository;

  public RuleEngineDecisionRepositoryAdapter(TransactionDecisionRepository decisionRepository) {
    this.decisionRepository = decisionRepository;
  }

  @Override
  public TransactionDecision save(TransactionDecision decision) {
    return decisionRepository.save(decision);
  }
}
