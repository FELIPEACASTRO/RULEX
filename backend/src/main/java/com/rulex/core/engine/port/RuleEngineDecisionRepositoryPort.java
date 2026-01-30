package com.rulex.core.engine.port;

import com.rulex.entity.TransactionDecision;

public interface RuleEngineDecisionRepositoryPort {

  TransactionDecision save(TransactionDecision decision);
}
