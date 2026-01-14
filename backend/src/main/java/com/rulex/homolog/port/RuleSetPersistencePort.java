package com.rulex.homolog.port;

import com.rulex.entity.homolog.RuleSetEntity;
import java.util.Optional;
import java.util.UUID;

public interface RuleSetPersistencePort {
  RuleSetEntity save(RuleSetEntity ruleSet);

  Optional<RuleSetEntity> findById(UUID id);
}
