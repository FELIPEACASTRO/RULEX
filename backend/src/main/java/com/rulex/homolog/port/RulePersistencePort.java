package com.rulex.homolog.port;

import com.rulex.entity.homolog.RuleEntity;
import java.util.Optional;
import java.util.UUID;

public interface RulePersistencePort {
  RuleEntity save(RuleEntity rule);

  Optional<RuleEntity> findById(UUID id);
}
