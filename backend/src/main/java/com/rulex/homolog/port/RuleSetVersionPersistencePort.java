package com.rulex.homolog.port;

import com.rulex.entity.homolog.RuleSetVersionEntity;
import java.util.Optional;
import java.util.UUID;

public interface RuleSetVersionPersistencePort {
  RuleSetVersionEntity save(RuleSetVersionEntity version);

  Optional<RuleSetVersionEntity> findById(UUID id);
}
