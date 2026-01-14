package com.rulex.homolog.port;

import com.rulex.entity.homolog.ActiveRuleSetEntity;
import java.util.Optional;

public interface ActiveRuleSetPersistencePort {
  Optional<ActiveRuleSetEntity> findById(short id);

  ActiveRuleSetEntity save(ActiveRuleSetEntity entity);
}
