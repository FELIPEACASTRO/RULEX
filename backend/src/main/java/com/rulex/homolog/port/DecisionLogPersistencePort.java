package com.rulex.homolog.port;

import com.rulex.entity.homolog.DecisionLogEntity;

public interface DecisionLogPersistencePort {
  DecisionLogEntity save(DecisionLogEntity entity);
}
