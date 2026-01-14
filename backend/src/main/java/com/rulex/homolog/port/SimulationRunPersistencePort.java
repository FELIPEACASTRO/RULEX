package com.rulex.homolog.port;

import com.rulex.entity.homolog.SimulationRunEntity;

public interface SimulationRunPersistencePort {
  SimulationRunEntity save(SimulationRunEntity entity);
}
