package com.rulex.homolog.adapter;

import com.rulex.entity.homolog.SimulationRunEntity;
import com.rulex.homolog.port.SimulationRunPersistencePort;
import com.rulex.repository.homolog.SimulationRunRepository;
import org.springframework.stereotype.Component;

@Component
@SuppressWarnings("null")
public class SimulationRunPersistenceAdapter implements SimulationRunPersistencePort {

  private final SimulationRunRepository simulationRunRepository;

  public SimulationRunPersistenceAdapter(SimulationRunRepository simulationRunRepository) {
    this.simulationRunRepository = simulationRunRepository;
  }

  @Override
  public SimulationRunEntity save(SimulationRunEntity entity) {
    return simulationRunRepository.save(entity);
  }
}
