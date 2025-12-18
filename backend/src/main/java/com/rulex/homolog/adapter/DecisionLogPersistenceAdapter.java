package com.rulex.homolog.adapter;

import com.rulex.entity.homolog.DecisionLogEntity;
import com.rulex.homolog.port.DecisionLogPersistencePort;
import com.rulex.repository.homolog.DecisionLogRepository;
import org.springframework.stereotype.Component;

@Component
public class DecisionLogPersistenceAdapter implements DecisionLogPersistencePort {

  private final DecisionLogRepository decisionLogRepository;

  public DecisionLogPersistenceAdapter(DecisionLogRepository decisionLogRepository) {
    this.decisionLogRepository = decisionLogRepository;
  }

  @Override
  public DecisionLogEntity save(DecisionLogEntity entity) {
    return decisionLogRepository.save(entity);
  }
}
