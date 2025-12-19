package com.rulex.homolog.adapter;

import com.rulex.entity.homolog.DecisionLogEntity;
import com.rulex.homolog.port.DecisionLogPersistencePort;
import com.rulex.repository.homolog.DecisionLogRepository;
import java.util.Objects;
import org.springframework.stereotype.Component;

@Component
public class DecisionLogPersistenceAdapter implements DecisionLogPersistencePort {

  private final DecisionLogRepository decisionLogRepository;

  public DecisionLogPersistenceAdapter(DecisionLogRepository decisionLogRepository) {
    this.decisionLogRepository = decisionLogRepository;
  }

  @Override
  public DecisionLogEntity save(DecisionLogEntity entity) {
    DecisionLogEntity log = Objects.requireNonNull(entity, "entity");
    return Objects.requireNonNull(decisionLogRepository.save(log));
  }
}
