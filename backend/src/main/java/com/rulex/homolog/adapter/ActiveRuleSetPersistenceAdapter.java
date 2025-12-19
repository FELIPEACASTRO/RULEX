package com.rulex.homolog.adapter;

import com.rulex.entity.homolog.ActiveRuleSetEntity;
import com.rulex.homolog.port.ActiveRuleSetPersistencePort;
import com.rulex.repository.homolog.ActiveRuleSetRepository;
import java.util.Optional;
import org.springframework.stereotype.Component;

@Component
@SuppressWarnings("null")
public class ActiveRuleSetPersistenceAdapter implements ActiveRuleSetPersistencePort {

  private final ActiveRuleSetRepository activeRuleSetRepository;

  public ActiveRuleSetPersistenceAdapter(ActiveRuleSetRepository activeRuleSetRepository) {
    this.activeRuleSetRepository = activeRuleSetRepository;
  }

  @Override
  public Optional<ActiveRuleSetEntity> findById(short id) {
    return activeRuleSetRepository.findById(id);
  }

  @Override
  public ActiveRuleSetEntity save(ActiveRuleSetEntity entity) {
    return java.util.Objects.requireNonNull(activeRuleSetRepository.save(entity));
  }
}
