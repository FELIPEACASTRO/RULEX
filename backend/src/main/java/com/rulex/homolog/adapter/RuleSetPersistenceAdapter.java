package com.rulex.homolog.adapter;

import com.rulex.entity.homolog.RuleSetEntity;
import com.rulex.homolog.port.RuleSetPersistencePort;
import com.rulex.repository.homolog.RuleSetRepository;
import java.util.Optional;
import java.util.UUID;
import org.springframework.stereotype.Component;

@Component
@SuppressWarnings("null")
public class RuleSetPersistenceAdapter implements RuleSetPersistencePort {

  private final RuleSetRepository ruleSetRepository;

  public RuleSetPersistenceAdapter(RuleSetRepository ruleSetRepository) {
    this.ruleSetRepository = ruleSetRepository;
  }

  @Override
  public RuleSetEntity save(RuleSetEntity ruleSet) {
    return java.util.Objects.requireNonNull(ruleSetRepository.save(ruleSet));
  }

  @Override
  public Optional<RuleSetEntity> findById(UUID id) {
    return ruleSetRepository.findById(java.util.Objects.requireNonNull(id));
  }
}
