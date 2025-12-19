package com.rulex.homolog.adapter;

import com.rulex.entity.homolog.RuleSetVersionEntity;
import com.rulex.homolog.port.RuleSetVersionPersistencePort;
import com.rulex.repository.homolog.RuleSetVersionRepository;
import java.util.Optional;
import java.util.UUID;
import org.springframework.stereotype.Component;

@Component
@SuppressWarnings("null")
public class RuleSetVersionPersistenceAdapter implements RuleSetVersionPersistencePort {

  private final RuleSetVersionRepository ruleSetVersionRepository;

  public RuleSetVersionPersistenceAdapter(RuleSetVersionRepository ruleSetVersionRepository) {
    this.ruleSetVersionRepository = ruleSetVersionRepository;
  }

  @Override
  public RuleSetVersionEntity save(RuleSetVersionEntity version) {
    return java.util.Objects.requireNonNull(ruleSetVersionRepository.save(version));
  }

  @Override
  public Optional<RuleSetVersionEntity> findById(UUID id) {
    return ruleSetVersionRepository.findById(java.util.Objects.requireNonNull(id));
  }
}
