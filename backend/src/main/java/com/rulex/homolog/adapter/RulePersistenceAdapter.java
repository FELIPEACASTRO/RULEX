package com.rulex.homolog.adapter;

import com.rulex.entity.homolog.RuleEntity;
import com.rulex.homolog.port.RulePersistencePort;
import com.rulex.repository.homolog.RuleRepository;
import java.util.Optional;
import java.util.UUID;
import org.springframework.stereotype.Component;

@Component
public class RulePersistenceAdapter implements RulePersistencePort {

  private final RuleRepository ruleRepository;

  public RulePersistenceAdapter(RuleRepository ruleRepository) {
    this.ruleRepository = ruleRepository;
  }

  @Override
  public RuleEntity save(RuleEntity rule) {
    return ruleRepository.save(rule);
  }

  @Override
  public Optional<RuleEntity> findById(UUID id) {
    return ruleRepository.findById(id);
  }
}
