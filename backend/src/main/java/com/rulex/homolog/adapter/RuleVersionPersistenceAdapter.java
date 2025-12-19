package com.rulex.homolog.adapter;

import com.rulex.entity.homolog.RuleVersionEntity;
import com.rulex.homolog.port.RuleVersionPersistencePort;
import com.rulex.repository.homolog.RuleVersionRepository;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.springframework.stereotype.Component;

@Component
@SuppressWarnings("null")
public class RuleVersionPersistenceAdapter implements RuleVersionPersistencePort {

  private final RuleVersionRepository ruleVersionRepository;

  public RuleVersionPersistenceAdapter(RuleVersionRepository ruleVersionRepository) {
    this.ruleVersionRepository = ruleVersionRepository;
  }

  @Override
  public RuleVersionEntity save(RuleVersionEntity version) {
    return ruleVersionRepository.save(version);
  }

  @Override
  public Optional<RuleVersionEntity> findById(UUID id) {
    return ruleVersionRepository.findById(id);
  }

  @Override
  public List<RuleVersionEntity> findByRuleIdOrderByVersionDesc(UUID ruleId) {
    return ruleVersionRepository.findByRuleIdOrderByVersionDesc(ruleId);
  }

  @Override
  public Optional<RuleVersionEntity> findByRuleIdAndVersion(UUID ruleId, int version) {
    return ruleVersionRepository.findByRuleIdAndVersion(ruleId, version);
  }
}
