package com.rulex.homolog.port;

import com.rulex.entity.homolog.RuleVersionEntity;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface RuleVersionPersistencePort {
  RuleVersionEntity save(RuleVersionEntity version);

  Optional<RuleVersionEntity> findById(UUID id);

  List<RuleVersionEntity> findByRuleIdOrderByVersionDesc(UUID ruleId);

  Optional<RuleVersionEntity> findByRuleIdAndVersion(UUID ruleId, int version);
}
