package com.rulex.homolog.port;

import com.rulex.entity.homolog.RuleSetVersionItemEntity;
import java.util.List;
import java.util.UUID;

public interface RuleSetVersionItemPersistencePort {
  RuleSetVersionItemEntity save(RuleSetVersionItemEntity item);

  List<RuleSetVersionItemEntity> findByRuleSetVersionIdOrderBySortOrderAsc(UUID ruleSetVersionId);
}
