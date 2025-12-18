package com.rulex.repository.homolog;

import com.rulex.entity.homolog.RuleSetVersionItemEntity;
import java.util.List;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;

public interface RuleSetVersionItemRepository
    extends JpaRepository<RuleSetVersionItemEntity, RuleSetVersionItemEntity.Pk> {
  List<RuleSetVersionItemEntity> findByRuleSetVersionIdOrderBySortOrderAsc(UUID ruleSetVersionId);
}
