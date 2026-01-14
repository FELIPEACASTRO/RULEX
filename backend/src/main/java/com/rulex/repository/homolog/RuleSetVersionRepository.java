package com.rulex.repository.homolog;

import com.rulex.entity.homolog.RuleSetVersionEntity;
import com.rulex.entity.homolog.RuleStatus;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface RuleSetVersionRepository extends JpaRepository<RuleSetVersionEntity, UUID> {

  List<RuleSetVersionEntity> findByRuleSetIdOrderByVersionDesc(UUID ruleSetId);

  Optional<RuleSetVersionEntity> findByRuleSetIdAndVersion(UUID ruleSetId, Integer version);

  List<RuleSetVersionEntity> findByRuleSetIdAndStatus(UUID ruleSetId, RuleStatus status);

  @Query("select max(rsv.version) from RuleSetVersionEntity rsv where rsv.ruleSetId = :ruleSetId")
  Integer findMaxVersion(UUID ruleSetId);
}
