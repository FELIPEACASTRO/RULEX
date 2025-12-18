package com.rulex.repository.homolog;

import com.rulex.entity.homolog.RuleStatus;
import com.rulex.entity.homolog.RuleVersionEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface RuleVersionRepository extends JpaRepository<RuleVersionEntity, UUID> {

    List<RuleVersionEntity> findByRuleIdOrderByVersionDesc(UUID ruleId);

    @Query("select max(rv.version) from RuleVersionEntity rv where rv.ruleId = :ruleId")
    Integer findMaxVersion(UUID ruleId);

    Optional<RuleVersionEntity> findByRuleIdAndVersion(UUID ruleId, Integer version);

    List<RuleVersionEntity> findByRuleIdAndStatus(UUID ruleId, RuleStatus status);
}
