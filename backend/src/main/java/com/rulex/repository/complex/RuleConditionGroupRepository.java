package com.rulex.repository.complex;

import com.rulex.entity.complex.RuleConditionGroup;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

/** Repositório para grupos de condições. */
@Repository
public interface RuleConditionGroupRepository extends JpaRepository<RuleConditionGroup, UUID> {

  /** Busca todos os grupos de uma versão de regra */
  List<RuleConditionGroup> findByRuleVersionIdOrderByPositionAsc(UUID ruleVersionId);

  /** Busca o grupo raiz (sem pai) de uma versão de regra */
  Optional<RuleConditionGroup> findByRuleVersionIdAndParentGroupIdIsNull(UUID ruleVersionId);

  /** Busca grupos filhos de um grupo pai */
  List<RuleConditionGroup> findByParentGroupIdOrderByPositionAsc(UUID parentGroupId);

  /** Busca grupos habilitados de uma versão de regra */
  List<RuleConditionGroup> findByRuleVersionIdAndEnabledTrueOrderByPositionAsc(UUID ruleVersionId);

  /** Conta quantos grupos existem em uma versão de regra */
  long countByRuleVersionId(UUID ruleVersionId);

  /** Verifica a profundidade máxima de aninhamento */
  @Query(
      value =
          """
        WITH RECURSIVE group_hierarchy AS (
            SELECT id, parent_group_id, 1 as depth
            FROM rule_condition_groups
            WHERE rule_version_id = :ruleVersionId AND parent_group_id IS NULL
            UNION ALL
            SELECT g.id, g.parent_group_id, gh.depth + 1
            FROM rule_condition_groups g
            INNER JOIN group_hierarchy gh ON g.parent_group_id = gh.id
        )
        SELECT COALESCE(MAX(depth), 0) FROM group_hierarchy
        """,
      nativeQuery = true)
  int findMaxDepthByRuleVersionId(@Param("ruleVersionId") UUID ruleVersionId);

  /** Deleta todos os grupos de uma versão de regra */
  void deleteByRuleVersionId(UUID ruleVersionId);

  /** Busca todos os grupos de uma versão de regra (alias) */
  default List<RuleConditionGroup> findByRuleVersionId(UUID ruleVersionId) {
    return findByRuleVersionIdOrderByPositionAsc(ruleVersionId);
  }

  /** Busca o grupo raiz de uma versão de regra (alias) */
  default Optional<RuleConditionGroup> findRootByRuleVersionId(UUID ruleVersionId) {
    return findByRuleVersionIdAndParentGroupIdIsNull(ruleVersionId);
  }
}
