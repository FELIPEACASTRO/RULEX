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

  // ========== Métodos para rule_versions ==========

  /** Busca todos os grupos de uma versão de regra */
  List<RuleConditionGroup> findByRuleVersionIdOrderByPositionAsc(UUID ruleVersionId);

  /** Busca o grupo raiz (sem pai) de uma versão de regra */
  Optional<RuleConditionGroup> findByRuleVersionIdAndParentGroupIdIsNull(UUID ruleVersionId);

  /** Busca grupos habilitados de uma versão de regra */
  List<RuleConditionGroup> findByRuleVersionIdAndEnabledTrueOrderByPositionAsc(UUID ruleVersionId);

  /** Conta quantos grupos existem em uma versão de regra */
  long countByRuleVersionId(UUID ruleVersionId);

  /** Deleta todos os grupos de uma versão de regra */
  void deleteByRuleVersionId(UUID ruleVersionId);

  // ========== Métodos para complex_rules ==========

  /** Busca todos os grupos de uma regra complexa */
  List<RuleConditionGroup> findByComplexRuleIdOrderByPositionAsc(UUID complexRuleId);

  /** Busca o grupo raiz (sem pai) de uma regra complexa */
  Optional<RuleConditionGroup> findByComplexRuleIdAndParentGroupIdIsNull(UUID complexRuleId);

  /** Busca grupos habilitados de uma regra complexa */
  List<RuleConditionGroup> findByComplexRuleIdAndEnabledTrueOrderByPositionAsc(UUID complexRuleId);

  /** Conta quantos grupos existem em uma regra complexa */
  long countByComplexRuleId(UUID complexRuleId);

  /** Deleta todos os grupos de uma regra complexa */
  void deleteByComplexRuleId(UUID complexRuleId);

  // ========== Métodos comuns ==========

  /** Busca grupos filhos de um grupo pai */
  List<RuleConditionGroup> findByParentGroupIdOrderByPositionAsc(UUID parentGroupId);

  /** Verifica a profundidade máxima de aninhamento para rule_version */
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

  /** Verifica a profundidade máxima de aninhamento para complex_rule */
  @Query(
      value =
          """
        WITH RECURSIVE group_hierarchy AS (
            SELECT id, parent_group_id, 1 as depth
            FROM rule_condition_groups
            WHERE complex_rule_id = :complexRuleId AND parent_group_id IS NULL
            UNION ALL
            SELECT g.id, g.parent_group_id, gh.depth + 1
            FROM rule_condition_groups g
            INNER JOIN group_hierarchy gh ON g.parent_group_id = gh.id
        )
        SELECT COALESCE(MAX(depth), 0) FROM group_hierarchy
        """,
      nativeQuery = true)
  int findMaxDepthByComplexRuleId(@Param("complexRuleId") UUID complexRuleId);

  // ========== Aliases para compatibilidade ==========

  /** Busca todos os grupos de uma versão de regra (alias) */
  default List<RuleConditionGroup> findByRuleVersionId(UUID ruleVersionId) {
    return findByRuleVersionIdOrderByPositionAsc(ruleVersionId);
  }

  /** Busca o grupo raiz de uma versão de regra (alias) */
  default Optional<RuleConditionGroup> findRootByRuleVersionId(UUID ruleVersionId) {
    return findByRuleVersionIdAndParentGroupIdIsNull(ruleVersionId);
  }

  /** Busca todos os grupos de uma regra complexa (alias) */
  default List<RuleConditionGroup> findByComplexRuleId(UUID complexRuleId) {
    return findByComplexRuleIdOrderByPositionAsc(complexRuleId);
  }

  /** Busca o grupo raiz de uma regra complexa (alias) */
  default Optional<RuleConditionGroup> findRootByComplexRuleId(UUID complexRuleId) {
    return findByComplexRuleIdAndParentGroupIdIsNull(complexRuleId);
  }
}
