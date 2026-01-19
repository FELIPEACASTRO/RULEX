package com.rulex.repository.complex;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import java.util.List;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

/** Repositório para condições individuais. */
@Repository
public interface RuleConditionRepository extends JpaRepository<RuleCondition, UUID> {

  /** Busca todas as condições de um grupo */
  List<RuleCondition> findByGroupIdOrderByPositionAsc(UUID groupId);

  /** Busca condições habilitadas de um grupo */
  List<RuleCondition> findByGroupIdAndEnabledTrueOrderByPositionAsc(UUID groupId);

  /** Busca condições por nome de campo */
  List<RuleCondition> findByFieldName(String fieldName);

  /** Busca condições por operador */
  List<RuleCondition> findByOperator(ConditionOperator operator);

  /** Conta condições em um grupo */
  long countByGroupId(UUID groupId);

  /** Busca todas as condições de uma versão de regra (através dos grupos) */
  @Query(
      """
        SELECT c FROM RuleCondition c
        JOIN c.group g
        WHERE g.ruleVersionId = :ruleVersionId
        ORDER BY g.position, c.position
        """)
  List<RuleCondition> findAllByRuleVersionId(@Param("ruleVersionId") UUID ruleVersionId);

  /** Busca campos únicos usados em uma versão de regra */
  @Query(
      """
        SELECT DISTINCT c.fieldName FROM RuleCondition c
        JOIN c.group g
        WHERE g.ruleVersionId = :ruleVersionId
        """)
  List<String> findDistinctFieldNamesByRuleVersionId(@Param("ruleVersionId") UUID ruleVersionId);

  /** Deleta todas as condições de um grupo */
  void deleteByGroupId(UUID groupId);
}
