package com.rulex.repository;

import com.rulex.entity.RuleAbTest;
import com.rulex.entity.RuleAbTest.AbTestStatus;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

/** Repository para operações com testes A/B de regras. */
@Repository
public interface RuleAbTestRepository extends JpaRepository<RuleAbTest, Long> {

  /** Busca testes por status. */
  List<RuleAbTest> findByStatus(AbTestStatus status);

  /** Busca testes ativos (em execução). */
  List<RuleAbTest> findByStatusOrderByStartedAtDesc(AbTestStatus status);

  /** Busca testes por regra de controle. */
  List<RuleAbTest> findByControlRuleId(UUID controlRuleId);

  /** Busca testes por regra de tratamento. */
  List<RuleAbTest> findByTreatmentRuleId(UUID treatmentRuleId);

  /** Busca teste ativo para uma regra (controle ou tratamento). */
  @Query(
      "SELECT rat FROM RuleAbTest rat WHERE rat.status = 'RUNNING' "
          + "AND (rat.controlRuleId = :ruleId OR rat.treatmentRuleId = :ruleId)")
  List<RuleAbTest> findActiveTestsForRule(@Param("ruleId") UUID ruleId);

  /** Busca teste por nome. */
  Optional<RuleAbTest> findByName(String name);

  /** Verifica se existe teste ativo para uma regra. */
  @Query(
      "SELECT CASE WHEN COUNT(rat) > 0 THEN true ELSE false END FROM RuleAbTest rat "
          + "WHERE rat.status = 'RUNNING' "
          + "AND (rat.controlRuleId = :ruleId OR rat.treatmentRuleId = :ruleId)")
  boolean existsActiveTestForRule(@Param("ruleId") UUID ruleId);

  /** Busca testes criados por um usuário. */
  List<RuleAbTest> findByCreatedByOrderByCreatedAtDesc(String createdBy);

  /** Busca todos os testes ordenados por data de criação. */
  List<RuleAbTest> findAllByOrderByCreatedAtDesc();

  /** Conta testes por status. */
  long countByStatus(AbTestStatus status);
}
