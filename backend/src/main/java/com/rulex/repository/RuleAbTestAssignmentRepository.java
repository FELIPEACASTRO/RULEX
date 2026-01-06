package com.rulex.repository;

import com.rulex.entity.RuleAbTestAssignment;
import com.rulex.entity.RuleAbTestAssignment.AssignedGroup;
import java.time.OffsetDateTime;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

/** Repository para operações com assignments de testes A/B. */
@Repository
public interface RuleAbTestAssignmentRepository extends JpaRepository<RuleAbTestAssignment, Long> {

  /** Busca assignments por test ID. */
  List<RuleAbTestAssignment> findByTestId(Long testId);

  /** Busca assignments por test ID com paginação. */
  Page<RuleAbTestAssignment> findByTestId(Long testId, Pageable pageable);

  /** Busca assignments por test ID e grupo. */
  List<RuleAbTestAssignment> findByTestIdAndAssignedGroup(Long testId, AssignedGroup assignedGroup);

  /** Busca assignment por test ID e PAN hash (para consistência de grupo). */
  Optional<RuleAbTestAssignment> findFirstByTestIdAndPanHashOrderByCreatedAtDesc(
      Long testId, String panHash);

  /** Busca assignments por transaction ID. */
  List<RuleAbTestAssignment> findByTransactionId(Long transactionId);

  /** Conta assignments por test ID e grupo. */
  long countByTestIdAndAssignedGroup(Long testId, AssignedGroup assignedGroup);

  /** Conta assignments triggered por test ID e grupo. */
  long countByTestIdAndAssignedGroupAndTriggeredTrue(Long testId, AssignedGroup assignedGroup);

  /**
   * Calcula estatísticas de um teste A/B.
   * Retorna: [grupo, total, triggered]
   */
  @Query(
      "SELECT rata.assignedGroup, COUNT(rata), SUM(CASE WHEN rata.triggered = true THEN 1 ELSE 0 END) "
          + "FROM RuleAbTestAssignment rata WHERE rata.testId = :testId "
          + "GROUP BY rata.assignedGroup")
  List<Object[]> calculateStatsByTestId(@Param("testId") Long testId);

  /**
   * Calcula estatísticas de um teste A/B em um período.
   */
  @Query(
      "SELECT rata.assignedGroup, COUNT(rata), SUM(CASE WHEN rata.triggered = true THEN 1 ELSE 0 END) "
          + "FROM RuleAbTestAssignment rata WHERE rata.testId = :testId "
          + "AND rata.createdAt BETWEEN :start AND :end "
          + "GROUP BY rata.assignedGroup")
  List<Object[]> calculateStatsByTestIdAndPeriod(
      @Param("testId") Long testId,
      @Param("start") OffsetDateTime start,
      @Param("end") OffsetDateTime end);

  /** Busca PANs únicos em um teste. */
  @Query("SELECT DISTINCT rata.panHash FROM RuleAbTestAssignment rata WHERE rata.testId = :testId")
  List<String> findDistinctPanHashesByTestId(@Param("testId") Long testId);

  /** Conta PANs únicos em um teste. */
  @Query("SELECT COUNT(DISTINCT rata.panHash) FROM RuleAbTestAssignment rata WHERE rata.testId = :testId")
  long countDistinctPanHashesByTestId(@Param("testId") Long testId);

  /** Deleta assignments de um teste. */
  @Modifying
  @Query("DELETE FROM RuleAbTestAssignment rata WHERE rata.testId = :testId")
  int deleteByTestId(@Param("testId") Long testId);

  /** Deleta registros antigos (para limpeza de dados). */
  @Modifying
  @Query("DELETE FROM RuleAbTestAssignment rata WHERE rata.createdAt < :before")
  int deleteOlderThan(@Param("before") OffsetDateTime before);
}
