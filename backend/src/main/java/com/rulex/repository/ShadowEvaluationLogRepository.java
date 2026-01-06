package com.rulex.repository;

import com.rulex.entity.ShadowEvaluationLog;
import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

/** Repository para operações com logs de avaliação shadow. */
@Repository
public interface ShadowEvaluationLogRepository extends JpaRepository<ShadowEvaluationLog, Long> {

  /** Busca logs por rule ID. */
  List<ShadowEvaluationLog> findByRuleId(UUID ruleId);

  /** Busca logs por rule ID com paginação. */
  Page<ShadowEvaluationLog> findByRuleId(UUID ruleId, Pageable pageable);

  /** Busca logs por rule ID que foram triggered. */
  List<ShadowEvaluationLog> findByRuleIdAndTriggeredTrue(UUID ruleId);

  /** Busca logs por transaction ID. */
  List<ShadowEvaluationLog> findByTransactionId(Long transactionId);

  /** Busca logs por PAN hash. */
  List<ShadowEvaluationLog> findByPanHash(String panHash);

  /** Conta quantas vezes uma regra foi triggered em shadow mode. */
  long countByRuleIdAndTriggeredTrue(UUID ruleId);

  /** Conta total de avaliações de uma regra em shadow mode. */
  long countByRuleId(UUID ruleId);

  /** Busca logs avaliados após uma data. */
  List<ShadowEvaluationLog> findByEvaluatedAtAfter(OffsetDateTime after);

  /** Busca logs de uma regra avaliados após uma data. */
  List<ShadowEvaluationLog> findByRuleIdAndEvaluatedAtAfter(UUID ruleId, OffsetDateTime after);

  /**
   * Calcula estatísticas de uma regra shadow.
   * Retorna: [total, triggered, falsePositives, falseNegatives]
   */
  @Query(
      "SELECT COUNT(sel), "
          + "SUM(CASE WHEN sel.triggered = true THEN 1 ELSE 0 END), "
          + "SUM(CASE WHEN sel.triggered = true AND sel.actualDecision = 'APROVADO' THEN 1 ELSE 0 END), "
          + "SUM(CASE WHEN sel.triggered = false AND sel.actualDecision IN ('FRAUDE', 'SUSPEITA_DE_FRAUDE') THEN 1 ELSE 0 END) "
          + "FROM ShadowEvaluationLog sel WHERE sel.ruleId = :ruleId")
  List<Object[]> calculateStatsByRuleId(@Param("ruleId") UUID ruleId);

  /**
   * Calcula estatísticas de uma regra shadow em um período.
   */
  @Query(
      "SELECT COUNT(sel), "
          + "SUM(CASE WHEN sel.triggered = true THEN 1 ELSE 0 END), "
          + "SUM(CASE WHEN sel.triggered = true AND sel.actualDecision = 'APROVADO' THEN 1 ELSE 0 END), "
          + "SUM(CASE WHEN sel.triggered = false AND sel.actualDecision IN ('FRAUDE', 'SUSPEITA_DE_FRAUDE') THEN 1 ELSE 0 END) "
          + "FROM ShadowEvaluationLog sel WHERE sel.ruleId = :ruleId AND sel.evaluatedAt BETWEEN :start AND :end")
  List<Object[]> calculateStatsByRuleIdAndPeriod(
      @Param("ruleId") UUID ruleId,
      @Param("start") OffsetDateTime start,
      @Param("end") OffsetDateTime end);

  /** Calcula a latência média de uma regra. */
  @Query("SELECT AVG(sel.latencyMicros) FROM ShadowEvaluationLog sel WHERE sel.ruleId = :ruleId")
  Double calculateAvgLatencyByRuleId(@Param("ruleId") UUID ruleId);

  /** Deleta registros antigos (para limpeza de dados). */
  @Modifying
  @Query("DELETE FROM ShadowEvaluationLog sel WHERE sel.evaluatedAt < :before")
  int deleteOlderThan(@Param("before") OffsetDateTime before);
}
