package com.rulex.repository;

import com.rulex.entity.RuleMetrics;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

/** Repositório para métricas de regras. */
@Repository
public interface RuleMetricsRepository extends JpaRepository<RuleMetrics, Long> {

  /** Busca métricas de uma regra por data. */
  Optional<RuleMetrics> findByRuleIdAndMetricDate(Long ruleId, LocalDate metricDate);

  /** Busca métricas de uma regra em um período. */
  List<RuleMetrics> findByRuleIdAndMetricDateBetweenOrderByMetricDateDesc(
      Long ruleId, LocalDate startDate, LocalDate endDate);

  /** Busca métricas de todas as regras para uma data. */
  List<RuleMetrics> findByMetricDateOrderByTotalTriggeredDesc(LocalDate metricDate);

  /** Busca top N regras por número de disparos em um período. */
  @Query(
      "SELECT m FROM RuleMetrics m WHERE m.metricDate BETWEEN :startDate AND :endDate "
          + "GROUP BY m.ruleId, m.ruleName, m.id, m.metricDate, m.totalEvaluations, m.totalTriggered, "
          + "m.truePositives, m.falsePositives, m.totalAmountBlocked, m.avgProcessingTimeMs, m.createdAt, m.updatedAt "
          + "ORDER BY SUM(m.totalTriggered) DESC")
  List<RuleMetrics> findTopRulesByTriggers(
      @Param("startDate") LocalDate startDate, @Param("endDate") LocalDate endDate);

  /** Calcula totais agregados por regra em um período. */
  @Query(
      "SELECT m.ruleId, m.ruleName, SUM(m.totalEvaluations), SUM(m.totalTriggered), "
          + "SUM(m.truePositives), SUM(m.falsePositives), SUM(m.totalAmountBlocked) "
          + "FROM RuleMetrics m WHERE m.metricDate BETWEEN :startDate AND :endDate "
          + "GROUP BY m.ruleId, m.ruleName ORDER BY SUM(m.totalTriggered) DESC")
  List<Object[]> getAggregatedMetricsByPeriod(
      @Param("startDate") LocalDate startDate, @Param("endDate") LocalDate endDate);

  /** Busca regras com alta taxa de falsos positivos. */
  @Query(
      "SELECT m FROM RuleMetrics m WHERE m.metricDate = :date "
          + "AND m.totalTriggered > 0 "
          + "AND (CAST(m.falsePositives AS double) / CAST(m.totalTriggered AS double)) > :threshold "
          + "ORDER BY (CAST(m.falsePositives AS double) / CAST(m.totalTriggered AS double)) DESC")
  List<RuleMetrics> findRulesWithHighFalsePositiveRate(
      @Param("date") LocalDate date, @Param("threshold") double threshold);

  /** Busca métricas recentes de uma regra. */
  List<RuleMetrics> findTop30ByRuleIdOrderByMetricDateDesc(Long ruleId);

  /** Soma total de valor bloqueado por regra em um período. */
  @Query(
      "SELECT SUM(m.totalAmountBlocked) FROM RuleMetrics m "
          + "WHERE m.ruleId = :ruleId AND m.metricDate BETWEEN :startDate AND :endDate")
  java.math.BigDecimal sumAmountBlockedByRuleAndPeriod(
      @Param("ruleId") Long ruleId,
      @Param("startDate") LocalDate startDate,
      @Param("endDate") LocalDate endDate);
}
