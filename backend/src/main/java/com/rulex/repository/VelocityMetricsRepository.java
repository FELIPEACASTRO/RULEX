package com.rulex.repository;

import com.rulex.entity.VelocityMetrics;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

/** Repository para operações com métricas de velocity. */
@Repository
public interface VelocityMetricsRepository extends JpaRepository<VelocityMetrics, Long> {

  /** Busca métricas por data e hora. */
  Optional<VelocityMetrics> findByMetricDateAndMetricHour(LocalDate metricDate, Integer metricHour);

  /** Busca métricas de uma data específica. */
  List<VelocityMetrics> findByMetricDateOrderByMetricHour(LocalDate metricDate);

  /** Busca métricas de um período. */
  List<VelocityMetrics> findByMetricDateBetweenOrderByMetricDateAscMetricHourAsc(
      LocalDate startDate, LocalDate endDate);

  /** Calcula totais de um dia. */
  @Query(
      "SELECT SUM(vm.totalChecks), SUM(vm.cacheHits), SUM(vm.cacheMisses), "
          + "SUM(vm.dbQueries), SUM(vm.thresholdBreaches) "
          + "FROM VelocityMetrics vm WHERE vm.metricDate = :date")
  List<Object[]> calculateDailyTotals(@Param("date") LocalDate date);

  /** Calcula médias de latência de um dia. */
  @Query(
      "SELECT AVG(vm.avgLatencyMicros), AVG(vm.p95LatencyMicros), AVG(vm.p99LatencyMicros) "
          + "FROM VelocityMetrics vm WHERE vm.metricDate = :date")
  List<Object[]> calculateDailyLatencyAverages(@Param("date") LocalDate date);

  /** Busca métricas com threshold breaches. */
  @Query("SELECT vm FROM VelocityMetrics vm WHERE vm.thresholdBreaches > 0 ORDER BY vm.metricDate DESC, vm.metricHour DESC")
  List<VelocityMetrics> findWithThresholdBreaches();

  /** Calcula taxa de cache hit de um período. */
  @Query(
      "SELECT SUM(vm.cacheHits) * 1.0 / NULLIF(SUM(vm.totalChecks), 0) "
          + "FROM VelocityMetrics vm WHERE vm.metricDate BETWEEN :startDate AND :endDate")
  Optional<Double> calculateCacheHitRate(
      @Param("startDate") LocalDate startDate, @Param("endDate") LocalDate endDate);

  /** Busca a última métrica registrada. */
  Optional<VelocityMetrics> findFirstByOrderByMetricDateDescMetricHourDesc();

  /** Conta registros por data. */
  long countByMetricDate(LocalDate metricDate);
}
