package com.rulex.entity;

import jakarta.persistence.*;
import java.time.LocalDate;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Entidade para métricas do serviço de velocity. Armazena estatísticas horárias de performance e
 * uso do serviço de velocity para monitoramento.
 */
@Entity
@Table(
    name = "velocity_metrics",
    indexes = {@Index(name = "idx_velocity_metrics_date", columnList = "metric_date")},
    uniqueConstraints = {
      @UniqueConstraint(
          name = "uk_velocity_metrics_date_hour",
          columnNames = {"metric_date", "metric_hour"})
    })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class VelocityMetrics {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "metric_date", nullable = false)
  private LocalDate metricDate;

  @Column(name = "metric_hour", nullable = false)
  private Integer metricHour;

  // Counters
  @Column(name = "total_checks")
  @Builder.Default
  private Long totalChecks = 0L;

  @Column(name = "cache_hits")
  @Builder.Default
  private Long cacheHits = 0L;

  @Column(name = "cache_misses")
  @Builder.Default
  private Long cacheMisses = 0L;

  @Column(name = "db_queries")
  @Builder.Default
  private Long dbQueries = 0L;

  // Performance
  @Column(name = "avg_latency_micros")
  private Double avgLatencyMicros;

  @Column(name = "p95_latency_micros")
  private Double p95LatencyMicros;

  @Column(name = "p99_latency_micros")
  private Double p99LatencyMicros;

  // Alerts triggered
  @Column(name = "threshold_breaches")
  @Builder.Default
  private Long thresholdBreaches = 0L;

  @PrePersist
  protected void onCreate() {
    if (metricDate == null) {
      metricDate = LocalDate.now();
    }
    if (metricHour == null) {
      metricHour = java.time.LocalTime.now().getHour();
    }
    initializeDefaults();
  }

  private void initializeDefaults() {
    if (totalChecks == null) totalChecks = 0L;
    if (cacheHits == null) cacheHits = 0L;
    if (cacheMisses == null) cacheMisses = 0L;
    if (dbQueries == null) dbQueries = 0L;
    if (thresholdBreaches == null) thresholdBreaches = 0L;
  }

  /** Incrementa o contador de checks totais. */
  public void incrementTotalChecks() {
    if (totalChecks == null) totalChecks = 0L;
    totalChecks++;
  }

  /** Incrementa o contador de cache hits. */
  public void incrementCacheHits() {
    if (cacheHits == null) cacheHits = 0L;
    cacheHits++;
  }

  /** Incrementa o contador de cache misses. */
  public void incrementCacheMisses() {
    if (cacheMisses == null) cacheMisses = 0L;
    cacheMisses++;
  }

  /** Incrementa o contador de queries ao banco. */
  public void incrementDbQueries() {
    if (dbQueries == null) dbQueries = 0L;
    dbQueries++;
  }

  /** Incrementa o contador de threshold breaches. */
  public void incrementThresholdBreaches() {
    if (thresholdBreaches == null) thresholdBreaches = 0L;
    thresholdBreaches++;
  }

  /** Calcula a taxa de cache hit. Retorna 0 se não houver checks. */
  public double getCacheHitRate() {
    if (totalChecks == null || totalChecks == 0) {
      return 0.0;
    }
    return (double) (cacheHits != null ? cacheHits : 0) / totalChecks;
  }

  /** Calcula a taxa de threshold breaches. Retorna 0 se não houver checks. */
  public double getThresholdBreachRate() {
    if (totalChecks == null || totalChecks == 0) {
      return 0.0;
    }
    return (double) (thresholdBreaches != null ? thresholdBreaches : 0) / totalChecks;
  }
}
