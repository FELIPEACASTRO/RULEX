package com.rulex.entity;

import jakarta.persistence.*;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Entidade para métricas de performance e eficácia das regras.
 * Permite análise de falsos positivos e otimização de regras.
 */
@Entity
@Table(
    name = "rule_metrics",
    indexes = {
      @Index(name = "idx_rule_metrics_date", columnList = "metric_date"),
      @Index(name = "idx_rule_metrics_rule_id", columnList = "rule_id")
    },
    uniqueConstraints = {
      @UniqueConstraint(name = "uk_rule_metrics_rule_date", columnNames = {"rule_id", "metric_date"})
    })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RuleMetrics {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "rule_id", nullable = false)
  private Long ruleId;

  @Column(name = "rule_name", nullable = false, length = 100)
  private String ruleName;

  @Column(name = "metric_date", nullable = false)
  private LocalDate metricDate;

  @Column(name = "total_evaluations")
  @Builder.Default
  private Long totalEvaluations = 0L;

  @Column(name = "total_triggered")
  @Builder.Default
  private Long totalTriggered = 0L;

  @Column(name = "true_positives")
  @Builder.Default
  private Long truePositives = 0L;

  @Column(name = "false_positives")
  @Builder.Default
  private Long falsePositives = 0L;

  @Column(name = "total_amount_blocked", precision = 19, scale = 2)
  @Builder.Default
  private BigDecimal totalAmountBlocked = BigDecimal.ZERO;

  @Column(name = "avg_processing_time_ms", precision = 10, scale = 2)
  @Builder.Default
  private BigDecimal avgProcessingTimeMs = BigDecimal.ZERO;

  @Column(name = "created_at", nullable = false, updatable = false)
  private LocalDateTime createdAt;

  @Column(name = "updated_at", nullable = false)
  private LocalDateTime updatedAt;

  @PrePersist
  protected void onCreate() {
    LocalDateTime now = LocalDateTime.now();
    if (createdAt == null) {
      createdAt = now;
    }
    if (updatedAt == null) {
      updatedAt = now;
    }
  }

  @PreUpdate
  protected void onUpdate() {
    updatedAt = LocalDateTime.now();
  }

  /**
   * Calcula a taxa de disparo (trigger rate).
   */
  public double getTriggerRate() {
    if (totalEvaluations == null || totalEvaluations == 0) {
      return 0.0;
    }
    return (double) totalTriggered / totalEvaluations * 100;
  }

  /**
   * Calcula a taxa de falsos positivos.
   */
  public double getFalsePositiveRate() {
    if (totalTriggered == null || totalTriggered == 0) {
      return 0.0;
    }
    return (double) falsePositives / totalTriggered * 100;
  }

  /**
   * Calcula a precisão da regra.
   */
  public double getPrecision() {
    if (totalTriggered == null || totalTriggered == 0) {
      return 0.0;
    }
    return (double) truePositives / totalTriggered * 100;
  }

  /**
   * Incrementa contadores de avaliação.
   */
  public void incrementEvaluation(boolean triggered, BigDecimal amount) {
    this.totalEvaluations++;
    if (triggered) {
      this.totalTriggered++;
      if (amount != null) {
        this.totalAmountBlocked = this.totalAmountBlocked.add(amount);
      }
    }
  }

  /**
   * Registra feedback de falso positivo.
   */
  public void recordFalsePositive() {
    this.falsePositives++;
  }

  /**
   * Registra feedback de verdadeiro positivo.
   */
  public void recordTruePositive() {
    this.truePositives++;
  }
}
