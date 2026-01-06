package com.rulex.entity;

import jakarta.persistence.*;
import java.time.OffsetDateTime;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;

/**
 * Entidade para testes A/B de regras. Permite comparar o desempenho de duas regras (controle vs
 * tratamento) em um subconjunto do tráfego.
 */
@Entity
@Table(name = "rule_ab_tests")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RuleAbTest {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "name", nullable = false, length = 200)
  private String name;

  @Column(name = "description", columnDefinition = "TEXT")
  private String description;

  // Control group (existing rule)
  @Column(name = "control_rule_id", nullable = false)
  private UUID controlRuleId;

  // Treatment group (new rule to test)
  @Column(name = "treatment_rule_id", nullable = false)
  private UUID treatmentRuleId;

  // Traffic split
  @Column(name = "treatment_percentage", nullable = false)
  @Builder.Default
  private Integer treatmentPercentage = 50;

  // Targeting (optional)
  @Column(name = "target_segment", columnDefinition = "jsonb")
  @JdbcTypeCode(SqlTypes.JSON)
  private String targetSegment;

  // Status
  @Column(name = "status", length = 20)
  @Enumerated(EnumType.STRING)
  @Builder.Default
  private AbTestStatus status = AbTestStatus.DRAFT;

  @Column(name = "started_at")
  private OffsetDateTime startedAt;

  @Column(name = "ended_at")
  private OffsetDateTime endedAt;

  // Results
  @Column(name = "control_triggers")
  @Builder.Default
  private Long controlTriggers = 0L;

  @Column(name = "treatment_triggers")
  @Builder.Default
  private Long treatmentTriggers = 0L;

  @Column(name = "control_false_positives")
  @Builder.Default
  private Long controlFalsePositives = 0L;

  @Column(name = "treatment_false_positives")
  @Builder.Default
  private Long treatmentFalsePositives = 0L;

  @Column(name = "created_by", length = 100)
  private String createdBy;

  @Column(name = "created_at")
  private OffsetDateTime createdAt;

  @PrePersist
  protected void onCreate() {
    if (createdAt == null) {
      createdAt = OffsetDateTime.now();
    }
    if (status == null) {
      status = AbTestStatus.DRAFT;
    }
    if (treatmentPercentage == null) {
      treatmentPercentage = 50;
    }
    initializeCounters();
  }

  private void initializeCounters() {
    if (controlTriggers == null) controlTriggers = 0L;
    if (treatmentTriggers == null) treatmentTriggers = 0L;
    if (controlFalsePositives == null) controlFalsePositives = 0L;
    if (treatmentFalsePositives == null) treatmentFalsePositives = 0L;
  }

  /** Inicia o teste A/B. */
  public void start() {
    this.status = AbTestStatus.RUNNING;
    this.startedAt = OffsetDateTime.now();
  }

  /** Pausa o teste A/B. */
  public void pause() {
    this.status = AbTestStatus.PAUSED;
  }

  /** Retoma o teste A/B pausado. */
  public void resume() {
    this.status = AbTestStatus.RUNNING;
  }

  /** Finaliza o teste A/B. */
  public void complete() {
    this.status = AbTestStatus.COMPLETED;
    this.endedAt = OffsetDateTime.now();
  }

  /** Verifica se o teste está ativo. */
  public boolean isActive() {
    return status == AbTestStatus.RUNNING;
  }

  /** Calcula a taxa de trigger do grupo de controle. */
  public double getControlTriggerRate(long totalControlSamples) {
    if (totalControlSamples == 0) return 0.0;
    return (double) controlTriggers / totalControlSamples;
  }

  /** Calcula a taxa de trigger do grupo de tratamento. */
  public double getTreatmentTriggerRate(long totalTreatmentSamples) {
    if (totalTreatmentSamples == 0) return 0.0;
    return (double) treatmentTriggers / totalTreatmentSamples;
  }

  /** Calcula a taxa de falsos positivos do grupo de controle. */
  public double getControlFalsePositiveRate() {
    if (controlTriggers == null || controlTriggers == 0) return 0.0;
    return (double) controlFalsePositives / controlTriggers;
  }

  /** Calcula a taxa de falsos positivos do grupo de tratamento. */
  public double getTreatmentFalsePositiveRate() {
    if (treatmentTriggers == null || treatmentTriggers == 0) return 0.0;
    return (double) treatmentFalsePositives / treatmentTriggers;
  }

  /** Status do teste A/B. */
  public enum AbTestStatus {
    /** Teste criado mas não iniciado */
    DRAFT,
    /** Teste em execução */
    RUNNING,
    /** Teste pausado */
    PAUSED,
    /** Teste finalizado */
    COMPLETED
  }
}
