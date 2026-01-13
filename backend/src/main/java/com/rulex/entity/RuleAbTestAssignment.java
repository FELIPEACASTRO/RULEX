package com.rulex.entity;

import jakarta.persistence.*;
import java.time.OffsetDateTime;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Entidade para assignments de testes A/B. Registra a qual grupo (controle ou tratamento) cada
 * transação foi atribuída e o resultado da avaliação.
 */
@Entity
@Table(
    name = "rule_ab_test_assignments",
    indexes = {
      @Index(name = "idx_ab_test_id", columnList = "test_id"),
      @Index(name = "idx_ab_test_pan", columnList = "test_id, pan_hash")
    })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RuleAbTestAssignment {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "test_id", nullable = false)
  private Long testId;

  @Column(name = "transaction_id", nullable = false)
  private Long transactionId;

  @Column(name = "pan_hash", nullable = false, length = 64)
  private String panHash;

  @Column(name = "assigned_group", nullable = false, length = 20)
  @Enumerated(EnumType.STRING)
  private AssignedGroup assignedGroup;

  @Column(name = "evaluated_rule_id", nullable = false)
  private UUID evaluatedRuleId;

  // Result
  @Column(name = "triggered")
  @Builder.Default
  private Boolean triggered = false;

  @Column(name = "score")
  @Builder.Default
  private Integer score = 0;

  @Column(name = "action", length = 50)
  private String action;

  @Column(name = "created_at")
  private OffsetDateTime createdAt;

  @PrePersist
  protected void onCreate() {
    if (createdAt == null) {
      createdAt = OffsetDateTime.now();
    }
    if (triggered == null) {
      triggered = false;
    }
    if (score == null) {
      score = 0;
    }
  }

  /** Verifica se esta atribuição é do grupo de controle. */
  public boolean isControl() {
    return assignedGroup == AssignedGroup.CONTROL;
  }

  /** Verifica se esta atribuição é do grupo de tratamento. */
  public boolean isTreatment() {
    return assignedGroup == AssignedGroup.TREATMENT;
  }

  /** Grupos de atribuição para testes A/B. */
  public enum AssignedGroup {
    /** Grupo de controle (regra existente) */
    CONTROL,
    /** Grupo de tratamento (nova regra sendo testada) */
    TREATMENT
  }
}
