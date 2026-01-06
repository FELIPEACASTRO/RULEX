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
 * Entidade para log de avaliação de regras em shadow mode. Registra o resultado da avaliação de
 * regras que estão em modo shadow (avaliam mas não atuam) para comparação com a decisão real.
 */
@Entity
@Table(
    name = "shadow_evaluation_log",
    indexes = {
      @Index(name = "idx_shadow_eval_rule_id", columnList = "rule_id"),
      @Index(name = "idx_shadow_eval_triggered", columnList = "rule_id, triggered"),
      @Index(name = "idx_shadow_eval_date", columnList = "evaluated_at")
    })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ShadowEvaluationLog {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "rule_id", nullable = false)
  private UUID ruleId;

  @Column(name = "transaction_id")
  private Long transactionId;

  @Column(name = "pan_hash", length = 64)
  private String panHash;

  // Evaluation result
  @Column(name = "triggered", nullable = false)
  @Builder.Default
  private Boolean triggered = false;

  @Column(name = "score")
  @Builder.Default
  private Integer score = 0;

  @Column(name = "recommended_action", length = 50)
  private String recommendedAction;

  // What actually happened (for comparison)
  @Column(name = "actual_decision", length = 50)
  private String actualDecision;

  @Column(name = "actual_score")
  private Integer actualScore;

  // Performance
  @Column(name = "latency_micros")
  private Long latencyMicros;

  // Metadata
  @Column(name = "evaluated_at")
  private OffsetDateTime evaluatedAt;

  @Column(name = "metadata", columnDefinition = "jsonb")
  @JdbcTypeCode(SqlTypes.JSON)
  private String metadata;

  @PrePersist
  protected void onCreate() {
    if (evaluatedAt == null) {
      evaluatedAt = OffsetDateTime.now();
    }
    if (triggered == null) {
      triggered = false;
    }
    if (score == null) {
      score = 0;
    }
  }

  /**
   * Verifica se a regra shadow teria mudado a decisão. Retorna true se a regra foi acionada mas a
   * decisão real foi diferente.
   */
  public boolean wouldHaveChangedDecision() {
    if (!Boolean.TRUE.equals(triggered)) {
      return false;
    }
    return recommendedAction != null && !recommendedAction.equals(actualDecision);
  }

  /**
   * Calcula a diferença de score entre a recomendação shadow e a decisão real. Retorna null se não
   * houver dados suficientes.
   */
  public Integer getScoreDifference() {
    if (score == null || actualScore == null) {
      return null;
    }
    return score - actualScore;
  }

  /** Verifica se a avaliação foi um falso positivo (shadow acionou, mas não deveria). */
  public boolean isFalsePositive() {
    return Boolean.TRUE.equals(triggered)
        && "APROVADO".equals(actualDecision);
  }

  /** Verifica se a avaliação foi um falso negativo (shadow não acionou, mas deveria). */
  public boolean isFalseNegative() {
    return !Boolean.TRUE.equals(triggered)
        && ("FRAUDE".equals(actualDecision) || "SUSPEITA_DE_FRAUDE".equals(actualDecision));
  }
}
