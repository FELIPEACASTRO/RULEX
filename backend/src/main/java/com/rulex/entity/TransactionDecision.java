package com.rulex.entity;

import jakarta.persistence.*;
import java.time.LocalDateTime;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Entidade que registra a decisão tomada para uma transação. Inclui a classificação, regras
 * aplicadas e scores calculados para auditoria.
 */
@Entity
@Table(
    name = "transaction_decisions",
    indexes = {
      @Index(name = "idx_transaction_id", columnList = "transaction_id"),
      @Index(name = "idx_classification", columnList = "classification"),
      @Index(name = "idx_decision_date", columnList = "created_at")
    })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class TransactionDecision {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "transaction_id", nullable = false)
  private Transaction transaction;

  /** Classificação da transação: APPROVED, SUSPICIOUS, FRAUD */
  @Column(nullable = false, length = 20)
  @Enumerated(EnumType.STRING)
  private TransactionClassification classification;

  /** Score de risco calculado (0-100) */
  @Column(nullable = false)
  private Integer riskScore;

  /** Descrição das regras aplicadas em JSON */
  @Column(columnDefinition = "TEXT")
  private String rulesApplied;

  /** Detalhes dos scores calculados em JSON */
  @Column(columnDefinition = "TEXT")
  private String scoreDetails;

  /** Motivo da decisão */
  @Column(columnDefinition = "TEXT")
  private String reason;

  /** Versão das regras utilizadas */
  @Column(length = 20)
  private String rulesVersion;

  /** Timestamp da decisão */
  @Column(nullable = false, updatable = false)
  private LocalDateTime createdAt;

  @PrePersist
  protected void onCreate() {
    if (createdAt == null) {
      createdAt = LocalDateTime.now();
    }
  }

  public enum TransactionClassification {
    APPROVED("Aprovada"),
    SUSPICIOUS("Suspeita de Fraude"),
    FRAUD("Fraude");

    private final String label;

    TransactionClassification(String label) {
      this.label = label;
    }

    public String getLabel() {
      return label;
    }
  }
}
