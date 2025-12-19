package com.rulex.entity;

import jakarta.persistence.*;
import java.time.LocalDateTime;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Entidade que registra todas as ações para auditoria e compliance. Inclui processamento de
 * transações, alterações de configurações e decisões.
 */
@Entity
@Table(
    name = "audit_logs",
    indexes = {
      @Index(name = "idx_transaction_id", columnList = "transaction_id"),
      @Index(name = "idx_action_type", columnList = "action_type"),
      @Index(name = "idx_audit_date", columnList = "created_at")
    })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class AuditLog {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  /** ID da transação associada (se aplicável) */
  @Column(name = "transaction_id")
  private Long transactionId;

  /**
   * Tipo de ação: TRANSACTION_PROCESSED, RULE_CREATED, RULE_UPDATED, RULE_DELETED, CONFIG_CHANGED
   */
  @Column(nullable = false, length = 50)
  @Enumerated(EnumType.STRING)
  private AuditActionType actionType;

  /** Descrição da ação */
  @Column(columnDefinition = "TEXT")
  private String description;

  /** Detalhes adicionais em JSON */
  @Column(columnDefinition = "TEXT")
  private String details;

  /** Usuário que realizou a ação (se aplicável) */
  @Column(length = 100)
  private String performedBy;

  /** Resultado da ação: SUCCESS, FAILURE */
  @Column(nullable = false, length = 20)
  @Enumerated(EnumType.STRING)
  private AuditResult result;

  /** Mensagem de erro (se houver) */
  @Column(columnDefinition = "TEXT")
  private String errorMessage;

  /** IP de origem (se aplicável) */
  @Column(length = 45)
  private String sourceIp;

  /** Timestamp da ação */
  @Column(nullable = false, updatable = false)
  private LocalDateTime createdAt;

  @PrePersist
  protected void onCreate() {
    if (createdAt == null) {
      createdAt = LocalDateTime.now();
    }
  }

  public enum AuditActionType {
    TRANSACTION_PROCESSED("Transação Processada"),
    RULE_CREATED("Regra Criada"),
    RULE_UPDATED("Regra Atualizada"),
    RULE_DELETED("Regra Deletada"),
    CONFIG_CHANGED("Configuração Alterada"),
    DECISION_MADE("Decisão Tomada"),
    REPORT_GENERATED("Relatório Gerado");

    private final String label;

    AuditActionType(String label) {
      this.label = label;
    }

    public String getLabel() {
      return label;
    }
  }

  public enum AuditResult {
    SUCCESS("Sucesso"),
    FAILURE("Falha");

    private final String label;

    AuditResult(String label) {
      this.label = label;
    }

    public String getLabel() {
      return label;
    }
  }
}
