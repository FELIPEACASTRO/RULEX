package com.rulex.entity;

import jakarta.persistence.*;
import java.time.LocalDateTime;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Entidade para workflow de aprovação de regras (4 olhos). Implementa segregação de funções para
 * compliance.
 */
@Entity
@Table(
    name = "rule_approvals",
    indexes = {
      @Index(name = "idx_rule_approvals_status", columnList = "status"),
      @Index(name = "idx_rule_approvals_rule_id", columnList = "rule_id"),
      @Index(name = "idx_rule_approvals_requested_by", columnList = "requested_by")
    })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RuleApproval {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "rule_id", nullable = false)
  private Long ruleId;

  @Column(name = "rule_name", nullable = false, length = 100)
  private String ruleName;

  @Column(name = "action_type", nullable = false, length = 20)
  @Enumerated(EnumType.STRING)
  private ActionType actionType;

  @Column(name = "requested_by", nullable = false, length = 100)
  private String requestedBy;

  @Column(name = "requested_at", nullable = false)
  private LocalDateTime requestedAt;

  @Column(name = "approved_by", length = 100)
  private String approvedBy;

  @Column(name = "approved_at")
  private LocalDateTime approvedAt;

  @Column(name = "rejected_by", length = 100)
  private String rejectedBy;

  @Column(name = "rejected_at")
  private LocalDateTime rejectedAt;

  @Column(nullable = false, length = 20)
  @Enumerated(EnumType.STRING)
  private ApprovalStatus status;

  @Column(name = "payload_json", columnDefinition = "TEXT")
  private String payloadJson;

  @Column(columnDefinition = "TEXT")
  private String comments;

  @Column(name = "client_ip", length = 45)
  private String clientIp;

  @PrePersist
  protected void onCreate() {
    if (requestedAt == null) {
      requestedAt = LocalDateTime.now();
    }
    if (status == null) {
      status = ApprovalStatus.PENDING;
    }
  }

  public enum ActionType {
    CREATE("Criação"),
    UPDATE("Atualização"),
    DELETE("Exclusão"),
    TOGGLE("Ativação/Desativação");

    private final String label;

    ActionType(String label) {
      this.label = label;
    }

    public String getLabel() {
      return label;
    }
  }

  public enum ApprovalStatus {
    PENDING("Pendente"),
    APPROVED("Aprovado"),
    REJECTED("Rejeitado"),
    CANCELLED("Cancelado");

    private final String label;

    ApprovalStatus(String label) {
      this.label = label;
    }

    public String getLabel() {
      return label;
    }
  }
}
