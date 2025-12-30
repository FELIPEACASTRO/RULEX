package com.rulex.entity.complex;

import jakarta.persistence.*;
import java.time.OffsetDateTime;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

/** Entidade para detalhes granulares da execução de cada condição para auditoria. */
@Entity
@Table(
    name = "rule_execution_details",
    indexes = {
      @Index(name = "idx_exec_details_decision_log", columnList = "decision_log_id"),
      @Index(name = "idx_exec_details_rule_version", columnList = "rule_version_id")
    })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RuleExecutionDetail {

  @Id
  @GeneratedValue(strategy = GenerationType.UUID)
  private UUID id;

  @Column(name = "decision_log_id", nullable = false)
  private UUID decisionLogId;

  @Column(name = "rule_version_id", nullable = false)
  private UUID ruleVersionId;

  @Column(name = "condition_id")
  private UUID conditionId;

  @Column(name = "group_id")
  private UUID groupId;

  @Column(name = "field_name")
  private String fieldName;

  @Column(name = "field_value")
  private String fieldValue;

  private String operator;

  @Column(name = "expected_value")
  private String expectedValue;

  @Column(nullable = false)
  private Boolean result;

  @Column(name = "execution_time_ms")
  private Integer executionTimeMs;

  @Column(name = "error_message")
  private String errorMessage;

  @CreationTimestamp
  @Column(name = "created_at", nullable = false, updatable = false)
  private OffsetDateTime createdAt;
}
