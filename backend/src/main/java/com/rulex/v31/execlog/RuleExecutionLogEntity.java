package com.rulex.v31.execlog;

import com.fasterxml.jackson.databind.JsonNode;
import com.rulex.entity.homolog.DecisionOutcome;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Id;
import jakarta.persistence.PrePersist;
import jakarta.persistence.Table;
import java.time.OffsetDateTime;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;

@Entity
@Table(name = "rule_execution_log")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RuleExecutionLogEntity {

  @Id
  @Column(name = "id", columnDefinition = "uuid")
  private UUID id;

  @Enumerated(EnumType.STRING)
  @JdbcTypeCode(SqlTypes.NAMED_ENUM)
  @Column(name = "event_type", nullable = false, columnDefinition = "execution_event_type")
  private ExecutionEventType eventType;

  @Column(name = "correlation_id")
  private String correlationId;

  @Column(name = "external_transaction_id", length = 64)
  private String externalTransactionId;

  @Column(name = "payload_raw_hash", length = 64)
  private String payloadRawHash;

  @Column(name = "attempted_payload_hash", length = 64)
  private String attemptedPayloadHash;

  @Column(name = "is_tamper", nullable = false)
  private boolean tamper;

  @Column(name = "ruleset_version_id", columnDefinition = "uuid")
  private UUID rulesetVersionId;

  @Column(name = "refdata_version_id", columnDefinition = "uuid")
  private UUID refdataVersionId;

  @Enumerated(EnumType.STRING)
  @JdbcTypeCode(SqlTypes.NAMED_ENUM)
  @Column(name = "decision", nullable = false, columnDefinition = "decision_outcome")
  private DecisionOutcome decision;

  @Column(name = "risk_score", nullable = false)
  private int riskScore;

  @JdbcTypeCode(SqlTypes.JSON)
  @Column(name = "rules_fired_json", nullable = false, columnDefinition = "jsonb")
  private JsonNode rulesFiredJson;

  @JdbcTypeCode(SqlTypes.JSON)
  @Column(name = "decision_path_json", nullable = false, columnDefinition = "jsonb")
  private JsonNode decisionPathJson;

  @JdbcTypeCode(SqlTypes.JSON)
  @Column(name = "why_not_fired_json", columnDefinition = "jsonb")
  private JsonNode whyNotFiredJson;

  @JdbcTypeCode(SqlTypes.JSON)
  @Column(name = "context_flags_json", columnDefinition = "jsonb")
  private JsonNode contextFlagsJson;

  @JdbcTypeCode(SqlTypes.JSON)
  @Column(name = "error_json", columnDefinition = "jsonb")
  private JsonNode errorJson;

  @Column(name = "created_at", nullable = false)
  private OffsetDateTime createdAt;

  @PrePersist
  void onCreate() {
    if (id == null) {
      id = UUID.randomUUID();
    }
    if (createdAt == null) {
      createdAt = OffsetDateTime.now();
    }
  }
}
