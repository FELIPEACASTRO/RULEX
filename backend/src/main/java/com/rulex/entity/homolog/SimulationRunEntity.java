package com.rulex.entity.homolog;

import jakarta.persistence.*;
import java.time.OffsetDateTime;
import java.util.UUID;
import lombok.*;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.annotations.UuidGenerator;
import org.hibernate.type.SqlTypes;

@Entity
@Table(name = "simulation_runs")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SimulationRunEntity {

  @Id @UuidGenerator @GeneratedValue private UUID id;

  @Column private String name;

  @Column(name = "requested_by")
  private UUID requestedBy;

  @Column(name = "rule_set_version_id")
  private UUID ruleSetVersionId;

  @JdbcTypeCode(SqlTypes.JSON)
  @Column(name = "payload_json", nullable = false, columnDefinition = "jsonb")
  private String payloadJson;

  @Enumerated(EnumType.STRING)
  @JdbcTypeCode(SqlTypes.NAMED_ENUM)
  @Column(nullable = false, columnDefinition = "decision_outcome")
  private DecisionOutcome decision;

  @Column(name = "risk_score", nullable = false)
  private Integer riskScore;

  @JdbcTypeCode(SqlTypes.JSON)
  @Column(name = "triggered_rules_json", nullable = false, columnDefinition = "jsonb")
  private String triggeredRulesJson;

  @JdbcTypeCode(SqlTypes.JSON)
  @Column(name = "explain_json", nullable = false, columnDefinition = "jsonb")
  private String explainJson;

  @Column(name = "created_at", nullable = false)
  private OffsetDateTime createdAt;

  @PrePersist
  void onCreate() {
    if (createdAt == null) {
      createdAt = OffsetDateTime.now();
    }
  }
}
