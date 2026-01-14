package com.rulex.entity.homolog;

import jakarta.persistence.*;
import java.time.OffsetDateTime;
import java.util.UUID;
import lombok.*;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.annotations.UuidGenerator;
import org.hibernate.type.SqlTypes;

@Entity
@Table(
    name = "rule_versions",
    uniqueConstraints = {
      @UniqueConstraint(
          name = "uk_rule_version",
          columnNames = {"rule_id", "version"})
    })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RuleVersionEntity {

  @Id @UuidGenerator @GeneratedValue private UUID id;

  @Column(name = "rule_id", nullable = false)
  private UUID ruleId;

  @Column(nullable = false)
  private Integer version;

  @Enumerated(EnumType.STRING)
  @JdbcTypeCode(SqlTypes.NAMED_ENUM)
  @Column(nullable = false, columnDefinition = "rule_status")
  private RuleStatus status;

  @Column(nullable = false)
  private Integer priority;

  @Column(nullable = false)
  private Integer severity;

  @Enumerated(EnumType.STRING)
  @JdbcTypeCode(SqlTypes.NAMED_ENUM)
  @Column(nullable = false, columnDefinition = "decision_outcome")
  private DecisionOutcome decision;

  @Column(name = "reason_template", nullable = false)
  private String reasonTemplate;

  @Column(name = "fields_used", nullable = false)
  private String[] fieldsUsed;

  @Enumerated(EnumType.STRING)
  @JdbcTypeCode(SqlTypes.NAMED_ENUM)
  @Column(nullable = false, columnDefinition = "logic_operator")
  private LogicOperator logic;

  @JdbcTypeCode(SqlTypes.JSON)
  @Column(name = "conditions_json", nullable = false, columnDefinition = "jsonb")
  private String conditionsJson;

  @Column(nullable = false)
  private Boolean enabled;

  @Column(name = "created_by")
  private UUID createdBy;

  @Column(nullable = false)
  private OffsetDateTime createdAt;

  @PrePersist
  void onCreate() {
    if (createdAt == null) {
      createdAt = OffsetDateTime.now();
    }
    if (status == null) {
      status = RuleStatus.DRAFT;
    }
    if (logic == null) {
      logic = LogicOperator.AND;
    }
    if (enabled == null) {
      enabled = Boolean.TRUE;
    }
  }
}
