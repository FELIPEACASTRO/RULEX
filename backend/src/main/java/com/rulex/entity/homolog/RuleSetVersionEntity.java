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
    name = "rule_set_versions",
    uniqueConstraints = {
      @UniqueConstraint(
          name = "uk_ruleset_version",
          columnNames = {"rule_set_id", "version"})
    })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RuleSetVersionEntity {

  @Id @UuidGenerator @GeneratedValue private UUID id;

  @Column(name = "rule_set_id", nullable = false)
  private UUID ruleSetId;

  @Column(nullable = false)
  private Integer version;

  @Enumerated(EnumType.STRING)
  @JdbcTypeCode(SqlTypes.NAMED_ENUM)
  @Column(nullable = false, columnDefinition = "rule_status")
  private RuleStatus status;

  @Column private String notes;

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
  }
}
