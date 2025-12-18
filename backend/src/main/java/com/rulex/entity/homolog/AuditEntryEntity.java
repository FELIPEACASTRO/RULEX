package com.rulex.entity.homolog;

import jakarta.persistence.*;
import java.time.OffsetDateTime;
import java.util.UUID;
import lombok.*;
import org.hibernate.annotations.UuidGenerator;

@Entity
@Table(
    name = "audit_log",
    indexes = {
      @Index(name = "idx_audit_log_created_at", columnList = "created_at"),
      @Index(name = "idx_audit_log_action_type", columnList = "action_type")
    })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class AuditEntryEntity {

  @Id @UuidGenerator @GeneratedValue private UUID id;

  @Enumerated(EnumType.STRING)
  @Column(name = "action_type", nullable = false)
  private AuditActionType actionType;

  @Column(name = "entity_type")
  private String entityType;

  @Column(name = "entity_id")
  private UUID entityId;

  @Column(name = "performed_by")
  private UUID performedBy;

  @Column(name = "diff_json", columnDefinition = "jsonb")
  private String diffJson;

  @Column(name = "details_json", columnDefinition = "jsonb")
  private String detailsJson;

  @Enumerated(EnumType.STRING)
  @Column(nullable = false)
  private AuditResult result;

  @Column(name = "error_message")
  private String errorMessage;

  @Column(name = "created_at", nullable = false)
  private OffsetDateTime createdAt;

  @PrePersist
  void onCreate() {
    if (createdAt == null) {
      createdAt = OffsetDateTime.now();
    }
    if (result == null) {
      result = AuditResult.SUCCESS;
    }
  }
}
