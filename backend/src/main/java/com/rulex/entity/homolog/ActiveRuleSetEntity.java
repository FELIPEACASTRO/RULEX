package com.rulex.entity.homolog;

import jakarta.persistence.*;
import java.time.OffsetDateTime;
import java.util.UUID;
import lombok.*;

@Entity
@Table(name = "active_rule_set")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ActiveRuleSetEntity {

  @Id private Short id;

  @Column(name = "rule_set_version_id", nullable = false)
  private UUID ruleSetVersionId;

  @Column(name = "activated_by")
  private UUID activatedBy;

  @Column(name = "activated_at", nullable = false)
  private OffsetDateTime activatedAt;

  @PrePersist
  void onCreate() {
    if (id == null) {
      id = 1;
    }
    if (activatedAt == null) {
      activatedAt = OffsetDateTime.now();
    }
  }
}
