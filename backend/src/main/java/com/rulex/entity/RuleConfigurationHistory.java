package com.rulex.entity;

import jakarta.persistence.*;
import java.time.LocalDateTime;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/** Histórico de mudanças de RuleConfiguration (append-only). */
@Entity
@Table(
    name = "rule_configuration_history",
    indexes = {
      @Index(name = "idx_rule_hist_rule_id", columnList = "rule_id"),
      @Index(name = "idx_rule_hist_created_at", columnList = "created_at")
    })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RuleConfigurationHistory {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "rule_id", nullable = false)
  private Long ruleId;

  @Column(name = "rule_name", nullable = false, length = 100)
  private String ruleName;

  @Column(nullable = false)
  private Integer version;

  @Column(name = "previous_json", columnDefinition = "TEXT")
  private String previousJson;

  @Column(name = "current_json", columnDefinition = "TEXT")
  private String currentJson;

  @Column(name = "performed_by", length = 100)
  private String performedBy;

  @Column(name = "client_ip", length = 45)
  private String clientIp;

  @Column(name = "user_agent", length = 500)
  private String userAgent;

  @Column(name = "action_type", length = 20)
  private String actionType;

  @Column(name = "created_at", nullable = false, updatable = false)
  private LocalDateTime createdAt;

  @PrePersist
  protected void onCreate() {
    if (createdAt == null) {
      createdAt = LocalDateTime.now();
    }
    if (actionType == null) {
      if (previousJson == null && currentJson != null) {
        actionType = "CREATE";
      } else if (previousJson != null && currentJson == null) {
        actionType = "DELETE";
      } else {
        actionType = "UPDATE";
      }
    }
  }
}
