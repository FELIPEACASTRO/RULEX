package com.rulex.entity.homolog;

import jakarta.persistence.*;
import java.math.BigDecimal;
import java.time.OffsetDateTime;
import java.util.UUID;
import lombok.*;
import org.hibernate.annotations.UuidGenerator;

@Entity
@Table(
    name = "decision_log",
    indexes = {
      @Index(name = "idx_decision_log_customer_id", columnList = "customer_id"),
      @Index(name = "idx_decision_log_created_at", columnList = "created_at")
    })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class DecisionLogEntity {

  @Id @UuidGenerator @GeneratedValue private UUID id;

  @Column(name = "external_transaction_id", nullable = false)
  private String externalTransactionId;

  @Column(name = "customer_id", nullable = false)
  private String customerId;

  @Column(name = "merchant_id")
  private String merchantId;

  @Column(name = "amount", precision = 18, scale = 2)
  private BigDecimal amount;

  @Column(name = "currency_code")
  private Integer currencyCode;

  @Enumerated(EnumType.STRING)
  @Column(nullable = false)
  private DecisionOutcome decision;

  @Column(name = "risk_score", nullable = false)
  private Integer riskScore;

  @Column(name = "triggered_rules_json", nullable = false, columnDefinition = "jsonb")
  private String triggeredRulesJson;

  @Column(name = "explain_json", nullable = false, columnDefinition = "jsonb")
  private String explainJson;

  @Column(name = "payload_json", nullable = false, columnDefinition = "jsonb")
  private String payloadJson;

  @Column(name = "pan_masked")
  private String panMasked;

  @Column(name = "ruleset_version_id")
  private UUID rulesetVersionId;

  @Column(name = "created_at", nullable = false)
  private OffsetDateTime createdAt;

  @PrePersist
  void onCreate() {
    if (createdAt == null) {
      createdAt = OffsetDateTime.now();
    }
  }
}
