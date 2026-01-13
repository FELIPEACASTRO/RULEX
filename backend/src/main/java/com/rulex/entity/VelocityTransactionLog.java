package com.rulex.entity;

import jakarta.persistence.*;
import java.math.BigDecimal;
import java.time.OffsetDateTime;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/** Log de transações otimizado para agregações de velocidade. */
@Entity
@Table(name = "velocity_transaction_log")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class VelocityTransactionLog {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "external_transaction_id", nullable = false, unique = true, length = 64)
  private String externalTransactionId;

  @Column(name = "pan_hash", nullable = false, length = 64)
  private String panHash;

  @Column(name = "customer_id", length = 64)
  private String customerId;

  @Column(name = "merchant_id", length = 64)
  private String merchantId;

  @Column(name = "amount", nullable = false, precision = 18, scale = 2)
  private BigDecimal amount;

  @Column(name = "currency_code")
  private Integer currencyCode;

  @Column(name = "mcc")
  private Integer mcc;

  @Column(name = "merchant_country", length = 10)
  private String merchantCountry;

  @Column(name = "decision", length = 20)
  private String decision;

  @Column(name = "risk_score")
  private Integer riskScore;

  @Column(name = "transaction_at", nullable = false)
  private OffsetDateTime transactionAt;

  @Column(name = "created_at")
  private OffsetDateTime createdAt;

  @PrePersist
  protected void onCreate() {
    createdAt = OffsetDateTime.now();
  }
}
