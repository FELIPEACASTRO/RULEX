package com.rulex.entity;

import jakarta.persistence.*;
import java.math.BigDecimal;
import java.time.OffsetDateTime;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

@Entity
@Table(name = "customer_chargeback_history")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CustomerChargebackHistory {

  @Id
  @GeneratedValue(strategy = GenerationType.UUID)
  private UUID id;

  @Column(name = "customer_id", nullable = false, length = 100)
  private String customerId;

  @Column(name = "merchant_id", nullable = false, length = 100)
  private String merchantId;

  @Column(name = "transaction_id", length = 100)
  private String transactionId;

  @Column(name = "chargeback_date", nullable = false)
  private OffsetDateTime chargebackDate;

  @Column(name = "chargeback_amount", nullable = false, precision = 18, scale = 2)
  private BigDecimal chargebackAmount;

  @Column(name = "chargeback_reason", length = 100)
  private String chargebackReason;

  @Column(name = "chargeback_code", length = 20)
  private String chargebackCode;

  @Column(name = "status", nullable = false, length = 20)
  @Builder.Default
  private String status = "PENDING";

  @CreationTimestamp
  @Column(name = "created_at", nullable = false, updatable = false)
  private OffsetDateTime createdAt;
}
