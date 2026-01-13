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
import org.hibernate.annotations.UpdateTimestamp;

@Entity
@Table(name = "merchant_chargebacks")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class MerchantChargeback {

  @Id
  @GeneratedValue(strategy = GenerationType.UUID)
  private UUID id;

  @Column(name = "merchant_id", nullable = false, unique = true, length = 100)
  private String merchantId;

  @Column(name = "merchant_name")
  private String merchantName;

  @Column(name = "mcc", length = 4)
  private String mcc;

  @Column(name = "total_transactions", nullable = false)
  @Builder.Default
  private Long totalTransactions = 0L;

  @Column(name = "total_chargebacks", nullable = false)
  @Builder.Default
  private Long totalChargebacks = 0L;

  @Column(name = "chargeback_rate", nullable = false, precision = 5, scale = 4)
  @Builder.Default
  private BigDecimal chargebackRate = BigDecimal.ZERO;

  @Column(name = "total_amount", nullable = false, precision = 18, scale = 2)
  @Builder.Default
  private BigDecimal totalAmount = BigDecimal.ZERO;

  @Column(name = "chargeback_amount", nullable = false, precision = 18, scale = 2)
  @Builder.Default
  private BigDecimal chargebackAmount = BigDecimal.ZERO;

  @Column(name = "last_chargeback_date")
  private OffsetDateTime lastChargebackDate;

  @Column(name = "first_transaction_date")
  private OffsetDateTime firstTransactionDate;

  @Column(name = "last_transaction_date")
  private OffsetDateTime lastTransactionDate;

  @Column(name = "risk_level", nullable = false, length = 20)
  @Builder.Default
  private String riskLevel = "LOW";

  @Column(name = "is_blocked", nullable = false)
  @Builder.Default
  private Boolean isBlocked = false;

  @CreationTimestamp
  @Column(name = "created_at", nullable = false, updatable = false)
  private OffsetDateTime createdAt;

  @UpdateTimestamp
  @Column(name = "updated_at", nullable = false)
  private OffsetDateTime updatedAt;
}
