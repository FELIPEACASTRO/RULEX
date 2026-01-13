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
@Table(name = "customer_account_info")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CustomerAccountInfo {

  @Id
  @GeneratedValue(strategy = GenerationType.UUID)
  private UUID id;

  @Column(name = "customer_id", nullable = false, unique = true, length = 100)
  private String customerId;

  @Column(name = "account_number", length = 50)
  private String accountNumber;

  @Column(name = "account_created_at", nullable = false)
  private OffsetDateTime accountCreatedAt;

  @Column(name = "account_type", length = 50)
  private String accountType;

  @Column(name = "kyc_level", nullable = false, length = 20)
  @Builder.Default
  private String kycLevel = "BASIC";

  @Column(name = "kyc_verified_at")
  private OffsetDateTime kycVerifiedAt;

  @Column(name = "first_transaction_at")
  private OffsetDateTime firstTransactionAt;

  @Column(name = "total_transactions", nullable = false)
  @Builder.Default
  private Long totalTransactions = 0L;

  @Column(name = "total_amount", nullable = false, precision = 18, scale = 2)
  @Builder.Default
  private BigDecimal totalAmount = BigDecimal.ZERO;

  @Column(name = "risk_score", nullable = false)
  @Builder.Default
  private Integer riskScore = 0;

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
