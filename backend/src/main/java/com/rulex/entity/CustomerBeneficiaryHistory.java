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
@Table(name = "customer_beneficiary_history")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CustomerBeneficiaryHistory {

  @Id
  @GeneratedValue(strategy = GenerationType.UUID)
  private UUID id;

  @Column(name = "customer_id", nullable = false, length = 100)
  private String customerId;

  @Column(name = "beneficiary_id", nullable = false, length = 100)
  private String beneficiaryId;

  @Column(name = "beneficiary_name")
  private String beneficiaryName;

  @Column(name = "beneficiary_document", length = 20)
  private String beneficiaryDocument;

  @Column(name = "beneficiary_bank", length = 10)
  private String beneficiaryBank;

  @Column(name = "beneficiary_account", length = 50)
  private String beneficiaryAccount;

  @Column(name = "beneficiary_type", length = 50)
  private String beneficiaryType;

  @Column(name = "first_transaction_date", nullable = false)
  private OffsetDateTime firstTransactionDate;

  @Column(name = "last_transaction_date", nullable = false)
  private OffsetDateTime lastTransactionDate;

  @Column(name = "transaction_count", nullable = false)
  @Builder.Default
  private Long transactionCount = 1L;

  @Column(name = "total_amount", nullable = false, precision = 18, scale = 2)
  @Builder.Default
  private BigDecimal totalAmount = BigDecimal.ZERO;

  @Column(name = "is_trusted", nullable = false)
  @Builder.Default
  private Boolean isTrusted = false;

  @CreationTimestamp
  @Column(name = "created_at", nullable = false, updatable = false)
  private OffsetDateTime createdAt;

  @UpdateTimestamp
  @Column(name = "updated_at", nullable = false)
  private OffsetDateTime updatedAt;
}
