package com.rulex.entity;

import jakarta.persistence.*;
import java.math.BigDecimal;
import java.time.OffsetDateTime;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.UpdateTimestamp;

@Entity
@Table(name = "customer_last_transaction")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CustomerLastTransaction {

  @Id
  @GeneratedValue(strategy = GenerationType.UUID)
  private UUID id;

  @Column(name = "customer_id", nullable = false, unique = true, length = 100)
  private String customerId;

  @Column(name = "card_number_hash", length = 64)
  private String cardNumberHash;

  @Column(name = "last_transaction_id", length = 100)
  private String lastTransactionId;

  @Column(name = "last_transaction_date", nullable = false)
  private OffsetDateTime lastTransactionDate;

  @Column(name = "last_transaction_amount", precision = 18, scale = 2)
  private BigDecimal lastTransactionAmount;

  @Column(name = "last_merchant_id", length = 100)
  private String lastMerchantId;

  @Column(name = "last_merchant_name")
  private String lastMerchantName;

  @Column(name = "last_mcc", length = 4)
  private String lastMcc;

  @Column(name = "last_country_code", length = 3)
  private String lastCountryCode;

  @Column(name = "last_city", length = 100)
  private String lastCity;

  @Column(name = "transaction_count_today", nullable = false)
  @Builder.Default
  private Long transactionCountToday = 0L;

  @Column(name = "transaction_count_week", nullable = false)
  @Builder.Default
  private Long transactionCountWeek = 0L;

  @Column(name = "transaction_count_month", nullable = false)
  @Builder.Default
  private Long transactionCountMonth = 0L;

  @UpdateTimestamp
  @Column(name = "updated_at", nullable = false)
  private OffsetDateTime updatedAt;
}
