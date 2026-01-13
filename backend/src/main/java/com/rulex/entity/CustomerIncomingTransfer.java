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
@Table(name = "customer_incoming_transfers")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CustomerIncomingTransfer {

  @Id
  @GeneratedValue(strategy = GenerationType.UUID)
  private UUID id;

  @Column(name = "customer_id", nullable = false, length = 100)
  private String customerId;

  @Column(name = "account_number", length = 50)
  private String accountNumber;

  @Column(name = "transfer_amount", nullable = false, precision = 18, scale = 2)
  private BigDecimal transferAmount;

  @Column(name = "transfer_date", nullable = false)
  private OffsetDateTime transferDate;

  @Column(name = "transfer_type", nullable = false, length = 50)
  private String transferType;

  @Column(name = "source_bank", length = 10)
  private String sourceBank;

  @Column(name = "source_account", length = 50)
  private String sourceAccount;

  @Column(name = "source_name")
  private String sourceName;

  @Column(name = "description", length = 500)
  private String description;

  @CreationTimestamp
  @Column(name = "created_at", nullable = false, updatable = false)
  private OffsetDateTime createdAt;
}
