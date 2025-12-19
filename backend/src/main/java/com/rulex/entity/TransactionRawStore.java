package com.rulex.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Index;
import jakarta.persistence.Table;
import java.time.LocalDateTime;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(
    name = "transaction_raw_store",
    indexes = {
      @Index(name = "idx_transaction_raw_store_extid", columnList = "external_transaction_id"),
      @Index(name = "idx_transaction_raw_store_hash", columnList = "payload_raw_hash")
    })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class TransactionRawStore {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "external_transaction_id", nullable = false, length = 64)
  private String externalTransactionId;

  @Column(name = "payload_raw_hash", nullable = false, length = 64)
  private String payloadRawHash;

  @Column(name = "payload_raw_json", nullable = false, columnDefinition = "TEXT")
  private String payloadRawJson;

  @Column(name = "created_at", nullable = false, updatable = false)
  private LocalDateTime createdAt;
}
