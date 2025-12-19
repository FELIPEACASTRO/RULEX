package com.rulex.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Index;
import jakarta.persistence.Lob;
import jakarta.persistence.Table;
import java.util.Arrays;
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
  @Column(name = "external_transaction_id", nullable = false, length = 64)
  private String externalTransactionId;

  @Column(name = "payload_raw_hash", nullable = false, length = 64)
  private String payloadRawHash;

  @Lob
  @Column(name = "payload_raw_bytes", nullable = false, columnDefinition = "BYTEA")
  private byte[] payloadRawBytes;

  @Column(name = "content_type", length = 100)
  private String contentType;

  @Column(name = "created_at", nullable = false, updatable = false)
  private LocalDateTime createdAt;

  @Override
  public String toString() {
    return "TransactionRawStore{" +
        "externalTransactionId='" + externalTransactionId + '\'' +
        ", payloadRawHash='" + payloadRawHash + '\'' +
        ", payloadRawBytesLength=" + (payloadRawBytes == null ? 0 : payloadRawBytes.length) +
        ", contentType='" + contentType + '\'' +
        ", createdAt=" + createdAt +
        '}';
  }
}
