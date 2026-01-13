package com.rulex.entity;

import jakarta.persistence.*;
import java.time.OffsetDateTime;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Entidade para associação entre dispositivos e PANs. Rastreia quais cartões foram usados em quais
 * dispositivos para detecção de device farming.
 */
@Entity
@Table(
    name = "device_pan_associations",
    indexes = {
      @Index(name = "idx_device_pan_fp", columnList = "fingerprint_hash"),
      @Index(name = "idx_device_pan_pan", columnList = "pan_hash")
    },
    uniqueConstraints = {
      @UniqueConstraint(
          name = "uk_device_pan",
          columnNames = {"fingerprint_hash", "pan_hash"})
    })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class DevicePanAssociation {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "fingerprint_hash", nullable = false, length = 64)
  private String fingerprintHash;

  @Column(name = "pan_hash", nullable = false, length = 64)
  private String panHash;

  @Column(name = "first_seen")
  private OffsetDateTime firstSeen;

  @Column(name = "last_seen")
  private OffsetDateTime lastSeen;

  @Column(name = "transaction_count")
  @Builder.Default
  private Long transactionCount = 1L;

  @Column(name = "is_primary_device")
  @Builder.Default
  private Boolean isPrimaryDevice = false;

  @Column(name = "risk_level", length = 20)
  @Enumerated(EnumType.STRING)
  @Builder.Default
  private RiskLevel riskLevel = RiskLevel.LOW;

  @PrePersist
  protected void onCreate() {
    OffsetDateTime now = OffsetDateTime.now();
    if (firstSeen == null) {
      firstSeen = now;
    }
    lastSeen = now;
    if (transactionCount == null) {
      transactionCount = 1L;
    }
    if (riskLevel == null) {
      riskLevel = RiskLevel.LOW;
    }
  }

  @PreUpdate
  protected void onUpdate() {
    lastSeen = OffsetDateTime.now();
  }

  /** Incrementa o contador de transações. */
  public void incrementTransactionCount() {
    if (transactionCount == null) {
      transactionCount = 0L;
    }
    transactionCount++;
    lastSeen = OffsetDateTime.now();
  }

  /** Níveis de risco para a associação device/PAN. */
  public enum RiskLevel {
    LOW,
    MEDIUM,
    HIGH,
    CRITICAL
  }
}
