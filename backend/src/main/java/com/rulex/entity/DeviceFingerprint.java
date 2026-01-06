package com.rulex.entity;

import jakarta.persistence.*;
import java.time.OffsetDateTime;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;

/**
 * Entidade para fingerprints de dispositivos. Armazena características únicas do dispositivo para
 * detecção de fraude e device farming.
 */
@Entity
@Table(
    name = "device_fingerprints",
    indexes = {
      @Index(name = "idx_device_fp_hash", columnList = "fingerprint_hash")
    })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class DeviceFingerprint {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "fingerprint_hash", nullable = false, unique = true, length = 64)
  private String fingerprintHash;

  // Core fingerprint components
  @Column(name = "user_agent_family", length = 100)
  private String userAgentFamily;

  @Column(name = "user_agent_version", length = 50)
  private String userAgentVersion;

  @Column(name = "platform", length = 50)
  private String platform;

  @Column(name = "screen_resolution", length = 20)
  private String screenResolution;

  @Column(name = "color_depth")
  private Integer colorDepth;

  @Column(name = "device_memory")
  private Integer deviceMemory;

  @Column(name = "hardware_concurrency")
  private Integer hardwareConcurrency;

  @Column(name = "language", length = 20)
  private String language;

  @Column(name = "timezone", length = 100)
  private String timezone;

  // Canvas/WebGL hashes (unique per device)
  @Column(name = "canvas_hash", length = 64)
  private String canvasHash;

  @Column(name = "webgl_hash", length = 64)
  private String webglHash;

  @Column(name = "audio_hash", length = 64)
  private String audioHash;

  // Additional signals
  @Column(name = "plugins_hash", length = 64)
  private String pluginsHash;

  @Column(name = "fonts_hash", length = 64)
  private String fontsHash;

  // Risk indicators
  @Column(name = "is_tor")
  @Builder.Default
  private Boolean isTor = false;

  @Column(name = "is_vpn")
  @Builder.Default
  private Boolean isVpn = false;

  @Column(name = "is_datacenter")
  @Builder.Default
  private Boolean isDatacenter = false;

  @Column(name = "is_emulator")
  @Builder.Default
  private Boolean isEmulator = false;

  // Statistics
  @Column(name = "first_seen")
  private OffsetDateTime firstSeen;

  @Column(name = "last_seen")
  private OffsetDateTime lastSeen;

  @Column(name = "transaction_count")
  @Builder.Default
  private Long transactionCount = 0L;

  // Metadata
  @Column(name = "raw_data", columnDefinition = "jsonb")
  @JdbcTypeCode(SqlTypes.JSON)
  private String rawData;

  @PrePersist
  protected void onCreate() {
    OffsetDateTime now = OffsetDateTime.now();
    if (firstSeen == null) {
      firstSeen = now;
    }
    lastSeen = now;
    if (transactionCount == null) {
      transactionCount = 0L;
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

  /** Verifica se o dispositivo tem indicadores de risco. */
  public boolean hasRiskIndicators() {
    return Boolean.TRUE.equals(isTor)
        || Boolean.TRUE.equals(isVpn)
        || Boolean.TRUE.equals(isDatacenter)
        || Boolean.TRUE.equals(isEmulator);
  }
}
