package com.rulex.repository;

import com.rulex.entity.DeviceFingerprint;
import java.time.OffsetDateTime;
import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

/** Repository para operações com fingerprints de dispositivos. */
@Repository
public interface DeviceFingerprintRepository extends JpaRepository<DeviceFingerprint, Long> {

  /** Busca um fingerprint pelo hash. */
  Optional<DeviceFingerprint> findByFingerprintHash(String fingerprintHash);

  /** Verifica se um fingerprint existe. */
  boolean existsByFingerprintHash(String fingerprintHash);

  /** Busca fingerprints com indicadores de risco. */
  @Query(
      "SELECT df FROM DeviceFingerprint df WHERE "
          + "df.isTor = true OR df.isVpn = true OR df.isDatacenter = true OR df.isEmulator = true")
  List<DeviceFingerprint> findWithRiskIndicators();

  /** Busca fingerprints vistos após uma data. */
  List<DeviceFingerprint> findByLastSeenAfter(OffsetDateTime date);

  /** Busca fingerprints por plataforma. */
  List<DeviceFingerprint> findByPlatform(String platform);

  /** Conta fingerprints únicos por plataforma. */
  @Query("SELECT df.platform, COUNT(df) FROM DeviceFingerprint df GROUP BY df.platform")
  List<Object[]> countByPlatform();

  /** Busca dispositivos suspeitos de device farming (muitas transações). */
  @Query(
      "SELECT df FROM DeviceFingerprint df WHERE df.transactionCount > :threshold "
          + "ORDER BY df.transactionCount DESC")
  List<DeviceFingerprint> findSuspiciousDevices(@Param("threshold") Long threshold);

  /** Busca fingerprints por user agent family. */
  List<DeviceFingerprint> findByUserAgentFamily(String userAgentFamily);
}
