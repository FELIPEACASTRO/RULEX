package com.rulex.repository;

import com.rulex.entity.DevicePanAssociation;
import com.rulex.entity.DevicePanAssociation.RiskLevel;
import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

/** Repository para operações com associações device-PAN. */
@Repository
public interface DevicePanAssociationRepository extends JpaRepository<DevicePanAssociation, Long> {

  /** Busca associação por fingerprint e PAN hash. */
  Optional<DevicePanAssociation> findByFingerprintHashAndPanHash(
      String fingerprintHash, String panHash);

  /** Busca todas as associações de um fingerprint. */
  List<DevicePanAssociation> findByFingerprintHash(String fingerprintHash);

  /** Busca todas as associações de um PAN. */
  List<DevicePanAssociation> findByPanHash(String panHash);

  /** Conta quantos PANs distintos estão associados a um fingerprint. */
  @Query("SELECT COUNT(DISTINCT dpa.panHash) FROM DevicePanAssociation dpa WHERE dpa.fingerprintHash = :fingerprintHash")
  long countDistinctPansByFingerprintHash(@Param("fingerprintHash") String fingerprintHash);

  /** Conta quantos fingerprints distintos estão associados a um PAN. */
  @Query("SELECT COUNT(DISTINCT dpa.fingerprintHash) FROM DevicePanAssociation dpa WHERE dpa.panHash = :panHash")
  long countDistinctFingerprintsByPanHash(@Param("panHash") String panHash);

  /** Busca associações com nível de risco específico. */
  List<DevicePanAssociation> findByRiskLevel(RiskLevel riskLevel);

  /** Busca associações de alto risco. */
  @Query("SELECT dpa FROM DevicePanAssociation dpa WHERE dpa.riskLevel IN ('HIGH', 'CRITICAL')")
  List<DevicePanAssociation> findHighRiskAssociations();

  /** Busca dispositivos primários de um PAN. */
  List<DevicePanAssociation> findByPanHashAndIsPrimaryDeviceTrue(String panHash);

  /**
   * Busca fingerprints suspeitos de device farming (associados a muitos PANs).
   */
  @Query(
      "SELECT dpa.fingerprintHash, COUNT(DISTINCT dpa.panHash) as panCount "
          + "FROM DevicePanAssociation dpa "
          + "GROUP BY dpa.fingerprintHash "
          + "HAVING COUNT(DISTINCT dpa.panHash) >= :threshold "
          + "ORDER BY panCount DESC")
  List<Object[]> findDeviceFarmingSuspects(@Param("threshold") long threshold);

  /** Verifica se um PAN já foi visto em um dispositivo. */
  boolean existsByFingerprintHashAndPanHash(String fingerprintHash, String panHash);
}
