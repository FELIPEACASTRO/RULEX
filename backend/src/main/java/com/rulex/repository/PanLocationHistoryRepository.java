package com.rulex.repository;

import com.rulex.entity.PanLocationHistory;
import com.rulex.entity.PanLocationHistory.TravelRisk;
import java.time.OffsetDateTime;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

/** Repository para operações com histórico de localização de PANs. */
@Repository
public interface PanLocationHistoryRepository extends JpaRepository<PanLocationHistory, Long> {

  /** Busca histórico de localização de um PAN ordenado por tempo (mais recente primeiro). */
  List<PanLocationHistory> findByPanHashOrderByTransactionTimeDesc(String panHash);

  /** Busca histórico de localização de um PAN com limite. */
  List<PanLocationHistory> findByPanHashOrderByTransactionTimeDesc(String panHash, Pageable pageable);

  /** Busca a última localização de um PAN. */
  Optional<PanLocationHistory> findFirstByPanHashOrderByTransactionTimeDesc(String panHash);

  /** Busca localizações de um PAN após uma data. */
  List<PanLocationHistory> findByPanHashAndTransactionTimeAfterOrderByTransactionTimeDesc(
      String panHash, OffsetDateTime after);

  /** Busca localizações com risco de viagem específico. */
  List<PanLocationHistory> findByTravelRisk(TravelRisk travelRisk);

  /** Busca localizações com viagem impossível. */
  @Query("SELECT plh FROM PanLocationHistory plh WHERE plh.travelRisk = 'IMPOSSIBLE'")
  List<PanLocationHistory> findImpossibleTravels();

  /** Busca localizações de alto risco (HIGH ou IMPOSSIBLE). */
  @Query("SELECT plh FROM PanLocationHistory plh WHERE plh.travelRisk IN ('HIGH', 'IMPOSSIBLE')")
  List<PanLocationHistory> findHighRiskTravels();

  /** Conta localizações distintas de um PAN nas últimas N horas. */
  @Query(
      "SELECT COUNT(DISTINCT plh.country) FROM PanLocationHistory plh "
          + "WHERE plh.panHash = :panHash AND plh.transactionTime > :since")
  long countDistinctCountriesSince(
      @Param("panHash") String panHash, @Param("since") OffsetDateTime since);

  /** Busca a velocidade máxima de um PAN nas últimas N horas. */
  @Query(
      "SELECT MAX(plh.speedKmh) FROM PanLocationHistory plh "
          + "WHERE plh.panHash = :panHash AND plh.transactionTime > :since")
  Optional<Double> findMaxSpeedSince(
      @Param("panHash") String panHash, @Param("since") OffsetDateTime since);

  /** Deleta registros antigos (para limpeza de dados). */
  @Modifying
  @Query("DELETE FROM PanLocationHistory plh WHERE plh.createdAt < :before")
  int deleteOlderThan(@Param("before") OffsetDateTime before);

  /** Busca localizações por país. */
  List<PanLocationHistory> findByCountry(String country);

  /** Busca localizações por transação. */
  Optional<PanLocationHistory> findByTransactionId(Long transactionId);
}
