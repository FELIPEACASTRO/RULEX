package com.rulex.repository;

import com.rulex.entity.VelocityCounter;
import java.time.OffsetDateTime;
import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

/** Repositório para contadores de velocidade. */
@Repository
public interface VelocityCounterRepository extends JpaRepository<VelocityCounter, Long> {

  /** Busca contador por chave e janela. */
  Optional<VelocityCounter> findByKeyTypeAndKeyValueAndWindowTypeAndWindowStart(
      String keyType, String keyValue, String windowType, OffsetDateTime windowStart);

  /** Busca contadores ativos para uma chave. */
  @Query(
      "SELECT v FROM VelocityCounter v WHERE v.keyType = :keyType AND v.keyValue = :keyValue "
          + "AND v.windowEnd >= :now ORDER BY v.windowType, v.windowStart DESC")
  List<VelocityCounter> findActiveCounters(
      @Param("keyType") String keyType,
      @Param("keyValue") String keyValue,
      @Param("now") OffsetDateTime now);

  /** Busca contador mais recente para uma chave e tipo de janela. */
  @Query(
      "SELECT v FROM VelocityCounter v WHERE v.keyType = :keyType AND v.keyValue = :keyValue "
          + "AND v.windowType = :windowType AND v.windowEnd >= :now "
          + "ORDER BY v.windowStart DESC LIMIT 1")
  Optional<VelocityCounter> findLatestCounter(
      @Param("keyType") String keyType,
      @Param("keyValue") String keyValue,
      @Param("windowType") String windowType,
      @Param("now") OffsetDateTime now);

  /** Busca contadores expirados para limpeza. */
  @Query("SELECT v FROM VelocityCounter v WHERE v.windowEnd < :cutoff")
  List<VelocityCounter> findExpiredCounters(@Param("cutoff") OffsetDateTime cutoff);

  /** Deleta contadores expirados. */
  @Query("DELETE FROM VelocityCounter v WHERE v.windowEnd < :cutoff")
  void deleteExpiredCounters(@Param("cutoff") OffsetDateTime cutoff);
}
