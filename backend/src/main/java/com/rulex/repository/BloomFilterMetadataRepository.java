package com.rulex.repository;

import com.rulex.entity.BloomFilterMetadata;
import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

/**
 * Repository para metadados de filtros Bloom.
 *
 * <p>Gerencia persistência de estatísticas e configurações dos filtros Bloom.
 */
@Repository
public interface BloomFilterMetadataRepository extends JpaRepository<BloomFilterMetadata, Long> {

  /** Busca metadados por tipo de filtro e tipo de entidade. */
  Optional<BloomFilterMetadata> findByFilterTypeAndEntityType(String filterType, String entityType);

  /** Lista todos os filtros de um tipo. */
  List<BloomFilterMetadata> findByFilterType(String filterType);

  /** Lista todos os filtros de uma entidade. */
  List<BloomFilterMetadata> findByEntityType(String entityType);

  /** Busca filtros que precisam de rebuild. */
  @Query(
      "SELECT b FROM BloomFilterMetadata b WHERE b.nextScheduledRebuild IS NOT NULL "
          + "AND b.nextScheduledRebuild <= CURRENT_TIMESTAMP")
  List<BloomFilterMetadata> findFiltersNeedingRebuild();

  /** Atualiza contadores de lookup. */
  @Modifying
  @Query(
      "UPDATE BloomFilterMetadata b SET "
          + "b.totalLookups = b.totalLookups + 1, "
          + "b.bloomHits = b.bloomHits + :hits, "
          + "b.bloomMisses = b.bloomMisses + :misses, "
          + "b.confirmedPositives = b.confirmedPositives + :confirmed, "
          + "b.falsePositives = b.falsePositives + :falsePos, "
          + "b.updatedAt = CURRENT_TIMESTAMP "
          + "WHERE b.filterType = :filterType AND b.entityType = :entityType")
  int updateLookupStats(
      @Param("filterType") String filterType,
      @Param("entityType") String entityType,
      @Param("hits") long hits,
      @Param("misses") long misses,
      @Param("confirmed") long confirmed,
      @Param("falsePos") long falsePos);

  /** Atualiza informações de rebuild. */
  @Modifying
  @Query(
      "UPDATE BloomFilterMetadata b SET "
          + "b.elementCount = :elementCount, "
          + "b.bitCount = :bitCount, "
          + "b.lastRebuild = CURRENT_TIMESTAMP, "
          + "b.rebuildDurationMs = :durationMs, "
          + "b.nextScheduledRebuild = :nextRebuild, "
          + "b.updatedAt = CURRENT_TIMESTAMP "
          + "WHERE b.filterType = :filterType AND b.entityType = :entityType")
  int updateRebuildInfo(
      @Param("filterType") String filterType,
      @Param("entityType") String entityType,
      @Param("elementCount") long elementCount,
      @Param("bitCount") long bitCount,
      @Param("durationMs") long durationMs,
      @Param("nextRebuild") java.time.OffsetDateTime nextRebuild);

  /** Busca filtros com alta taxa de falsos positivos. */
  @Query(
      "SELECT b FROM BloomFilterMetadata b WHERE b.bloomMisses > 0 "
          + "AND (CAST(b.falsePositives AS double) / b.bloomMisses) > :threshold")
  List<BloomFilterMetadata> findFiltersWithHighFalsePositiveRate(
      @Param("threshold") double threshold);

  /** Estatísticas agregadas por tipo de filtro. */
  @Query(
      "SELECT b.filterType, SUM(b.totalLookups), SUM(b.bloomHits), SUM(b.bloomMisses) "
          + "FROM BloomFilterMetadata b GROUP BY b.filterType")
  List<Object[]> getStatsByFilterType();
}
