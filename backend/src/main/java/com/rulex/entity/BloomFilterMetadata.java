package com.rulex.entity;

import jakarta.persistence.*;
import java.time.OffsetDateTime;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Entidade para metadados de filtros Bloom.
 *
 * <p>Armazena estatísticas e configurações dos filtros Bloom usados para verificação rápida de
 * listas (blacklist, whitelist, greylist).
 */
@Entity
@Table(name = "bloom_filter_metadata")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class BloomFilterMetadata {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "filter_type", nullable = false, length = 50)
  private String filterType; // BLACKLIST, WHITELIST, GREYLIST

  @Column(name = "entity_type", nullable = false, length = 50)
  private String entityType; // PAN, MERCHANT_ID, CPF, etc.

  // Estatísticas do filtro
  @Column(name = "element_count")
  @Builder.Default
  private Long elementCount = 0L;

  @Column(name = "bit_count")
  @Builder.Default
  private Long bitCount = 0L;

  @Column(name = "hash_functions")
  @Builder.Default
  private Integer hashFunctions = 7;

  @Column(name = "false_positive_rate")
  @Builder.Default
  private Double falsePositiveRate = 0.01;

  // Rastreamento de rebuild
  @Column(name = "last_rebuild")
  private OffsetDateTime lastRebuild;

  @Column(name = "rebuild_duration_ms")
  private Long rebuildDurationMs;

  @Column(name = "next_scheduled_rebuild")
  private OffsetDateTime nextScheduledRebuild;

  // Métricas de performance
  @Column(name = "total_lookups")
  @Builder.Default
  private Long totalLookups = 0L;

  @Column(name = "bloom_hits")
  @Builder.Default
  private Long bloomHits = 0L; // Negativos definitivos (sem query DB)

  @Column(name = "bloom_misses")
  @Builder.Default
  private Long bloomMisses = 0L; // Possíveis positivos (query DB necessária)

  @Column(name = "confirmed_positives")
  @Builder.Default
  private Long confirmedPositives = 0L; // DB confirmou na lista

  @Column(name = "false_positives")
  @Builder.Default
  private Long falsePositives = 0L; // Bloom disse sim, DB disse não

  @Column(name = "created_at")
  @Builder.Default
  private OffsetDateTime createdAt = OffsetDateTime.now();

  @Column(name = "updated_at")
  private OffsetDateTime updatedAt;

  @PreUpdate
  protected void onUpdate() {
    this.updatedAt = OffsetDateTime.now();
  }

  /** Incrementa contadores de lookup. */
  public void recordLookup(boolean bloomSaidYes, boolean actuallyInList) {
    this.totalLookups++;
    if (!bloomSaidYes) {
      this.bloomHits++; // Definitivamente não está na lista
    } else {
      this.bloomMisses++; // Precisa verificar no DB
      if (actuallyInList) {
        this.confirmedPositives++;
      } else {
        this.falsePositives++;
      }
    }
  }

  /** Calcula a taxa real de falsos positivos. */
  public double getActualFalsePositiveRate() {
    if (bloomMisses == 0) return 0.0;
    return (double) falsePositives / bloomMisses;
  }

  /** Calcula a eficiência do filtro (queries DB evitadas). */
  public double getEfficiency() {
    if (totalLookups == 0) return 0.0;
    return (double) bloomHits / totalLookups;
  }
}
