package com.rulex.entity;

import jakarta.persistence.*;
import java.time.OffsetDateTime;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Entidade para versionamento de dados de referência.
 *
 * <p>Rastreia versões de dados de referência como listas de BIN, MCC, países de alto risco, etc.
 * Permite auditoria e rollback de mudanças em dados de referência.
 */
@Entity
@Table(name = "refdata_versions")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RefdataVersion {

  @Id
  @GeneratedValue(strategy = GenerationType.UUID)
  private UUID id;

  @Column(name = "key", nullable = false, unique = true)
  private String key; // Ex: "bin_lookup_v1.2", "mcc_categories_2024"

  @Column(name = "notes", columnDefinition = "TEXT")
  private String notes;

  @Column(name = "created_at", nullable = false)
  @Builder.Default
  private OffsetDateTime createdAt = OffsetDateTime.now();
}
