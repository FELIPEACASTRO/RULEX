package com.rulex.v31.feature;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import java.time.OffsetDateTime;
import java.util.UUID;
import lombok.Getter;
import lombok.Setter;

/**
 * Feature definition entity - metadata for deterministic features available to hard rules.
 *
 * <p>Features can come from: payload fields, velocity store (counters), feature store (pre-computed),
 * or runtime calculations. All must be deterministic (same input → same output).
 */
@Entity
@Table(name = "feature_definitions")
@Getter
@Setter
public class FeatureDefinitionEntity {

  @Id
  private UUID id;

  @Column(name = "feature_name", nullable = false, unique = true)
  private String featureName;

  @Column(name = "feature_type", nullable = false)
  private String featureType;

  @Column(name = "entity_type")
  private String entityType;

  @Column(name = "window_name")
  private String windowName;

  @Column(name = "formula")
  private String formula;

  @Column(name = "description")
  private String description;

  @Column(name = "source", nullable = false)
  private String source;

  @Column(name = "data_type", nullable = false)
  private String dataType;

  @Column(name = "allowed_operators", columnDefinition = "text[]", nullable = false)
  private String[] allowedOperators;

  @Column(name = "refresh_strategy")
  private String refreshStrategy;

  @Column(name = "version", nullable = false)
  private int version;

  @Column(name = "active", nullable = false)
  private boolean active;

  @Column(name = "created_at", nullable = false)
  private OffsetDateTime createdAt;

  @Column(name = "updated_at", nullable = false)
  private OffsetDateTime updatedAt;
}
