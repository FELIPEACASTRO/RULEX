package com.rulex.v31.feature;

import java.util.List;
import org.springframework.stereotype.Service;

/**
 * Service for feature catalog operations.
 *
 * <p>Provides access to feature definitions (metadata) for the Rule Builder UI
 * and rule validation. All features are deterministic.
 */
@Service
public class FeatureCatalogService {

  private final FeatureDefinitionRepository repository;

  public FeatureCatalogService(FeatureDefinitionRepository repository) {
    this.repository = repository;
  }

  /**
   * List all active feature definitions with optional filters.
   *
   * @param featureType filter by type (PAYLOAD_FIELD, VELOCITY, TEMPORAL, etc.)
   * @param entityType filter by entity (card, customer, merchant, etc.)
   * @param source filter by source (payload, velocity_store, feature_store, runtime)
   * @return list of matching feature definitions
   */
  public List<FeatureDefinitionEntity> list(String featureType, String entityType, String source) {
    String ft = normalize(featureType);
    String et = normalize(entityType);
    String src = normalize(source);

    return repository.findFiltered(ft, et, src);
  }

  /**
   * Get all active features.
   *
   * @return list of all active feature definitions
   */
  public List<FeatureDefinitionEntity> listAll() {
    return repository.findByActiveTrue();
  }

  /**
   * Get features by type (e.g., VELOCITY, TEMPORAL).
   *
   * @param featureType the feature type
   * @return list of features of that type
   */
  public List<FeatureDefinitionEntity> listByType(String featureType) {
    if (featureType == null || featureType.isBlank()) {
      return List.of();
    }
    return repository.findByFeatureTypeAndActiveTrue(featureType.trim().toUpperCase());
  }

  /**
   * Get features by entity type (e.g., card, customer, merchant).
   *
   * @param entityType the entity type
   * @return list of features for that entity
   */
  public List<FeatureDefinitionEntity> listByEntity(String entityType) {
    if (entityType == null || entityType.isBlank()) {
      return List.of();
    }
    return repository.findByEntityTypeAndActiveTrue(entityType.trim().toLowerCase());
  }

  /**
   * Get a specific feature by name.
   *
   * @param featureName the feature name
   * @return the feature definition or null if not found
   */
  public FeatureDefinitionEntity getByName(String featureName) {
    if (featureName == null || featureName.isBlank()) {
      return null;
    }
    return repository.findByFeatureNameAndActiveTrue(featureName.trim());
  }

  private String normalize(String value) {
    if (value == null || value.isBlank()) {
      return null;
    }
    return value.trim();
  }
}
