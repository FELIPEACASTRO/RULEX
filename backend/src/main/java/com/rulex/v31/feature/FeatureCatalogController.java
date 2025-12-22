package com.rulex.v31.feature;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * REST controller for the Feature Catalog API.
 *
 * <p>Provides endpoints to query feature definitions (metadata) for the Rule Builder UI.
 * All features are deterministic (no ML, no auto-tuning).
 */
@RestController
@RequestMapping("/feature-catalog")
public class FeatureCatalogController {

  private final FeatureCatalogService service;

  public FeatureCatalogController(FeatureCatalogService service) {
    this.service = service;
  }

  /**
   * List all active feature definitions with optional filters.
   *
   * @param featureType filter by type (PAYLOAD_FIELD, VELOCITY, TEMPORAL, GRAPH, GEO, TEXT, SCHEMA, DERIVED, CONTEXTUAL)
   * @param entityType filter by entity (card, customer, merchant, device, ip, acquirer)
   * @param source filter by source (payload, velocity_store, feature_store, runtime)
   * @return list of feature definitions
   */
  @GetMapping
  public ResponseEntity<List<Map<String, Object>>> list(
      @RequestParam(required = false) String featureType,
      @RequestParam(required = false) String entityType,
      @RequestParam(required = false) String source) {

    List<FeatureDefinitionEntity> features = service.list(featureType, entityType, source);
    return ResponseEntity.ok(toResponse(features));
  }

  /**
   * Get a specific feature by name.
   *
   * @param featureName the feature name
   * @return the feature definition or 404 if not found
   */
  @GetMapping("/{featureName}")
  public ResponseEntity<Map<String, Object>> getByName(@PathVariable String featureName) {
    FeatureDefinitionEntity feature = service.getByName(featureName);
    if (feature == null) {
      return ResponseEntity.notFound().build();
    }
    return ResponseEntity.ok(toResponse(feature));
  }

  /**
   * Get list of feature types (enum values).
   *
   * @return list of valid feature types
   */
  @GetMapping("/types")
  public ResponseEntity<List<String>> getFeatureTypes() {
    return ResponseEntity.ok(
        List.of(
            "PAYLOAD_FIELD",
            "TEMPORAL",
            "VELOCITY",
            "GRAPH",
            "GEO",
            "TEXT",
            "SCHEMA",
            "DERIVED",
            "CONTEXTUAL"));
  }

  /**
   * Get list of entity types.
   *
   * @return list of valid entity types
   */
  @GetMapping("/entity-types")
  public ResponseEntity<List<String>> getEntityTypes() {
    return ResponseEntity.ok(
        List.of("card", "customer", "merchant", "device", "ip", "acquirer"));
  }

  /**
   * Get list of data sources.
   *
   * @return list of valid sources
   */
  @GetMapping("/sources")
  public ResponseEntity<List<String>> getSources() {
    return ResponseEntity.ok(
        List.of("payload", "velocity_store", "feature_store", "runtime"));
  }

  private List<Map<String, Object>> toResponse(List<FeatureDefinitionEntity> features) {
    List<Map<String, Object>> result = new ArrayList<>(features.size());
    for (FeatureDefinitionEntity f : features) {
      result.add(toResponse(f));
    }
    return result;
  }

  private Map<String, Object> toResponse(FeatureDefinitionEntity f) {
    Map<String, Object> map = new LinkedHashMap<>();
    map.put("featureName", f.getFeatureName());
    map.put("featureType", f.getFeatureType());
    map.put("entityType", f.getEntityType());
    map.put("windowName", f.getWindowName());
    map.put("formula", f.getFormula());
    map.put("description", f.getDescription());
    map.put("source", f.getSource());
    map.put("dataType", f.getDataType());
    map.put("allowedOperators", f.getAllowedOperators() != null ? List.of(f.getAllowedOperators()) : List.of());
    map.put("refreshStrategy", f.getRefreshStrategy());
    map.put("version", f.getVersion());
    return map;
  }
}
