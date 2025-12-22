package com.rulex.v31.features;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.Optional;

/** Default provider for contexts that don't wire DB-backed features (e.g., unit tests). */
public class NoOpFeatureProvider implements FeatureProvider {
  @Override
  public Optional<JsonNode> getFeature(String entityKey, String featureName) {
    return Optional.empty();
  }
}
