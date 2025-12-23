package com.rulex.v31.features;

import com.fasterxml.jackson.databind.JsonNode;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.util.Optional;

/**
 * Provider for derived features (velocity/graph/geo/text/prob) without altering inbound payload.
 *
 * <p>Implementations must be safe-by-default: missing data should be treated as "unknown" and never
 * throw for normal rule evaluation.
 *
 * <p>All features are DETERMINISTIC: same input → same output.
 */
public interface FeatureProvider {

  Optional<JsonNode> getFeature(String entityKey, String featureName);

  default Optional<OffsetDateTime> getLastEventTimestamp(String entityKey, String eventType) {
    return Optional.empty();
  }

  default boolean isHoliday(String country, String uf, LocalDate date) {
    return false;
  }

  // =========================================
  // VELOCITY/COUNTER features
  // =========================================

  /**
   * Get a velocity metric (count, sum, rate, etc.) for an entity within a window.
   *
   * @param entityKey the entity key (e.g., PAN hash, customer ID)
   * @param metricName the metric name (e.g., txn_count, txn_sum, decline_count)
   * @param windowName the window (e.g., 1h, 24h, 7d)
   * @return the metric value or empty if not found
   */
  default Optional<BigDecimal> getVelocityMetric(
      String entityKey, String metricName, String windowName) {
    return Optional.empty();
  }

  /**
   * Get the count of events for an entity within a window.
   *
   * @param entityKey the entity key
   * @param windowName the window (e.g., 1h, 24h, 7d)
   * @return the count or 0 if not found
   */
  default long getCount(String entityKey, String windowName) {
    return getVelocityMetric(entityKey, "count", windowName).map(BigDecimal::longValue).orElse(0L);
  }

  /**
   * Get the sum of amounts for an entity within a window.
   *
   * @param entityKey the entity key
   * @param windowName the window (e.g., 1h, 24h, 7d)
   * @return the sum or BigDecimal.ZERO if not found
   */
  default BigDecimal getSum(String entityKey, String windowName) {
    return getVelocityMetric(entityKey, "sum", windowName).orElse(BigDecimal.ZERO);
  }

  /**
   * Get the unique count for an entity within a window.
   *
   * @param entityKey the entity key
   * @param metricName the metric (e.g., unique_merchants, unique_countries)
   * @param windowName the window
   * @return the unique count or 0 if not found
   */
  default long getUniqueCount(String entityKey, String metricName, String windowName) {
    return getVelocityMetric(entityKey, metricName, windowName)
        .map(BigDecimal::longValue)
        .orElse(0L);
  }

  // =========================================
  // GRAPH features
  // =========================================

  /**
   * Check if a link between two entities is new (first seen within the window).
   *
   * @param srcType source type (e.g., "card")
   * @param srcId source ID (e.g., PAN hash)
   * @param dstType destination type (e.g., "merchant")
   * @param dstId destination ID (e.g., merchant ID)
   * @param windowDays window in days
   * @return true if the link is new (first seen within window)
   */
  default boolean isNewLink(
      String srcType, String srcId, String dstType, String dstId, int windowDays) {
    return false;
  }

  /**
   * Get the degree (number of distinct connections) for an entity.
   *
   * @param entityType the entity type (e.g., "card", "merchant")
   * @param entityId the entity ID
   * @param neighborType the neighbor type to count (e.g., "merchant" for card→merchant edges)
   * @param windowDays window in days
   * @return the degree or 0 if not found
   */
  default long getDegree(String entityType, String entityId, String neighborType, int windowDays) {
    return 0L;
  }
}
