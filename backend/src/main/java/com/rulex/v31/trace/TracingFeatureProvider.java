package com.rulex.v31.trace;

import com.fasterxml.jackson.databind.JsonNode;
import com.rulex.v31.features.FeatureProvider;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.util.Optional;
import java.util.function.Supplier;

/** Decorator that records FeatureProvider usage into the current trace collector (if any). */
public class TracingFeatureProvider implements FeatureProvider {

  private final FeatureProvider delegate;
  private final Supplier<FeatureUsageCollector> collectorSupplier;

  public TracingFeatureProvider(
      FeatureProvider delegate, Supplier<FeatureUsageCollector> collectorSupplier) {
    this.delegate = delegate;
    this.collectorSupplier = collectorSupplier;
  }

  @Override
  public Optional<JsonNode> getFeature(String entityKey, String featureName) {
    Optional<JsonNode> v = delegate.getFeature(entityKey, featureName);

    FeatureUsageCollector c = collectorSupplier.get();
    if (c != null) {
      c.record(
          "feature", featureName, entityKey, null, null, v.map(JsonNode::toString).orElse(null));
    }

    return v;
  }

  @Override
  public Optional<OffsetDateTime> getLastEventTimestamp(String entityKey, String eventType) {
    Optional<OffsetDateTime> v = delegate.getLastEventTimestamp(entityKey, eventType);

    FeatureUsageCollector c = collectorSupplier.get();
    if (c != null) {
      c.record(
          "feature",
          "LAST_EVENT_TIMESTAMP",
          entityKey,
          null,
          eventType,
          v.map(Object::toString).orElse(null));
    }

    return v;
  }

  @Override
  public boolean isHoliday(String country, String uf, LocalDate date) {
    boolean v = delegate.isHoliday(country, uf, date);

    FeatureUsageCollector c = collectorSupplier.get();
    if (c != null) {
      c.record("function", "IS_HOLIDAY", null, null, null, country + "|" + uf + "|" + date);
    }

    return v;
  }
}
