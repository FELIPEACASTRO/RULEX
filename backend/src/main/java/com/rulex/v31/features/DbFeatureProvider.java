package com.rulex.v31.features;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.Optional;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;

/**
 * Database-backed feature provider for deterministic features.
 *
 * <p>Queries feature_store, velocity_store, holidays, and graph_edges tables.
 * All operations are safe-by-default (return empty/false on errors).
 */
@Service
public class DbFeatureProvider implements FeatureProvider {

  private final JdbcTemplate jdbc;
  private final ObjectMapper objectMapper;

  public DbFeatureProvider(JdbcTemplate jdbc, ObjectMapper objectMapper) {
    this.jdbc = jdbc;
    this.objectMapper = objectMapper;
  }

  @Override
  public Optional<JsonNode> getFeature(String entityKey, String featureName) {
    try {
      String json =
          jdbc.query(
              "SELECT value_jsonb::text FROM feature_store WHERE entity_key = ? AND feature_name = ?",
              ps -> {
                ps.setString(1, entityKey);
                ps.setString(2, featureName);
              },
              rs -> rs.next() ? rs.getString(1) : null);

      if (json == null || json.isBlank()) {
        return Optional.empty();
      }
      return Optional.of(objectMapper.readTree(json));
    } catch (Exception ignored) {
      return Optional.empty();
    }
  }

  @Override
  public Optional<OffsetDateTime> getLastEventTimestamp(String entityKey, String eventType) {
    // Stored as feature_name = "last_event_ts:<EVENT_TYPE>" with value_jsonb = {"ts":"..."}
    String featureName = "last_event_ts:" + (eventType == null ? "" : eventType.trim());
    return getFeature(entityKey, featureName)
        .flatMap(
            node -> {
              JsonNode ts = node.get("ts");
              if (ts == null || ts.isNull()) {
                return Optional.empty();
              }
              try {
                return Optional.of(OffsetDateTime.parse(ts.asText()));
              } catch (Exception ignored) {
                return Optional.empty();
              }
            });
  }

  @Override
  public boolean isHoliday(String country, String uf, LocalDate date) {
    if (country == null || country.isBlank() || date == null) {
      return false;
    }
    try {
      String c = country.trim();
      String u = (uf == null || uf.isBlank()) ? null : uf.trim();

      Integer found;
      if (u == null) {
        // No UF provided: only match national holidays.
        found =
            jdbc.queryForObject(
                "SELECT 1 FROM holidays WHERE country = ? AND date = ? AND uf IS NULL LIMIT 1",
                Integer.class,
                c,
                date);
      } else {
        // UF provided: match either a national holiday (uf NULL) or a state holiday.
        found =
            jdbc.queryForObject(
                "SELECT 1 FROM holidays WHERE country = ? AND date = ? AND (uf IS NULL OR uf = ?) LIMIT 1",
                Integer.class,
                c,
                date,
                u);
      }
      return found != null && found == 1;
    } catch (Exception ignored) {
      return false;
    }
  }

  // =========================================
  // VELOCITY/COUNTER features
  // =========================================

  @Override
  public Optional<BigDecimal> getVelocityMetric(
      String entityKey, String metricName, String windowName) {
    if (entityKey == null || entityKey.isBlank()
        || metricName == null || metricName.isBlank()
        || windowName == null || windowName.isBlank()) {
      return Optional.empty();
    }
    try {
      // Get the most recent bucket for this entity/metric/window
      BigDecimal value =
          jdbc.queryForObject(
              """
              SELECT value FROM velocity_store
              WHERE entity_key = ? AND metric_name = ? AND window_name = ?
              ORDER BY bucket_ts DESC
              LIMIT 1
              """,
              BigDecimal.class,
              entityKey.trim(),
              metricName.trim(),
              windowName.trim());
      return Optional.ofNullable(value);
    } catch (Exception ignored) {
      return Optional.empty();
    }
  }

  @Override
  public long getCount(String entityKey, String windowName) {
    return getVelocityMetric(entityKey, "count", windowName)
        .map(BigDecimal::longValue)
        .orElse(0L);
  }

  @Override
  public BigDecimal getSum(String entityKey, String windowName) {
    return getVelocityMetric(entityKey, "sum", windowName).orElse(BigDecimal.ZERO);
  }

  @Override
  public long getUniqueCount(String entityKey, String metricName, String windowName) {
    return getVelocityMetric(entityKey, metricName, windowName)
        .map(BigDecimal::longValue)
        .orElse(0L);
  }

  // =========================================
  // GRAPH features
  // =========================================

  @Override
  public boolean isNewLink(
      String srcType, String srcId, String dstType, String dstId, int windowDays) {
    if (srcType == null || srcId == null || dstType == null || dstId == null) {
      return false;
    }
    try {
      // Check if edge exists and was first_seen within windowDays
      Integer found =
          jdbc.queryForObject(
              """
              SELECT 1 FROM graph_edges
              WHERE src_type = ? AND src_id = ? AND dst_type = ? AND dst_id = ?
                AND first_seen >= now() - (? || ' days')::INTERVAL
              LIMIT 1
              """,
              Integer.class,
              srcType.trim(),
              srcId.trim(),
              dstType.trim(),
              dstId.trim(),
              windowDays);
      return found != null && found == 1;
    } catch (Exception ignored) {
      // Edge doesn't exist = new link
      return true;
    }
  }

  @Override
  public long getDegree(String entityType, String entityId, String neighborType, int windowDays) {
    if (entityType == null || entityId == null || neighborType == null) {
      return 0L;
    }
    try {
      Long count =
          jdbc.queryForObject(
              """
              SELECT COUNT(DISTINCT dst_id) FROM graph_edges
              WHERE src_type = ? AND src_id = ? AND dst_type = ?
                AND last_seen >= now() - (? || ' days')::INTERVAL
              """,
              Long.class,
              entityType.trim(),
              entityId.trim(),
              neighborType.trim(),
              windowDays);
      return count != null ? count : 0L;
    } catch (Exception ignored) {
      return 0L;
    }
  }

  public OffsetDateTime nowUtc() {
    return OffsetDateTime.now(ZoneOffset.UTC);
  }
}

