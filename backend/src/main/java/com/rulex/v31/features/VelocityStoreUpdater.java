package com.rulex.v31.features;

import java.math.BigDecimal;
import java.time.Duration;
import java.time.OffsetDateTime;
import lombok.RequiredArgsConstructor;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * Deterministic velocity/counter updater.
 *
 * <p>Computes windowed aggregates from the persisted `transactions` table using the canonical
 * `transaction_ts_utc` column. Writes snapshots into `velocity_store` keyed by: (entity_key,
 * metric_name, window_name, bucket_ts).
 */
@Service
@RequiredArgsConstructor
public class VelocityStoreUpdater {

  private final JdbcTemplate jdbc;

  @Transactional
  public void updateCardVelocity(String maskedPan, OffsetDateTime asOfUtc) {
    if (maskedPan == null || maskedPan.isBlank() || asOfUtc == null) {
      return;
    }

    // Feature names correspond to V9 seeds (feature_definitions.feature_name)
    upsertMetric(
        maskedPan, "txn_count_1h", "1h", asOfUtc, countTx(maskedPan, asOfUtc, Duration.ofHours(1)));
    upsertMetric(
        maskedPan,
        "txn_count_24h",
        "24h",
        asOfUtc,
        countTx(maskedPan, asOfUtc, Duration.ofHours(24)));
    upsertMetric(
        maskedPan, "txn_sum_1h", "1h", asOfUtc, sumTx(maskedPan, asOfUtc, Duration.ofHours(1)));
    upsertMetric(
        maskedPan, "txn_sum_24h", "24h", asOfUtc, sumTx(maskedPan, asOfUtc, Duration.ofHours(24)));
    upsertMetric(
        maskedPan,
        "unique_merchants_24h",
        "24h",
        asOfUtc,
        uniqueMerchants(maskedPan, asOfUtc, Duration.ofHours(24)));
    upsertMetric(
        maskedPan,
        "unique_countries_24h",
        "24h",
        asOfUtc,
        uniqueCountries(maskedPan, asOfUtc, Duration.ofHours(24)));
  }

  private long countTx(String maskedPan, OffsetDateTime asOfUtc, Duration window) {
    OffsetDateTime start = asOfUtc.minus(window);
    Long v =
        jdbc.queryForObject(
            """
            SELECT COUNT(*)
            FROM transactions
            WHERE pan = ?
              AND transaction_ts_utc IS NOT NULL
              AND transaction_ts_utc > ?
              AND transaction_ts_utc <= ?
            """,
            Long.class,
            maskedPan,
            start,
            asOfUtc);
    return v == null ? 0L : v;
  }

  private BigDecimal sumTx(String maskedPan, OffsetDateTime asOfUtc, Duration window) {
    OffsetDateTime start = asOfUtc.minus(window);
    BigDecimal v =
        jdbc.queryForObject(
            """
            SELECT COALESCE(SUM(transaction_amount), 0)
            FROM transactions
            WHERE pan = ?
              AND transaction_ts_utc IS NOT NULL
              AND transaction_ts_utc > ?
              AND transaction_ts_utc <= ?
            """,
            BigDecimal.class,
            maskedPan,
            start,
            asOfUtc);
    return v == null ? BigDecimal.ZERO : v;
  }

  private long uniqueMerchants(String maskedPan, OffsetDateTime asOfUtc, Duration window) {
    OffsetDateTime start = asOfUtc.minus(window);
    Long v =
        jdbc.queryForObject(
            """
            SELECT COUNT(DISTINCT merchant_id)
            FROM transactions
            WHERE pan = ?
              AND merchant_id IS NOT NULL
              AND transaction_ts_utc IS NOT NULL
              AND transaction_ts_utc > ?
              AND transaction_ts_utc <= ?
            """,
            Long.class,
            maskedPan,
            start,
            asOfUtc);
    return v == null ? 0L : v;
  }

  private long uniqueCountries(String maskedPan, OffsetDateTime asOfUtc, Duration window) {
    OffsetDateTime start = asOfUtc.minus(window);
    Long v =
        jdbc.queryForObject(
            """
            SELECT COUNT(DISTINCT merchant_country_code)
            FROM transactions
            WHERE pan = ?
              AND merchant_country_code IS NOT NULL
              AND transaction_ts_utc IS NOT NULL
              AND transaction_ts_utc > ?
              AND transaction_ts_utc <= ?
            """,
            Long.class,
            maskedPan,
            start,
            asOfUtc);
    return v == null ? 0L : v;
  }

  private void upsertMetric(
      String entityKey,
      String metricName,
      String windowName,
      OffsetDateTime bucketTs,
      Number value) {
    BigDecimal val = value == null ? null : new BigDecimal(String.valueOf(value));
    if (val == null) {
      return;
    }

    // Deterministic: ensure at most one row exists for the same logical key.
    jdbc.update(
        """
        DELETE FROM velocity_store
        WHERE entity_key = ?
          AND metric_name = ?
          AND window_name = ?
          AND bucket_ts = ?
        """,
        entityKey,
        metricName,
        windowName,
        bucketTs);

    jdbc.update(
        """
        INSERT INTO velocity_store(entity_key, metric_name, window_name, value, bucket_ts)
        VALUES (?,?,?,?,?)
        """,
        entityKey,
        metricName,
        windowName,
        val,
        bucketTs);
  }
}
