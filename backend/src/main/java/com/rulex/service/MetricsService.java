package com.rulex.service;

import com.rulex.dto.MetricsDTO;
import com.rulex.entity.TransactionDecision;
import com.rulex.repository.TransactionDecisionRepository;
import com.rulex.repository.TransactionRepository;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/** Serviço para cálculo de métricas em tempo real. */
@Service
@RequiredArgsConstructor
@Slf4j
@Transactional(readOnly = true)
public class MetricsService {

  private final TransactionRepository transactionRepository;
  private final TransactionDecisionRepository decisionRepository;

  /** Obtém métricas gerais do sistema. */
  public MetricsDTO getMetrics(String period) {
    LocalDateTime since = calculateSince(period != null ? period : "24h");

    Long totalTransactions = transactionRepository.countSince(since);
    Long approvedCount =
        decisionRepository.countByClassificationSince(
            TransactionDecision.TransactionClassification.APPROVED, since);
    Long suspiciousCount =
        decisionRepository.countByClassificationSince(
            TransactionDecision.TransactionClassification.SUSPICIOUS, since);
    Long fraudCount =
        decisionRepository.countByClassificationSince(
            TransactionDecision.TransactionClassification.FRAUD, since);

    BigDecimal approvalRate = calculatePercentage(approvedCount, totalTransactions);
    BigDecimal fraudRate = calculatePercentage(fraudCount, totalTransactions);
    BigDecimal suspiciousRate = calculatePercentage(suspiciousCount, totalTransactions);

    return MetricsDTO.builder()
        .totalTransactions(totalTransactions)
        .approvedTransactions(approvedCount)
        .suspiciousTransactions(suspiciousCount)
        .fraudTransactions(fraudCount)
        .approvalRate(approvalRate)
        .fraudRate(fraudRate)
        .suspiciousRate(suspiciousRate)
        .period(period != null ? period : "24h")
          .timestamp(LocalDateTime.now())
        .build();
  }

  /** Obtém métricas por MCC. */
  public Map<Integer, Map<String, Object>> getMetricsByMcc(String period) {
    Map<Integer, Map<String, Object>> mccMetrics = new HashMap<>();

    LocalDateTime since = calculateSince(period != null ? period : "24h");
    for (Object[] row : decisionRepository.aggregateByMccSince(since)) {
      Integer mcc = ((Number) row[0]).intValue();
      long total = ((Number) row[1]).longValue();
      long fraud = ((Number) row[2]).longValue();
      long suspicious = ((Number) row[3]).longValue();
      long approved = ((Number) row[4]).longValue();

      mccMetrics.put(
          mcc,
          Map.of(
              "total", total,
              "approved", approved,
              "suspicious", suspicious,
              "fraud", fraud,
              "fraudRate", calculatePercentage(fraud, total)));
    }

    return mccMetrics;
  }

  /** Obtém métricas por merchant. */
  public Map<String, Map<String, Object>> getMetricsByMerchant(String period) {
    Map<String, Map<String, Object>> merchantMetrics = new HashMap<>();

    LocalDateTime since = calculateSince(period != null ? period : "24h");
    for (Object[] row : decisionRepository.aggregateByMerchantSince(since)) {
      String merchantId = (String) row[0];
      String merchantName = (String) row[1];
      long total = ((Number) row[2]).longValue();
      long fraud = ((Number) row[3]).longValue();

      merchantMetrics.put(
          merchantId != null ? merchantId : "UNKNOWN",
          Map.of(
              "merchantId", merchantId,
              "merchantName", merchantName,
              "total", total,
              "fraud", fraud,
              "fraudRate", calculatePercentage(fraud, total)));
    }

    return merchantMetrics;
  }

  /** Obtém timeline de métricas. */
  public Map<String, Object> getMetricsTimeline(String granularity) {
    Map<String, Object> timeline = new HashMap<>();

    String g = (granularity == null || granularity.isBlank()) ? "hour" : granularity;
    LocalDateTime since = calculateSince("24h");

    var formatter = DateTimeFormatter.ISO_DATE_TIME;
    var buckets = new java.util.ArrayList<Map<String, Object>>();

    var rows =
        switch (g) {
          case "day" -> decisionRepository.timelineDailySince(since.minusDays(30));
          default -> decisionRepository.timelineHourlySince(since);
        };

    for (Object[] row : rows) {
      java.sql.Timestamp bucketTs = (java.sql.Timestamp) row[0];
      long total = ((Number) row[1]).longValue();
      long fraud = ((Number) row[2]).longValue();

      buckets.add(
          Map.of(
              "bucket", bucketTs.toLocalDateTime().format(formatter),
              "total", total,
              "fraud", fraud));
    }

    timeline.put("granularity", g);
    timeline.put("buckets", buckets);

    return timeline;
  }

  /** Calcula o período desde agora. */
  private LocalDateTime calculateSince(String period) {
    return switch (period) {
      case "1h" -> LocalDateTime.now().minusHours(1);
      case "24h" -> LocalDateTime.now().minusHours(24);
      case "7d" -> LocalDateTime.now().minusDays(7);
      case "30d" -> LocalDateTime.now().minusDays(30);
      case "90d" -> LocalDateTime.now().minusDays(90);
      default -> LocalDateTime.now().minusHours(24);
    };
  }

  /** Calcula percentual. */
  private BigDecimal calculatePercentage(Long count, Long total) {
    if (total == 0) {
      return BigDecimal.ZERO;
    }
    return BigDecimal.valueOf(count)
        .divide(BigDecimal.valueOf(total), 2, RoundingMode.HALF_UP)
        .multiply(BigDecimal.valueOf(100));
  }
}
