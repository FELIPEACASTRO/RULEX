package com.rulex.service;

import com.rulex.entity.RuleMetrics;
import com.rulex.repository.RuleMetricsRepository;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * Serviço para métricas de performance e eficácia das regras.
 */
@Service
@RequiredArgsConstructor
@Slf4j
@Transactional
public class RuleMetricsService {

  private final RuleMetricsRepository metricsRepository;

  /**
   * Registra uma avaliação de regra.
   */
  public void recordEvaluation(Long ruleId, String ruleName, boolean triggered, BigDecimal amount, long processingTimeMs) {
    LocalDate today = LocalDate.now();

    RuleMetrics metrics = metricsRepository.findByRuleIdAndMetricDate(ruleId, today)
        .orElseGet(() -> RuleMetrics.builder()
            .ruleId(ruleId)
            .ruleName(ruleName)
            .metricDate(today)
            .build());

    metrics.setTotalEvaluations(metrics.getTotalEvaluations() + 1);

    if (triggered) {
      metrics.setTotalTriggered(metrics.getTotalTriggered() + 1);
      if (amount != null) {
        metrics.setTotalAmountBlocked(metrics.getTotalAmountBlocked().add(amount));
      }
    }

    // Atualizar média de tempo de processamento (média móvel)
    BigDecimal currentAvg = metrics.getAvgProcessingTimeMs();
    long totalEval = metrics.getTotalEvaluations();
    BigDecimal newAvg = currentAvg
        .multiply(BigDecimal.valueOf(totalEval - 1))
        .add(BigDecimal.valueOf(processingTimeMs))
        .divide(BigDecimal.valueOf(totalEval), 2, RoundingMode.HALF_UP);
    metrics.setAvgProcessingTimeMs(newAvg);

    metricsRepository.save(metrics);
  }

  /**
   * Registra feedback de falso positivo.
   */
  public void recordFalsePositive(Long ruleId) {
    LocalDate today = LocalDate.now();
    Optional<RuleMetrics> metricsOpt = metricsRepository.findByRuleIdAndMetricDate(ruleId, today);

    if (metricsOpt.isPresent()) {
      RuleMetrics metrics = metricsOpt.get();
      metrics.setFalsePositives(metrics.getFalsePositives() + 1);
      metricsRepository.save(metrics);
    }
  }

  /**
   * Registra feedback de verdadeiro positivo.
   */
  public void recordTruePositive(Long ruleId) {
    LocalDate today = LocalDate.now();
    Optional<RuleMetrics> metricsOpt = metricsRepository.findByRuleIdAndMetricDate(ruleId, today);

    if (metricsOpt.isPresent()) {
      RuleMetrics metrics = metricsOpt.get();
      metrics.setTruePositives(metrics.getTruePositives() + 1);
      metricsRepository.save(metrics);
    }
  }

  /**
   * Obtém métricas de uma regra para um período.
   */
  @Transactional(readOnly = true)
  public RuleMetricsSummary getRuleMetrics(Long ruleId, LocalDate startDate, LocalDate endDate) {
    List<RuleMetrics> metrics = metricsRepository
        .findByRuleIdAndMetricDateBetweenOrderByMetricDateDesc(ruleId, startDate, endDate);

    if (metrics.isEmpty()) {
      return RuleMetricsSummary.builder()
          .ruleId(ruleId)
          .startDate(startDate)
          .endDate(endDate)
          .totalEvaluations(0L)
          .totalTriggered(0L)
          .triggerRate(0.0)
          .truePositives(0L)
          .falsePositives(0L)
          .precision(0.0)
          .falsePositiveRate(0.0)
          .totalAmountBlocked(BigDecimal.ZERO)
          .avgProcessingTimeMs(0.0)
          .build();
    }

    long totalEvaluations = metrics.stream().mapToLong(RuleMetrics::getTotalEvaluations).sum();
    long totalTriggered = metrics.stream().mapToLong(RuleMetrics::getTotalTriggered).sum();
    long truePositives = metrics.stream().mapToLong(RuleMetrics::getTruePositives).sum();
    long falsePositives = metrics.stream().mapToLong(RuleMetrics::getFalsePositives).sum();
    BigDecimal totalAmountBlocked = metrics.stream()
        .map(RuleMetrics::getTotalAmountBlocked)
        .reduce(BigDecimal.ZERO, BigDecimal::add);

    double avgProcessingTime = metrics.stream()
        .mapToDouble(m -> m.getAvgProcessingTimeMs().doubleValue())
        .average()
        .orElse(0.0);

    double triggerRate = totalEvaluations > 0 ? (double) totalTriggered / totalEvaluations * 100 : 0;
    double precision = totalTriggered > 0 ? (double) truePositives / totalTriggered * 100 : 0;
    double falsePositiveRate = totalTriggered > 0 ? (double) falsePositives / totalTriggered * 100 : 0;

    return RuleMetricsSummary.builder()
        .ruleId(ruleId)
        .ruleName(metrics.get(0).getRuleName())
        .startDate(startDate)
        .endDate(endDate)
        .totalEvaluations(totalEvaluations)
        .totalTriggered(totalTriggered)
        .triggerRate(triggerRate)
        .truePositives(truePositives)
        .falsePositives(falsePositives)
        .precision(precision)
        .falsePositiveRate(falsePositiveRate)
        .totalAmountBlocked(totalAmountBlocked)
        .avgProcessingTimeMs(avgProcessingTime)
        .dailyMetrics(metrics)
        .build();
  }

  /**
   * Obtém métricas agregadas de todas as regras.
   */
  @Transactional(readOnly = true)
  public List<RuleMetricsSummary> getAllRulesMetrics(LocalDate startDate, LocalDate endDate) {
    List<Object[]> aggregated = metricsRepository.getAggregatedMetricsByPeriod(startDate, endDate);

    return aggregated.stream()
        .map(row -> {
          Long ruleId = (Long) row[0];
          String ruleName = (String) row[1];
          Long totalEval = ((Number) row[2]).longValue();
          Long totalTrig = ((Number) row[3]).longValue();
          Long truePosCount = ((Number) row[4]).longValue();
          Long falsePosCount = ((Number) row[5]).longValue();
          BigDecimal amountBlocked = (BigDecimal) row[6];

          double triggerRate = totalEval > 0 ? (double) totalTrig / totalEval * 100 : 0;
          double precision = totalTrig > 0 ? (double) truePosCount / totalTrig * 100 : 0;
          double fpRate = totalTrig > 0 ? (double) falsePosCount / totalTrig * 100 : 0;

          return RuleMetricsSummary.builder()
              .ruleId(ruleId)
              .ruleName(ruleName)
              .startDate(startDate)
              .endDate(endDate)
              .totalEvaluations(totalEval)
              .totalTriggered(totalTrig)
              .triggerRate(triggerRate)
              .truePositives(truePosCount)
              .falsePositives(falsePosCount)
              .precision(precision)
              .falsePositiveRate(fpRate)
              .totalAmountBlocked(amountBlocked != null ? amountBlocked : BigDecimal.ZERO)
              .build();
        })
        .toList();
  }

  /**
   * Obtém regras com alta taxa de falsos positivos.
   */
  @Transactional(readOnly = true)
  public List<RuleMetrics> getRulesWithHighFalsePositiveRate(double threshold) {
    return metricsRepository.findRulesWithHighFalsePositiveRate(LocalDate.now(), threshold);
  }

  /**
   * Obtém dashboard de métricas.
   */
  @Transactional(readOnly = true)
  public MetricsDashboard getDashboard(LocalDate startDate, LocalDate endDate) {
    List<RuleMetricsSummary> allMetrics = getAllRulesMetrics(startDate, endDate);

    long totalEvaluations = allMetrics.stream().mapToLong(RuleMetricsSummary::getTotalEvaluations).sum();
    long totalTriggered = allMetrics.stream().mapToLong(RuleMetricsSummary::getTotalTriggered).sum();
    long totalTruePositives = allMetrics.stream().mapToLong(RuleMetricsSummary::getTruePositives).sum();
    long totalFalsePositives = allMetrics.stream().mapToLong(RuleMetricsSummary::getFalsePositives).sum();
    BigDecimal totalAmountBlocked = allMetrics.stream()
        .map(RuleMetricsSummary::getTotalAmountBlocked)
        .reduce(BigDecimal.ZERO, BigDecimal::add);

    double overallTriggerRate = totalEvaluations > 0 ? (double) totalTriggered / totalEvaluations * 100 : 0;
    double overallPrecision = totalTriggered > 0 ? (double) totalTruePositives / totalTriggered * 100 : 0;
    double overallFpRate = totalTriggered > 0 ? (double) totalFalsePositives / totalTriggered * 100 : 0;

    // Top 5 regras por disparos
    List<RuleMetricsSummary> topByTriggers = allMetrics.stream()
        .sorted((a, b) -> Long.compare(b.getTotalTriggered(), a.getTotalTriggered()))
        .limit(5)
        .toList();

    // Top 5 regras por valor bloqueado
    List<RuleMetricsSummary> topByAmount = allMetrics.stream()
        .sorted((a, b) -> b.getTotalAmountBlocked().compareTo(a.getTotalAmountBlocked()))
        .limit(5)
        .toList();

    // Regras com problemas (alta taxa de FP)
    List<RuleMetricsSummary> problematicRules = allMetrics.stream()
        .filter(m -> m.getFalsePositiveRate() > 20.0 && m.getTotalTriggered() > 10)
        .sorted((a, b) -> Double.compare(b.getFalsePositiveRate(), a.getFalsePositiveRate()))
        .limit(5)
        .toList();

    return MetricsDashboard.builder()
        .startDate(startDate)
        .endDate(endDate)
        .totalRulesAnalyzed(allMetrics.size())
        .totalEvaluations(totalEvaluations)
        .totalTriggered(totalTriggered)
        .totalTruePositives(totalTruePositives)
        .totalFalsePositives(totalFalsePositives)
        .totalAmountBlocked(totalAmountBlocked)
        .overallTriggerRate(overallTriggerRate)
        .overallPrecision(overallPrecision)
        .overallFalsePositiveRate(overallFpRate)
        .topRulesByTriggers(topByTriggers)
        .topRulesByAmountBlocked(topByAmount)
        .problematicRules(problematicRules)
        .build();
  }

  // DTOs

  @Data
  @Builder
  public static class RuleMetricsSummary {
    private Long ruleId;
    private String ruleName;
    private LocalDate startDate;
    private LocalDate endDate;
    private long totalEvaluations;
    private long totalTriggered;
    private double triggerRate;
    private long truePositives;
    private long falsePositives;
    private double precision;
    private double falsePositiveRate;
    private BigDecimal totalAmountBlocked;
    private double avgProcessingTimeMs;
    private List<RuleMetrics> dailyMetrics;
  }

  @Data
  @Builder
  public static class MetricsDashboard {
    private LocalDate startDate;
    private LocalDate endDate;
    private int totalRulesAnalyzed;
    private long totalEvaluations;
    private long totalTriggered;
    private long totalTruePositives;
    private long totalFalsePositives;
    private BigDecimal totalAmountBlocked;
    private double overallTriggerRate;
    private double overallPrecision;
    private double overallFalsePositiveRate;
    private List<RuleMetricsSummary> topRulesByTriggers;
    private List<RuleMetricsSummary> topRulesByAmountBlocked;
    private List<RuleMetricsSummary> problematicRules;
  }
}
