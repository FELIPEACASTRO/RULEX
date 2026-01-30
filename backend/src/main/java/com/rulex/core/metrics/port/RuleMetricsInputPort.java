package com.rulex.core.metrics.port;

import com.rulex.core.metrics.usecase.RuleMetricsUseCase.MetricsDashboard;
import com.rulex.core.metrics.usecase.RuleMetricsUseCase.RuleMetricsSummary;
import com.rulex.entity.RuleMetrics;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

public interface RuleMetricsInputPort {

  void recordEvaluation(
      Long ruleId, String ruleName, boolean triggered, BigDecimal amount, long processingTimeMs);

  void recordFalsePositive(Long ruleId);

  void recordTruePositive(Long ruleId);

  RuleMetricsSummary getRuleMetrics(Long ruleId, LocalDate startDate, LocalDate endDate);

  List<RuleMetricsSummary> getAllRulesMetrics(LocalDate startDate, LocalDate endDate);

  List<RuleMetrics> getRulesWithHighFalsePositiveRate(double threshold);

  MetricsDashboard getDashboard(LocalDate startDate, LocalDate endDate);
}
