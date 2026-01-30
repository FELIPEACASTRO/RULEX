package com.rulex.core.metrics.port;

import com.rulex.entity.RuleMetrics;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

public interface RuleMetricsRepositoryPort {

  Optional<RuleMetrics> findByRuleIdAndMetricDate(Long ruleId, LocalDate metricDate);

  List<RuleMetrics> findByRuleIdAndMetricDateBetweenOrderByMetricDateDesc(
      Long ruleId, LocalDate startDate, LocalDate endDate);

  List<Object[]> getAggregatedMetricsByPeriod(LocalDate startDate, LocalDate endDate);

  List<RuleMetrics> findRulesWithHighFalsePositiveRate(LocalDate metricDate, double threshold);

  RuleMetrics save(RuleMetrics metrics);
}
