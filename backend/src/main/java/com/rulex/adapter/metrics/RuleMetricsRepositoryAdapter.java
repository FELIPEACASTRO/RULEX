package com.rulex.adapter.metrics;

import com.rulex.core.metrics.port.RuleMetricsRepositoryPort;
import com.rulex.entity.RuleMetrics;
import com.rulex.repository.RuleMetricsRepository;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import org.springframework.stereotype.Component;

@Component
public class RuleMetricsRepositoryAdapter implements RuleMetricsRepositoryPort {

  private final RuleMetricsRepository repository;

  public RuleMetricsRepositoryAdapter(RuleMetricsRepository repository) {
    this.repository = repository;
  }

  @Override
  public Optional<RuleMetrics> findByRuleIdAndMetricDate(Long ruleId, LocalDate metricDate) {
    return repository.findByRuleIdAndMetricDate(ruleId, metricDate);
  }

  @Override
  public List<RuleMetrics> findByRuleIdAndMetricDateBetweenOrderByMetricDateDesc(
      Long ruleId, LocalDate startDate, LocalDate endDate) {
    return repository.findByRuleIdAndMetricDateBetweenOrderByMetricDateDesc(
        ruleId, startDate, endDate);
  }

  @Override
  public List<Object[]> getAggregatedMetricsByPeriod(LocalDate startDate, LocalDate endDate) {
    return repository.getAggregatedMetricsByPeriod(startDate, endDate);
  }

  @Override
  public List<RuleMetrics> findRulesWithHighFalsePositiveRate(
      LocalDate metricDate, double threshold) {
    return repository.findRulesWithHighFalsePositiveRate(metricDate, threshold);
  }

  @Override
  public RuleMetrics save(RuleMetrics metrics) {
    return repository.save(metrics);
  }
}
