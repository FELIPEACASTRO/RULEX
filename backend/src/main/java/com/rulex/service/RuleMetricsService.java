package com.rulex.service;

import com.rulex.core.metrics.port.RuleMetricsInputPort;
import com.rulex.core.metrics.port.RuleMetricsRepositoryPort;
import com.rulex.core.metrics.usecase.RuleMetricsUseCase;
import com.rulex.core.metrics.usecase.RuleMetricsUseCase.MetricsDashboard;
import com.rulex.core.metrics.usecase.RuleMetricsUseCase.RuleMetricsSummary;
import com.rulex.entity.RuleMetrics;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/** Adapter Spring para o caso de uso de métricas de regras. */
@Service
@Transactional
public class RuleMetricsService implements RuleMetricsInputPort {

  private final RuleMetricsUseCase useCase;

  public RuleMetricsService(RuleMetricsRepositoryPort repositoryPort) {
    this.useCase = new RuleMetricsUseCase(repositoryPort);
  }

  @Override
  public void recordEvaluation(
      Long ruleId, String ruleName, boolean triggered, BigDecimal amount, long processingTimeMs) {
    useCase.recordEvaluation(ruleId, ruleName, triggered, amount, processingTimeMs);
  }

  @Override
  public void recordFalsePositive(Long ruleId) {
    useCase.recordFalsePositive(ruleId);
  }

  @Override
  public void recordTruePositive(Long ruleId) {
    useCase.recordTruePositive(ruleId);
  }

  @Override
  public RuleMetricsSummary getRuleMetrics(Long ruleId, LocalDate startDate, LocalDate endDate) {
    return useCase.getRuleMetrics(ruleId, startDate, endDate);
  }

  @Override
  public List<RuleMetricsSummary> getAllRulesMetrics(LocalDate startDate, LocalDate endDate) {
    return useCase.getAllRulesMetrics(startDate, endDate);
  }

  @Override
  public List<RuleMetrics> getRulesWithHighFalsePositiveRate(double threshold) {
    return useCase.getRulesWithHighFalsePositiveRate(threshold);
  }

  @Override
  public MetricsDashboard getDashboard(LocalDate startDate, LocalDate endDate) {
    return useCase.getDashboard(startDate, endDate);
  }
}
