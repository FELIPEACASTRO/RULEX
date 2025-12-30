package com.rulex.controller;

import com.rulex.service.RuleMetricsService;
import com.rulex.service.RuleMetricsService.MetricsDashboard;
import com.rulex.service.RuleMetricsService.RuleMetricsSummary;
import java.time.LocalDate;
import java.util.List;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/** Controller para métricas de regras. */
@RestController
@RequestMapping("/rules/metrics")
@RequiredArgsConstructor
@Slf4j
public class RuleMetricsController {

  private final RuleMetricsService metricsService;

  /** Obtém dashboard de métricas. GET /api/rules/metrics/dashboard */
  @GetMapping("/dashboard")
  public ResponseEntity<MetricsDashboard> getDashboard(
      @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
          LocalDate startDate,
      @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
          LocalDate endDate) {

    if (startDate == null) {
      startDate = LocalDate.now().minusDays(30);
    }
    if (endDate == null) {
      endDate = LocalDate.now();
    }

    log.info("Obtendo dashboard de métricas de {} a {}", startDate, endDate);
    MetricsDashboard dashboard = metricsService.getDashboard(startDate, endDate);
    return ResponseEntity.ok(dashboard);
  }

  /** Obtém métricas de uma regra específica. GET /api/rules/metrics/{ruleId} */
  @GetMapping("/{ruleId}")
  public ResponseEntity<RuleMetricsSummary> getRuleMetrics(
      @PathVariable Long ruleId,
      @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
          LocalDate startDate,
      @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
          LocalDate endDate) {

    if (startDate == null) {
      startDate = LocalDate.now().minusDays(30);
    }
    if (endDate == null) {
      endDate = LocalDate.now();
    }

    log.info("Obtendo métricas da regra {} de {} a {}", ruleId, startDate, endDate);
    RuleMetricsSummary metrics = metricsService.getRuleMetrics(ruleId, startDate, endDate);
    return ResponseEntity.ok(metrics);
  }

  /** Obtém métricas de todas as regras. GET /api/rules/metrics/all */
  @GetMapping("/all")
  public ResponseEntity<List<RuleMetricsSummary>> getAllRulesMetrics(
      @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
          LocalDate startDate,
      @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
          LocalDate endDate) {

    if (startDate == null) {
      startDate = LocalDate.now().minusDays(30);
    }
    if (endDate == null) {
      endDate = LocalDate.now();
    }

    log.info("Obtendo métricas de todas as regras de {} a {}", startDate, endDate);
    List<RuleMetricsSummary> metrics = metricsService.getAllRulesMetrics(startDate, endDate);
    return ResponseEntity.ok(metrics);
  }

  /** Registra feedback de falso positivo. POST /api/rules/metrics/{ruleId}/false-positive */
  @PostMapping("/{ruleId}/false-positive")
  public ResponseEntity<Void> recordFalsePositive(@PathVariable Long ruleId) {
    log.info("Registrando falso positivo para regra {}", ruleId);
    metricsService.recordFalsePositive(ruleId);
    return ResponseEntity.ok().build();
  }

  /** Registra feedback de verdadeiro positivo. POST /api/rules/metrics/{ruleId}/true-positive */
  @PostMapping("/{ruleId}/true-positive")
  public ResponseEntity<Void> recordTruePositive(@PathVariable Long ruleId) {
    log.info("Registrando verdadeiro positivo para regra {}", ruleId);
    metricsService.recordTruePositive(ruleId);
    return ResponseEntity.ok().build();
  }
}
