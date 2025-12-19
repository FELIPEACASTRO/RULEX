package com.rulex.controller;

import com.rulex.dto.MetricsDTO;
import com.rulex.service.MetricsService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/** Controller REST para métricas em tempo real. */
@RestController
@RequestMapping("/metrics")
@RequiredArgsConstructor
@Slf4j
public class MetricsController {

  private final MetricsService metricsService;

  /** Obtém métricas gerais do sistema. GET /api/metrics */
  @GetMapping
  public ResponseEntity<MetricsDTO> getMetrics(@RequestParam(required = false) String period) {

    log.info("Obtendo métricas: period={}", period);

    MetricsDTO metrics = metricsService.getMetrics(period);
    return ResponseEntity.ok(metrics);
  }

  /** Obtém métricas por MCC. GET /api/metrics/mcc */
  @GetMapping("/mcc")
  public ResponseEntity<?> getMetricsByMcc(@RequestParam(required = false) String period) {

    log.info("Obtendo métricas por MCC: period={}", period);

    var metrics = metricsService.getMetricsByMcc(period);
    return ResponseEntity.ok(metrics);
  }

  /** Obtém métricas por merchant. GET /api/metrics/merchant */
  @GetMapping("/merchant")
  public ResponseEntity<?> getMetricsByMerchant(@RequestParam(required = false) String period) {

    log.info("Obtendo métricas por merchant: period={}", period);

    var metrics = metricsService.getMetricsByMerchant(period);
    return ResponseEntity.ok(metrics);
  }

  /** Obtém métricas por período. GET /api/metrics/timeline */
  @GetMapping("/timeline")
  public ResponseEntity<?> getMetricsTimeline(@RequestParam(required = false) String granularity) {

    log.info("Obtendo timeline de métricas: granularity={}", granularity);

    var metrics = metricsService.getMetricsTimeline(granularity);
    return ResponseEntity.ok(metrics);
  }
}
