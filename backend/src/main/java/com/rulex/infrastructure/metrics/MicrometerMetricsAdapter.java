package com.rulex.infrastructure.metrics;

import com.rulex.application.port.out.MetricsPort;
import io.micrometer.core.instrument.Counter;
import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.Timer;
import java.util.concurrent.TimeUnit;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Adapter Micrometer para métricas de performance.
 *
 * <p>Implementa MetricsPort usando Micrometer/Prometheus.
 */
@Component
@Slf4j
public class MicrometerMetricsAdapter implements MetricsPort {

  private final MeterRegistry registry;

  // Timers
  private final Timer processingTimer;

  // Counters
  private final Counter transactionCounter;
  private final Counter errorCounter;
  private final Counter cacheHitCounter;
  private final Counter cacheMissCounter;
  private final Counter ruleTriggeredCounter;
  private final Counter shadowRuleTriggeredCounter;

  public MicrometerMetricsAdapter(MeterRegistry registry) {
    this.registry = registry;

    // Inicializa métricas
    this.processingTimer =
        Timer.builder("rulex.transaction.processing.time")
            .description("Tempo de processamento de transações")
            .register(registry);

    this.transactionCounter =
        Counter.builder("rulex.transactions.total")
            .description("Total de transações processadas")
            .register(registry);

    this.errorCounter =
        Counter.builder("rulex.errors.total")
            .description("Total de erros")
            .register(registry);

    this.cacheHitCounter =
        Counter.builder("rulex.cache.hits")
            .description("Cache hits")
            .register(registry);

    this.cacheMissCounter =
        Counter.builder("rulex.cache.misses")
            .description("Cache misses")
            .register(registry);

    this.ruleTriggeredCounter =
        Counter.builder("rulex.rules.triggered")
            .description("Regras acionadas")
            .register(registry);

    this.shadowRuleTriggeredCounter =
        Counter.builder("rulex.rules.shadow.triggered")
            .description("Regras shadow acionadas")
            .register(registry);
  }

  @Override
  public void recordProcessingTime(long processingTimeMs, String classification) {
    processingTimer.record(processingTimeMs, TimeUnit.MILLISECONDS);

    // Timer por classificação
    Timer.builder("rulex.transaction.processing.time.by.classification")
        .tag("classification", classification)
        .register(registry)
        .record(processingTimeMs, TimeUnit.MILLISECONDS);
  }

  @Override
  public void incrementTransactionCount(String classification) {
    transactionCounter.increment();

    Counter.builder("rulex.transactions.by.classification")
        .tag("classification", classification)
        .register(registry)
        .increment();
  }

  @Override
  public void recordRuleTriggered(String ruleName, String ruleType) {
    ruleTriggeredCounter.increment();

    Counter.builder("rulex.rules.triggered.by.name")
        .tag("rule", ruleName)
        .tag("type", ruleType)
        .register(registry)
        .increment();
  }

  @Override
  public void recordShadowRuleTriggered(String ruleName) {
    shadowRuleTriggeredCounter.increment();

    Counter.builder("rulex.rules.shadow.triggered.by.name")
        .tag("rule", ruleName)
        .register(registry)
        .increment();
  }

  @Override
  public void incrementErrorCount(String errorType) {
    errorCounter.increment();

    Counter.builder("rulex.errors.by.type")
        .tag("type", errorType)
        .register(registry)
        .increment();
  }

  @Override
  public void recordCacheSize(int size) {
    registry.gauge("rulex.cache.size", size);
  }

  @Override
  public void recordCacheAccess(boolean hit) {
    if (hit) {
      cacheHitCounter.increment();
    } else {
      cacheMissCounter.increment();
    }
  }

  @Override
  public void recordRiskScore(int score) {
    registry
        .summary("rulex.risk.score.distribution")
        .record(score);
  }

  @Override
  public void recordRulesEvaluated(int count) {
    registry
        .summary("rulex.rules.evaluated.count")
        .record(count);
  }
}
