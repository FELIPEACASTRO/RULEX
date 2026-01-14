package com.rulex.service;

import io.micrometer.core.instrument.Counter;
import io.micrometer.core.instrument.Gauge;
import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.Timer;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Supplier;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

/**
 * Service for RULEX business metrics using Micrometer.
 *
 * <p>Provides metrics for: - Rule evaluations and triggers - Transaction processing - Fraud
 * detection rates - Performance monitoring
 *
 * <p>All metrics are exposed via /actuator/prometheus endpoint.
 */
@Service
@Slf4j
public class RulexMetricsService {

  private static final String METRIC_PREFIX = "rulex_";

  private final MeterRegistry meterRegistry;

  // Counters
  private final Counter transactionsEvaluatedCounter;
  private final Counter fraudDetectedCounter;

  // Gauges for rule counts
  private final AtomicLong activeRulesCount = new AtomicLong(0);
  private final AtomicLong inactiveRulesCount = new AtomicLong(0);

  // Gauges for fraud blocked amount
  private final AtomicLong fraudBlockedAmountCents = new AtomicLong(0);

  // Maps for dynamic rule metrics
  private final ConcurrentHashMap<String, Counter> ruleTriggersCounters = new ConcurrentHashMap<>();
  private final ConcurrentHashMap<String, Timer> ruleEvaluationTimers = new ConcurrentHashMap<>();
  private final ConcurrentHashMap<String, AtomicLong> rulesByType = new ConcurrentHashMap<>();
  private final ConcurrentHashMap<String, Counter> fraudByTypology = new ConcurrentHashMap<>();
  private final ConcurrentHashMap<String, Counter> fraudByChannel = new ConcurrentHashMap<>();
  private final ConcurrentHashMap<String, Counter> decisionCounters = new ConcurrentHashMap<>();
  private final ConcurrentHashMap<String, Counter> ruleUpdateCounters = new ConcurrentHashMap<>();

  // Timers
  private final Timer transactionEvaluationTimer;

  public RulexMetricsService(MeterRegistry meterRegistry) {
    this.meterRegistry = meterRegistry;

    // Initialize base counters
    this.transactionsEvaluatedCounter =
        Counter.builder(METRIC_PREFIX + "transactions_evaluated_total")
            .description("Total number of transactions evaluated")
            .tag("application", "rulex-api")
            .register(meterRegistry);

    this.fraudDetectedCounter =
        Counter.builder(METRIC_PREFIX + "fraud_detected_total")
            .description("Total number of fraud cases detected")
            .tag("application", "rulex-api")
            .register(meterRegistry);

    // Initialize timers
    this.transactionEvaluationTimer =
        Timer.builder(METRIC_PREFIX + "transaction_evaluation_duration_seconds")
            .description("Transaction evaluation duration in seconds")
            .tag("application", "rulex-api")
            .publishPercentiles(0.5, 0.95, 0.99)
            .register(meterRegistry);

    // Initialize gauges
    Gauge.builder(METRIC_PREFIX + "rules_total", activeRulesCount::get)
        .description("Total number of rules")
        .tag("application", "rulex-api")
        .tag("status", "active")
        .register(meterRegistry);

    Gauge.builder(METRIC_PREFIX + "rules_total", inactiveRulesCount::get)
        .description("Total number of rules")
        .tag("application", "rulex-api")
        .tag("status", "inactive")
        .register(meterRegistry);

    Gauge.builder(
            METRIC_PREFIX + "fraud_blocked_amount_total",
            () -> fraudBlockedAmountCents.get() / 100.0)
        .description("Total amount blocked due to fraud in BRL")
        .tag("application", "rulex-api")
        .register(meterRegistry);

    log.info("RULEX metrics service initialized");
  }

  /** Records a transaction evaluation. */
  public void recordTransactionEvaluated() {
    transactionsEvaluatedCounter.increment();
  }

  /** Records a fraud detection. */
  public void recordFraudDetected() {
    fraudDetectedCounter.increment();
  }

  /**
   * Records the amount blocked due to fraud.
   *
   * @param amountBrl the amount in BRL
   */
  public void recordFraudBlockedAmount(double amountBrl) {
    fraudBlockedAmountCents.addAndGet((long) (amountBrl * 100));
  }

  /**
   * Records a rule trigger event.
   *
   * @param ruleName the name of the rule that was triggered
   */
  public void recordRuleTrigger(String ruleName) {
    Counter counter =
        ruleTriggersCounters.computeIfAbsent(
            ruleName,
            name ->
                Counter.builder(METRIC_PREFIX + "rule_triggers_total")
                    .description("Number of times a rule was triggered")
                    .tag("application", "rulex-api")
                    .tag("rule_name", name)
                    .register(meterRegistry));
    counter.increment();
  }

  /**
   * Records rule evaluation timing.
   *
   * @param ruleName the rule name
   * @param durationNanos duration in nanoseconds
   */
  public void recordRuleEvaluationTime(String ruleName, long durationNanos) {
    Timer timer =
        ruleEvaluationTimers.computeIfAbsent(
            ruleName,
            name ->
                Timer.builder(METRIC_PREFIX + "rule_evaluation_duration_seconds")
                    .description("Rule evaluation duration in seconds")
                    .tag("application", "rulex-api")
                    .tag("rule_name", name)
                    .publishPercentiles(0.5, 0.95, 0.99)
                    .register(meterRegistry));
    timer.record(java.time.Duration.ofNanos(durationNanos));
  }

  /**
   * Times a transaction evaluation.
   *
   * @param supplier the evaluation logic
   * @return the result of the evaluation
   */
  public <T> T timeTransactionEvaluation(Supplier<T> supplier) {
    return transactionEvaluationTimer.record(supplier);
  }

  /**
   * Updates rule counts by type.
   *
   * @param ruleType the rule type
   * @param count the count
   */
  public void updateRuleCountByType(String ruleType, long count) {
    AtomicLong gauge =
        rulesByType.computeIfAbsent(
            ruleType,
            type -> {
              AtomicLong atomicLong = new AtomicLong(0);
              Gauge.builder(METRIC_PREFIX + "rules_by_type_total", atomicLong::get)
                  .description("Number of rules by type")
                  .tag("application", "rulex-api")
                  .tag("type", type)
                  .register(meterRegistry);
              return atomicLong;
            });
    gauge.set(count);
  }

  /**
   * Updates active/inactive rule counts.
   *
   * @param activeCount count of active rules
   * @param inactiveCount count of inactive rules
   */
  public void updateRuleCounts(long activeCount, long inactiveCount) {
    activeRulesCount.set(activeCount);
    inactiveRulesCount.set(inactiveCount);
  }

  /**
   * Records fraud by typology.
   *
   * @param typology the fraud typology
   */
  public void recordFraudByTypology(String typology) {
    Counter counter =
        fraudByTypology.computeIfAbsent(
            typology,
            t ->
                Counter.builder(METRIC_PREFIX + "fraud_by_typology_total")
                    .description("Fraud detections by typology")
                    .tag("application", "rulex-api")
                    .tag("typology", t)
                    .register(meterRegistry));
    counter.increment();
  }

  /**
   * Records fraud by channel.
   *
   * @param channel the transaction channel
   */
  public void recordFraudByChannel(String channel) {
    Counter counter =
        fraudByChannel.computeIfAbsent(
            channel,
            c ->
                Counter.builder(METRIC_PREFIX + "fraud_by_channel_total")
                    .description("Fraud detections by channel")
                    .tag("application", "rulex-api")
                    .tag("channel", c)
                    .register(meterRegistry));
    counter.increment();
  }

  /**
   * Records a transaction decision.
   *
   * @param decision the decision (APPROVED, BLOCKED, REVIEW, CHALLENGE)
   */
  public void recordDecision(String decision) {
    Counter counter =
        decisionCounters.computeIfAbsent(
            decision,
            d ->
                Counter.builder(METRIC_PREFIX + "transaction_decisions_total")
                    .description("Transaction decisions by type")
                    .tag("application", "rulex-api")
                    .tag("decision", d)
                    .register(meterRegistry));
    counter.increment();
  }

  /**
   * Records a rule update operation.
   *
   * @param operation the operation type (CREATE, UPDATE, DELETE, TOGGLE)
   */
  public void recordRuleUpdate(String operation) {
    Counter counter =
        ruleUpdateCounters.computeIfAbsent(
            operation,
            op ->
                Counter.builder(METRIC_PREFIX + "rule_updates_total")
                    .description("Rule configuration updates by operation")
                    .tag("application", "rulex-api")
                    .tag("operation", op)
                    .register(meterRegistry));
    counter.increment();
  }
}
