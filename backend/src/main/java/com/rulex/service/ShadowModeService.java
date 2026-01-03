package com.rulex.service;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.RuleConfiguration;
import java.time.Duration;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

/**
 * Shadow Mode Service for safe rule testing in production.
 *
 * <p>Shadow mode allows rules to be evaluated against real production traffic without affecting
 * actual decisions. This enables:
 *
 * <ul>
 *   <li>Safe testing of new rules before activation
 *   <li>Comparison of rule changes (A/B testing)
 *   <li>Gradual rollout with canary deployments
 *   <li>Rule performance measurement without risk
 * </ul>
 *
 * <p>How it works:
 *
 * <ol>
 *   <li>Rules marked as SHADOW are evaluated alongside active rules
 *   <li>Shadow rule results are logged but NOT included in final decision
 *   <li>Statistics track what WOULD have happened vs what DID happen
 *   <li>When satisfied, promote shadow rule to active
 * </ol>
 *
 * <p>Benefits:
 *
 * <ul>
 *   <li>Zero risk: Shadow rules never block legitimate transactions
 *   <li>Real data: Tests with actual production traffic patterns
 *   <li>Measurable: Track false positive/negative rates before deployment
 *   <li>Gradual: Can run shadow for days/weeks before activating
 * </ul>
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class ShadowModeService {

  // Configuration
  private static final Duration STATS_RETENTION = Duration.ofDays(30);
  private static final int MAX_SHADOW_RESULTS_PER_RULE = 100_000;

  // Shadow rule execution - async to not impact main flow
  private final ExecutorService shadowExecutor = Executors.newVirtualThreadPerTaskExecutor();

  // Statistics per shadow rule
  private final Map<Long, ShadowRuleStats> shadowStats = new ConcurrentHashMap<>();

  // Recent shadow evaluations for analysis
  private final Map<Long, List<ShadowEvaluation>> recentEvaluations = new ConcurrentHashMap<>();

  // Global counters
  private final AtomicLong totalShadowEvaluations = new AtomicLong(0);
  private final AtomicLong shadowRulesTriggered = new AtomicLong(0);
  private final AtomicLong potentialFalsePositives = new AtomicLong(0);
  private final AtomicLong potentialCatches = new AtomicLong(0);

  /** Shadow rule execution result. */
  public record ShadowEvaluation(
      Long ruleId,
      String ruleName,
      String transactionId,
      boolean triggered,
      int score,
      String action,
      LocalDateTime evaluatedAt,
      long latencyMicros,
      Map<String, Object> metadata) {}

  /** Statistics for a shadow rule. */
  public record ShadowRuleStats(
      Long ruleId,
      String ruleName,
      AtomicLong evaluationCount,
      AtomicLong triggerCount,
      AtomicLong totalScore,
      AtomicLong wouldHaveBlocked,
      AtomicLong wouldHaveFlagged,
      AtomicLong wouldHaveAllowed,
      AtomicLong potentialFalsePositives, // Triggered but active rules allowed
      AtomicLong potentialCatches, // Triggered and transaction was actually fraud
      double avgLatencyMicros,
      LocalDateTime firstEvaluation,
      LocalDateTime lastEvaluation) {
    public static ShadowRuleStats create(Long ruleId, String ruleName) {
      return new ShadowRuleStats(
          ruleId,
          ruleName,
          new AtomicLong(0),
          new AtomicLong(0),
          new AtomicLong(0),
          new AtomicLong(0),
          new AtomicLong(0),
          new AtomicLong(0),
          new AtomicLong(0),
          new AtomicLong(0),
          0.0,
          LocalDateTime.now(),
          LocalDateTime.now());
    }
  }

  /** Shadow rule promotion readiness assessment. */
  public record PromotionAssessment(
      Long ruleId,
      String ruleName,
      boolean isReady,
      double triggerRate,
      double estimatedFalsePositiveRate,
      double estimatedCatchRate,
      long evaluationCount,
      long minimumRecommended,
      List<String> concerns,
      String recommendation) {}

  /** Enum for shadow rule modes. */
  public enum ShadowMode {
    /** Rule is active and affects decisions */
    DISABLED,
    /** Rule is evaluated but doesn't affect decisions */
    SHADOW,
    /** Rule is evaluated for a percentage of traffic */
    CANARY
  }

  /**
   * Executes a rule in shadow mode (asynchronously).
   *
   * @param rule The rule to evaluate
   * @param transaction The transaction to evaluate against
   * @param ruleEvaluator Function to evaluate the rule
   * @param actualDecision The actual decision made by active rules
   */
  public void executeShadow(
      RuleConfiguration rule,
      TransactionRequest transaction,
      RuleEvaluator ruleEvaluator,
      String actualDecision) {
    shadowExecutor.submit(
        () -> {
          try {
            long startNanos = System.nanoTime();

            // Evaluate the shadow rule
            RuleEvaluationResult result = ruleEvaluator.evaluate(rule, transaction);

            long latencyMicros = (System.nanoTime() - startNanos) / 1000;

            // Record the evaluation
            recordShadowEvaluation(rule, transaction, result, actualDecision, latencyMicros);

          } catch (Exception e) {
            log.warn("Shadow rule evaluation failed for rule {}: {}", rule.getId(), e.getMessage());
          }
        });
  }

  /** Gets statistics for a specific shadow rule. */
  public ShadowRuleStats getStats(Long ruleId) {
    return shadowStats.get(ruleId);
  }

  /** Gets statistics for all shadow rules. */
  public List<ShadowRuleStats> getAllStats() {
    return List.copyOf(shadowStats.values());
  }

  /** Gets recent evaluations for a shadow rule. */
  public List<ShadowEvaluation> getRecentEvaluations(Long ruleId, int limit) {
    List<ShadowEvaluation> evals = recentEvaluations.get(ruleId);
    if (evals == null) return List.of();

    return evals.stream()
        .sorted((a, b) -> b.evaluatedAt().compareTo(a.evaluatedAt()))
        .limit(limit)
        .collect(Collectors.toList());
  }

  /** Assesses if a shadow rule is ready for promotion to active. */
  public PromotionAssessment assessPromotion(Long ruleId) {
    ShadowRuleStats stats = shadowStats.get(ruleId);
    if (stats == null) {
      return new PromotionAssessment(
          ruleId,
          "Unknown",
          false,
          0,
          0,
          0,
          0,
          10000,
          List.of("No shadow data available for this rule"),
          "Run rule in shadow mode to gather data");
    }

    List<String> concerns = new java.util.ArrayList<>();
    long evalCount = stats.evaluationCount().get();
    long triggerCount = stats.triggerCount().get();

    // Calculate rates
    double triggerRate = evalCount > 0 ? (double) triggerCount / evalCount * 100 : 0;
    double estimatedFPRate =
        triggerCount > 0 ? (double) stats.potentialFalsePositives().get() / triggerCount * 100 : 0;
    double estimatedCatchRate =
        triggerCount > 0 ? (double) stats.potentialCatches().get() / triggerCount * 100 : 0;

    // Minimum evaluations check
    long minimumRecommended = 10_000;
    boolean hasEnoughData = evalCount >= minimumRecommended;
    if (!hasEnoughData) {
      concerns.add(
          "Insufficient data: "
              + evalCount
              + " evaluations (recommend "
              + minimumRecommended
              + ")");
    }

    // Trigger rate check (too high might indicate too aggressive)
    if (triggerRate > 5.0) {
      concerns.add(
          "High trigger rate: "
              + String.format("%.2f%%", triggerRate)
              + " (may cause many false positives)");
    }

    // False positive check
    if (estimatedFPRate > 1.0) {
      concerns.add(
          "Estimated FP rate: " + String.format("%.2f%%", estimatedFPRate) + " (recommend < 1%)");
    }

    // Catch rate check
    if (estimatedCatchRate < 50 && triggerCount > 100) {
      concerns.add(
          "Low catch rate: "
              + String.format("%.2f%%", estimatedCatchRate)
              + " (rule may not be effective)");
    }

    boolean isReady = hasEnoughData && concerns.isEmpty();
    String recommendation =
        isReady
            ? "Rule appears ready for promotion. Consider canary deployment first."
            : "Address concerns before promoting. Continue shadow evaluation.";

    return new PromotionAssessment(
        ruleId,
        stats.ruleName(),
        isReady,
        triggerRate,
        estimatedFPRate,
        estimatedCatchRate,
        evalCount,
        minimumRecommended,
        concerns,
        recommendation);
  }

  /** Gets global shadow statistics. */
  public Map<String, Object> getGlobalStats() {
    return Map.of(
        "totalShadowEvaluations", totalShadowEvaluations.get(),
        "shadowRulesTriggered", shadowRulesTriggered.get(),
        "potentialFalsePositives", potentialFalsePositives.get(),
        "potentialCatches", potentialCatches.get(),
        "activeShadowRules", shadowStats.size());
  }

  /** Resets statistics for a rule (e.g., after rule modification). */
  public void resetStats(Long ruleId) {
    shadowStats.remove(ruleId);
    recentEvaluations.remove(ruleId);
    log.info("Reset shadow stats for rule {}", ruleId);
  }

  /**
   * Marks a known fraud transaction for better statistics. Call this when a transaction is
   * confirmed as fraud (e.g., chargeback).
   */
  public void markTransactionAsFraud(String transactionId) {
    // Update all shadow evaluations for this transaction
    for (var entry : recentEvaluations.entrySet()) {
      Long ruleId = entry.getKey();
      List<ShadowEvaluation> evals = entry.getValue();

      boolean ruleTriggered =
          evals.stream()
              .filter(e -> e.transactionId().equals(transactionId))
              .anyMatch(ShadowEvaluation::triggered);

      if (ruleTriggered) {
        ShadowRuleStats stats = shadowStats.get(ruleId);
        if (stats != null) {
          stats.potentialCatches().incrementAndGet();
          potentialCatches.incrementAndGet();
        }
      }
    }
  }

  // ========== Internal Methods ==========

  private void recordShadowEvaluation(
      RuleConfiguration rule,
      TransactionRequest transaction,
      RuleEvaluationResult result,
      String actualDecision,
      long latencyMicros) {
    Long ruleId = rule.getId();
    String ruleName = rule.getRuleName();
    boolean triggered = result.triggered();
    int score = result.score();
    String action = result.action();

    // Update global counters
    totalShadowEvaluations.incrementAndGet();
    if (triggered) {
      shadowRulesTriggered.incrementAndGet();
    }

    // Update rule-specific stats
    ShadowRuleStats stats =
        shadowStats.computeIfAbsent(ruleId, k -> ShadowRuleStats.create(ruleId, ruleName));

    stats.evaluationCount().incrementAndGet();
    if (triggered) {
      stats.triggerCount().incrementAndGet();
      stats.totalScore().addAndGet(score);

      // Track what would have happened
      switch (action) {
        case "BLOCK" -> stats.wouldHaveBlocked().incrementAndGet();
        case "FLAG", "REVIEW" -> stats.wouldHaveFlagged().incrementAndGet();
        default -> stats.wouldHaveAllowed().incrementAndGet();
      }

      // Check for potential false positive
      // (Shadow rule triggered but actual decision was ALLOW)
      if ("ALLOW".equals(actualDecision) || "APPROVE".equals(actualDecision)) {
        stats.potentialFalsePositives().incrementAndGet();
        potentialFalsePositives.incrementAndGet();
      }
    }

    // Record recent evaluation
    ShadowEvaluation eval =
        new ShadowEvaluation(
            ruleId,
            ruleName,
            transaction.getExternalTransactionId() != null
                ? transaction.getExternalTransactionId()
                : "unknown",
            triggered,
            score,
            action,
            LocalDateTime.now(),
            latencyMicros,
            Map.of("panHash", hashPan(transaction.getPan())));

    recentEvaluations.compute(
        ruleId,
        (k, existing) -> {
          if (existing == null) {
            existing = new java.util.concurrent.CopyOnWriteArrayList<>();
          }
          existing.add(eval);
          // Limit size
          while (existing.size() > MAX_SHADOW_RESULTS_PER_RULE) {
            existing.remove(0);
          }
          return existing;
        });

    log.debug(
        "Shadow eval: rule={}, triggered={}, score={}, action={}, actualDecision={}",
        ruleName,
        triggered,
        score,
        action,
        actualDecision);
  }

  private String hashPan(String pan) {
    if (pan == null || pan.length() < 4) return "****";
    return "****" + pan.substring(pan.length() - 4);
  }

  @Scheduled(fixedRate = 86400_000) // Daily cleanup
  public void cleanupOldEvaluations() {
    LocalDateTime cutoff = LocalDateTime.now().minus(STATS_RETENTION);

    for (var entry : recentEvaluations.entrySet()) {
      entry.getValue().removeIf(e -> e.evaluatedAt().isBefore(cutoff));
    }

    // Remove empty entries
    recentEvaluations.entrySet().removeIf(e -> e.getValue().isEmpty());

    log.info("Shadow evaluation cleanup complete. Active rules: {}", shadowStats.size());
  }

  // ========== Supporting Interfaces ==========

  /** Functional interface for rule evaluation. */
  @FunctionalInterface
  public interface RuleEvaluator {
    RuleEvaluationResult evaluate(RuleConfiguration rule, TransactionRequest transaction);
  }

  /** Result of a rule evaluation. */
  public record RuleEvaluationResult(boolean triggered, int score, String action, String reason) {
    public static RuleEvaluationResult notTriggered() {
      return new RuleEvaluationResult(false, 0, "ALLOW", null);
    }

    public static RuleEvaluationResult triggered(int score, String action, String reason) {
      return new RuleEvaluationResult(true, score, action, reason);
    }
  }
}
