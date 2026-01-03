package com.rulex.service;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.RuleConfiguration;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

/**
 * Parallel Rule Execution Engine for high-throughput fraud detection.
 * 
 * <p>Optimizes rule execution through:
 * <ul>
 *   <li>Parallel execution of independent rules using Virtual Threads</li>
 *   <li>Early termination on definite BLOCK decisions</li>
 *   <li>Tiered execution (fast rules first, slow rules if needed)</li>
 *   <li>Timeout protection per rule</li>
 *   <li>Graceful degradation on failures</li>
 * </ul>
 * 
 * <p>Execution Strategy:
 * <ol>
 *   <li>TIER 1 (P99 &lt; 1ms): Blocklists, simple field checks</li>
 *   <li>TIER 2 (P99 &lt; 10ms): Velocity checks, pattern matching</li>
 *   <li>TIER 3 (P99 &lt; 100ms): Complex aggregations, ML scoring</li>
 * </ol>
 * 
 * <p>If TIER 1 returns BLOCK, skip remaining tiers (early termination).
 */
@Service
@Slf4j
public class ParallelRuleExecutionService {

    // Virtual thread executor for parallel rule execution
    private final ExecutorService virtualThreadExecutor = Executors.newVirtualThreadPerTaskExecutor();

    // Statistics
    private final AtomicLong totalExecutions = new AtomicLong(0);
    private final AtomicLong earlyTerminations = new AtomicLong(0);
    private final AtomicLong timeouts = new AtomicLong(0);
    private final AtomicLong parallelExecutions = new AtomicLong(0);
    private final Map<String, RuleExecutionStats> ruleStats = new ConcurrentHashMap<>();

    // Configuration
    private static final Duration DEFAULT_RULE_TIMEOUT = Duration.ofMillis(100);
    private static final Duration TIER1_TIMEOUT = Duration.ofMillis(5);
    private static final Duration TIER2_TIMEOUT = Duration.ofMillis(50);
    private static final Duration TIER3_TIMEOUT = Duration.ofMillis(200);

    /**
     * Rule execution tier based on expected latency.
     */
    public enum ExecutionTier {
        /** Fast rules: blocklists, simple checks (< 1ms) */
        TIER_1(1),
        /** Medium rules: velocity, patterns (< 10ms) */
        TIER_2(2),
        /** Slow rules: complex aggregations (< 100ms) */
        TIER_3(3);

        private final int order;
        ExecutionTier(int order) { this.order = order; }
        public int getOrder() { return order; }
    }

    /**
     * Rule with execution metadata.
     */
    public record ExecutableRule(
            Long id,
            String name,
            ExecutionTier tier,
            int priority,
            Function<TransactionRequest, RuleResult> evaluator
    ) implements Comparable<ExecutableRule> {
        @Override
        public int compareTo(ExecutableRule other) {
            int tierCompare = Integer.compare(this.tier.getOrder(), other.tier.getOrder());
            return tierCompare != 0 ? tierCompare : Integer.compare(other.priority, this.priority);
        }
    }

    /**
     * Result of a single rule execution.
     */
    public record RuleResult(
            Long ruleId,
            String ruleName,
            boolean triggered,
            int score,
            String action, // ALLOW, FLAG, REVIEW, BLOCK
            String reason,
            long latencyMicros,
            ExecutionTier tier,
            boolean timedOut,
            boolean failed
    ) {
        public static RuleResult notTriggered(Long ruleId, String name, long latency, ExecutionTier tier) {
            return new RuleResult(ruleId, name, false, 0, "ALLOW", null, latency, tier, false, false);
        }

        public static RuleResult triggered(Long ruleId, String name, int score, String action, 
                                            String reason, long latency, ExecutionTier tier) {
            return new RuleResult(ruleId, name, true, score, action, reason, latency, tier, false, false);
        }

        public static RuleResult timeout(Long ruleId, String name, ExecutionTier tier) {
            return new RuleResult(ruleId, name, false, 0, "ALLOW", "TIMEOUT", 0, tier, true, false);
        }

        public static RuleResult failed(Long ruleId, String name, String reason, ExecutionTier tier) {
            return new RuleResult(ruleId, name, false, 0, "ALLOW", reason, 0, tier, false, true);
        }

        public boolean isBlockingDecision() {
            return "BLOCK".equals(action) || "DECLINE".equals(action);
        }
    }

    /**
     * Aggregated execution result.
     */
    public record ExecutionResult(
            List<RuleResult> results,
            String finalDecision,
            int totalScore,
            long totalLatencyMicros,
            int rulesEvaluated,
            int rulesTriggered,
            int rulesSkipped,
            boolean earlyTerminated,
            List<String> triggeredRules,
            Map<String, Object> metadata
    ) {}

    /**
     * Statistics for a rule.
     */
    private static class RuleExecutionStats {
        final AtomicLong executions = new AtomicLong(0);
        final AtomicLong totalLatency = new AtomicLong(0);
        final AtomicLong triggers = new AtomicLong(0);
        final AtomicLong timeouts = new AtomicLong(0);
        final AtomicLong failures = new AtomicLong(0);

        void record(RuleResult result) {
            executions.incrementAndGet();
            totalLatency.addAndGet(result.latencyMicros());
            if (result.triggered()) triggers.incrementAndGet();
            if (result.timedOut()) timeouts.incrementAndGet();
            if (result.failed()) failures.incrementAndGet();
        }

        Map<String, Object> toMap() {
            long execs = executions.get();
            return Map.of(
                    "executions", execs,
                    "avgLatencyMicros", execs > 0 ? totalLatency.get() / execs : 0,
                    "triggerRate", execs > 0 ? (double) triggers.get() / execs * 100 : 0,
                    "timeoutRate", execs > 0 ? (double) timeouts.get() / execs * 100 : 0,
                    "failureRate", execs > 0 ? (double) failures.get() / execs * 100 : 0
            );
        }
    }

    /**
     * Executes rules in parallel with tiered execution and early termination.
     * 
     * @param transaction The transaction to evaluate
     * @param rules List of executable rules
     * @return Aggregated execution result
     */
    public ExecutionResult executeRules(TransactionRequest transaction, List<ExecutableRule> rules) {
        long startNanos = System.nanoTime();
        totalExecutions.incrementAndGet();

        // Sort rules by tier and priority
        List<ExecutableRule> sortedRules = rules.stream()
                .sorted()
                .collect(Collectors.toList());

        // Group rules by tier
        Map<ExecutionTier, List<ExecutableRule>> tierGroups = sortedRules.stream()
                .collect(Collectors.groupingBy(ExecutableRule::tier));

        List<RuleResult> allResults = new ArrayList<>();
        int totalScore = 0;
        int rulesSkipped = 0;
        boolean earlyTerminated = false;

        // Execute tier by tier
        for (ExecutionTier tier : ExecutionTier.values()) {
            if (earlyTerminated) {
                rulesSkipped += tierGroups.getOrDefault(tier, List.of()).size();
                continue;
            }

            List<ExecutableRule> tierRules = tierGroups.getOrDefault(tier, List.of());
            if (tierRules.isEmpty()) continue;

            // Execute all rules in this tier in parallel
            List<RuleResult> tierResults = executeParallel(transaction, tierRules, getTimeout(tier));
            allResults.addAll(tierResults);

            // Check for early termination (BLOCK decision)
            for (RuleResult result : tierResults) {
                if (result.triggered()) {
                    totalScore += result.score();
                    if (result.isBlockingDecision()) {
                        earlyTerminated = true;
                        earlyTerminations.incrementAndGet();
                        log.debug("Early termination: {} triggered BLOCK", result.ruleName());
                        break;
                    }
                }
            }
        }

        long totalLatencyMicros = (System.nanoTime() - startNanos) / 1000;

        // Determine final decision
        String finalDecision = determineFinalDecision(allResults, totalScore);

        // Collect triggered rule names
        List<String> triggeredRules = allResults.stream()
                .filter(RuleResult::triggered)
                .map(RuleResult::ruleName)
                .collect(Collectors.toList());

        return new ExecutionResult(
                allResults,
                finalDecision,
                totalScore,
                totalLatencyMicros,
                allResults.size(),
                triggeredRules.size(),
                rulesSkipped,
                earlyTerminated,
                triggeredRules,
                Map.of(
                        "tier1Rules", tierGroups.getOrDefault(ExecutionTier.TIER_1, List.of()).size(),
                        "tier2Rules", tierGroups.getOrDefault(ExecutionTier.TIER_2, List.of()).size(),
                        "tier3Rules", tierGroups.getOrDefault(ExecutionTier.TIER_3, List.of()).size()
                )
        );
    }

    /**
     * Executes rules in parallel using virtual threads.
     */
    private List<RuleResult> executeParallel(TransactionRequest transaction, 
                                              List<ExecutableRule> rules,
                                              Duration timeout) {
        if (rules.isEmpty()) return List.of();
        
        parallelExecutions.addAndGet(rules.size());

        // Create futures for all rules
        List<CompletableFuture<RuleResult>> futures = rules.stream()
                .map(rule -> CompletableFuture.supplyAsync(
                        () -> executeRule(transaction, rule, timeout),
                        virtualThreadExecutor
                ))
                .collect(Collectors.toList());

        // Wait for all to complete with overall timeout
        try {
            @SuppressWarnings("rawtypes")
            CompletableFuture[] futuresArray = futures.toArray(new CompletableFuture[0]);
            CompletableFuture.allOf(futuresArray)
                    .get(timeout.toMillis() * 2, TimeUnit.MILLISECONDS);
        } catch (Exception e) {
            log.warn("Some rules timed out during parallel execution: {}", e.getMessage());
        }

        // Collect results
        return futures.stream()
                .map(f -> {
                    try {
                        return f.getNow(null);
                    } catch (Exception e) {
                        return null;
                    }
                })
                .filter(r -> r != null)
                .peek(this::recordStats)
                .collect(Collectors.toList());
    }

    /**
     * Executes a single rule with timeout protection.
     */
    private RuleResult executeRule(TransactionRequest transaction, ExecutableRule rule, Duration timeout) {
        long startNanos = System.nanoTime();
        
        try {
            // Execute with timeout
            CompletableFuture<RuleResult> future = CompletableFuture.supplyAsync(
                    () -> rule.evaluator().apply(transaction),
                    virtualThreadExecutor
            );

            RuleResult result = future.get(timeout.toMillis(), TimeUnit.MILLISECONDS);
            return result;

        } catch (java.util.concurrent.TimeoutException e) {
            timeouts.incrementAndGet();
            log.warn("Rule {} timed out after {}ms", rule.name(), timeout.toMillis());
            return RuleResult.timeout(rule.id(), rule.name(), rule.tier());
            
        } catch (Exception e) {
            log.error("Rule {} failed: {}", rule.name(), e.getMessage());
            return RuleResult.failed(rule.id(), rule.name(), e.getMessage(), rule.tier());
        }
    }

    private Duration getTimeout(ExecutionTier tier) {
        return switch (tier) {
            case TIER_1 -> TIER1_TIMEOUT;
            case TIER_2 -> TIER2_TIMEOUT;
            case TIER_3 -> TIER3_TIMEOUT;
        };
    }

    private String determineFinalDecision(List<RuleResult> results, int totalScore) {
        // Check for explicit BLOCK
        boolean hasBlock = results.stream()
                .filter(RuleResult::triggered)
                .anyMatch(r -> "BLOCK".equals(r.action()) || "DECLINE".equals(r.action()));
        
        if (hasBlock) return "BLOCK";

        // Check for REVIEW
        boolean hasReview = results.stream()
                .filter(RuleResult::triggered)
                .anyMatch(r -> "REVIEW".equals(r.action()));
        
        if (hasReview || totalScore >= 80) return "REVIEW";

        // Check for FLAG
        boolean hasFlag = results.stream()
                .filter(RuleResult::triggered)
                .anyMatch(r -> "FLAG".equals(r.action()));
        
        if (hasFlag || totalScore >= 50) return "FLAG";

        return "ALLOW";
    }

    private void recordStats(RuleResult result) {
        ruleStats.computeIfAbsent(result.ruleName(), k -> new RuleExecutionStats())
                .record(result);
    }

    /**
     * Gets execution statistics.
     */
    public Map<String, Object> getStatistics() {
        return Map.of(
                "totalExecutions", totalExecutions.get(),
                "parallelExecutions", parallelExecutions.get(),
                "earlyTerminations", earlyTerminations.get(),
                "timeouts", timeouts.get(),
                "earlyTerminationRate", totalExecutions.get() > 0 
                        ? (double) earlyTerminations.get() / totalExecutions.get() * 100 : 0,
                "ruleStats", ruleStats.entrySet().stream()
                        .collect(Collectors.toMap(Map.Entry::getKey, e -> e.getValue().toMap()))
        );
    }

    /**
     * Gets statistics for a specific rule.
     */
    public Map<String, Object> getRuleStats(String ruleName) {
        RuleExecutionStats stats = ruleStats.get(ruleName);
        return stats != null ? stats.toMap() : Map.of();
    }
}
