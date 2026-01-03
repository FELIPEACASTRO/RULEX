package com.rulex.service;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.RuleConfiguration;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

/**
 * A/B Testing Service for comparing rule variants.
 * 
 * <p>Enables controlled experiments to compare rule changes:
 * <ul>
 *   <li>Compare new rule version vs existing (CONTROL vs TREATMENT)</li>
 *   <li>Statistical significance calculation</li>
 *   <li>Sticky assignment - same PAN always gets same variant</li>
 *   <li>Automatic winner detection</li>
 * </ul>
 * 
 * <p>Key Metrics Tracked:
 * <ul>
 *   <li>Trigger Rate: % of transactions triggering the rule</li>
 *   <li>False Positive Rate: Triggers on legitimate transactions</li>
 *   <li>True Positive Rate: Catches on confirmed fraud</li>
 *   <li>Latency: Performance impact of rule</li>
 * </ul>
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class ABTestingService {

    // Active experiments
    private final Map<Long, Experiment> activeExperiments = new ConcurrentHashMap<>();
    
    // Assignment cache (sticky assignment)
    private final Map<String, ExperimentAssignment> assignmentCache = new ConcurrentHashMap<>();

    /**
     * Experiment definition.
     */
    public record Experiment(
            Long id,
            String name,
            String description,
            Long controlRuleId,
            Long treatmentRuleId,
            int treatmentPercentage,
            ExperimentStatus status,
            LocalDateTime startedAt,
            LocalDateTime endedAt,
            ExperimentMetrics controlMetrics,
            ExperimentMetrics treatmentMetrics,
            String targetSegment // JSON for targeting
    ) {
        public Experiment withStatus(ExperimentStatus newStatus) {
            return new Experiment(id, name, description, controlRuleId, treatmentRuleId,
                    treatmentPercentage, newStatus, startedAt, 
                    newStatus == ExperimentStatus.COMPLETED ? LocalDateTime.now() : endedAt,
                    controlMetrics, treatmentMetrics, targetSegment);
        }
    }

    /**
     * Experiment status.
     */
    public enum ExperimentStatus {
        DRAFT,
        RUNNING,
        PAUSED,
        COMPLETED,
        CANCELLED
    }

    /**
     * Experiment group.
     */
    public enum ExperimentGroup {
        CONTROL,
        TREATMENT
    }

    /**
     * Metrics for an experiment arm.
     */
    public static class ExperimentMetrics {
        private final AtomicLong evaluations = new AtomicLong(0);
        private final AtomicLong triggers = new AtomicLong(0);
        private final AtomicLong truePositives = new AtomicLong(0);
        private final AtomicLong falsePositives = new AtomicLong(0);
        private final AtomicLong totalLatencyMicros = new AtomicLong(0);
        private final AtomicLong totalScore = new AtomicLong(0);

        public void recordEvaluation(boolean triggered, int score, long latencyMicros) {
            evaluations.incrementAndGet();
            if (triggered) {
                triggers.incrementAndGet();
                totalScore.addAndGet(score);
            }
            totalLatencyMicros.addAndGet(latencyMicros);
        }

        public void recordOutcome(boolean wasFraud) {
            if (wasFraud) {
                truePositives.incrementAndGet();
            } else {
                falsePositives.incrementAndGet();
            }
        }

        public Map<String, Object> toMap() {
            long evals = evaluations.get();
            long trigs = triggers.get();
            return Map.of(
                    "evaluations", evals,
                    "triggers", trigs,
                    "triggerRate", evals > 0 ? (double) trigs / evals * 100 : 0,
                    "truePositives", truePositives.get(),
                    "falsePositives", falsePositives.get(),
                    "avgScore", trigs > 0 ? (double) totalScore.get() / trigs : 0,
                    "avgLatencyMicros", evals > 0 ? (double) totalLatencyMicros.get() / evals : 0
            );
        }
    }

    /**
     * Assignment result.
     */
    public record ExperimentAssignment(
            Long experimentId,
            ExperimentGroup group,
            Long ruleIdToEvaluate,
            LocalDateTime assignedAt
    ) {}

    /**
     * Creates a new A/B test experiment.
     */
    public Experiment createExperiment(String name, String description,
                                        Long controlRuleId, Long treatmentRuleId,
                                        int treatmentPercentage, String targetSegment) {
        Long id = System.currentTimeMillis(); // Simple ID generation
        
        Experiment experiment = new Experiment(
                id, name, description,
                controlRuleId, treatmentRuleId,
                treatmentPercentage,
                ExperimentStatus.DRAFT,
                null, null,
                new ExperimentMetrics(),
                new ExperimentMetrics(),
                targetSegment
        );
        
        activeExperiments.put(id, experiment);
        log.info("Created A/B test experiment: {} (control={}, treatment={})",
                name, controlRuleId, treatmentRuleId);
        
        return experiment;
    }

    /**
     * Starts an experiment.
     */
    public Experiment startExperiment(Long experimentId) {
        Experiment experiment = activeExperiments.get(experimentId);
        if (experiment == null) {
            throw new IllegalArgumentException("Experiment not found: " + experimentId);
        }
        
        if (experiment.status() != ExperimentStatus.DRAFT && 
            experiment.status() != ExperimentStatus.PAUSED) {
            throw new IllegalStateException("Cannot start experiment in status: " + experiment.status());
        }

        Experiment updated = new Experiment(
                experiment.id(), experiment.name(), experiment.description(),
                experiment.controlRuleId(), experiment.treatmentRuleId(),
                experiment.treatmentPercentage(),
                ExperimentStatus.RUNNING,
                LocalDateTime.now(), null,
                experiment.controlMetrics(), experiment.treatmentMetrics(),
                experiment.targetSegment()
        );
        
        activeExperiments.put(experimentId, updated);
        log.info("Started A/B test experiment: {}", experiment.name());
        
        return updated;
    }

    /**
     * Gets experiment assignment for a transaction.
     * Uses sticky assignment based on PAN hash.
     */
    public Optional<ExperimentAssignment> getAssignment(String panHash, Long ruleId) {
        // Find running experiments for this rule
        Optional<Experiment> experiment = activeExperiments.values().stream()
                .filter(e -> e.status() == ExperimentStatus.RUNNING)
                .filter(e -> e.controlRuleId().equals(ruleId) || e.treatmentRuleId().equals(ruleId))
                .findFirst();
        
        if (experiment.isEmpty()) {
            return Optional.empty();
        }

        Experiment exp = experiment.get();
        String cacheKey = panHash + ":" + exp.id();
        
        // Check for existing assignment (sticky)
        ExperimentAssignment existing = assignmentCache.get(cacheKey);
        if (existing != null) {
            return Optional.of(existing);
        }

        // Assign based on hash
        ExperimentGroup group = assignGroup(panHash, exp.treatmentPercentage());
        Long ruleIdToEvaluate = group == ExperimentGroup.CONTROL 
                ? exp.controlRuleId() 
                : exp.treatmentRuleId();
        
        ExperimentAssignment assignment = new ExperimentAssignment(
                exp.id(), group, ruleIdToEvaluate, LocalDateTime.now()
        );
        
        assignmentCache.put(cacheKey, assignment);
        return Optional.of(assignment);
    }

    /**
     * Records evaluation result for an experiment.
     */
    public void recordEvaluation(Long experimentId, ExperimentGroup group,
                                  boolean triggered, int score, long latencyMicros) {
        Experiment experiment = activeExperiments.get(experimentId);
        if (experiment == null) return;

        ExperimentMetrics metrics = group == ExperimentGroup.CONTROL 
                ? experiment.controlMetrics() 
                : experiment.treatmentMetrics();
        
        metrics.recordEvaluation(triggered, score, latencyMicros);
    }

    /**
     * Records outcome when fraud status is confirmed.
     */
    public void recordOutcome(String panHash, boolean wasFraud) {
        // Find all assignments for this PAN
        assignmentCache.entrySet().stream()
                .filter(e -> e.getKey().startsWith(panHash + ":"))
                .forEach(e -> {
                    ExperimentAssignment assignment = e.getValue();
                    Experiment experiment = activeExperiments.get(assignment.experimentId());
                    if (experiment != null && experiment.status() == ExperimentStatus.RUNNING) {
                        ExperimentMetrics metrics = assignment.group() == ExperimentGroup.CONTROL
                                ? experiment.controlMetrics()
                                : experiment.treatmentMetrics();
                        metrics.recordOutcome(wasFraud);
                    }
                });
    }

    /**
     * Gets experiment results.
     */
    public Map<String, Object> getExperimentResults(Long experimentId) {
        Experiment experiment = activeExperiments.get(experimentId);
        if (experiment == null) {
            return Map.of("error", "Experiment not found");
        }

        Map<String, Object> control = experiment.controlMetrics().toMap();
        Map<String, Object> treatment = experiment.treatmentMetrics().toMap();
        
        // Calculate statistical significance
        double pValue = calculatePValue(experiment);
        boolean isSignificant = pValue < 0.05;
        
        String winner = determineWinner(experiment, isSignificant);

        return Map.of(
                "experimentId", experimentId,
                "name", experiment.name(),
                "status", experiment.status(),
                "control", control,
                "treatment", treatment,
                "pValue", String.format("%.4f", pValue),
                "isSignificant", isSignificant,
                "winner", winner,
                "recommendation", generateRecommendation(experiment, winner, isSignificant)
        );
    }

    /**
     * Completes an experiment.
     */
    public Experiment completeExperiment(Long experimentId) {
        Experiment experiment = activeExperiments.get(experimentId);
        if (experiment == null) {
            throw new IllegalArgumentException("Experiment not found: " + experimentId);
        }

        Experiment completed = experiment.withStatus(ExperimentStatus.COMPLETED);
        activeExperiments.put(experimentId, completed);
        
        // Cleanup assignments
        assignmentCache.entrySet().removeIf(e -> e.getKey().endsWith(":" + experimentId));
        
        log.info("Completed A/B test experiment: {}", experiment.name());
        return completed;
    }

    /**
     * Lists all experiments.
     */
    public List<Experiment> listExperiments() {
        return List.copyOf(activeExperiments.values());
    }

    /**
     * Lists running experiments.
     */
    public List<Experiment> listRunningExperiments() {
        return activeExperiments.values().stream()
                .filter(e -> e.status() == ExperimentStatus.RUNNING)
                .collect(Collectors.toList());
    }

    // ========== Internal Methods ==========

    /**
     * Assigns group based on deterministic hash of PAN.
     * This ensures same PAN always gets same group.
     */
    private ExperimentGroup assignGroup(String panHash, int treatmentPercentage) {
        int bucket = Math.abs(hash(panHash)) % 100;
        return bucket < treatmentPercentage ? ExperimentGroup.TREATMENT : ExperimentGroup.CONTROL;
    }

    private int hash(String value) {
        try {
            MessageDigest md = MessageDigest.getInstance("MD5");
            byte[] digest = md.digest(value.getBytes());
            return ((digest[0] & 0xFF) << 24) | ((digest[1] & 0xFF) << 16) 
                 | ((digest[2] & 0xFF) << 8) | (digest[3] & 0xFF);
        } catch (NoSuchAlgorithmException e) {
            return value.hashCode();
        }
    }

    /**
     * Calculates p-value using chi-squared test for trigger rates.
     */
    private double calculatePValue(Experiment experiment) {
        long controlN = experiment.controlMetrics().evaluations.get();
        long controlTriggers = experiment.controlMetrics().triggers.get();
        long treatmentN = experiment.treatmentMetrics().evaluations.get();
        long treatmentTriggers = experiment.treatmentMetrics().triggers.get();

        if (controlN < 100 || treatmentN < 100) {
            return 1.0; // Not enough data
        }

        // Simplified chi-squared test
        double controlRate = (double) controlTriggers / controlN;
        double treatmentRate = (double) treatmentTriggers / treatmentN;
        double pooledRate = (double) (controlTriggers + treatmentTriggers) / (controlN + treatmentN);

        if (pooledRate == 0 || pooledRate == 1) {
            return 1.0;
        }

        double se = Math.sqrt(pooledRate * (1 - pooledRate) * (1.0/controlN + 1.0/treatmentN));
        if (se == 0) return 1.0;

        double z = Math.abs(controlRate - treatmentRate) / se;
        
        // Approximate p-value from z-score (two-tailed)
        return 2 * (1 - normalCDF(z));
    }

    private double normalCDF(double z) {
        // Approximation of standard normal CDF
        double t = 1.0 / (1.0 + 0.2316419 * Math.abs(z));
        double d = 0.3989423 * Math.exp(-z * z / 2);
        double p = d * t * (0.3193815 + t * (-0.3565638 + t * (1.781478 + t * (-1.821256 + t * 1.330274))));
        return z > 0 ? 1 - p : p;
    }

    private String determineWinner(Experiment experiment, boolean isSignificant) {
        if (!isSignificant) {
            return "NONE - Not statistically significant";
        }

        Map<String, Object> control = experiment.controlMetrics().toMap();
        Map<String, Object> treatment = experiment.treatmentMetrics().toMap();
        
        double controlFP = (double) (Long) control.get("falsePositives");
        double treatmentFP = (double) (Long) treatment.get("falsePositives");
        double controlTP = (double) (Long) control.get("truePositives");
        double treatmentTP = (double) (Long) treatment.get("truePositives");

        // Prefer lower false positives, then higher true positives
        if (treatmentFP < controlFP * 0.9) {
            return "TREATMENT - Lower false positive rate";
        }
        if (treatmentTP > controlTP * 1.1) {
            return "TREATMENT - Higher catch rate";
        }
        if (controlFP < treatmentFP * 0.9) {
            return "CONTROL - Lower false positive rate";
        }
        
        return "NONE - No clear winner";
    }

    private String generateRecommendation(Experiment experiment, String winner, boolean isSignificant) {
        long totalEvals = experiment.controlMetrics().evaluations.get() 
                        + experiment.treatmentMetrics().evaluations.get();
        
        if (totalEvals < 10_000) {
            return "Continue experiment - need at least 10,000 evaluations for reliable results";
        }
        
        if (!isSignificant) {
            if (totalEvals < 50_000) {
                return "Continue experiment - results not yet statistically significant";
            }
            return "Rules are equivalent - consider other factors like latency or maintainability";
        }
        
        if (winner.startsWith("TREATMENT")) {
            return "Promote treatment rule to production - " + winner;
        }
        
        if (winner.startsWith("CONTROL")) {
            return "Keep control rule - treatment did not improve metrics";
        }
        
        return "Manual review recommended";
    }
}
