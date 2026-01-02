package com.rulex.service;

import com.rulex.entity.RuleConfiguration;
import com.rulex.entity.RuleConfiguration.RuleType;
import com.rulex.repository.RuleConfigurationRepository;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * Serviço de ordenação otimizada de regras para máxima eficiência de execução.
 * 
 * Baseado em pesquisa acadêmica (PESQUISA_REGRAS_DURAS_EFICIENTES.md):
 * - Ordenação inteligente pode reduzir latência em até 60%
 * - Short-circuit evaluation com regras mais eficientes primeiro
 * - Balanceamento entre custo computacional e taxa de hit
 * 
 * Algoritmo de ordenação:
 * 1. Score = (hitRate / computationCost) * weight
 * 2. Regras com maior score executam primeiro
 * 3. Permite short-circuit quando classificação final é determinada
 * 4. Recalcula periodicamente baseado em métricas reais
 * 
 * Estratégias implementadas:
 * - Cost-based ordering: regras baratas primeiro
 * - Hit-rate ordering: regras com mais hits primeiro
 * - Hybrid ordering: balanceia custo e hit rate
 * - Classification-based: FRAUD/SUSPICIOUS primeiro para short-circuit
 * 
 * @see docs/PESQUISA_REGRAS_DURAS_EFICIENTES.md
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class RuleOrderingService {

    private final RuleConfigurationRepository ruleConfigRepository;
    private final RuleMetricsService ruleMetricsService;

    // Cache de ordenação otimizada (atualizado periodicamente)
    private volatile List<RuleConfiguration> optimizedOrderCache = new ArrayList<>();
    private volatile LocalDateTime lastOptimizationTime;

    // Cache de custos computacionais estimados por tipo de regra
    private static final Map<RuleType, Double> BASE_COMPUTATION_COSTS = Map.of(
            RuleType.SECURITY, 1.0,    // Verificações simples de campo
            RuleType.CONTEXT, 2.0,      // Lookup de contexto
            RuleType.VELOCITY, 5.0,     // Consulta Redis/BD
            RuleType.ANOMALY, 10.0      // Cálculos estatísticos
    );

    // Métricas em tempo real por regra
    private final Map<String, RuleExecutionMetrics> ruleMetrics = new ConcurrentHashMap<>();

    /**
     * Retorna lista de regras na ordem otimizada para execução.
     * Usa cache atualizado periodicamente.
     */
    public List<RuleConfiguration> getOptimizedRuleOrder() {
        if (optimizedOrderCache.isEmpty() || shouldRecompute()) {
            synchronized (this) {
                if (optimizedOrderCache.isEmpty() || shouldRecompute()) {
                    optimizedOrderCache = computeOptimalOrder();
                    lastOptimizationTime = LocalDateTime.now();
                    log.info("RuleOrdering: Cache de ordenação atualizado com {} regras", 
                            optimizedOrderCache.size());
                }
            }
        }
        return optimizedOrderCache;
    }

    /**
     * Retorna ordenação específica por estratégia.
     */
    public List<RuleConfiguration> getOrderByStrategy(OrderingStrategy strategy) {
        List<RuleConfiguration> rules = ruleConfigRepository.findByEnabled(true);
        
        return switch (strategy) {
            case COST_BASED -> orderByCost(rules);
            case HIT_RATE -> orderByHitRate(rules);
            case HYBRID -> computeHybridOrder(rules);
            case CLASSIFICATION_FIRST -> orderByClassification(rules);
            case WEIGHT_BASED -> orderByWeight(rules);
        };
    }

    /**
     * Registra métricas de execução para uma regra (chamado após cada avaliação).
     */
    public void recordExecution(String ruleName, long executionTimeNanos, boolean triggered) {
        ruleMetrics.compute(ruleName, (key, existing) -> {
            RuleExecutionMetrics metrics = existing != null ? existing : new RuleExecutionMetrics();
            metrics.recordExecution(executionTimeNanos, triggered);
            return metrics;
        });
    }

    /**
     * Calcula a ordem ótima usando algoritmo híbrido.
     */
    private List<RuleConfiguration> computeOptimalOrder() {
        List<RuleConfiguration> rules = ruleConfigRepository.findByEnabled(true);
        
        if (rules.isEmpty()) {
            return rules;
        }

        // Calcular score para cada regra
        List<ScoredRule> scoredRules = rules.stream()
                .map(rule -> {
                    double score = calculateOptimalScore(rule);
                    return new ScoredRule(rule, score);
                })
                .sorted((a, b) -> Double.compare(b.score, a.score)) // Maior score primeiro
                .collect(Collectors.toList());

        // Log do ranking
        if (log.isDebugEnabled()) {
            log.debug("RuleOrdering: Ranking otimizado:");
            for (int i = 0; i < Math.min(10, scoredRules.size()); i++) {
                ScoredRule sr = scoredRules.get(i);
                log.debug("  {}. {} (score: {:.4f})", i + 1, sr.rule.getRuleName(), sr.score);
            }
        }

        return scoredRules.stream()
                .map(sr -> sr.rule)
                .collect(Collectors.toList());
    }

    /**
     * Calcula score ótimo para uma regra usando fórmula híbrida:
     * Score = (hitRate * weight) / (computationCost * (1 + avgLatencyMs/10))
     * 
     * Regras que detectam mais fraudes com menor custo têm maior score.
     */
    private double calculateOptimalScore(RuleConfiguration rule) {
        // Obter métricas de execução
        RuleExecutionMetrics execMetrics = ruleMetrics.get(rule.getRuleName());
        
        double hitRate = 0.01; // Default 1%
        double avgLatencyMs = 1.0; // Default 1ms
        
        if (execMetrics != null && execMetrics.totalExecutions > 100) {
            hitRate = execMetrics.getHitRate();
            avgLatencyMs = execMetrics.getAverageLatencyMs();
        }

        // Custo base por tipo de regra
        double baseCost = BASE_COMPUTATION_COSTS.getOrDefault(rule.getRuleType(), 5.0);
        
        // Peso configurado (normalizado 0-1)
        double weight = rule.getWeight() / 100.0;
        
        // Boost para regras que classificam como FRAUD (short-circuit value)
        double classificationBoost = switch (rule.getClassification()) {
            case FRAUD -> 2.0;
            case SUSPICIOUS -> 1.5;
            default -> 1.0;
        };
        
        // Fórmula de score
        double effectiveCost = baseCost * (1 + avgLatencyMs / 10);
        double score = (hitRate * weight * classificationBoost) / effectiveCost;
        
        // Penalidade para regras sem métricas suficientes
        if (execMetrics == null || execMetrics.totalExecutions < 100) {
            score *= 0.5;
        }
        
        return score;
    }

    /**
     * Ordenação por custo computacional (mais baratas primeiro).
     */
    private List<RuleConfiguration> orderByCost(List<RuleConfiguration> rules) {
        return rules.stream()
                .sorted(Comparator.comparingDouble(r -> 
                        BASE_COMPUTATION_COSTS.getOrDefault(r.getRuleType(), 5.0)))
                .collect(Collectors.toList());
    }

    /**
     * Ordenação por taxa de hit (maior hit rate primeiro).
     */
    private List<RuleConfiguration> orderByHitRate(List<RuleConfiguration> rules) {
        return rules.stream()
                .sorted((a, b) -> {
                    double hitA = getHitRate(a.getRuleName());
                    double hitB = getHitRate(b.getRuleName());
                    return Double.compare(hitB, hitA);
                })
                .collect(Collectors.toList());
    }

    /**
     * Ordenação híbrida (cost/hit rate balance).
     */
    private List<RuleConfiguration> computeHybridOrder(List<RuleConfiguration> rules) {
        return rules.stream()
                .sorted((a, b) -> {
                    double scoreA = getHitRate(a.getRuleName()) / 
                            BASE_COMPUTATION_COSTS.getOrDefault(a.getRuleType(), 5.0);
                    double scoreB = getHitRate(b.getRuleName()) / 
                            BASE_COMPUTATION_COSTS.getOrDefault(b.getRuleType(), 5.0);
                    return Double.compare(scoreB, scoreA);
                })
                .collect(Collectors.toList());
    }

    /**
     * Ordenação por classificação (FRAUD > SUSPICIOUS > APPROVED).
     * Permite short-circuit mais rápido.
     */
    private List<RuleConfiguration> orderByClassification(List<RuleConfiguration> rules) {
        Map<Object, Integer> classOrder = Map.of(
                com.rulex.entity.TransactionDecision.TransactionClassification.FRAUD, 0,
                com.rulex.entity.TransactionDecision.TransactionClassification.SUSPICIOUS, 1,
                com.rulex.entity.TransactionDecision.TransactionClassification.APPROVED, 2
        );
        
        return rules.stream()
                .sorted(Comparator.comparingInt(r -> 
                        classOrder.getOrDefault(r.getClassification(), 3)))
                .collect(Collectors.toList());
    }

    /**
     * Ordenação por peso (maior peso primeiro).
     */
    private List<RuleConfiguration> orderByWeight(List<RuleConfiguration> rules) {
        return rules.stream()
                .sorted(Comparator.comparingInt(RuleConfiguration::getWeight).reversed())
                .collect(Collectors.toList());
    }

    /**
     * Verifica se deve recalcular a ordenação (a cada 5 minutos).
     */
    private boolean shouldRecompute() {
        if (lastOptimizationTime == null) return true;
        return lastOptimizationTime.plusMinutes(5).isBefore(LocalDateTime.now());
    }

    private double getHitRate(String ruleName) {
        RuleExecutionMetrics metrics = ruleMetrics.get(ruleName);
        if (metrics != null && metrics.totalExecutions > 0) {
            return metrics.getHitRate();
        }
        return 0.01; // Default 1%
    }

    /**
     * Força recálculo da ordenação otimizada.
     */
    public void forceRecompute() {
        synchronized (this) {
            optimizedOrderCache = computeOptimalOrder();
            lastOptimizationTime = LocalDateTime.now();
            log.info("RuleOrdering: Ordenação recalculada forçadamente");
        }
    }

    /**
     * Retorna relatório de eficiência da ordenação atual.
     */
    public OrderingEfficiencyReport getEfficiencyReport() {
        List<RuleConfiguration> currentOrder = getOptimizedRuleOrder();
        
        if (currentOrder.isEmpty()) {
            return OrderingEfficiencyReport.builder()
                    .timestamp(LocalDateTime.now())
                    .totalRules(0)
                    .estimatedLatencyReductionPercent(BigDecimal.ZERO)
                    .topPerformers(Collections.emptyList())
                    .build();
        }

        // Calcular métricas de eficiência
        double totalCost = 0;
        double weightedHitPosition = 0;
        List<RulePerformanceInfo> topPerformers = new ArrayList<>();

        for (int i = 0; i < currentOrder.size(); i++) {
            RuleConfiguration rule = currentOrder.get(i);
            RuleExecutionMetrics metrics = ruleMetrics.get(rule.getRuleName());
            
            double cost = BASE_COMPUTATION_COSTS.getOrDefault(rule.getRuleType(), 5.0);
            totalCost += cost;
            
            if (metrics != null && metrics.totalExecutions > 0) {
                weightedHitPosition += (i + 1) * metrics.getHitRate();
                
                if (topPerformers.size() < 5) {
                    topPerformers.add(RulePerformanceInfo.builder()
                            .ruleName(rule.getRuleName())
                            .position(i + 1)
                            .hitRate(BigDecimal.valueOf(metrics.getHitRate() * 100)
                                    .setScale(2, RoundingMode.HALF_UP))
                            .avgLatencyMs(BigDecimal.valueOf(metrics.getAverageLatencyMs())
                                    .setScale(2, RoundingMode.HALF_UP))
                            .score(BigDecimal.valueOf(calculateOptimalScore(rule))
                                    .setScale(4, RoundingMode.HALF_UP))
                            .build());
                }
            }
        }

        // Estimar redução de latência comparado com ordenação aleatória
        BigDecimal latencyReduction = calculateLatencyReduction(currentOrder);

        return OrderingEfficiencyReport.builder()
                .timestamp(LocalDateTime.now())
                .totalRules(currentOrder.size())
                .totalComputationCost(BigDecimal.valueOf(totalCost).setScale(2, RoundingMode.HALF_UP))
                .weightedHitPosition(BigDecimal.valueOf(weightedHitPosition).setScale(4, RoundingMode.HALF_UP))
                .estimatedLatencyReductionPercent(latencyReduction)
                .lastOptimizationTime(lastOptimizationTime)
                .topPerformers(topPerformers)
                .build();
    }

    private BigDecimal calculateLatencyReduction(List<RuleConfiguration> optimizedOrder) {
        if (optimizedOrder.isEmpty()) return BigDecimal.ZERO;
        
        // Simular latência média com ordenação otimizada vs aleatória
        double optimizedLatency = 0;
        double randomLatency = 0;
        
        List<RuleConfiguration> randomOrder = new ArrayList<>(optimizedOrder);
        Collections.shuffle(randomOrder, new Random(42)); // Seed fixo para consistência
        
        for (int i = 0; i < optimizedOrder.size(); i++) {
            // Probabilidade de parar em cada regra (short-circuit)
            double stopProbOptimized = getHitRate(optimizedOrder.get(i).getRuleName());
            double stopProbRandom = getHitRate(randomOrder.get(i).getRuleName());
            
            double costOptimized = BASE_COMPUTATION_COSTS.getOrDefault(
                    optimizedOrder.get(i).getRuleType(), 5.0);
            double costRandom = BASE_COMPUTATION_COSTS.getOrDefault(
                    randomOrder.get(i).getRuleType(), 5.0);
            
            optimizedLatency += costOptimized * (1 - stopProbOptimized);
            randomLatency += costRandom * (1 - stopProbRandom);
        }
        
        if (randomLatency <= 0) return BigDecimal.ZERO;
        
        double reduction = ((randomLatency - optimizedLatency) / randomLatency) * 100;
        return BigDecimal.valueOf(Math.max(0, reduction)).setScale(1, RoundingMode.HALF_UP);
    }

    /**
     * Job agendado para recalcular ordenação (a cada 5 minutos).
     */
    @Scheduled(fixedRate = 300000) // 5 minutos
    public void scheduledRecompute() {
        forceRecompute();
    }

    // ============ Classes auxiliares ============

    private record ScoredRule(RuleConfiguration rule, double score) {}

    /**
     * Métricas de execução em tempo real para uma regra.
     */
    private static class RuleExecutionMetrics {
        long totalExecutions = 0;
        long totalHits = 0;
        long totalLatencyNanos = 0;

        synchronized void recordExecution(long latencyNanos, boolean triggered) {
            totalExecutions++;
            totalLatencyNanos += latencyNanos;
            if (triggered) {
                totalHits++;
            }
        }

        double getHitRate() {
            return totalExecutions > 0 ? (double) totalHits / totalExecutions : 0;
        }

        double getAverageLatencyMs() {
            return totalExecutions > 0 ? (totalLatencyNanos / totalExecutions) / 1_000_000.0 : 0;
        }
    }

    public enum OrderingStrategy {
        COST_BASED,
        HIT_RATE,
        HYBRID,
        CLASSIFICATION_FIRST,
        WEIGHT_BASED
    }

    @Data
    @Builder
    public static class OrderingEfficiencyReport {
        private LocalDateTime timestamp;
        private int totalRules;
        private BigDecimal totalComputationCost;
        private BigDecimal weightedHitPosition;
        private BigDecimal estimatedLatencyReductionPercent;
        private LocalDateTime lastOptimizationTime;
        private List<RulePerformanceInfo> topPerformers;
    }

    @Data
    @Builder
    public static class RulePerformanceInfo {
        private String ruleName;
        private int position;
        private BigDecimal hitRate;
        private BigDecimal avgLatencyMs;
        private BigDecimal score;
    }
}
