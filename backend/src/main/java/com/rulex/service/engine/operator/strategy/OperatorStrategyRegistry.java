package com.rulex.service.engine.operator.strategy;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.exception.UnsupportedOperatorException;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import jakarta.annotation.PostConstruct;
import java.util.Comparator;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Registry central para todas as OperatorStrategy.
 * 
 * <p>Auto-descobre todas as implementações de OperatorStrategy via injeção de dependência
 * e mapeia cada operador para sua strategy correspondente.
 * 
 * <p>Benefícios:
 * <ul>
 *   <li>Descoberta automática - novas strategies são registradas automaticamente</li>
 *   <li>Lookup O(1) - mapa de operador para strategy</li>
 *   <li>Validação na inicialização - detecta operadores duplicados ou não mapeados</li>
 *   <li>Métricas - pode rastrear uso de cada strategy</li>
 * </ul>
 * 
 * @author Refactoring - RULEX Team
 * @since 2.0.0
 */
@Component
@Slf4j
public class OperatorStrategyRegistry {

    private final List<OperatorStrategy> strategies;
    private final Map<ConditionOperator, OperatorStrategy> operatorToStrategy;
    private final Map<String, List<ConditionOperator>> categoryToOperators;

    public OperatorStrategyRegistry(List<OperatorStrategy> strategies) {
        this.strategies = strategies.stream()
            .sorted(Comparator.comparingInt(OperatorStrategy::priority))
            .toList();
        this.operatorToStrategy = new EnumMap<>(ConditionOperator.class);
        this.categoryToOperators = new java.util.HashMap<>();
    }

    @PostConstruct
    public void initialize() {
        log.info("Initializing OperatorStrategyRegistry with {} strategies", strategies.size());
        
        for (OperatorStrategy strategy : strategies) {
            String category = strategy.category();
            Set<ConditionOperator> operators = strategy.supportedOperators();
            
            log.debug("Registering strategy {} with {} operators: {}", 
                strategy.getClass().getSimpleName(), operators.size(), operators);

            for (ConditionOperator op : operators) {
                OperatorStrategy existing = operatorToStrategy.put(op, strategy);
                if (existing != null) {
                    log.warn("Operator {} was already registered by {}. Overwriting with {}",
                        op, existing.getClass().getSimpleName(), strategy.getClass().getSimpleName());
                }
            }

            categoryToOperators.computeIfAbsent(category, k -> new java.util.ArrayList<>())
                .addAll(operators);
        }

        // Log summary
        int totalOperators = operatorToStrategy.size();
        int totalEnumValues = ConditionOperator.values().length;
        int unmapped = totalEnumValues - totalOperators;
        
        log.info("OperatorStrategyRegistry initialized: {} operators mapped, {} unmapped out of {} total",
            totalOperators, unmapped, totalEnumValues);

        if (unmapped > 0) {
            Set<ConditionOperator> mappedOps = operatorToStrategy.keySet();
            List<ConditionOperator> unmappedOps = java.util.Arrays.stream(ConditionOperator.values())
                .filter(op -> !mappedOps.contains(op))
                .toList();
            log.warn("Unmapped operators (will use fallback): {}", unmappedOps);
        }
    }

    /**
     * Avalia uma condição usando a strategy apropriada.
     * 
     * @param fieldValue valor do campo
     * @param condition condição a avaliar
     * @param context contexto de avaliação
     * @return resultado da avaliação
     * @throws UnsupportedOperatorException se operador não tem strategy
     */
    public boolean evaluate(Object fieldValue, RuleCondition condition, EvaluationContext context) {
        ConditionOperator operator = condition.getOperator();
        OperatorStrategy strategy = operatorToStrategy.get(operator);
        
        if (strategy == null) {
            throw new UnsupportedOperatorException(operator, "NOT_IMPLEMENTED",
                "Operador " + operator + " não possui strategy implementada");
        }

        return strategy.evaluate(fieldValue, condition, context);
    }

    /**
     * Verifica se um operador tem strategy registrada.
     */
    public boolean hasStrategy(ConditionOperator operator) {
        return operatorToStrategy.containsKey(operator);
    }

    /**
     * Retorna a strategy para um operador específico.
     */
    public OperatorStrategy getStrategy(ConditionOperator operator) {
        return operatorToStrategy.get(operator);
    }

    /**
     * Retorna todos os operadores suportados.
     */
    public Set<ConditionOperator> getSupportedOperators() {
        return Set.copyOf(operatorToStrategy.keySet());
    }

    /**
     * Retorna operadores por categoria.
     */
    public Map<String, List<ConditionOperator>> getOperatorsByCategory() {
        return Map.copyOf(categoryToOperators);
    }

    /**
     * Retorna estatísticas do registry.
     */
    public RegistryStats getStats() {
        int totalStrategies = strategies.size();
        int totalMapped = operatorToStrategy.size();
        int totalEnum = ConditionOperator.values().length;
        
        Map<String, Integer> byCategory = categoryToOperators.entrySet().stream()
            .collect(Collectors.toMap(Map.Entry::getKey, e -> e.getValue().size()));

        return new RegistryStats(totalStrategies, totalMapped, totalEnum, byCategory);
    }

    /**
     * Estatísticas do registry.
     */
    public record RegistryStats(
        int totalStrategies,
        int totalMappedOperators,
        int totalEnumOperators,
        Map<String, Integer> operatorsByCategory
    ) {
        public int unmappedOperators() {
            return totalEnumOperators - totalMappedOperators;
        }

        public double coveragePercent() {
            return totalEnumOperators > 0 
                ? (totalMappedOperators * 100.0 / totalEnumOperators) 
                : 0;
        }
    }
}
