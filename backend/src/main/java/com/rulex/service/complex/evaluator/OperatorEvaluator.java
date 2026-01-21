package com.rulex.service.complex.evaluator;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.Set;

/**
 * Interface para avaliadores de operadores.
 * Cada implementação é responsável por um conjunto de operadores relacionados.
 * 
 * <p>Padrão Strategy: permite adicionar novos operadores sem modificar o ComplexRuleEvaluator.
 * 
 * @see OperatorEvaluatorRegistry
 */
public interface OperatorEvaluator {
    
    /**
     * Retorna o conjunto de operadores suportados por este avaliador.
     * @return Set imutável de operadores suportados
     */
    Set<ConditionOperator> getSupportedOperators();
    
    /**
     * Verifica se este avaliador suporta o operador dado.
     * @param operator O operador a verificar
     * @return true se suportado, false caso contrário
     */
    default boolean supports(ConditionOperator operator) {
        return getSupportedOperators().contains(operator);
    }
    
    /**
     * Avalia uma condição.
     * 
     * @param condition A condição a ser avaliada
     * @param context O contexto de avaliação contendo payload e metadados
     * @return true se a condição foi satisfeita, false caso contrário
     * @throws com.rulex.exception.UnsupportedOperatorException se o operador não está implementado
     * @throws com.rulex.exception.RuleEvaluationException se ocorrer erro durante avaliação
     */
    boolean evaluate(RuleCondition condition, EvaluationContext context);
    
    /**
     * Retorna o nome da categoria deste avaliador.
     * @return Nome da categoria (ex: "BASIC_COMPARISON", "VELOCITY", etc.)
     */
    default String getCategory() {
        return getClass().getSimpleName().replace("Evaluator", "").toUpperCase();
    }
}
