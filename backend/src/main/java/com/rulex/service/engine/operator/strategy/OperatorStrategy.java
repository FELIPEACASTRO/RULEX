package com.rulex.service.engine.operator.strategy;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.Set;

/**
 * Interface Strategy para avaliação de operadores.
 * 
 * <p>Cada implementação é responsável por um conjunto específico de operadores,
 * seguindo o padrão Strategy para eliminar o switch gigante no ComplexRuleEvaluator.
 * 
 * <p>Benefícios:
 * <ul>
 *   <li>Open/Closed Principle - adicionar operador = criar nova classe</li>
 *   <li>Single Responsibility - cada strategy cuida de operadores relacionados</li>
 *   <li>Testabilidade - cada strategy pode ser testada isoladamente</li>
 *   <li>Manutenibilidade - código organizado por domínio</li>
 * </ul>
 * 
 * <p>Exemplo de uso:
 * <pre>
 * {@code
 * @Component
 * public class ComparisonOperatorStrategy implements OperatorStrategy {
 *     @Override
 *     public Set<ConditionOperator> supportedOperators() {
 *         return Set.of(EQ, NEQ, GT, GTE, LT, LTE);
 *     }
 *     
 *     @Override
 *     public boolean evaluate(Object fieldValue, RuleCondition condition, EvaluationContext ctx) {
 *         // implementação
 *     }
 * }
 * }
 * </pre>
 * 
 * @author Refactoring - RULEX Team
 * @since 2.0.0
 */
public interface OperatorStrategy {

    /**
     * Retorna o conjunto de operadores suportados por esta strategy.
     * 
     * @return conjunto imutável de operadores
     */
    Set<ConditionOperator> supportedOperators();

    /**
     * Avalia uma condição usando o operador especificado.
     * 
     * @param fieldValue valor do campo a ser avaliado
     * @param condition condição completa (contém operador, valor esperado, etc.)
     * @param context contexto de avaliação (transação, request, dados derivados)
     * @return true se a condição foi satisfeita
     */
    boolean evaluate(Object fieldValue, RuleCondition condition, EvaluationContext context);

    /**
     * Retorna a categoria desta strategy (para documentação e logging).
     * 
     * @return nome da categoria (ex: "COMPARISON", "STRING", "VELOCITY")
     */
    default String category() {
        return this.getClass().getSimpleName().replace("OperatorStrategy", "").toUpperCase();
    }

    /**
     * Indica se esta strategy requer serviços externos (DB, Redis, Neo4j).
     * 
     * @return true se requer serviços externos
     */
    default boolean requiresExternalServices() {
        return false;
    }

    /**
     * Retorna a prioridade de execução (menor = executa primeiro).
     * Strategies sem dependências externas devem ter prioridade menor.
     * 
     * @return prioridade (default 100)
     */
    default int priority() {
        return requiresExternalServices() ? 200 : 100;
    }
}
