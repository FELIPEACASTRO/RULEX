package com.rulex.service.engine.operator.strategy;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Strategy para operadores de nulo e booleano.
 * 
 * <p>Operadores suportados:
 * <ul>
 *   <li>IS_NULL - valor é nulo</li>
 *   <li>NOT_NULL - valor não é nulo</li>
 *   <li>IS_TRUE - valor é verdadeiro (truthy)</li>
 *   <li>IS_FALSE - valor é falso (falsy)</li>
 * </ul>
 * 
 * @author Refactoring - RULEX Team
 * @since 2.0.0
 */
@Component
@Slf4j
public class NullBooleanOperatorStrategy implements OperatorStrategy {

    private static final Set<ConditionOperator> SUPPORTED = Set.of(
        ConditionOperator.IS_NULL,
        ConditionOperator.NOT_NULL,
        ConditionOperator.IS_TRUE,
        ConditionOperator.IS_FALSE
    );

    @Override
    public Set<ConditionOperator> supportedOperators() {
        return SUPPORTED;
    }

    @Override
    public String category() {
        return "NULL_BOOLEAN";
    }

    @Override
    public int priority() {
        return 5; // Máxima prioridade - operadores mais simples
    }

    @Override
    public boolean evaluate(Object fieldValue, RuleCondition condition, EvaluationContext context) {
        ConditionOperator operator = condition.getOperator();

        return switch (operator) {
            case IS_NULL -> fieldValue == null;
            case NOT_NULL -> fieldValue != null;
            case IS_TRUE -> isTruthy(fieldValue);
            case IS_FALSE -> fieldValue != null && !isTruthy(fieldValue);
            default -> {
                log.warn("Unexpected operator in NullBooleanOperatorStrategy: {}", operator);
                yield false;
            }
        };
    }

    /**
     * Verifica se valor é "truthy" (verdadeiro em sentido amplo).
     * 
     * <p>Valores considerados truthy:
     * <ul>
     *   <li>Boolean.TRUE</li>
     *   <li>String "true", "yes", "1", "on" (case insensitive)</li>
     *   <li>Número diferente de zero</li>
     * </ul>
     */
    private boolean isTruthy(Object value) {
        if (value == null) {
            return false;
        }

        if (value instanceof Boolean b) {
            return b;
        }

        if (value instanceof Number n) {
            return n.doubleValue() != 0;
        }

        if (value instanceof String s) {
            String lower = s.toLowerCase().trim();
            return "true".equals(lower) 
                || "yes".equals(lower) 
                || "1".equals(lower) 
                || "on".equals(lower)
                || "sim".equals(lower);
        }

        // Para outros tipos, considera truthy se não for nulo
        return true;
    }
}
