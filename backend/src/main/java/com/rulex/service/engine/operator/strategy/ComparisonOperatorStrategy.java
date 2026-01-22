package com.rulex.service.engine.operator.strategy;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.math.BigDecimal;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Strategy para operadores de comparação básica.
 *
 * <p>Operadores suportados:
 * <ul>
 *   <li>EQ - igual</li>
 *   <li>NEQ - diferente</li>
 *   <li>GT - maior que</li>
 *   <li>GTE - maior ou igual</li>
 *   <li>LT - menor que</li>
 *   <li>LTE - menor ou igual</li>
 *   <li>BETWEEN - entre dois valores</li>
 *   <li>NOT_BETWEEN - fora do intervalo</li>
 * </ul>
 *
 * @author Refactoring - RULEX Team
 * @since 2.0.0
 */
@Component
@Slf4j
public class ComparisonOperatorStrategy implements OperatorStrategy {

    private static final Set<ConditionOperator> SUPPORTED = Set.of(
        ConditionOperator.EQ,
        ConditionOperator.NEQ,
        ConditionOperator.GT,
        ConditionOperator.GTE,
        ConditionOperator.LT,
        ConditionOperator.LTE,
        ConditionOperator.BETWEEN,
        ConditionOperator.NOT_BETWEEN
    );

    @Override
    public Set<ConditionOperator> supportedOperators() {
        return SUPPORTED;
    }

    @Override
    public String category() {
        return "COMPARISON";
    }

    @Override
    public int priority() {
        return 10; // Alta prioridade - operadores básicos e rápidos
    }

    @Override
    public boolean evaluate(Object fieldValue, RuleCondition condition, EvaluationContext context) {
        ConditionOperator operator = condition.getOperator();
        String expectedValue = condition.getValueSingle();
        Boolean caseSensitive = condition.getCaseSensitive();

        return switch (operator) {
            case EQ -> evaluateEquals(fieldValue, expectedValue, caseSensitive);
            case NEQ -> !evaluateEquals(fieldValue, expectedValue, caseSensitive);
            case GT -> compareValues(fieldValue, expectedValue) > 0;
            case GTE -> compareValues(fieldValue, expectedValue) >= 0;
            case LT -> compareValues(fieldValue, expectedValue) < 0;
            case LTE -> compareValues(fieldValue, expectedValue) <= 0;
            case BETWEEN -> evaluateBetween(fieldValue, condition);
            case NOT_BETWEEN -> !evaluateBetween(fieldValue, condition);
            default -> {
                log.warn("Unexpected operator in ComparisonOperatorStrategy: {}", operator);
                yield false;
            }
        };
    }

    /**
     * Avalia igualdade entre valores.
     */
    private boolean evaluateEquals(Object fieldValue, String expected, Boolean caseSensitive) {
        if (fieldValue == null && expected == null) {
            return true;
        }
        if (fieldValue == null || expected == null) {
            return false;
        }

        // Comparação numérica
        if (fieldValue instanceof Number || fieldValue instanceof BigDecimal) {
            try {
                BigDecimal fieldBd = toBigDecimal(fieldValue);
                BigDecimal expectedBd = new BigDecimal(expected);
                return fieldBd != null && fieldBd.compareTo(expectedBd) == 0;
            } catch (NumberFormatException e) {
                // Não é número, comparar como string
            }
        }

        // Comparação de string
        String fieldStr = String.valueOf(fieldValue);
        boolean isCaseSensitive = caseSensitive != null && caseSensitive;

        return isCaseSensitive
            ? fieldStr.equals(expected)
            : fieldStr.equalsIgnoreCase(expected);
    }

    /**
     * Compara dois valores numericamente ou lexicograficamente.
     */
    private int compareValues(Object fieldValue, String expected) {
        if (fieldValue == null) {
            return expected == null ? 0 : -1;
        }
        if (expected == null) {
            return 1;
        }

        // Tentar comparação numérica
        try {
            BigDecimal fieldBd = toBigDecimal(fieldValue);
            BigDecimal expectedBd = new BigDecimal(expected);
            if (fieldBd != null) {
                return fieldBd.compareTo(expectedBd);
            }
        } catch (NumberFormatException e) {
            // Não é número
        }

        // Comparação lexicográfica
        return String.valueOf(fieldValue).compareTo(expected);
    }

    /**
     * Avalia se valor está entre min e max.
     */
    private boolean evaluateBetween(Object fieldValue, RuleCondition condition) {
        if (fieldValue == null) {
            return false;
        }

        // Usar valueMin e valueMax se disponíveis
        String minStr = condition.getValueMin();
        String maxStr = condition.getValueMax();

        // Fallback para valueSingle no formato "min,max"
        if ((minStr == null || maxStr == null) && condition.getValueSingle() != null) {
            String valueSingle = condition.getValueSingle();
            if (valueSingle.contains(",")) {
                String[] parts = valueSingle.split(",", 2);
                minStr = parts[0].trim();
                maxStr = parts.length > 1 ? parts[1].trim() : null;
            }
        }

        if (minStr == null || maxStr == null) {
            log.warn("BETWEEN operator requires min and max values");
            return false;
        }

        try {
            BigDecimal value = toBigDecimal(fieldValue);
            BigDecimal min = new BigDecimal(minStr);
            BigDecimal max = new BigDecimal(maxStr);

            if (value == null) {
                return false;
            }

            return value.compareTo(min) >= 0 && value.compareTo(max) <= 0;
        } catch (NumberFormatException e) {
            log.warn("Failed to parse BETWEEN values: min={}, max={}", minStr, maxStr);
            return false;
        }
    }

    /**
     * Converte valor para BigDecimal.
     */
    private BigDecimal toBigDecimal(Object value) {
        if (value == null) {
            return null;
        }
        if (value instanceof BigDecimal bd) {
            return bd;
        }
        if (value instanceof Number num) {
            return BigDecimal.valueOf(num.doubleValue());
        }
        try {
            return new BigDecimal(String.valueOf(value));
        } catch (NumberFormatException e) {
            return null;
        }
    }
}
