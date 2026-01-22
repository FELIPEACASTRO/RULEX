package com.rulex.service.engine.operator.strategy;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.math.BigDecimal;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Strategy para operadores de comparação entre campos.
 *
 * <p>Operadores suportados:
 * <ul>
 *   <li>FIELD_EQ - campo igual a outro campo</li>
 *   <li>FIELD_NEQ - campo diferente de outro campo</li>
 *   <li>FIELD_GT - campo maior que outro campo</li>
 *   <li>FIELD_GTE - campo maior ou igual a outro campo</li>
 *   <li>FIELD_LT - campo menor que outro campo</li>
 *   <li>FIELD_LTE - campo menor ou igual a outro campo</li>
 * </ul>
 *
 * @author Refactoring - RULEX Team
 * @since 2.0.0
 */
@Component
@Slf4j
public class FieldComparisonOperatorStrategy implements OperatorStrategy {

    private static final Set<ConditionOperator> SUPPORTED = Set.of(
        ConditionOperator.FIELD_EQ,
        ConditionOperator.FIELD_NEQ,
        ConditionOperator.FIELD_GT,
        ConditionOperator.FIELD_GTE,
        ConditionOperator.FIELD_LT,
        ConditionOperator.FIELD_LTE
    );

    @Override
    public Set<ConditionOperator> supportedOperators() {
        return SUPPORTED;
    }

    @Override
    public String category() {
        return "FIELD_COMPARISON";
    }

    @Override
    public int priority() {
        return 25;
    }

    @Override
    public boolean evaluate(Object fieldValue, RuleCondition condition, EvaluationContext context) {
        ConditionOperator operator = condition.getOperator();

        // Obter valor do campo de referência
        String refFieldName = condition.getValueFieldRef();
        if (refFieldName == null || refFieldName.isBlank()) {
            refFieldName = condition.getValueSingle(); // Fallback
        }

        if (refFieldName == null || refFieldName.isBlank()) {
            log.warn("Field comparison operator requires valueFieldRef or valueSingle");
            return false;
        }

        Object refValue = getFieldValue(refFieldName, context);

        return switch (operator) {
            case FIELD_EQ -> compareFields(fieldValue, refValue) == 0;
            case FIELD_NEQ -> compareFields(fieldValue, refValue) != 0;
            case FIELD_GT -> compareFields(fieldValue, refValue) > 0;
            case FIELD_GTE -> compareFields(fieldValue, refValue) >= 0;
            case FIELD_LT -> compareFields(fieldValue, refValue) < 0;
            case FIELD_LTE -> compareFields(fieldValue, refValue) <= 0;
            default -> {
                log.warn("Unexpected operator in FieldComparisonOperatorStrategy: {}", operator);
                yield false;
            }
        };
    }

    /**
     * Compara dois valores de campos.
     *
     * @return negativo se a < b, zero se a == b, positivo se a > b
     */
    private int compareFields(Object a, Object b) {
        if (a == null && b == null) {
            return 0;
        }
        if (a == null) {
            return -1;
        }
        if (b == null) {
            return 1;
        }

        // Tentar comparação numérica
        BigDecimal numA = toBigDecimal(a);
        BigDecimal numB = toBigDecimal(b);

        if (numA != null && numB != null) {
            return numA.compareTo(numB);
        }

        // Comparação de string
        return String.valueOf(a).compareTo(String.valueOf(b));
    }

    /**
     * Obtém valor de um campo do contexto.
     */
    private Object getFieldValue(String fieldName, EvaluationContext context) {
        if (context == null) {
            return null;
        }

        // Tentar obter do payload primeiro
        if (context.getPayload() != null && context.getPayload().containsKey(fieldName)) {
            return context.getPayload().get(fieldName);
        }

        // Tentar obter das variáveis
        if (context.getVariables() != null && context.getVariables().containsKey(fieldName)) {
            return context.getVariables().get(fieldName);
        }

        // Tentar obter do transactionRequest via reflection
        var request = context.getTransactionRequest();
        if (request == null) {
            return null;
        }

        try {
            var field = request.getClass().getDeclaredField(fieldName);
            field.setAccessible(true);
            return field.get(request);
        } catch (NoSuchFieldException e) {
            // Tentar via getter
            try {
                String getterName = "get" + Character.toUpperCase(fieldName.charAt(0)) + fieldName.substring(1);
                var method = request.getClass().getMethod(getterName);
                return method.invoke(request);
            } catch (Exception ex) {
                log.debug("Failed to get field value for {}: {}", fieldName, ex.getMessage());
                return null;
            }
        } catch (Exception e) {
            log.debug("Failed to get field value for {}: {}", fieldName, e.getMessage());
            return null;
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
