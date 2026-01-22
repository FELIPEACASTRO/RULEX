package com.rulex.service.engine.operator.strategy;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Strategy para operadores matemáticos.
 *
 * <p>Operadores suportados:
 * <ul>
 *   <li>MOD_EQ - módulo igual a</li>
 *   <li>MOD_NEQ - módulo diferente de</li>
 *   <li>GT_FIELD_MULTIPLIER - maior que campo multiplicado</li>
 *   <li>DECIMAL_PLACES_GT - casas decimais maior que</li>
 *   <li>EXPIRES_WITHIN_DAYS - expira dentro de N dias</li>
 * </ul>
 *
 * @author Refactoring - RULEX Team
 * @since 2.0.0
 */
@Component
@Slf4j
public class MathOperatorStrategy implements OperatorStrategy {

    private static final Set<ConditionOperator> SUPPORTED = Set.of(
        ConditionOperator.MOD_EQ,
        ConditionOperator.MOD_NEQ,
        ConditionOperator.GT_FIELD_MULTIPLIER,
        ConditionOperator.DECIMAL_PLACES_GT
    );

    @Override
    public Set<ConditionOperator> supportedOperators() {
        return SUPPORTED;
    }

    @Override
    public String category() {
        return "MATH";
    }

    @Override
    public int priority() {
        return 35;
    }

    @Override
    public boolean evaluate(Object fieldValue, RuleCondition condition, EvaluationContext context) {
        ConditionOperator operator = condition.getOperator();

        if (fieldValue == null) {
            return false;
        }

        return switch (operator) {
            case MOD_EQ -> evaluateModulo(fieldValue, condition, true);
            case MOD_NEQ -> evaluateModulo(fieldValue, condition, false);
            case GT_FIELD_MULTIPLIER -> evaluateGtFieldMultiplier(fieldValue, condition, context);
            case DECIMAL_PLACES_GT -> evaluateDecimalPlacesGt(fieldValue, condition);
            default -> {
                log.warn("Unexpected operator in MathOperatorStrategy: {}", operator);
                yield false;
            }
        };
    }

    /**
     * Avalia operação de módulo.
     *
     * @param expectEqual true para MOD_EQ, false para MOD_NEQ
     */
    private boolean evaluateModulo(Object fieldValue, RuleCondition condition, boolean expectEqual) {
        BigDecimal value = toBigDecimal(fieldValue);
        if (value == null) {
            return false;
        }

        String valueSingle = condition.getValueSingle();
        if (valueSingle == null || valueSingle.isBlank()) {
            log.warn("MOD operator requires valueSingle in format 'divisor,expected' or just 'divisor'");
            return false;
        }

        String[] parts = valueSingle.split(",");
        BigDecimal divisor;
        BigDecimal expected = BigDecimal.ZERO;

        try {
            divisor = new BigDecimal(parts[0].trim());
            if (parts.length > 1) {
                expected = new BigDecimal(parts[1].trim());
            }
        } catch (NumberFormatException e) {
            log.warn("Invalid MOD values: {}", valueSingle);
            return false;
        }

        if (divisor.compareTo(BigDecimal.ZERO) == 0) {
            log.warn("Division by zero in MOD operation");
            return false;
        }

        BigDecimal remainder = value.remainder(divisor);
        boolean isEqual = remainder.compareTo(expected) == 0;

        return expectEqual ? isEqual : !isEqual;
    }

    /**
     * Avalia se valor é maior que outro campo multiplicado por um fator.
     * Exemplo: amount > avgAmount * 5
     */
    private boolean evaluateGtFieldMultiplier(Object fieldValue, RuleCondition condition, EvaluationContext context) {
        BigDecimal value = toBigDecimal(fieldValue);
        if (value == null) {
            return false;
        }

        // Formato esperado: "fieldName,multiplier"
        String valueSingle = condition.getValueSingle();
        if (valueSingle == null || !valueSingle.contains(",")) {
            log.warn("GT_FIELD_MULTIPLIER requires 'fieldName,multiplier' format");
            return false;
        }

        String[] parts = valueSingle.split(",", 2);
        String refFieldName = parts[0].trim();
        BigDecimal multiplier;

        try {
            multiplier = new BigDecimal(parts[1].trim());
        } catch (NumberFormatException e) {
            log.warn("Invalid multiplier in GT_FIELD_MULTIPLIER: {}", parts[1]);
            return false;
        }

        Object refValue = getFieldValue(refFieldName, context);
        BigDecimal refBigDecimal = toBigDecimal(refValue);

        if (refBigDecimal == null) {
            return false;
        }

        BigDecimal threshold = refBigDecimal.multiply(multiplier);
        return value.compareTo(threshold) > 0;
    }

    /**
     * Avalia se o número tem mais casas decimais que o especificado.
     */
    private boolean evaluateDecimalPlacesGt(Object fieldValue, RuleCondition condition) {
        BigDecimal value = toBigDecimal(fieldValue);
        if (value == null) {
            return false;
        }

        int threshold;
        try {
            threshold = Integer.parseInt(condition.getValueSingle());
        } catch (NumberFormatException e) {
            log.warn("Invalid threshold for DECIMAL_PLACES_GT: {}", condition.getValueSingle());
            return false;
        }

        // Contar casas decimais
        int decimalPlaces = getDecimalPlaces(value);
        return decimalPlaces > threshold;
    }

    /**
     * Conta o número de casas decimais de um BigDecimal.
     */
    private int getDecimalPlaces(BigDecimal value) {
        String str = value.stripTrailingZeros().toPlainString();
        int index = str.indexOf('.');
        if (index < 0) {
            return 0;
        }
        return str.length() - index - 1;
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

        // Tentar obter do transactionRequest
        var request = context.getTransactionRequest();
        if (request == null) {
            return null;
        }

        try {
            String getterName = "get" + Character.toUpperCase(fieldName.charAt(0)) + fieldName.substring(1);
            var method = request.getClass().getMethod(getterName);
            return method.invoke(request);
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
