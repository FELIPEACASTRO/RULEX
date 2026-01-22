package com.rulex.service.engine.operator.strategy;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Strategy para operadores de lista.
 *
 * <p>Operadores suportados:
 * <ul>
 *   <li>IN - valor está na lista</li>
 *   <li>NOT_IN - valor não está na lista</li>
 *   <li>ARRAY_CONTAINS - array contém valor</li>
 *   <li>ARRAY_NOT_CONTAINS - array não contém valor</li>
 *   <li>ARRAY_SIZE_EQ - tamanho do array igual a</li>
 *   <li>ARRAY_SIZE_GT - tamanho do array maior que</li>
 *   <li>ARRAY_SIZE_LT - tamanho do array menor que</li>
 * </ul>
 *
 * @author Refactoring - RULEX Team
 * @since 2.0.0
 */
@Component
@Slf4j
public class ListOperatorStrategy implements OperatorStrategy {

    private static final Set<ConditionOperator> SUPPORTED = Set.of(
        ConditionOperator.IN,
        ConditionOperator.NOT_IN,
        ConditionOperator.ARRAY_CONTAINS,
        ConditionOperator.ARRAY_NOT_CONTAINS,
        ConditionOperator.ARRAY_SIZE_EQ,
        ConditionOperator.ARRAY_SIZE_GT,
        ConditionOperator.ARRAY_SIZE_LT
    );

    @Override
    public Set<ConditionOperator> supportedOperators() {
        return SUPPORTED;
    }

    @Override
    public String category() {
        return "LIST";
    }

    @Override
    public int priority() {
        return 30;
    }

    @Override
    public boolean evaluate(Object fieldValue, RuleCondition condition, EvaluationContext context) {
        ConditionOperator operator = condition.getOperator();

        return switch (operator) {
            case IN -> evaluateIn(fieldValue, condition);
            case NOT_IN -> !evaluateIn(fieldValue, condition);
            case ARRAY_CONTAINS -> evaluateArrayContains(fieldValue, condition);
            case ARRAY_NOT_CONTAINS -> !evaluateArrayContains(fieldValue, condition);
            case ARRAY_SIZE_EQ -> evaluateArraySize(fieldValue, condition, 0);
            case ARRAY_SIZE_GT -> evaluateArraySize(fieldValue, condition, 1);
            case ARRAY_SIZE_LT -> evaluateArraySize(fieldValue, condition, -1);
            default -> {
                log.warn("Unexpected operator in ListOperatorStrategy: {}", operator);
                yield false;
            }
        };
    }

    /**
     * Verifica se valor está na lista especificada.
     */
    private boolean evaluateIn(Object fieldValue, RuleCondition condition) {
        if (fieldValue == null) {
            return false;
        }

        List<String> values = getListValues(condition);
        if (values.isEmpty()) {
            return false;
        }

        String fieldStr = String.valueOf(fieldValue);
        Boolean caseSensitive = condition.getCaseSensitive();
        boolean isCaseSensitive = caseSensitive != null && caseSensitive;

        if (isCaseSensitive) {
            return values.contains(fieldStr);
        } else {
            String fieldLower = fieldStr.toLowerCase();
            return values.stream()
                .anyMatch(v -> v.toLowerCase().equals(fieldLower));
        }
    }

    /**
     * Verifica se array contém valor especificado.
     */
    private boolean evaluateArrayContains(Object fieldValue, RuleCondition condition) {
        if (fieldValue == null) {
            return false;
        }

        String searchValue = condition.getValueSingle();
        if (searchValue == null) {
            return false;
        }

        // Se fieldValue é uma lista/array
        if (fieldValue instanceof List<?> list) {
            return list.stream()
                .anyMatch(item -> searchValue.equals(String.valueOf(item)));
        }

        if (fieldValue.getClass().isArray()) {
            Object[] array = (Object[]) fieldValue;
            return Arrays.stream(array)
                .anyMatch(item -> searchValue.equals(String.valueOf(item)));
        }

        // Se é uma string separada por vírgula
        if (fieldValue instanceof String str) {
            return Arrays.stream(str.split(","))
                .map(String::trim)
                .anyMatch(searchValue::equals);
        }

        return false;
    }

    /**
     * Avalia tamanho do array.
     *
     * @param comparison 0 = igual, 1 = maior que, -1 = menor que
     */
    private boolean evaluateArraySize(Object fieldValue, RuleCondition condition, int comparison) {
        if (fieldValue == null) {
            return false;
        }

        int expectedSize;
        try {
            expectedSize = Integer.parseInt(condition.getValueSingle());
        } catch (NumberFormatException e) {
            log.warn("Invalid array size value: {}", condition.getValueSingle());
            return false;
        }

        int actualSize = getSize(fieldValue);

        return switch (comparison) {
            case 0 -> actualSize == expectedSize;
            case 1 -> actualSize > expectedSize;
            case -1 -> actualSize < expectedSize;
            default -> false;
        };
    }

    /**
     * Obtém tamanho de lista/array/string.
     */
    private int getSize(Object value) {
        if (value instanceof List<?> list) {
            return list.size();
        }
        if (value.getClass().isArray()) {
            return ((Object[]) value).length;
        }
        if (value instanceof String str) {
            // Conta elementos separados por vírgula
            if (str.isEmpty()) {
                return 0;
            }
            return str.split(",").length;
        }
        return 1; // Valor único
    }

    /**
     * Obtém lista de valores da condição.
     */
    private List<String> getListValues(RuleCondition condition) {
        // Primeiro tenta usar valueArray (lista)
        if (condition.getValueArray() != null && !condition.getValueArray().isEmpty()) {
            return condition.getValueArray();
        }

        // Fallback para valueSingle separado por vírgula
        String valueSingle = condition.getValueSingle();
        if (valueSingle == null || valueSingle.isEmpty()) {
            return List.of();
        }

        return Arrays.stream(valueSingle.split(","))
            .map(String::trim)
            .filter(s -> !s.isEmpty())
            .collect(Collectors.toList());
    }
}
