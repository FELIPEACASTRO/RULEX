package com.rulex.service.complex.evaluator;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.RuleCondition.ConditionOperator;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Avaliador para operadores de array e matemáticos.
 *
 * <p>Operadores suportados:
 * <ul>
 *   <li>ARRAY_CONTAINS, ARRAY_NOT_CONTAINS - elemento em array</li>
 *   <li>ARRAY_SIZE_EQ, ARRAY_SIZE_GT, ARRAY_SIZE_LT - tamanho de array</li>
 *   <li>ARRAY_ALL_MATCH, ARRAY_ANY_MATCH - predicados em array</li>
 *   <li>MOD_EQ - módulo</li>
 *   <li>MATH_ABS_LT, MATH_ABS_GT - valor absoluto</li>
 *   <li>PERCENTAGE_OF_LT, PERCENTAGE_OF_GT - percentual</li>
 * </ul>
 */
@Component
@Slf4j
public class ArrayMathOperatorEvaluator implements OperatorEvaluator {

    private static final Set<ConditionOperator> SUPPORTED = Set.of(
        ConditionOperator.ARRAY_CONTAINS,
        ConditionOperator.ARRAY_NOT_CONTAINS,
        ConditionOperator.ARRAY_SIZE_EQ,
        ConditionOperator.ARRAY_SIZE_GT,
        ConditionOperator.ARRAY_SIZE_LT,
        ConditionOperator.MOD_EQ,
        ConditionOperator.MOD_NEQ
    );

    @Override
    public Set<ConditionOperator> getSupportedOperators() {
        return SUPPORTED;
    }

    @Override
    public boolean evaluate(RuleCondition condition, EvaluationContext context) {
        ConditionOperator op = condition.getOperator();
        String fieldName = condition.getFieldName();
        Object fieldValue = getFieldValue(context, fieldName);

        log.debug("ArrayMathOperatorEvaluator: op={}, field={}, value={}", op, fieldName, fieldValue);

        return switch (op) {
            case ARRAY_CONTAINS -> evaluateArrayContains(fieldValue, condition.getValueSingle());
            case ARRAY_NOT_CONTAINS -> !evaluateArrayContains(fieldValue, condition.getValueSingle());
            case ARRAY_SIZE_EQ -> evaluateArraySize(fieldValue, condition.getValueSingle(), 0);
            case ARRAY_SIZE_GT -> evaluateArraySize(fieldValue, condition.getValueSingle(), 1);
            case ARRAY_SIZE_LT -> evaluateArraySize(fieldValue, condition.getValueSingle(), -1);
            case MOD_EQ -> evaluateModEq(fieldValue, condition.getValueSingle(), condition.getValueMin());
            case MOD_NEQ -> !evaluateModEq(fieldValue, condition.getValueSingle(), condition.getValueMin());
            default -> false;
        };
    }

    private Object getFieldValue(EvaluationContext context, String fieldName) {
        if (context == null || fieldName == null) {
            return null;
        }

        Map<String, Object> payload = context.getPayload();
        if (payload != null && payload.containsKey(fieldName)) {
            return payload.get(fieldName);
        }

        if (context.getTransactionRequest() != null) {
            try {
                var request = context.getTransactionRequest();
                var field = request.getClass().getDeclaredField(fieldName);
                field.setAccessible(true);
                return field.get(request);
            } catch (Exception e) {
                log.trace("Campo {} não encontrado no TransactionRequest", fieldName);
            }
        }

        return null;
    }

    private boolean evaluateArrayContains(Object fieldValue, String element) {
        if (fieldValue == null || element == null) return false;

        if (fieldValue instanceof Collection<?> collection) {
            return collection.stream()
                .anyMatch(item -> element.equals(String.valueOf(item)));
        }

        if (fieldValue.getClass().isArray()) {
            Object[] array = (Object[]) fieldValue;
            for (Object item : array) {
                if (element.equals(String.valueOf(item))) return true;
            }
        }

        // Se for string, verificar se contém como substring
        if (fieldValue instanceof String str) {
            return str.contains(element);
        }

        return false;
    }

    private boolean evaluateArraySize(Object fieldValue, String expectedSize, int comparison) {
        if (fieldValue == null || expectedSize == null) return false;

        int size = getSize(fieldValue);
        int expected;
        try {
            expected = Integer.parseInt(expectedSize);
        } catch (NumberFormatException e) {
            return false;
        }

        return switch (comparison) {
            case 0 -> size == expected;  // EQ
            case 1 -> size > expected;   // GT
            case -1 -> size < expected;  // LT
            default -> false;
        };
    }

    private int getSize(Object value) {
        if (value instanceof Collection<?> c) return c.size();
        if (value instanceof Map<?, ?> m) return m.size();
        if (value instanceof String s) return s.length();
        if (value.getClass().isArray()) return java.lang.reflect.Array.getLength(value);
        return 0;
    }

    private boolean evaluateArrayAllMatch(Object fieldValue, List<String> expectedValues) {
        if (fieldValue == null || expectedValues == null || expectedValues.isEmpty()) return false;

        Collection<?> collection = toCollection(fieldValue);
        if (collection == null) return false;

        for (String expected : expectedValues) {
            boolean found = collection.stream()
                .anyMatch(item -> expected.equals(String.valueOf(item)));
            if (!found) return false;
        }
        return true;
    }

    private boolean evaluateArrayAnyMatch(Object fieldValue, List<String> expectedValues) {
        if (fieldValue == null || expectedValues == null || expectedValues.isEmpty()) return false;

        Collection<?> collection = toCollection(fieldValue);
        if (collection == null) return false;

        for (String expected : expectedValues) {
            boolean found = collection.stream()
                .anyMatch(item -> expected.equals(String.valueOf(item)));
            if (found) return true;
        }
        return false;
    }

    private Collection<?> toCollection(Object value) {
        if (value instanceof Collection<?> c) return c;
        if (value.getClass().isArray()) {
            return java.util.Arrays.asList((Object[]) value);
        }
        return null;
    }

    private boolean evaluateModEq(Object fieldValue, String divisor, String expected) {
        if (fieldValue == null || divisor == null || expected == null) return false;

        try {
            long value = toLong(fieldValue);
            long div = Long.parseLong(divisor);
            long exp = Long.parseLong(expected);

            if (div == 0) return false;
            return value % div == exp;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    private boolean evaluateMathAbs(Object fieldValue, String threshold, boolean lessThan) {
        if (fieldValue == null || threshold == null) return false;

        try {
            BigDecimal value = toBigDecimal(fieldValue);
            BigDecimal thresh = new BigDecimal(threshold);
            BigDecimal absValue = value.abs();

            return lessThan ? absValue.compareTo(thresh) < 0 : absValue.compareTo(thresh) > 0;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    private boolean evaluatePercentageOf(EvaluationContext context, String fieldName, String baseFieldName, String threshold, boolean lessThan) {
        if (fieldName == null || baseFieldName == null || threshold == null) return false;

        Object fieldValue = getFieldValue(context, fieldName);
        Object baseValue = getFieldValue(context, baseFieldName);

        if (fieldValue == null || baseValue == null) return false;

        try {
            BigDecimal value = toBigDecimal(fieldValue);
            BigDecimal base = toBigDecimal(baseValue);
            BigDecimal thresh = new BigDecimal(threshold);

            if (base.compareTo(BigDecimal.ZERO) == 0) return false;

            BigDecimal percentage = value.multiply(BigDecimal.valueOf(100))
                .divide(base, 2, RoundingMode.HALF_UP);

            return lessThan ? percentage.compareTo(thresh) < 0 : percentage.compareTo(thresh) > 0;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    private long toLong(Object value) {
        if (value instanceof Number n) return n.longValue();
        return Long.parseLong(String.valueOf(value));
    }

    private BigDecimal toBigDecimal(Object value) {
        if (value instanceof BigDecimal bd) return bd;
        if (value instanceof Number n) return BigDecimal.valueOf(n.doubleValue());
        return new BigDecimal(String.valueOf(value));
    }

    @Override
    public String getCategory() {
        return "ARRAY_MATH";
    }
}
