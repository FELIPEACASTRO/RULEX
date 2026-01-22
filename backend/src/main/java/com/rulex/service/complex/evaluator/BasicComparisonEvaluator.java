package com.rulex.service.complex.evaluator;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.math.BigDecimal;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Avaliador para operadores de comparação básica.
 *
 * <p>Operadores suportados:
 *
 * <ul>
 *   <li>EQ, NEQ - igualdade
 *   <li>GT, GTE, LT, LTE - comparação numérica
 *   <li>IN, NOT_IN - pertencimento a lista
 *   <li>BETWEEN, NOT_BETWEEN - intervalo
 *   <li>IS_NULL, NOT_NULL - nulidade
 *   <li>IS_TRUE, IS_FALSE - booleano
 * </ul>
 */
@Component
@Slf4j
public class BasicComparisonEvaluator implements OperatorEvaluator {

  private static final Set<ConditionOperator> SUPPORTED =
      Set.of(
          ConditionOperator.EQ,
          ConditionOperator.NEQ,
          ConditionOperator.GT,
          ConditionOperator.GTE,
          ConditionOperator.LT,
          ConditionOperator.LTE,
          ConditionOperator.IN,
          ConditionOperator.NOT_IN,
          ConditionOperator.BETWEEN,
          ConditionOperator.NOT_BETWEEN,
          ConditionOperator.IS_NULL,
          ConditionOperator.NOT_NULL,
          ConditionOperator.IS_TRUE,
          ConditionOperator.IS_FALSE);

  @Override
  public Set<ConditionOperator> getSupportedOperators() {
    return SUPPORTED;
  }

  @Override
  public boolean evaluate(RuleCondition condition, EvaluationContext context) {
    ConditionOperator op = condition.getOperator();
    String fieldName = condition.getFieldName();
    Object fieldValue = getFieldValue(context, fieldName);

    log.debug(
        "BasicComparisonEvaluator: op={}, field={}, value={}, expected={}",
        op,
        fieldName,
        fieldValue,
        condition.getValueSingle());

    return switch (op) {
      case EQ ->
          evaluateEquals(fieldValue, condition.getValueSingle(), condition.getCaseSensitive());
      case NEQ ->
          !evaluateEquals(fieldValue, condition.getValueSingle(), condition.getCaseSensitive());
      case GT -> evaluateComparison(fieldValue, condition.getValueSingle()) > 0;
      case GTE -> evaluateComparison(fieldValue, condition.getValueSingle()) >= 0;
      case LT -> evaluateComparison(fieldValue, condition.getValueSingle()) < 0;
      case LTE -> evaluateComparison(fieldValue, condition.getValueSingle()) <= 0;
      case IN -> evaluateIn(fieldValue, condition.getValueArray(), condition.getCaseSensitive());
      case NOT_IN ->
          !evaluateIn(fieldValue, condition.getValueArray(), condition.getCaseSensitive());
      case BETWEEN -> evaluateBetween(fieldValue, condition.getValueMin(), condition.getValueMax());
      case NOT_BETWEEN ->
          !evaluateBetween(fieldValue, condition.getValueMin(), condition.getValueMax());
      case IS_NULL -> fieldValue == null;
      case NOT_NULL -> fieldValue != null;
      case IS_TRUE -> isTrue(fieldValue);
      case IS_FALSE -> isFalse(fieldValue);
      default -> false;
    };
  }

  private Object getFieldValue(EvaluationContext context, String fieldName) {
    if (context == null || fieldName == null) {
      return null;
    }

    // Tentar obter do payload
    Map<String, Object> payload = context.getPayload();
    if (payload != null && payload.containsKey(fieldName)) {
      return payload.get(fieldName);
    }

    // Tentar obter do TransactionRequest via reflection
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

  private boolean evaluateEquals(Object fieldValue, String expected, Boolean caseSensitive) {
    if (fieldValue == null || expected == null) {
      return fieldValue == null && expected == null;
    }

    String fieldStr = String.valueOf(fieldValue);
    boolean isCaseSensitive = caseSensitive != null && caseSensitive;

    if (isCaseSensitive) {
      return fieldStr.equals(expected);
    } else {
      return fieldStr.equalsIgnoreCase(expected);
    }
  }

  private int evaluateComparison(Object fieldValue, String expected) {
    if (fieldValue == null || expected == null) {
      return fieldValue == null ? -1 : 1;
    }

    try {
      BigDecimal fieldNum = toBigDecimal(fieldValue);
      BigDecimal expectedNum = new BigDecimal(expected);
      return fieldNum.compareTo(expectedNum);
    } catch (NumberFormatException e) {
      // Fallback para comparação de string
      return String.valueOf(fieldValue).compareTo(expected);
    }
  }

  private BigDecimal toBigDecimal(Object value) {
    if (value instanceof BigDecimal bd) {
      return bd;
    } else if (value instanceof Number num) {
      return BigDecimal.valueOf(num.doubleValue());
    } else {
      return new BigDecimal(String.valueOf(value));
    }
  }

  private boolean evaluateIn(Object fieldValue, List<String> values, Boolean caseSensitive) {
    if (fieldValue == null || values == null || values.isEmpty()) {
      return false;
    }

    String fieldStr = String.valueOf(fieldValue);
    boolean isCaseSensitive = caseSensitive != null && caseSensitive;

    for (String value : values) {
      if (isCaseSensitive) {
        if (fieldStr.equals(value)) return true;
      } else {
        if (fieldStr.equalsIgnoreCase(value)) return true;
      }
    }
    return false;
  }

  private boolean evaluateBetween(Object fieldValue, String min, String max) {
    if (fieldValue == null || min == null || max == null) {
      return false;
    }

    try {
      BigDecimal fieldNum = toBigDecimal(fieldValue);
      BigDecimal minNum = new BigDecimal(min);
      BigDecimal maxNum = new BigDecimal(max);

      return fieldNum.compareTo(minNum) >= 0 && fieldNum.compareTo(maxNum) <= 0;
    } catch (NumberFormatException e) {
      log.warn("Erro ao comparar BETWEEN: field={}, min={}, max={}", fieldValue, min, max);
      return false;
    }
  }

  private boolean isEmpty(Object value) {
    if (value == null) return true;
    if (value instanceof String s) return s.isEmpty();
    if (value instanceof Collection<?> c) return c.isEmpty();
    if (value instanceof Map<?, ?> m) return m.isEmpty();
    if (value.getClass().isArray()) return java.lang.reflect.Array.getLength(value) == 0;
    return false;
  }

  private boolean isTrue(Object value) {
    if (value == null) return false;
    if (value instanceof Boolean b) return b;
    String str = String.valueOf(value).toLowerCase(Locale.ROOT);
    return "true".equals(str) || "1".equals(str) || "yes".equals(str) || "y".equals(str);
  }

  private boolean isFalse(Object value) {
    if (value == null) return false;
    if (value instanceof Boolean b) return !b;
    String str = String.valueOf(value).toLowerCase(Locale.ROOT);
    return "false".equals(str) || "0".equals(str) || "no".equals(str) || "n".equals(str);
  }

  @Override
  public String getCategory() {
    return "BASIC_COMPARISON";
  }
}
