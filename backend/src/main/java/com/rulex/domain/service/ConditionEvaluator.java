package com.rulex.domain.service;

import com.rulex.domain.model.Rule;
import com.rulex.domain.model.RuleCondition;
import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

/**
 * Domain Service para avaliação de condições de regras.
 *
 * <p>Lógica pura de domínio sem dependências de framework. Avalia condições contra um payload
 * representado como Map.
 *
 * <p>Este serviço NÃO acessa banco de dados, cache ou serviços externos.
 */
public class ConditionEvaluator {

  /**
   * Avalia se uma condição é verdadeira para o payload.
   *
   * @param condition a condição a avaliar
   * @param payload mapa com valores do payload
   * @return true se condição é atendida
   */
  public boolean evaluate(RuleCondition condition, Map<String, Object> payload) {
    if (condition == null || payload == null) {
      return false;
    }

    String field = condition.getField();
    String operator = condition.getOperator();
    String value = condition.getValue();

    Object fieldValue = extractFieldValue(payload, field);

    return evaluateOperator(operator, fieldValue, value);
  }

  /**
   * Avalia múltiplas condições com operador lógico.
   *
   * @param conditions lista de condições
   * @param logicOperator AND ou OR
   * @param payload mapa com valores do payload
   * @return true se condições são atendidas conforme operador
   */
  public boolean evaluateAll(
      List<RuleCondition> conditions,
      Rule.LogicOperator logicOperator,
      Map<String, Object> payload) {

    if (conditions == null || conditions.isEmpty()) {
      return false;
    }

    if (logicOperator == Rule.LogicOperator.AND) {
      for (RuleCondition condition : conditions) {
        if (!evaluate(condition, payload)) {
          return false; // AND: qualquer falso = falso
        }
      }
      return true;
    } else {
      // OR
      for (RuleCondition condition : conditions) {
        if (evaluate(condition, payload)) {
          return true; // OR: qualquer verdadeiro = verdadeiro
        }
      }
      return false;
    }
  }

  /**
   * Avalia condição e retorna explicação.
   *
   * @param condition a condição
   * @param payload o payload
   * @return tupla (resultado, explicação)
   */
  public EvaluationResult evaluateWithExplanation(
      RuleCondition condition, Map<String, Object> payload) {
    boolean result = evaluate(condition, payload);
    Object fieldValue = extractFieldValue(payload, condition.getField());

    String explanation =
        String.format(
            "%s %s %s (atual: %s) → %s",
            condition.getField(),
            condition.getOperator(),
            condition.getValue(),
            fieldValue,
            result ? "TRUE" : "FALSE");

    return new EvaluationResult(result, explanation);
  }

  /** Resultado de avaliação com explicação */
  public record EvaluationResult(boolean triggered, String explanation) {}

  // ========== Operadores ==========

  private boolean evaluateOperator(String operator, Object fieldValue, String expectedValue) {
    if (operator == null) {
      return false;
    }

    return switch (operator.toUpperCase()) {
        // Igualdade
      case "EQ", "EQUALS" -> evaluateEquals(fieldValue, expectedValue);
      case "NEQ", "NOT_EQUALS" -> !evaluateEquals(fieldValue, expectedValue);

        // Comparação numérica
      case "GT", "GREATER_THAN" -> evaluateGreaterThan(fieldValue, expectedValue);
      case "GTE", "GREATER_THAN_OR_EQUAL" -> evaluateGreaterThanOrEqual(fieldValue, expectedValue);
      case "LT", "LESS_THAN" -> evaluateLessThan(fieldValue, expectedValue);
      case "LTE", "LESS_THAN_OR_EQUAL" -> evaluateLessThanOrEqual(fieldValue, expectedValue);

        // Range
      case "BETWEEN" -> evaluateBetween(fieldValue, expectedValue);
      case "NOT_BETWEEN" -> !evaluateBetween(fieldValue, expectedValue);

        // String
      case "CONTAINS" -> evaluateContains(fieldValue, expectedValue);
      case "NOT_CONTAINS" -> !evaluateContains(fieldValue, expectedValue);
      case "STARTS_WITH" -> evaluateStartsWith(fieldValue, expectedValue);
      case "ENDS_WITH" -> evaluateEndsWith(fieldValue, expectedValue);
      case "REGEX", "MATCHES" -> evaluateRegex(fieldValue, expectedValue);

        // Lista
      case "IN" -> evaluateIn(fieldValue, expectedValue);
      case "NOT_IN" -> !evaluateIn(fieldValue, expectedValue);

        // Nulos
      case "IS_NULL" -> fieldValue == null;
      case "IS_NOT_NULL", "NOT_NULL" -> fieldValue != null;

        // Booleanos
      case "IS_TRUE" -> evaluateIsTrue(fieldValue);
      case "IS_FALSE" -> !evaluateIsTrue(fieldValue);

      default -> {
        // Operador desconhecido - log e retorna false
        yield false;
      }
    };
  }

  private boolean evaluateEquals(Object fieldValue, String expectedValue) {
    if (fieldValue == null && expectedValue == null) return true;
    if (fieldValue == null || expectedValue == null) return false;

    String fieldStr = String.valueOf(fieldValue);
    return fieldStr.equalsIgnoreCase(expectedValue);
  }

  private boolean evaluateGreaterThan(Object fieldValue, String expectedValue) {
    BigDecimal fieldNum = toBigDecimal(fieldValue);
    BigDecimal expectedNum = toBigDecimal(expectedValue);
    if (fieldNum == null || expectedNum == null) return false;
    return fieldNum.compareTo(expectedNum) > 0;
  }

  private boolean evaluateGreaterThanOrEqual(Object fieldValue, String expectedValue) {
    BigDecimal fieldNum = toBigDecimal(fieldValue);
    BigDecimal expectedNum = toBigDecimal(expectedValue);
    if (fieldNum == null || expectedNum == null) return false;
    return fieldNum.compareTo(expectedNum) >= 0;
  }

  private boolean evaluateLessThan(Object fieldValue, String expectedValue) {
    BigDecimal fieldNum = toBigDecimal(fieldValue);
    BigDecimal expectedNum = toBigDecimal(expectedValue);
    if (fieldNum == null || expectedNum == null) return false;
    return fieldNum.compareTo(expectedNum) < 0;
  }

  private boolean evaluateLessThanOrEqual(Object fieldValue, String expectedValue) {
    BigDecimal fieldNum = toBigDecimal(fieldValue);
    BigDecimal expectedNum = toBigDecimal(expectedValue);
    if (fieldNum == null || expectedNum == null) return false;
    return fieldNum.compareTo(expectedNum) <= 0;
  }

  private boolean evaluateBetween(Object fieldValue, String expectedValue) {
    // Formato esperado: "min,max" ou "min|max"
    if (expectedValue == null) return false;
    String[] parts = expectedValue.split("[,|]");
    if (parts.length != 2) return false;

    BigDecimal fieldNum = toBigDecimal(fieldValue);
    BigDecimal min = toBigDecimal(parts[0].trim());
    BigDecimal max = toBigDecimal(parts[1].trim());

    if (fieldNum == null || min == null || max == null) return false;
    return fieldNum.compareTo(min) >= 0 && fieldNum.compareTo(max) <= 0;
  }

  private boolean evaluateContains(Object fieldValue, String expectedValue) {
    if (fieldValue == null || expectedValue == null) return false;
    return String.valueOf(fieldValue).toLowerCase().contains(expectedValue.toLowerCase());
  }

  private boolean evaluateStartsWith(Object fieldValue, String expectedValue) {
    if (fieldValue == null || expectedValue == null) return false;
    return String.valueOf(fieldValue).toLowerCase().startsWith(expectedValue.toLowerCase());
  }

  private boolean evaluateEndsWith(Object fieldValue, String expectedValue) {
    if (fieldValue == null || expectedValue == null) return false;
    return String.valueOf(fieldValue).toLowerCase().endsWith(expectedValue.toLowerCase());
  }

  private boolean evaluateRegex(Object fieldValue, String expectedValue) {
    if (fieldValue == null || expectedValue == null) return false;
    try {
      Pattern pattern = Pattern.compile(expectedValue, Pattern.CASE_INSENSITIVE);
      return pattern.matcher(String.valueOf(fieldValue)).matches();
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateIn(Object fieldValue, String expectedValue) {
    if (fieldValue == null || expectedValue == null) return false;
    String fieldStr = String.valueOf(fieldValue).toLowerCase();
    String[] values = expectedValue.split("[,|]");
    for (String v : values) {
      if (fieldStr.equals(v.trim().toLowerCase())) {
        return true;
      }
    }
    return false;
  }

  private boolean evaluateIsTrue(Object fieldValue) {
    if (fieldValue == null) return false;
    if (fieldValue instanceof Boolean b) return b;
    String str = String.valueOf(fieldValue).toLowerCase();
    return "true".equals(str) || "1".equals(str) || "y".equals(str) || "yes".equals(str);
  }

  // ========== Utilitários ==========

  /**
   * Extrai valor de campo do payload, suportando campos aninhados (ex: "customer.address.city").
   */
  @SuppressWarnings("unchecked")
  private Object extractFieldValue(Map<String, Object> payload, String field) {
    if (field == null || field.isBlank()) return null;

    // Campos aninhados com ponto
    if (field.contains(".")) {
      String[] parts = field.split("\\.");
      Object current = payload;
      for (String part : parts) {
        if (current instanceof Map<?, ?> map) {
          current = map.get(part);
        } else {
          return null;
        }
      }
      return current;
    }

    return payload.get(field);
  }

  private BigDecimal toBigDecimal(Object value) {
    if (value == null) return null;
    if (value instanceof BigDecimal bd) return bd;
    if (value instanceof Number n) return BigDecimal.valueOf(n.doubleValue());
    try {
      return new BigDecimal(String.valueOf(value).trim());
    } catch (NumberFormatException e) {
      return null;
    }
  }
}
