package com.rulex.service.engine;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * ARCH-003: Responsável por avaliar condições de regras.
 * Extraído do RuleEngineService para Single Responsibility.
 * 
 * Contém métodos de comparação, parsing e matching usados na avaliação de regras.
 */
@Component
@Slf4j
public class ConditionMatcher {

  /**
   * Verifica se um valor é "truthy" (não nulo e não vazio).
   */
  public boolean truthy(Object v) {
    if (v == null) return false;
    if (v instanceof String s) return !s.isBlank();
    if (v instanceof Boolean b) return b;
    if (v instanceof Number n) return n.doubleValue() != 0;
    return true;
  }

  /**
   * Compara dois valores usando o operador especificado.
   */
  public boolean compareFieldValues(Object leftValue, Object rightValue, String operator) {
    if (leftValue == null || rightValue == null) {
      return switch (operator) {
        case "EQ", "eq", "=" -> leftValue == rightValue;
        case "NEQ", "neq", "!=" -> leftValue != rightValue;
        default -> false;
      };
    }

    // Tenta comparação numérica
    BigDecimal leftNum = toBigDecimal(leftValue);
    BigDecimal rightNum = toBigDecimal(rightValue);

    if (leftNum != null && rightNum != null) {
      int cmp = leftNum.compareTo(rightNum);
      return switch (operator.toUpperCase()) {
        case "EQ", "=" -> cmp == 0;
        case "NEQ", "!=" -> cmp != 0;
        case "GT", ">" -> cmp > 0;
        case "GTE", ">=" -> cmp >= 0;
        case "LT", "<" -> cmp < 0;
        case "LTE", "<=" -> cmp <= 0;
        default -> false;
      };
    }

    // Fallback para comparação de strings
    String leftStr = leftValue.toString();
    String rightStr = rightValue.toString();
    int cmp = leftStr.compareTo(rightStr);

    return switch (operator.toUpperCase()) {
      case "EQ", "=" -> cmp == 0;
      case "NEQ", "!=" -> cmp != 0;
      case "GT", ">" -> cmp > 0;
      case "GTE", ">=" -> cmp >= 0;
      case "LT", "<" -> cmp < 0;
      case "LTE", "<=" -> cmp <= 0;
      case "CONTAINS" -> leftStr.contains(rightStr);
      case "NOT_CONTAINS" -> !leftStr.contains(rightStr);
      case "STARTS_WITH" -> leftStr.startsWith(rightStr);
      case "ENDS_WITH" -> leftStr.endsWith(rightStr);
      default -> false;
    };
  }

  /**
   * Converte um valor para BigDecimal de forma segura.
   */
  public BigDecimal toBigDecimal(Object value) {
    if (value == null) return null;
    if (value instanceof BigDecimal bd) return bd;
    if (value instanceof Number n) return BigDecimal.valueOf(n.doubleValue());
    try {
      return new BigDecimal(value.toString().trim());
    } catch (NumberFormatException e) {
      return null;
    }
  }

  /**
   * Verifica se um valor está entre dois limites.
   */
  public boolean betweenNumber(BigDecimal value, String rawRange, boolean inclusive) {
    if (value == null || rawRange == null) return false;
    
    String[] parts = rawRange.split("[|,]");
    if (parts.length != 2) return false;

    try {
      BigDecimal min = new BigDecimal(parts[0].trim());
      BigDecimal max = new BigDecimal(parts[1].trim());

      if (inclusive) {
        return value.compareTo(min) >= 0 && value.compareTo(max) <= 0;
      } else {
        return value.compareTo(min) > 0 && value.compareTo(max) < 0;
      }
    } catch (NumberFormatException e) {
      log.warn("Formato inválido para between: {}", rawRange);
      return false;
    }
  }

  /**
   * Verifica se um valor corresponde a uma expressão regular.
   */
  public boolean matchesRegex(String value, String rawRegex) {
    if (value == null || rawRegex == null) return false;
    try {
      return Pattern.matches(rawRegex, value);
    } catch (PatternSyntaxException e) {
      log.warn("Regex inválida: {}", rawRegex);
      return false;
    }
  }

  /**
   * Verifica se um número está em uma lista.
   */
  public boolean inListNumber(BigDecimal value, String rawList) {
    if (value == null || rawList == null) return false;
    
    return parseListTokens(rawList).stream()
        .map(this::toBigDecimal)
        .filter(bd -> bd != null)
        .anyMatch(bd -> bd.compareTo(value) == 0);
  }

  /**
   * Verifica se uma string está em uma lista (case-insensitive).
   */
  public boolean inListString(String value, String rawList) {
    if (value == null || rawList == null) return false;
    
    return parseListTokens(rawList).stream()
        .anyMatch(item -> item.equalsIgnoreCase(value));
  }

  /**
   * Avalia operador de módulo zero.
   */
  public boolean evaluateModuloZero(Object leftValue, String rawValue) {
    BigDecimal left = toBigDecimal(leftValue);
    BigDecimal divisor = toBigDecimal(rawValue);
    
    if (left == null || divisor == null || divisor.compareTo(BigDecimal.ZERO) == 0) {
      return false;
    }
    
    return left.remainder(divisor).compareTo(BigDecimal.ZERO) == 0;
  }

  /**
   * Avalia se o número de casas decimais é maior que o threshold.
   */
  public boolean evaluateDecimalPlacesGt(Object leftValue, String rawValue) {
    if (leftValue == null || rawValue == null) return false;
    
    String strValue = leftValue.toString();
    int decimalIndex = strValue.indexOf('.');
    if (decimalIndex < 0) return false;
    
    int decimalPlaces = strValue.length() - decimalIndex - 1;
    try {
      int threshold = Integer.parseInt(rawValue.trim());
      return decimalPlaces > threshold;
    } catch (NumberFormatException e) {
      return false;
    }
  }

  /**
   * Avalia porcentagem de um campo em relação a outro.
   */
  public boolean evaluatePercentageOfField(
      BigDecimal value, BigDecimal referenceValue, String operator, BigDecimal percentage) {
    if (value == null || referenceValue == null || percentage == null) return false;
    if (referenceValue.compareTo(BigDecimal.ZERO) == 0) return false;

    BigDecimal percentValue = referenceValue.multiply(percentage)
        .divide(BigDecimal.valueOf(100), 2, java.math.RoundingMode.HALF_UP);

    return switch (operator.toUpperCase()) {
      case "GT", ">" -> value.compareTo(percentValue) > 0;
      case "GTE", ">=" -> value.compareTo(percentValue) >= 0;
      case "LT", "<" -> value.compareTo(percentValue) < 0;
      case "LTE", "<=" -> value.compareTo(percentValue) <= 0;
      case "EQ", "=" -> value.compareTo(percentValue) == 0;
      default -> false;
    };
  }

  /**
   * Compara valores long.
   */
  public boolean compareLong(long actual, String operator, long threshold) {
    return switch (operator.toUpperCase()) {
      case "GT", ">" -> actual > threshold;
      case "GTE", ">=" -> actual >= threshold;
      case "LT", "<" -> actual < threshold;
      case "LTE", "<=" -> actual <= threshold;
      case "EQ", "=" -> actual == threshold;
      case "NEQ", "!=" -> actual != threshold;
      default -> false;
    };
  }

  /**
   * Compara valores BigDecimal.
   */
  public boolean compareBigDecimal(BigDecimal actual, String operator, BigDecimal threshold) {
    if (actual == null || threshold == null) return false;
    int cmp = actual.compareTo(threshold);
    return switch (operator.toUpperCase()) {
      case "GT", ">" -> cmp > 0;
      case "GTE", ">=" -> cmp >= 0;
      case "LT", "<" -> cmp < 0;
      case "LTE", "<=" -> cmp <= 0;
      case "EQ", "=" -> cmp == 0;
      case "NEQ", "!=" -> cmp != 0;
      default -> false;
    };
  }

  /**
   * Normaliza operador para formato canônico.
   */
  public String normalizeOperator(String raw) {
    if (raw == null) return "EQ";
    return switch (raw.toUpperCase().trim()) {
      case "=", "==", "EQUALS" -> "EQ";
      case "!=", "<>", "NOT_EQUALS" -> "NEQ";
      case ">", "GREATER_THAN" -> "GT";
      case ">=", "GREATER_THAN_OR_EQUAL" -> "GTE";
      case "<", "LESS_THAN" -> "LT";
      case "<=", "LESS_THAN_OR_EQUAL" -> "LTE";
      default -> raw.toUpperCase().trim();
    };
  }

  /**
   * Parse de lista de tokens separados por vírgula ou pipe.
   */
  public List<String> parseListTokens(String raw) {
    if (raw == null || raw.isBlank()) return List.of();
    return Arrays.stream(raw.split("[|,]"))
        .map(String::trim)
        .filter(s -> !s.isEmpty())
        .toList();
  }

  /**
   * Clamp de score para range 0-100.
   */
  public int clampScore(int score) {
    return Math.max(0, Math.min(100, score));
  }
}
