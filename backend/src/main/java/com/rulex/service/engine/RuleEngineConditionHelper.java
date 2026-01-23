package com.rulex.service.engine;

import com.rulex.dto.RuleConditionDTO;
import com.rulex.dto.TransactionRequest;
import com.rulex.util.RegexValidator;
import java.beans.PropertyDescriptor;
import java.math.BigDecimal;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class RuleEngineConditionHelper {

  private final ConditionMatcher conditionMatcher;

  public boolean evaluateCondition(TransactionRequest request, RuleConditionDTO condition) {
    String operatorRaw = condition.getOperator() == null ? "" : condition.getOperator().trim();
    String operator = conditionMatcher.normalizeOperator(operatorRaw);
    String rawValue = condition.getValue() == null ? "" : condition.getValue();

    Object leftValue = readComputedLeftValue(request, condition.getField());

    // Operadores unários (não exigem value)
    switch (operator) {
      case "IS_NULL":
        return leftValue == null;
      case "IS_NOT_NULL":
        return leftValue != null;
      case "IS_TRUE":
        return conditionMatcher.truthy(leftValue);
      case "IS_FALSE":
        return leftValue != null && !conditionMatcher.truthy(leftValue);
      default:
        // seguir
    }

    // Operadores que comparam com outro campo
    switch (operator) {
      case "GT_FIELD", "EQ_FIELD", "NE_FIELD", "GTE_FIELD", "LT_FIELD", "LTE_FIELD" -> {
        Object rightValue = readComputedLeftValue(request, rawValue);
        return compareFieldValues(leftValue, rightValue, operator);
      }
      default -> {
        // seguir
      }
    }

    if (leftValue == null) {
      return false;
    }

    // Operadores numéricos
    if (leftValue instanceof Number || isNumericString(leftValue)) {
      BigDecimal left = toBigDecimal(leftValue);
      if (left == null) {
        return false;
      }
      return switch (operator) {
        case "IN" -> inListNumberFlexible(left, rawValue);
        case "NOT_IN" -> !inListNumberFlexible(left, rawValue);
        case "BETWEEN" -> betweenNumber(left, rawValue, true);
        case "NOT_BETWEEN" -> !betweenNumber(left, rawValue, true);
        case "PERCENTAGE_OF_FIELD", "PERCENT_OF" ->
            evaluatePercentageOfField(leftValue, request, rawValue);
        case "MODULO_ZERO" -> evaluateModuloZero(leftValue, rawValue);
        case "DECIMAL_PLACES_GT" -> evaluateDecimalPlacesGt(leftValue, rawValue);
        case "GT", "GTE", "LT", "LTE", "EQ", "NE" -> {
          BigDecimal right = toBigDecimal(rawValue);
          if (right == null) {
            yield false;
          }
          yield switch (operator) {
            case "GT" -> left.compareTo(right) > 0;
            case "GTE" -> left.compareTo(right) >= 0;
            case "LT" -> left.compareTo(right) < 0;
            case "LTE" -> left.compareTo(right) <= 0;
            case "EQ" -> left.compareTo(right) == 0;
            case "NE" -> left.compareTo(right) != 0;
            default -> false;
          };
        }
        default -> false;
      };
    }

    String left = String.valueOf(leftValue);
    String right = unquote(rawValue);

    return switch (operator) {
      case "EQ" -> left.equals(right);
      case "NE" -> !left.equals(right);
      case "CONTAINS" -> left.contains(right);
      case "NOT_CONTAINS" -> !left.contains(right);
      case "STARTS_WITH" -> left.startsWith(right);
      case "ENDS_WITH" -> left.endsWith(right);
      case "MATCHES_REGEX" -> matchesRegex(left, rawValue);
      case "IN" -> inListStringFlexible(left, rawValue);
      case "NOT_IN" -> !inListStringFlexible(left, rawValue);
      default -> false;
    };
  }

  public String explainCondition(TransactionRequest request, RuleConditionDTO condition, boolean result) {
    Object fieldValue = readComputedLeftValue(request, condition.getField());
    return condition.getField()
        + " "
        + condition.getOperator()
        + " "
        + condition.getValue()
        + " (actual="
        + fieldValue
        + ") => "
        + result;
  }

  private boolean compareFieldValues(Object leftValue, Object rightValue, String operator) {
    if (leftValue == null || rightValue == null) {
      return false;
    }

    if (leftValue instanceof Number || rightValue instanceof Number) {
      BigDecimal left = toBigDecimal(leftValue);
      BigDecimal right = toBigDecimal(rightValue);
      if (left == null || right == null) return false;
      return switch (operator) {
        case "GT_FIELD" -> left.compareTo(right) > 0;
        case "GTE_FIELD" -> left.compareTo(right) >= 0;
        case "LT_FIELD" -> left.compareTo(right) < 0;
        case "LTE_FIELD" -> left.compareTo(right) <= 0;
        case "EQ_FIELD" -> left.compareTo(right) == 0;
        case "NE_FIELD" -> left.compareTo(right) != 0;
        default -> false;
      };
    }

    String left = String.valueOf(leftValue);
    String right = String.valueOf(rightValue);
    return switch (operator) {
      case "EQ_FIELD" -> left.equals(right);
      case "NE_FIELD" -> !left.equals(right);
      case "GT_FIELD" -> left.compareTo(right) > 0;
      case "GTE_FIELD" -> left.compareTo(right) >= 0;
      case "LT_FIELD" -> left.compareTo(right) < 0;
      case "LTE_FIELD" -> left.compareTo(right) <= 0;
      default -> false;
    };
  }

  private BigDecimal toBigDecimal(Object value) {
    if (value == null) return null;
    if (value instanceof BigDecimal bd) return bd;
    try {
      return new BigDecimal(String.valueOf(value));
    } catch (Exception e) {
      return null;
    }
  }

  private boolean evaluatePercentageOfField(
      Object leftValue, TransactionRequest request, String rawValue) {
    if (leftValue == null || rawValue == null || rawValue.isBlank()) {
      return false;
    }
    String[] parts = rawValue.split(":");
    if (parts.length < 2) {
      return false;
    }

    String otherField = parts[0].trim();
    BigDecimal min = parseBigDecimalSafe(parts[1]);
    BigDecimal max = parts.length >= 3 ? parseBigDecimalSafe(parts[2]) : null;
    if (min == null) {
      return false;
    }

    Object otherValue = readComputedLeftValue(request, otherField);
    BigDecimal left = toBigDecimal(leftValue);
    BigDecimal right = toBigDecimal(otherValue);
    if (left == null || right == null || right.compareTo(BigDecimal.ZERO) == 0) {
      return false;
    }

    BigDecimal pct =
        left.multiply(BigDecimal.valueOf(100)).divide(right, 6, java.math.RoundingMode.HALF_UP);
    if (max == null) {
      return pct.compareTo(min) >= 0;
    }
    return pct.compareTo(min) >= 0 && pct.compareTo(max) <= 0;
  }

  private BigDecimal parseBigDecimalSafe(String value) {
    try {
      return new BigDecimal(value.trim());
    } catch (Exception e) {
      return null;
    }
  }

  private boolean evaluateModuloZero(Object leftValue, String rawValue) {
    BigDecimal left = toBigDecimal(leftValue);
    BigDecimal divisor = parseBigDecimalSafe(rawValue == null ? "" : rawValue);
    if (left == null || divisor == null || divisor.compareTo(BigDecimal.ZERO) == 0) {
      return false;
    }
    return left.remainder(divisor).compareTo(BigDecimal.ZERO) == 0;
  }

  private boolean evaluateDecimalPlacesGt(Object leftValue, String rawValue) {
    BigDecimal left = toBigDecimal(leftValue);
    if (left == null) {
      return false;
    }
    int threshold = 0;
    try {
      threshold = Integer.parseInt(rawValue.trim());
    } catch (Exception e) {
      return false;
    }
    String normalized = left.stripTrailingZeros().toPlainString();
    int idx = normalized.indexOf('.');
    int decimals = idx >= 0 ? normalized.length() - idx - 1 : 0;
    return decimals > threshold;
  }

  private boolean betweenNumber(BigDecimal left, String raw, boolean inclusive) {
    List<String> parts = parseListTokens(raw);
    if (parts.size() < 2) {
      // aceitar "min..max"
      String s = raw == null ? "" : raw.trim();
      if (s.contains("..")) {
        String[] p = s.split("\\.\\.", 2);
        parts = List.of(p[0].trim(), p[1].trim());
      } else {
        return false;
      }
    }
    BigDecimal a = new BigDecimal(parts.get(0));
    BigDecimal b = new BigDecimal(parts.get(1));
    BigDecimal min = a.min(b);
    BigDecimal max = a.max(b);
    return inclusive
        ? left.compareTo(min) >= 0 && left.compareTo(max) <= 0
        : left.compareTo(min) > 0 && left.compareTo(max) < 0;
  }

  private boolean matchesRegex(String left, String rawRegex) {
    // Usa RegexValidator para proteção contra ReDoS
    return RegexValidator.safeFind(rawRegex, left);
  }

  private boolean inListNumberFlexible(BigDecimal left, String rawList) {
    for (String token : parseListTokens(rawList)) {
      if (token.isEmpty()) continue;
      if (left.compareTo(new BigDecimal(token)) == 0) return true;
    }
    return false;
  }

  private boolean inListStringFlexible(String left, String rawList) {
    for (String token : parseListTokens(rawList)) {
      if (left.equals(token)) return true;
    }
    return false;
  }

  /** Aceita: - "a,b,c" - "[a,b,c]" - "['RU','CN']" - "[7995, 7994]" */
  private List<String> parseListTokens(String raw) {
    if (raw == null) return List.of();
    String s = raw.trim();
    if (s.startsWith("[") && s.endsWith("]") && s.length() >= 2) {
      s = s.substring(1, s.length() - 1).trim();
    }
    if (s.isEmpty()) return List.of();
    String[] tokens = s.split(",");
    List<String> out = new java.util.ArrayList<>(tokens.length);
    for (String t : tokens) {
      String v = t == null ? "" : t.trim();
      if ((v.startsWith("'") && v.endsWith("'")) || (v.startsWith("\"") && v.endsWith("\""))) {
        v = v.substring(1, v.length() - 1);
      }
      out.add(v.trim());
    }
    return out;
  }

  private String unquote(String raw) {
    if (raw == null) return "";
    String v = raw.trim();
    if ((v.startsWith("'") && v.endsWith("'")) || (v.startsWith("\"") && v.endsWith("\""))) {
      if (v.length() >= 2) {
        v = v.substring(1, v.length() - 1);
      }
    }
    return v;
  }

  private Object readComputedLeftValue(TransactionRequest request, String fieldExpr) {
    if (fieldExpr == null) {
      return null;
    }
    String raw = fieldExpr.trim();
    if (raw.isEmpty()) {
      return null;
    }

    // ABS(x)
    java.util.regex.Matcher unary =
        java.util.regex.Pattern.compile("^(ABS|LEN|LOWER|UPPER|TRIM)\\(([A-Za-z0-9_]+)\\)$")
            .matcher(raw);
    if (unary.matches()) {
      String fn = unary.group(1);
      String arg = unary.group(2);
      Object base = readFieldValue(request, TransactionRequest.class, arg);
      return applyUnary(fn, base);
    }

    // ABS(<expr>) onde <expr> pode ser "a-b" (compatível com FRAUDE_REGRAS_DURAS_EXPORT.yaml)
    java.util.regex.Matcher absExpr =
        java.util.regex.Pattern.compile("^ABS\\((.+)\\)$").matcher(raw);
    if (absExpr.matches()) {
      Object v = evalNumericExpression(request, absExpr.group(1));
      return applyUnary("ABS", v);
    }

    // ABS_DIFF(a,b)
    java.util.regex.Matcher absDiff =
        java.util.regex.Pattern.compile("^ABS_DIFF\\(([A-Za-z0-9_]+)\\s*,\\s*([A-Za-z0-9_]+)\\)$")
            .matcher(raw);
    if (absDiff.matches()) {
      Object a = readFieldValue(request, TransactionRequest.class, absDiff.group(1));
      Object b = readFieldValue(request, TransactionRequest.class, absDiff.group(2));
      return absDiff(a, b);
    }

    // COALESCE(field, literal)
    java.util.regex.Matcher coalesce =
        java.util.regex.Pattern.compile("^COALESCE\\(([A-Za-z0-9_]+)\\s*,\\s*(.+)\\)$")
            .matcher(raw);
    if (coalesce.matches()) {
      Object base = readFieldValue(request, TransactionRequest.class, coalesce.group(1));
      if (base != null) return base;
      String literal = coalesce.group(2).trim();
      if ((literal.startsWith("'") && literal.endsWith("'"))
          || (literal.startsWith("\"") && literal.endsWith("\""))) {
        literal = literal.substring(1, literal.length() - 1);
      }
      return literal;
    }

    return readFieldValue(request, TransactionRequest.class, raw);
  }

  private Object applyUnary(String fn, Object v) {
    if (v == null) return null;
    return switch (fn) {
      case "ABS" -> {
        try {
          BigDecimal n =
              (v instanceof BigDecimal) ? (BigDecimal) v : new BigDecimal(String.valueOf(v));
          yield n.abs();
        } catch (Exception e) {
          yield null;
        }
      }
      case "LEN" -> String.valueOf(v).length();
      case "LOWER" -> String.valueOf(v).toLowerCase(java.util.Locale.ROOT);
      case "UPPER" -> String.valueOf(v).toUpperCase(java.util.Locale.ROOT);
      case "TRIM" -> String.valueOf(v).trim();
      default -> v;
    };
  }

  private Object absDiff(Object a, Object b) {
    if (a == null || b == null) return null;
    try {
      BigDecimal na =
          (a instanceof BigDecimal) ? (BigDecimal) a : new BigDecimal(String.valueOf(a));
      BigDecimal nb =
          (b instanceof BigDecimal) ? (BigDecimal) b : new BigDecimal(String.valueOf(b));
      return na.subtract(nb).abs();
    } catch (Exception e) {
      return null;
    }
  }

  /**
   * Avaliador mínimo de expressão numérica para o LHS (campo).
   *
   * <p>Objetivo: suportar padrão real do YAML: {@code ABS(atcCard - atcHost)}.
   *
   * <p>Suporta apenas: - literal numérico (ex.: "5", "10.2") - campo (property do
   * TransactionRequest) - subtração binária: "a-b" com espaços opcionais
   *
   * <p>Se não conseguir avaliar, retorna null.
   */
  private Object evalNumericExpression(TransactionRequest request, String expr) {
    if (expr == null) return null;
    String e = expr.trim();
    if (e.isEmpty()) return null;

    int minus = indexOfTopLevelMinus(e);
    if (minus > 0) {
      String left = e.substring(0, minus).trim();
      String right = e.substring(minus + 1).trim();
      BigDecimal a = resolveToBigDecimal(request, left);
      BigDecimal b = resolveToBigDecimal(request, right);
      if (a == null || b == null) return null;
      return a.subtract(b);
    }

    return resolveToBigDecimal(request, e);
  }

  private int indexOfTopLevelMinus(String e) {
    // Evita confundir número negativo "-5" no primeiro char.
    for (int i = 1; i < e.length(); i++) {
      if (e.charAt(i) == '-') return i;
    }
    return -1;
  }

  private BigDecimal resolveToBigDecimal(TransactionRequest request, String token) {
    if (token == null) return null;
    String t = token.trim();
    if (t.isEmpty()) return null;
    try {
      return new BigDecimal(t);
    } catch (Exception e) { // SEC-006 FIX
      // not a literal
    }
    Object v = readFieldValue(request, TransactionRequest.class, t);
    if (v == null) return null;
    try {
      return (v instanceof BigDecimal) ? (BigDecimal) v : new BigDecimal(String.valueOf(v));
    } catch (Exception e) {
      return null;
    }
  }

  private Object readFieldValue(Object subject, Class<?> subjectClass, String fieldName) {
    try {
      PropertyDescriptor pd = new PropertyDescriptor(fieldName, subjectClass);
      return pd.getReadMethod().invoke(subject);
    } catch (Exception e) {
      return null;
    }
  }

  private boolean isNumericString(Object value) {
    if (value instanceof String s) {
      try {
        new BigDecimal(s.trim());
        return true;
      } catch (Exception e) {
        return false;
      }
    }
    return false;
  }
}
