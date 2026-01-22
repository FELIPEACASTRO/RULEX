package com.rulex.service.evaluation;

import com.rulex.dto.RuleConditionDTO;
import com.rulex.dto.TransactionRequest;
import com.rulex.util.RegexValidator;
import java.beans.PropertyDescriptor;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.springframework.stereotype.Component;

/**
 * Avaliador de condições simples para regras.
 *
 * <p>Extraído de RuleEngineService para melhorar a manutenibilidade. Responsável por avaliar
 * condições individuais de regras contra uma transação.
 */
@Component
public class SimpleConditionEvaluator {

  private static final Pattern UNARY_PATTERN =
      Pattern.compile("^(ABS|LEN|LOWER|UPPER|TRIM)\\(([A-Za-z0-9_]+)\\)$");
  private static final Pattern ABS_EXPR_PATTERN = Pattern.compile("^ABS\\((.+)\\)$");
  private static final Pattern ABS_DIFF_PATTERN =
      Pattern.compile("^ABS_DIFF\\(([A-Za-z0-9_]+)\\s*,\\s*([A-Za-z0-9_]+)\\)$");

  /**
   * Avalia uma condição contra uma transação.
   *
   * @param request a transação
   * @param condition a condição a avaliar
   * @return true se a condição é satisfeita
   */
  public boolean evaluateCondition(TransactionRequest request, RuleConditionDTO condition) {
    String operatorRaw = condition.getOperator() == null ? "" : condition.getOperator().trim();
    String operator = normalizeOperator(operatorRaw);
    String rawValue = condition.getValue() == null ? "" : condition.getValue();

    Object leftValue = readComputedLeftValue(request, condition.getField());

    // Operadores unários (não exigem value)
    switch (operator) {
      case "IS_NULL":
        return leftValue == null;
      case "IS_NOT_NULL":
        return leftValue != null;
      case "IS_TRUE":
        return truthy(leftValue);
      case "IS_FALSE":
        return leftValue != null && !truthy(leftValue);
      default:
        // seguir
    }

    // Operadores que comparam com outro campo
    switch (operator) {
      case "GT_FIELD", "EQ_FIELD", "NE_FIELD", "GTE_FIELD", "LT_FIELD", "LTE_FIELD" -> {
        Object rightValue = readComputedLeftValue(request, rawValue);
        return compareFieldValues(leftValue, rightValue, operator);
      }
      case "PERCENTAGE_OF_FIELD" -> {
        return evaluatePercentageOfField(leftValue, request, rawValue);
      }
      case "MODULO_ZERO" -> {
        return evaluateModuloZero(leftValue, rawValue);
      }
      case "DECIMAL_PLACES_GT" -> {
        return evaluateDecimalPlacesGt(leftValue, rawValue);
      }
    }

    // Operadores de comparação numérica
    BigDecimal left = toBigDecimal(leftValue);
    BigDecimal right = toBigDecimal(rawValue);

    switch (operator) {
      case "EQ", "EQUALS" -> {
        if (left != null && right != null) return left.compareTo(right) == 0;
        String ls = leftValue == null ? "" : String.valueOf(leftValue);
        return ls.equalsIgnoreCase(rawValue);
      }
      case "NE", "NOT_EQUALS" -> {
        if (left != null && right != null) return left.compareTo(right) != 0;
        String ls = leftValue == null ? "" : String.valueOf(leftValue);
        return !ls.equalsIgnoreCase(rawValue);
      }
      case "GT" -> {
        return left != null && right != null && left.compareTo(right) > 0;
      }
      case "GTE" -> {
        return left != null && right != null && left.compareTo(right) >= 0;
      }
      case "LT" -> {
        return left != null && right != null && left.compareTo(right) < 0;
      }
      case "LTE" -> {
        return left != null && right != null && left.compareTo(right) <= 0;
      }
      case "BETWEEN" -> {
        return betweenNumber(left, rawValue, true);
      }
      case "BETWEEN_EXCLUSIVE" -> {
        return betweenNumber(left, rawValue, false);
      }
      case "IN" -> {
        if (left != null) return inListNumberFlexible(left, rawValue);
        String ls = leftValue == null ? "" : String.valueOf(leftValue);
        return inListStringFlexible(ls, rawValue);
      }
      case "NOT_IN" -> {
        if (left != null) return !inListNumberFlexible(left, rawValue);
        String ls = leftValue == null ? "" : String.valueOf(leftValue);
        return !inListStringFlexible(ls, rawValue);
      }
      case "CONTAINS" -> {
        String ls = leftValue == null ? "" : String.valueOf(leftValue);
        return ls.toLowerCase().contains(rawValue.toLowerCase());
      }
      case "NOT_CONTAINS" -> {
        String ls = leftValue == null ? "" : String.valueOf(leftValue);
        return !ls.toLowerCase().contains(rawValue.toLowerCase());
      }
      case "STARTS_WITH" -> {
        String ls = leftValue == null ? "" : String.valueOf(leftValue);
        return ls.toLowerCase().startsWith(rawValue.toLowerCase());
      }
      case "ENDS_WITH" -> {
        String ls = leftValue == null ? "" : String.valueOf(leftValue);
        return ls.toLowerCase().endsWith(rawValue.toLowerCase());
      }
      case "REGEX", "MATCHES" -> {
        String ls = leftValue == null ? "" : String.valueOf(leftValue);
        return matchesRegex(ls, rawValue);
      }
    }

    return false;
  }

  /**
   * Explica uma condição avaliada.
   *
   * @param request a transação
   * @param condition a condição
   * @param result o resultado da avaliação
   * @return uma string explicando a avaliação
   */
  public String explainCondition(
      TransactionRequest request, RuleConditionDTO condition, boolean result) {
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

  // ========== Métodos auxiliares ==========

  boolean truthy(Object v) {
    if (v == null) return false;
    if (v instanceof Boolean b) return b;
    if (v instanceof Number n) return n.doubleValue() != 0;
    String s = String.valueOf(v).trim().toLowerCase();
    return "true".equals(s) || "1".equals(s) || "yes".equals(s);
  }

  String normalizeOperator(String raw) {
    if (raw == null) return "";
    String up = raw.trim().toUpperCase();
    return switch (up) {
      case "=", "==" -> "EQ";
      case "!=", "<>" -> "NE";
      case ">" -> "GT";
      case ">=" -> "GTE";
      case "<" -> "LT";
      case "<=" -> "LTE";
      default -> up;
    };
  }

  boolean compareFieldValues(Object leftValue, Object rightValue, String operator) {
    BigDecimal l = toBigDecimal(leftValue);
    BigDecimal r = toBigDecimal(rightValue);
    if (l == null || r == null) {
      String ls = leftValue == null ? "" : String.valueOf(leftValue);
      String rs = rightValue == null ? "" : String.valueOf(rightValue);
      return switch (operator) {
        case "EQ_FIELD" -> ls.equalsIgnoreCase(rs);
        case "NE_FIELD" -> !ls.equalsIgnoreCase(rs);
        default -> false;
      };
    }
    int cmp = l.compareTo(r);
    return switch (operator) {
      case "GT_FIELD" -> cmp > 0;
      case "GTE_FIELD" -> cmp >= 0;
      case "LT_FIELD" -> cmp < 0;
      case "LTE_FIELD" -> cmp <= 0;
      case "EQ_FIELD" -> cmp == 0;
      case "NE_FIELD" -> cmp != 0;
      default -> false;
    };
  }

  BigDecimal toBigDecimal(Object value) {
    if (value == null) return null;
    if (value instanceof BigDecimal bd) return bd;
    try {
      return new BigDecimal(String.valueOf(value).trim());
    } catch (Exception e) {
      return null;
    }
  }

  boolean evaluatePercentageOfField(Object leftValue, TransactionRequest request, String rawValue) {
    String[] parts = rawValue.split(",");
    if (parts.length < 2) return false;
    String refField = parts[0].trim();
    BigDecimal pct;
    try {
      pct = new BigDecimal(parts[1].trim());
    } catch (Exception e) {
      return false;
    }
    Object refVal = readFieldValue(request, TransactionRequest.class, refField);
    BigDecimal refBd = toBigDecimal(refVal);
    BigDecimal leftBd = toBigDecimal(leftValue);
    if (refBd == null || leftBd == null) return false;
    BigDecimal threshold =
        refBd.multiply(pct).divide(BigDecimal.valueOf(100), RoundingMode.HALF_UP);
    return leftBd.compareTo(threshold) > 0;
  }

  BigDecimal parseBigDecimalSafe(String value) {
    if (value == null) return null;
    try {
      return new BigDecimal(value.trim());
    } catch (Exception e) {
      return null;
    }
  }

  boolean evaluateModuloZero(Object leftValue, String rawValue) {
    BigDecimal left = toBigDecimal(leftValue);
    BigDecimal divisor = parseBigDecimalSafe(rawValue);
    if (left == null || divisor == null || divisor.compareTo(BigDecimal.ZERO) == 0) return false;
    return left.remainder(divisor).compareTo(BigDecimal.ZERO) == 0;
  }

  boolean evaluateDecimalPlacesGt(Object leftValue, String rawValue) {
    BigDecimal left = toBigDecimal(leftValue);
    if (left == null) return false;
    int threshold;
    try {
      threshold = Integer.parseInt(rawValue.trim());
    } catch (Exception e) {
      return false;
    }
    int scale = left.stripTrailingZeros().scale();
    return scale > threshold;
  }

  boolean betweenNumber(BigDecimal left, String raw, boolean inclusive) {
    if (left == null) return false;
    String[] parts = raw.split(",");
    if (parts.length < 2) return false;
    BigDecimal lo = parseBigDecimalSafe(parts[0]);
    BigDecimal hi = parseBigDecimalSafe(parts[1]);
    if (lo == null || hi == null) return false;
    if (inclusive) {
      return left.compareTo(lo) >= 0 && left.compareTo(hi) <= 0;
    } else {
      return left.compareTo(lo) > 0 && left.compareTo(hi) < 0;
    }
  }

  boolean matchesRegex(String left, String rawRegex) {
    if (!RegexValidator.validate(rawRegex).valid()) return false;
    try {
      return Pattern.compile(rawRegex).matcher(left).matches();
    } catch (Exception e) {
      return false;
    }
  }

  boolean inListNumberFlexible(BigDecimal left, String rawList) {
    for (String token : parseListTokens(rawList)) {
      BigDecimal t = parseBigDecimalSafe(token);
      if (t != null && left.compareTo(t) == 0) return true;
    }
    return false;
  }

  boolean inListStringFlexible(String left, String rawList) {
    for (String token : parseListTokens(rawList)) {
      if (left.equals(token)) return true;
    }
    return false;
  }

  List<String> parseListTokens(String raw) {
    if (raw == null) return List.of();
    String s = raw.trim();
    if (s.startsWith("[") && s.endsWith("]") && s.length() >= 2) {
      s = s.substring(1, s.length() - 1).trim();
    }
    if (s.isEmpty()) return List.of();
    String[] tokens = s.split(",");
    List<String> out = new ArrayList<>(tokens.length);
    for (String t : tokens) {
      String v = t == null ? "" : t.trim();
      if ((v.startsWith("'") && v.endsWith("'")) || (v.startsWith("\"") && v.endsWith("\""))) {
        v = v.substring(1, v.length() - 1);
      }
      out.add(v.trim());
    }
    return out;
  }

  String unquote(String raw) {
    if (raw == null) return "";
    String v = raw.trim();
    if ((v.startsWith("'") && v.endsWith("'")) || (v.startsWith("\"") && v.endsWith("\""))) {
      if (v.length() >= 2) {
        v = v.substring(1, v.length() - 1);
      }
    }
    return v;
  }

  // ========== Leitura de campos ==========

  Object readComputedLeftValue(TransactionRequest request, String fieldExpr) {
    if (fieldExpr == null) return null;
    String raw = fieldExpr.trim();
    if (raw.isEmpty()) return null;

    // ABS(x)
    Matcher unary = UNARY_PATTERN.matcher(raw);
    if (unary.matches()) {
      String fn = unary.group(1);
      String arg = unary.group(2);
      Object base = readFieldValue(request, TransactionRequest.class, arg);
      return applyUnary(fn, base);
    }

    // ABS(<expr>)
    Matcher absExpr = ABS_EXPR_PATTERN.matcher(raw);
    if (absExpr.matches()) {
      Object v = evalNumericExpression(request, absExpr.group(1));
      return applyUnary("ABS", v);
    }

    // ABS_DIFF(a,b)
    Matcher absDiff = ABS_DIFF_PATTERN.matcher(raw);
    if (absDiff.matches()) {
      Object a = readFieldValue(request, TransactionRequest.class, absDiff.group(1));
      Object b = readFieldValue(request, TransactionRequest.class, absDiff.group(2));
      return absDiff(a, b);
    }

    // Expressão numérica simples (a - b)
    if (raw.contains("-") && !raw.startsWith("-")) {
      return evalNumericExpression(request, raw);
    }

    // Campo simples
    return readFieldValue(request, TransactionRequest.class, raw);
  }

  Object applyUnary(String fn, Object v) {
    if (v == null) return null;
    return switch (fn) {
      case "ABS" -> {
        BigDecimal bd = toBigDecimal(v);
        yield bd == null ? null : bd.abs();
      }
      case "LEN" -> String.valueOf(v).length();
      case "LOWER" -> String.valueOf(v).toLowerCase();
      case "UPPER" -> String.valueOf(v).toUpperCase();
      case "TRIM" -> String.valueOf(v).trim();
      default -> v;
    };
  }

  Object absDiff(Object a, Object b) {
    BigDecimal ba = toBigDecimal(a);
    BigDecimal bb = toBigDecimal(b);
    if (ba == null || bb == null) return null;
    return ba.subtract(bb).abs();
  }

  Object evalNumericExpression(TransactionRequest request, String expr) {
    String e = expr.trim();
    int idx = indexOfTopLevelMinus(e);
    if (idx > 0) {
      BigDecimal left = resolveToBigDecimal(request, e.substring(0, idx).trim());
      BigDecimal right = resolveToBigDecimal(request, e.substring(idx + 1).trim());
      if (left != null && right != null) {
        return left.subtract(right);
      }
    }
    return resolveToBigDecimal(request, e);
  }

  int indexOfTopLevelMinus(String e) {
    for (int i = 1; i < e.length(); i++) {
      if (e.charAt(i) == '-') return i;
    }
    return -1;
  }

  BigDecimal resolveToBigDecimal(TransactionRequest request, String token) {
    if (token == null) return null;
    String t = token.trim();
    if (t.isEmpty()) return null;
    try {
      return new BigDecimal(t);
    } catch (Exception e) {
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

  Object readFieldValue(Object subject, Class<?> subjectClass, String fieldName) {
    if (subject == null || fieldName == null) return null;
    try {
      PropertyDescriptor pd = new PropertyDescriptor(fieldName, subjectClass);
      return pd.getReadMethod().invoke(subject);
    } catch (Exception e) {
      return null;
    }
  }
}
