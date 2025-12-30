package com.rulex.service.complex;

import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Avaliador de expressões matemáticas e condicionais. Suporta operações aritméticas, funções
 * matemáticas, condicionais e funções de data.
 */
@Component
@Slf4j
public class ExpressionEvaluator {

  private static final MathContext MATH_CONTEXT = new MathContext(10, RoundingMode.HALF_UP);

  // Padrões para parsing
  private static final Pattern FUNCTION_PATTERN = Pattern.compile("(\\w+)\\s*\\(([^)]+)\\)");
  private static final Pattern VARIABLE_PATTERN =
      Pattern.compile("\\$\\{(\\w+(?:\\.\\w+)*)}|\\b([a-zA-Z_][a-zA-Z0-9_]*)\\b");
  private static final Pattern NUMBER_PATTERN = Pattern.compile("-?\\d+(\\.\\d+)?");

  /** Avalia uma expressão no contexto fornecido */
  public Object evaluate(String expression, Map<String, Object> context) {
    if (expression == null || expression.trim().isEmpty()) {
      return null;
    }

    try {
      // Substituir variáveis
      String resolved = resolveVariables(expression, context);

      // Avaliar funções
      resolved = evaluateFunctions(resolved, context);

      // Avaliar expressão aritmética
      return evaluateArithmetic(resolved);

    } catch (Exception e) {
      log.warn("Erro ao avaliar expressão '{}': {}", expression, e.getMessage());
      return null;
    }
  }

  /** Resolve variáveis na expressão */
  @SuppressWarnings("unchecked")
  private String resolveVariables(String expression, Map<String, Object> context) {
    Matcher matcher = VARIABLE_PATTERN.matcher(expression);
    StringBuffer result = new StringBuffer();

    while (matcher.find()) {
      String varName = matcher.group(1) != null ? matcher.group(1) : matcher.group(2);

      // Ignorar palavras-chave e funções
      if (isKeyword(varName) || isFunction(varName)) {
        continue;
      }

      Object value = resolveNestedValue(varName, context);
      String replacement = value != null ? String.valueOf(value) : "0";
      matcher.appendReplacement(result, Matcher.quoteReplacement(replacement));
    }
    matcher.appendTail(result);

    return result.toString();
  }

  /** Resolve valor aninhado (ex: customer.address.city) */
  @SuppressWarnings("unchecked")
  private Object resolveNestedValue(String path, Map<String, Object> context) {
    String[] parts = path.split("\\.");
    Object current = context;

    for (String part : parts) {
      if (current == null) return null;
      if (current instanceof Map) {
        current = ((Map<String, Object>) current).get(part);
      } else {
        return null;
      }
    }
    return current;
  }

  /** Avalia funções na expressão */
  private String evaluateFunctions(String expression, Map<String, Object> context) {
    Matcher matcher = FUNCTION_PATTERN.matcher(expression);
    StringBuffer result = new StringBuffer();

    while (matcher.find()) {
      String funcName = matcher.group(1).toUpperCase();
      String args = matcher.group(2);

      Object funcResult = evaluateFunction(funcName, args, context);
      String replacement = funcResult != null ? String.valueOf(funcResult) : "0";
      matcher.appendReplacement(result, Matcher.quoteReplacement(replacement));
    }
    matcher.appendTail(result);

    return result.toString();
  }

  /** Avalia uma função específica */
  private Object evaluateFunction(String funcName, String args, Map<String, Object> context) {
    String[] argList = args.split(",");

    return switch (funcName) {
        // Funções matemáticas
      case "ABS" -> evaluateAbs(argList);
      case "ROUND" -> evaluateRound(argList);
      case "FLOOR" -> evaluateFloor(argList);
      case "CEIL", "CEILING" -> evaluateCeil(argList);
      case "MIN" -> evaluateMin(argList);
      case "MAX" -> evaluateMax(argList);
      case "SQRT" -> evaluateSqrt(argList);
      case "POW", "POWER" -> evaluatePow(argList);
      case "MOD" -> evaluateMod(argList);

        // Funções de string
      case "LEN", "LENGTH" -> evaluateLength(argList);
      case "UPPER" -> evaluateUpper(argList);
      case "LOWER" -> evaluateLower(argList);
      case "TRIM" -> evaluateTrim(argList);
      case "SUBSTRING", "SUBSTR" -> evaluateSubstring(argList);

        // Funções de data
      case "NOW" -> LocalDateTime.now().toString();
      case "TODAY" -> LocalDate.now().toString();
      case "DAYS_BETWEEN" -> evaluateDaysBetween(argList);
      case "HOURS_BETWEEN" -> evaluateHoursBetween(argList);

        // Funções condicionais
      case "IF" -> evaluateIf(argList, context);
      case "COALESCE", "IFNULL" -> evaluateCoalesce(argList);

        // Funções de agregação (simplificadas)
      case "SUM" -> evaluateSum(argList, context);
      case "COUNT" -> evaluateCount(argList, context);
      case "AVG" -> evaluateAvg(argList, context);

      default -> {
        log.warn("Função desconhecida: {}", funcName);
        yield null;
      }
    };
  }

  /** Avalia expressão aritmética simples */
  private Object evaluateArithmetic(String expression) {
    // Remover espaços
    expression = expression.replaceAll("\\s+", "");

    // Se é apenas um número, retornar
    if (NUMBER_PATTERN.matcher(expression).matches()) {
      return new BigDecimal(expression);
    }

    // Avaliar operações na ordem correta
    try {
      return evaluateAddSub(expression);
    } catch (Exception e) {
      // Se não é uma expressão aritmética válida, retornar como string
      return expression;
    }
  }

  private BigDecimal evaluateAddSub(String expr) {
    // Encontrar + ou - fora de parênteses
    int parenDepth = 0;
    for (int i = expr.length() - 1; i >= 0; i--) {
      char c = expr.charAt(i);
      if (c == ')') parenDepth++;
      else if (c == '(') parenDepth--;
      else if (parenDepth == 0 && (c == '+' || c == '-') && i > 0) {
        BigDecimal left = evaluateAddSub(expr.substring(0, i));
        BigDecimal right = evaluateMulDiv(expr.substring(i + 1));
        return c == '+' ? left.add(right) : left.subtract(right);
      }
    }
    return evaluateMulDiv(expr);
  }

  private BigDecimal evaluateMulDiv(String expr) {
    int parenDepth = 0;
    for (int i = expr.length() - 1; i >= 0; i--) {
      char c = expr.charAt(i);
      if (c == ')') parenDepth++;
      else if (c == '(') parenDepth--;
      else if (parenDepth == 0 && (c == '*' || c == '/')) {
        BigDecimal left = evaluateMulDiv(expr.substring(0, i));
        BigDecimal right = evaluatePrimary(expr.substring(i + 1));
        return c == '*' ? left.multiply(right) : left.divide(right, MATH_CONTEXT);
      }
    }
    return evaluatePrimary(expr);
  }

  private BigDecimal evaluatePrimary(String expr) {
    expr = expr.trim();

    // Parênteses
    if (expr.startsWith("(") && expr.endsWith(")")) {
      return evaluateAddSub(expr.substring(1, expr.length() - 1));
    }

    // Número
    return new BigDecimal(expr);
  }

  // ========== Implementações de funções ==========

  private BigDecimal evaluateAbs(String[] args) {
    return new BigDecimal(args[0].trim()).abs();
  }

  private BigDecimal evaluateRound(String[] args) {
    BigDecimal value = new BigDecimal(args[0].trim());
    int scale = args.length > 1 ? Integer.parseInt(args[1].trim()) : 0;
    return value.setScale(scale, RoundingMode.HALF_UP);
  }

  private BigDecimal evaluateFloor(String[] args) {
    return new BigDecimal(args[0].trim()).setScale(0, RoundingMode.FLOOR);
  }

  private BigDecimal evaluateCeil(String[] args) {
    return new BigDecimal(args[0].trim()).setScale(0, RoundingMode.CEILING);
  }

  private BigDecimal evaluateMin(String[] args) {
    BigDecimal min = new BigDecimal(args[0].trim());
    for (int i = 1; i < args.length; i++) {
      BigDecimal val = new BigDecimal(args[i].trim());
      if (val.compareTo(min) < 0) min = val;
    }
    return min;
  }

  private BigDecimal evaluateMax(String[] args) {
    BigDecimal max = new BigDecimal(args[0].trim());
    for (int i = 1; i < args.length; i++) {
      BigDecimal val = new BigDecimal(args[i].trim());
      if (val.compareTo(max) > 0) max = val;
    }
    return max;
  }

  private BigDecimal evaluateSqrt(String[] args) {
    double value = Double.parseDouble(args[0].trim());
    return BigDecimal.valueOf(Math.sqrt(value));
  }

  private BigDecimal evaluatePow(String[] args) {
    double base = Double.parseDouble(args[0].trim());
    double exp = Double.parseDouble(args[1].trim());
    return BigDecimal.valueOf(Math.pow(base, exp));
  }

  private BigDecimal evaluateMod(String[] args) {
    BigDecimal dividend = new BigDecimal(args[0].trim());
    BigDecimal divisor = new BigDecimal(args[1].trim());
    return dividend.remainder(divisor);
  }

  private Integer evaluateLength(String[] args) {
    return args[0].trim().replace("'", "").replace("\"", "").length();
  }

  private String evaluateUpper(String[] args) {
    return args[0].trim().replace("'", "").replace("\"", "").toUpperCase();
  }

  private String evaluateLower(String[] args) {
    return args[0].trim().replace("'", "").replace("\"", "").toLowerCase();
  }

  private String evaluateTrim(String[] args) {
    return args[0].trim().replace("'", "").replace("\"", "").trim();
  }

  private String evaluateSubstring(String[] args) {
    String str = args[0].trim().replace("'", "").replace("\"", "");
    int start = Integer.parseInt(args[1].trim());
    int end = args.length > 2 ? Integer.parseInt(args[2].trim()) : str.length();
    return str.substring(start, Math.min(end, str.length()));
  }

  private Long evaluateDaysBetween(String[] args) {
    try {
      LocalDate date1 = LocalDate.parse(args[0].trim().replace("'", ""));
      LocalDate date2 =
          args.length > 1 ? LocalDate.parse(args[1].trim().replace("'", "")) : LocalDate.now();
      return ChronoUnit.DAYS.between(date1, date2);
    } catch (Exception e) {
      return 0L;
    }
  }

  private Long evaluateHoursBetween(String[] args) {
    try {
      LocalDateTime dt1 = LocalDateTime.parse(args[0].trim().replace("'", ""));
      LocalDateTime dt2 =
          args.length > 1
              ? LocalDateTime.parse(args[1].trim().replace("'", ""))
              : LocalDateTime.now();
      return ChronoUnit.HOURS.between(dt1, dt2);
    } catch (Exception e) {
      return 0L;
    }
  }

  private Object evaluateIf(String[] args, Map<String, Object> context) {
    if (args.length < 3) return null;

    String condition = args[0].trim();
    String trueValue = args[1].trim();
    String falseValue = args[2].trim();

    // Avaliar condição simples
    boolean result = evaluateSimpleCondition(condition, context);
    return result ? parseValue(trueValue) : parseValue(falseValue);
  }

  private boolean evaluateSimpleCondition(String condition, Map<String, Object> context) {
    // Suporta: >, <, >=, <=, ==, !=
    String[] operators = {">=", "<=", "!=", "==", ">", "<"};

    for (String op : operators) {
      if (condition.contains(op)) {
        String[] parts = condition.split(Pattern.quote(op));
        if (parts.length == 2) {
          BigDecimal left = new BigDecimal(parts[0].trim());
          BigDecimal right = new BigDecimal(parts[1].trim());
          int cmp = left.compareTo(right);

          return switch (op) {
            case ">" -> cmp > 0;
            case "<" -> cmp < 0;
            case ">=" -> cmp >= 0;
            case "<=" -> cmp <= 0;
            case "==" -> cmp == 0;
            case "!=" -> cmp != 0;
            default -> false;
          };
        }
      }
    }
    return false;
  }

  private Object evaluateCoalesce(String[] args) {
    for (String arg : args) {
      String trimmed = arg.trim();
      if (!trimmed.isEmpty() && !"null".equalsIgnoreCase(trimmed)) {
        return parseValue(trimmed);
      }
    }
    return null;
  }

  @SuppressWarnings("unchecked")
  private BigDecimal evaluateSum(String[] args, Map<String, Object> context) {
    String fieldPath = args[0].trim();
    Object value = resolveNestedValue(fieldPath, context);

    if (value instanceof List) {
      return ((List<Object>) value)
          .stream()
              .map(v -> new BigDecimal(String.valueOf(v)))
              .reduce(BigDecimal.ZERO, BigDecimal::add);
    }
    return BigDecimal.ZERO;
  }

  @SuppressWarnings("unchecked")
  private Integer evaluateCount(String[] args, Map<String, Object> context) {
    String fieldPath = args[0].trim();
    Object value = resolveNestedValue(fieldPath, context);

    if (value instanceof List) {
      return ((List<Object>) value).size();
    }
    return 0;
  }

  @SuppressWarnings("unchecked")
  private BigDecimal evaluateAvg(String[] args, Map<String, Object> context) {
    String fieldPath = args[0].trim();
    Object value = resolveNestedValue(fieldPath, context);

    if (value instanceof List) {
      List<Object> list = (List<Object>) value;
      if (list.isEmpty()) return BigDecimal.ZERO;

      BigDecimal sum =
          list.stream()
              .map(v -> new BigDecimal(String.valueOf(v)))
              .reduce(BigDecimal.ZERO, BigDecimal::add);
      return sum.divide(BigDecimal.valueOf(list.size()), MATH_CONTEXT);
    }
    return BigDecimal.ZERO;
  }

  private Object parseValue(String value) {
    value = value.trim().replace("'", "").replace("\"", "");

    // Tentar como número
    try {
      return new BigDecimal(value);
    } catch (NumberFormatException e) {
      // Retornar como string
      return value;
    }
  }

  private boolean isKeyword(String word) {
    return List.of("AND", "OR", "NOT", "TRUE", "FALSE", "NULL").contains(word.toUpperCase());
  }

  private boolean isFunction(String word) {
    return List.of(
            "ABS",
            "ROUND",
            "FLOOR",
            "CEIL",
            "CEILING",
            "MIN",
            "MAX",
            "SQRT",
            "POW",
            "POWER",
            "MOD",
            "LEN",
            "LENGTH",
            "UPPER",
            "LOWER",
            "TRIM",
            "SUBSTRING",
            "SUBSTR",
            "NOW",
            "TODAY",
            "DAYS_BETWEEN",
            "HOURS_BETWEEN",
            "IF",
            "COALESCE",
            "IFNULL",
            "SUM",
            "COUNT",
            "AVG")
        .contains(word.toUpperCase());
  }
}
