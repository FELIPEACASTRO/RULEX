package com.rulex.v31.ast;

import com.fasterxml.jackson.databind.JsonNode;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import com.rulex.service.complex.evaluator.OperatorEvaluator;
import com.rulex.service.complex.evaluator.OperatorEvaluatorRegistry;
import com.rulex.util.RegexValidator;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Locale;
import java.util.Objects;

/** Deterministic AST evaluator for V3.1 (safe subset of JSONPath: $.a.b.c). */
public class AstEvaluator {

  private final OperatorEvaluatorRegistry operatorEvaluatorRegistry;

  public AstEvaluator() {
    this.operatorEvaluatorRegistry = null;
  }

  public AstEvaluator(OperatorEvaluatorRegistry operatorEvaluatorRegistry) {
    this.operatorEvaluatorRegistry = operatorEvaluatorRegistry;
  }

  public boolean evaluate(JsonNode ast, JsonNode payload) {
    if (ast == null || payload == null) {
      return false;
    }
    String type = text(ast.get("type"));
    if (type == null) {
      return false;
    }

    return switch (type.toUpperCase(Locale.ROOT)) {
      case "GROUP" -> evalGroup(ast, payload);
      case "CONDITION" -> evalCondition(ast, payload);
      default -> false;
    };
  }

  private boolean evalGroup(JsonNode node, JsonNode payload) {
    String op = text(node.get("op"));
    JsonNode children = node.get("children");
    if (op == null || children == null || !children.isArray()) {
      return false;
    }

    return switch (op.toUpperCase(Locale.ROOT)) {
      case "AND" -> {
        for (JsonNode c : children) {
          if (!evaluate(c, payload)) {
            yield false;
          }
        }
        yield true;
      }
      case "OR" -> {
        for (JsonNode c : children) {
          if (evaluate(c, payload)) {
            yield true;
          }
        }
        yield false;
      }
      case "NOT" -> {
        if (children.size() != 1) {
          yield false;
        }
        yield !evaluate(children.get(0), payload);
      }
      default -> false;
    };
  }

  private boolean evalCondition(JsonNode node, JsonNode payload) {
    JsonNode left = node.get("left");
    String operator = text(node.get("operator"));
    JsonNode right = node.get("right");
    if (left == null || operator == null) {
      return false;
    }

    Object actual = evalExpr(left, payload);
    String op = operator.toUpperCase(Locale.ROOT);

    if (operatorEvaluatorRegistry != null) {
      ConditionOperator conditionOperator = mapToConditionOperator(op);
      if (conditionOperator != null) {
        return evaluateWithRegistry(node, conditionOperator, left, right, payload, actual);
      }
    }

    return switch (op) {
      case "IS_NULL" -> actual == null;
      case "IS_NOT_NULL" -> actual != null;
      case "IS_TRUE" -> Boolean.TRUE.equals(coerceBoolean(actual));
      case "IS_FALSE" -> Boolean.FALSE.equals(coerceBoolean(actual));
      case "EQ" -> Objects.equals(actual, evalRight(right, payload, actual));
      case "NE" -> !Objects.equals(actual, evalRight(right, payload, actual));
      case "GT" -> compare(actual, evalRight(right, payload, actual)) > 0;
      case "GTE" -> compare(actual, evalRight(right, payload, actual)) >= 0;
      case "LT" -> compare(actual, evalRight(right, payload, actual)) < 0;
      case "LTE" -> compare(actual, evalRight(right, payload, actual)) <= 0;
      case "IN" -> in(actual, right, payload, true);
      case "NOT_IN" -> in(actual, right, payload, false);
      case "BETWEEN" -> between(actual, right, payload, true);
      case "NOT_BETWEEN" -> between(actual, right, payload, false);
      case "CONTAINS" -> contains(actual, evalRight(right, payload, actual), true);
      case "NOT_CONTAINS" -> contains(actual, evalRight(right, payload, actual), false);
      case "STARTS_WITH" -> startsWith(actual, evalRight(right, payload, actual));
      case "ENDS_WITH" -> endsWith(actual, evalRight(right, payload, actual));
      case "MATCHES_REGEX" -> matchesRegex(actual, evalRight(right, payload, actual));
      case "BEFORE" -> compareDate(actual, evalRight(right, payload, actual)) < 0;
      case "AFTER" -> compareDate(actual, evalRight(right, payload, actual)) > 0;
      case "ON" -> compareDate(actual, evalRight(right, payload, actual)) == 0;
      case "NOT_ON" -> compareDate(actual, evalRight(right, payload, actual)) != 0;
      default -> false;
    };
  }

  private boolean evaluateWithRegistry(
      JsonNode node,
      ConditionOperator conditionOperator,
      JsonNode left,
      JsonNode right,
      JsonNode payload,
      Object actual) {
    Map<String, Object> payloadMap = toPayloadMap(payload);
    String fieldPath = getFieldJsonPath(left);
    String fieldName = deriveFieldName(fieldPath);
    if (fieldName == null || fieldName.isBlank()) {
      fieldName = "value";
    }

    if (actual != null && payloadMap != null && !payloadMap.containsKey(fieldName)) {
      payloadMap.put(fieldName, actual);
    }

    RuleCondition.RuleConditionBuilder builder =
        RuleCondition.builder()
            .operator(conditionOperator)
            .fieldName(fieldName)
            .fieldPath(fieldPath)
            .enabled(true)
            .caseSensitive(true)
            .negate(false);

    if (right == null || right.isNull()) {
      // no-op for unary operators
    } else if (right.isArray()) {
      List<String> values = new ArrayList<>();
      for (JsonNode item : right) {
        Object v =
            item.isObject() && item.has("type") ? evalExpr(item, payload) : constValue(item);
        values.add(v == null ? null : String.valueOf(v));
      }
      builder.valueArray(values);
    } else if (right.isObject() && !right.has("type") && right.has("min") && right.has("max")) {
      Object min = evalRight(right.get("min"), payload, actual);
      Object max = evalRight(right.get("max"), payload, actual);
      builder.valueMin(min == null ? null : String.valueOf(min));
      builder.valueMax(max == null ? null : String.valueOf(max));
    } else if (right.isObject() && right.has("type")) {
      String rightType = text(right.get("type"));
      if (rightType != null && rightType.equalsIgnoreCase("FIELD")) {
        String refPath = getFieldJsonPath(right);
        String refName = deriveFieldName(refPath);
        builder.valueFieldRef(refName != null ? refName : refPath);
      } else {
        Object v = evalExpr(right, payload);
        builder.valueSingle(v == null ? null : String.valueOf(v));
      }
    } else {
      Object v = evalRight(right, payload, actual);
      builder.valueSingle(v == null ? null : String.valueOf(v));
    }

    RuleCondition condition = builder.build();
    EvaluationContext ctx =
        EvaluationContext.builder()
            .payload(payloadMap)
            .transactionRequest(null)
            .build();

    OperatorEvaluator evaluator = operatorEvaluatorRegistry.getEvaluator(conditionOperator);
    return evaluator.evaluate(condition, ctx);
  }

  private Map<String, Object> toPayloadMap(JsonNode payload) {
    if (payload == null || payload.isNull()) {
      return new HashMap<>();
    }
    Object value = convertJson(payload);
    if (value instanceof Map<?, ?> map) {
      Map<String, Object> result = new HashMap<>();
      for (Map.Entry<?, ?> entry : map.entrySet()) {
        result.put(String.valueOf(entry.getKey()), entry.getValue());
      }
      return result;
    }
    return new HashMap<>();
  }

  private Object convertJson(JsonNode node) {
    if (node == null || node.isNull()) {
      return null;
    }
    if (node.isObject()) {
      Map<String, Object> map = new HashMap<>();
      node.fieldNames()
          .forEachRemaining(
              key -> {
                JsonNode value = node.get(key);
                map.put(key, convertJson(value));
              });
      return map;
    }
    if (node.isArray()) {
      List<Object> list = new ArrayList<>();
      for (JsonNode item : node) {
        list.add(convertJson(item));
      }
      return list;
    }
    return constValue(node);
  }

  private ConditionOperator mapToConditionOperator(String op) {
    if (op == null) {
      return null;
    }
    String mapped =
        switch (op) {
          case "NE" -> "NEQ";
          case "MATCHES_REGEX" -> "REGEX";
          case "NOT_MATCHES_REGEX" -> "NOT_REGEX";
          case "IS_NOT_NULL" -> "NOT_NULL";
          default -> op;
        };
    try {
      return ConditionOperator.valueOf(mapped);
    } catch (IllegalArgumentException e) {
      return null;
    }
  }

  private String getFieldJsonPath(JsonNode expr) {
    if (expr == null || !expr.isObject()) {
      return null;
    }
    String type = text(expr.get("type"));
    if (type == null || !type.equalsIgnoreCase("FIELD")) {
      return null;
    }
    return text(expr.get("jsonPath"));
  }

  private String deriveFieldName(String jsonPath) {
    if (jsonPath == null || !jsonPath.startsWith("$")) {
      return null;
    }
    String trimmed = jsonPath.startsWith("$.") ? jsonPath.substring(2) : jsonPath.substring(1);
    if (trimmed.isBlank()) {
      return null;
    }
    String[] parts = trimmed.split("\\.");
    return parts.length == 0 ? trimmed : parts[parts.length - 1];
  }

  private Object evalRight(JsonNode right, JsonNode payload, Object actual) {
    if (right == null || right.isNull()) {
      return null;
    }
    if (right.isObject() && right.has("type")) {
      return evalExpr(right, payload);
    }
    if (right.isValueNode()) {
      return coerceToActualType(actual, right);
    }
    return right;
  }

  private Object coerceToActualType(Object actual, JsonNode raw) {
    if (raw == null || raw.isNull()) {
      return null;
    }
    if (actual instanceof Integer || actual instanceof Long) {
      if (raw.isNumber()) {
        return raw.asLong();
      }
      if (raw.isTextual()) {
        try {
          return Long.parseLong(raw.asText());
        } catch (Exception e) { // SEC-006 FIX
          return raw.asText();
        }
      }
    }
    if (actual instanceof BigDecimal) {
      if (raw.isNumber()) {
        return raw.decimalValue();
      }
      if (raw.isTextual()) {
        try {
          return new BigDecimal(raw.asText());
        } catch (Exception e) { // SEC-006 FIX
          return raw.asText();
        }
      }
    }
    if (actual instanceof Boolean) {
      return raw.isBoolean() ? raw.asBoolean() : Boolean.parseBoolean(raw.asText());
    }
    if (actual instanceof String) {
      return raw.asText();
    }
    if (raw.isNumber()) {
      return raw.decimalValue();
    }
    if (raw.isBoolean()) {
      return raw.asBoolean();
    }
    if (raw.isTextual()) {
      return raw.asText();
    }
    return raw;
  }

  private boolean in(Object actual, JsonNode right, JsonNode payload, boolean positive) {
    if (actual == null || right == null || right.isNull()) {
      return !positive;
    }

    List<Object> list = new ArrayList<>();
    if (right.isArray()) {
      for (JsonNode item : right) {
        if (item.isObject() && item.has("type")) {
          list.add(evalExpr(item, payload));
        } else {
          list.add(item.isValueNode() ? item.asText() : item.toString());
        }
      }
    } else {
      Object single = evalRight(right, payload, actual);
      list.add(single);
    }

    String actualStr = String.valueOf(actual);
    boolean contains = list.stream().anyMatch(v -> Objects.equals(String.valueOf(v), actualStr));
    return positive == contains;
  }

  private boolean between(Object actual, JsonNode right, JsonNode payload, boolean positive) {
    if (actual == null || right == null || !right.isObject()) {
      return !positive;
    }
    Object min = evalRight(right.get("min"), payload, actual);
    Object max = evalRight(right.get("max"), payload, actual);
    boolean ok = compare(actual, min) >= 0 && compare(actual, max) <= 0;
    return positive == ok;
  }

  private boolean contains(Object actual, Object expected, boolean positive) {
    if (!(actual instanceof String s) || expected == null) {
      return !positive;
    }
    boolean c = s.contains(String.valueOf(expected));
    return positive == c;
  }

  private boolean startsWith(Object actual, Object expected) {
    if (!(actual instanceof String s) || expected == null) {
      return false;
    }
    return s.startsWith(String.valueOf(expected));
  }

  private boolean endsWith(Object actual, Object expected) {
    if (!(actual instanceof String s) || expected == null) {
      return false;
    }
    return s.endsWith(String.valueOf(expected));
  }

  private boolean matchesRegex(Object actual, Object expected) {
    if (!(actual instanceof String s) || expected == null) {
      return false;
    }
    String pattern = String.valueOf(expected);
    // Usa RegexValidator para proteção contra ReDoS
    return RegexValidator.safeFind(pattern, s);
  }

  private int compare(Object a, Object b) {
    if (a == null || b == null) {
      return -1;
    }
    if (a instanceof BigDecimal bd) {
      return bd.compareTo(new BigDecimal(String.valueOf(b)));
    }
    if (a instanceof Integer ai) {
      return Integer.compare(ai, Integer.parseInt(String.valueOf(b)));
    }
    if (a instanceof Long al) {
      return Long.compare(al, Long.parseLong(String.valueOf(b)));
    }
    if (a instanceof String as) {
      return as.compareTo(String.valueOf(b));
    }
    return String.valueOf(a).compareTo(String.valueOf(b));
  }

  private int compareDate(Object a, Object b) {
    LocalDate da = coerceDate(a);
    LocalDate db = coerceDate(b);
    if (da == null || db == null) {
      return -1;
    }
    return da.compareTo(db);
  }

  private LocalDate coerceDate(Object v) {
    if (v == null) {
      return null;
    }
    if (v instanceof Integer i) {
      return yyyymmddToDate(i);
    }
    if (v instanceof Long l) {
      return yyyymmddToDate(l.intValue());
    }
    String s = String.valueOf(v);
    // Pattern simples e seguro - não precisa de timeout
    if (RegexValidator.matchesSimplePattern(s, "\\d{8}")) {
      return yyyymmddToDate(Integer.parseInt(s));
    }
    return null;
  }

  private LocalDate yyyymmddToDate(int yyyymmdd) {
    int y = yyyymmdd / 10000;
    int m = (yyyymmdd / 100) % 100;
    int d = yyyymmdd % 100;
    return LocalDate.of(y, m, d);
  }

  private Object evalExpr(JsonNode expr, JsonNode payload) {
    if (expr == null || !expr.isObject()) {
      return null;
    }

    String type = text(expr.get("type"));
    if (type == null) {
      return null;
    }

    return switch (type.toUpperCase(Locale.ROOT)) {
      case "FIELD" -> resolveField(expr, payload);
      case "CONST" -> constValue(expr.get("value"));
      case "FUNC" -> evalFunc(expr, payload);
      default -> null;
    };
  }

  private Object resolveField(JsonNode fieldNode, JsonNode payload) {
    String jsonPath = text(fieldNode.get("jsonPath"));
    if (jsonPath == null || !jsonPath.startsWith("$.") || payload == null) {
      return null;
    }
    String[] parts = jsonPath.substring(2).split("\\.");
    JsonNode cur = payload;
    for (String p : parts) {
      if (cur == null) {
        return null;
      }
      cur = cur.get(p);
    }
    return constValue(cur);
  }

  private Object evalFunc(JsonNode func, JsonNode payload) {
    String name = text(func.get("name"));
    JsonNode args = func.get("args");
    List<Object> values = new ArrayList<>();
    if (args != null && args.isArray()) {
      for (JsonNode a : args) {
        if (a.isObject() && a.has("type")) {
          values.add(evalExpr(a, payload));
        } else {
          values.add(constValue(a));
        }
      }
    }

    if (name == null) {
      return null;
    }

    return switch (name.toUpperCase(Locale.ROOT)) {
      case "TRIM" -> values.isEmpty() ? null : trim(values.get(0));
      case "LOWER" -> values.isEmpty() ? null : toString(values.get(0)).toLowerCase(Locale.ROOT);
      case "UPPER" -> values.isEmpty() ? null : toString(values.get(0)).toUpperCase(Locale.ROOT);
      case "LEN" -> values.isEmpty() ? null : toString(values.get(0)).length();
      case "ABS" -> values.isEmpty() ? null : abs(values.get(0));
      case "COALESCE" -> values.stream().filter(Objects::nonNull).findFirst().orElse(null);
      case "TO_DATE_YYYYMMDD" -> values.isEmpty() ? null : toYyyymmdd(values.get(0));
      case "TO_TIME_PAD6_HHMMSS" -> values.isEmpty() ? null : toTimePad6(values.get(0));
      case "PARSE_GMTOFFSET" -> values.isEmpty() ? null : parseGmtOffset(values.get(0));
      default -> null;
    };
  }

  private Object constValue(JsonNode node) {
    if (node == null || node.isNull()) {
      return null;
    }
    if (node.isNumber()) {
      return node.decimalValue();
    }
    if (node.isBoolean()) {
      return node.asBoolean();
    }
    if (node.isTextual()) {
      return node.asText();
    }
    if (node.isObject() || node.isArray()) {
      return node;
    }
    return node.asText();
  }

  private Boolean coerceBoolean(Object v) {
    if (v == null) {
      return null;
    }
    if (v instanceof Boolean b) {
      return b;
    }
    return Boolean.parseBoolean(String.valueOf(v));
  }

  private String trim(Object v) {
    if (v == null) {
      return null;
    }
    return String.valueOf(v).trim();
  }

  private String toString(Object v) {
    return v == null ? "" : String.valueOf(v);
  }

  private Object abs(Object v) {
    if (v == null) {
      return null;
    }
    try {
      BigDecimal bd = new BigDecimal(String.valueOf(v));
      return bd.abs();
    } catch (Exception e) {
      return null;
    }
  }

  private Integer toYyyymmdd(Object v) {
    if (v == null) {
      return null;
    }
    String s = String.valueOf(v).replaceAll("[^0-9]", "");
    if (s.length() != 8) {
      return null;
    }
    return Integer.parseInt(s);
  }

  private Integer toTimePad6(Object v) {
    if (v == null) {
      return null;
    }
    String s = String.valueOf(v).replaceAll("[^0-9]", "");
    if (s.length() > 6) {
      return null;
    }
    s = "0".repeat(6 - s.length()) + s;
    return Integer.parseInt(s);
  }

  private String parseGmtOffset(Object v) {
    if (v == null) {
      return null;
    }
    String s = String.valueOf(v).trim();
    // Pattern simples e seguro - não precisa de timeout
    if (!RegexValidator.matchesSimplePattern(s, "[+-]\\d{2}(\\.|:)\\d{2}")) {
      return null;
    }
    return s.replace(':', '.');
  }

  private String text(JsonNode node) {
    if (node == null || node.isNull()) {
      return null;
    }
    return node.asText();
  }
}
