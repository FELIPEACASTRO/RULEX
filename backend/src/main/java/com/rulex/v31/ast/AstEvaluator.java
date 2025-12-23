package com.rulex.v31.ast;

import com.fasterxml.jackson.databind.JsonNode;
import com.rulex.dto.TransactionRequest;
import com.rulex.util.PanMaskingUtil;
import com.rulex.v31.features.FeatureProvider;
import com.rulex.v31.features.NoOpFeatureProvider;
import com.rulex.v31.trace.FeatureUsageCollector;
import com.rulex.v31.trace.FeatureUsed;
import com.rulex.v31.trace.TracingFeatureProvider;
import java.math.BigDecimal;
import java.text.Normalizer;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.Set;
import java.util.regex.Pattern;

/** Deterministic AST evaluator for V3.1 (safe subset of JSONPath: $.a.b.c). */
public class AstEvaluator {

  private static final Set<String> ALLOWED_ROOT_FIELDS = allowedRootFields();

  private final FeatureProvider featureProvider;

  private final ThreadLocal<FeatureUsageCollector> traceCollector = new ThreadLocal<>();

  public AstEvaluator() {
    this(new NoOpFeatureProvider());
  }

  public AstEvaluator(FeatureProvider featureProvider) {
    FeatureProvider base = featureProvider == null ? new NoOpFeatureProvider() : featureProvider;
    this.featureProvider = new TracingFeatureProvider(base, traceCollector::get);
  }

  /**
   * Evaluates and returns the (deduplicated) list of payload fields/features/functions used.
   *
   * <p>Values are stored as SHA-256 hashes only.
   */
  public EvaluationTraceResult evaluateWithTrace(JsonNode ast, JsonNode payload) {
    FeatureUsageCollector collector = new FeatureUsageCollector();
    traceCollector.set(collector);
    try {
      boolean match = evaluate(ast, payload);
      return new EvaluationTraceResult(match, collector.snapshot());
    } finally {
      traceCollector.remove();
    }
  }

  public record EvaluationTraceResult(boolean match, List<FeatureUsed> featuresUsed) {}

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
        } catch (Exception ignored) {
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
        } catch (Exception ignored) {
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
    return Pattern.compile(pattern).matcher(s).find();
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
    if (v instanceof LocalDate ld) {
      return ld;
    }
    if (v instanceof java.sql.Date sd) {
      return sd.toLocalDate();
    }
    if (v instanceof Integer i) {
      return yyyymmddToDate(i);
    }
    if (v instanceof Long l) {
      return yyyymmddToDate(l.intValue());
    }
    if (v instanceof BigDecimal bd) {
      try {
        return yyyymmddToDate(bd.intValueExact());
      } catch (Exception ignored) {
        return null;
      }
    }
    if (v instanceof Number n) {
      return yyyymmddToDate((int) n.longValue());
    }
    String s = String.valueOf(v);
    if (s.matches("\\d{8}")) {
      return yyyymmddToDate(Integer.parseInt(s));
    }
    if (s.matches("\\d{4}-\\d{2}-\\d{2}")) {
      try {
        return LocalDate.parse(s);
      } catch (Exception ignored) {
        return null;
      }
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
    if (jsonPath == null || payload == null) {
      return null;
    }

    if (jsonPath.startsWith("$feature.")) {
      String featureName = jsonPath.substring("$feature.".length()).trim();
      if (!isValidFeatureName(featureName)) {
        return null;
      }

      String pan = text(payload.get("pan"));
      String entityKey = PanMaskingUtil.mask(pan);
      if (entityKey == null || entityKey.isBlank()) {
        return null;
      }

      // Tracing is handled by TracingFeatureProvider wrapper.
      JsonNode n = featureProvider.getFeature(entityKey, featureName).orElse(null);
      return constValue(n);
    }

    if (!jsonPath.startsWith("$.")) {
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
    Object value = constValue(cur);

    FeatureUsageCollector collector = traceCollector.get();
    if (collector != null) {
      collector.record("payload", jsonPath, null, null, null, value);
    }

    return value;
  }

  private static boolean isValidFeatureName(String featureName) {
    if (featureName == null || featureName.isBlank() || featureName.length() > 100) {
      return false;
    }
    for (int i = 0; i < featureName.length(); i++) {
      char c = featureName.charAt(i);
      boolean ok =
          (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_';
      if (!ok) {
        return false;
      }
    }
    return true;
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

        // =============================
        // Temporal / sequential
        // =============================
      case "IS_WEEKEND" -> isWeekend(values.isEmpty() ? null : values.get(0));
      case "IN_TIME_RANGE" -> inTimeRange(values);
      case "SAME_DAY" -> sameDay(values);
      case "SAME_HOUR" -> sameHour(values);
      case "SAME_WEEK" -> sameWeek(values);
      case "BUCKET" -> bucket(values);
      case "TIME_SINCE_LAST" -> timeSinceLast(values);
      case "IS_HOLIDAY" -> isHoliday(values);
      case "IS_BUSINESS_DAY" -> isBusinessDay(values);

        // =============================
        // Schema / robustness
        // =============================
      case "HAS_FIELD" -> hasField(payload, values);
      case "MISSING_FIELD" -> !hasField(payload, values);
      case "UNKNOWN_FIELDS_PRESENT" -> unknownFieldsPresent(payload);
      case "FIELD_TYPE_MISMATCH" -> fieldTypeMismatch(payload, values);
      case "JSON_PATH_EXISTS" -> hasField(payload, values);
      case "JSON_PATH_VALUE" -> jsonPathValue(payload, values);

        // =============================
        // Text
        // =============================
      case "REMOVE_DIACRITICS" -> removeDiacritics(values.isEmpty() ? null : values.get(0));
      case "STRIP_PUNCTUATION" -> stripPunctuation(values.isEmpty() ? null : values.get(0));
      case "NORMALIZE_TEXT" -> normalizeText(values.isEmpty() ? null : values.get(0));
      case "SIMILARITY_LEVENSHTEIN" -> similarityLevenshtein(values);
      case "SIMILARITY_JARO" -> similarityJaro(values);
      case "ENTROPY" -> entropy(values.isEmpty() ? null : values.get(0));
      case "TOKEN_COUNT" -> tokenCount(values.isEmpty() ? null : values.get(0));
      case "KEYWORD_BLACKLIST" -> keywordBlacklist(values);

        // =============================
        // Prob / fuzzy
        // =============================
      case "CONFIDENCE_GATE" -> confidenceGate(values);
      case "FUZZY_MATCH" -> fuzzyMatch(values);
      case "FUZZY_SET_MEMBERSHIP" -> fuzzySetMembership(values);
      default -> null;
    };
  }

  private boolean isWeekend(Object yyyymmdd) {
    LocalDate d = coerceDate(yyyymmdd);
    if (d == null) {
      return false;
    }
    return switch (d.getDayOfWeek()) {
      case SATURDAY, SUNDAY -> true;
      default -> false;
    };
  }

  private boolean inTimeRange(List<Object> values) {
    // args: time, start, end, timezone(optional)
    if (values.size() < 3) {
      return false;
    }
    Integer t = toTimePad6(values.get(0));
    Integer start = toTimePad6(values.get(1));
    Integer end = toTimePad6(values.get(2));
    if (t == null || start == null || end == null) {
      return false;
    }

    // If start<=end: normal range; else wraps midnight.
    if (start <= end) {
      return t >= start && t <= end;
    }
    return t >= start || t <= end;
  }

  private boolean sameDay(List<Object> values) {
    if (values.size() < 2) {
      return false;
    }
    LocalDate a = coerceDate(values.get(0));
    LocalDate b = coerceDate(values.get(1));
    return a != null && b != null && a.equals(b);
  }

  private boolean sameHour(List<Object> values) {
    // args: timeA, timeB (HHMMSS)
    if (values.size() < 2) {
      return false;
    }
    Integer a = toTimePad6(values.get(0));
    Integer b = toTimePad6(values.get(1));
    if (a == null || b == null) {
      return false;
    }
    return (a / 10000) == (b / 10000);
  }

  private boolean sameWeek(List<Object> values) {
    if (values.size() < 2) {
      return false;
    }
    LocalDate a = coerceDate(values.get(0));
    LocalDate b = coerceDate(values.get(1));
    if (a == null || b == null) {
      return false;
    }
    // Minimal ISO week match: compare Monday-of-week.
    return a.with(java.time.DayOfWeek.MONDAY).equals(b.with(java.time.DayOfWeek.MONDAY));
  }

  private Object bucket(List<Object> values) {
    // args: yyyymmdd, hhmmss, windowName ("5m"|"1h"|"1d")
    if (values.size() < 3) {
      return null;
    }
    LocalDate d = coerceDate(values.get(0));
    Integer t = toTimePad6(values.get(1));
    String window = values.get(2) == null ? null : String.valueOf(values.get(2));
    if (d == null || t == null || window == null) {
      return null;
    }
    int hh = t / 10000;
    int mm = (t / 100) % 100;
    int ss = t % 100;
    var dt = d.atTime(hh, mm, ss).atOffset(ZoneOffset.UTC);

    long bucketSeconds;
    String w = window.trim().toLowerCase(Locale.ROOT);
    bucketSeconds =
        switch (w) {
          case "5m" -> 300;
          case "1h" -> 3600;
          case "1d" -> 86400;
          default -> 0;
        };
    if (bucketSeconds <= 0) {
      return null;
    }
    long epoch = dt.toEpochSecond();
    long bucketEpoch = (epoch / bucketSeconds) * bucketSeconds;
    return OffsetDateTime.ofInstant(java.time.Instant.ofEpochSecond(bucketEpoch), ZoneOffset.UTC)
        .toString();
  }

  private Object timeSinceLast(List<Object> values) {
    // args: eventType, entityKey, currentTs(optional ISO)
    if (values.size() < 2) {
      return null;
    }
    String eventType = values.get(0) == null ? null : String.valueOf(values.get(0));
    String entityKey = values.get(1) == null ? null : String.valueOf(values.get(1));
    if (eventType == null || entityKey == null || eventType.isBlank() || entityKey.isBlank()) {
      return null;
    }
    final OffsetDateTime now = coerceOffsetDateTime(values.size() >= 3 ? values.get(2) : null);

    return featureProvider
        .getLastEventTimestamp(entityKey, eventType)
        .map(ts -> Math.max(0, java.time.Duration.between(ts, now).toSeconds()))
        // If missing, treat as "never seen" (very large)
        .orElse(365L * 24L * 3600L);
  }

  private OffsetDateTime coerceOffsetDateTime(Object v) {
    if (v == null) {
      return OffsetDateTime.now(ZoneOffset.UTC);
    }
    try {
      return OffsetDateTime.parse(String.valueOf(v));
    } catch (Exception ignored) {
      return OffsetDateTime.now(ZoneOffset.UTC);
    }
  }

  private boolean isHoliday(List<Object> values) {
    // args: country, uf(optional), yyyymmdd
    if (values.size() < 2) {
      return false;
    }
    String country = values.get(0) == null ? null : String.valueOf(values.get(0));
    String uf;
    Object dateArg;
    if (values.size() >= 3) {
      uf = values.get(1) == null ? null : String.valueOf(values.get(1));
      dateArg = values.get(2);
    } else {
      uf = null;
      dateArg = values.get(1);
    }
    LocalDate d = coerceDate(dateArg);
    return featureProvider.isHoliday(country, uf, d);
  }

  private boolean isBusinessDay(List<Object> values) {
    if (values.size() < 2) {
      return false;
    }
    Object dateArg = values.size() >= 3 ? values.get(2) : values.get(1);
    LocalDate d = coerceDate(dateArg);
    if (d == null) {
      return false;
    }
    if (isWeekend(d)) {
      return false;
    }
    return !isHoliday(values);
  }

  private boolean hasField(JsonNode payload, List<Object> values) {
    if (payload == null || values.isEmpty()) {
      return false;
    }
    String p = values.get(0) == null ? null : String.valueOf(values.get(0));
    if (p == null || p.isBlank()) {
      return false;
    }

    String jsonPath;
    if (p.startsWith("$.")) {
      jsonPath = p;
    } else {
      jsonPath = "$." + p;
    }
    if (jsonPath.length() > 200) {
      return false;
    }

    JsonNode n = resolveJsonPathNode(payload, jsonPath);
    return n != null && !n.isMissingNode() && !n.isNull();
  }

  private boolean unknownFieldsPresent(JsonNode payload) {
    if (payload == null || !payload.isObject()) {
      return false;
    }
    if (ALLOWED_ROOT_FIELDS.isEmpty()) {
      return false;
    }
    var it = payload.fieldNames();
    while (it.hasNext()) {
      String f = it.next();
      if (!ALLOWED_ROOT_FIELDS.contains(f)) {
        return true;
      }
    }
    return false;
  }

  private boolean fieldTypeMismatch(JsonNode payload, List<Object> values) {
    // args: jsonPath, expectedType
    if (payload == null || values.size() < 2) {
      return false;
    }
    String jsonPath = values.get(0) == null ? null : String.valueOf(values.get(0));
    String expected = values.get(1) == null ? null : String.valueOf(values.get(1));
    if (jsonPath == null || expected == null) {
      return false;
    }
    JsonNode n = resolveJsonPathNode(payload, jsonPath);
    if (n == null || n.isMissingNode() || n.isNull()) {
      return false;
    }
    String e = expected.trim().toLowerCase(Locale.ROOT);
    return switch (e) {
      case "string" -> !n.isTextual();
      case "number", "integer" -> !n.isNumber();
      case "boolean" -> !n.isBoolean();
      case "object" -> !n.isObject();
      case "array" -> !n.isArray();
      default -> false;
    };
  }

  private Object jsonPathValue(JsonNode payload, List<Object> values) {
    if (payload == null || values.isEmpty()) {
      return null;
    }
    String jsonPath = values.get(0) == null ? null : String.valueOf(values.get(0));
    JsonNode n = resolveJsonPathNode(payload, jsonPath);
    return constValue(n);
  }

  private JsonNode resolveJsonPathNode(JsonNode payload, String jsonPath) {
    if (payload == null || jsonPath == null || !jsonPath.startsWith("$.")) {
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
    return cur;
  }

  private String removeDiacritics(Object v) {
    if (v == null) {
      return null;
    }
    String s = String.valueOf(v);
    String normalized = Normalizer.normalize(s, Normalizer.Form.NFD);
    return normalized.replaceAll("\\p{M}", "");
  }

  private String stripPunctuation(Object v) {
    if (v == null) {
      return null;
    }
    return String.valueOf(v).replaceAll("[^\\p{L}\\p{N}\\s]", " ");
  }

  private String normalizeText(Object v) {
    if (v == null) {
      return null;
    }
    String s = String.valueOf(v);
    s = trim(s);
    s = removeDiacritics(s);
    s = stripPunctuation(s);
    s = s.toLowerCase(Locale.ROOT);
    s = s.replaceAll("\\s+", " ").trim();
    return s;
  }

  private Object similarityLevenshtein(List<Object> values) {
    if (values.size() < 2) {
      return null;
    }
    String a = normalizeText(values.get(0));
    String b = normalizeText(values.get(1));
    if (a == null || b == null) {
      return null;
    }
    int dist = levenshtein(a, b);
    int max = Math.max(a.length(), b.length());
    if (max == 0) {
      return BigDecimal.ONE;
    }
    double sim = 1.0 - ((double) dist / (double) max);
    return BigDecimal.valueOf(Math.max(0.0, Math.min(1.0, sim)));
  }

  private Object similarityJaro(List<Object> values) {
    if (values.size() < 2) {
      return null;
    }
    String a = normalizeText(values.get(0));
    String b = normalizeText(values.get(1));
    if (a == null || b == null) {
      return null;
    }
    return BigDecimal.valueOf(jaroWinkler(a, b));
  }

  private Object entropy(Object v) {
    if (v == null) {
      return null;
    }
    String s = String.valueOf(v);
    if (s.isEmpty()) {
      return BigDecimal.ZERO;
    }
    int[] freq = new int[256];
    byte[] bytes = s.getBytes(java.nio.charset.StandardCharsets.UTF_8);
    for (byte b : bytes) {
      freq[b & 0xff]++;
    }
    double ent = 0.0;
    for (int f : freq) {
      if (f == 0) continue;
      double p = (double) f / (double) bytes.length;
      ent += -p * (Math.log(p) / Math.log(2));
    }
    return BigDecimal.valueOf(ent);
  }

  private Object tokenCount(Object v) {
    if (v == null) {
      return 0;
    }
    String s = normalizeText(v);
    if (s == null || s.isBlank()) {
      return 0;
    }
    return s.split("\\s+").length;
  }

  private boolean keywordBlacklist(List<Object> values) {
    // args: text, keywords(array|string)
    if (values.size() < 2) {
      return false;
    }
    String text = normalizeText(values.get(0));
    if (text == null) {
      return false;
    }
    Object kw = values.get(1);
    List<String> keywords = new ArrayList<>();
    if (kw instanceof JsonNode jn && jn.isArray()) {
      for (JsonNode item : jn) {
        if (item != null && item.isTextual()) {
          keywords.add(normalizeText(item.asText()));
        }
      }
    } else {
      keywords.add(normalizeText(kw));
    }
    for (String k : keywords) {
      if (k != null && !k.isBlank() && text.contains(k)) {
        return true;
      }
    }
    return false;
  }

  private boolean confidenceGate(List<Object> values) {
    // args: confidence, threshold
    if (values.size() < 2) {
      return false;
    }
    try {
      BigDecimal conf = new BigDecimal(String.valueOf(values.get(0)));
      BigDecimal thr = new BigDecimal(String.valueOf(values.get(1)));
      return conf.compareTo(thr) >= 0;
    } catch (Exception ignored) {
      return false;
    }
  }

  private boolean fuzzyMatch(List<Object> values) {
    // args: text, value, threshold
    if (values.size() < 3) {
      return false;
    }
    String a = normalizeText(values.get(0));
    String b = normalizeText(values.get(1));
    if (a == null || b == null) {
      return false;
    }
    BigDecimal thr;
    try {
      thr = new BigDecimal(String.valueOf(values.get(2)));
    } catch (Exception ignored) {
      return false;
    }
    BigDecimal sim = (BigDecimal) similarityJaro(List.of(a, b));
    return sim != null && sim.compareTo(thr) >= 0;
  }

  private Object fuzzySetMembership(List<Object> values) {
    // args: setName
    if (values.isEmpty() || values.get(0) == null) {
      return null;
    }
    String setName = String.valueOf(values.get(0));
    if (setName.isBlank()) {
      return null;
    }
    // Convention: entity_key = "__FUZZYSET__" and feature_name = <setName>
    return featureProvider
        .getFeature("__FUZZYSET__", setName)
        .map(n -> n.has("membership") ? n.get("membership") : n)
        .orElse(null);
  }

  private static Set<String> allowedRootFields() {
    Set<String> s = new HashSet<>();
    try {
      for (var f : TransactionRequest.class.getDeclaredFields()) {
        var jp = f.getAnnotation(com.fasterxml.jackson.annotation.JsonProperty.class);
        if (jp != null && jp.value() != null && !jp.value().isBlank()) {
          s.add(jp.value().trim());
        }
      }
    } catch (Exception ignored) {
      // If reflection fails, keep empty set (unknownFieldsPresent becomes conservative=false).
    }
    return Set.copyOf(s);
  }

  private int levenshtein(String a, String b) {
    int n = a.length();
    int m = b.length();
    if (n == 0) return m;
    if (m == 0) return n;
    int[] prev = new int[m + 1];
    int[] cur = new int[m + 1];
    for (int j = 0; j <= m; j++) prev[j] = j;
    for (int i = 1; i <= n; i++) {
      cur[0] = i;
      char ca = a.charAt(i - 1);
      for (int j = 1; j <= m; j++) {
        int cost = (ca == b.charAt(j - 1)) ? 0 : 1;
        cur[j] = Math.min(Math.min(cur[j - 1] + 1, prev[j] + 1), prev[j - 1] + cost);
      }
      int[] tmp = prev;
      prev = cur;
      cur = tmp;
    }
    return prev[m];
  }

  private double jaroWinkler(String s1, String s2) {
    double jaro = jaro(s1, s2);
    int prefix = 0;
    for (int i = 0; i < Math.min(4, Math.min(s1.length(), s2.length())); i++) {
      if (s1.charAt(i) == s2.charAt(i)) prefix++;
      else break;
    }
    return jaro + 0.1 * prefix * (1 - jaro);
  }

  private double jaro(String s1, String s2) {
    if (s1.equals(s2)) return 1.0;
    int len1 = s1.length();
    int len2 = s2.length();
    if (len1 == 0 || len2 == 0) return 0.0;
    int matchDistance = Math.max(len1, len2) / 2 - 1;
    boolean[] s1Matches = new boolean[len1];
    boolean[] s2Matches = new boolean[len2];
    int matches = 0;
    for (int i = 0; i < len1; i++) {
      int start = Math.max(0, i - matchDistance);
      int end = Math.min(i + matchDistance + 1, len2);
      for (int j = start; j < end; j++) {
        if (s2Matches[j]) continue;
        if (s1.charAt(i) != s2.charAt(j)) continue;
        s1Matches[i] = true;
        s2Matches[j] = true;
        matches++;
        break;
      }
    }
    if (matches == 0) return 0.0;
    double t = 0;
    int k = 0;
    for (int i = 0; i < len1; i++) {
      if (!s1Matches[i]) continue;
      while (!s2Matches[k]) k++;
      if (s1.charAt(i) != s2.charAt(k)) t++;
      k++;
    }
    t /= 2.0;
    return ((matches / (double) len1) + (matches / (double) len2) + ((matches - t) / matches))
        / 3.0;
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
    if (!s.matches("[+-]\\d{2}(\\.|:)\\d{2}")) {
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
