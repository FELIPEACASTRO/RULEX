package com.rulex.v31.ast;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Set;

/** V3.1 AST validator with safety limits (depth/nodes/IN items/regex heuristics). */
public class AstValidator {

  public static final int DEFAULT_MAX_DEPTH = 20;
  public static final int DEFAULT_MAX_NODES = 500;
  public static final int DEFAULT_MAX_IN_ITEMS = 200;
  public static final int DEFAULT_MAX_REGEX_LENGTH = 128;

  private static final Set<String> GROUP_OPS = Set.of("AND", "OR", "NOT");
  private static final Set<String> OPERATORS =
      Set.of(
          "EQ",
          "NE",
          "GT",
          "LT",
          "GTE",
          "LTE",
          "IN",
          "NOT_IN",
          "BETWEEN",
          "NOT_BETWEEN",
          "CONTAINS",
          "NOT_CONTAINS",
          "STARTS_WITH",
          "ENDS_WITH",
          "MATCHES_REGEX",
          "IS_NULL",
          "IS_NOT_NULL",
          "IS_TRUE",
          "IS_FALSE",
          "BEFORE",
          "AFTER",
          "ON",
          "NOT_ON");

  private static final Set<String> FUNC_ALLOWLIST =
      Set.of(
          "TRIM",
          "LOWER",
          "UPPER",
          "LEN",
          "ABS",
          "COALESCE",
          "TO_DATE_YYYYMMDD",
          "TO_TIME_PAD6_HHMMSS",
          "PARSE_GMTOFFSET",
          // Temporal / sequential
          "IS_WEEKEND",
          "IN_TIME_RANGE",
          "SAME_DAY",
          "SAME_HOUR",
          "SAME_WEEK",
          "BUCKET",
          "TIME_SINCE_LAST",
          "IS_HOLIDAY",
          "IS_BUSINESS_DAY",
          // Schema / robustness
          "HAS_FIELD",
          "MISSING_FIELD",
          "UNKNOWN_FIELDS_PRESENT",
          "FIELD_TYPE_MISMATCH",
          "JSON_PATH_EXISTS",
          "JSON_PATH_VALUE",
          // Text
          "NORMALIZE_TEXT",
          "REMOVE_DIACRITICS",
          "STRIP_PUNCTUATION",
          "SIMILARITY_LEVENSHTEIN",
          "SIMILARITY_JARO",
          "ENTROPY",
          "TOKEN_COUNT",
          "KEYWORD_BLACKLIST",
          // Prob / fuzzy
          "CONFIDENCE_GATE",
          "FUZZY_MATCH",
          "FUZZY_SET_MEMBERSHIP");

  private final int maxDepth;
  private final int maxNodes;
  private final int maxInItems;
  private final int maxRegexLength;

  public AstValidator() {
    this(DEFAULT_MAX_DEPTH, DEFAULT_MAX_NODES, DEFAULT_MAX_IN_ITEMS, DEFAULT_MAX_REGEX_LENGTH);
  }

  public AstValidator(int maxDepth, int maxNodes, int maxInItems, int maxRegexLength) {
    this.maxDepth = maxDepth;
    this.maxNodes = maxNodes;
    this.maxInItems = maxInItems;
    this.maxRegexLength = maxRegexLength;
  }

  public AstValidationResult validate(JsonNode root) {
    List<AstValidationError> errors = new ArrayList<>();
    if (root == null || root.isNull()) {
      errors.add(new AstValidationError("$", "AST root obrigatório"));
      return new AstValidationResult(false, errors);
    }

    Counter counter = new Counter();
    validateNode(root, "$", 0, counter, errors);

    return new AstValidationResult(errors.isEmpty(), errors);
  }

  private void validateNode(
      JsonNode node, String path, int depth, Counter counter, List<AstValidationError> errors) {

    if (depth > maxDepth) {
      errors.add(new AstValidationError(path, "AST excede profundidade máxima: " + maxDepth));
      return;
    }

    if (++counter.nodes > maxNodes) {
      errors.add(new AstValidationError(path, "AST excede número máximo de nós: " + maxNodes));
      return;
    }

    if (!node.isObject()) {
      errors.add(new AstValidationError(path, "Nó AST deve ser objeto"));
      return;
    }

    String type = text(node.get("type"));
    if (type == null) {
      errors.add(new AstValidationError(path + ".type", "type obrigatório"));
      return;
    }

    switch (type.toUpperCase(Locale.ROOT)) {
      case "GROUP" -> validateGroup(node, path, depth, counter, errors);
      case "CONDITION" -> validateCondition(node, path, depth, counter, errors);
      case "FIELD" -> validateField(node, path, errors);
      case "FUNC" -> validateFunc(node, path, depth, counter, errors);
      case "CONST" -> validateConst(node, path, errors);
      default -> errors.add(new AstValidationError(path + ".type", "type inválido: " + type));
    }
  }

  private void validateGroup(
      JsonNode node, String path, int depth, Counter counter, List<AstValidationError> errors) {

    String op = text(node.get("op"));
    if (op == null || !GROUP_OPS.contains(op.toUpperCase(Locale.ROOT))) {
      errors.add(new AstValidationError(path + ".op", "op inválido (AND|OR|NOT)"));
      return;
    }

    JsonNode children = node.get("children");
    if (children == null || !children.isArray()) {
      errors.add(new AstValidationError(path + ".children", "children deve ser array"));
      return;
    }

    if (op.equalsIgnoreCase("NOT") && children.size() != 1) {
      errors.add(new AstValidationError(path + ".children", "NOT deve ter exatamente 1 filho"));
    }

    for (int i = 0; i < children.size(); i++) {
      validateNode(children.get(i), path + ".children[" + i + "]", depth + 1, counter, errors);
    }
  }

  private void validateCondition(
      JsonNode node, String path, int depth, Counter counter, List<AstValidationError> errors) {

    JsonNode left = node.get("left");
    if (left == null) {
      errors.add(new AstValidationError(path + ".left", "left obrigatório"));
    } else {
      validateNode(left, path + ".left", depth + 1, counter, errors);
    }

    String operator = text(node.get("operator"));
    if (operator == null || !OPERATORS.contains(operator.toUpperCase(Locale.ROOT))) {
      errors.add(new AstValidationError(path + ".operator", "operator inválido"));
    }

    JsonNode right = node.get("right");
    if (right == null || right.isMissingNode()) {
      // allowed for IS_NULL/IS_NOT_NULL/IS_TRUE/IS_FALSE
      return;
    }

    if (right.isArray()) {
      if (right.size() > maxInItems) {
        errors.add(
            new AstValidationError(path + ".right", "Lista excede maxInItems: " + maxInItems));
      }
      for (int i = 0; i < right.size(); i++) {
        JsonNode item = right.get(i);
        if (item.isObject()) {
          validateNode(item, path + ".right[" + i + "]", depth + 1, counter, errors);
        } else {
          validatePrimitive(item, path + ".right[" + i + "]", errors);
        }
      }
      return;
    }

    if (right.isObject()) {
      // could be CONST/FIELD/FUNC or a range object
      if (right.has("type")) {
        validateNode(right, path + ".right", depth + 1, counter, errors);
      } else {
        // range object: {min:..., max:...}
        if (!right.has("min") || !right.has("max")) {
          errors.add(new AstValidationError(path + ".right", "Range deve ter min e max"));
        }
      }
      return;
    }

    validatePrimitive(right, path + ".right", errors);

    // Regex safety heuristic
    if (operator != null && operator.equalsIgnoreCase("MATCHES_REGEX")) {
      String pattern = right.isTextual() ? right.asText() : null;
      if (pattern == null) {
        errors.add(new AstValidationError(path + ".right", "Regex pattern deve ser string"));
      } else {
        validateRegex(pattern, path + ".right", errors);
      }
    }
  }

  private void validateField(JsonNode node, String path, List<AstValidationError> errors) {
    String jsonPath = text(node.get("jsonPath"));
    if (jsonPath == null || jsonPath.length() > 200) {
      errors.add(new AstValidationError(path + ".jsonPath", "jsonPath inválido"));
    } else if (jsonPath.startsWith("$feature.")) {
      String featureName = jsonPath.substring("$feature.".length()).trim();
      if (!isValidFeatureName(featureName)) {
        errors.add(new AstValidationError(path + ".jsonPath", "feature name inválido"));
      }
    } else if (!jsonPath.startsWith("$.")) {
      errors.add(new AstValidationError(path + ".jsonPath", "jsonPath inválido"));
    }

    String dataType = text(node.get("dataType"));
    if (dataType == null || dataType.isBlank()) {
      errors.add(new AstValidationError(path + ".dataType", "dataType obrigatório"));
    }
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

  private void validateFunc(
      JsonNode node, String path, int depth, Counter counter, List<AstValidationError> errors) {

    String name = text(node.get("name"));
    if (name == null || !FUNC_ALLOWLIST.contains(name.toUpperCase(Locale.ROOT))) {
      errors.add(new AstValidationError(path + ".name", "Função não permitida"));
    }

    JsonNode args = node.get("args");
    if (args != null && args.isArray()) {
      for (int i = 0; i < args.size(); i++) {
        JsonNode a = args.get(i);
        if (a.isObject()) {
          validateNode(a, path + ".args[" + i + "]", depth + 1, counter, errors);
        } else {
          validatePrimitive(a, path + ".args[" + i + "]", errors);
        }
      }
    }

    String returnType = text(node.get("returnType"));
    if (returnType == null || returnType.isBlank()) {
      errors.add(new AstValidationError(path + ".returnType", "returnType obrigatório"));
    }
  }

  private void validateConst(JsonNode node, String path, List<AstValidationError> errors) {
    String dataType = text(node.get("dataType"));
    if (dataType == null || dataType.isBlank()) {
      errors.add(new AstValidationError(path + ".dataType", "dataType obrigatório"));
    }

    JsonNode value = node.get("value");
    if (value == null) {
      errors.add(new AstValidationError(path + ".value", "value obrigatório"));
      return;
    }

    validatePrimitive(value, path + ".value", errors);
  }

  private void validatePrimitive(JsonNode node, String path, List<AstValidationError> errors) {
    if (node == null || node.isNull()) {
      return;
    }
    if (node.isTextual()) {
      String s = node.asText();
      if (containsControlChars(s)) {
        errors.add(new AstValidationError(path, "String contém caracteres de controle"));
      }
    }
  }

  private boolean containsControlChars(String s) {
    for (int i = 0; i < s.length(); i++) {
      char c = s.charAt(i);
      if (c < 0x20 && c != '\t' && c != '\n' && c != '\r') {
        return true;
      }
    }
    return false;
  }

  private void validateRegex(String pattern, String path, List<AstValidationError> errors) {
    if (pattern.length() > maxRegexLength) {
      errors.add(new AstValidationError(path, "Regex muito longa"));
      return;
    }

    // Very small ReDoS heuristic: reject nested quantifiers like (.+)+ or (a*)+
    String p = pattern;
    if (p.matches(".*\\(.*[+*].*\\)[+*].*")) {
      errors.add(
          new AstValidationError(path, "Regex potencialmente vulnerável (nested quantifier)"));
    }

    // Also reject lookbehinds and backreferences (complexity)
    if (p.contains("(?<=") || p.contains("(?<!") || p.matches(".*\\\\[1-9].*")) {
      errors.add(new AstValidationError(path, "Regex não permitida (lookbehind/backreference)"));
    }
  }

  private String text(JsonNode node) {
    if (node == null || node.isNull() || !node.isValueNode()) {
      return null;
    }
    return node.asText();
  }

  private static class Counter {
    int nodes = 0;
  }
}
