package com.rulex.domain.service;

import static org.assertj.core.api.Assertions.assertThat;

import com.rulex.domain.model.Rule;
import com.rulex.domain.model.RuleCondition;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

@DisplayName("ConditionEvaluator - Domain Service")
class ConditionEvaluatorTest {

  private ConditionEvaluator evaluator;

  @BeforeEach
  void setUp() {
    evaluator = new ConditionEvaluator();
  }

  @Nested
  @DisplayName("Equality Operators")
  class EqualityOperators {

    @Test
    @DisplayName("EQ: should return true when values are equal (case-insensitive)")
    void testEquals_CaseInsensitive() {
      RuleCondition condition = createCondition("country", "EQ", "BRAZIL");
      Map<String, Object> payload = Map.of("country", "brazil");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("EQ: should return false when values differ")
    void testEquals_Different() {
      RuleCondition condition = createCondition("country", "EQUALS", "USA");
      Map<String, Object> payload = Map.of("country", "BRAZIL");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("NEQ: should return true when values differ")
    void testNotEquals_Different() {
      RuleCondition condition = createCondition("status", "NEQ", "BLOCKED");
      Map<String, Object> payload = Map.of("status", "ACTIVE");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("NEQ: should return false when values are equal")
    void testNotEquals_Equal() {
      RuleCondition condition = createCondition("status", "NOT_EQUALS", "ACTIVE");
      Map<String, Object> payload = Map.of("status", "active");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isFalse();
    }
  }

  @Nested
  @DisplayName("Numeric Comparison Operators")
  class NumericComparison {

    @ParameterizedTest
    @CsvSource({
      "100, 50, true",
      "100, 100, false",
      "100, 150, false",
      "50.5, 50.4, true",
      "50.5, 50.6, false"
    })
    @DisplayName("GT: should compare numbers correctly")
    void testGreaterThan(String fieldValue, String expectedValue, boolean expected) {
      RuleCondition condition = createCondition("amount", "GT", expectedValue);
      Map<String, Object> payload = Map.of("amount", new BigDecimal(fieldValue));

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isEqualTo(expected);
    }

    @ParameterizedTest
    @CsvSource({
      "100, 50, true",
      "100, 100, true",
      "100, 150, false",
      "50.5, 50.5, true"
    })
    @DisplayName("GTE: should compare numbers with >= logic")
    void testGreaterThanOrEqual(String fieldValue, String expectedValue, boolean expected) {
      RuleCondition condition = createCondition("amount", "GTE", expectedValue);
      Map<String, Object> payload = Map.of("amount", new BigDecimal(fieldValue));

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isEqualTo(expected);
    }

    @ParameterizedTest
    @CsvSource({"50, 100, true", "100, 100, false", "150, 100, false"})
    @DisplayName("LT: should compare numbers with < logic")
    void testLessThan(String fieldValue, String expectedValue, boolean expected) {
      RuleCondition condition = createCondition("amount", "LT", expectedValue);
      Map<String, Object> payload = Map.of("amount", new BigDecimal(fieldValue));

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isEqualTo(expected);
    }

    @ParameterizedTest
    @CsvSource({"50, 100, true", "100, 100, true", "150, 100, false"})
    @DisplayName("LTE: should compare numbers with <= logic")
    void testLessThanOrEqual(String fieldValue, String expectedValue, boolean expected) {
      RuleCondition condition = createCondition("amount", "LTE", expectedValue);
      Map<String, Object> payload = Map.of("amount", new BigDecimal(fieldValue));

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isEqualTo(expected);
    }

    @Test
    @DisplayName("GT: should return false when field value is null")
    void testGreaterThan_NullFieldValue() {
      RuleCondition condition = createCondition("amount", "GT", "100");
      Map<String, Object> payload = Map.of("other", "value");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("GT: should return false when expected value is not numeric")
    void testGreaterThan_InvalidExpectedValue() {
      RuleCondition condition = createCondition("amount", "GT", "invalid");
      Map<String, Object> payload = Map.of("amount", "100");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("GT: should handle Integer, Long, Double types")
    void testGreaterThan_DifferentNumericTypes() {
      RuleCondition condition = createCondition("amount", "GT", "50");

      assertThat(evaluator.evaluate(condition, Map.of("amount", 100))).isTrue();
      assertThat(evaluator.evaluate(condition, Map.of("amount", 100L))).isTrue();
      assertThat(evaluator.evaluate(condition, Map.of("amount", 100.5))).isTrue();
      assertThat(evaluator.evaluate(condition, Map.of("amount", "100"))).isTrue();
    }
  }

  @Nested
  @DisplayName("Range Operators")
  class RangeOperators {

    @Test
    @DisplayName("BETWEEN: should return true when value is within range (inclusive)")
    void testBetween_InRange() {
      RuleCondition condition = createCondition("amount", "BETWEEN", "100,200");
      Map<String, Object> payload = Map.of("amount", new BigDecimal("150"));

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("BETWEEN: should return true when value equals min boundary")
    void testBetween_MinBoundary() {
      RuleCondition condition = createCondition("amount", "BETWEEN", "100,200");
      Map<String, Object> payload = Map.of("amount", new BigDecimal("100"));

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("BETWEEN: should return true when value equals max boundary")
    void testBetween_MaxBoundary() {
      RuleCondition condition = createCondition("amount", "BETWEEN", "100,200");
      Map<String, Object> payload = Map.of("amount", new BigDecimal("200"));

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("BETWEEN: should return false when value is below range")
    void testBetween_BelowRange() {
      RuleCondition condition = createCondition("amount", "BETWEEN", "100,200");
      Map<String, Object> payload = Map.of("amount", new BigDecimal("50"));

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("BETWEEN: should return false when value is above range")
    void testBetween_AboveRange() {
      RuleCondition condition = createCondition("amount", "BETWEEN", "100,200");
      Map<String, Object> payload = Map.of("amount", new BigDecimal("250"));

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("BETWEEN: should support pipe separator")
    void testBetween_PipeSeparator() {
      RuleCondition condition = createCondition("amount", "BETWEEN", "100|200");
      Map<String, Object> payload = Map.of("amount", new BigDecimal("150"));

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("BETWEEN: should return false with invalid format")
    void testBetween_InvalidFormat() {
      RuleCondition condition = createCondition("amount", "BETWEEN", "100");
      Map<String, Object> payload = Map.of("amount", new BigDecimal("150"));

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("NOT_BETWEEN: should return true when value is outside range")
    void testNotBetween_OutsideRange() {
      RuleCondition condition = createCondition("amount", "NOT_BETWEEN", "100,200");
      Map<String, Object> payload = Map.of("amount", new BigDecimal("250"));

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("NOT_BETWEEN: should return false when value is within range")
    void testNotBetween_InRange() {
      RuleCondition condition = createCondition("amount", "NOT_BETWEEN", "100,200");
      Map<String, Object> payload = Map.of("amount", new BigDecimal("150"));

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isFalse();
    }
  }

  @Nested
  @DisplayName("String Operators")
  class StringOperators {

    @Test
    @DisplayName("CONTAINS: should return true when substring exists (case-insensitive)")
    void testContains_Found() {
      RuleCondition condition = createCondition("description", "CONTAINS", "fraud");
      Map<String, Object> payload = Map.of("description", "Suspected FRAUD transaction");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("CONTAINS: should return false when substring not found")
    void testContains_NotFound() {
      RuleCondition condition = createCondition("description", "CONTAINS", "fraud");
      Map<String, Object> payload = Map.of("description", "Normal transaction");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("NOT_CONTAINS: should return true when substring not found")
    void testNotContains_NotFound() {
      RuleCondition condition = createCondition("description", "NOT_CONTAINS", "test");
      Map<String, Object> payload = Map.of("description", "Production data");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("STARTS_WITH: should return true when string starts with prefix")
    void testStartsWith_Match() {
      RuleCondition condition = createCondition("email", "STARTS_WITH", "admin");
      Map<String, Object> payload = Map.of("email", "admin@example.com");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("STARTS_WITH: should be case-insensitive")
    void testStartsWith_CaseInsensitive() {
      RuleCondition condition = createCondition("email", "STARTS_WITH", "ADMIN");
      Map<String, Object> payload = Map.of("email", "admin@example.com");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("ENDS_WITH: should return true when string ends with suffix")
    void testEndsWith_Match() {
      RuleCondition condition = createCondition("filename", "ENDS_WITH", ".pdf");
      Map<String, Object> payload = Map.of("filename", "document.PDF");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("ENDS_WITH: should return false when suffix not found")
    void testEndsWith_NoMatch() {
      RuleCondition condition = createCondition("filename", "ENDS_WITH", ".pdf");
      Map<String, Object> payload = Map.of("filename", "document.txt");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("REGEX: should match valid regex pattern")
    void testRegex_Match() {
      RuleCondition condition = createCondition("email", "REGEX", "^[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,}$");
      Map<String, Object> payload = Map.of("email", "user@example.com");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("REGEX: should return false when pattern does not match")
    void testRegex_NoMatch() {
      RuleCondition condition = createCondition("email", "REGEX", "^[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,}$");
      Map<String, Object> payload = Map.of("email", "invalid-email");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("REGEX: should return false with invalid regex")
    void testRegex_InvalidPattern() {
      RuleCondition condition = createCondition("field", "MATCHES", "[invalid(regex");
      Map<String, Object> payload = Map.of("field", "value");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("REGEX: should be case-insensitive")
    void testRegex_CaseInsensitive() {
      RuleCondition condition = createCondition("code", "REGEX", "^[A-Z]{3}$");
      Map<String, Object> payload = Map.of("code", "abc");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isTrue();
    }
  }

  @Nested
  @DisplayName("List Operators")
  class ListOperators {

    @Test
    @DisplayName("IN: should return true when value is in comma-separated list")
    void testIn_FoundComma() {
      RuleCondition condition = createCondition("country", "IN", "USA,BRAZIL,CANADA");
      Map<String, Object> payload = Map.of("country", "BRAZIL");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("IN: should return true when value is in pipe-separated list")
    void testIn_FoundPipe() {
      RuleCondition condition = createCondition("country", "IN", "USA|BRAZIL|CANADA");
      Map<String, Object> payload = Map.of("country", "brazil");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("IN: should return false when value is not in list")
    void testIn_NotFound() {
      RuleCondition condition = createCondition("country", "IN", "USA,BRAZIL,CANADA");
      Map<String, Object> payload = Map.of("country", "UK");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("IN: should be case-insensitive")
    void testIn_CaseInsensitive() {
      RuleCondition condition = createCondition("status", "IN", "ACTIVE,PENDING,BLOCKED");
      Map<String, Object> payload = Map.of("status", "active");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("IN: should trim whitespace")
    void testIn_Whitespace() {
      RuleCondition condition = createCondition("status", "IN", " ACTIVE , PENDING , BLOCKED ");
      Map<String, Object> payload = Map.of("status", "PENDING");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("NOT_IN: should return true when value is not in list")
    void testNotIn_NotFound() {
      RuleCondition condition = createCondition("country", "NOT_IN", "USA,BRAZIL");
      Map<String, Object> payload = Map.of("country", "UK");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("NOT_IN: should return false when value is in list")
    void testNotIn_Found() {
      RuleCondition condition = createCondition("country", "NOT_IN", "USA,BRAZIL");
      Map<String, Object> payload = Map.of("country", "USA");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isFalse();
    }
  }

  @Nested
  @DisplayName("Null Operators")
  class NullOperators {

    @Test
    @DisplayName("IS_NULL: should return true when field is null")
    void testIsNull_Null() {
      RuleCondition condition = createCondition("optionalField", "IS_NULL", null);
      Map<String, Object> payload = new HashMap<>();
      payload.put("optionalField", null);

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("IS_NULL: should return true when field is missing")
    void testIsNull_Missing() {
      RuleCondition condition = createCondition("missingField", "IS_NULL", null);
      Map<String, Object> payload = Map.of("otherField", "value");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("IS_NULL: should return false when field has value")
    void testIsNull_NotNull() {
      RuleCondition condition = createCondition("field", "IS_NULL", null);
      Map<String, Object> payload = Map.of("field", "value");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("IS_NOT_NULL: should return true when field has value")
    void testIsNotNull_NotNull() {
      RuleCondition condition = createCondition("field", "IS_NOT_NULL", null);
      Map<String, Object> payload = Map.of("field", "value");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("IS_NOT_NULL: should return false when field is null")
    void testIsNotNull_Null() {
      RuleCondition condition = createCondition("field", "NOT_NULL", null);
      Map<String, Object> payload = new HashMap<>();
      payload.put("field", null);

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isFalse();
    }
  }

  @Nested
  @DisplayName("Boolean Operators")
  class BooleanOperators {

    @Test
    @DisplayName("IS_TRUE: should return true for Boolean true")
    void testIsTrue_BooleanTrue() {
      RuleCondition condition = createCondition("active", "IS_TRUE", null);
      Map<String, Object> payload = Map.of("active", true);

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isTrue();
    }

    @ParameterizedTest
    @ValueSource(strings = {"true", "TRUE", "1", "y", "Y", "yes", "YES"})
    @DisplayName("IS_TRUE: should return true for truthy string values")
    void testIsTrue_TruthyStrings(String value) {
      RuleCondition condition = createCondition("active", "IS_TRUE", null);
      Map<String, Object> payload = Map.of("active", value);

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("IS_TRUE: should return false for Boolean false")
    void testIsTrue_BooleanFalse() {
      RuleCondition condition = createCondition("active", "IS_TRUE", null);
      Map<String, Object> payload = Map.of("active", false);

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isFalse();
    }

    @ParameterizedTest
    @ValueSource(strings = {"false", "0", "n", "no", "random"})
    @DisplayName("IS_TRUE: should return false for non-truthy values")
    void testIsTrue_FalsyValues(String value) {
      RuleCondition condition = createCondition("active", "IS_TRUE", null);
      Map<String, Object> payload = Map.of("active", value);

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("IS_FALSE: should return true for Boolean false")
    void testIsFalse_BooleanFalse() {
      RuleCondition condition = createCondition("active", "IS_FALSE", null);
      Map<String, Object> payload = Map.of("active", false);

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("IS_FALSE: should return false for Boolean true")
    void testIsFalse_BooleanTrue() {
      RuleCondition condition = createCondition("active", "IS_FALSE", null);
      Map<String, Object> payload = Map.of("active", true);

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isFalse();
    }
  }

  @Nested
  @DisplayName("Nested Field Access")
  class NestedFields {

    @Test
    @DisplayName("Should extract nested field value with dot notation")
    void testNestedField_ThreeLevels() {
      RuleCondition condition = createCondition("customer.address.city", "EQ", "Sao Paulo");
      Map<String, Object> payload =
          Map.of(
              "customer",
              Map.of("address", Map.of("city", "Sao Paulo", "country", "Brazil"), "name", "John"));

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Should return false when nested field does not exist")
    void testNestedField_Missing() {
      RuleCondition condition = createCondition("customer.address.city", "EQ", "Sao Paulo");
      Map<String, Object> payload = Map.of("customer", Map.of("name", "John"));

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("Should return false when intermediate field is not a map")
    void testNestedField_NotMap() {
      RuleCondition condition = createCondition("customer.address.city", "EQ", "Sao Paulo");
      Map<String, Object> payload = Map.of("customer", "not a map");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isFalse();
    }
  }

  @Nested
  @DisplayName("Multiple Conditions - Logic Operators")
  class MultipleConditions {

    @Test
    @DisplayName("evaluateAll AND: should return true when all conditions match")
    void testEvaluateAll_And_AllMatch() {
      List<RuleCondition> conditions =
          List.of(
              createCondition("amount", "GT", "100"),
              createCondition("country", "EQ", "BRAZIL"),
              createCondition("status", "EQ", "ACTIVE"));

      Map<String, Object> payload =
          Map.of(
              "amount", new BigDecimal("150"),
              "country", "BRAZIL",
              "status", "ACTIVE");

      boolean result = evaluator.evaluateAll(conditions, Rule.LogicOperator.AND, payload);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("evaluateAll AND: should return false when one condition fails")
    void testEvaluateAll_And_OneFailure() {
      List<RuleCondition> conditions =
          List.of(
              createCondition("amount", "GT", "100"),
              createCondition("country", "EQ", "USA"), // This will fail
              createCondition("status", "EQ", "ACTIVE"));

      Map<String, Object> payload =
          Map.of(
              "amount", new BigDecimal("150"),
              "country", "BRAZIL",
              "status", "ACTIVE");

      boolean result = evaluator.evaluateAll(conditions, Rule.LogicOperator.AND, payload);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("evaluateAll OR: should return true when at least one condition matches")
    void testEvaluateAll_Or_OneMatch() {
      List<RuleCondition> conditions =
          List.of(
              createCondition("amount", "LT", "100"), // Fails
              createCondition("country", "EQ", "BRAZIL"), // Passes
              createCondition("status", "EQ", "BLOCKED")); // Fails

      Map<String, Object> payload =
          Map.of(
              "amount", new BigDecimal("150"),
              "country", "BRAZIL",
              "status", "ACTIVE");

      boolean result = evaluator.evaluateAll(conditions, Rule.LogicOperator.OR, payload);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("evaluateAll OR: should return false when all conditions fail")
    void testEvaluateAll_Or_AllFail() {
      List<RuleCondition> conditions =
          List.of(
              createCondition("amount", "LT", "100"),
              createCondition("country", "EQ", "USA"),
              createCondition("status", "EQ", "BLOCKED"));

      Map<String, Object> payload =
          Map.of(
              "amount", new BigDecimal("150"),
              "country", "BRAZIL",
              "status", "ACTIVE");

      boolean result = evaluator.evaluateAll(conditions, Rule.LogicOperator.OR, payload);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("evaluateAll: should return false with empty condition list")
    void testEvaluateAll_EmptyList() {
      boolean result = evaluator.evaluateAll(List.of(), Rule.LogicOperator.AND, Map.of());

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("evaluateAll: should return false with null conditions")
    void testEvaluateAll_NullConditions() {
      boolean result = evaluator.evaluateAll(null, Rule.LogicOperator.AND, Map.of());

      assertThat(result).isFalse();
    }
  }

  @Nested
  @DisplayName("Evaluation with Explanation")
  class EvaluationExplanation {

    @Test
    @DisplayName("evaluateWithExplanation: should return result with explanation")
    void testEvaluateWithExplanation_Success() {
      RuleCondition condition = createCondition("amount", "GT", "100");
      Map<String, Object> payload = Map.of("amount", new BigDecimal("150"));

      ConditionEvaluator.EvaluationResult result =
          evaluator.evaluateWithExplanation(condition, payload);

      assertThat(result.triggered()).isTrue();
      assertThat(result.explanation())
          .contains("amount")
          .contains("GT")
          .contains("100")
          .contains("150")
          .contains("TRUE");
    }

    @Test
    @DisplayName("evaluateWithExplanation: should include FALSE in explanation when fails")
    void testEvaluateWithExplanation_Failure() {
      RuleCondition condition = createCondition("amount", "GT", "100");
      Map<String, Object> payload = Map.of("amount", new BigDecimal("50"));

      ConditionEvaluator.EvaluationResult result =
          evaluator.evaluateWithExplanation(condition, payload);

      assertThat(result.triggered()).isFalse();
      assertThat(result.explanation())
          .contains("amount")
          .contains("GT")
          .contains("100")
          .contains("50")
          .contains("FALSE");
    }
  }

  @Nested
  @DisplayName("Edge Cases and Error Handling")
  class EdgeCases {

    @Test
    @DisplayName("evaluate: should return false when condition is null")
    void testEvaluate_NullCondition() {
      boolean result = evaluator.evaluate(null, Map.of("field", "value"));

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("evaluate: should return false when payload is null")
    void testEvaluate_NullPayload() {
      RuleCondition condition = createCondition("field", "EQ", "value");

      boolean result = evaluator.evaluate(condition, null);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("evaluate: should return false with unknown operator")
    void testEvaluate_UnknownOperator() {
      RuleCondition condition = createCondition("field", "INVALID_OPERATOR", "value");
      Map<String, Object> payload = Map.of("field", "value");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("evaluate: should return false when operator is null")
    void testEvaluate_NullOperator() {
      RuleCondition condition = new RuleCondition("field", null, "value");

      boolean result = evaluator.evaluate(condition, Map.of("field", "value"));

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("CONTAINS: should return false when field value is null")
    void testContains_NullFieldValue() {
      RuleCondition condition = createCondition("field", "CONTAINS", "value");
      Map<String, Object> payload = Map.of("other", "data");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("CONTAINS: should return false when expected value is null")
    void testContains_NullExpectedValue() {
      RuleCondition condition = createCondition("field", "CONTAINS", null);
      Map<String, Object> payload = Map.of("field", "value");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("IN: should return false when field value is null")
    void testIn_NullFieldValue() {
      RuleCondition condition = createCondition("field", "IN", "A,B,C");
      Map<String, Object> payload = Map.of("other", "value");

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("IS_TRUE: should return false when field is null")
    void testIsTrue_NullValue() {
      RuleCondition condition = createCondition("field", "IS_TRUE", null);
      Map<String, Object> payload = new HashMap<>();
      payload.put("field", null);

      boolean result = evaluator.evaluate(condition, payload);

      assertThat(result).isFalse();
    }
  }

  // ========== Helper Methods ==========

  private RuleCondition createCondition(String field, String operator, String value) {
    return new RuleCondition(field, operator, value);
  }
}
