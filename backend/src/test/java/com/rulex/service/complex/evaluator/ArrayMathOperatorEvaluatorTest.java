package com.rulex.service.complex.evaluator;

import static org.assertj.core.api.Assertions.assertThat;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

@DisplayName("ArrayMathOperatorEvaluator Tests")
class ArrayMathOperatorEvaluatorTest {

  private ArrayMathOperatorEvaluator evaluator;

  @BeforeEach
  void setUp() {
    evaluator = new ArrayMathOperatorEvaluator();
  }

  private RuleCondition createCondition(String fieldName, ConditionOperator operator, String value) {
    RuleCondition condition = new RuleCondition();
    condition.setFieldName(fieldName);
    condition.setOperator(operator);
    condition.setValueSingle(value);
    return condition;
  }

  private EvaluationContext createContext(Map<String, Object> payload) {
    return EvaluationContext.builder().payload(payload).build();
  }

  @Test
  @DisplayName("deve suportar operadores de array/matemáticos")
  void shouldSupportOperators() {
    assertThat(evaluator.getSupportedOperators())
        .contains(ConditionOperator.ARRAY_CONTAINS, ConditionOperator.MOD_EQ);
  }

  @Test
  @DisplayName("deve retornar categoria correta")
  void shouldReturnCategory() {
    assertThat(evaluator.getCategory()).isEqualTo("ARRAY_MATH");
  }

  @Nested
  @DisplayName("ARRAY_CONTAINS")
  class ArrayContainsTests {

    @Test
    void shouldReturnTrueWhenElementExists() {
      RuleCondition condition = createCondition("tags", ConditionOperator.ARRAY_CONTAINS, "fraud");
      EvaluationContext context = createContext(Map.of("tags", List.of("safe", "fraud")));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }

    @Test
    void shouldReturnFalseWhenElementMissing() {
      RuleCondition condition = createCondition("tags", ConditionOperator.ARRAY_CONTAINS, "fraud");
      EvaluationContext context = createContext(Map.of("tags", List.of("safe")));

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }
  }

  @Nested
  @DisplayName("ARRAY_CONTAINS_ANY")
  class ArrayContainsAnyTests {

    @Test
    void shouldReturnTrueWhenAnyMatches() {
      RuleCondition condition = createCondition("tags", ConditionOperator.ARRAY_CONTAINS_ANY, "fraud,chargeback");
      EvaluationContext context = createContext(Map.of("tags", List.of("safe", "chargeback")));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("ARRAY_SUM_GT")
  class ArraySumTests {

    @Test
    void shouldReturnTrueWhenSumAboveThreshold() {
      RuleCondition condition = createCondition("amounts", ConditionOperator.ARRAY_SUM_GT, "10");
      EvaluationContext context = createContext(Map.of("amounts", List.of(3, 4, 5)));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("ARRAY_SIZE")
  class ArraySizeTests {

    @ParameterizedTest
    @CsvSource({
      "ARRAY_SIZE_EQ,3,true",
      "ARRAY_SIZE_EQ,2,false",
      "ARRAY_SIZE_GT,2,true",
      "ARRAY_SIZE_GT,3,false",
      "ARRAY_SIZE_LT,4,true",
      "ARRAY_SIZE_LT,3,false"
    })
    void shouldEvaluateArraySize(ConditionOperator operator, String expectedSize, boolean expected) {
      RuleCondition condition = createCondition("items", operator, expectedSize);
      EvaluationContext context = createContext(Map.of("items", List.of(1, 2, 3)));

      assertThat(evaluator.evaluate(condition, context)).isEqualTo(expected);
    }
  }

  @Nested
  @DisplayName("MOD_EQ")
  class ModTests {

    @Test
    void shouldReturnTrueWhenModuloMatches() {
      RuleCondition condition = createCondition("value", ConditionOperator.MOD_EQ, "2");
      condition.setValueMin("1");
      EvaluationContext context = createContext(Map.of("value", 5));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }

    @Test
    void shouldReturnFalseWhenModuloDoesNotMatch() {
      RuleCondition condition = createCondition("value", ConditionOperator.MOD_EQ, "2");
      condition.setValueMin("0");
      EvaluationContext context = createContext(Map.of("value", 5));

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }
  }

  @Nested
  @DisplayName("Casos de borda")
  class EdgeCases {

    @Test
    void shouldHandleNullPayload() {
      RuleCondition condition = createCondition("items", ConditionOperator.ARRAY_CONTAINS, "a");
      EvaluationContext context = createContext(null);

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }

    @Test
    void shouldHandleMissingField() {
      RuleCondition condition = createCondition("items", ConditionOperator.ARRAY_CONTAINS, "a");
      EvaluationContext context = createContext(new HashMap<>());

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }
  }
}
