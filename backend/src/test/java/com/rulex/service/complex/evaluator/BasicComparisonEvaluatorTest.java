package com.rulex.service.complex.evaluator;

import static org.assertj.core.api.Assertions.assertThat;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
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

/**
 * Testes unitários para BasicComparisonEvaluator.
 *
 * <p>GAP-2 FIX: Cobertura de testes para evaluators.
 *
 * @version 1.0.0
 */
@DisplayName("BasicComparisonEvaluator Tests")
class BasicComparisonEvaluatorTest {

  private BasicComparisonEvaluator evaluator;

  @BeforeEach
  void setUp() {
    evaluator = new BasicComparisonEvaluator();
  }

  // ========== HELPER METHODS ==========

  private RuleCondition createCondition(
      String fieldName, ConditionOperator operator, String value) {
    RuleCondition condition = new RuleCondition();
    condition.setFieldName(fieldName);
    condition.setOperator(operator);
    condition.setValueSingle(value);
    return condition;
  }

  private RuleCondition createConditionWithArray(
      String fieldName, ConditionOperator operator, List<String> values) {
    RuleCondition condition = new RuleCondition();
    condition.setFieldName(fieldName);
    condition.setOperator(operator);
    condition.setValueArray(values);
    return condition;
  }

  private EvaluationContext createContext(Map<String, Object> payload) {
    return EvaluationContext.builder().payload(payload).build();
  }

  // ========== SUPPORTED OPERATORS ==========

  @Test
  @DisplayName("Deve suportar operadores de comparação básica")
  void shouldSupportBasicComparisonOperators() {
    var supported = evaluator.getSupportedOperators();

    assertThat(supported)
        .contains(
            ConditionOperator.EQ,
            ConditionOperator.NEQ,
            ConditionOperator.GT,
            ConditionOperator.GTE,
            ConditionOperator.LT,
            ConditionOperator.LTE,
            ConditionOperator.IN,
            ConditionOperator.NOT_IN,
            ConditionOperator.BETWEEN,
            ConditionOperator.NOT_BETWEEN,
            ConditionOperator.IS_NULL,
            ConditionOperator.NOT_NULL,
            ConditionOperator.IS_TRUE,
            ConditionOperator.IS_FALSE);
  }

  @Test
  @DisplayName("Deve retornar categoria correta")
  void shouldReturnCorrectCategory() {
    assertThat(evaluator.getCategory()).isEqualTo("BASIC_COMPARISON");
  }

  // ========== EQ OPERATOR ==========

  @Nested
  @DisplayName("Operador EQ")
  class EqOperatorTests {

    @Test
    @DisplayName("EQ deve retornar true para valores iguais (string)")
    void eqShouldReturnTrueForEqualStrings() {
      RuleCondition condition = createCondition("status", ConditionOperator.EQ, "APPROVED");
      EvaluationContext context = createContext(Map.of("status", "APPROVED"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("EQ deve retornar false para valores diferentes")
    void eqShouldReturnFalseForDifferentValues() {
      RuleCondition condition = createCondition("status", ConditionOperator.EQ, "APPROVED");
      EvaluationContext context = createContext(Map.of("status", "DECLINED"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("EQ deve funcionar com números")
    void eqShouldWorkWithNumbers() {
      RuleCondition condition = createCondition("amount", ConditionOperator.EQ, "100");
      EvaluationContext context = createContext(Map.of("amount", 100));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("EQ deve ser case-insensitive para strings")
    void eqShouldBeCaseInsensitive() {
      RuleCondition condition = createCondition("status", ConditionOperator.EQ, "approved");
      EvaluationContext context = createContext(Map.of("status", "APPROVED"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("EQ deve retornar false para campo ausente")
    void eqShouldReturnFalseForMissingField() {
      RuleCondition condition = createCondition("nonExistent", ConditionOperator.EQ, "value");
      EvaluationContext context = createContext(Map.of());

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }
  }

  // ========== NEQ OPERATOR ==========

  @Nested
  @DisplayName("Operador NEQ")
  class NeqOperatorTests {

    @Test
    @DisplayName("NEQ deve retornar true para valores diferentes")
    void neqShouldReturnTrueForDifferentValues() {
      RuleCondition condition = createCondition("status", ConditionOperator.NEQ, "FRAUD");
      EvaluationContext context = createContext(Map.of("status", "APPROVED"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("NEQ deve retornar false para valores iguais")
    void neqShouldReturnFalseForEqualValues() {
      RuleCondition condition = createCondition("status", ConditionOperator.NEQ, "APPROVED");
      EvaluationContext context = createContext(Map.of("status", "APPROVED"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }
  }

  // ========== GT/GTE/LT/LTE OPERATORS ==========

  @Nested
  @DisplayName("Operadores de Comparação Numérica")
  class NumericComparisonTests {

    @ParameterizedTest
    @CsvSource({
      "150, 100, true", // 150 > 100
      "100, 100, false", // 100 > 100
      "50, 100, false" // 50 > 100
    })
    @DisplayName("GT deve comparar corretamente")
    void gtShouldCompareCorrectly(int fieldValue, int threshold, boolean expected) {
      RuleCondition condition =
          createCondition("amount", ConditionOperator.GT, String.valueOf(threshold));
      EvaluationContext context = createContext(Map.of("amount", fieldValue));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isEqualTo(expected);
    }

    @ParameterizedTest
    @CsvSource({
      "150, 100, true", // 150 >= 100
      "100, 100, true", // 100 >= 100
      "50, 100, false" // 50 >= 100
    })
    @DisplayName("GTE deve comparar corretamente")
    void gteShouldCompareCorrectly(int fieldValue, int threshold, boolean expected) {
      RuleCondition condition =
          createCondition("amount", ConditionOperator.GTE, String.valueOf(threshold));
      EvaluationContext context = createContext(Map.of("amount", fieldValue));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isEqualTo(expected);
    }

    @ParameterizedTest
    @CsvSource({
      "50, 100, true", // 50 < 100
      "100, 100, false", // 100 < 100
      "150, 100, false" // 150 < 100
    })
    @DisplayName("LT deve comparar corretamente")
    void ltShouldCompareCorrectly(int fieldValue, int threshold, boolean expected) {
      RuleCondition condition =
          createCondition("amount", ConditionOperator.LT, String.valueOf(threshold));
      EvaluationContext context = createContext(Map.of("amount", fieldValue));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isEqualTo(expected);
    }

    @ParameterizedTest
    @CsvSource({
      "50, 100, true", // 50 <= 100
      "100, 100, true", // 100 <= 100
      "150, 100, false" // 150 <= 100
    })
    @DisplayName("LTE deve comparar corretamente")
    void lteShouldCompareCorrectly(int fieldValue, int threshold, boolean expected) {
      RuleCondition condition =
          createCondition("amount", ConditionOperator.LTE, String.valueOf(threshold));
      EvaluationContext context = createContext(Map.of("amount", fieldValue));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isEqualTo(expected);
    }

    @Test
    @DisplayName("Comparações devem funcionar com BigDecimal")
    void comparisonsShouldWorkWithBigDecimal() {
      RuleCondition condition = createCondition("amount", ConditionOperator.GT, "99.99");
      EvaluationContext context = createContext(Map.of("amount", new BigDecimal("100.00")));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Comparações devem funcionar com strings numéricas")
    void comparisonsShouldWorkWithNumericStrings() {
      RuleCondition condition = createCondition("score", ConditionOperator.GTE, "80");
      EvaluationContext context = createContext(Map.of("score", "85"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }
  }

  // ========== IN/NOT_IN OPERATORS ==========

  @Nested
  @DisplayName("Operadores IN e NOT_IN")
  class InOperatorTests {

    @Test
    @DisplayName("IN deve retornar true quando valor está na lista")
    void inShouldReturnTrueWhenValueInList() {
      RuleCondition condition =
          createConditionWithArray("country", ConditionOperator.IN, List.of("BR", "US", "UK"));
      EvaluationContext context = createContext(Map.of("country", "BR"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("IN deve retornar false quando valor não está na lista")
    void inShouldReturnFalseWhenValueNotInList() {
      RuleCondition condition =
          createConditionWithArray("country", ConditionOperator.IN, List.of("BR", "US", "UK"));
      EvaluationContext context = createContext(Map.of("country", "CN"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("NOT_IN deve retornar true quando valor não está na lista")
    void notInShouldReturnTrueWhenValueNotInList() {
      RuleCondition condition =
          createConditionWithArray("country", ConditionOperator.NOT_IN, List.of("RU", "KP", "IR"));
      EvaluationContext context = createContext(Map.of("country", "BR"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("NOT_IN deve retornar false quando valor está na lista")
    void notInShouldReturnFalseWhenValueInList() {
      RuleCondition condition =
          createConditionWithArray("country", ConditionOperator.NOT_IN, List.of("RU", "KP", "IR"));
      EvaluationContext context = createContext(Map.of("country", "RU"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("IN deve ser case-insensitive")
    void inShouldBeCaseInsensitive() {
      RuleCondition condition =
          createConditionWithArray("status", ConditionOperator.IN, List.of("approved", "pending"));
      EvaluationContext context = createContext(Map.of("status", "APPROVED"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }
  }

  // ========== BETWEEN OPERATOR ==========

  @Nested
  @DisplayName("Operadores BETWEEN e NOT_BETWEEN")
  class BetweenOperatorTests {

    @Test
    @DisplayName("BETWEEN deve retornar true quando valor está no intervalo")
    void betweenShouldReturnTrueWhenValueInRange() {
      RuleCondition condition = createCondition("amount", ConditionOperator.BETWEEN, "100:500");
      EvaluationContext context = createContext(Map.of("amount", 250));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("BETWEEN deve incluir limites")
    void betweenShouldIncludeBoundaries() {
      RuleCondition condition = createCondition("amount", ConditionOperator.BETWEEN, "100:500");

      // Teste limite inferior
      EvaluationContext contextMin = createContext(Map.of("amount", 100));
      assertThat(evaluator.evaluate(condition, contextMin)).isTrue();

      // Teste limite superior
      EvaluationContext contextMax = createContext(Map.of("amount", 500));
      assertThat(evaluator.evaluate(condition, contextMax)).isTrue();
    }

    @Test
    @DisplayName("BETWEEN deve retornar false quando valor está fora do intervalo")
    void betweenShouldReturnFalseWhenValueOutOfRange() {
      RuleCondition condition = createCondition("amount", ConditionOperator.BETWEEN, "100:500");
      EvaluationContext context = createContext(Map.of("amount", 600));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("NOT_BETWEEN deve retornar true quando valor está fora do intervalo")
    void notBetweenShouldReturnTrueWhenValueOutOfRange() {
      RuleCondition condition = createCondition("amount", ConditionOperator.NOT_BETWEEN, "100:500");
      EvaluationContext context = createContext(Map.of("amount", 600));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }
  }

  // ========== NULL OPERATORS ==========

  @Nested
  @DisplayName("Operadores IS_NULL e NOT_NULL")
  class NullOperatorTests {

    @Test
    @DisplayName("IS_NULL deve retornar true para campo nulo")
    void isNullShouldReturnTrueForNullField() {
      Map<String, Object> payload = new HashMap<>();
      payload.put("optionalField", null);
      RuleCondition condition = createCondition("optionalField", ConditionOperator.IS_NULL, null);
      EvaluationContext context = createContext(payload);

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("IS_NULL deve retornar true para campo ausente")
    void isNullShouldReturnTrueForMissingField() {
      RuleCondition condition = createCondition("nonExistent", ConditionOperator.IS_NULL, null);
      EvaluationContext context = createContext(Map.of());

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("IS_NULL deve retornar false para campo com valor")
    void isNullShouldReturnFalseForNonNullField() {
      RuleCondition condition = createCondition("status", ConditionOperator.IS_NULL, null);
      EvaluationContext context = createContext(Map.of("status", "ACTIVE"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("NOT_NULL deve retornar true para campo com valor")
    void notNullShouldReturnTrueForNonNullField() {
      RuleCondition condition = createCondition("status", ConditionOperator.NOT_NULL, null);
      EvaluationContext context = createContext(Map.of("status", "ACTIVE"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("NOT_NULL deve retornar false para campo nulo")
    void notNullShouldReturnFalseForNullField() {
      Map<String, Object> payload = new HashMap<>();
      payload.put("optionalField", null);
      RuleCondition condition = createCondition("optionalField", ConditionOperator.NOT_NULL, null);
      EvaluationContext context = createContext(payload);

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }
  }

  // ========== BOOLEAN OPERATORS ==========

  @Nested
  @DisplayName("Operadores IS_TRUE e IS_FALSE")
  class BooleanOperatorTests {

    @ParameterizedTest
    @ValueSource(strings = {"true", "TRUE", "True", "1", "yes", "YES", "y", "Y"})
    @DisplayName("IS_TRUE deve reconhecer valores truthy")
    void isTrueShouldRecognizeTruthyValues(String value) {
      RuleCondition condition = createCondition("flag", ConditionOperator.IS_TRUE, null);
      EvaluationContext context = createContext(Map.of("flag", value));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("IS_TRUE deve funcionar com Boolean")
    void isTrueShouldWorkWithBoolean() {
      RuleCondition condition = createCondition("flag", ConditionOperator.IS_TRUE, null);
      EvaluationContext context = createContext(Map.of("flag", Boolean.TRUE));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @ParameterizedTest
    @ValueSource(strings = {"false", "FALSE", "False", "0", "no", "NO", "n", "N"})
    @DisplayName("IS_FALSE deve reconhecer valores falsy")
    void isFalseShouldRecognizeFalsyValues(String value) {
      RuleCondition condition = createCondition("flag", ConditionOperator.IS_FALSE, null);
      EvaluationContext context = createContext(Map.of("flag", value));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("IS_FALSE deve funcionar com Boolean")
    void isFalseShouldWorkWithBoolean() {
      RuleCondition condition = createCondition("flag", ConditionOperator.IS_FALSE, null);
      EvaluationContext context = createContext(Map.of("flag", Boolean.FALSE));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }
  }

  // ========== EDGE CASES ==========

  @Nested
  @DisplayName("Casos de Borda")
  class EdgeCaseTests {

    @Test
    @DisplayName("Deve tratar valores vazios graciosamente")
    void shouldHandleEmptyValuesGracefully() {
      RuleCondition condition = createCondition("field", ConditionOperator.EQ, "");
      EvaluationContext context = createContext(Map.of("field", ""));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve tratar payload nulo graciosamente")
    void shouldHandleNullPayloadGracefully() {
      RuleCondition condition = createCondition("field", ConditionOperator.IS_NULL, null);
      EvaluationContext context = EvaluationContext.builder().build();

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve tratar comparação com tipos mistos")
    void shouldHandleMixedTypeComparison() {
      RuleCondition condition = createCondition("amount", ConditionOperator.EQ, "100");
      EvaluationContext context = createContext(Map.of("amount", 100L));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve tratar valores com espaços")
    void shouldHandleValuesWithSpaces() {
      RuleCondition condition = createCondition("status", ConditionOperator.EQ, "  APPROVED  ");
      EvaluationContext context = createContext(Map.of("status", "APPROVED"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }
  }
}
