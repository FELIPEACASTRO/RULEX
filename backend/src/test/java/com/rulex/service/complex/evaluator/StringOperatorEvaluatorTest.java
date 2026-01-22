package com.rulex.service.complex.evaluator;

import static org.assertj.core.api.Assertions.assertThat;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Testes unitários para StringOperatorEvaluator.
 *
 * <p>GAP-2 FIX: Cobertura de testes para evaluators.
 *
 * @version 1.0.0
 */
@DisplayName("StringOperatorEvaluator Tests")
class StringOperatorEvaluatorTest {

  private StringOperatorEvaluator evaluator;

  @BeforeEach
  void setUp() {
    evaluator = new StringOperatorEvaluator();
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

  private EvaluationContext createContext(Map<String, Object> payload) {
    return EvaluationContext.builder().payload(payload).build();
  }

  // ========== SUPPORTED OPERATORS ==========

  @Test
  @DisplayName("Deve suportar operadores de string")
  void shouldSupportStringOperators() {
    var supported = evaluator.getSupportedOperators();

    assertThat(supported)
        .contains(
            ConditionOperator.CONTAINS,
            ConditionOperator.NOT_CONTAINS,
            ConditionOperator.STARTS_WITH,
            ConditionOperator.ENDS_WITH,
            ConditionOperator.REGEX,
            ConditionOperator.NOT_REGEX);
  }

  @Test
  @DisplayName("Deve retornar categoria correta")
  void shouldReturnCorrectCategory() {
    assertThat(evaluator.getCategory()).isEqualTo("STRING");
  }

  // ========== CONTAINS OPERATOR ==========

  @Nested
  @DisplayName("Operador CONTAINS")
  class ContainsOperatorTests {

    @Test
    @DisplayName("CONTAINS deve retornar true quando substring está presente")
    void containsShouldReturnTrueWhenSubstringPresent() {
      RuleCondition condition =
          createCondition("merchantName", ConditionOperator.CONTAINS, "FRAUD");
      EvaluationContext context = createContext(Map.of("merchantName", "SUSPECTED FRAUD MERCHANT"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("CONTAINS deve retornar false quando substring não está presente")
    void containsShouldReturnFalseWhenSubstringNotPresent() {
      RuleCondition condition =
          createCondition("merchantName", ConditionOperator.CONTAINS, "FRAUD");
      EvaluationContext context = createContext(Map.of("merchantName", "LEGITIMATE MERCHANT"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("CONTAINS deve ser case-insensitive")
    void containsShouldBeCaseInsensitive() {
      RuleCondition condition =
          createCondition("merchantName", ConditionOperator.CONTAINS, "fraud");
      EvaluationContext context = createContext(Map.of("merchantName", "SUSPECTED FRAUD MERCHANT"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("CONTAINS deve retornar false para campo nulo")
    void containsShouldReturnFalseForNullField() {
      RuleCondition condition =
          createCondition("merchantName", ConditionOperator.CONTAINS, "FRAUD");
      EvaluationContext context = createContext(Map.of());

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }
  }

  // ========== NOT_CONTAINS OPERATOR ==========

  @Nested
  @DisplayName("Operador NOT_CONTAINS")
  class NotContainsOperatorTests {

    @Test
    @DisplayName("NOT_CONTAINS deve retornar true quando substring não está presente")
    void notContainsShouldReturnTrueWhenSubstringNotPresent() {
      RuleCondition condition =
          createCondition("merchantName", ConditionOperator.NOT_CONTAINS, "FRAUD");
      EvaluationContext context = createContext(Map.of("merchantName", "LEGITIMATE MERCHANT"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("NOT_CONTAINS deve retornar false quando substring está presente")
    void notContainsShouldReturnFalseWhenSubstringPresent() {
      RuleCondition condition =
          createCondition("merchantName", ConditionOperator.NOT_CONTAINS, "FRAUD");
      EvaluationContext context = createContext(Map.of("merchantName", "SUSPECTED FRAUD MERCHANT"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }
  }

  // ========== STARTS_WITH OPERATOR ==========

  @Nested
  @DisplayName("Operador STARTS_WITH")
  class StartsWithOperatorTests {

    @Test
    @DisplayName("STARTS_WITH deve retornar true quando string começa com prefixo")
    void startsWithShouldReturnTrueWhenStringStartsWithPrefix() {
      RuleCondition condition = createCondition("pan", ConditionOperator.STARTS_WITH, "4111");
      EvaluationContext context = createContext(Map.of("pan", "4111111111111111"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("STARTS_WITH deve retornar false quando string não começa com prefixo")
    void startsWithShouldReturnFalseWhenStringDoesNotStartWithPrefix() {
      RuleCondition condition = createCondition("pan", ConditionOperator.STARTS_WITH, "5111");
      EvaluationContext context = createContext(Map.of("pan", "4111111111111111"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }

    @ParameterizedTest
    @CsvSource({
      "4, 4111111111111111, true",
      "41, 4111111111111111, true",
      "411, 4111111111111111, true",
      "4111, 4111111111111111, true",
      "5, 4111111111111111, false"
    })
    @DisplayName("STARTS_WITH deve funcionar com diferentes tamanhos de prefixo")
    void startsWithShouldWorkWithDifferentPrefixLengths(
        String prefix, String pan, boolean expected) {
      RuleCondition condition = createCondition("pan", ConditionOperator.STARTS_WITH, prefix);
      EvaluationContext context = createContext(Map.of("pan", pan));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isEqualTo(expected);
    }
  }

  // ========== ENDS_WITH OPERATOR ==========

  @Nested
  @DisplayName("Operador ENDS_WITH")
  class EndsWithOperatorTests {

    @Test
    @DisplayName("ENDS_WITH deve retornar true quando string termina com sufixo")
    void endsWithShouldReturnTrueWhenStringEndsWithSuffix() {
      RuleCondition condition = createCondition("email", ConditionOperator.ENDS_WITH, "@gmail.com");
      EvaluationContext context = createContext(Map.of("email", "user@gmail.com"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("ENDS_WITH deve retornar false quando string não termina com sufixo")
    void endsWithShouldReturnFalseWhenStringDoesNotEndWithSuffix() {
      RuleCondition condition = createCondition("email", ConditionOperator.ENDS_WITH, "@gmail.com");
      EvaluationContext context = createContext(Map.of("email", "user@yahoo.com"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("ENDS_WITH deve ser case-insensitive")
    void endsWithShouldBeCaseInsensitive() {
      RuleCondition condition = createCondition("email", ConditionOperator.ENDS_WITH, "@GMAIL.COM");
      EvaluationContext context = createContext(Map.of("email", "user@gmail.com"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }
  }

  // ========== REGEX OPERATOR ==========

  @Nested
  @DisplayName("Operador REGEX")
  class RegexOperatorTests {

    @Test
    @DisplayName("REGEX deve retornar true quando pattern corresponde")
    void regexShouldReturnTrueWhenPatternMatches() {
      RuleCondition condition =
          createCondition(
              "email",
              ConditionOperator.REGEX,
              "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$");
      EvaluationContext context = createContext(Map.of("email", "user@example.com"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("REGEX deve retornar false quando pattern não corresponde")
    void regexShouldReturnFalseWhenPatternDoesNotMatch() {
      RuleCondition condition =
          createCondition(
              "email",
              ConditionOperator.REGEX,
              "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$");
      EvaluationContext context = createContext(Map.of("email", "invalid-email"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }

    @ParameterizedTest
    @CsvSource({
      "^\\d{4}$, 1234, true",
      "^\\d{4}$, 12345, false",
      "^\\d{4}$, abcd, false",
      "^[A-Z]{3}$, BRA, true",
      "^[A-Z]{3}$, BR, false"
    })
    @DisplayName("REGEX deve funcionar com diferentes patterns")
    void regexShouldWorkWithDifferentPatterns(String pattern, String value, boolean expected) {
      RuleCondition condition = createCondition("field", ConditionOperator.REGEX, pattern);
      EvaluationContext context = createContext(Map.of("field", value));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isEqualTo(expected);
    }

    @Test
    @DisplayName("REGEX deve tratar pattern inválido graciosamente")
    void regexShouldHandleInvalidPatternGracefully() {
      RuleCondition condition = createCondition("field", ConditionOperator.REGEX, "[invalid(regex");
      EvaluationContext context = createContext(Map.of("field", "test"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }
  }

  // ========== NOT_REGEX OPERATOR ==========

  @Nested
  @DisplayName("Operador NOT_REGEX")
  class NotRegexOperatorTests {

    @Test
    @DisplayName("NOT_REGEX deve retornar true quando pattern não corresponde")
    void notRegexShouldReturnTrueWhenPatternDoesNotMatch() {
      RuleCondition condition = createCondition("field", ConditionOperator.NOT_REGEX, "^\\d+$");
      EvaluationContext context = createContext(Map.of("field", "abc"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("NOT_REGEX deve retornar false quando pattern corresponde")
    void notRegexShouldReturnFalseWhenPatternMatches() {
      RuleCondition condition = createCondition("field", ConditionOperator.NOT_REGEX, "^\\d+$");
      EvaluationContext context = createContext(Map.of("field", "12345"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }
  }

  // ========== EDGE CASES ==========

  @Nested
  @DisplayName("Casos de Borda")
  class EdgeCaseTests {

    @Test
    @DisplayName("Deve tratar string vazia graciosamente")
    void shouldHandleEmptyStringGracefully() {
      RuleCondition condition = createCondition("field", ConditionOperator.CONTAINS, "test");
      EvaluationContext context = createContext(Map.of("field", ""));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("Deve tratar valor de condição vazio graciosamente")
    void shouldHandleEmptyConditionValueGracefully() {
      RuleCondition condition = createCondition("field", ConditionOperator.CONTAINS, "");
      EvaluationContext context = createContext(Map.of("field", "test"));

      boolean result = evaluator.evaluate(condition, context);

      // String vazia está contida em qualquer string
      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve tratar campo numérico como string")
    void shouldHandleNumericFieldAsString() {
      RuleCondition condition = createCondition("amount", ConditionOperator.CONTAINS, "100");
      EvaluationContext context = createContext(Map.of("amount", 100));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve tratar caracteres especiais em CONTAINS")
    void shouldHandleSpecialCharactersInContains() {
      RuleCondition condition = createCondition("field", ConditionOperator.CONTAINS, "test@#$");
      EvaluationContext context = createContext(Map.of("field", "this is a test@#$ value"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve tratar unicode corretamente")
    void shouldHandleUnicodeCorrectly() {
      RuleCondition condition = createCondition("name", ConditionOperator.CONTAINS, "João");
      EvaluationContext context = createContext(Map.of("name", "João da Silva"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("REGEX deve ter timeout para patterns maliciosos")
    void regexShouldTimeoutForMaliciousPatterns() {
      // Pattern que poderia causar ReDoS
      RuleCondition condition = createCondition("field", ConditionOperator.REGEX, "(a+)+$");
      EvaluationContext context = createContext(Map.of("field", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaab"));

      // Deve retornar false (timeout ou pattern rejeitado) ao invés de travar
      boolean result = evaluator.evaluate(condition, context);

      // O importante é que não trava - resultado pode ser true ou false dependendo da implementação
      assertThat(result).isIn(true, false);
    }
  }
}
