package com.rulex.service.complex.evaluator.util;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.exception.RuleEvaluationException;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Testes unitários para EvaluatorUtils.
 *
 * <p>GAP-7 FIX: Testes para classe utilitária centralizada.
 *
 * @version 1.0.0
 */
@DisplayName("EvaluatorUtils Tests")
class EvaluatorUtilsTest {

  // ========== HELPER METHODS ==========

  private EvaluationContext createContext(Map<String, Object> payload) {
    return EvaluationContext.builder().payload(payload).build();
  }

  // ========== PARSING TESTS ==========

  @Nested
  @DisplayName("Parsing de Long")
  class ParseLongTests {

    @Test
    @DisplayName("Deve parsear Long válido")
    void shouldParseValidLong() {
      assertThat(EvaluatorUtils.parseLongSafe("12345", 0)).isEqualTo(12345L);
    }

    @Test
    @DisplayName("Deve retornar default para valor inválido")
    void shouldReturnDefaultForInvalidValue() {
      assertThat(EvaluatorUtils.parseLongSafe("invalid", 99)).isEqualTo(99L);
    }

    @ParameterizedTest
    @NullAndEmptySource
    @DisplayName("Deve retornar default para null ou vazio")
    void shouldReturnDefaultForNullOrEmpty(String value) {
      assertThat(EvaluatorUtils.parseLongSafe(value, 42)).isEqualTo(42L);
    }

    @Test
    @DisplayName("Deve tratar espaços em branco")
    void shouldHandleWhitespace() {
      assertThat(EvaluatorUtils.parseLongSafe("  123  ", 0)).isEqualTo(123L);
    }
  }

  @Nested
  @DisplayName("Parsing de Integer")
  class ParseIntTests {

    @Test
    @DisplayName("Deve parsear Integer válido")
    void shouldParseValidInt() {
      assertThat(EvaluatorUtils.parseIntSafe("42", 0)).isEqualTo(42);
    }

    @Test
    @DisplayName("Deve retornar default para valor inválido")
    void shouldReturnDefaultForInvalidValue() {
      assertThat(EvaluatorUtils.parseIntSafe("not-a-number", 10)).isEqualTo(10);
    }
  }

  @Nested
  @DisplayName("Parsing de Double")
  class ParseDoubleTests {

    @Test
    @DisplayName("Deve parsear Double válido")
    void shouldParseValidDouble() {
      assertThat(EvaluatorUtils.parseDoubleSafe("3.14", 0)).isEqualTo(3.14);
    }

    @Test
    @DisplayName("Deve parsear inteiro como Double")
    void shouldParseIntegerAsDouble() {
      assertThat(EvaluatorUtils.parseDoubleSafe("42", 0)).isEqualTo(42.0);
    }

    @Test
    @DisplayName("Deve retornar default para valor inválido")
    void shouldReturnDefaultForInvalidValue() {
      assertThat(EvaluatorUtils.parseDoubleSafe("abc", 1.5)).isEqualTo(1.5);
    }
  }

  @Nested
  @DisplayName("Parsing de BigDecimal")
  class ParseBigDecimalTests {

    @Test
    @DisplayName("Deve parsear BigDecimal válido")
    void shouldParseValidBigDecimal() {
      assertThat(EvaluatorUtils.parseBigDecimalSafe("123.45", BigDecimal.ZERO))
          .isEqualByComparingTo("123.45");
    }

    @Test
    @DisplayName("Deve converter Object para BigDecimal")
    void shouldConvertObjectToBigDecimal() {
      assertThat(EvaluatorUtils.toBigDecimalSafe(100, BigDecimal.ZERO)).isEqualByComparingTo("100");
      assertThat(EvaluatorUtils.toBigDecimalSafe(100.5, BigDecimal.ZERO))
          .isEqualByComparingTo("100.5");
      assertThat(EvaluatorUtils.toBigDecimalSafe(new BigDecimal("99.99"), BigDecimal.ZERO))
          .isEqualByComparingTo("99.99");
    }
  }

  // ========== FIELD VALUE EXTRACTION ==========

  @Nested
  @DisplayName("Extração de Valores")
  class FieldValueExtractionTests {

    @Test
    @DisplayName("Deve extrair valor do payload")
    void shouldExtractValueFromPayload() {
      EvaluationContext context = createContext(Map.of("status", "APPROVED"));

      Optional<Object> value = EvaluatorUtils.getFieldValue("status", context);

      assertThat(value).isPresent().contains("APPROVED");
    }

    @Test
    @DisplayName("Deve retornar empty para campo ausente")
    void shouldReturnEmptyForMissingField() {
      EvaluationContext context = createContext(Map.of());

      Optional<Object> value = EvaluatorUtils.getFieldValue("nonExistent", context);

      assertThat(value).isEmpty();
    }

    @Test
    @DisplayName("Deve extrair String com default")
    void shouldExtractStringWithDefault() {
      EvaluationContext context = createContext(Map.of("name", "John"));

      assertThat(EvaluatorUtils.getStringValue("name", context, "default")).isEqualTo("John");
      assertThat(EvaluatorUtils.getStringValue("missing", context, "default")).isEqualTo("default");
    }

    @Test
    @DisplayName("Deve extrair Long com default")
    void shouldExtractLongWithDefault() {
      EvaluationContext context = createContext(Map.of("count", 42L));

      assertThat(EvaluatorUtils.getLongValue("count", context, 0)).isEqualTo(42L);
      assertThat(EvaluatorUtils.getLongValue("missing", context, 99)).isEqualTo(99L);
    }

    @Test
    @DisplayName("Deve extrair Boolean com default")
    void shouldExtractBooleanWithDefault() {
      EvaluationContext context = createContext(Map.of("flag", true));

      assertThat(EvaluatorUtils.getBooleanValue("flag", context, false)).isTrue();
      assertThat(EvaluatorUtils.getBooleanValue("missing", context, false)).isFalse();
    }
  }

  // ========== TYPE CONVERSION ==========

  @Nested
  @DisplayName("Conversão de Tipos")
  class TypeConversionTests {

    @ParameterizedTest
    @ValueSource(strings = {"true", "TRUE", "True", "1", "yes", "YES", "y", "Y", "sim"})
    @DisplayName("toBoolean deve reconhecer valores truthy")
    void toBooleanShouldRecognizeTruthyValues(String value) {
      assertThat(EvaluatorUtils.toBoolean(value)).isTrue();
    }

    @ParameterizedTest
    @ValueSource(strings = {"false", "FALSE", "0", "no", "n", "random"})
    @DisplayName("toBoolean deve reconhecer valores falsy")
    void toBooleanShouldRecognizeFalsyValues(String value) {
      assertThat(EvaluatorUtils.toBoolean(value)).isFalse();
    }

    @Test
    @DisplayName("toBoolean deve tratar Boolean diretamente")
    void toBooleanShouldHandleBooleanDirectly() {
      assertThat(EvaluatorUtils.toBoolean(Boolean.TRUE)).isTrue();
      assertThat(EvaluatorUtils.toBoolean(Boolean.FALSE)).isFalse();
    }

    @Test
    @DisplayName("toBoolean deve retornar false para null")
    void toBooleanShouldReturnFalseForNull() {
      assertThat(EvaluatorUtils.toBoolean(null)).isFalse();
    }

    @Test
    @DisplayName("toLong deve converter diferentes tipos")
    void toLongShouldConvertDifferentTypes() {
      assertThat(EvaluatorUtils.toLong(42, 0)).isEqualTo(42L);
      assertThat(EvaluatorUtils.toLong(42L, 0)).isEqualTo(42L);
      assertThat(EvaluatorUtils.toLong(42.9, 0)).isEqualTo(42L);
      assertThat(EvaluatorUtils.toLong("42", 0)).isEqualTo(42L);
      assertThat(EvaluatorUtils.toLong(null, 99)).isEqualTo(99L);
    }

    @Test
    @DisplayName("toString deve converter com default")
    void toStringShouldConvertWithDefault() {
      assertThat(EvaluatorUtils.toString(42, "default")).isEqualTo("42");
      assertThat(EvaluatorUtils.toString(null, "default")).isEqualTo("default");
    }
  }

  // ========== COMPARISONS ==========

  @Nested
  @DisplayName("Comparações")
  class ComparisonTests {

    @Test
    @DisplayName("compareNumeric deve comparar corretamente")
    void compareNumericShouldCompareCorrectly() {
      assertThat(EvaluatorUtils.compareNumeric(10, 5)).isPositive();
      assertThat(EvaluatorUtils.compareNumeric(5, 10)).isNegative();
      assertThat(EvaluatorUtils.compareNumeric(5, 5)).isZero();
    }

    @Test
    @DisplayName("isBetween deve verificar intervalo corretamente")
    void isBetweenShouldCheckRangeCorrectly() {
      assertThat(EvaluatorUtils.isBetween(5, 1, 10)).isTrue();
      assertThat(EvaluatorUtils.isBetween(1, 1, 10)).isTrue(); // limite inferior
      assertThat(EvaluatorUtils.isBetween(10, 1, 10)).isTrue(); // limite superior
      assertThat(EvaluatorUtils.isBetween(0, 1, 10)).isFalse();
      assertThat(EvaluatorUtils.isBetween(11, 1, 10)).isFalse();
    }

    @Test
    @DisplayName("isBetween deve retornar false para valores nulos")
    void isBetweenShouldReturnFalseForNullValues() {
      assertThat(EvaluatorUtils.isBetween(null, 1, 10)).isFalse();
      assertThat(EvaluatorUtils.isBetween(5, null, 10)).isFalse();
      assertThat(EvaluatorUtils.isBetween(5, 1, null)).isFalse();
    }
  }

  // ========== VALIDATIONS ==========

  @Nested
  @DisplayName("Validações")
  class ValidationTests {

    @Test
    @DisplayName("isNullOrEmpty deve detectar valores nulos e vazios")
    void isNullOrEmptyShouldDetectNullAndEmpty() {
      assertThat(EvaluatorUtils.isNullOrEmpty(null)).isTrue();
      assertThat(EvaluatorUtils.isNullOrEmpty("")).isTrue();
      assertThat(EvaluatorUtils.isNullOrEmpty("   ")).isTrue();
      assertThat(EvaluatorUtils.isNullOrEmpty(List.of())).isTrue();
      assertThat(EvaluatorUtils.isNullOrEmpty(Map.of())).isTrue();
      assertThat(EvaluatorUtils.isNullOrEmpty(new String[0])).isTrue();
    }

    @Test
    @DisplayName("isNullOrEmpty deve retornar false para valores preenchidos")
    void isNullOrEmptyShouldReturnFalseForFilledValues() {
      assertThat(EvaluatorUtils.isNullOrEmpty("text")).isFalse();
      assertThat(EvaluatorUtils.isNullOrEmpty(List.of("item"))).isFalse();
      assertThat(EvaluatorUtils.isNullOrEmpty(Map.of("key", "value"))).isFalse();
      assertThat(EvaluatorUtils.isNullOrEmpty(new String[] {"item"})).isFalse();
    }

    @Test
    @DisplayName("isNotNullOrEmpty deve ser inverso de isNullOrEmpty")
    void isNotNullOrEmptyShouldBeInverse() {
      assertThat(EvaluatorUtils.isNotNullOrEmpty("text")).isTrue();
      assertThat(EvaluatorUtils.isNotNullOrEmpty(null)).isFalse();
      assertThat(EvaluatorUtils.isNotNullOrEmpty("")).isFalse();
    }
  }

  // ========== EXCEPTION HANDLING ==========

  @Nested
  @DisplayName("Tratamento de Exceções")
  class ExceptionHandlingTests {

    @Test
    @DisplayName("evaluateSafely deve retornar resultado da operação")
    void evaluateSafelyShouldReturnOperationResult() {
      boolean result = EvaluatorUtils.evaluateSafely(() -> true, "TEST_OP", false);
      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("evaluateSafely deve retornar default em caso de exceção")
    void evaluateSafelyShouldReturnDefaultOnException() {
      boolean result =
          EvaluatorUtils.evaluateSafely(
              () -> {
                throw new RuntimeException("Test error");
              },
              "TEST_OP",
              false);
      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("evaluateSafely deve tratar null como default")
    void evaluateSafelyShouldTreatNullAsDefault() {
      boolean result = EvaluatorUtils.evaluateSafely(() -> null, "TEST_OP", true);
      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("evaluateOrThrow deve retornar resultado da operação")
    void evaluateOrThrowShouldReturnOperationResult() {
      RuleCondition condition = new RuleCondition();
      condition.setFieldName("test");

      boolean result = EvaluatorUtils.evaluateOrThrow(() -> true, "TEST_OP", condition);
      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("evaluateOrThrow deve lançar exceção em caso de erro")
    void evaluateOrThrowShouldThrowOnError() {
      RuleCondition condition = new RuleCondition();
      condition.setFieldName("test");

      assertThatThrownBy(
              () ->
                  EvaluatorUtils.evaluateOrThrow(
                      () -> {
                        throw new RuntimeException("Test error");
                      },
                      "TEST_OP",
                      condition))
          .isInstanceOf(RuleEvaluationException.class)
          .hasMessageContaining("TEST_OP");
    }

    @Test
    @DisplayName("evaluateOrThrow deve lançar exceção para resultado null")
    void evaluateOrThrowShouldThrowForNullResult() {
      RuleCondition condition = new RuleCondition();
      condition.setFieldName("test");

      assertThatThrownBy(() -> EvaluatorUtils.evaluateOrThrow(() -> null, "TEST_OP", condition))
          .isInstanceOf(RuleEvaluationException.class)
          .hasMessageContaining("null");
    }
  }

  // ========== PARSING CONDITIONS ==========

  @Nested
  @DisplayName("Parsing de Condições")
  class ConditionParsingTests {

    @Test
    @DisplayName("parseThresholdAndWindow deve parsear formato threshold:window")
    void parseThresholdAndWindowShouldParseFormat() {
      double[] result = EvaluatorUtils.parseThresholdAndWindow("100:24", 0, 1);
      assertThat(result[0]).isEqualTo(100.0);
      assertThat(result[1]).isEqualTo(24.0);
    }

    @Test
    @DisplayName("parseThresholdAndWindow deve usar default para window ausente")
    void parseThresholdAndWindowShouldUseDefaultForMissingWindow() {
      double[] result = EvaluatorUtils.parseThresholdAndWindow("100", 0, 12);
      assertThat(result[0]).isEqualTo(100.0);
      assertThat(result[1]).isEqualTo(12.0);
    }

    @Test
    @DisplayName("parseThresholdAndWindow deve usar defaults para valor nulo")
    void parseThresholdAndWindowShouldUseDefaultsForNull() {
      double[] result = EvaluatorUtils.parseThresholdAndWindow(null, 50, 6);
      assertThat(result[0]).isEqualTo(50.0);
      assertThat(result[1]).isEqualTo(6.0);
    }

    @Test
    @DisplayName("parseValueList deve parsear lista separada por vírgula")
    void parseValueListShouldParseCommaSeparatedList() {
      List<String> result = EvaluatorUtils.parseValueList("a,b,c");
      assertThat(result).containsExactly("a", "b", "c");
    }

    @Test
    @DisplayName("parseValueList deve tratar espaços")
    void parseValueListShouldHandleSpaces() {
      List<String> result = EvaluatorUtils.parseValueList("a , b , c");
      assertThat(result).containsExactly("a", "b", "c");
    }

    @Test
    @DisplayName("parseValueList deve retornar lista vazia para null")
    void parseValueListShouldReturnEmptyForNull() {
      List<String> result = EvaluatorUtils.parseValueList(null);
      assertThat(result).isEmpty();
    }
  }

  // ========== DATE/TIME ==========

  @Nested
  @DisplayName("Data e Hora")
  class DateTimeTests {

    @Test
    @DisplayName("parseDate deve parsear data válida")
    void parseDateShouldParseValidDate() {
      Optional<LocalDate> result = EvaluatorUtils.parseDate("2024-06-15");
      assertThat(result).isPresent().contains(LocalDate.of(2024, 6, 15));
    }

    @Test
    @DisplayName("parseDate deve retornar empty para data inválida")
    void parseDateShouldReturnEmptyForInvalidDate() {
      Optional<LocalDate> result = EvaluatorUtils.parseDate("invalid");
      assertThat(result).isEmpty();
    }

    @Test
    @DisplayName("parseTime deve parsear hora válida")
    void parseTimeShouldParseValidTime() {
      Optional<LocalTime> result = EvaluatorUtils.parseTime("10:30");
      assertThat(result).isPresent().contains(LocalTime.of(10, 30));
    }

    @Test
    @DisplayName("parseDateTime deve parsear datetime válido")
    void parseDateTimeShouldParseValidDateTime() {
      Optional<LocalDateTime> result = EvaluatorUtils.parseDateTime("2024-06-15T10:30:00");
      assertThat(result).isPresent().contains(LocalDateTime.of(2024, 6, 15, 10, 30, 0));
    }

    @Test
    @DisplayName("getHourOfDay deve extrair hora de diferentes tipos")
    void getHourOfDayShouldExtractHourFromDifferentTypes() {
      assertThat(EvaluatorUtils.getHourOfDay(LocalTime.of(14, 30))).isEqualTo(14);
      assertThat(EvaluatorUtils.getHourOfDay(LocalDateTime.of(2024, 6, 15, 14, 30))).isEqualTo(14);
      assertThat(
              EvaluatorUtils.getHourOfDay(
                  OffsetDateTime.of(2024, 6, 15, 14, 30, 0, 0, ZoneOffset.UTC)))
          .isEqualTo(14);
      assertThat(EvaluatorUtils.getHourOfDay(null)).isEqualTo(-1);
    }

    @Test
    @DisplayName("getDayOfWeek deve extrair dia da semana")
    void getDayOfWeekShouldExtractDayOfWeek() {
      // 2024-06-17 é Segunda-feira (1)
      assertThat(EvaluatorUtils.getDayOfWeek(LocalDate.of(2024, 6, 17))).isEqualTo(1);
      // 2024-06-15 é Sábado (6)
      assertThat(EvaluatorUtils.getDayOfWeek(LocalDate.of(2024, 6, 15))).isEqualTo(6);
      // 2024-06-16 é Domingo (7)
      assertThat(EvaluatorUtils.getDayOfWeek(LocalDate.of(2024, 6, 16))).isEqualTo(7);
      assertThat(EvaluatorUtils.getDayOfWeek(null)).isEqualTo(-1);
    }
  }
}
