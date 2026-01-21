package com.rulex.service.complex.evaluator;

import static org.assertj.core.api.Assertions.assertThat;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Testes unitários para DateTimeOperatorEvaluator.
 *
 * <p>GAP-2 FIX: Cobertura de testes para evaluators.
 *
 * @version 1.0.0
 */
@DisplayName("DateTimeOperatorEvaluator Tests")
class DateTimeOperatorEvaluatorTest {

    private DateTimeOperatorEvaluator evaluator;

    @BeforeEach
    void setUp() {
        evaluator = new DateTimeOperatorEvaluator();
    }

    // ========== HELPER METHODS ==========

    private RuleCondition createCondition(String fieldName, ConditionOperator operator, String value) {
        RuleCondition condition = new RuleCondition();
        condition.setFieldName(fieldName);
        condition.setOperator(operator);
        condition.setValueSingle(value);
        return condition;
    }

    private EvaluationContext createContext(Map<String, Object> payload) {
        return EvaluationContext.builder()
            .payload(payload)
            .build();
    }

    // ========== SUPPORTED OPERATORS ==========

    @Test
    @DisplayName("Deve suportar operadores de data/hora")
    void shouldSupportDateTimeOperators() {
        var supported = evaluator.getSupportedOperators();

        assertThat(supported).contains(
            ConditionOperator.DATE_BEFORE,
            ConditionOperator.DATE_AFTER,
            ConditionOperator.DATE_BETWEEN,
            ConditionOperator.TIME_BEFORE,
            ConditionOperator.TIME_AFTER,
            ConditionOperator.TIME_BETWEEN,
            ConditionOperator.DAY_OF_WEEK_IN,
            ConditionOperator.HOUR_BETWEEN,
            ConditionOperator.IS_WEEKEND
        );
    }

    @Test
    @DisplayName("Deve retornar categoria correta")
    void shouldReturnCorrectCategory() {
        assertThat(evaluator.getCategory()).isEqualTo("DATETIME");
    }

    // ========== DATE_BEFORE ==========

    @Nested
    @DisplayName("Operador DATE_BEFORE")
    class DateBeforeTests {

        @Test
        @DisplayName("Deve retornar true quando data é anterior")
        void shouldReturnTrueWhenDateIsBefore() {
            RuleCondition condition = createCondition("transactionDate", ConditionOperator.DATE_BEFORE, "2024-12-31");
            EvaluationContext context = createContext(Map.of("transactionDate", LocalDate.of(2024, 6, 15)));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isTrue();
        }

        @Test
        @DisplayName("Deve retornar false quando data é posterior")
        void shouldReturnFalseWhenDateIsAfter() {
            RuleCondition condition = createCondition("transactionDate", ConditionOperator.DATE_BEFORE, "2024-01-01");
            EvaluationContext context = createContext(Map.of("transactionDate", LocalDate.of(2024, 6, 15)));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isFalse();
        }

        @Test
        @DisplayName("Deve funcionar com string de data")
        void shouldWorkWithDateString() {
            RuleCondition condition = createCondition("transactionDate", ConditionOperator.DATE_BEFORE, "2024-12-31");
            EvaluationContext context = createContext(Map.of("transactionDate", "2024-06-15"));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isTrue();
        }
    }

    // ========== DATE_AFTER ==========

    @Nested
    @DisplayName("Operador DATE_AFTER")
    class DateAfterTests {

        @Test
        @DisplayName("Deve retornar true quando data é posterior")
        void shouldReturnTrueWhenDateIsAfter() {
            RuleCondition condition = createCondition("transactionDate", ConditionOperator.DATE_AFTER, "2024-01-01");
            EvaluationContext context = createContext(Map.of("transactionDate", LocalDate.of(2024, 6, 15)));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isTrue();
        }

        @Test
        @DisplayName("Deve retornar false quando data é anterior")
        void shouldReturnFalseWhenDateIsBefore() {
            RuleCondition condition = createCondition("transactionDate", ConditionOperator.DATE_AFTER, "2024-12-31");
            EvaluationContext context = createContext(Map.of("transactionDate", LocalDate.of(2024, 6, 15)));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isFalse();
        }
    }

    // ========== DATE_BETWEEN ==========

    @Nested
    @DisplayName("Operador DATE_BETWEEN")
    class DateBetweenTests {

        @Test
        @DisplayName("Deve retornar true quando data está no intervalo")
        void shouldReturnTrueWhenDateInRange() {
            RuleCondition condition = createCondition("transactionDate", ConditionOperator.DATE_BETWEEN, "2024-01-01:2024-12-31");
            EvaluationContext context = createContext(Map.of("transactionDate", LocalDate.of(2024, 6, 15)));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isTrue();
        }

        @Test
        @DisplayName("Deve retornar false quando data está fora do intervalo")
        void shouldReturnFalseWhenDateOutOfRange() {
            RuleCondition condition = createCondition("transactionDate", ConditionOperator.DATE_BETWEEN, "2024-01-01:2024-05-31");
            EvaluationContext context = createContext(Map.of("transactionDate", LocalDate.of(2024, 6, 15)));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isFalse();
        }

        @Test
        @DisplayName("Deve incluir limites do intervalo")
        void shouldIncludeBoundaries() {
            RuleCondition condition = createCondition("transactionDate", ConditionOperator.DATE_BETWEEN, "2024-06-15:2024-06-15");
            EvaluationContext context = createContext(Map.of("transactionDate", LocalDate.of(2024, 6, 15)));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isTrue();
        }
    }

    // ========== TIME_BEFORE ==========

    @Nested
    @DisplayName("Operador TIME_BEFORE")
    class TimeBeforeTests {

        @Test
        @DisplayName("Deve retornar true quando hora é anterior")
        void shouldReturnTrueWhenTimeIsBefore() {
            RuleCondition condition = createCondition("transactionTime", ConditionOperator.TIME_BEFORE, "18:00");
            EvaluationContext context = createContext(Map.of("transactionTime", LocalTime.of(10, 30)));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isTrue();
        }

        @Test
        @DisplayName("Deve retornar false quando hora é posterior")
        void shouldReturnFalseWhenTimeIsAfter() {
            RuleCondition condition = createCondition("transactionTime", ConditionOperator.TIME_BEFORE, "08:00");
            EvaluationContext context = createContext(Map.of("transactionTime", LocalTime.of(10, 30)));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isFalse();
        }
    }

    // ========== TIME_AFTER ==========

    @Nested
    @DisplayName("Operador TIME_AFTER")
    class TimeAfterTests {

        @Test
        @DisplayName("Deve retornar true quando hora é posterior")
        void shouldReturnTrueWhenTimeIsAfter() {
            RuleCondition condition = createCondition("transactionTime", ConditionOperator.TIME_AFTER, "08:00");
            EvaluationContext context = createContext(Map.of("transactionTime", LocalTime.of(10, 30)));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isTrue();
        }

        @Test
        @DisplayName("Deve retornar false quando hora é anterior")
        void shouldReturnFalseWhenTimeIsBefore() {
            RuleCondition condition = createCondition("transactionTime", ConditionOperator.TIME_AFTER, "18:00");
            EvaluationContext context = createContext(Map.of("transactionTime", LocalTime.of(10, 30)));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isFalse();
        }
    }

    // ========== TIME_BETWEEN ==========

    @Nested
    @DisplayName("Operador TIME_BETWEEN")
    class TimeBetweenTests {

        @Test
        @DisplayName("Deve retornar true quando hora está no intervalo")
        void shouldReturnTrueWhenTimeInRange() {
            RuleCondition condition = createCondition("transactionTime", ConditionOperator.TIME_BETWEEN, "08:00:18:00");
            EvaluationContext context = createContext(Map.of("transactionTime", LocalTime.of(10, 30)));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isTrue();
        }

        @Test
        @DisplayName("Deve retornar false quando hora está fora do intervalo")
        void shouldReturnFalseWhenTimeOutOfRange() {
            RuleCondition condition = createCondition("transactionTime", ConditionOperator.TIME_BETWEEN, "08:00:12:00");
            EvaluationContext context = createContext(Map.of("transactionTime", LocalTime.of(15, 30)));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isFalse();
        }
    }

    // ========== HOUR_BETWEEN ==========

    @Nested
    @DisplayName("Operador HOUR_BETWEEN")
    class HourBetweenTests {

        @ParameterizedTest
        @CsvSource({
            "10, 8:18, true",   // 10 entre 8 e 18
            "20, 8:18, false",  // 20 fora de 8-18
            "8, 8:18, true",    // limite inferior
            "18, 8:18, true"    // limite superior
        })
        @DisplayName("HOUR_BETWEEN deve comparar corretamente")
        void hourBetweenShouldCompareCorrectly(int hour, String range, boolean expected) {
            RuleCondition condition = createCondition("transactionTime", ConditionOperator.HOUR_BETWEEN, range);
            EvaluationContext context = createContext(Map.of("transactionTime", LocalTime.of(hour, 0)));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isEqualTo(expected);
        }

        @Test
        @DisplayName("Deve funcionar com OffsetDateTime")
        void shouldWorkWithOffsetDateTime() {
            RuleCondition condition = createCondition("timestamp", ConditionOperator.HOUR_BETWEEN, "8:18");
            OffsetDateTime timestamp = OffsetDateTime.of(2024, 6, 15, 10, 30, 0, 0, ZoneOffset.UTC);
            EvaluationContext context = createContext(Map.of("timestamp", timestamp));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isTrue();
        }
    }

    // ========== DAY_OF_WEEK_IN ==========

    @Nested
    @DisplayName("Operador DAY_OF_WEEK_IN")
    class DayOfWeekInTests {

        @Test
        @DisplayName("Deve retornar true quando dia da semana está na lista")
        void shouldReturnTrueWhenDayInList() {
            // 1=Segunda, 2=Terça, ..., 7=Domingo
            RuleCondition condition = createCondition("transactionDate", ConditionOperator.DAY_OF_WEEK_IN, "1,2,3,4,5");
            // 2024-06-17 é uma Segunda-feira (1)
            EvaluationContext context = createContext(Map.of("transactionDate", LocalDate.of(2024, 6, 17)));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isTrue();
        }

        @Test
        @DisplayName("Deve retornar false quando dia da semana não está na lista")
        void shouldReturnFalseWhenDayNotInList() {
            // Apenas dias úteis
            RuleCondition condition = createCondition("transactionDate", ConditionOperator.DAY_OF_WEEK_IN, "1,2,3,4,5");
            // 2024-06-15 é um Sábado (6)
            EvaluationContext context = createContext(Map.of("transactionDate", LocalDate.of(2024, 6, 15)));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isFalse();
        }
    }

    // ========== IS_WEEKEND ==========

    @Nested
    @DisplayName("Operador IS_WEEKEND")
    class IsWeekendTests {

        @Test
        @DisplayName("Deve retornar true para sábado")
        void shouldReturnTrueForSaturday() {
            RuleCondition condition = createCondition("transactionDate", ConditionOperator.IS_WEEKEND, null);
            // 2024-06-15 é um Sábado
            EvaluationContext context = createContext(Map.of("transactionDate", LocalDate.of(2024, 6, 15)));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isTrue();
        }

        @Test
        @DisplayName("Deve retornar true para domingo")
        void shouldReturnTrueForSunday() {
            RuleCondition condition = createCondition("transactionDate", ConditionOperator.IS_WEEKEND, null);
            // 2024-06-16 é um Domingo
            EvaluationContext context = createContext(Map.of("transactionDate", LocalDate.of(2024, 6, 16)));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isTrue();
        }

        @Test
        @DisplayName("Deve retornar false para dia útil")
        void shouldReturnFalseForWeekday() {
            RuleCondition condition = createCondition("transactionDate", ConditionOperator.IS_WEEKEND, null);
            // 2024-06-17 é uma Segunda-feira
            EvaluationContext context = createContext(Map.of("transactionDate", LocalDate.of(2024, 6, 17)));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isFalse();
        }
    }

    // ========== EDGE CASES ==========

    @Nested
    @DisplayName("Casos de Borda")
    class EdgeCaseTests {

        @Test
        @DisplayName("Deve tratar data nula graciosamente")
        void shouldHandleNullDateGracefully() {
            RuleCondition condition = createCondition("transactionDate", ConditionOperator.DATE_BEFORE, "2024-12-31");
            EvaluationContext context = createContext(Map.of());

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isFalse();
        }

        @Test
        @DisplayName("Deve tratar formato de data inválido graciosamente")
        void shouldHandleInvalidDateFormatGracefully() {
            RuleCondition condition = createCondition("transactionDate", ConditionOperator.DATE_BEFORE, "invalid-date");
            EvaluationContext context = createContext(Map.of("transactionDate", LocalDate.of(2024, 6, 15)));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isFalse();
        }

        @Test
        @DisplayName("Deve funcionar com LocalDateTime")
        void shouldWorkWithLocalDateTime() {
            RuleCondition condition = createCondition("timestamp", ConditionOperator.DATE_BEFORE, "2024-12-31");
            EvaluationContext context = createContext(Map.of("timestamp", LocalDateTime.of(2024, 6, 15, 10, 30)));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isTrue();
        }

        @Test
        @DisplayName("Deve funcionar com OffsetDateTime")
        void shouldWorkWithOffsetDateTime() {
            RuleCondition condition = createCondition("timestamp", ConditionOperator.DATE_BEFORE, "2024-12-31");
            OffsetDateTime timestamp = OffsetDateTime.of(2024, 6, 15, 10, 30, 0, 0, ZoneOffset.UTC);
            EvaluationContext context = createContext(Map.of("timestamp", timestamp));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isTrue();
        }

        @Test
        @DisplayName("Deve tratar intervalo mal formatado graciosamente")
        void shouldHandleMalformedRangeGracefully() {
            RuleCondition condition = createCondition("transactionDate", ConditionOperator.DATE_BETWEEN, "invalid");
            EvaluationContext context = createContext(Map.of("transactionDate", LocalDate.of(2024, 6, 15)));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isFalse();
        }
    }
}
