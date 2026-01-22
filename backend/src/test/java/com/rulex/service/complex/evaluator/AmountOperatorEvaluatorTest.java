package com.rulex.service.complex.evaluator;

import static org.assertj.core.api.Assertions.assertThat;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.math.BigDecimal;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

/**
 * Testes unitários para AmountOperatorEvaluator.
 *
 * <p>GAP-2 FIX: Cobertura de testes para evaluators.
 *
 * @version 1.0.0
 */
@DisplayName("AmountOperatorEvaluator Tests")
class AmountOperatorEvaluatorTest {

  private AmountOperatorEvaluator evaluator;

  @BeforeEach
  void setUp() {
    evaluator = new AmountOperatorEvaluator();
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
  @DisplayName("Deve suportar operadores de valor")
  void shouldSupportAmountOperators() {
    var supported = evaluator.getSupportedOperators();

    assertThat(supported)
        .contains(
            ConditionOperator.AMOUNT_DEVIATION_FROM_AVG,
            ConditionOperator.AMOUNT_ROUNDING_BEHAVIOR,
            ConditionOperator.AMOUNT_SPIKE,
            ConditionOperator.AMOUNT_SUM_PER_CARD_HOUR,
            ConditionOperator.AMOUNT_SUM_PER_CUSTOMER_DAY,
            ConditionOperator.AMOUNT_VARIANCE_ANOMALY,
            ConditionOperator.DECIMAL_PLACES_GT,
            ConditionOperator.ROUND_AMOUNT_FREQUENCY,
            ConditionOperator.SMALL_AMOUNT_VELOCITY,
            ConditionOperator.LARGE_AMOUNT_FREQUENCY);
  }

  @Test
  @DisplayName("Deve retornar categoria correta")
  void shouldReturnCorrectCategory() {
    assertThat(evaluator.getCategory()).isEqualTo("AMOUNT");
  }

  // ========== AMOUNT_DEVIATION_FROM_AVG ==========

  @Nested
  @DisplayName("Operador AMOUNT_DEVIATION_FROM_AVG")
  class AmountDeviationFromAvgTests {

    @Test
    @DisplayName("Deve retornar true quando desvio excede threshold")
    void shouldReturnTrueWhenDeviationExceedsThreshold() {
      RuleCondition condition =
          createCondition("amount", ConditionOperator.AMOUNT_DEVIATION_FROM_AVG, "2");
      EvaluationContext context =
          createContext(
              Map.of(
                  "amount", 3000.0,
                  "averageAmount", 1000.0,
                  "stdDevAmount", 500.0));

      boolean result = evaluator.evaluate(condition, context);

      // (3000 - 1000) / 500 = 4 > 2 = true
      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve retornar false quando desvio não excede threshold")
    void shouldReturnFalseWhenDeviationDoesNotExceedThreshold() {
      RuleCondition condition =
          createCondition("amount", ConditionOperator.AMOUNT_DEVIATION_FROM_AVG, "2");
      EvaluationContext context =
          createContext(
              Map.of(
                  "amount", 1500.0,
                  "averageAmount", 1000.0,
                  "stdDevAmount", 500.0));

      boolean result = evaluator.evaluate(condition, context);

      // (1500 - 1000) / 500 = 1 > 2 = false
      assertThat(result).isFalse();
    }
  }

  // ========== AMOUNT_ROUNDING_BEHAVIOR ==========

  @Nested
  @DisplayName("Operador AMOUNT_ROUNDING_BEHAVIOR")
  class AmountRoundingBehaviorTests {

    @Test
    @DisplayName("Deve detectar comportamento de arredondamento")
    void shouldDetectRoundingBehavior() {
      RuleCondition condition =
          createCondition("amount", ConditionOperator.AMOUNT_ROUNDING_BEHAVIOR, null);
      EvaluationContext context =
          createContext(
              Map.of(
                  "amount", 1000.0,
                  "roundAmountCount", 8,
                  "totalTransactionCount", 10));

      boolean result = evaluator.evaluate(condition, context);

      // 80% de transações com valores redondos = comportamento suspeito
      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve retornar false para comportamento normal")
    void shouldReturnFalseForNormalBehavior() {
      RuleCondition condition =
          createCondition("amount", ConditionOperator.AMOUNT_ROUNDING_BEHAVIOR, null);
      EvaluationContext context =
          createContext(
              Map.of(
                  "amount", 1234.56,
                  "roundAmountCount", 2,
                  "totalTransactionCount", 10));

      boolean result = evaluator.evaluate(condition, context);

      // 20% de transações com valores redondos = normal
      assertThat(result).isFalse();
    }
  }

  // ========== AMOUNT_SPIKE ==========

  @Nested
  @DisplayName("Operador AMOUNT_SPIKE")
  class AmountSpikeTests {

    @Test
    @DisplayName("Deve detectar spike de valor")
    void shouldDetectAmountSpike() {
      RuleCondition condition = createCondition("amount", ConditionOperator.AMOUNT_SPIKE, "3");
      EvaluationContext context =
          createContext(
              Map.of(
                  "amount", 5000.0,
                  "averageAmount", 1000.0,
                  "stdDevAmount", 200.0));

      boolean result = evaluator.evaluate(condition, context);

      // (5000 - 1000) / 200 = 20 > 3 = true
      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve retornar false quando não há spike")
    void shouldReturnFalseWhenNoSpike() {
      RuleCondition condition = createCondition("amount", ConditionOperator.AMOUNT_SPIKE, "3");
      EvaluationContext context =
          createContext(
              Map.of(
                  "amount", 1200.0,
                  "averageAmount", 1000.0,
                  "stdDevAmount", 200.0));

      boolean result = evaluator.evaluate(condition, context);

      // (1200 - 1000) / 200 = 1 > 3 = false
      assertThat(result).isFalse();
    }
  }

  // ========== AMOUNT_SUM_PER_CARD_HOUR ==========

  @Nested
  @DisplayName("Operador AMOUNT_SUM_PER_CARD_HOUR")
  class AmountSumPerCardHourTests {

    @Test
    @DisplayName("Deve retornar true quando soma por cartão/hora excede threshold")
    void shouldReturnTrueWhenSumExceedsThreshold() {
      RuleCondition condition =
          createCondition("amount", ConditionOperator.AMOUNT_SUM_PER_CARD_HOUR, "5000");
      EvaluationContext context =
          createContext(Map.of("cardNumber", "4111111111111111", "amountSumPerCardHour", 7500.0));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve retornar false quando soma não excede threshold")
    void shouldReturnFalseWhenSumDoesNotExceedThreshold() {
      RuleCondition condition =
          createCondition("amount", ConditionOperator.AMOUNT_SUM_PER_CARD_HOUR, "5000");
      EvaluationContext context =
          createContext(Map.of("cardNumber", "4111111111111111", "amountSumPerCardHour", 3000.0));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }
  }

  // ========== AMOUNT_SUM_PER_CUSTOMER_DAY ==========

  @Nested
  @DisplayName("Operador AMOUNT_SUM_PER_CUSTOMER_DAY")
  class AmountSumPerCustomerDayTests {

    @Test
    @DisplayName("Deve retornar true quando soma por cliente/dia excede threshold")
    void shouldReturnTrueWhenSumExceedsThreshold() {
      RuleCondition condition =
          createCondition("amount", ConditionOperator.AMOUNT_SUM_PER_CUSTOMER_DAY, "10000");
      EvaluationContext context =
          createContext(Map.of("customerId", "customer-123", "amountSumPerCustomerDay", 15000.0));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }
  }

  // ========== AMOUNT_VARIANCE_ANOMALY ==========

  @Nested
  @DisplayName("Operador AMOUNT_VARIANCE_ANOMALY")
  class AmountVarianceAnomalyTests {

    @Test
    @DisplayName("Deve detectar anomalia de variância")
    void shouldDetectVarianceAnomaly() {
      RuleCondition condition =
          createCondition("amount", ConditionOperator.AMOUNT_VARIANCE_ANOMALY, null);
      EvaluationContext context =
          createContext(
              Map.of(
                  "amountVariance", 50000.0,
                  "expectedVariance", 10000.0));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve retornar false para variância normal")
    void shouldReturnFalseForNormalVariance() {
      RuleCondition condition =
          createCondition("amount", ConditionOperator.AMOUNT_VARIANCE_ANOMALY, null);
      EvaluationContext context =
          createContext(
              Map.of(
                  "amountVariance", 10000.0,
                  "expectedVariance", 10000.0));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }
  }

  // ========== DECIMAL_PLACES_GT ==========

  @Nested
  @DisplayName("Operador DECIMAL_PLACES_GT")
  class DecimalPlacesGtTests {

    @Test
    @DisplayName("Deve retornar true quando casas decimais excedem threshold")
    void shouldReturnTrueWhenDecimalPlacesExceedThreshold() {
      RuleCondition condition = createCondition("amount", ConditionOperator.DECIMAL_PLACES_GT, "2");
      EvaluationContext context = createContext(Map.of("amount", 123.4567));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve retornar false quando casas decimais não excedem threshold")
    void shouldReturnFalseWhenDecimalPlacesDoNotExceedThreshold() {
      RuleCondition condition = createCondition("amount", ConditionOperator.DECIMAL_PLACES_GT, "2");
      EvaluationContext context = createContext(Map.of("amount", 123.45));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }
  }

  // ========== ROUND_AMOUNT_FREQUENCY ==========

  @Nested
  @DisplayName("Operador ROUND_AMOUNT_FREQUENCY")
  class RoundAmountFrequencyTests {

    @Test
    @DisplayName("Deve detectar alta frequência de valores redondos")
    void shouldDetectHighRoundAmountFrequency() {
      RuleCondition condition =
          createCondition("amount", ConditionOperator.ROUND_AMOUNT_FREQUENCY, "0.5");
      EvaluationContext context = createContext(Map.of("roundAmountFrequency", 0.8));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve retornar false para frequência normal")
    void shouldReturnFalseForNormalFrequency() {
      RuleCondition condition =
          createCondition("amount", ConditionOperator.ROUND_AMOUNT_FREQUENCY, "0.5");
      EvaluationContext context = createContext(Map.of("roundAmountFrequency", 0.3));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }
  }

  // ========== SMALL_AMOUNT_VELOCITY ==========

  @Nested
  @DisplayName("Operador SMALL_AMOUNT_VELOCITY")
  class SmallAmountVelocityTests {

    @Test
    @DisplayName("Deve detectar alta velocidade de valores pequenos (smurfing)")
    void shouldDetectHighSmallAmountVelocity() {
      RuleCondition condition =
          createCondition("amount", ConditionOperator.SMALL_AMOUNT_VELOCITY, "10");
      EvaluationContext context = createContext(Map.of("smallAmountCount", 15, "amount", 50.0));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve retornar false para velocidade normal")
    void shouldReturnFalseForNormalVelocity() {
      RuleCondition condition =
          createCondition("amount", ConditionOperator.SMALL_AMOUNT_VELOCITY, "10");
      EvaluationContext context = createContext(Map.of("smallAmountCount", 5, "amount", 50.0));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }
  }

  // ========== LARGE_AMOUNT_FREQUENCY ==========

  @Nested
  @DisplayName("Operador LARGE_AMOUNT_FREQUENCY")
  class LargeAmountFrequencyTests {

    @Test
    @DisplayName("Deve detectar alta frequência de valores grandes")
    void shouldDetectHighLargeAmountFrequency() {
      RuleCondition condition =
          createCondition("amount", ConditionOperator.LARGE_AMOUNT_FREQUENCY, "0.3");
      EvaluationContext context = createContext(Map.of("largeAmountFrequency", 0.5));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }
  }

  // ========== EDGE CASES ==========

  @Nested
  @DisplayName("Casos de Borda")
  class EdgeCaseTests {

    @Test
    @DisplayName("Deve tratar campo ausente graciosamente")
    void shouldHandleMissingFieldGracefully() {
      RuleCondition condition = createCondition("nonExistent", ConditionOperator.AMOUNT_SPIKE, "3");
      EvaluationContext context = createContext(Map.of());

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("Deve tratar valor nulo graciosamente")
    void shouldHandleNullValueGracefully() {
      RuleCondition condition = createCondition("amount", ConditionOperator.AMOUNT_SPIKE, "3");
      java.util.HashMap<String, Object> payload = new java.util.HashMap<>();
      payload.put("amount", null);
      EvaluationContext context = createContext(payload);

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("Deve funcionar com BigDecimal")
    void shouldWorkWithBigDecimal() {
      RuleCondition condition = createCondition("amount", ConditionOperator.DECIMAL_PLACES_GT, "2");
      EvaluationContext context = createContext(Map.of("amount", new BigDecimal("123.4567")));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve funcionar com valores como strings")
    void shouldWorkWithValuesAsStrings() {
      RuleCondition condition =
          createCondition("amount", ConditionOperator.AMOUNT_SUM_PER_CARD_HOUR, "5000");
      EvaluationContext context =
          createContext(
              Map.of(
                  "cardNumber", "4111111111111111",
                  "amountSumPerCardHour", "7500.0"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve tratar valores negativos")
    void shouldHandleNegativeValues() {
      RuleCondition condition = createCondition("amount", ConditionOperator.AMOUNT_SPIKE, "3");
      EvaluationContext context =
          createContext(
              Map.of(
                  "amount", -1000.0,
                  "averageAmount", 1000.0,
                  "stdDevAmount", 200.0));

      boolean result = evaluator.evaluate(condition, context);

      // Valor negativo muito diferente da média
      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve tratar desvio padrão zero")
    void shouldHandleZeroStdDev() {
      RuleCondition condition = createCondition("amount", ConditionOperator.AMOUNT_SPIKE, "3");
      EvaluationContext context =
          createContext(
              Map.of(
                  "amount", 1500.0,
                  "averageAmount", 1000.0,
                  "stdDevAmount", 0.0));

      boolean result = evaluator.evaluate(condition, context);

      // Com stdDev = 0, qualquer diferença é infinita, mas deve tratar graciosamente
      assertThat(result).isFalse();
    }
  }
}
