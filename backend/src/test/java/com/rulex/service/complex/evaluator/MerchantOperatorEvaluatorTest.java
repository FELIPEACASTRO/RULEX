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

/**
 * Testes unitários para MerchantOperatorEvaluator.
 *
 * <p>GAP-2 FIX: Cobertura de testes para evaluators.
 *
 * @version 1.0.0
 */
@DisplayName("MerchantOperatorEvaluator Tests")
class MerchantOperatorEvaluatorTest {

  private MerchantOperatorEvaluator evaluator;

  @BeforeEach
  void setUp() {
    evaluator = new MerchantOperatorEvaluator();
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
  @DisplayName("Deve suportar operadores de merchant")
  void shouldSupportMerchantOperators() {
    var supported = evaluator.getSupportedOperators();

    assertThat(supported)
        .contains(
            ConditionOperator.MERCHANT_AGE_CHECK,
            ConditionOperator.MERCHANT_CHARGEBACK_HISTORY,
            ConditionOperator.MERCHANT_FRAUD_RATE_CHECK,
            ConditionOperator.MERCHANT_REPUTATION_SCORE,
            ConditionOperator.MERCHANT_FIRST_SEEN,
            ConditionOperator.MERCHANT_COUNTRY_MISMATCH,
            ConditionOperator.MERCHANT_VELOCITY_SPIKE);
  }

  @Test
  @DisplayName("Deve retornar categoria correta")
  void shouldReturnCorrectCategory() {
    assertThat(evaluator.getCategory()).isEqualTo("MERCHANT");
  }

  // ========== MERCHANT_AGE_CHECK ==========

  @Nested
  @DisplayName("Operador MERCHANT_AGE_CHECK")
  class MerchantAgeCheckTests {

    @Test
    @DisplayName("Deve retornar true quando merchant é muito novo")
    void shouldReturnTrueWhenMerchantTooNew() {
      RuleCondition condition =
          createCondition("merchant", ConditionOperator.MERCHANT_AGE_CHECK, "30");
      EvaluationContext context = createContext(Map.of("merchantAgeDays", 15));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve retornar false quando merchant é antigo")
    void shouldReturnFalseWhenMerchantOld() {
      RuleCondition condition =
          createCondition("merchant", ConditionOperator.MERCHANT_AGE_CHECK, "30");
      EvaluationContext context = createContext(Map.of("merchantAgeDays", 90));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }
  }

  // ========== MERCHANT_CHARGEBACK_HISTORY ==========

  @Nested
  @DisplayName("Operador MERCHANT_CHARGEBACK_HISTORY")
  class MerchantChargebackHistoryTests {

    @Test
    @DisplayName("Deve detectar histórico de chargeback alto")
    void shouldDetectHighChargebackHistory() {
      RuleCondition condition =
          createCondition("merchant", ConditionOperator.MERCHANT_CHARGEBACK_HISTORY, "0.01");
      EvaluationContext context = createContext(Map.of("merchantChargebackRate", 0.05));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve retornar false para histórico de chargeback baixo")
    void shouldReturnFalseForLowChargebackHistory() {
      RuleCondition condition =
          createCondition("merchant", ConditionOperator.MERCHANT_CHARGEBACK_HISTORY, "0.01");
      EvaluationContext context = createContext(Map.of("merchantChargebackRate", 0.005));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }
  }

  // ========== MERCHANT_FRAUD_RATE_CHECK ==========

  @Nested
  @DisplayName("Operador MERCHANT_FRAUD_RATE_CHECK")
  class MerchantFraudRateCheckTests {

    @Test
    @DisplayName("Deve detectar taxa de fraude alta")
    void shouldDetectHighFraudRate() {
      RuleCondition condition =
          createCondition("merchant", ConditionOperator.MERCHANT_FRAUD_RATE_CHECK, "0.02");
      EvaluationContext context = createContext(Map.of("merchantFraudRate", 0.05));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve retornar false para taxa de fraude baixa")
    void shouldReturnFalseForLowFraudRate() {
      RuleCondition condition =
          createCondition("merchant", ConditionOperator.MERCHANT_FRAUD_RATE_CHECK, "0.02");
      EvaluationContext context = createContext(Map.of("merchantFraudRate", 0.01));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }
  }

  // ========== MERCHANT_REPUTATION_SCORE ==========

  @Nested
  @DisplayName("Operador MERCHANT_REPUTATION_SCORE")
  class MerchantReputationScoreTests {

    @Test
    @DisplayName("Deve detectar score de reputação baixo")
    void shouldDetectLowReputationScore() {
      RuleCondition condition =
          createCondition("merchant", ConditionOperator.MERCHANT_REPUTATION_SCORE, "50");
      EvaluationContext context = createContext(Map.of("merchantReputationScore", 30));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve retornar false para score de reputação alto")
    void shouldReturnFalseForHighReputationScore() {
      RuleCondition condition =
          createCondition("merchant", ConditionOperator.MERCHANT_REPUTATION_SCORE, "50");
      EvaluationContext context = createContext(Map.of("merchantReputationScore", 80));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }
  }

  // ========== MERCHANT_FIRST_SEEN ==========

  @Nested
  @DisplayName("Operador MERCHANT_FIRST_SEEN")
  class MerchantFirstSeenTests {

    @Test
    @DisplayName("Deve detectar primeira transação com merchant")
    void shouldDetectFirstTransactionWithMerchant() {
      RuleCondition condition =
          createCondition("merchant", ConditionOperator.MERCHANT_FIRST_SEEN, null);
      EvaluationContext context = createContext(Map.of("isFirstMerchantTransaction", true));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve retornar false para merchant conhecido")
    void shouldReturnFalseForKnownMerchant() {
      RuleCondition condition =
          createCondition("merchant", ConditionOperator.MERCHANT_FIRST_SEEN, null);
      EvaluationContext context = createContext(Map.of("isFirstMerchantTransaction", false));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }
  }

  // ========== MERCHANT_COUNTRY_MISMATCH ==========

  @Nested
  @DisplayName("Operador MERCHANT_COUNTRY_MISMATCH")
  class MerchantCountryMismatchTests {

    @Test
    @DisplayName("Deve detectar mismatch de país do merchant")
    void shouldDetectMerchantCountryMismatch() {
      RuleCondition condition =
          createCondition("merchant", ConditionOperator.MERCHANT_COUNTRY_MISMATCH, null);
      EvaluationContext context =
          createContext(
              Map.of(
                  "merchantCountry", "US",
                  "cardCountry", "BR",
                  "merchantCountryMismatch", true));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve retornar false quando países correspondem")
    void shouldReturnFalseWhenCountriesMatch() {
      RuleCondition condition =
          createCondition("merchant", ConditionOperator.MERCHANT_COUNTRY_MISMATCH, null);
      EvaluationContext context =
          createContext(
              Map.of(
                  "merchantCountry", "BR",
                  "cardCountry", "BR",
                  "merchantCountryMismatch", false));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }
  }

  // ========== MERCHANT_VELOCITY_SPIKE ==========

  @Nested
  @DisplayName("Operador MERCHANT_VELOCITY_SPIKE")
  class MerchantVelocitySpikeTests {

    @Test
    @DisplayName("Deve detectar spike de velocidade do merchant")
    void shouldDetectMerchantVelocitySpike() {
      RuleCondition condition =
          createCondition("merchant", ConditionOperator.MERCHANT_VELOCITY_SPIKE, "3");
      EvaluationContext context =
          createContext(
              Map.of(
                  "merchantCurrentVelocity", 100,
                  "merchantAverageVelocity", 20,
                  "merchantVelocityStdDev", 10));

      boolean result = evaluator.evaluate(condition, context);

      // (100 - 20) / 10 = 8 > 3 = true
      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve retornar false quando não há spike")
    void shouldReturnFalseWhenNoSpike() {
      RuleCondition condition =
          createCondition("merchant", ConditionOperator.MERCHANT_VELOCITY_SPIKE, "3");
      EvaluationContext context =
          createContext(
              Map.of(
                  "merchantCurrentVelocity", 25,
                  "merchantAverageVelocity", 20,
                  "merchantVelocityStdDev", 10));

      boolean result = evaluator.evaluate(condition, context);

      // (25 - 20) / 10 = 0.5 > 3 = false
      assertThat(result).isFalse();
    }
  }

  // ========== MERCHANT_CROSS_BORDER_RATIO ==========

  @Nested
  @DisplayName("Operador MERCHANT_CROSS_BORDER_RATIO")
  class MerchantCrossBorderRatioTests {

    @Test
    @DisplayName("Deve detectar ratio alto de transações cross-border")
    void shouldDetectHighCrossBorderRatio() {
      RuleCondition condition =
          createCondition("merchant", ConditionOperator.MERCHANT_CROSS_BORDER_RATIO, "0.3");
      EvaluationContext context = createContext(Map.of("merchantCrossBorderRatio", 0.6));

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
      RuleCondition condition =
          createCondition("nonExistent", ConditionOperator.MERCHANT_FIRST_SEEN, null);
      EvaluationContext context = createContext(Map.of());

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("Deve tratar valor nulo graciosamente")
    void shouldHandleNullValueGracefully() {
      RuleCondition condition =
          createCondition("merchant", ConditionOperator.MERCHANT_FIRST_SEEN, null);
      java.util.HashMap<String, Object> payload = new java.util.HashMap<>();
      payload.put("isFirstMerchantTransaction", null);
      EvaluationContext context = createContext(payload);

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("Deve funcionar com valores como strings")
    void shouldWorkWithValuesAsStrings() {
      RuleCondition condition =
          createCondition("merchant", ConditionOperator.MERCHANT_FIRST_SEEN, null);
      EvaluationContext context = createContext(Map.of("isFirstMerchantTransaction", "true"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve tratar threshold inválido graciosamente")
    void shouldHandleInvalidThresholdGracefully() {
      RuleCondition condition =
          createCondition("merchant", ConditionOperator.MERCHANT_AGE_CHECK, "invalid");
      EvaluationContext context = createContext(Map.of("merchantAgeDays", 15));

      boolean result = evaluator.evaluate(condition, context);

      // Com threshold inválido (0), 15 < 0 = false
      assertThat(result).isFalse();
    }
  }
}
