package com.rulex.service.complex.evaluator;

import static org.assertj.core.api.Assertions.assertThat;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.time.LocalDate;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

@DisplayName("MiscOperatorEvaluator Tests")
class MiscOperatorEvaluatorTest {

  private MiscOperatorEvaluator evaluator;

  @BeforeEach
  void setUp() {
    evaluator = new MiscOperatorEvaluator();
  }

  private RuleCondition condition(ConditionOperator operator, String value) {
    RuleCondition condition = new RuleCondition();
    condition.setOperator(operator);
    condition.setValueSingle(value);
    condition.setFieldName("field");
    return condition;
  }

  private EvaluationContext context(Map<String, Object> payload) {
    return EvaluationContext.builder().payload(payload).build();
  }

  @Test
  @DisplayName("deve suportar operadores misc")
  void shouldSupportOperators() {
    assertThat(evaluator.getSupportedOperators())
        .contains(
            ConditionOperator.IS_VOIP,
            ConditionOperator.IS_HOLIDAY,
            ConditionOperator.NAME_SIMILARITY_LT);
  }

  @Test
  @DisplayName("deve retornar categoria correta")
  void shouldReturnCategory() {
    assertThat(evaluator.getCategory()).isEqualTo("MISC");
  }

  @Nested
  @DisplayName("IS_VOIP")
  class IsVoipTests {

    @Test
    void shouldReturnTrueWhenVoipPrefix() {
      RuleCondition condition = condition(ConditionOperator.IS_VOIP, null);
      EvaluationContext context = context(Map.of("phone", "0300123456"));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("IS_CRYPTO_RANSOM_AMOUNT")
  class CryptoRansomTests {

    @Test
    void shouldReturnTrueWhenAmountNearRansomValue() {
      RuleCondition condition = condition(ConditionOperator.IS_CRYPTO_RANSOM_AMOUNT, null);
      EvaluationContext context = context(Map.of("amount", "1000"));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }

    @Test
    void shouldReturnFalseWhenAmountNotRansomValue() {
      RuleCondition condition = condition(ConditionOperator.IS_CRYPTO_RANSOM_AMOUNT, null);
      EvaluationContext context = context(Map.of("amount", "123"));

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }
  }

  @Nested
  @DisplayName("IS_HOLIDAY")
  class HolidayTests {

    @Test
    void shouldReturnTrueForHolidayDate() {
      RuleCondition condition = condition(ConditionOperator.IS_HOLIDAY, "true");
      EvaluationContext context = context(Map.of("transactionDate", LocalDate.of(2025, 1, 1)));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("HAS_FAILED_3DS_LAST_N_MINUTES")
  class Failed3dsTests {

    @Test
    void shouldReturnTrueWhenFlagPresent() {
      RuleCondition condition = condition(ConditionOperator.HAS_FAILED_3DS_LAST_N_MINUTES, null);
      EvaluationContext context = context(Map.of("hasFailed3ds", true));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("HAS_INCOMING_TRANSFER_LAST_N_HOURS")
  class IncomingTransferTests {

    @Test
    void shouldReturnTrueWhenIncomingAmountPositive() {
      RuleCondition condition =
          condition(ConditionOperator.HAS_INCOMING_TRANSFER_LAST_N_HOURS, null);
      EvaluationContext context = context(Map.of("lastIncomingAmount", "50"));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("GTE_PERCENT_OF_LAST_INCOMING")
  class PercentLastIncomingTests {

    @Test
    void shouldReturnTrueWhenPercentAboveThreshold() {
      RuleCondition condition = condition(ConditionOperator.GTE_PERCENT_OF_LAST_INCOMING, "80");
      EvaluationContext context = context(Map.of("amount", "90", "lastIncomingAmount", "100"));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("PIX_KEY_CHANGED_LAST_N_DAYS")
  class PixKeyChangedTests {

    @Test
    void shouldReturnTrueWhenDaysBelowThreshold() {
      RuleCondition condition = condition(ConditionOperator.PIX_KEY_CHANGED_LAST_N_DAYS, "7");
      EvaluationContext context = context(Map.of("daysSincePixKeyChange", 3));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("NAME_SIMILARITY_LT")
  class NameSimilarityTests {

    @Test
    void shouldReturnTrueWhenSimilarityBelowThreshold() {
      RuleCondition condition = condition(ConditionOperator.NAME_SIMILARITY_LT, "0.9");
      EvaluationContext context = context(Map.of("name1", "Alice", "name2", "Bob"));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("NAME_TRANSLITERATION_MATCH")
  class TransliterationTests {

    @Test
    void shouldReturnTrueWhenNamesNormalizeEqual() {
      RuleCondition condition = condition(ConditionOperator.NAME_TRANSLITERATION_MATCH, null);
      EvaluationContext context = context(Map.of("name1", "José", "name2", "Jose"));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }
}
