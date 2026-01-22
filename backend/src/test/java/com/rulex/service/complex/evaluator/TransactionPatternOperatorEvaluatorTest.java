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

@DisplayName("TransactionPatternOperatorEvaluator Tests")
class TransactionPatternOperatorEvaluatorTest {

  private TransactionPatternOperatorEvaluator evaluator;

  @BeforeEach
  void setUp() {
    evaluator = new TransactionPatternOperatorEvaluator();
  }

  private RuleCondition condition(ConditionOperator operator, String value) {
    RuleCondition condition = new RuleCondition();
    condition.setOperator(operator);
    condition.setValueSingle(value);
    return condition;
  }

  private EvaluationContext context(Map<String, Object> payload) {
    return EvaluationContext.builder().payload(payload).build();
  }

  @Test
  @DisplayName("deve suportar operadores de padrão")
  void shouldSupportOperators() {
    assertThat(evaluator.getSupportedOperators())
        .contains(
            ConditionOperator.TRANSACTION_COUNT_PER_CARD_HOUR,
            ConditionOperator.SPLIT_TRANSACTION_DETECTION,
            ConditionOperator.MICRO_DEPOSIT_VELOCITY);
  }

  @Test
  @DisplayName("deve retornar categoria correta")
  void shouldReturnCategory() {
    assertThat(evaluator.getCategory()).isEqualTo("TRANSACTION_PATTERN");
  }

  @Nested
  @DisplayName("TRANSACTION_COUNT_PER_CARD_HOUR")
  class CountPerCardTests {

    @Test
    void shouldReturnTrueWhenAboveThreshold() {
      RuleCondition condition = condition(ConditionOperator.TRANSACTION_COUNT_PER_CARD_HOUR, "10");
      EvaluationContext context = context(Map.of("txCountPerCardHour", 12));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }

    @Test
    void shouldReturnFalseWhenBelowThreshold() {
      RuleCondition condition = condition(ConditionOperator.TRANSACTION_COUNT_PER_CARD_HOUR, "10");
      EvaluationContext context = context(Map.of("txCountPerCardHour", 5));

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }
  }

  @Nested
  @DisplayName("SPLIT_TRANSACTION_DETECTION")
  class SplitTransactionTests {

    @Test
    void shouldReturnTrueWhenSplitDetected() {
      RuleCondition condition = condition(ConditionOperator.SPLIT_TRANSACTION_DETECTION, null);
      EvaluationContext context = context(Map.of("splitTransactionDetected", true));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("PATTERN_SPLIT_TRANSACTION")
  class PatternSplitTests {

    @Test
    void shouldReturnTrueWhenPatternSplitFlagged() {
      RuleCondition condition = condition(ConditionOperator.PATTERN_SPLIT_TRANSACTION, null);
      EvaluationContext context = context(Map.of("patternSplitTransaction", true));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("SPLIT_PAYMENT_PATTERN")
  class SplitPaymentTests {

    @Test
    void shouldReturnTrueWhenSplitPaymentFlagged() {
      RuleCondition condition = condition(ConditionOperator.SPLIT_PAYMENT_PATTERN, null);
      EvaluationContext context = context(Map.of("splitPaymentPattern", true));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("LAYERED_TRANSFER_PATTERN")
  class LayeredTransferTests {

    @Test
    void shouldReturnTrueWhenLayeredTransferFlagged() {
      RuleCondition condition = condition(ConditionOperator.LAYERED_TRANSFER_PATTERN, null);
      EvaluationContext context = context(Map.of("layeredTransferPattern", true));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("MICRO_DEPOSIT_VELOCITY")
  class MicroDepositTests {

    @Test
    void shouldReturnTrueWhenVelocityHigh() {
      RuleCondition condition = condition(ConditionOperator.MICRO_DEPOSIT_VELOCITY, "5");
      EvaluationContext context = context(Map.of("microDepositVelocity", 8));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("TRANSACTION_ATTEMPT_COUNT_PER_CARD")
  class AttemptCountTests {

    @Test
    void shouldReturnTrueWhenAttemptsAboveThreshold() {
      RuleCondition condition =
          condition(ConditionOperator.TRANSACTION_ATTEMPT_COUNT_PER_CARD, "5");
      EvaluationContext context = context(Map.of("txAttemptCountPerCard", 7));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("TRANSACTION_FREQUENCY_ANOMALY")
  class FrequencyAnomalyTests {

    @Test
    void shouldReturnTrueWhenFrequencyAnomaly() {
      RuleCondition condition = condition(ConditionOperator.TRANSACTION_FREQUENCY_ANOMALY, null);
      EvaluationContext context = context(Map.of("txFrequencyAnomaly", true));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("RAPID_MOVEMENT")
  class RapidMovementTests {

    @Test
    void shouldReturnTrueWhenRapidMovementDetected() {
      RuleCondition condition = condition(ConditionOperator.RAPID_MOVEMENT, null);
      EvaluationContext context = context(Map.of("rapidMovementDetected", true));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("MICRO_TRANSACTION_TEST")
  class MicroTransactionTests {

    @Test
    void shouldReturnTrueWhenMicroTransactionFlagged() {
      RuleCondition condition = condition(ConditionOperator.MICRO_TRANSACTION_TEST, null);
      EvaluationContext context = context(Map.of("microTransactionTest", true));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }
}
