package com.rulex.service.complex.evaluator;

import static org.assertj.core.api.Assertions.assertThat;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

@DisplayName("AmlFraudOperatorEvaluator Tests")
class AmlFraudOperatorEvaluatorTest {

  private AmlFraudOperatorEvaluator evaluator;

  @BeforeEach
  void setUp() {
    evaluator = new AmlFraudOperatorEvaluator();
  }

  private RuleCondition condition(ConditionOperator operator, String value) {
    RuleCondition condition = new RuleCondition();
    condition.setOperator(operator);
    condition.setValueSingle(value);
    return condition;
  }

  private EvaluationContext context(Map<String, Object> payload, TransactionRequest request) {
    return EvaluationContext.builder().payload(payload).transactionRequest(request).build();
  }

  @Test
  @DisplayName("deve suportar operadores AML")
  void shouldSupportOperators() {
    assertThat(evaluator.getSupportedOperators())
        .contains(ConditionOperator.STRUCTURING_DETECTION, ConditionOperator.LAYERING_PATTERN);
  }

  @Test
  @DisplayName("deve retornar categoria correta")
  void shouldReturnCategory() {
    assertThat(evaluator.getCategory()).isEqualTo("AML_FRAUD");
  }

  @Nested
  @DisplayName("STRUCTURING_DETECTION")
  class StructuringTests {

    @Test
    void shouldReturnTrueForStructuringRangeWithSimilarCount() {
      RuleCondition condition = condition(ConditionOperator.STRUCTURING_DETECTION, "3");
      TransactionRequest request = new TransactionRequest();
      request.setTransactionAmount(new BigDecimal("9500"));
      Map<String, Object> payload = Map.of("similarAmountTxnCount", 3);

      assertThat(evaluator.evaluate(condition, context(payload, request))).isTrue();
    }

    @Test
    void shouldReturnTrueForRoundAmountInRange() {
      RuleCondition condition = condition(ConditionOperator.STRUCTURING_DETECTION, "5");
      TransactionRequest request = new TransactionRequest();
      request.setTransactionAmount(new BigDecimal("9000"));

      assertThat(evaluator.evaluate(condition, context(new HashMap<>(), request))).isTrue();
    }

    @Test
    void shouldReturnFalseWhenOutOfRange() {
      RuleCondition condition = condition(ConditionOperator.STRUCTURING_DETECTION, "3");
      TransactionRequest request = new TransactionRequest();
      request.setTransactionAmount(new BigDecimal("500"));

      assertThat(
              evaluator.evaluate(condition, context(Map.of("similarAmountTxnCount", 10), request)))
          .isFalse();
    }

    @Test
    void shouldReturnFalseWhenAmountMissing() {
      RuleCondition condition = condition(ConditionOperator.STRUCTURING_DETECTION, "3");

      assertThat(evaluator.evaluate(condition, context(new HashMap<>(), new TransactionRequest())))
          .isFalse();
    }
  }

  @Nested
  @DisplayName("LAYERING_PATTERN")
  class LayeringTests {

    @Test
    void shouldReturnTrueWhenMultipleHopsAndRapid() {
      RuleCondition condition = condition(ConditionOperator.LAYERING_PATTERN, "3");
      Map<String, Object> payload =
          Map.of(
              "transactionHopCount", 4,
              "minutesToNextTransaction", 10);

      assertThat(evaluator.evaluate(condition, context(payload, null))).isTrue();
    }

    @Test
    void shouldReturnTrueWhenMultipleHopsAndCrossBorder() {
      RuleCondition condition = condition(ConditionOperator.LAYERING_PATTERN, "3");
      Map<String, Object> payload =
          Map.of("intermediaryAccountCount", 3, "crossBorderTransfer", true);

      assertThat(evaluator.evaluate(condition, context(payload, null))).isTrue();
    }

    @Test
    void shouldReturnFalseWhenIndicatorsMissing() {
      RuleCondition condition = condition(ConditionOperator.LAYERING_PATTERN, "3");
      Map<String, Object> payload = Map.of("transactionHopCount", 1);

      assertThat(evaluator.evaluate(condition, context(payload, null))).isFalse();
    }

    @Test
    void shouldReturnFalseWhenNotRapidOrCrossBorder() {
      RuleCondition condition = condition(ConditionOperator.LAYERING_PATTERN, "3");
      Map<String, Object> payload =
          Map.of("transactionHopCount", 4, "minutesToNextTransaction", 120);

      assertThat(evaluator.evaluate(condition, context(payload, null))).isFalse();
    }
  }
}
