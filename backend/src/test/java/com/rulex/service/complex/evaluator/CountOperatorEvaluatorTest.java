package com.rulex.service.complex.evaluator;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.VelocityService;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.math.BigDecimal;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@DisplayName("CountOperatorEvaluator Tests")
@ExtendWith(MockitoExtension.class)
class CountOperatorEvaluatorTest {

  @Mock private VelocityService velocityService;

  private CountOperatorEvaluator evaluator;

  @BeforeEach
  void setUp() {
    evaluator = new CountOperatorEvaluator(velocityService);
  }

  private RuleCondition condition(ConditionOperator operator, String value, String fieldName) {
    RuleCondition condition = new RuleCondition();
    condition.setOperator(operator);
    condition.setValueSingle(value);
    condition.setValueMin("7");
    condition.setFieldName(fieldName);
    return condition;
  }

  private EvaluationContext context(Map<String, Object> payload, TransactionRequest request) {
    return EvaluationContext.builder().payload(payload).transactionRequest(request).build();
  }

  @Test
  @DisplayName("deve suportar operadores de contagem")
  void shouldSupportOperators() {
    assertThat(evaluator.getSupportedOperators())
        .contains(
            ConditionOperator.COUNT_LAST_N_DAYS,
            ConditionOperator.COUNT_DISTINCT_PANS_LAST_N_HOURS,
            ConditionOperator.COUNT_DISTINCT_MERCHANTS_LAST_N_HOURS);
  }

  @Test
  @DisplayName("deve retornar categoria correta")
  void shouldReturnCategory() {
    assertThat(evaluator.getCategory()).isEqualTo("COUNT");
  }

  @Nested
  @DisplayName("COUNT_LAST_N_DAYS")
  class CountLastNDaysTests {

    @Test
    void shouldUseVelocityServiceWhenRequestPresent() {
      RuleCondition condition = condition(ConditionOperator.COUNT_LAST_N_DAYS, "5", "cardNumber");

      when(velocityService.getAggregation(any(), any(), any(), any()))
          .thenReturn(new BigDecimal("10"));

      TransactionRequest request = new TransactionRequest();
      EvaluationContext context = context(Map.of(), request);

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }

    @Test
    void shouldUsePayloadWhenRequestMissing() {
      RuleCondition condition =
          condition(ConditionOperator.COUNT_LAST_N_DAYS, "5", "transactionCountLastNDays");
      EvaluationContext context = context(Map.of("transactionCountLastNDays", 8), null);

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("COUNT_DISTINCT_USER_AGENTS_LAST_N_HOURS")
  class DistinctUserAgentTests {

    @Test
    void shouldReturnTrueWhenPayloadAboveThreshold() {
      RuleCondition condition =
          condition(
              ConditionOperator.COUNT_DISTINCT_USER_AGENTS_LAST_N_HOURS,
              "3",
              "distinctUserAgentCount");
      EvaluationContext context = context(Map.of("distinctUserAgentCount", 5), null);

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("COUNT_DISTINCT_PANS_LAST_N_HOURS")
  class DistinctPanTests {

    @Test
    void shouldReturnTrueWhenPayloadAboveThreshold() {
      RuleCondition condition =
          condition(ConditionOperator.COUNT_DISTINCT_PANS_LAST_N_HOURS, "2", "distinctPanCount");
      EvaluationContext context = context(Map.of("distinctPanCount", 4), null);

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("COUNT_MFA_DENIALS_LAST_N_HOURS")
  class MfaDenialTests {

    @Test
    void shouldReturnTrueWhenPayloadAboveThreshold() {
      RuleCondition condition =
          condition(ConditionOperator.COUNT_MFA_DENIALS_LAST_N_HOURS, "1", "mfaDenialCount");
      EvaluationContext context = context(Map.of("mfaDenialCount", 2), null);

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("COUNT_FAILURES_LAST_N_HOURS")
  class FailureCountTests {

    @Test
    void shouldReturnTrueWhenPayloadAboveThreshold() {
      RuleCondition condition =
          condition(ConditionOperator.COUNT_FAILURES_LAST_N_HOURS, "2", "failureCount");
      EvaluationContext context = context(Map.of("failureCount", 5), null);

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }
}
