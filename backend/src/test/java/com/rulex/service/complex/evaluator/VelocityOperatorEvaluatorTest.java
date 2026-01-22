package com.rulex.service.complex.evaluator;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.VelocityService;
import com.rulex.service.VelocityService.VelocityStats;
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

@DisplayName("VelocityOperatorEvaluator Tests")
@ExtendWith(MockitoExtension.class)
class VelocityOperatorEvaluatorTest {

  @Mock
  private VelocityService velocityService;

  private VelocityOperatorEvaluator evaluator;

  @BeforeEach
  void setUp() {
    evaluator = new VelocityOperatorEvaluator(velocityService);
  }

  private RuleCondition condition(ConditionOperator operator, String value) {
    RuleCondition condition = new RuleCondition();
    condition.setOperator(operator);
    condition.setFieldName("cardNumber");
    condition.setValueSingle(value);
    return condition;
  }

  private EvaluationContext context(TransactionRequest request) {
    return EvaluationContext.builder()
        .payload(Map.of("cardNumber", "4111111111111111"))
        .transactionRequest(request)
        .build();
  }

  @Test
  @DisplayName("deve suportar operadores de velocidade")
  void shouldSupportOperators() {
    assertThat(evaluator.getSupportedOperators())
        .contains(
            ConditionOperator.VELOCITY_COUNT_GT,
            ConditionOperator.VELOCITY_SUM_GT,
            ConditionOperator.COUNT_LAST_N_HOURS);
  }

  @Test
  @DisplayName("deve retornar categoria correta")
  void shouldReturnCategory() {
    assertThat(evaluator.getCategory()).isEqualTo("VELOCITY");
  }

  @Nested
  @DisplayName("VELOCITY_COUNT_GT")
  class VelocityCountGtTests {

    @Test
    void shouldReturnTrueWhenCountAboveThreshold() {
      RuleCondition condition = condition(ConditionOperator.VELOCITY_COUNT_GT, "5");
      VelocityStats stats = VelocityStats.builder().transactionCount(10).build();

      when(velocityService.getStats(any(), any(), any())).thenReturn(stats);

      assertThat(evaluator.evaluate(condition, context(new TransactionRequest()))).isTrue();
    }
  }

  @Nested
  @DisplayName("VELOCITY_COUNT_LT")
  class VelocityCountLtTests {

    @Test
    void shouldReturnTrueWhenCountBelowThreshold() {
      RuleCondition condition = condition(ConditionOperator.VELOCITY_COUNT_LT, "10");
      VelocityStats stats = VelocityStats.builder().transactionCount(2).build();

      when(velocityService.getStats(any(), any(), any())).thenReturn(stats);

      assertThat(evaluator.evaluate(condition, context(new TransactionRequest()))).isTrue();
    }
  }

  @Nested
  @DisplayName("VELOCITY_SUM_GT")
  class VelocitySumGtTests {

    @Test
    void shouldReturnTrueWhenSumAboveThreshold() {
      RuleCondition condition = condition(ConditionOperator.VELOCITY_SUM_GT, "100");

      when(velocityService.getAggregation(any(), any(), any(), any()))
          .thenReturn(new BigDecimal("500"));

      assertThat(evaluator.evaluate(condition, context(new TransactionRequest()))).isTrue();
    }
  }

  @Nested
  @DisplayName("VELOCITY_SUM_LT")
  class VelocitySumLtTests {

    @Test
    void shouldReturnTrueWhenSumBelowThreshold() {
      RuleCondition condition = condition(ConditionOperator.VELOCITY_SUM_LT, "500");

      when(velocityService.getAggregation(any(), any(), any(), any()))
          .thenReturn(new BigDecimal("100"));

      assertThat(evaluator.evaluate(condition, context(new TransactionRequest()))).isTrue();
    }
  }

  @Nested
  @DisplayName("VELOCITY_AVG_LT")
  class VelocityAvgLtTests {

    @Test
    void shouldReturnTrueWhenAvgBelowThreshold() {
      RuleCondition condition = condition(ConditionOperator.VELOCITY_AVG_LT, "100");

      when(velocityService.getAggregation(any(), any(), any(), any()))
          .thenReturn(new BigDecimal("50"));

      assertThat(evaluator.evaluate(condition, context(new TransactionRequest()))).isTrue();
    }
  }

  @Nested
  @DisplayName("VELOCITY_DISTINCT_GT")
  class VelocityDistinctGtTests {

    @Test
    void shouldReturnTrueWhenDistinctAboveThreshold() {
      RuleCondition condition = condition(ConditionOperator.VELOCITY_DISTINCT_GT, "2");

      when(velocityService.getAggregation(any(), any(), any(), any()))
          .thenReturn(new BigDecimal("5"));

      assertThat(evaluator.evaluate(condition, context(new TransactionRequest()))).isTrue();
    }
  }

  @Nested
  @DisplayName("COUNT_DISTINCT_COUNTRIES_LAST_N_HOURS")
  class CountDistinctCountriesTests {

    @Test
    void shouldReturnTrueWhenDistinctCountriesAboveThreshold() {
      RuleCondition condition = condition(ConditionOperator.COUNT_DISTINCT_COUNTRIES_LAST_N_HOURS, "2:4");

      when(velocityService.getAggregation(any(), any(), any(), any()))
          .thenReturn(new BigDecimal("4"));

      assertThat(evaluator.evaluate(condition, context(new TransactionRequest()))).isTrue();
    }
  }

  @Nested
  @DisplayName("AVG_LAST_N_DAYS")
  class AvgLastNDaysTests {

    @Test
    void shouldReturnTrueWhenAverageAboveThreshold() {
      RuleCondition condition = condition(ConditionOperator.AVG_LAST_N_DAYS, "100:7");

      when(velocityService.getAggregation(any(), any(), any(), any()))
          .thenReturn(new BigDecimal("200"));

      assertThat(evaluator.evaluate(condition, context(new TransactionRequest()))).isTrue();
    }
  }

  @Nested
  @DisplayName("COUNT_LAST_N_HOURS")
  class CountLastNHoursTests {

    @Test
    void shouldReturnTrueWhenCountAboveThreshold() {
      RuleCondition condition = condition(ConditionOperator.COUNT_LAST_N_HOURS, "5:2");

      when(velocityService.getAggregation(any(), any(), any(), any()))
          .thenReturn(new BigDecimal("10"));

      assertThat(evaluator.evaluate(condition, context(new TransactionRequest()))).isTrue();
    }
  }

  @Nested
  @DisplayName("SUM_LAST_N_DAYS")
  class SumLastNDaysTests {

    @Test
    void shouldReturnTrueWhenSumAboveThreshold() {
      RuleCondition condition = condition(ConditionOperator.SUM_LAST_N_DAYS, "100:7");

      when(velocityService.getAggregation(any(), any(), any(), any()))
          .thenReturn(new BigDecimal("200"));

      assertThat(evaluator.evaluate(condition, context(new TransactionRequest()))).isTrue();
    }
  }

  @Nested
  @DisplayName("SUM_LAST_N_HOURS")
  class SumLastNHoursTests {

    @Test
    void shouldReturnTrueWhenSumAboveThreshold() {
      RuleCondition condition = condition(ConditionOperator.SUM_LAST_N_HOURS, "50:2");

      when(velocityService.getAggregation(any(), any(), any(), any()))
          .thenReturn(new BigDecimal("100"));

      assertThat(evaluator.evaluate(condition, context(new TransactionRequest()))).isTrue();
    }
  }

  @Nested
  @DisplayName("MIN_AMOUNT_LAST_N_DAYS")
  class MinAmountLastNDaysTests {

    @Test
    void shouldReturnTrueWhenMinBelowThreshold() {
      RuleCondition condition = condition(ConditionOperator.MIN_AMOUNT_LAST_N_DAYS, "100:7");

      when(velocityService.getAggregation(any(), any(), any(), any()))
          .thenReturn(new BigDecimal("50"));

      assertThat(evaluator.evaluate(condition, context(new TransactionRequest()))).isTrue();
    }
  }
}
