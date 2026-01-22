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

@DisplayName("BaselOperatorEvaluator Tests")
class BaselOperatorEvaluatorTest {

  private BaselOperatorEvaluator evaluator;

  @BeforeEach
  void setUp() {
    evaluator = new BaselOperatorEvaluator();
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
  @DisplayName("deve suportar operadores Basel")
  void shouldSupportOperators() {
    assertThat(evaluator.getSupportedOperators())
        .contains(
            ConditionOperator.BSL_BUSINESS_INDICATOR,
            ConditionOperator.BSL_BUCKET_CLASSIFICATION,
            ConditionOperator.BSL_SCENARIO_ANALYSIS);
  }

  @Test
  @DisplayName("deve retornar categoria correta")
  void shouldReturnCategory() {
    assertThat(evaluator.getCategory()).isEqualTo("BASEL_III");
  }

  @Nested
  @DisplayName("BSL_BUSINESS_INDICATOR")
  class BusinessIndicatorTests {

    @Test
    void shouldReturnTrueWhenAboveThreshold() {
      RuleCondition condition = condition(ConditionOperator.BSL_BUSINESS_INDICATOR, "1000");
      EvaluationContext context = context(Map.of("field", 2000));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }

    @Test
    void shouldReturnFalseWhenBelowThreshold() {
      RuleCondition condition = condition(ConditionOperator.BSL_BUSINESS_INDICATOR, "1000");
      EvaluationContext context = context(Map.of("field", 500));

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }
  }

  @Nested
  @DisplayName("BSL_BUCKET_CLASSIFICATION")
  class BucketClassificationTests {

    @Test
    void shouldMatch1BnBucket() {
      RuleCondition condition = condition(ConditionOperator.BSL_BUCKET_CLASSIFICATION, "1BN");
      EvaluationContext context = context(Map.of("field", 900_000_000));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }

    @Test
    void shouldMatchAbove30BnBucket() {
      RuleCondition condition =
          condition(ConditionOperator.BSL_BUCKET_CLASSIFICATION, "ABOVE_30BN");
      EvaluationContext context = context(Map.of("field", 31_000_000_000L));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("BSL_RETENTION_PERIOD")
  class RetentionPeriodTests {

    @Test
    void shouldReturnTrueWhenRetentionMeetsMinimum() {
      RuleCondition condition = condition(ConditionOperator.BSL_RETENTION_PERIOD, "10");
      EvaluationContext context = context(Map.of("field", 12));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("BSL_SCENARIO_ANALYSIS")
  class ScenarioAnalysisTests {

    @Test
    void shouldReturnTrueWhenAnalyzed() {
      RuleCondition condition = condition(ConditionOperator.BSL_SCENARIO_ANALYSIS, null);
      EvaluationContext context = context(Map.of("field", "ANALYZED"));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }
}
