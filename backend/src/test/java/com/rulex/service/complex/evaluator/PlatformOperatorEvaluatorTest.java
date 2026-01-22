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

@DisplayName("PlatformOperatorEvaluator Tests")
class PlatformOperatorEvaluatorTest {

  private PlatformOperatorEvaluator evaluator;

  @BeforeEach
  void setUp() {
    evaluator = new PlatformOperatorEvaluator();
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
  @DisplayName("deve suportar operadores platform")
  void shouldSupportOperators() {
    assertThat(evaluator.getSupportedOperators())
        .contains(
            ConditionOperator.PLT_IDENTITY_RESOLUTION,
            ConditionOperator.PLT_ML_FRAUD_RISK_OUTCOME,
            ConditionOperator.PLT_REVIEWLIST_QUEUE);
  }

  @Test
  @DisplayName("deve retornar categoria correta")
  void shouldReturnCategory() {
    assertThat(evaluator.getCategory()).isEqualTo("PLATFORM");
  }

  @Nested
  @DisplayName("PLT_IDENTITY_RESOLUTION")
  class IdentityResolutionTests {

    @Test
    void shouldReturnTrueWhenConfidenceAboveThreshold() {
      RuleCondition condition = condition(ConditionOperator.PLT_IDENTITY_RESOLUTION, "0.8");
      EvaluationContext context = context(Map.of("field", 0.9));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("PLT_RISK_PROFILE_ASSIGNMENT")
  class RiskProfileTests {

    @Test
    void shouldReturnTrueWhenProfileMatches() {
      RuleCondition condition = condition(ConditionOperator.PLT_RISK_PROFILE_ASSIGNMENT, "HIGH");
      EvaluationContext context = context(Map.of("field", "HIGH"));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("PLT_ML_FRAUD_RISK_OUTCOME")
  class MlFraudRiskTests {

    @Test
    void shouldReturnTrueWhenRiskAboveThreshold() {
      RuleCondition condition = condition(ConditionOperator.PLT_ML_FRAUD_RISK_OUTCOME, "0.7");
      EvaluationContext context = context(Map.of("field", 0.9));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("PLT_RADAR_METADATA_MATCHING")
  class RadarMetadataTests {

    @Test
    void shouldReturnTrueWhenMatchFlagged() {
      RuleCondition condition = condition(ConditionOperator.PLT_RADAR_METADATA_MATCHING, null);
      EvaluationContext context = context(Map.of("field", "MATCH"));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("PLT_VELOCITY_FILTERS")
  class VelocityFiltersTests {

    @Test
    void shouldReturnTrueWhenCountAboveThreshold() {
      RuleCondition condition = condition(ConditionOperator.PLT_VELOCITY_FILTERS, "10");
      EvaluationContext context = context(Map.of("field", 12));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("PLT_REVIEWLIST_QUEUE")
  class ReviewListTests {

    @Test
    void shouldReturnTrueWhenReviewFlag() {
      RuleCondition condition = condition(ConditionOperator.PLT_REVIEWLIST_QUEUE, null);
      EvaluationContext context = context(Map.of("field", "REVIEW"));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }
}
