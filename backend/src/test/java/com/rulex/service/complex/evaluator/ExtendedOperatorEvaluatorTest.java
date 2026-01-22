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

@DisplayName("ExtendedOperatorEvaluator Tests")
class ExtendedOperatorEvaluatorTest {

  private ExtendedOperatorEvaluator evaluator;

  @BeforeEach
  void setUp() {
    evaluator = new ExtendedOperatorEvaluator();
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
  @DisplayName("deve suportar operadores estendidos")
  void shouldSupportOperators() {
    assertThat(evaluator.getSupportedOperators())
        .contains(
            ConditionOperator.VELOCITY_SPIKE,
            ConditionOperator.TIME_SINCE_LAST_LT,
            ConditionOperator.FIELD_GT,
            ConditionOperator.EMAIL_DOMAIN_AGE);
  }

  @Test
  @DisplayName("deve retornar categoria correta")
  void shouldReturnCategory() {
    assertThat(evaluator.getCategory()).isEqualTo("EXTENDED");
  }

  @Nested
  @DisplayName("VELOCITY_SPIKE")
  class VelocitySpikeTests {

    @Test
    void shouldReturnTrueWhenPayloadAboveThreshold() {
      RuleCondition condition = condition(ConditionOperator.VELOCITY_SPIKE, "2");
      EvaluationContext context = context(Map.of("velocitySpike", 3));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("EXPIRES_WITHIN_DAYS")
  class ExpiresWithinDaysTests {

    @Test
    void shouldReturnTrueWhenExpiryWithinThreshold() {
      RuleCondition condition = condition(ConditionOperator.EXPIRES_WITHIN_DAYS, "10");
      EvaluationContext context = context(Map.of("expiryDate", LocalDate.now().plusDays(5)));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("TIME_SINCE_LAST_LT")
  class TimeSinceLastTests {

    @Test
    void shouldReturnTrueWhenBelowThreshold() {
      RuleCondition condition = condition(ConditionOperator.TIME_SINCE_LAST_LT, "5");
      EvaluationContext context = context(Map.of("timeSinceLastTx", 3));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("FIELD_GT")
  class FieldGtTests {

    @Test
    void shouldReturnTrueWhenFieldGreaterThanOtherField() {
      RuleCondition condition = condition(ConditionOperator.FIELD_GT, null);
      condition.setValueSingle("otherField");
      EvaluationContext context = context(Map.of("field", 10, "otherField", 3));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("FIELD_EQ")
  class FieldEqTests {

    @Test
    void shouldReturnTrueWhenFieldsEqual() {
      RuleCondition condition = condition(ConditionOperator.FIELD_EQ, null);
      condition.setValueSingle("otherField");
      EvaluationContext context = context(Map.of("field", 10, "otherField", 10));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("GT_CURRENT_DATE")
  class GreaterThanCurrentDateTests {

    @Test
    void shouldReturnTrueWhenDateAfterToday() {
      RuleCondition condition = condition(ConditionOperator.GT_CURRENT_DATE, null);
      condition.setFieldName("targetDate");
      EvaluationContext context = context(Map.of("targetDate", LocalDate.now().plusDays(1)));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("EMAIL_DOMAIN_AGE")
  class EmailDomainAgeTests {

    @Test
    void shouldReturnTrueWhenAgeBelowThreshold() {
      RuleCondition condition = condition(ConditionOperator.EMAIL_DOMAIN_AGE, "30");
      EvaluationContext context = context(Map.of("emailDomainAge", 10));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }
}
