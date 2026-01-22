package com.rulex.service.complex.evaluator;

import static org.assertj.core.api.Assertions.assertThat;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.HashMap;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

@DisplayName("SCAOperatorEvaluator Tests")
class SCAOperatorEvaluatorTest {

  private SCAOperatorEvaluator evaluator;

  @BeforeEach
  void setUp() {
    evaluator = new SCAOperatorEvaluator();
  }

  private RuleCondition createCondition(ConditionOperator operator, String value) {
    RuleCondition condition = new RuleCondition();
    condition.setOperator(operator);
    condition.setValueSingle(value);
    return condition;
  }

  private EvaluationContext createContext(Map<String, Object> payload) {
    return EvaluationContext.builder().payload(payload).build();
  }

  @Test
  @DisplayName("deve suportar operadores SCA")
  void shouldSupportOperators() {
    assertThat(evaluator.getSupportedOperators())
        .contains(
            ConditionOperator.SCA_REQUIRED,
            ConditionOperator.SCA_METHOD_ALLOWED,
            ConditionOperator.SCA_CHALLENGE_PASSED,
            ConditionOperator.SCA_EXEMPTION_APPLIED);
  }

  @Test
  @DisplayName("deve retornar categoria correta")
  void shouldReturnCategory() {
    assertThat(evaluator.getCategory()).isEqualTo("SCA");
  }

  @Nested
  @DisplayName("SCA_REQUIRED")
  class ScaRequiredTests {

    @Test
    void shouldReturnTrueWhenRequired() {
      RuleCondition condition = createCondition(ConditionOperator.SCA_REQUIRED, null);
      EvaluationContext context = createContext(Map.of("scaRequired", true));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }

    @Test
    void shouldReturnFalseWhenNotRequired() {
      RuleCondition condition = createCondition(ConditionOperator.SCA_REQUIRED, null);
      EvaluationContext context = createContext(Map.of("scaRequired", false));

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }
  }

  @Nested
  @DisplayName("SCA_METHOD_ALLOWED")
  class ScaMethodAllowedTests {

    @Test
    void shouldReturnTrueWhenMethodAllowed() {
      RuleCondition condition = createCondition(ConditionOperator.SCA_METHOD_ALLOWED, "biometric");
      EvaluationContext context = createContext(Map.of("scaMethod", "BIOMETRIC"));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }

    @Test
    void shouldReturnFalseWhenMethodNotAllowed() {
      RuleCondition condition = createCondition(ConditionOperator.SCA_METHOD_ALLOWED, "otp");
      EvaluationContext context = createContext(Map.of("scaMethod", "biometric"));

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }

    @Test
    void shouldReturnTrueWhenMethodMatchesIgnoringCase() {
      RuleCondition condition = createCondition(ConditionOperator.SCA_METHOD_ALLOWED, "OTP");
      EvaluationContext context = createContext(Map.of("scaMethod", "otp"));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("SCA_CHALLENGE_PASSED")
  class ScaChallengePassedTests {

    @Test
    void shouldReturnTrueWhenChallengePassed() {
      RuleCondition condition = createCondition(ConditionOperator.SCA_CHALLENGE_PASSED, null);
      EvaluationContext context = createContext(Map.of("challengePassed", true));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }

    @Test
    void shouldReturnFalseWhenChallengeFailed() {
      RuleCondition condition = createCondition(ConditionOperator.SCA_CHALLENGE_PASSED, null);
      EvaluationContext context = createContext(Map.of("challengePassed", false));

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }
  }

  @Nested
  @DisplayName("SCA_EXEMPTION_APPLIED")
  class ScaExemptionAppliedTests {

    @Test
    void shouldReturnTrueWhenExemptionApplied() {
      RuleCondition condition = createCondition(ConditionOperator.SCA_EXEMPTION_APPLIED, null);
      EvaluationContext context = createContext(new HashMap<>());

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }

    @Test
    void shouldReturnTrueWithNullPayload() {
      RuleCondition condition = createCondition(ConditionOperator.SCA_EXEMPTION_APPLIED, null);
      EvaluationContext context = EvaluationContext.builder().payload(null).build();

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }
}
