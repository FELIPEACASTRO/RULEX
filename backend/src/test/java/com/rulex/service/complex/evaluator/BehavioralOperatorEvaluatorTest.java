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
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Testes unitários para BehavioralOperatorEvaluator.
 *
 * <p>GAP-2 FIX: Cobertura de testes para evaluators.
 */
@DisplayName("BehavioralOperatorEvaluator Tests")
class BehavioralOperatorEvaluatorTest {

  private BehavioralOperatorEvaluator evaluator;

  @BeforeEach
  void setUp() {
    evaluator = new BehavioralOperatorEvaluator();
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
  @DisplayName("deve suportar operadores comportamentais")
  void shouldSupportOperators() {
    assertThat(evaluator.getSupportedOperators())
        .contains(
            ConditionOperator.ADAPTIVE_BEHAVIORAL_ANALYTICS,
            ConditionOperator.LOGIN_PATTERN_DEVIATION,
            ConditionOperator.FACE_TO_ID_PHOTO_MATCHING);
  }

  @Test
  @DisplayName("deve retornar categoria correta")
  void shouldReturnCategory() {
    assertThat(evaluator.getCategory()).isEqualTo("BEHAVIORAL");
  }

  @Nested
  @DisplayName("Operadores com score")
  class ScoreOperators {

    @Test
    @DisplayName("ADAPTIVE_BEHAVIORAL_ANALYTICS deve retornar true acima do threshold")
    void shouldEvaluateAdaptiveBehavioralScore() {
      RuleCondition condition = createCondition(ConditionOperator.ADAPTIVE_BEHAVIORAL_ANALYTICS, "0.7");
      EvaluationContext context = createContext(Map.of("adaptiveBehavioralScore", 0.9));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }

    @Test
    @DisplayName("BEHAVIORAL_BASELINE_DEVIATION deve retornar false abaixo do threshold")
    void shouldEvaluateBaselineDeviation() {
      RuleCondition condition = createCondition(ConditionOperator.BEHAVIORAL_BASELINE_DEVIATION, "2.0");
      EvaluationContext context = createContext(Map.of("behavioralBaselineDeviation", 1.5));

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }

    @Test
    @DisplayName("RISK_PROFILE_DEVIATION deve retornar true acima do threshold")
    void shouldEvaluateRiskProfileDeviation() {
      RuleCondition condition = createCondition(ConditionOperator.RISK_PROFILE_DEVIATION, "0.3");
      EvaluationContext context = createContext(Map.of("riskProfileDeviation", 0.6));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("Operadores booleanos")
  class BooleanOperators {

    @ParameterizedTest(name = "{0} deve avaliar booleano do payload")
    @CsvSource({
      "CHANNEL_SWITCH_PATTERN,channelSwitchPattern,true,true",
      "LOGIN_PATTERN_DEVIATION,loginPatternDeviation,true,true",
      "SESSION_BEHAVIOR_ANOMALY,sessionBehaviorAnomaly,false,false",
      "NAVIGATION_PATTERN_ANOMALY,navigationPatternAnomaly,true,true",
      "TIME_PREFERENCE_SHIFT,timePreferenceShift,false,false",
      "UNUSUAL_BUSINESS_PATTERN,unusualBusinessPattern,true,true",
      "AUDIO_FINGERPRINT_NEW,audioFingerprintNew,true,true",
      "CANVAS_FINGERPRINT_MISMATCH,canvasFingerprintMismatch,true,true",
      "WEBGL_FINGERPRINT_ANOMALY,webglFingerprintAnomaly,false,false",
      "TOUCH_SUPPORT_INCONSISTENCY,touchSupportInconsistency,true,true"
    })
    void shouldEvaluateBooleanOperators(
        ConditionOperator operator,
        String payloadKey,
        boolean payloadValue,
        boolean expected) {
      RuleCondition condition = createCondition(operator, null);
      EvaluationContext context = createContext(Map.of(payloadKey, payloadValue));

      assertThat(evaluator.evaluate(condition, context)).isEqualTo(expected);
    }

    @Test
    @DisplayName("FACE_TO_ID_PHOTO_MATCHING deve retornar true quando score abaixo do threshold")
    void shouldEvaluateFaceMatchLessThan() {
      RuleCondition condition = createCondition(ConditionOperator.FACE_TO_ID_PHOTO_MATCHING, "0.8");
      EvaluationContext context = createContext(Map.of("faceToIdPhotoMatch", 0.6));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }

    @Test
    @DisplayName("FACE_TO_ID_PHOTO_MATCHING deve retornar false acima do threshold")
    void shouldEvaluateFaceMatchAboveThreshold() {
      RuleCondition condition = createCondition(ConditionOperator.FACE_TO_ID_PHOTO_MATCHING, "0.8");
      EvaluationContext context = createContext(Map.of("faceToIdPhotoMatch", 0.95));

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }
  }

  @Nested
  @DisplayName("Casos de borda")
  class EdgeCases {

    @Test
    @DisplayName("deve retornar false quando payload é nulo")
    void shouldHandleNullPayload() {
      RuleCondition condition = createCondition(ConditionOperator.CHANNEL_SWITCH_PATTERN, null);
      EvaluationContext context = createContext(null);

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }

    @Test
    @DisplayName("deve retornar false quando chave não existe")
    void shouldHandleMissingKey() {
      RuleCondition condition = createCondition(ConditionOperator.LOGIN_PATTERN_DEVIATION, null);
      EvaluationContext context = createContext(new HashMap<>());

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }
  }
}
