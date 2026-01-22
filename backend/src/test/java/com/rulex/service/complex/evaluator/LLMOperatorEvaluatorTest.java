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

@DisplayName("LLMOperatorEvaluator Tests")
class LLMOperatorEvaluatorTest {

  private LLMOperatorEvaluator evaluator;

  @BeforeEach
  void setUp() {
    evaluator = new LLMOperatorEvaluator();
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
  @DisplayName("deve suportar operadores LLM")
  void shouldSupportOperators() {
    assertThat(evaluator.getSupportedOperators())
        .contains(
            ConditionOperator.LLM_DEEPFAKE_VOICE_DETECTION,
            ConditionOperator.LLM_FRAUD_PATTERN_AUTODISCOVERY,
            ConditionOperator.LLM_TRANSACTION_DESCRIPTION_ANALYSIS);
  }

  @Test
  @DisplayName("deve retornar categoria correta")
  void shouldReturnCategory() {
    assertThat(evaluator.getCategory()).isEqualTo("LLM");
  }

  @Nested
  @DisplayName("LLM_DEEPFAKE_VOICE_DETECTION")
  class DeepfakeVoiceTests {

    @Test
    void shouldReturnTrueWhenScoreAboveThreshold() {
      RuleCondition condition = condition(ConditionOperator.LLM_DEEPFAKE_VOICE_DETECTION, "0.7");
      EvaluationContext context = context(Map.of("llmDeepfakeVoiceScore", 0.9));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }

    @Test
    void shouldReturnFalseWhenScoreBelowThreshold() {
      RuleCondition condition = condition(ConditionOperator.LLM_DEEPFAKE_VOICE_DETECTION, "0.7");
      EvaluationContext context = context(Map.of("llmDeepfakeVoiceScore", 0.2));

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }
  }

  @Nested
  @DisplayName("LLM_FRAUD_PATTERN_AUTODISCOVERY")
  class FraudPatternTests {

    @Test
    void shouldReturnTrueWhenDiscovered() {
      RuleCondition condition = condition(ConditionOperator.LLM_FRAUD_PATTERN_AUTODISCOVERY, null);
      EvaluationContext context = context(Map.of("llmFraudPatternDiscovered", true));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("LLM_ANOMALY_EXPLANATION_GENERATION")
  class AnomalyExplanationTests {

    @Test
    void shouldReturnTrueWhenAnomalyDetected() {
      RuleCondition condition = condition(ConditionOperator.LLM_ANOMALY_EXPLANATION_GENERATION, null);
      EvaluationContext context = context(Map.of("llmAnomalyDetected", true));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("LLM_CHATBOT_FRAUD_DETECTION")
  class ChatbotFraudTests {

    @Test
    void shouldReturnTrueWhenScoreAboveThreshold() {
      RuleCondition condition = condition(ConditionOperator.LLM_CHATBOT_FRAUD_DETECTION, "0.7");
      EvaluationContext context = context(Map.of("llmChatbotFraudScore", 0.9));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("LLM_TRANSACTION_DESCRIPTION_ANALYSIS")
  class DescriptionAnalysisTests {

    @Test
    void shouldReturnTrueWhenScoreAboveThreshold() {
      RuleCondition condition = condition(ConditionOperator.LLM_TRANSACTION_DESCRIPTION_ANALYSIS, "0.6");
      EvaluationContext context = context(Map.of("llmTransactionDescriptionScore", 0.9));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }

    @Test
    void shouldReturnTrueWhenBooleanFlagTrue() {
      RuleCondition condition = condition(ConditionOperator.LLM_TRANSACTION_DESCRIPTION_ANALYSIS, "0.6");
      EvaluationContext context = context(Map.of("llmTransactionDescriptionScore", true));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }
}
