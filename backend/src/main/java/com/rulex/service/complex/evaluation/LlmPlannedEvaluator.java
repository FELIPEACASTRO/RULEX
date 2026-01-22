package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.exception.UnsupportedOperatorException;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;

public final class LlmPlannedEvaluator {

  private LlmPlannedEvaluator() {}

  public static boolean evaluateLlmTransactionDescriptionAnalysis(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.LLM_TRANSACTION_DESCRIPTION_ANALYSIS,
        "Operador PLANNED - requer integração com API LLM. Consulte GET /api/operators/status.");
  }

  public static boolean evaluateLlmGenerativeRuleSynthesis(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.LLM_GENERATIVE_RULE_SYNTHESIS,
        "Operador PLANNED - requer integração com API LLM. Consulte GET /api/operators/status.");
  }

  public static boolean evaluateLlmAnomalyExplanationGeneration(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.LLM_ANOMALY_EXPLANATION_GENERATION,
        "Operador PLANNED - requer integração com API LLM. Consulte GET /api/operators/status.");
  }

  public static boolean evaluateLlmChatbotFraudDetection(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.LLM_CHATBOT_FRAUD_DETECTION,
        "Operador PLANNED - requer integração com API LLM. Consulte GET /api/operators/status.");
  }

  public static boolean evaluateLlmDeepfakeVoiceDetection(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.LLM_DEEPFAKE_VOICE_DETECTION,
        "Operador PLANNED - requer integração com API LLM. Consulte GET /api/operators/status.");
  }

  public static boolean evaluateLlmSyntheticImageDetection(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.LLM_SYNTHETIC_IMAGE_DETECTION,
        "Operador PLANNED - requer integração com API LLM. Consulte GET /api/operators/status.");
  }

  public static boolean evaluateLlmEmailPhishingAnalysis(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.LLM_EMAIL_PHISHING_ANALYSIS,
        "Operador PLANNED - requer integração com API LLM. Consulte GET /api/operators/status.");
  }

  public static boolean evaluateLlmSocialEngineeringClassification(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.LLM_SOCIAL_ENGINEERING_CLASSIFICATION,
        "Operador PLANNED - requer integração com API LLM. Consulte GET /api/operators/status.");
  }

  public static boolean evaluateLlmFraudAlertPrioritization(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.LLM_FRAUD_ALERT_PRIORITIZATION,
        "Operador PLANNED - requer integração com API LLM. Consulte GET /api/operators/status.");
  }

  public static boolean evaluateLlmMultiModalFraudDetection(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.LLM_MULTI_MODAL_FRAUD_DETECTION,
        "Operador PLANNED - requer integração com API LLM. Consulte GET /api/operators/status.");
  }

  public static boolean evaluateLlmAdversarialAttackResistance(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.LLM_ADVERSARIAL_ATTACK_RESISTANCE,
        "Operador PLANNED - requer integração com API LLM. Consulte GET /api/operators/status.");
  }

  public static boolean evaluateLlmFraudPatternAutodiscovery(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.LLM_FRAUD_PATTERN_AUTODISCOVERY,
        "Operador PLANNED - requer integração com API LLM. Consulte GET /api/operators/status.");
  }
}