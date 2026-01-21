package com.rulex.service.complex.evaluator;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.math.BigDecimal;
import java.util.Map;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Avaliador para operadores baseados em LLM (Large Language Models).
 */
@Component
@Slf4j
public class LLMOperatorEvaluator implements OperatorEvaluator {

    private static final Set<ConditionOperator> SUPPORTED = Set.of(
        ConditionOperator.LLM_ADVERSARIAL_ATTACK_RESISTANCE,
        ConditionOperator.LLM_ANOMALY_EXPLANATION_GENERATION,
        ConditionOperator.LLM_CHATBOT_FRAUD_DETECTION,
        ConditionOperator.LLM_DEEPFAKE_VOICE_DETECTION,
        ConditionOperator.LLM_EMAIL_PHISHING_ANALYSIS,
        ConditionOperator.LLM_FRAUD_ALERT_PRIORITIZATION,
        ConditionOperator.LLM_FRAUD_PATTERN_AUTODISCOVERY,
        ConditionOperator.LLM_GENERATIVE_RULE_SYNTHESIS,
        ConditionOperator.LLM_MULTI_MODAL_FRAUD_DETECTION,
        ConditionOperator.LLM_SOCIAL_ENGINEERING_CLASSIFICATION,
        ConditionOperator.LLM_SYNTHETIC_IMAGE_DETECTION,
        ConditionOperator.LLM_TRANSACTION_DESCRIPTION_ANALYSIS
    );

    @Override
    public Set<ConditionOperator> getSupportedOperators() {
        return SUPPORTED;
    }

    @Override
    public boolean evaluate(RuleCondition condition, EvaluationContext context) {
        ConditionOperator op = condition.getOperator();
        log.debug("LLMOperatorEvaluator: op={}", op);

        return switch (op) {
            case LLM_ADVERSARIAL_ATTACK_RESISTANCE -> evaluateScore(context, "llmAdversarialScore", condition, 0.7);
            case LLM_ANOMALY_EXPLANATION_GENERATION -> evaluateBoolean(context, "llmAnomalyDetected");
            case LLM_CHATBOT_FRAUD_DETECTION -> evaluateScore(context, "llmChatbotFraudScore", condition, 0.7);
            case LLM_DEEPFAKE_VOICE_DETECTION -> evaluateScore(context, "llmDeepfakeVoiceScore", condition, 0.7);
            case LLM_EMAIL_PHISHING_ANALYSIS -> evaluateScore(context, "llmEmailPhishingScore", condition, 0.6);
            case LLM_FRAUD_ALERT_PRIORITIZATION -> evaluateScore(context, "llmFraudAlertPriority", condition, 0.8);
            case LLM_FRAUD_PATTERN_AUTODISCOVERY -> evaluateBoolean(context, "llmFraudPatternDiscovered");
            case LLM_GENERATIVE_RULE_SYNTHESIS -> evaluateBoolean(context, "llmRuleSynthesized");
            case LLM_MULTI_MODAL_FRAUD_DETECTION -> evaluateScore(context, "llmMultiModalFraudScore", condition, 0.7);
            case LLM_SOCIAL_ENGINEERING_CLASSIFICATION -> evaluateScore(context, "llmSocialEngineeringScore", condition, 0.6);
            case LLM_SYNTHETIC_IMAGE_DETECTION -> evaluateScore(context, "llmSyntheticImageScore", condition, 0.7);
            case LLM_TRANSACTION_DESCRIPTION_ANALYSIS -> evaluateScore(context, "llmTransactionDescriptionScore", condition, 0.6);
            default -> false;
        };
    }

    private boolean evaluateBoolean(EvaluationContext context, String key) {
        Map<String, Object> payload = context.getPayload();
        if (payload == null) return false;
        Object value = payload.get(key);
        if (value == null) return false;
        return Boolean.parseBoolean(String.valueOf(value));
    }

    private boolean evaluateScore(EvaluationContext context, String key, RuleCondition condition, double defaultThreshold) {
        Map<String, Object> payload = context.getPayload();
        if (payload == null) return false;
        Object value = payload.get(key);
        if (value == null) return false;
        
        if (value instanceof Boolean) {
            return (Boolean) value;
        }
        
        BigDecimal score = parseBigDecimal(String.valueOf(value), BigDecimal.ZERO);
        BigDecimal threshold = parseBigDecimal(condition.getValueSingle(), new BigDecimal(defaultThreshold));
        return score.compareTo(threshold) > 0;
    }

    private BigDecimal parseBigDecimal(String value, BigDecimal defaultValue) {
        try {
            return new BigDecimal(value);
        } catch (Exception e) {
            return defaultValue;
        }
    }

    @Override
    public String getCategory() {
        return "LLM";
    }
}
