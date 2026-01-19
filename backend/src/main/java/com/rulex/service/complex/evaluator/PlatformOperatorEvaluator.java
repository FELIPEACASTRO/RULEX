package com.rulex.service.complex.evaluator;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.RuleCondition.ConditionOperator;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.Map;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Evaluator para operadores de Platform Best Practices.
 * Implementa funcionalidades inspiradas em FICO, Feedzai, SAS, Stripe, Adyen, PayPal.
 */
@Component
@Slf4j
public class PlatformOperatorEvaluator implements OperatorEvaluator {

  private static final Set<ConditionOperator> SUPPORTED = Set.of(
      // FICO
      ConditionOperator.PLT_BEHAVIOR_SORTED_LISTS,
      ConditionOperator.PLT_BUSINESS_RULES_SCENARIO,
      ConditionOperator.PLT_IDENTITY_RESOLUTION,
      ConditionOperator.PLT_COMPROMISE_MANAGER,
      ConditionOperator.PLT_INTELLIGENCE_NETWORK,
      // Feedzai
      ConditionOperator.PLT_RULES_MODELS_HYBRID,
      ConditionOperator.PLT_BEHAVIORAL_PROFILING,
      ConditionOperator.PLT_NETWORK_ANALYTICS,
      ConditionOperator.PLT_SAR_AUTOMATED,
      // SAS
      ConditionOperator.PLT_DS2_RULE_ENGINE,
      ConditionOperator.PLT_REAL_TIME_DETECTION,
      ConditionOperator.PLT_NETWORK_ENTITY_RESOLUTION,
      ConditionOperator.PLT_SCENARIO_SCORECARD,
      // Stripe
      ConditionOperator.PLT_RADAR_RULE_BACKTESTING,
      ConditionOperator.PLT_RADAR_METADATA_MATCHING,
      ConditionOperator.PLT_RADAR_INLINE_LISTS,
      ConditionOperator.PLT_RADAR_COMPLEX_CONDITIONS,
      // Adyen
      ConditionOperator.PLT_RISK_PROFILE_ASSIGNMENT,
      ConditionOperator.PLT_CUSTOM_RULE_BUILDER,
      ConditionOperator.PLT_RISK_LIST_COMPARISON,
      ConditionOperator.PLT_BACKTESTING_LABELING,
      ConditionOperator.PLT_ML_FRAUD_RISK_OUTCOME,
      // PayPal
      ConditionOperator.PLT_RISK_SCORE_CALCULATION,
      ConditionOperator.PLT_VELOCITY_FILTERS,
      ConditionOperator.PLT_LINKING_VELOCITY,
      ConditionOperator.PLT_BAD_ENTITY_NETWORK,
      ConditionOperator.PLT_REVIEWLIST_QUEUE,
      ConditionOperator.PLT_CONSORTIUM_DATA_CHECK
  );

  @Override
  public Set<ConditionOperator> getSupportedOperators() {
    return SUPPORTED;
  }

  @Override
  public boolean evaluate(RuleCondition condition, EvaluationContext context) {
    if (condition == null || context == null) {
      return false;
    }

    Object fieldValue = getFieldValue(context, condition.getFieldName());
    String threshold = condition.getValueSingle();
    Map<String, Object> payload = context.getPayload();

    return switch (condition.getOperator()) {
      // FICO
      case PLT_BEHAVIOR_SORTED_LISTS -> evaluateBehaviorSortedLists(fieldValue, threshold, payload);
      case PLT_BUSINESS_RULES_SCENARIO -> evaluateBusinessRulesScenario(fieldValue, threshold, payload);
      case PLT_IDENTITY_RESOLUTION -> evaluateIdentityResolution(fieldValue, threshold, payload);
      case PLT_COMPROMISE_MANAGER -> evaluateCompromiseManager(fieldValue, threshold, payload);
      case PLT_INTELLIGENCE_NETWORK -> evaluateIntelligenceNetwork(fieldValue, threshold, payload);
      // Feedzai
      case PLT_RULES_MODELS_HYBRID -> evaluateRulesModelsHybrid(fieldValue, threshold, payload);
      case PLT_BEHAVIORAL_PROFILING -> evaluateBehavioralProfiling(fieldValue, threshold, payload);
      case PLT_NETWORK_ANALYTICS -> evaluateNetworkAnalytics(fieldValue, threshold, payload);
      case PLT_SAR_AUTOMATED -> evaluateSarAutomated(fieldValue, threshold, payload);
      // SAS
      case PLT_DS2_RULE_ENGINE -> evaluateDs2RuleEngine(fieldValue, threshold, payload);
      case PLT_REAL_TIME_DETECTION -> evaluateRealTimeDetection(fieldValue, threshold, payload);
      case PLT_NETWORK_ENTITY_RESOLUTION -> evaluateNetworkEntityResolution(fieldValue, threshold, payload);
      case PLT_SCENARIO_SCORECARD -> evaluateScenarioScorecard(fieldValue, threshold, payload);
      // Stripe
      case PLT_RADAR_RULE_BACKTESTING -> evaluateRadarRuleBacktesting(fieldValue, threshold, payload);
      case PLT_RADAR_METADATA_MATCHING -> evaluateRadarMetadataMatching(fieldValue, threshold, payload);
      case PLT_RADAR_INLINE_LISTS -> evaluateRadarInlineLists(fieldValue, threshold, payload);
      case PLT_RADAR_COMPLEX_CONDITIONS -> evaluateRadarComplexConditions(fieldValue, threshold, payload);
      // Adyen
      case PLT_RISK_PROFILE_ASSIGNMENT -> evaluateRiskProfileAssignment(fieldValue, threshold, payload);
      case PLT_CUSTOM_RULE_BUILDER -> evaluateCustomRuleBuilder(fieldValue, threshold, payload);
      case PLT_RISK_LIST_COMPARISON -> evaluateRiskListComparison(fieldValue, threshold, payload);
      case PLT_BACKTESTING_LABELING -> evaluateBacktestingLabeling(fieldValue, threshold, payload);
      case PLT_ML_FRAUD_RISK_OUTCOME -> evaluateMlFraudRiskOutcome(fieldValue, threshold, payload);
      // PayPal
      case PLT_RISK_SCORE_CALCULATION -> evaluateRiskScoreCalculation(fieldValue, threshold, payload);
      case PLT_VELOCITY_FILTERS -> evaluateVelocityFilters(fieldValue, threshold, payload);
      case PLT_LINKING_VELOCITY -> evaluateLinkingVelocity(fieldValue, threshold, payload);
      case PLT_BAD_ENTITY_NETWORK -> evaluateBadEntityNetwork(fieldValue, threshold, payload);
      case PLT_REVIEWLIST_QUEUE -> evaluateReviewlistQueue(fieldValue, threshold, payload);
      case PLT_CONSORTIUM_DATA_CHECK -> evaluateConsortiumDataCheck(fieldValue, threshold, payload);
      default -> false;
    };
  }

  private Object getFieldValue(EvaluationContext context, String fieldName) {
    if (context == null || fieldName == null) return null;
    Map<String, Object> payload = context.getPayload();
    if (payload != null && payload.containsKey(fieldName)) {
      return payload.get(fieldName);
    }
    return null;
  }

  // ========== FICO ==========

  private boolean evaluateBehaviorSortedLists(Object fieldValue, String threshold, Map<String, Object> payload) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateBusinessRulesScenario(Object fieldValue, String threshold, Map<String, Object> payload) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateIdentityResolution(Object fieldValue, String threshold, Map<String, Object> payload) {
    try {
      double confidence = Double.parseDouble(fieldValue.toString());
      double minConfidence = threshold != null ? Double.parseDouble(threshold) : 0.8;
      return confidence >= minConfidence;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateCompromiseManager(Object fieldValue, String threshold, Map<String, Object> payload) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "COMPROMISED".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateIntelligenceNetwork(Object fieldValue, String threshold, Map<String, Object> payload) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  // ========== Feedzai ==========

  private boolean evaluateRulesModelsHybrid(Object fieldValue, String threshold, Map<String, Object> payload) {
    try {
      double score = Double.parseDouble(fieldValue.toString());
      double minScore = threshold != null ? Double.parseDouble(threshold) : 0.5;
      return score >= minScore;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateBehavioralProfiling(Object fieldValue, String threshold, Map<String, Object> payload) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "ANOMALY".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateNetworkAnalytics(Object fieldValue, String threshold, Map<String, Object> payload) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateSarAutomated(Object fieldValue, String threshold, Map<String, Object> payload) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "SAR_REQUIRED".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  // ========== SAS ==========

  private boolean evaluateDs2RuleEngine(Object fieldValue, String threshold, Map<String, Object> payload) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateRealTimeDetection(Object fieldValue, String threshold, Map<String, Object> payload) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "DETECTED".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateNetworkEntityResolution(Object fieldValue, String threshold, Map<String, Object> payload) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateScenarioScorecard(Object fieldValue, String threshold, Map<String, Object> payload) {
    try {
      double score = Double.parseDouble(fieldValue.toString());
      double minScore = threshold != null ? Double.parseDouble(threshold) : 50;
      return score >= minScore;
    } catch (Exception e) {
      return false;
    }
  }

  // ========== Stripe ==========

  private boolean evaluateRadarRuleBacktesting(Object fieldValue, String threshold, Map<String, Object> payload) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateRadarMetadataMatching(Object fieldValue, String threshold, Map<String, Object> payload) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "MATCH".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateRadarInlineLists(Object fieldValue, String threshold, Map<String, Object> payload) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "IN_LIST".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateRadarComplexConditions(Object fieldValue, String threshold, Map<String, Object> payload) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  // ========== Adyen ==========

  private boolean evaluateRiskProfileAssignment(Object fieldValue, String threshold, Map<String, Object> payload) {
    if (fieldValue == null) return false;
    String profile = fieldValue.toString().toUpperCase();
    String expectedProfile = threshold != null ? threshold.toUpperCase() : "HIGH";
    return profile.equals(expectedProfile);
  }

  private boolean evaluateCustomRuleBuilder(Object fieldValue, String threshold, Map<String, Object> payload) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateRiskListComparison(Object fieldValue, String threshold, Map<String, Object> payload) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "MATCH".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateBacktestingLabeling(Object fieldValue, String threshold, Map<String, Object> payload) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateMlFraudRiskOutcome(Object fieldValue, String threshold, Map<String, Object> payload) {
    try {
      double riskScore = Double.parseDouble(fieldValue.toString());
      double minRisk = threshold != null ? Double.parseDouble(threshold) : 0.7;
      return riskScore >= minRisk;
    } catch (Exception e) {
      return false;
    }
  }

  // ========== PayPal ==========

  private boolean evaluateRiskScoreCalculation(Object fieldValue, String threshold, Map<String, Object> payload) {
    try {
      double score = Double.parseDouble(fieldValue.toString());
      double minScore = threshold != null ? Double.parseDouble(threshold) : 50;
      return score >= minScore;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateVelocityFilters(Object fieldValue, String threshold, Map<String, Object> payload) {
    try {
      int count = Integer.parseInt(fieldValue.toString());
      int maxCount = threshold != null ? Integer.parseInt(threshold) : 10;
      return count >= maxCount;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateLinkingVelocity(Object fieldValue, String threshold, Map<String, Object> payload) {
    try {
      int links = Integer.parseInt(fieldValue.toString());
      int maxLinks = threshold != null ? Integer.parseInt(threshold) : 5;
      return links >= maxLinks;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateBadEntityNetwork(Object fieldValue, String threshold, Map<String, Object> payload) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "BAD".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateReviewlistQueue(Object fieldValue, String threshold, Map<String, Object> payload) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "REVIEW".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateConsortiumDataCheck(Object fieldValue, String threshold, Map<String, Object> payload) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "FLAGGED".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  @Override
  public String getCategory() {
    return "PLATFORM";
  }
}
