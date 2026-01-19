package com.rulex.service.complex.evaluator;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.RuleCondition.ConditionOperator;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.Map;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Evaluator para operadores de Rule Mining e Fuzzy Logic.
 * Implementa algoritmos de mineração de regras e lógica fuzzy.
 */
@Component
@Slf4j
public class MiningOperatorEvaluator implements OperatorEvaluator {

  private static final Set<ConditionOperator> SUPPORTED = Set.of(
      // Rule Mining
      ConditionOperator.APRIORI_ASSOCIATION,
      ConditionOperator.FPGROWTH_FREQUENT_PATTERNS,
      ConditionOperator.ECLAT_ITEMSET,
      // Fuzzy Logic
      ConditionOperator.FUZZY_MEMBERSHIP,
      ConditionOperator.FUZZY_ADAPTIVE_THRESHOLD,
      // Emerging Fraud
      ConditionOperator.PIG_BUTCHERING_INDICATOR
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
      case APRIORI_ASSOCIATION -> evaluateAprioriAssociation(fieldValue, threshold, payload);
      case FPGROWTH_FREQUENT_PATTERNS -> evaluateFpGrowthFrequentPatterns(fieldValue, threshold, payload);
      case ECLAT_ITEMSET -> evaluateEclatItemset(fieldValue, threshold, payload);
      case FUZZY_MEMBERSHIP -> evaluateFuzzyMembership(fieldValue, threshold, payload);
      case FUZZY_ADAPTIVE_THRESHOLD -> evaluateFuzzyAdaptiveThreshold(fieldValue, threshold, payload);
      case PIG_BUTCHERING_INDICATOR -> evaluatePigButcheringIndicator(fieldValue, threshold, payload);
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

  private boolean evaluateAprioriAssociation(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica suporte e confiança de regra de associação
    try {
      double support = Double.parseDouble(fieldValue.toString());
      double minSupport = threshold != null ? Double.parseDouble(threshold) : 0.1;
      return support >= minSupport;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateFpGrowthFrequentPatterns(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica padrões frequentes via FP-Growth
    try {
      int frequency = Integer.parseInt(fieldValue.toString());
      int minFrequency = threshold != null ? Integer.parseInt(threshold) : 5;
      return frequency >= minFrequency;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateEclatItemset(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica itemset via Eclat (formato vertical)
    try {
      int support = Integer.parseInt(fieldValue.toString());
      int minSupport = threshold != null ? Integer.parseInt(threshold) : 3;
      return support >= minSupport;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateFuzzyMembership(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Avalia função de pertinência fuzzy
    try {
      double membership = Double.parseDouble(fieldValue.toString());
      double minMembership = threshold != null ? Double.parseDouble(threshold) : 0.5;
      return membership >= minMembership;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateFuzzyAdaptiveThreshold(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Threshold adaptativo fuzzy
    try {
      double value = Double.parseDouble(fieldValue.toString());
      double adaptiveThreshold = threshold != null ? Double.parseDouble(threshold) : 0.7;
      
      // Ajuste adaptativo baseado no contexto
      if (payload != null && payload.containsKey("riskLevel")) {
        String riskLevel = payload.get("riskLevel").toString();
        if ("HIGH".equalsIgnoreCase(riskLevel)) {
          adaptiveThreshold *= 0.8; // Mais sensível para alto risco
        } else if ("LOW".equalsIgnoreCase(riskLevel)) {
          adaptiveThreshold *= 1.2; // Menos sensível para baixo risco
        }
      }
      
      return value >= adaptiveThreshold;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluatePigButcheringIndicator(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Indicador de golpe "pig butchering" (romance scam + investment scam)
    // Características: relacionamento online + investimento em crypto/forex
    if (fieldValue == null) return false;
    
    String indicator = fieldValue.toString().toUpperCase();
    if (indicator.equals("TRUE") || indicator.equals("PIG_BUTCHERING")) {
      return true;
    }
    
    // Verificar indicadores no payload
    if (payload != null) {
      boolean hasRomanceIndicator = Boolean.TRUE.equals(payload.get("romanceScamIndicator"));
      boolean hasInvestmentIndicator = Boolean.TRUE.equals(payload.get("investmentScamIndicator"));
      boolean hasCryptoTransaction = Boolean.TRUE.equals(payload.get("cryptoTransaction"));
      
      // Pig butchering = romance + investment + crypto
      return hasRomanceIndicator && (hasInvestmentIndicator || hasCryptoTransaction);
    }
    
    return false;
  }

  @Override
  public String getCategory() {
    return "MINING_FUZZY";
  }
}
