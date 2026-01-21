package com.rulex.service.complex.evaluator;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.Map;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Evaluator para operadores Basel III (Operational Risk).
 * Implementa cálculos e verificações de risco operacional.
 */
@Component
@Slf4j
public class BaselOperatorEvaluator implements OperatorEvaluator {

  private static final Set<ConditionOperator> SUPPORTED = Set.of(
      ConditionOperator.BSL_BUSINESS_INDICATOR,
      ConditionOperator.BSL_BUSINESS_INDICATOR_COMPONENT,
      ConditionOperator.BSL_INTERNAL_LOSS_MULTIPLIER,
      ConditionOperator.BSL_LOSS_DATA_COLLECTION,
      ConditionOperator.BSL_LOSS_EXCLUSION_APPROVAL,
      ConditionOperator.BSL_BUCKET_CLASSIFICATION,
      ConditionOperator.BSL_MARGINAL_COEFFICIENT,
      ConditionOperator.BSL_LOSS_THRESHOLD_SETTING,
      ConditionOperator.BSL_RETENTION_PERIOD,
      ConditionOperator.BSL_RISK_GOVERNANCE,
      ConditionOperator.BSL_LOSS_EVENT_REPORTING,
      ConditionOperator.BSL_CONTROL_DEFICIENCY,
      ConditionOperator.BSL_KRI_MONITORING,
      ConditionOperator.BSL_SCENARIO_ANALYSIS
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
      case BSL_BUSINESS_INDICATOR -> evaluateBusinessIndicator(fieldValue, threshold, payload);
      case BSL_BUSINESS_INDICATOR_COMPONENT -> evaluateBusinessIndicatorComponent(fieldValue, threshold, payload);
      case BSL_INTERNAL_LOSS_MULTIPLIER -> evaluateInternalLossMultiplier(fieldValue, threshold, payload);
      case BSL_LOSS_DATA_COLLECTION -> evaluateLossDataCollection(fieldValue, threshold, payload);
      case BSL_LOSS_EXCLUSION_APPROVAL -> evaluateLossExclusionApproval(fieldValue, threshold, payload);
      case BSL_BUCKET_CLASSIFICATION -> evaluateBucketClassification(fieldValue, threshold, payload);
      case BSL_MARGINAL_COEFFICIENT -> evaluateMarginalCoefficient(fieldValue, threshold, payload);
      case BSL_LOSS_THRESHOLD_SETTING -> evaluateLossThresholdSetting(fieldValue, threshold, payload);
      case BSL_RETENTION_PERIOD -> evaluateRetentionPeriod(fieldValue, threshold, payload);
      case BSL_RISK_GOVERNANCE -> evaluateRiskGovernance(fieldValue, threshold, payload);
      case BSL_LOSS_EVENT_REPORTING -> evaluateLossEventReporting(fieldValue, threshold, payload);
      case BSL_CONTROL_DEFICIENCY -> evaluateControlDeficiency(fieldValue, threshold, payload);
      case BSL_KRI_MONITORING -> evaluateKriMonitoring(fieldValue, threshold, payload);
      case BSL_SCENARIO_ANALYSIS -> evaluateScenarioAnalysis(fieldValue, threshold, payload);
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

  private boolean evaluateBusinessIndicator(Object fieldValue, String threshold, Map<String, Object> payload) {
    // BI = ILDC + SC + FC (Interest, Lease and Dividend Component + Services Component + Financial Component)
    try {
      double bi = Double.parseDouble(fieldValue.toString());
      double minBi = threshold != null ? Double.parseDouble(threshold) : 0;
      return bi >= minBi;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateBusinessIndicatorComponent(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica componente específico do BI (ILDC, SC ou FC)
    try {
      double component = Double.parseDouble(fieldValue.toString());
      double minComponent = threshold != null ? Double.parseDouble(threshold) : 0;
      return component >= minComponent;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateInternalLossMultiplier(Object fieldValue, String threshold, Map<String, Object> payload) {
    // ILM = Internal Loss Multiplier
    try {
      double ilm = Double.parseDouble(fieldValue.toString());
      double maxIlm = threshold != null ? Double.parseDouble(threshold) : 1.0;
      return ilm <= maxIlm;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateLossDataCollection(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica se coleta de dados de perda está completa
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "COMPLETE".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateLossExclusionApproval(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica aprovação de exclusão de perda
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "APPROVED".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateBucketClassification(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Classificação em buckets: €1bn, €3bn, €30bn
    try {
      double bi = Double.parseDouble(fieldValue.toString());
      String bucket = threshold != null ? threshold : "1BN";

      return switch (bucket.toUpperCase()) {
        case "1BN" -> bi <= 1_000_000_000L;
        case "3BN" -> bi > 1_000_000_000L && bi <= 3_000_000_000L;
        case "30BN" -> bi > 3_000_000_000L && bi <= 30_000_000_000L;
        case "ABOVE_30BN" -> bi > 30_000_000_000L;
        default -> false;
      };
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateMarginalCoefficient(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Coeficiente marginal: 12%, 15% ou 18%
    try {
      double coefficient = Double.parseDouble(fieldValue.toString());
      double expectedCoeff = threshold != null ? Double.parseDouble(threshold) : 0.12;
      return Math.abs(coefficient - expectedCoeff) < 0.001;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateLossThresholdSetting(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica configuração de threshold de perda (default €20k)
    try {
      double lossThreshold = Double.parseDouble(fieldValue.toString());
      double minThreshold = threshold != null ? Double.parseDouble(threshold) : 20000;
      return lossThreshold >= minThreshold;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateRetentionPeriod(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica período de retenção (mínimo 10 anos)
    try {
      int years = Integer.parseInt(fieldValue.toString());
      int minYears = threshold != null ? Integer.parseInt(threshold) : 10;
      return years >= minYears;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateRiskGovernance(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica framework de governança de risco
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "COMPLIANT".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateLossEventReporting(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica reporte de evento de perda
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "REPORTED".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateControlDeficiency(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica deficiência de controle
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "DEFICIENT".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateKriMonitoring(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica monitoramento de KRI (Key Risk Indicator)
    try {
      double kriValue = Double.parseDouble(fieldValue.toString());
      double kriThreshold = threshold != null ? Double.parseDouble(threshold) : 1.0;
      return kriValue > kriThreshold;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateScenarioAnalysis(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica análise de cenário
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "ANALYZED".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  @Override
  public String getCategory() {
    return "BASEL_III";
  }
}
