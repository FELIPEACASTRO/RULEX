package com.rulex.service.complex.evaluator;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.Map;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Evaluator para operadores SCA (Strong Customer Authentication).
 * Implementa isenções e regras PSD2/PSD3.
 */
@Component
@Slf4j
public class SCAOperatorEvaluator implements OperatorEvaluator {

  private static final Set<ConditionOperator> SUPPORTED = Set.of(
      ConditionOperator.SCA_LOW_VALUE_EXEMPTION,
      ConditionOperator.SCA_CONTACTLESS_EXEMPTION,
      ConditionOperator.SCA_TRA_EXEMPTION,
      ConditionOperator.SCA_TRUSTED_BENEFICIARY,
      ConditionOperator.SCA_RECURRING_TRANSACTION,
      ConditionOperator.SCA_MERCHANT_INITIATED,
      ConditionOperator.SCA_CORPORATE_PAYMENT,
      ConditionOperator.SCA_SECURE_CORPORATE_PROTOCOL,
      ConditionOperator.SCA_LIABILITY_SHIFT,
      ConditionOperator.SCA_DYNAMIC_3DS_ROUTING,
      ConditionOperator.SCA_FRAUD_RATE_MONITORING,
      ConditionOperator.SCA_CHALLENGE_MANDATORY
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
      case SCA_LOW_VALUE_EXEMPTION -> evaluateLowValueExemption(fieldValue, threshold, payload);
      case SCA_CONTACTLESS_EXEMPTION -> evaluateContactlessExemption(fieldValue, threshold, payload);
      case SCA_TRA_EXEMPTION -> evaluateTraExemption(fieldValue, threshold, payload);
      case SCA_TRUSTED_BENEFICIARY -> evaluateTrustedBeneficiary(fieldValue, threshold, payload);
      case SCA_RECURRING_TRANSACTION -> evaluateRecurringTransaction(fieldValue, threshold, payload);
      case SCA_MERCHANT_INITIATED -> evaluateMerchantInitiated(fieldValue, threshold, payload);
      case SCA_CORPORATE_PAYMENT -> evaluateCorporatePayment(fieldValue, threshold, payload);
      case SCA_SECURE_CORPORATE_PROTOCOL -> evaluateSecureCorporateProtocol(fieldValue, threshold, payload);
      case SCA_LIABILITY_SHIFT -> evaluateLiabilityShift(fieldValue, threshold, payload);
      case SCA_DYNAMIC_3DS_ROUTING -> evaluateDynamic3dsRouting(fieldValue, threshold, payload);
      case SCA_FRAUD_RATE_MONITORING -> evaluateFraudRateMonitoring(fieldValue, threshold, payload);
      case SCA_CHALLENGE_MANDATORY -> evaluateChallengeMandatory(fieldValue, threshold, payload);
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

  private boolean evaluateLowValueExemption(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Isenção para valores baixos (≤€30)
    try {
      double amount = Double.parseDouble(fieldValue.toString());
      double maxAmount = threshold != null ? Double.parseDouble(threshold) : 30.0;
      return amount <= maxAmount;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateContactlessExemption(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Isenção para contactless (≤€50)
    try {
      double amount = Double.parseDouble(fieldValue.toString());
      double maxAmount = threshold != null ? Double.parseDouble(threshold) : 50.0;
      boolean isContactless = payload != null && Boolean.TRUE.equals(payload.get("isContactless"));
      return isContactless && amount <= maxAmount;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateTraExemption(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Isenção por Transaction Risk Analysis
    try {
      double riskScore = Double.parseDouble(fieldValue.toString());
      double maxRisk = threshold != null ? Double.parseDouble(threshold) : 0.3;
      return riskScore <= maxRisk;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateTrustedBeneficiary(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Beneficiário confiável
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "TRUSTED".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateRecurringTransaction(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Transação recorrente
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "RECURRING".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateMerchantInitiated(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Transação iniciada pelo merchant
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "MIT".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateCorporatePayment(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Pagamento corporativo
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "CORPORATE".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateSecureCorporateProtocol(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Protocolo corporativo seguro
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateLiabilityShift(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Mudança de responsabilidade
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "SHIFT".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateDynamic3dsRouting(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Roteamento dinâmico 3DS
    if (fieldValue == null) return false;
    String routing = fieldValue.toString().toUpperCase();
    return routing.equals("FRICTIONLESS") || routing.equals("CHALLENGE") || routing.equals("EXEMPT");
  }

  private boolean evaluateFraudRateMonitoring(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Monitoramento de taxa de fraude do PSP
    try {
      double fraudRate = Double.parseDouble(fieldValue.toString());
      double maxRate = threshold != null ? Double.parseDouble(threshold) : 0.13; // 13 bps
      return fraudRate <= maxRate;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateChallengeMandatory(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Challenge obrigatório
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "MANDATORY".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  @Override
  public String getCategory() {
    return "SCA_PSD2";
  }
}
