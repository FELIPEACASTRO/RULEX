package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.exception.UnsupportedOperatorException;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;

public final class ScaPlannedEvaluator {

  private ScaPlannedEvaluator() {}

  public static boolean evaluateScaChallengeMandatory(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.SCA_CHALLENGE_MANDATORY,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  public static boolean evaluateScaContactlessExemption(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.SCA_CONTACTLESS_EXEMPTION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  public static boolean evaluateScaCorporatePayment(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.SCA_CORPORATE_PAYMENT,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  public static boolean evaluateScaDynamic3dsRouting(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.SCA_DYNAMIC_3DS_ROUTING,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  public static boolean evaluateScaFraudRateMonitoring(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.SCA_FRAUD_RATE_MONITORING,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  public static boolean evaluateScaLiabilityShift(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.SCA_LIABILITY_SHIFT,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  public static boolean evaluateScaLowValueExemption(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.SCA_LOW_VALUE_EXEMPTION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  public static boolean evaluateScaMerchantInitiated(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.SCA_MERCHANT_INITIATED,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  public static boolean evaluateScaRecurringTransaction(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.SCA_RECURRING_TRANSACTION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  public static boolean evaluateScaSecureCorporateProtocol(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.SCA_SECURE_CORPORATE_PROTOCOL,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  public static boolean evaluateScaTraExemption(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.SCA_TRA_EXEMPTION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  public static boolean evaluateScaTrustedBeneficiary(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.SCA_TRUSTED_BENEFICIARY,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }
}
