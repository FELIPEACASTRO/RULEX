package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import com.rulex.service.complex.context.FieldValueExtractor;
import com.rulex.service.complex.parsing.BooleanParser;
import com.rulex.service.complex.parsing.StringNormalizer;
import java.math.BigDecimal;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class RegulatoryComplianceEvaluator {

  private RegulatoryComplianceEvaluator() {}

  public static boolean evaluateScaExemptionTra(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      double maxRisk = Double.parseDouble(condition.getValueSingle().trim());
      Object riskObj = context.getPayload().get("tra_risk_score");
      if (riskObj == null) riskObj = context.getPayload().get("traRiskScore");
      double risk = riskObj instanceof Number ? ((Number) riskObj).doubleValue() : Double.MAX_VALUE;
      return risk <= maxRisk;
    } catch (Exception e) {
      log.error("Erro ao avaliar SCA_EXEMPTION_TRA: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateScaExemptionLowValue(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getTransactionRequest() == null) return false;
      BigDecimal maxAmount = new BigDecimal(condition.getValueSingle().trim());
      BigDecimal amount = context.getTransactionRequest().getTransactionAmount();
      if (amount == null) return false;
      return amount.compareTo(maxAmount) <= 0;
    } catch (Exception e) {
      log.error("Erro ao avaliar SCA_EXEMPTION_LOW_VALUE: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateScaExemptionTrustedBeneficiary(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object trustedObj = context.getPayload().get("trusted_beneficiary");
      if (trustedObj == null) trustedObj = context.getPayload().get("trustedBeneficiary");
      if (trustedObj == null)
        trustedObj =
            FieldValueExtractor.getFieldValue(
                condition.getFieldName(), condition.getFieldPath(), context);
      return Boolean.TRUE.equals(BooleanParser.toBoolean(trustedObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar SCA_EXEMPTION_TRUSTED_BENEFICIARY: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateScaExemptionRecurring(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object recurringObj = context.getPayload().get("recurring_payment");
      if (recurringObj == null) recurringObj = context.getPayload().get("recurringPayment");
      if (recurringObj == null)
        recurringObj =
            FieldValueExtractor.getFieldValue(
                condition.getFieldName(), condition.getFieldPath(), context);
      return Boolean.TRUE.equals(BooleanParser.toBoolean(recurringObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar SCA_EXEMPTION_RECURRING: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluatePsd3CopNameMatch(
      RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts =
          (condition.getValueSingle() == null ? "" : condition.getValueSingle()).split(":");
      if (parts.length < 2) {
        // fallback: usa fieldName vs payload[other_name]
        parts = new String[] {"other_name", "90"};
      }
      String otherField = parts[0].trim();
      int threshold = Integer.parseInt(parts[1].trim());

      Object v1 =
          FieldValueExtractor.getFieldValue(
              condition.getFieldName(), condition.getFieldPath(), context);
      Object v2 = FieldValueExtractor.getFieldValue(otherField, null, context);
      if (v1 == null || v2 == null) return false;

      String s1 = StringNormalizer.normalizeForMatch(String.valueOf(v1));
      String s2 = StringNormalizer.normalizeForMatch(String.valueOf(v2));
      if (s1.isBlank() || s2.isBlank()) return false;

      return NameSimilarityEvaluator.calculateSimilarity(s1, s2) >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar PSD3_COP_NAME_MATCH: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateDoraIncidentSeverity(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      int minSeverity = Integer.parseInt(condition.getValueSingle().trim());
      Object sevObj = context.getPayload().get("incident_severity");
      if (sevObj == null) sevObj = context.getPayload().get("incidentSeverity");
      int sev = sevObj instanceof Number ? ((Number) sevObj).intValue() : 0;
      return sev >= minSeverity;
    } catch (Exception e) {
      log.error("Erro ao avaliar DORA_INCIDENT_SEVERITY: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateEidasAssuranceLevel(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      String required =
          (condition.getValueSingle() == null ? "" : condition.getValueSingle()).trim();
      int requiredLevel = mapEidasLevel(required);
      Object levelObj = context.getPayload().get("eidas_assurance_level");
      if (levelObj == null) levelObj = context.getPayload().get("eidasAssuranceLevel");
      if (levelObj == null)
        levelObj =
            FieldValueExtractor.getFieldValue(
                condition.getFieldName(), condition.getFieldPath(), context);
      if (levelObj == null) return false;
      int actualLevel = mapEidasLevel(String.valueOf(levelObj));
      return actualLevel >= requiredLevel;
    } catch (Exception e) {
      log.error("Erro ao avaliar EIDAS_ASSURANCE_LEVEL: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateGdprDataRetentionCheck(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      int maxDays = Integer.parseInt(condition.getValueSingle().trim());
      Object ageObj = context.getPayload().get("data_age_days");
      if (ageObj == null) ageObj = context.getPayload().get("dataAgeDays");
      int ageDays = ageObj instanceof Number ? ((Number) ageObj).intValue() : 0;
      // Retorna true quando há violação (dados mais antigos que o permitido)
      return ageDays > maxDays;
    } catch (Exception e) {
      log.error("Erro ao avaliar GDPR_DATA_RETENTION_CHECK: {}", e.getMessage());
      return false;
    }
  }

  private static int mapEidasLevel(String level) {
    if (level == null) return 0;
    String v = StringNormalizer.normalizeForMatch(level);
    if (v.isBlank()) return 0;
    return switch (v) {
      case "low" -> 1;
      case "substantial" -> 2;
      case "high" -> 3;
      default -> {
        try {
          yield Integer.parseInt(v);
        } catch (NumberFormatException e) {
          yield 0;
        }
      }
    };
  }
}
