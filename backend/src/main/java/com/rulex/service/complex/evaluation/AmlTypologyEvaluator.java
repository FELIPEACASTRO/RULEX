package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import com.rulex.service.complex.context.FieldValueExtractor;
import com.rulex.service.complex.parsing.BooleanParser;
import java.math.BigDecimal;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class AmlTypologyEvaluator {

  private AmlTypologyEvaluator() {}

  public static boolean evaluateStructuringDetection(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      // Formato: "maxSingleAmount|minCount" (ex: "1000|5")
      String[] parts =
          (condition.getValueSingle() == null ? "" : condition.getValueSingle()).split("\\|");
      BigDecimal maxSingle =
          parts.length >= 1 && !parts[0].isBlank()
              ? new BigDecimal(parts[0].trim())
              : new BigDecimal("1000");
      int minCount =
          parts.length >= 2 && !parts[1].isBlank() ? Integer.parseInt(parts[1].trim()) : 5;

      Object countObj = context.getPayload().get("cash_like_tx_count_below_threshold");
      if (countObj == null) countObj = context.getPayload().get("structuringCount");
      int count = countObj instanceof Number ? ((Number) countObj).intValue() : 0;

      BigDecimal amount = null;
      if (context.getTransactionRequest() != null) {
        amount = context.getTransactionRequest().getTransactionAmount();
      }
      boolean currentBelow = amount != null && amount.compareTo(maxSingle) <= 0;

      return currentBelow && count >= minCount;
    } catch (Exception e) {
      log.error("Erro ao avaliar STRUCTURING_DETECTION: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateLayeringPattern(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      int minHops = Integer.parseInt(condition.getValueSingle().trim());
      Object hopsObj = context.getPayload().get("layering_hops");
      if (hopsObj == null) hopsObj = context.getPayload().get("layeringHops");
      int hops = hopsObj instanceof Number ? ((Number) hopsObj).intValue() : 0;
      return hops >= minHops;
    } catch (Exception e) {
      log.error("Erro ao avaliar LAYERING_PATTERN: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateRapidMovement(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      int maxMinutes = Integer.parseInt(condition.getValueSingle().trim());
      Object minutesObj = context.getPayload().get("minutes_between_in_out");
      if (minutesObj == null) minutesObj = context.getPayload().get("minutesBetweenInOut");
      int minutes =
          minutesObj instanceof Number ? ((Number) minutesObj).intValue() : Integer.MAX_VALUE;
      return minutes <= maxMinutes;
    } catch (Exception e) {
      log.error("Erro ao avaliar RAPID_MOVEMENT: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateIntegrationPattern(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object flagObj = context.getPayload().get("integration_detected");
      if (flagObj == null) flagObj = context.getPayload().get("integrationDetected");
      if (flagObj == null)
        flagObj =
            FieldValueExtractor.getFieldValue(
                condition.getFieldName(), condition.getFieldPath(), context);
      return Boolean.TRUE.equals(BooleanParser.toBoolean(flagObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar INTEGRATION_PATTERN: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateCashIntensiveRatio(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      double threshold = Double.parseDouble(condition.getValueSingle().trim());
      Object ratioObj = context.getPayload().get("cash_intensive_ratio");
      if (ratioObj == null) ratioObj = context.getPayload().get("cashIntensiveRatio");
      double ratio = ratioObj instanceof Number ? ((Number) ratioObj).doubleValue() : 0.0;
      return ratio > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar CASH_INTENSIVE_RATIO: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateUnusualBusinessPattern(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object flagObj = context.getPayload().get("unusual_business_pattern");
      if (flagObj == null) flagObj = context.getPayload().get("unusualBusinessPattern");
      if (flagObj == null)
        flagObj =
            FieldValueExtractor.getFieldValue(
                condition.getFieldName(), condition.getFieldPath(), context);
      return Boolean.TRUE.equals(BooleanParser.toBoolean(flagObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar UNUSUAL_BUSINESS_PATTERN: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateShellCompanyIndicator(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object flagObj = context.getPayload().get("shell_company_indicator");
      if (flagObj == null) flagObj = context.getPayload().get("shellCompanyIndicator");
      if (flagObj != null) return Boolean.TRUE.equals(BooleanParser.toBoolean(flagObj));

      // Heurística: empresa muito nova + sem funcionários.
      int maxAgeMonths = Integer.parseInt(condition.getValueSingle().trim());
      Object ageObj = context.getPayload().get("company_age_months");
      if (ageObj == null) ageObj = context.getPayload().get("companyAgeMonths");
      int ageMonths = ageObj instanceof Number ? ((Number) ageObj).intValue() : Integer.MAX_VALUE;

      Object empObj = context.getPayload().get("employee_count");
      if (empObj == null) empObj = context.getPayload().get("employeeCount");
      int employees = empObj instanceof Number ? ((Number) empObj).intValue() : 1;

      return ageMonths <= maxAgeMonths && employees <= 0;
    } catch (Exception e) {
      log.error("Erro ao avaliar SHELL_COMPANY_INDICATOR: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateTradeBasedMlIndicator(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object flagObj = context.getPayload().get("trade_based_ml_indicator");
      if (flagObj == null) flagObj = context.getPayload().get("tradeBasedMlIndicator");
      if (flagObj != null) return Boolean.TRUE.equals(BooleanParser.toBoolean(flagObj));

      double threshold = Double.parseDouble(condition.getValueSingle().trim());
      Object mismatchObj = context.getPayload().get("invoice_mismatch_pct");
      if (mismatchObj == null) mismatchObj = context.getPayload().get("invoiceMismatchPct");
      double mismatch = mismatchObj instanceof Number ? ((Number) mismatchObj).doubleValue() : 0.0;
      return mismatch > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar TRADE_BASED_ML_INDICATOR: {}", e.getMessage());
      return false;
    }
  }
}
