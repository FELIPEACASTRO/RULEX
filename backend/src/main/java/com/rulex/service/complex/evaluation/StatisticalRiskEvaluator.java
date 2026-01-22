package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.VelocityService;
import com.rulex.service.VelocityServiceFacade;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.math.BigDecimal;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class StatisticalRiskEvaluator {

  private StatisticalRiskEvaluator() {}

  public static boolean evaluateZScoreGt(
      RuleCondition condition,
      EvaluationContext context,
      VelocityServiceFacade velocityServiceFacade) {
    try {
      if (context.getPayload() == null && context.getTransactionRequest() == null) return false;
      double threshold = Double.parseDouble(condition.getValueSingle().trim());
      Object zObj = context.getPayload() != null ? context.getPayload().get("z_score") : null;
      if (zObj == null && context.getPayload() != null) {
        zObj = context.getPayload().get("zScore");
      }
      if (zObj instanceof Number) {
        return ((Number) zObj).doubleValue() > threshold;
      }
      if (context.getTransactionRequest() == null) return false;
      var stats =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(),
              VelocityService.KeyType.PAN,
              VelocityService.TimeWindow.HOUR_24);
      BigDecimal avg = stats.getAvgAmount();
      BigDecimal std = stats.getStdDevAmount();
      BigDecimal amount = context.getTransactionRequest().getTransactionAmount();
      if (avg == null || std == null || std.compareTo(BigDecimal.ZERO) == 0 || amount == null)
        return false;
      BigDecimal z = amount.subtract(avg).divide(std, 8, java.math.RoundingMode.HALF_UP).abs();
      return z.compareTo(BigDecimal.valueOf(threshold)) > 0;
    } catch (Exception e) {
      log.error("Erro ao avaliar Z_SCORE_GT: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateStandardDeviationGt(
      RuleCondition condition,
      EvaluationContext context,
      VelocityServiceFacade velocityServiceFacade) {
    try {
      if (context.getTransactionRequest() == null) return false;
      BigDecimal threshold = new BigDecimal(condition.getValueSingle().trim());
      var stats =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(),
              VelocityService.KeyType.PAN,
              VelocityService.TimeWindow.HOUR_24);
      BigDecimal std = stats.getStdDevAmount();
      if (std == null) return false;
      return std.compareTo(threshold) > 0;
    } catch (Exception e) {
      log.error("Erro ao avaliar STANDARD_DEVIATION_GT: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluatePercentileGt(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      double threshold = Double.parseDouble(condition.getValueSingle().trim());
      Object pctObj = context.getPayload().get("current_percentile");
      if (pctObj == null) pctObj = context.getPayload().get("currentPercentile");
      double pct = pctObj instanceof Number ? ((Number) pctObj).doubleValue() : 0.0;
      return pct > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar PERCENTILE_GT: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateCoefficientVariationGt(
      RuleCondition condition,
      EvaluationContext context,
      VelocityServiceFacade velocityServiceFacade) {
    try {
      if (context.getTransactionRequest() == null) return false;
      double threshold = Double.parseDouble(condition.getValueSingle().trim());
      var stats =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(),
              VelocityService.KeyType.PAN,
              VelocityService.TimeWindow.HOUR_24);
      BigDecimal avg = stats.getAvgAmount();
      BigDecimal std = stats.getStdDevAmount();
      if (avg == null || std == null || avg.compareTo(BigDecimal.ZERO) == 0) return false;
      BigDecimal cv = std.divide(avg.abs(), 8, java.math.RoundingMode.HALF_UP);
      return cv.compareTo(BigDecimal.valueOf(threshold)) > 0;
    } catch (Exception e) {
      log.error("Erro ao avaliar COEFFICIENT_VARIATION_GT: {}", e.getMessage());
      return false;
    }
  }
}
