package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.Map;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class SimpleStatsEvaluator {

  private SimpleStatsEvaluator() {}

  public static boolean evaluateBenfordLawDeviation(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      double threshold = Double.parseDouble(condition.getValueSingle().trim());
      Object devObj = context.getPayload().get("benford_deviation");
      if (devObj == null) devObj = context.getPayload().get("benfordDeviation");
      double deviation = devObj instanceof Number ? ((Number) devObj).doubleValue() : 0.0;
      return deviation > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar BENFORD_LAW_DEVIATION: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateSkewnessKurtosisAnomaly(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double threshold = Double.parseDouble(condition.getValueSingle().trim());

      Object skewnessObj = payload.get("skewness");
      Object kurtosisObj = payload.get("kurtosis");

      double skewness = skewnessObj instanceof Number ? ((Number) skewnessObj).doubleValue() : 0.0;
      double kurtosis =
          kurtosisObj instanceof Number ? ((Number) kurtosisObj).doubleValue() : 0.0;

      // Anomaly if either is beyond threshold
      return Math.abs(skewness) > threshold || Math.abs(kurtosis - 3) > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar SKEWNESS_KURTOSIS_ANOMALY: {}", e.getMessage());
      return false;
    }
  }
}
