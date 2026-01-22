package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.Map;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class StatisticalBehavioralEvaluator {

  private StatisticalBehavioralEvaluator() {}

  public static boolean evaluateChiSquareDistributionTest(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double significanceLevel = Double.parseDouble(condition.getValueSingle().trim());

      Object pValueObj = payload.get("chi_square_p_value");
      if (pValueObj == null) {
        pValueObj = payload.get("chiSquarePValue");
      }

      if (pValueObj == null) return false;
      double pValue = pValueObj instanceof Number ? ((Number) pValueObj).doubleValue() : 1.0;
      return pValue < significanceLevel; // Reject null hypothesis = anomaly
    } catch (Exception e) {
      log.error("Erro ao avaliar CHI_SQUARE_DISTRIBUTION_TEST: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateKolmogorovSmirnovTest(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double significanceLevel = Double.parseDouble(condition.getValueSingle().trim());

      Object pValueObj = payload.get("ks_test_p_value");
      if (pValueObj == null) {
        pValueObj = payload.get("ksTestPValue");
      }

      if (pValueObj == null) return false;
      double pValue = pValueObj instanceof Number ? ((Number) pValueObj).doubleValue() : 1.0;
      return pValue < significanceLevel;
    } catch (Exception e) {
      log.error("Erro ao avaliar KOLMOGOROV_SMIRNOV_TEST: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateAndersonDarlingTest(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double criticalValue = Double.parseDouble(condition.getValueSingle().trim());

      Object statObj = payload.get("anderson_darling_statistic");
      if (statObj == null) {
        statObj = payload.get("andersonDarlingStatistic");
      }

      if (statObj == null) return false;
      double statistic = statObj instanceof Number ? ((Number) statObj).doubleValue() : 0.0;
      return statistic > criticalValue;
    } catch (Exception e) {
      log.error("Erro ao avaliar ANDERSON_DARLING_TEST: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateTTestAmountDeviation(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double threshold = Double.parseDouble(condition.getValueSingle().trim());

      Object tStatObj = payload.get("t_test_statistic");
      if (tStatObj == null) {
        tStatObj = payload.get("tTestStatistic");
      }

      if (tStatObj == null) return false;
      double tStat = tStatObj instanceof Number ? ((Number) tStatObj).doubleValue() : 0.0;
      return Math.abs(tStat) > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar T_TEST_AMOUNT_DEVIATION: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateMannWhitneyUTest(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double significanceLevel = Double.parseDouble(condition.getValueSingle().trim());

      Object pValueObj = payload.get("mann_whitney_p_value");
      if (pValueObj == null) {
        pValueObj = payload.get("mannWhitneyPValue");
      }

      if (pValueObj == null) return false;
      double pValue = pValueObj instanceof Number ? ((Number) pValueObj).doubleValue() : 1.0;
      return pValue < significanceLevel;
    } catch (Exception e) {
      log.error("Erro ao avaliar MANN_WHITNEY_U_TEST: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateCorrelationAnomaly(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double threshold = Double.parseDouble(condition.getValueSingle().trim());

      Object corrObj = payload.get("correlation_coefficient");
      if (corrObj == null) {
        corrObj = payload.get("correlationCoefficient");
      }

      if (corrObj == null) return false;
      double correlation = corrObj instanceof Number ? ((Number) corrObj).doubleValue() : 0.0;
      return Math.abs(correlation) > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar CORRELATION_ANOMALY: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateRegressionResidualOutlier(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double threshold = Double.parseDouble(condition.getValueSingle().trim());

      Object residualObj = payload.get("standardized_residual");
      if (residualObj == null) {
        residualObj = payload.get("standardizedResidual");
      }

      if (residualObj == null) return false;
      double residual = residualObj instanceof Number ? ((Number) residualObj).doubleValue() : 0.0;
      return Math.abs(residual) > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar REGRESSION_RESIDUAL_OUTLIER: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateVarianceRatioTest(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double threshold = Double.parseDouble(condition.getValueSingle().trim());

      Object fStatObj = payload.get("f_statistic");
      if (fStatObj == null) {
        fStatObj = payload.get("fStatistic");
      }

      if (fStatObj == null) return false;
      double fStat = fStatObj instanceof Number ? ((Number) fStatObj).doubleValue() : 0.0;
      return fStat > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar VARIANCE_RATIO_TEST: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateEntropyScoreAnomaly(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double threshold = Double.parseDouble(condition.getValueSingle().trim());

      Object entropyObj = payload.get("entropy_score");
      if (entropyObj == null) {
        entropyObj = payload.get("entropyScore");
      }

      if (entropyObj == null) return false;
      double entropy = entropyObj instanceof Number ? ((Number) entropyObj).doubleValue() : 1.0;
      return entropy < threshold; // Low entropy is suspicious (too predictable)
    } catch (Exception e) {
      log.error("Erro ao avaliar ENTROPY_SCORE_ANOMALY: {}", e.getMessage());
      return false;
    }
  }
}
