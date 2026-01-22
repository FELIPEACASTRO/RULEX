package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.exception.UnsupportedOperatorException;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;

public final class StatisticalPlannedEvaluator {

  private StatisticalPlannedEvaluator() {}

  public static boolean evaluateStatKruskalWallisTest(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.STAT_KRUSKAL_WALLIS_TEST,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  public static boolean evaluateStatAnovaFTest(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.STAT_ANOVA_F_TEST,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  public static boolean evaluateStatIsolationForestScore(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.STAT_ISOLATION_FOREST_SCORE,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  public static boolean evaluateStatLocalOutlierFactor(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.STAT_LOCAL_OUTLIER_FACTOR,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  public static boolean evaluateStatOneClassSvmBoundary(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.STAT_ONE_CLASS_SVM_BOUNDARY,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  public static boolean evaluateStatKmeansClusterDistance(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.STAT_KMEANS_CLUSTER_DISTANCE,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  public static boolean evaluateStatDbscanNoiseDetection(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.STAT_DBSCAN_NOISE_DETECTION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  public static boolean evaluateStatGmmProbability(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.STAT_GMM_PROBABILITY,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  public static boolean evaluateStatMahalanobisDistance(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.STAT_MAHALANOBIS_DISTANCE,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  public static boolean evaluateStatGrubbsTest(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.STAT_GRUBBS_TEST,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  public static boolean evaluateStatDixonQTest(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.STAT_DIXON_Q_TEST,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  public static boolean evaluateStatShapiroWilkTest(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.STAT_SHAPIRO_WILK_TEST,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  public static boolean evaluateStatLeveneTest(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.STAT_LEVENE_TEST,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  public static boolean evaluateStatWelchTTest(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.STAT_WELCH_T_TEST,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  public static boolean evaluateStatBootstrapConfidenceInterval(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.STAT_BOOTSTRAP_CONFIDENCE_INTERVAL,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }
}
