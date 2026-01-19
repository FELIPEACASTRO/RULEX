package com.rulex.service.complex.evaluator;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.RuleCondition.ConditionOperator;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;
import java.util.Map;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Avaliador para operadores estatísticos.
 * Implementa testes estatísticos para detecção de anomalias.
 */
@Component
@Slf4j
public class StatisticalOperatorEvaluator implements OperatorEvaluator {

    private static final Set<ConditionOperator> SUPPORTED = Set.of(
        ConditionOperator.STAT_ANOVA_F_TEST,
        ConditionOperator.STAT_BOOTSTRAP_CONFIDENCE_INTERVAL,
        ConditionOperator.STAT_DBSCAN_NOISE_DETECTION,
        ConditionOperator.STAT_DIXON_Q_TEST,
        ConditionOperator.STAT_GMM_PROBABILITY,
        ConditionOperator.STAT_GRUBBS_TEST,
        ConditionOperator.STAT_ISOLATION_FOREST_SCORE,
        ConditionOperator.STAT_KMEANS_CLUSTER_DISTANCE,
        ConditionOperator.STAT_KRUSKAL_WALLIS_TEST,
        ConditionOperator.STAT_LEVENE_TEST,
        ConditionOperator.STAT_LOCAL_OUTLIER_FACTOR,
        ConditionOperator.STAT_MAHALANOBIS_DISTANCE,
        ConditionOperator.STAT_ONE_CLASS_SVM_BOUNDARY,
        ConditionOperator.STAT_SHAPIRO_WILK_TEST,
        ConditionOperator.STAT_WELCH_T_TEST,
        ConditionOperator.ANDERSON_DARLING_TEST,
        ConditionOperator.BENFORD_LAW_DEVIATION,
        ConditionOperator.CHI_SQUARE_DISTRIBUTION_TEST,
        ConditionOperator.COEFFICIENT_VARIATION_GT,
        ConditionOperator.CORRELATION_ANOMALY,
        ConditionOperator.KOLMOGOROV_SMIRNOV_TEST,
        ConditionOperator.MANN_WHITNEY_U_TEST,
        ConditionOperator.PERCENTILE_GT,
        ConditionOperator.REGRESSION_RESIDUAL_OUTLIER,
        ConditionOperator.SKEWNESS_KURTOSIS_ANOMALY,
        ConditionOperator.STANDARD_DEVIATION_GT,
        ConditionOperator.T_TEST_AMOUNT_DEVIATION,
        ConditionOperator.VARIANCE_RATIO_TEST,
        ConditionOperator.Z_SCORE_GT
    );

    @Override
    public Set<ConditionOperator> getSupportedOperators() {
        return SUPPORTED;
    }

    @Override
    public boolean evaluate(RuleCondition condition, EvaluationContext context) {
        ConditionOperator op = condition.getOperator();
        log.debug("StatisticalOperatorEvaluator: op={}", op);

        return switch (op) {
            case STAT_ANOVA_F_TEST -> evaluateAnovaFTest(condition, context);
            case STAT_BOOTSTRAP_CONFIDENCE_INTERVAL -> evaluateBootstrapCI(condition, context);
            case STAT_DBSCAN_NOISE_DETECTION -> evaluateDbscanNoise(condition, context);
            case STAT_DIXON_Q_TEST -> evaluateDixonQTest(condition, context);
            case STAT_GMM_PROBABILITY -> evaluateGmmProbability(condition, context);
            case STAT_GRUBBS_TEST -> evaluateGrubbsTest(condition, context);
            case STAT_ISOLATION_FOREST_SCORE -> evaluateIsolationForest(condition, context);
            case STAT_KMEANS_CLUSTER_DISTANCE -> evaluateKmeansDistance(condition, context);
            case STAT_KRUSKAL_WALLIS_TEST -> evaluateKruskalWallis(condition, context);
            case STAT_LEVENE_TEST -> evaluateLeveneTest(condition, context);
            case STAT_LOCAL_OUTLIER_FACTOR -> evaluateLof(condition, context);
            case STAT_MAHALANOBIS_DISTANCE -> evaluateMahalanobis(condition, context);
            case STAT_ONE_CLASS_SVM_BOUNDARY -> evaluateOneClassSvm(condition, context);
            case STAT_SHAPIRO_WILK_TEST -> evaluateShapiroWilk(condition, context);
            case STAT_WELCH_T_TEST -> evaluateWelchTTest(condition, context);
            case ANDERSON_DARLING_TEST -> evaluateAndersonDarling(condition, context);
            case BENFORD_LAW_DEVIATION -> evaluateBenfordLaw(condition, context);
            case CHI_SQUARE_DISTRIBUTION_TEST -> evaluateChiSquare(condition, context);
            case COEFFICIENT_VARIATION_GT -> evaluateCoefficientVariation(condition, context);
            case CORRELATION_ANOMALY -> evaluateCorrelationAnomaly(condition, context);
            case KOLMOGOROV_SMIRNOV_TEST -> evaluateKolmogorovSmirnov(condition, context);
            case MANN_WHITNEY_U_TEST -> evaluateMannWhitney(condition, context);
            case PERCENTILE_GT -> evaluatePercentileGt(condition, context);
            case REGRESSION_RESIDUAL_OUTLIER -> evaluateRegressionResidual(condition, context);
            case SKEWNESS_KURTOSIS_ANOMALY -> evaluateSkewnessKurtosis(condition, context);
            case STANDARD_DEVIATION_GT -> evaluateStandardDeviationGt(condition, context);
            case T_TEST_AMOUNT_DEVIATION -> evaluateTTestAmount(condition, context);
            case VARIANCE_RATIO_TEST -> evaluateVarianceRatio(condition, context);
            case Z_SCORE_GT -> evaluateZScoreGt(condition, context);
            default -> false;
        };
    }

    private Object getPayloadValue(EvaluationContext context, String... keys) {
        Map<String, Object> payload = context.getPayload();
        if (payload == null) return null;
        for (String key : keys) {
            Object value = payload.get(key);
            if (value != null) return value;
        }
        return null;
    }

    // ========== TESTES ESTATÍSTICOS ==========

    private boolean evaluateAnovaFTest(RuleCondition condition, EvaluationContext context) {
        Object pValue = getPayloadValue(context, "anovaFTestPValue", "fTestPValue");
        if (pValue == null) return false;
        BigDecimal p = parseBigDecimal(String.valueOf(pValue));
        BigDecimal alpha = parseBigDecimal(condition.getValueSingle(), new BigDecimal("0.05"));
        return p.compareTo(alpha) < 0; // Rejeita H0 se p < alpha
    }

    private boolean evaluateBootstrapCI(RuleCondition condition, EvaluationContext context) {
        Object outsideCI = getPayloadValue(context, "bootstrapOutsideCI", "outsideConfidenceInterval");
        if (outsideCI == null) return false;
        return Boolean.parseBoolean(String.valueOf(outsideCI));
    }

    private boolean evaluateDbscanNoise(RuleCondition condition, EvaluationContext context) {
        Object isNoise = getPayloadValue(context, "dbscanIsNoise", "isNoisePoint");
        if (isNoise == null) return false;
        return Boolean.parseBoolean(String.valueOf(isNoise));
    }

    private boolean evaluateDixonQTest(RuleCondition condition, EvaluationContext context) {
        Object isOutlier = getPayloadValue(context, "dixonQOutlier", "dixonOutlier");
        if (isOutlier == null) return false;
        return Boolean.parseBoolean(String.valueOf(isOutlier));
    }

    private boolean evaluateGmmProbability(RuleCondition condition, EvaluationContext context) {
        Object prob = getPayloadValue(context, "gmmProbability", "gmmProb");
        if (prob == null) return false;
        BigDecimal probability = parseBigDecimal(String.valueOf(prob));
        BigDecimal threshold = parseBigDecimal(condition.getValueSingle(), new BigDecimal("0.1"));
        return probability.compareTo(threshold) < 0; // Baixa probabilidade = anomalia
    }

    private boolean evaluateGrubbsTest(RuleCondition condition, EvaluationContext context) {
        Object isOutlier = getPayloadValue(context, "grubbsOutlier", "isGrubbsOutlier");
        if (isOutlier == null) return false;
        return Boolean.parseBoolean(String.valueOf(isOutlier));
    }

    private boolean evaluateIsolationForest(RuleCondition condition, EvaluationContext context) {
        Object score = getPayloadValue(context, "isolationForestScore", "ifScore");
        if (score == null) return false;
        BigDecimal iforestScore = parseBigDecimal(String.valueOf(score));
        BigDecimal threshold = parseBigDecimal(condition.getValueSingle(), new BigDecimal("0.6"));
        return iforestScore.compareTo(threshold) > 0; // Score alto = anomalia
    }

    private boolean evaluateKmeansDistance(RuleCondition condition, EvaluationContext context) {
        Object distance = getPayloadValue(context, "kmeansClusterDistance", "clusterDistance");
        if (distance == null) return false;
        BigDecimal dist = parseBigDecimal(String.valueOf(distance));
        BigDecimal threshold = parseBigDecimal(condition.getValueSingle(), new BigDecimal("3"));
        return dist.compareTo(threshold) > 0;
    }

    private boolean evaluateKruskalWallis(RuleCondition condition, EvaluationContext context) {
        Object pValue = getPayloadValue(context, "kruskalWallisPValue", "kwPValue");
        if (pValue == null) return false;
        BigDecimal p = parseBigDecimal(String.valueOf(pValue));
        BigDecimal alpha = parseBigDecimal(condition.getValueSingle(), new BigDecimal("0.05"));
        return p.compareTo(alpha) < 0;
    }

    private boolean evaluateLeveneTest(RuleCondition condition, EvaluationContext context) {
        Object pValue = getPayloadValue(context, "levenePValue", "leveneTestPValue");
        if (pValue == null) return false;
        BigDecimal p = parseBigDecimal(String.valueOf(pValue));
        BigDecimal alpha = parseBigDecimal(condition.getValueSingle(), new BigDecimal("0.05"));
        return p.compareTo(alpha) < 0;
    }

    private boolean evaluateLof(RuleCondition condition, EvaluationContext context) {
        Object lof = getPayloadValue(context, "localOutlierFactor", "lofScore");
        if (lof == null) return false;
        BigDecimal lofScore = parseBigDecimal(String.valueOf(lof));
        BigDecimal threshold = parseBigDecimal(condition.getValueSingle(), new BigDecimal("1.5"));
        return lofScore.compareTo(threshold) > 0;
    }

    private boolean evaluateMahalanobis(RuleCondition condition, EvaluationContext context) {
        Object distance = getPayloadValue(context, "mahalanobisDistance", "mahalDist");
        if (distance == null) return false;
        BigDecimal dist = parseBigDecimal(String.valueOf(distance));
        BigDecimal threshold = parseBigDecimal(condition.getValueSingle(), new BigDecimal("3"));
        return dist.compareTo(threshold) > 0;
    }

    private boolean evaluateOneClassSvm(RuleCondition condition, EvaluationContext context) {
        Object isOutlier = getPayloadValue(context, "oneClassSvmOutlier", "svmOutlier");
        if (isOutlier == null) return false;
        return Boolean.parseBoolean(String.valueOf(isOutlier));
    }

    private boolean evaluateShapiroWilk(RuleCondition condition, EvaluationContext context) {
        Object pValue = getPayloadValue(context, "shapiroWilkPValue", "swPValue");
        if (pValue == null) return false;
        BigDecimal p = parseBigDecimal(String.valueOf(pValue));
        BigDecimal alpha = parseBigDecimal(condition.getValueSingle(), new BigDecimal("0.05"));
        return p.compareTo(alpha) < 0; // Não normal se p < alpha
    }

    private boolean evaluateWelchTTest(RuleCondition condition, EvaluationContext context) {
        Object pValue = getPayloadValue(context, "welchTTestPValue", "welchPValue");
        if (pValue == null) return false;
        BigDecimal p = parseBigDecimal(String.valueOf(pValue));
        BigDecimal alpha = parseBigDecimal(condition.getValueSingle(), new BigDecimal("0.05"));
        return p.compareTo(alpha) < 0;
    }

    private boolean evaluateAndersonDarling(RuleCondition condition, EvaluationContext context) {
        Object statistic = getPayloadValue(context, "andersonDarlingStatistic", "adStatistic");
        if (statistic == null) return false;
        BigDecimal stat = parseBigDecimal(String.valueOf(statistic));
        BigDecimal criticalValue = parseBigDecimal(condition.getValueSingle(), new BigDecimal("0.787"));
        return stat.compareTo(criticalValue) > 0;
    }

    private boolean evaluateBenfordLaw(RuleCondition condition, EvaluationContext context) {
        Object deviation = getPayloadValue(context, "benfordDeviation", "benfordLawDeviation");
        if (deviation == null) return false;
        BigDecimal dev = parseBigDecimal(String.valueOf(deviation));
        BigDecimal threshold = parseBigDecimal(condition.getValueSingle(), new BigDecimal("0.1"));
        return dev.compareTo(threshold) > 0;
    }

    private boolean evaluateChiSquare(RuleCondition condition, EvaluationContext context) {
        Object pValue = getPayloadValue(context, "chiSquarePValue", "chi2PValue");
        if (pValue == null) return false;
        BigDecimal p = parseBigDecimal(String.valueOf(pValue));
        BigDecimal alpha = parseBigDecimal(condition.getValueSingle(), new BigDecimal("0.05"));
        return p.compareTo(alpha) < 0;
    }

    private boolean evaluateCoefficientVariation(RuleCondition condition, EvaluationContext context) {
        Object cv = getPayloadValue(context, "coefficientVariation", "cv");
        if (cv == null) return false;
        BigDecimal cvValue = parseBigDecimal(String.valueOf(cv));
        BigDecimal threshold = parseBigDecimal(condition.getValueSingle(), new BigDecimal("0.5"));
        return cvValue.compareTo(threshold) > 0;
    }

    private boolean evaluateCorrelationAnomaly(RuleCondition condition, EvaluationContext context) {
        Object anomaly = getPayloadValue(context, "correlationAnomaly", "corrAnomaly");
        if (anomaly == null) return false;
        return Boolean.parseBoolean(String.valueOf(anomaly));
    }

    private boolean evaluateKolmogorovSmirnov(RuleCondition condition, EvaluationContext context) {
        Object pValue = getPayloadValue(context, "ksPValue", "kolmogorovSmirnovPValue");
        if (pValue == null) return false;
        BigDecimal p = parseBigDecimal(String.valueOf(pValue));
        BigDecimal alpha = parseBigDecimal(condition.getValueSingle(), new BigDecimal("0.05"));
        return p.compareTo(alpha) < 0;
    }

    private boolean evaluateMannWhitney(RuleCondition condition, EvaluationContext context) {
        Object pValue = getPayloadValue(context, "mannWhitneyPValue", "mwPValue");
        if (pValue == null) return false;
        BigDecimal p = parseBigDecimal(String.valueOf(pValue));
        BigDecimal alpha = parseBigDecimal(condition.getValueSingle(), new BigDecimal("0.05"));
        return p.compareTo(alpha) < 0;
    }

    private boolean evaluatePercentileGt(RuleCondition condition, EvaluationContext context) {
        Object percentile = getPayloadValue(context, "valuePercentile", "percentile");
        if (percentile == null) return false;
        BigDecimal perc = parseBigDecimal(String.valueOf(percentile));
        BigDecimal threshold = parseBigDecimal(condition.getValueSingle(), new BigDecimal("95"));
        return perc.compareTo(threshold) > 0;
    }

    private boolean evaluateRegressionResidual(RuleCondition condition, EvaluationContext context) {
        Object residual = getPayloadValue(context, "regressionResidual", "residual");
        if (residual == null) return false;
        BigDecimal res = parseBigDecimal(String.valueOf(residual)).abs();
        BigDecimal threshold = parseBigDecimal(condition.getValueSingle(), new BigDecimal("2"));
        return res.compareTo(threshold) > 0;
    }

    private boolean evaluateSkewnessKurtosis(RuleCondition condition, EvaluationContext context) {
        Object anomaly = getPayloadValue(context, "skewnessKurtosisAnomaly", "skKurtAnomaly");
        if (anomaly == null) return false;
        return Boolean.parseBoolean(String.valueOf(anomaly));
    }

    private boolean evaluateStandardDeviationGt(RuleCondition condition, EvaluationContext context) {
        Object stdDev = getPayloadValue(context, "standardDeviation", "stdDev");
        if (stdDev == null) return false;
        BigDecimal sd = parseBigDecimal(String.valueOf(stdDev));
        BigDecimal threshold = parseBigDecimal(condition.getValueSingle(), new BigDecimal("1000"));
        return sd.compareTo(threshold) > 0;
    }

    private boolean evaluateTTestAmount(RuleCondition condition, EvaluationContext context) {
        Object pValue = getPayloadValue(context, "tTestPValue", "tTestAmountPValue");
        if (pValue == null) return false;
        BigDecimal p = parseBigDecimal(String.valueOf(pValue));
        BigDecimal alpha = parseBigDecimal(condition.getValueSingle(), new BigDecimal("0.05"));
        return p.compareTo(alpha) < 0;
    }

    private boolean evaluateVarianceRatio(RuleCondition condition, EvaluationContext context) {
        Object ratio = getPayloadValue(context, "varianceRatio", "fRatio");
        if (ratio == null) return false;
        BigDecimal r = parseBigDecimal(String.valueOf(ratio));
        BigDecimal threshold = parseBigDecimal(condition.getValueSingle(), new BigDecimal("2"));
        return r.compareTo(threshold) > 0;
    }

    private boolean evaluateZScoreGt(RuleCondition condition, EvaluationContext context) {
        Object zScore = getPayloadValue(context, "zScore", "zscore");
        if (zScore == null) return false;
        BigDecimal z = parseBigDecimal(String.valueOf(zScore)).abs();
        BigDecimal threshold = parseBigDecimal(condition.getValueSingle(), new BigDecimal("2"));
        return z.compareTo(threshold) > 0;
    }

    private BigDecimal parseBigDecimal(String value) {
        return parseBigDecimal(value, BigDecimal.ZERO);
    }

    private BigDecimal parseBigDecimal(String value, BigDecimal defaultValue) {
        try {
            return new BigDecimal(value);
        } catch (Exception e) {
            return defaultValue;
        }
    }

    @Override
    public String getCategory() {
        return "STATISTICAL";
    }
}
