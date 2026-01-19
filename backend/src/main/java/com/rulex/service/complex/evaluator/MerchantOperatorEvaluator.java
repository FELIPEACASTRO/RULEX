package com.rulex.service.complex.evaluator;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.RuleCondition.ConditionOperator;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Map;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Avaliador para operadores relacionados a merchants/comerciantes.
 * Implementa verificações de reputação, padrões e anomalias de merchants.
 */
@Component
@Slf4j
public class MerchantOperatorEvaluator implements OperatorEvaluator {

    private static final Set<ConditionOperator> SUPPORTED = Set.of(
        ConditionOperator.MERCHANT_AGE_CHECK,
        ConditionOperator.MERCHANT_AMOUNT_DISTRIBUTION,
        ConditionOperator.MERCHANT_CHARGEBACK_HISTORY,
        ConditionOperator.MERCHANT_CROSS_BORDER_RATIO,
        ConditionOperator.MERCHANT_CUSTOMER_CONCENTRATION,
        ConditionOperator.MERCHANT_DEVIATION,
        ConditionOperator.MERCHANT_DEVICE_DIVERSITY,
        ConditionOperator.MERCHANT_DORMANT_REACTIVATION,
        ConditionOperator.MERCHANT_FRAUD_RATE_CHECK,
        ConditionOperator.MERCHANT_GEOGRAPHIC_SPREAD,
        ConditionOperator.MERCHANT_HIGH_VALUE_FREQUENCY,
        ConditionOperator.MERCHANT_NEW_CUSTOMER_RATIO,
        ConditionOperator.MERCHANT_REFUND_RATIO,
        ConditionOperator.MERCHANT_REPUTATION_SCORE,
        ConditionOperator.MERCHANT_TIME_PATTERN,
        ConditionOperator.MERCHANT_TRANSACTION_VOLUME,
        ConditionOperator.MERCHANT_CATEGORY_CHANGE,
        ConditionOperator.MERCHANT_FIRST_SEEN,
        ConditionOperator.MERCHANT_COUNTRY_MISMATCH,
        ConditionOperator.MERCHANT_VELOCITY_SPIKE
    );

    @Override
    public Set<ConditionOperator> getSupportedOperators() {
        return SUPPORTED;
    }

    @Override
    public boolean evaluate(RuleCondition condition, EvaluationContext context) {
        ConditionOperator op = condition.getOperator();
        log.debug("MerchantOperatorEvaluator: op={}", op);

        return switch (op) {
            case MERCHANT_AGE_CHECK -> evaluateMerchantAgeCheck(condition, context);
            case MERCHANT_AMOUNT_DISTRIBUTION -> evaluateMerchantAmountDistribution(condition, context);
            case MERCHANT_CHARGEBACK_HISTORY -> evaluateMerchantChargebackHistory(condition, context);
            case MERCHANT_CROSS_BORDER_RATIO -> evaluateMerchantCrossBorderRatio(condition, context);
            case MERCHANT_CUSTOMER_CONCENTRATION -> evaluateMerchantCustomerConcentration(condition, context);
            case MERCHANT_DEVIATION -> evaluateMerchantDeviation(condition, context);
            case MERCHANT_DEVICE_DIVERSITY -> evaluateMerchantDeviceDiversity(condition, context);
            case MERCHANT_DORMANT_REACTIVATION -> evaluateMerchantDormantReactivation(condition, context);
            case MERCHANT_FRAUD_RATE_CHECK -> evaluateMerchantFraudRateCheck(condition, context);
            case MERCHANT_GEOGRAPHIC_SPREAD -> evaluateMerchantGeographicSpread(condition, context);
            case MERCHANT_HIGH_VALUE_FREQUENCY -> evaluateMerchantHighValueFrequency(condition, context);
            case MERCHANT_NEW_CUSTOMER_RATIO -> evaluateMerchantNewCustomerRatio(condition, context);
            case MERCHANT_REFUND_RATIO -> evaluateMerchantRefundRatio(condition, context);
            case MERCHANT_REPUTATION_SCORE -> evaluateMerchantReputationScore(condition, context);
            case MERCHANT_TIME_PATTERN -> evaluateMerchantTimePattern(condition, context);
            case MERCHANT_TRANSACTION_VOLUME -> evaluateMerchantTransactionVolume(condition, context);
            case MERCHANT_CATEGORY_CHANGE -> evaluateMerchantCategoryChange(condition, context);
            case MERCHANT_FIRST_SEEN -> evaluateMerchantFirstSeen(condition, context);
            case MERCHANT_COUNTRY_MISMATCH -> evaluateMerchantCountryMismatch(condition, context);
            case MERCHANT_VELOCITY_SPIKE -> evaluateMerchantVelocitySpike(condition, context);
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

    /**
     * MERCHANT_AGE_CHECK - Verifica idade do merchant (dias desde cadastro).
     */
    private boolean evaluateMerchantAgeCheck(RuleCondition condition, EvaluationContext context) {
        Object ageObj = getPayloadValue(context, "merchantAge", "merchantAgeDays", "merchant_age");
        if (ageObj == null) return false;

        int age = parseIntSafe(String.valueOf(ageObj), Integer.MAX_VALUE);
        int threshold = parseIntSafe(condition.getValueSingle(), 30);

        log.debug("MERCHANT_AGE_CHECK: age={} days, threshold={}", age, threshold);
        return age < threshold;
    }

    /**
     * MERCHANT_AMOUNT_DISTRIBUTION - Verifica distribuição anormal de valores.
     */
    private boolean evaluateMerchantAmountDistribution(RuleCondition condition, EvaluationContext context) {
        Object distObj = getPayloadValue(context, "merchantAmountDistribution", "amountDistributionScore");
        if (distObj == null) return false;

        BigDecimal score = parseBigDecimal(String.valueOf(distObj), BigDecimal.ZERO);
        BigDecimal threshold = parseBigDecimal(condition.getValueSingle(), new BigDecimal("0.7"));

        return score.compareTo(threshold) > 0;
    }

    /**
     * MERCHANT_CHARGEBACK_HISTORY - Verifica histórico de chargebacks do merchant.
     */
    private boolean evaluateMerchantChargebackHistory(RuleCondition condition, EvaluationContext context) {
        Object rateObj = getPayloadValue(context, "merchantChargebackRate", "chargebackRate", "cbRate");
        if (rateObj == null) return false;

        BigDecimal rate = parseBigDecimal(String.valueOf(rateObj), BigDecimal.ZERO);
        BigDecimal threshold = parseBigDecimal(condition.getValueSingle(), new BigDecimal("1"));

        log.debug("MERCHANT_CHARGEBACK_HISTORY: rate={}%, threshold={}%", rate, threshold);
        return rate.compareTo(threshold) > 0;
    }

    /**
     * MERCHANT_CROSS_BORDER_RATIO - Ratio de transações cross-border.
     */
    private boolean evaluateMerchantCrossBorderRatio(RuleCondition condition, EvaluationContext context) {
        Object ratioObj = getPayloadValue(context, "merchantCrossBorderRatio", "crossBorderRatio");
        if (ratioObj == null) return false;

        BigDecimal ratio = parseBigDecimal(String.valueOf(ratioObj), BigDecimal.ZERO);
        BigDecimal threshold = parseBigDecimal(condition.getValueSingle(), new BigDecimal("30"));

        return ratio.compareTo(threshold) > 0;
    }

    /**
     * MERCHANT_CUSTOMER_CONCENTRATION - Concentração de clientes (poucos clientes = suspeito).
     */
    private boolean evaluateMerchantCustomerConcentration(RuleCondition condition, EvaluationContext context) {
        Object concObj = getPayloadValue(context, "merchantCustomerConcentration", "customerConcentration");
        if (concObj == null) return false;

        BigDecimal concentration = parseBigDecimal(String.valueOf(concObj), BigDecimal.ZERO);
        BigDecimal threshold = parseBigDecimal(condition.getValueSingle(), new BigDecimal("80"));

        // Alta concentração = poucos clientes representam maioria das transações
        return concentration.compareTo(threshold) > 0;
    }

    /**
     * MERCHANT_DEVIATION - Desvio do padrão normal do merchant.
     */
    private boolean evaluateMerchantDeviation(RuleCondition condition, EvaluationContext context) {
        Object devObj = getPayloadValue(context, "merchantDeviation", "deviationScore");
        if (devObj == null) return false;

        BigDecimal deviation = parseBigDecimal(String.valueOf(devObj), BigDecimal.ZERO);
        BigDecimal threshold = parseBigDecimal(condition.getValueSingle(), new BigDecimal("2"));

        return deviation.compareTo(threshold) > 0;
    }

    /**
     * MERCHANT_DEVICE_DIVERSITY - Diversidade de dispositivos usados.
     */
    private boolean evaluateMerchantDeviceDiversity(RuleCondition condition, EvaluationContext context) {
        Object divObj = getPayloadValue(context, "merchantDeviceDiversity", "deviceDiversity");
        if (divObj == null) return false;

        int diversity = parseIntSafe(String.valueOf(divObj), 0);
        int threshold = parseIntSafe(condition.getValueSingle(), 100);

        // Muitos dispositivos diferentes = suspeito
        return diversity > threshold;
    }

    /**
     * MERCHANT_DORMANT_REACTIVATION - Merchant dormante reativado.
     */
    private boolean evaluateMerchantDormantReactivation(RuleCondition condition, EvaluationContext context) {
        Object dormantObj = getPayloadValue(context, "merchantDormantReactivation", "isDormantReactivated", "dormantDays");
        if (dormantObj == null) return false;

        if (dormantObj instanceof Boolean) {
            return (Boolean) dormantObj;
        }

        int dormantDays = parseIntSafe(String.valueOf(dormantObj), 0);
        int threshold = parseIntSafe(condition.getValueSingle(), 90);

        return dormantDays > threshold;
    }

    /**
     * MERCHANT_FRAUD_RATE_CHECK - Taxa de fraude do merchant.
     */
    private boolean evaluateMerchantFraudRateCheck(RuleCondition condition, EvaluationContext context) {
        Object rateObj = getPayloadValue(context, "merchantFraudRate", "fraudRate");
        if (rateObj == null) return false;

        BigDecimal rate = parseBigDecimal(String.valueOf(rateObj), BigDecimal.ZERO);
        BigDecimal threshold = parseBigDecimal(condition.getValueSingle(), new BigDecimal("0.5"));

        return rate.compareTo(threshold) > 0;
    }

    /**
     * MERCHANT_GEOGRAPHIC_SPREAD - Dispersão geográfica das transações.
     */
    private boolean evaluateMerchantGeographicSpread(RuleCondition condition, EvaluationContext context) {
        Object spreadObj = getPayloadValue(context, "merchantGeographicSpread", "geoSpread", "countryCount");
        if (spreadObj == null) return false;

        int spread = parseIntSafe(String.valueOf(spreadObj), 0);
        int threshold = parseIntSafe(condition.getValueSingle(), 10);

        return spread > threshold;
    }

    /**
     * MERCHANT_HIGH_VALUE_FREQUENCY - Frequência de transações de alto valor.
     */
    private boolean evaluateMerchantHighValueFrequency(RuleCondition condition, EvaluationContext context) {
        Object freqObj = getPayloadValue(context, "merchantHighValueFrequency", "highValueFreq");
        if (freqObj == null) return false;

        BigDecimal frequency = parseBigDecimal(String.valueOf(freqObj), BigDecimal.ZERO);
        BigDecimal threshold = parseBigDecimal(condition.getValueSingle(), new BigDecimal("20"));

        return frequency.compareTo(threshold) > 0;
    }

    /**
     * MERCHANT_NEW_CUSTOMER_RATIO - Ratio de clientes novos.
     */
    private boolean evaluateMerchantNewCustomerRatio(RuleCondition condition, EvaluationContext context) {
        Object ratioObj = getPayloadValue(context, "merchantNewCustomerRatio", "newCustomerRatio");
        if (ratioObj == null) return false;

        BigDecimal ratio = parseBigDecimal(String.valueOf(ratioObj), BigDecimal.ZERO);
        BigDecimal threshold = parseBigDecimal(condition.getValueSingle(), new BigDecimal("80"));

        return ratio.compareTo(threshold) > 0;
    }

    /**
     * MERCHANT_REFUND_RATIO - Ratio de reembolsos.
     */
    private boolean evaluateMerchantRefundRatio(RuleCondition condition, EvaluationContext context) {
        Object ratioObj = getPayloadValue(context, "merchantRefundRatio", "refundRatio");
        if (ratioObj == null) return false;

        BigDecimal ratio = parseBigDecimal(String.valueOf(ratioObj), BigDecimal.ZERO);
        BigDecimal threshold = parseBigDecimal(condition.getValueSingle(), new BigDecimal("10"));

        return ratio.compareTo(threshold) > 0;
    }

    /**
     * MERCHANT_REPUTATION_SCORE - Score de reputação do merchant.
     */
    private boolean evaluateMerchantReputationScore(RuleCondition condition, EvaluationContext context) {
        Object scoreObj = getPayloadValue(context, "merchantReputationScore", "reputationScore");
        if (scoreObj == null) return false;

        BigDecimal score = parseBigDecimal(String.valueOf(scoreObj), new BigDecimal("100"));
        BigDecimal threshold = parseBigDecimal(condition.getValueSingle(), new BigDecimal("50"));

        // Score baixo = suspeito
        return score.compareTo(threshold) < 0;
    }

    /**
     * MERCHANT_TIME_PATTERN - Padrão de horário anormal.
     */
    private boolean evaluateMerchantTimePattern(RuleCondition condition, EvaluationContext context) {
        Object patternObj = getPayloadValue(context, "merchantTimePattern", "timePatternAnomaly");
        if (patternObj == null) return false;

        if (patternObj instanceof Boolean) {
            return (Boolean) patternObj;
        }

        BigDecimal anomalyScore = parseBigDecimal(String.valueOf(patternObj), BigDecimal.ZERO);
        BigDecimal threshold = parseBigDecimal(condition.getValueSingle(), new BigDecimal("0.7"));

        return anomalyScore.compareTo(threshold) > 0;
    }

    /**
     * MERCHANT_TRANSACTION_VOLUME - Volume de transações anormal.
     */
    private boolean evaluateMerchantTransactionVolume(RuleCondition condition, EvaluationContext context) {
        Object volumeObj = getPayloadValue(context, "merchantTransactionVolume", "txVolume");
        if (volumeObj == null) return false;

        int volume = parseIntSafe(String.valueOf(volumeObj), 0);
        int threshold = parseIntSafe(condition.getValueSingle(), 1000);

        return volume > threshold;
    }

    /**
     * MERCHANT_CATEGORY_CHANGE - Mudança de categoria do merchant.
     */
    private boolean evaluateMerchantCategoryChange(RuleCondition condition, EvaluationContext context) {
        Object changeObj = getPayloadValue(context, "merchantCategoryChange", "categoryChanged");
        if (changeObj == null) return false;

        if (changeObj instanceof Boolean) {
            return (Boolean) changeObj;
        }

        return "true".equalsIgnoreCase(String.valueOf(changeObj));
    }

    /**
     * MERCHANT_FIRST_SEEN - Primeira vez que vemos este merchant.
     */
    private boolean evaluateMerchantFirstSeen(RuleCondition condition, EvaluationContext context) {
        Object firstSeenObj = getPayloadValue(context, "merchantFirstSeen", "isNewMerchant", "firstSeen");
        if (firstSeenObj == null) return false;

        if (firstSeenObj instanceof Boolean) {
            return (Boolean) firstSeenObj;
        }

        return "true".equalsIgnoreCase(String.valueOf(firstSeenObj));
    }

    /**
     * MERCHANT_COUNTRY_MISMATCH - País do merchant não corresponde.
     */
    private boolean evaluateMerchantCountryMismatch(RuleCondition condition, EvaluationContext context) {
        Object mismatchObj = getPayloadValue(context, "merchantCountryMismatch", "countryMismatch");
        if (mismatchObj == null) return false;

        if (mismatchObj instanceof Boolean) {
            return (Boolean) mismatchObj;
        }

        return "true".equalsIgnoreCase(String.valueOf(mismatchObj));
    }

    /**
     * MERCHANT_VELOCITY_SPIKE - Pico na velocidade de transações do merchant.
     */
    private boolean evaluateMerchantVelocitySpike(RuleCondition condition, EvaluationContext context) {
        Object spikeObj = getPayloadValue(context, "merchantVelocitySpike", "velocitySpike");
        if (spikeObj == null) return false;

        if (spikeObj instanceof Boolean) {
            return (Boolean) spikeObj;
        }

        BigDecimal spikeScore = parseBigDecimal(String.valueOf(spikeObj), BigDecimal.ZERO);
        BigDecimal threshold = parseBigDecimal(condition.getValueSingle(), new BigDecimal("0.8"));

        return spikeScore.compareTo(threshold) > 0;
    }

    private int parseIntSafe(String value, int defaultValue) {
        try {
            return Integer.parseInt(value);
        } catch (Exception e) {
            return defaultValue;
        }
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
        return "MERCHANT";
    }
}
