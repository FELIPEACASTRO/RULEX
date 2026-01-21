package com.rulex.service.complex.evaluator;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;
import java.util.Map;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Avaliador para operadores relacionados a valores/amounts.
 * Implementa detecção de anomalias, spikes, variância e padrões de valores.
 */
@Component
@Slf4j
public class AmountOperatorEvaluator implements OperatorEvaluator {

    private static final Set<ConditionOperator> SUPPORTED = Set.of(
        ConditionOperator.AMOUNT_DEVIATION_FROM_AVG,
        ConditionOperator.AMOUNT_ROUNDING_BEHAVIOR,
        ConditionOperator.AMOUNT_SPIKE,
        ConditionOperator.AMOUNT_SUM_PER_CARD_HOUR,
        ConditionOperator.AMOUNT_SUM_PER_CUSTOMER_DAY,
        ConditionOperator.AMOUNT_VARIANCE_ANOMALY,
        ConditionOperator.DECIMAL_PLACES_GT,
        ConditionOperator.ROUND_AMOUNT_FREQUENCY,
        ConditionOperator.SMALL_AMOUNT_VELOCITY,
        ConditionOperator.LARGE_AMOUNT_FREQUENCY
    );

    @Override
    public Set<ConditionOperator> getSupportedOperators() {
        return SUPPORTED;
    }

    @Override
    public boolean evaluate(RuleCondition condition, EvaluationContext context) {
        ConditionOperator op = condition.getOperator();
        log.debug("AmountOperatorEvaluator: op={}", op);

        return switch (op) {
            case AMOUNT_DEVIATION_FROM_AVG -> evaluateAmountDeviationFromAvg(condition, context);
            case AMOUNT_ROUNDING_BEHAVIOR -> evaluateAmountRoundingBehavior(condition, context);
            case AMOUNT_SPIKE -> evaluateAmountSpike(condition, context);
            case AMOUNT_SUM_PER_CARD_HOUR -> evaluateAmountSumPerCardHour(condition, context);
            case AMOUNT_SUM_PER_CUSTOMER_DAY -> evaluateAmountSumPerCustomerDay(condition, context);
            case AMOUNT_VARIANCE_ANOMALY -> evaluateAmountVarianceAnomaly(condition, context);
            case DECIMAL_PLACES_GT -> evaluateDecimalPlacesGt(condition, context);
            case ROUND_AMOUNT_FREQUENCY -> evaluateRoundAmountFrequency(condition, context);
            case SMALL_AMOUNT_VELOCITY -> evaluateSmallAmountVelocity(condition, context);
            case LARGE_AMOUNT_FREQUENCY -> evaluateLargeAmountFrequency(condition, context);
            default -> false;
        };
    }

    private BigDecimal getAmount(EvaluationContext context) {
        Map<String, Object> payload = context.getPayload();
        if (payload == null) return null;

        Object amount = payload.get("amount");
        if (amount == null) amount = payload.get("transactionAmount");
        if (amount == null) return null;

        try {
            return new BigDecimal(String.valueOf(amount));
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * AMOUNT_DEVIATION_FROM_AVG - Desvio do valor em relação à média histórica.
     */
    private boolean evaluateAmountDeviationFromAvg(RuleCondition condition, EvaluationContext context) {
        BigDecimal amount = getAmount(context);
        if (amount == null) return false;

        Map<String, Object> payload = context.getPayload();
        Object avgObj = payload.get("averageAmount");
        if (avgObj == null) avgObj = payload.get("avgAmount");
        if (avgObj == null) avgObj = payload.get("historicalAvgAmount");

        if (avgObj == null) return false;

        BigDecimal avg = new BigDecimal(String.valueOf(avgObj));
        if (avg.compareTo(BigDecimal.ZERO) == 0) return false;

        // Calcular desvio percentual
        BigDecimal deviation = amount.subtract(avg).abs()
            .divide(avg, 4, RoundingMode.HALF_UP)
            .multiply(new BigDecimal("100"));

        BigDecimal threshold = parseThreshold(condition.getValueSingle(), new BigDecimal("50"));

        log.debug("AMOUNT_DEVIATION_FROM_AVG: amount={}, avg={}, deviation={}%, threshold={}%",
            amount, avg, deviation, threshold);
        return deviation.compareTo(threshold) > 0;
    }

    /**
     * AMOUNT_ROUNDING_BEHAVIOR - Detecta comportamento de arredondamento suspeito.
     */
    private boolean evaluateAmountRoundingBehavior(RuleCondition condition, EvaluationContext context) {
        BigDecimal amount = getAmount(context);
        if (amount == null) return false;

        // Verificar se é valor redondo (múltiplo de 100, 500, 1000, etc.)
        BigDecimal[] roundValues = {
            new BigDecimal("100"),
            new BigDecimal("500"),
            new BigDecimal("1000"),
            new BigDecimal("5000"),
            new BigDecimal("10000")
        };

        for (BigDecimal roundValue : roundValues) {
            if (amount.remainder(roundValue).compareTo(BigDecimal.ZERO) == 0) {
                log.debug("AMOUNT_ROUNDING_BEHAVIOR: amount={} é múltiplo de {}", amount, roundValue);
                return true;
            }
        }

        return false;
    }

    /**
     * AMOUNT_SPIKE - Detecta pico de valor em relação ao histórico.
     */
    private boolean evaluateAmountSpike(RuleCondition condition, EvaluationContext context) {
        BigDecimal amount = getAmount(context);
        if (amount == null) return false;

        Map<String, Object> payload = context.getPayload();
        Object maxHistObj = payload.get("maxHistoricalAmount");
        if (maxHistObj == null) maxHistObj = payload.get("maxAmount");

        if (maxHistObj == null) {
            // Se não há histórico, considerar spike se valor > threshold
            BigDecimal threshold = parseThreshold(condition.getValueSingle(), new BigDecimal("10000"));
            return amount.compareTo(threshold) > 0;
        }

        BigDecimal maxHist = new BigDecimal(String.valueOf(maxHistObj));
        BigDecimal multiplier = parseThreshold(condition.getValueSingle(), new BigDecimal("2"));

        // Spike se valor > maxHistórico * multiplier
        BigDecimal spikeThreshold = maxHist.multiply(multiplier);

        log.debug("AMOUNT_SPIKE: amount={}, maxHist={}, threshold={}", amount, maxHist, spikeThreshold);
        return amount.compareTo(spikeThreshold) > 0;
    }

    /**
     * AMOUNT_SUM_PER_CARD_HOUR - Soma de valores por cartão na última hora.
     */
    private boolean evaluateAmountSumPerCardHour(RuleCondition condition, EvaluationContext context) {
        Map<String, Object> payload = context.getPayload();
        if (payload == null) return false;

        Object sumObj = payload.get("amountSumPerCardHour");
        if (sumObj == null) sumObj = payload.get("cardHourlySum");
        if (sumObj == null) return false;

        BigDecimal sum = new BigDecimal(String.valueOf(sumObj));
        BigDecimal threshold = parseThreshold(condition.getValueSingle(), new BigDecimal("5000"));

        return sum.compareTo(threshold) > 0;
    }

    /**
     * AMOUNT_SUM_PER_CUSTOMER_DAY - Soma de valores por cliente no dia.
     */
    private boolean evaluateAmountSumPerCustomerDay(RuleCondition condition, EvaluationContext context) {
        Map<String, Object> payload = context.getPayload();
        if (payload == null) return false;

        Object sumObj = payload.get("amountSumPerCustomerDay");
        if (sumObj == null) sumObj = payload.get("customerDailySum");
        if (sumObj == null) return false;

        BigDecimal sum = new BigDecimal(String.valueOf(sumObj));
        BigDecimal threshold = parseThreshold(condition.getValueSingle(), new BigDecimal("50000"));

        return sum.compareTo(threshold) > 0;
    }

    /**
     * AMOUNT_VARIANCE_ANOMALY - Detecta anomalia na variância dos valores.
     */
    private boolean evaluateAmountVarianceAnomaly(RuleCondition condition, EvaluationContext context) {
        Map<String, Object> payload = context.getPayload();
        if (payload == null) return false;

        Object varianceObj = payload.get("amountVariance");
        if (varianceObj == null) return false;

        BigDecimal variance = new BigDecimal(String.valueOf(varianceObj));
        BigDecimal threshold = parseThreshold(condition.getValueSingle(), new BigDecimal("1000000"));

        return variance.compareTo(threshold) > 0;
    }

    /**
     * DECIMAL_PLACES_GT - Número de casas decimais maior que threshold.
     */
    private boolean evaluateDecimalPlacesGt(RuleCondition condition, EvaluationContext context) {
        BigDecimal amount = getAmount(context);
        if (amount == null) return false;

        int decimalPlaces = amount.scale();
        int threshold = parseIntSafe(condition.getValueSingle(), 2);

        return decimalPlaces > threshold;
    }

    /**
     * PERCENTAGE_OF_BALANCE_GT - Percentual do saldo maior que threshold.
     */
    private boolean evaluatePercentageOfBalanceGt(RuleCondition condition, EvaluationContext context) {
        BigDecimal amount = getAmount(context);
        if (amount == null) return false;

        Map<String, Object> payload = context.getPayload();
        Object balanceObj = payload.get("accountBalance");
        if (balanceObj == null) balanceObj = payload.get("balance");
        if (balanceObj == null) return false;

        BigDecimal balance = new BigDecimal(String.valueOf(balanceObj));
        if (balance.compareTo(BigDecimal.ZERO) <= 0) return false;

        BigDecimal percentage = amount.divide(balance, 4, RoundingMode.HALF_UP)
            .multiply(new BigDecimal("100"));
        BigDecimal threshold = parseThreshold(condition.getValueSingle(), new BigDecimal("80"));

        return percentage.compareTo(threshold) > 0;
    }

    /**
     * ROUND_AMOUNT_FREQUENCY - Frequência de valores redondos.
     */
    private boolean evaluateRoundAmountFrequency(RuleCondition condition, EvaluationContext context) {
        Map<String, Object> payload = context.getPayload();
        if (payload == null) return false;

        Object freqObj = payload.get("roundAmountFrequency");
        if (freqObj == null) return false;

        BigDecimal frequency = new BigDecimal(String.valueOf(freqObj));
        BigDecimal threshold = parseThreshold(condition.getValueSingle(), new BigDecimal("70"));

        return frequency.compareTo(threshold) > 0;
    }

    /**
     * ROUND_AMOUNT_PATTERN - Padrão de valores redondos consecutivos.
     */
    private boolean evaluateRoundAmountPattern(RuleCondition condition, EvaluationContext context) {
        Map<String, Object> payload = context.getPayload();
        if (payload == null) return false;

        Object patternObj = payload.get("roundAmountPattern");
        if (patternObj != null) {
            return Boolean.parseBoolean(String.valueOf(patternObj));
        }

        // Verificar últimos valores
        Object lastAmountsObj = payload.get("lastAmounts");
        if (lastAmountsObj instanceof List<?> lastAmounts) {
            int roundCount = 0;
            for (Object amt : lastAmounts) {
                BigDecimal amount = new BigDecimal(String.valueOf(amt));
                if (amount.remainder(new BigDecimal("100")).compareTo(BigDecimal.ZERO) == 0) {
                    roundCount++;
                }
            }
            int threshold = parseIntSafe(condition.getValueSingle(), 3);
            return roundCount >= threshold;
        }

        return false;
    }

    /**
     * SMALL_AMOUNT_VELOCITY - Velocidade de transações de pequeno valor.
     */
    private boolean evaluateSmallAmountVelocity(RuleCondition condition, EvaluationContext context) {
        Map<String, Object> payload = context.getPayload();
        if (payload == null) return false;

        Object velocityObj = payload.get("smallAmountVelocity");
        if (velocityObj == null) velocityObj = payload.get("smallTxVelocity");
        if (velocityObj == null) return false;

        int velocity = parseIntSafe(String.valueOf(velocityObj), 0);
        int threshold = parseIntSafe(condition.getValueSingle(), 10);

        return velocity > threshold;
    }

    /**
     * LARGE_AMOUNT_FREQUENCY - Frequência de transações de alto valor.
     */
    private boolean evaluateLargeAmountFrequency(RuleCondition condition, EvaluationContext context) {
        Map<String, Object> payload = context.getPayload();
        if (payload == null) return false;

        Object freqObj = payload.get("largeAmountFrequency");
        if (freqObj == null) return false;

        int frequency = parseIntSafe(String.valueOf(freqObj), 0);
        int threshold = parseIntSafe(condition.getValueSingle(), 5);

        return frequency > threshold;
    }

    private BigDecimal parseThreshold(String value, BigDecimal defaultValue) {
        try {
            return new BigDecimal(value);
        } catch (Exception e) {
            return defaultValue;
        }
    }

    private int parseIntSafe(String value, int defaultValue) {
        try {
            return Integer.parseInt(value);
        } catch (Exception e) {
            return defaultValue;
        }
    }

    @Override
    public String getCategory() {
        return "AMOUNT";
    }
}
