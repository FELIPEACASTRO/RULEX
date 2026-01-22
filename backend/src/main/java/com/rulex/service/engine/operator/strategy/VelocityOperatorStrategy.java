package com.rulex.service.engine.operator.strategy;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.VelocityService;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.math.BigDecimal;
import java.util.Set;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Strategy para operadores de velocity (agregações temporais).
 *
 * <p>Operadores suportados:
 * <ul>
 *   <li>VELOCITY_COUNT_GT - contagem maior que</li>
 *   <li>VELOCITY_COUNT_LT - contagem menor que</li>
 *   <li>VELOCITY_SUM_GT - soma maior que</li>
 *   <li>VELOCITY_SUM_LT - soma menor que</li>
 *   <li>VELOCITY_AVG_GT - média maior que</li>
 *   <li>VELOCITY_AVG_LT - média menor que</li>
 *   <li>VELOCITY_DISTINCT_GT - distintos maior que</li>
 *   <li>VELOCITY_DISTINCT_LT - distintos menor que</li>
 * </ul>
 *
 * @author Refactoring - RULEX Team
 * @since 2.0.0
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class VelocityOperatorStrategy implements OperatorStrategy {

    private static final Set<ConditionOperator> SUPPORTED = Set.of(
        ConditionOperator.VELOCITY_COUNT_GT,
        ConditionOperator.VELOCITY_COUNT_LT,
        ConditionOperator.VELOCITY_SUM_GT,
        ConditionOperator.VELOCITY_SUM_LT,
        ConditionOperator.VELOCITY_AVG_GT,
        ConditionOperator.VELOCITY_AVG_LT,
        ConditionOperator.VELOCITY_DISTINCT_GT,
        ConditionOperator.VELOCITY_DISTINCT_LT
    );

    private final VelocityService velocityService;

    @Override
    public Set<ConditionOperator> supportedOperators() {
        return SUPPORTED;
    }

    @Override
    public String category() {
        return "VELOCITY";
    }

    @Override
    public int priority() {
        return 200; // Prioridade baixa - requer queries ao banco/cache
    }

    @Override
    public boolean requiresExternalServices() {
        return true;
    }

    @Override
    public boolean evaluate(Object fieldValue, RuleCondition condition, EvaluationContext context) {
        ConditionOperator operator = condition.getOperator();

        // Obter TransactionRequest do contexto
        TransactionRequest request = context.getTransactionRequest();
        if (request == null) {
            log.warn("TransactionRequest is null in context for velocity evaluation");
            return false;
        }

        try {
            return switch (operator) {
                case VELOCITY_COUNT_GT -> evaluateVelocityCount(request, condition, true);
                case VELOCITY_COUNT_LT -> evaluateVelocityCount(request, condition, false);
                case VELOCITY_SUM_GT -> evaluateVelocitySum(request, condition, true);
                case VELOCITY_SUM_LT -> evaluateVelocitySum(request, condition, false);
                case VELOCITY_AVG_GT -> evaluateVelocityAvg(request, condition, true);
                case VELOCITY_AVG_LT -> evaluateVelocityAvg(request, condition, false);
                case VELOCITY_DISTINCT_GT -> evaluateVelocityDistinct(request, condition, true);
                case VELOCITY_DISTINCT_LT -> evaluateVelocityDistinct(request, condition, false);
                default -> {
                    log.warn("Unexpected operator in VelocityOperatorStrategy: {}", operator);
                    yield false;
                }
            };
        } catch (Exception e) {
            log.warn("Error evaluating velocity operator {}: {}", operator, e.getMessage());
            return false;
        }
    }

    private boolean evaluateVelocityCount(String identifier, RuleCondition condition,
                                          EvaluationContext context, boolean greaterThan) {
        VelocityParams params = parseVelocityParams(condition);
        if (params == null) {
            return false;
        }

        long count = velocityService.getTransactionCount(
            identifier,
            params.timeWindow()
        );

        return greaterThan ? count > params.threshold() : count < params.threshold();
    }

    private boolean evaluateVelocitySum(String identifier, RuleCondition condition,
                                        EvaluationContext context, boolean greaterThan) {
        VelocityParams params = parseVelocityParams(condition);
        if (params == null) {
            return false;
        }

        BigDecimal sum = velocityService.getTransactionSum(
            identifier,
            params.timeWindow()
        );

        BigDecimal threshold = new BigDecimal(String.valueOf(params.threshold()));
        int comparison = sum.compareTo(threshold);
        return greaterThan ? comparison > 0 : comparison < 0;
    }

    private boolean evaluateVelocityAvg(String identifier, RuleCondition condition,
                                        EvaluationContext context, boolean greaterThan) {
        VelocityParams params = parseVelocityParams(condition);
        if (params == null) {
            return false;
        }

        BigDecimal avg = velocityService.getTransactionAverage(
            identifier,
            params.timeWindow()
        );

        BigDecimal threshold = new BigDecimal(String.valueOf(params.threshold()));
        int comparison = avg.compareTo(threshold);
        return greaterThan ? comparison > 0 : comparison < 0;
    }

    private boolean evaluateVelocityDistinct(String identifier, RuleCondition condition,
                                             EvaluationContext context, boolean greaterThan) {
        VelocityParams params = parseVelocityParams(condition);
        if (params == null) {
            return false;
        }

        // O campo distinct é especificado no fieldPath ou valueSingle
        String distinctField = condition.getFieldPath();
        if (distinctField == null || distinctField.isBlank()) {
            distinctField = "merchantId"; // default
        }

        long count = velocityService.getDistinctCount(
            identifier,
            distinctField,
            params.timeWindow()
        );

        return greaterThan ? count > params.threshold() : count < params.threshold();
    }

    private boolean evaluateSumLastNDays(String identifier, RuleCondition condition, EvaluationContext context) {
        int days = parseIntParam(condition.getValueMin(), 7);
        BigDecimal threshold = parseBigDecimalParam(condition.getValueSingle(), BigDecimal.ZERO);

        BigDecimal sum = velocityService.getTransactionSum(
            identifier,
            VelocityService.TimeWindow.fromDays(days)
        );

        return sum.compareTo(threshold) > 0;
    }

    private boolean evaluateCountLastNHours(String identifier, RuleCondition condition, EvaluationContext context) {
        int hours = parseIntParam(condition.getValueMin(), 24);
        long threshold = parseLongParam(condition.getValueSingle(), 0L);

        long count = velocityService.getTransactionCount(
            identifier,
            VelocityService.TimeWindow.fromHours(hours)
        );

        return count > threshold;
    }

    private boolean evaluateAvgLastNDays(String identifier, RuleCondition condition, EvaluationContext context) {
        int days = parseIntParam(condition.getValueMin(), 30);
        BigDecimal threshold = parseBigDecimalParam(condition.getValueSingle(), BigDecimal.ZERO);

        BigDecimal avg = velocityService.getTransactionAverage(
            identifier,
            VelocityService.TimeWindow.fromDays(days)
        );

        return avg.compareTo(threshold) > 0;
    }

    private boolean evaluateCountDistinctMerchants(String identifier, RuleCondition condition, EvaluationContext context) {
        int days = parseIntParam(condition.getValueMin(), 7);
        long threshold = parseLongParam(condition.getValueSingle(), 0L);

        long count = velocityService.getDistinctCount(
            identifier,
            "merchantId",
            VelocityService.TimeWindow.fromDays(days)
        );

        return count > threshold;
    }

    private boolean evaluateCountDistinctCountries(String identifier, RuleCondition condition, EvaluationContext context) {
        int hours = parseIntParam(condition.getValueMin(), 24);
        long threshold = parseLongParam(condition.getValueSingle(), 0L);

        long count = velocityService.getDistinctCount(
            identifier,
            "merchantCountryCode",
            VelocityService.TimeWindow.fromHours(hours)
        );

        return count > threshold;
    }

    private boolean evaluateMaxAmountLastNDays(String identifier, RuleCondition condition, EvaluationContext context) {
        int days = parseIntParam(condition.getValueMin(), 30);
        BigDecimal threshold = parseBigDecimalParam(condition.getValueSingle(), BigDecimal.ZERO);

        BigDecimal max = velocityService.getMaxAmount(
            identifier,
            VelocityService.TimeWindow.fromDays(days)
        );

        return max.compareTo(threshold) > 0;
    }

    private boolean evaluateMinAmountLastNDays(String identifier, RuleCondition condition, EvaluationContext context) {
        int days = parseIntParam(condition.getValueMin(), 30);
        BigDecimal threshold = parseBigDecimalParam(condition.getValueSingle(), BigDecimal.ZERO);

        BigDecimal min = velocityService.getMinAmount(
            identifier,
            VelocityService.TimeWindow.fromDays(days)
        );

        return min.compareTo(threshold) < 0;
    }

    /**
     * Parse parâmetros de velocity do condition.
     */
    private VelocityParams parseVelocityParams(RuleCondition condition) {
        // Formato esperado em valueSingle: "timeWindowMinutes,threshold"
        // ou usar valueMin para timeWindow e valueSingle para threshold

        String valueSingle = condition.getValueSingle();
        String valueMin = condition.getValueMin();

        int timeWindowMinutes = 60; // default 1 hora
        long threshold = 0;

        if (valueMin != null && !valueMin.isBlank()) {
            try {
                timeWindowMinutes = Integer.parseInt(valueMin.trim());
            } catch (NumberFormatException e) {
                log.warn("Invalid timeWindow: {}", valueMin);
            }
        }

        if (valueSingle != null && !valueSingle.isBlank()) {
            if (valueSingle.contains(",")) {
                String[] parts = valueSingle.split(",");
                try {
                    timeWindowMinutes = Integer.parseInt(parts[0].trim());
                    threshold = Long.parseLong(parts[1].trim());
                } catch (NumberFormatException e) {
                    log.warn("Invalid velocity params: {}", valueSingle);
                    return null;
                }
            } else {
                try {
                    threshold = Long.parseLong(valueSingle.trim());
                } catch (NumberFormatException e) {
                    log.warn("Invalid threshold: {}", valueSingle);
                    return null;
                }
            }
        }

        return new VelocityParams(VelocityService.TimeWindow.fromMinutes(timeWindowMinutes), threshold);
    }

    private int parseIntParam(String value, int defaultValue) {
        if (value == null || value.isBlank()) {
            return defaultValue;
        }
        try {
            return Integer.parseInt(value.trim());
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }

    private long parseLongParam(String value, long defaultValue) {
        if (value == null || value.isBlank()) {
            return defaultValue;
        }
        try {
            return Long.parseLong(value.trim());
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }

    private BigDecimal parseBigDecimalParam(String value, BigDecimal defaultValue) {
        if (value == null || value.isBlank()) {
            return defaultValue;
        }
        try {
            return new BigDecimal(value.trim());
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }

    /**
     * Parâmetros de velocity.
     */
    private record VelocityParams(VelocityService.TimeWindow timeWindow, long threshold) {}
}
