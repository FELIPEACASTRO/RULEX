package com.rulex.service.engine.operator.strategy;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.VelocityService;
import com.rulex.service.VelocityService.AggregationType;
import com.rulex.service.VelocityService.KeyType;
import com.rulex.service.VelocityService.TimeWindow;
import com.rulex.service.VelocityService.VelocityStats;
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
            // Determinar KeyType e TimeWindow
            KeyType keyType = determineKeyType(condition);
            TimeWindow timeWindow = determineTimeWindow(condition);
            long threshold = parseThreshold(condition);

            // Obter stats usando a API do VelocityService
            VelocityStats stats = velocityService.getStats(request, keyType, timeWindow);
            if (stats == null || !stats.isFound()) {
                log.debug("No velocity stats found for request");
                return false;
            }

            return switch (operator) {
                case VELOCITY_COUNT_GT -> stats.getTransactionCount() > threshold;
                case VELOCITY_COUNT_LT -> stats.getTransactionCount() < threshold;
                case VELOCITY_SUM_GT -> stats.getTotalAmount().compareTo(BigDecimal.valueOf(threshold)) > 0;
                case VELOCITY_SUM_LT -> stats.getTotalAmount().compareTo(BigDecimal.valueOf(threshold)) < 0;
                case VELOCITY_AVG_GT -> stats.getAvgAmount().compareTo(BigDecimal.valueOf(threshold)) > 0;
                case VELOCITY_AVG_LT -> stats.getAvgAmount().compareTo(BigDecimal.valueOf(threshold)) < 0;
                case VELOCITY_DISTINCT_GT -> evaluateDistinctGt(stats, condition, threshold);
                case VELOCITY_DISTINCT_LT -> evaluateDistinctLt(stats, condition, threshold);
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

    /**
     * Avalia VELOCITY_DISTINCT_GT baseado no campo especificado.
     */
    private boolean evaluateDistinctGt(VelocityStats stats, RuleCondition condition, long threshold) {
        String distinctField = condition.getFieldPath();
        if (distinctField == null || distinctField.isBlank()) {
            distinctField = "merchant"; // default
        }

        long distinctCount = getDistinctCount(stats, distinctField);
        return distinctCount > threshold;
    }

    /**
     * Avalia VELOCITY_DISTINCT_LT baseado no campo especificado.
     */
    private boolean evaluateDistinctLt(VelocityStats stats, RuleCondition condition, long threshold) {
        String distinctField = condition.getFieldPath();
        if (distinctField == null || distinctField.isBlank()) {
            distinctField = "merchant"; // default
        }

        long distinctCount = getDistinctCount(stats, distinctField);
        return distinctCount < threshold;
    }

    /**
     * Obtém contagem de distintos do stats baseado no campo.
     */
    private long getDistinctCount(VelocityStats stats, String field) {
        return switch (field.toLowerCase()) {
            case "merchant", "merchantid", "merchant_id" -> stats.getDistinctMerchants();
            case "mcc" -> stats.getDistinctMccs();
            case "country", "merchantcountrycode", "merchant_country_code" -> stats.getDistinctCountries();
            case "pan" -> stats.getDistinctPans();
            default -> stats.getDistinctMerchants(); // fallback
        };
    }

    /**
     * Determina o KeyType baseado na condição.
     */
    private KeyType determineKeyType(RuleCondition condition) {
        String fieldName = condition.getFieldName();
        if (fieldName == null || fieldName.isBlank()) {
            return KeyType.PAN; // default
        }

        return switch (fieldName.toLowerCase()) {
            case "pan", "cardnumber", "card_number" -> KeyType.PAN;
            case "customerid", "customer_id", "customeridfromheader" -> KeyType.CUSTOMER_ID;
            case "merchantid", "merchant_id" -> KeyType.MERCHANT_ID;
            case "ipaddress", "ip_address", "ip" -> KeyType.IP_ADDRESS;
            case "deviceid", "device_id" -> KeyType.DEVICE_ID;
            default -> KeyType.PAN;
        };
    }

    /**
     * Determina o TimeWindow baseado na condição.
     * Formato esperado em valueMin: minutos (5, 15, 30, 60, 360, 720, 1440, 10080, 43200)
     */
    private TimeWindow determineTimeWindow(RuleCondition condition) {
        String valueMin = condition.getValueMin();
        if (valueMin == null || valueMin.isBlank()) {
            return TimeWindow.HOUR_1; // default 1 hora
        }

        try {
            int minutes = Integer.parseInt(valueMin.trim());
            return switch (minutes) {
                case 5 -> TimeWindow.MINUTE_5;
                case 15 -> TimeWindow.MINUTE_15;
                case 30 -> TimeWindow.MINUTE_30;
                case 60 -> TimeWindow.HOUR_1;
                case 360 -> TimeWindow.HOUR_6;
                case 720 -> TimeWindow.HOUR_12;
                case 1440 -> TimeWindow.HOUR_24;
                case 10080 -> TimeWindow.DAY_7;
                case 43200 -> TimeWindow.DAY_30;
                default -> {
                    // Encontrar o mais próximo
                    if (minutes <= 5) yield TimeWindow.MINUTE_5;
                    else if (minutes <= 15) yield TimeWindow.MINUTE_15;
                    else if (minutes <= 30) yield TimeWindow.MINUTE_30;
                    else if (minutes <= 60) yield TimeWindow.HOUR_1;
                    else if (minutes <= 360) yield TimeWindow.HOUR_6;
                    else if (minutes <= 720) yield TimeWindow.HOUR_12;
                    else if (minutes <= 1440) yield TimeWindow.HOUR_24;
                    else if (minutes <= 10080) yield TimeWindow.DAY_7;
                    else yield TimeWindow.DAY_30;
                }
            };
        } catch (NumberFormatException e) {
            log.warn("Invalid timeWindow value: {}, using default HOUR_1", valueMin);
            return TimeWindow.HOUR_1;
        }
    }

    /**
     * Parse do threshold da condição.
     */
    private long parseThreshold(RuleCondition condition) {
        String valueSingle = condition.getValueSingle();
        if (valueSingle == null || valueSingle.isBlank()) {
            return 0;
        }

        try {
            // Se contém vírgula, o threshold é o segundo valor
            if (valueSingle.contains(",")) {
                String[] parts = valueSingle.split(",");
                return Long.parseLong(parts[parts.length - 1].trim());
            }
            return Long.parseLong(valueSingle.trim());
        } catch (NumberFormatException e) {
            log.warn("Invalid threshold value: {}", valueSingle);
            return 0;
        }
    }
}
