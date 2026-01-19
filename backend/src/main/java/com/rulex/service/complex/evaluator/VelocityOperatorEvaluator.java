package com.rulex.service.complex.evaluator;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.service.VelocityService;
import com.rulex.service.VelocityService.AggregationType;
import com.rulex.service.VelocityService.KeyType;
import com.rulex.service.VelocityService.TimeWindow;
import com.rulex.service.VelocityService.VelocityStats;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.math.BigDecimal;
import java.util.Map;
import java.util.Set;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Avaliador para operadores de velocidade.
 *
 * <p>Operadores suportados:
 * <ul>
 *   <li>VELOCITY_COUNT_GT, VELOCITY_COUNT_LT - contagem de transações</li>
 *   <li>VELOCITY_SUM_GT, VELOCITY_SUM_LT - soma de valores</li>
 *   <li>VELOCITY_AVG_GT, VELOCITY_AVG_LT - média de valores</li>
 *   <li>VELOCITY_DISTINCT_GT, VELOCITY_DISTINCT_LT - contagem distinta</li>
 *   <li>COUNT_LAST_N_HOURS, SUM_LAST_N_DAYS, AVG_LAST_N_DAYS</li>
 *   <li>COUNT_DISTINCT_* - contagens distintas por período</li>
 * </ul>
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class VelocityOperatorEvaluator implements OperatorEvaluator {

    private final VelocityService velocityService;

    private static final Set<ConditionOperator> SUPPORTED = Set.of(
        ConditionOperator.VELOCITY_COUNT_GT,
        ConditionOperator.VELOCITY_COUNT_LT,
        ConditionOperator.VELOCITY_SUM_GT,
        ConditionOperator.VELOCITY_SUM_LT,
        ConditionOperator.VELOCITY_AVG_GT,
        ConditionOperator.VELOCITY_AVG_LT,
        ConditionOperator.VELOCITY_DISTINCT_GT,
        ConditionOperator.VELOCITY_DISTINCT_LT,
        ConditionOperator.SUM_LAST_N_DAYS,
        ConditionOperator.COUNT_LAST_N_HOURS,
        ConditionOperator.AVG_LAST_N_DAYS,
        ConditionOperator.COUNT_DISTINCT_MERCHANTS_LAST_N_DAYS,
        ConditionOperator.COUNT_DISTINCT_COUNTRIES_LAST_N_HOURS
    );

    @Override
    public Set<ConditionOperator> getSupportedOperators() {
        return SUPPORTED;
    }

    @Override
    public boolean evaluate(RuleCondition condition, EvaluationContext context) {
        ConditionOperator op = condition.getOperator();
        String fieldName = condition.getFieldName();
        String keyValue = getKeyValue(context, fieldName);

        log.debug("VelocityOperatorEvaluator: op={}, field={}, keyValue={}", op, fieldName, keyValue);

        if (keyValue == null || keyValue.isEmpty()) {
            log.debug("Key value is null or empty for field: {}", fieldName);
            return false;
        }

        try {
            return switch (op) {
                case VELOCITY_COUNT_GT -> evaluateVelocityCountGt(keyValue, condition, context);
                case VELOCITY_COUNT_LT -> evaluateVelocityCountLt(keyValue, condition, context);
                case VELOCITY_SUM_GT -> evaluateVelocitySumGt(keyValue, condition, context);
                case VELOCITY_SUM_LT -> evaluateVelocitySumLt(keyValue, condition, context);
                case VELOCITY_AVG_GT -> evaluateVelocityAvgGt(keyValue, condition, context);
                case VELOCITY_AVG_LT -> evaluateVelocityAvgLt(keyValue, condition, context);
                case VELOCITY_DISTINCT_GT -> evaluateVelocityDistinctGt(keyValue, condition, context);
                case VELOCITY_DISTINCT_LT -> evaluateVelocityDistinctLt(keyValue, condition, context);
                case SUM_LAST_N_DAYS -> evaluateSumLastNDays(keyValue, condition, context);
                case COUNT_LAST_N_HOURS -> evaluateCountLastNHours(keyValue, condition, context);
                case AVG_LAST_N_DAYS -> evaluateAvgLastNDays(keyValue, condition, context);
                case COUNT_DISTINCT_MERCHANTS_LAST_N_DAYS -> evaluateCountDistinctMerchantsLastNDays(keyValue, condition, context);
                case COUNT_DISTINCT_COUNTRIES_LAST_N_HOURS -> evaluateCountDistinctCountriesLastNHours(keyValue, condition, context);
                default -> false;
            };
        } catch (Exception e) {
            log.error("Erro ao avaliar operador de velocidade {}: {}", op, e.getMessage());
            return false;
        }
    }

    private String getKeyValue(EvaluationContext context, String fieldName) {
        if (context == null || fieldName == null) {
            return null;
        }

        Map<String, Object> payload = context.getPayload();
        if (payload != null && payload.containsKey(fieldName)) {
            Object value = payload.get(fieldName);
            return value != null ? String.valueOf(value) : null;
        }

        if (context.getTransactionRequest() != null) {
            try {
                var request = context.getTransactionRequest();
                var field = request.getClass().getDeclaredField(fieldName);
                field.setAccessible(true);
                Object value = field.get(request);
                return value != null ? String.valueOf(value) : null;
            } catch (Exception e) {
                log.trace("Campo {} não encontrado no TransactionRequest", fieldName);
            }
        }

        return null;
    }

    private boolean evaluateVelocityCountGt(String keyValue, RuleCondition condition, EvaluationContext context) {
        TransactionRequest request = context.getTransactionRequest();
        if (request == null) return false;

        KeyType keyType = resolveKeyType(condition.getFieldName());
        TimeWindow window = resolveTimeWindow(condition);
        VelocityStats stats = velocityService.getStats(request, keyType, window);

        long count = stats.getTransactionCount();
        long threshold = parseThreshold(condition.getValueSingle());
        log.debug("VELOCITY_COUNT_GT: count={}, threshold={}", count, threshold);
        return count > threshold;
    }

    private boolean evaluateVelocityCountLt(String keyValue, RuleCondition condition, EvaluationContext context) {
        TransactionRequest request = context.getTransactionRequest();
        if (request == null) return false;

        KeyType keyType = resolveKeyType(condition.getFieldName());
        TimeWindow window = resolveTimeWindow(condition);
        VelocityStats stats = velocityService.getStats(request, keyType, window);

        long count = stats.getTransactionCount();
        long threshold = parseThreshold(condition.getValueSingle());
        log.debug("VELOCITY_COUNT_LT: count={}, threshold={}", count, threshold);
        return count < threshold;
    }

    private boolean evaluateVelocitySumGt(String keyValue, RuleCondition condition, EvaluationContext context) {
        TransactionRequest request = context.getTransactionRequest();
        if (request == null) return false;

        KeyType keyType = resolveKeyType(condition.getFieldName());
        TimeWindow window = resolveTimeWindow(condition);
        BigDecimal sum = velocityService.getAggregation(request, keyType, window, AggregationType.SUM);

        BigDecimal threshold = parseBigDecimalThreshold(condition.getValueSingle());
        log.debug("VELOCITY_SUM_GT: sum={}, threshold={}", sum, threshold);
        return sum.compareTo(threshold) > 0;
    }

    private boolean evaluateVelocitySumLt(String keyValue, RuleCondition condition, EvaluationContext context) {
        TransactionRequest request = context.getTransactionRequest();
        if (request == null) return false;

        KeyType keyType = resolveKeyType(condition.getFieldName());
        TimeWindow window = resolveTimeWindow(condition);
        BigDecimal sum = velocityService.getAggregation(request, keyType, window, AggregationType.SUM);

        BigDecimal threshold = parseBigDecimalThreshold(condition.getValueSingle());
        log.debug("VELOCITY_SUM_LT: sum={}, threshold={}", sum, threshold);
        return sum.compareTo(threshold) < 0;
    }

    private boolean evaluateVelocityAvgGt(String keyValue, RuleCondition condition, EvaluationContext context) {
        TransactionRequest request = context.getTransactionRequest();
        if (request == null) return false;

        KeyType keyType = resolveKeyType(condition.getFieldName());
        TimeWindow window = resolveTimeWindow(condition);
        BigDecimal avg = velocityService.getAggregation(request, keyType, window, AggregationType.AVG);

        BigDecimal threshold = parseBigDecimalThreshold(condition.getValueSingle());
        log.debug("VELOCITY_AVG_GT: avg={}, threshold={}", avg, threshold);
        return avg.compareTo(threshold) > 0;
    }

    private boolean evaluateVelocityAvgLt(String keyValue, RuleCondition condition, EvaluationContext context) {
        TransactionRequest request = context.getTransactionRequest();
        if (request == null) return false;

        KeyType keyType = resolveKeyType(condition.getFieldName());
        TimeWindow window = resolveTimeWindow(condition);
        BigDecimal avg = velocityService.getAggregation(request, keyType, window, AggregationType.AVG);

        BigDecimal threshold = parseBigDecimalThreshold(condition.getValueSingle());
        log.debug("VELOCITY_AVG_LT: avg={}, threshold={}", avg, threshold);
        return avg.compareTo(threshold) < 0;
    }

    private boolean evaluateVelocityDistinctGt(String keyValue, RuleCondition condition, EvaluationContext context) {
        TransactionRequest request = context.getTransactionRequest();
        if (request == null) return false;

        KeyType keyType = resolveKeyType(condition.getFieldName());
        TimeWindow window = resolveTimeWindow(condition);
        BigDecimal count = velocityService.getAggregation(request, keyType, window, AggregationType.DISTINCT_MERCHANTS);

        long threshold = parseThreshold(condition.getValueSingle());
        log.debug("VELOCITY_DISTINCT_GT: count={}, threshold={}", count, threshold);
        return count.longValue() > threshold;
    }

    private boolean evaluateVelocityDistinctLt(String keyValue, RuleCondition condition, EvaluationContext context) {
        TransactionRequest request = context.getTransactionRequest();
        if (request == null) return false;

        KeyType keyType = resolveKeyType(condition.getFieldName());
        TimeWindow window = resolveTimeWindow(condition);
        BigDecimal count = velocityService.getAggregation(request, keyType, window, AggregationType.DISTINCT_MERCHANTS);

        long threshold = parseThreshold(condition.getValueSingle());
        log.debug("VELOCITY_DISTINCT_LT: count={}, threshold={}", count, threshold);
        return count.longValue() < threshold;
    }

    private boolean evaluateSumLastNDays(String keyValue, RuleCondition condition, EvaluationContext context) {
        TransactionRequest request = context.getTransactionRequest();
        if (request == null) return false;

        int days = parseIntSafe(condition.getValueMin(), 1);
        KeyType keyType = resolveKeyType(condition.getFieldName());
        TimeWindow window = resolveTimeWindowFromDays(days);
        BigDecimal sum = velocityService.getAggregation(request, keyType, window, AggregationType.SUM);

        BigDecimal threshold = parseBigDecimalThreshold(condition.getValueSingle());
        log.debug("SUM_LAST_N_DAYS: days={}, sum={}, threshold={}", days, sum, threshold);
        return sum.compareTo(threshold) > 0;
    }

    private boolean evaluateCountLastNHours(String keyValue, RuleCondition condition, EvaluationContext context) {
        TransactionRequest request = context.getTransactionRequest();
        if (request == null) return false;

        int hours = parseIntSafe(condition.getValueMin(), 1);
        KeyType keyType = resolveKeyType(condition.getFieldName());
        TimeWindow window = resolveTimeWindowFromHours(hours);
        BigDecimal count = velocityService.getAggregation(request, keyType, window, AggregationType.COUNT);

        long threshold = parseThreshold(condition.getValueSingle());
        log.debug("COUNT_LAST_N_HOURS: hours={}, count={}, threshold={}", hours, count, threshold);
        return count.longValue() > threshold;
    }

    private boolean evaluateAvgLastNDays(String keyValue, RuleCondition condition, EvaluationContext context) {
        TransactionRequest request = context.getTransactionRequest();
        if (request == null) return false;

        int days = parseIntSafe(condition.getValueMin(), 1);
        KeyType keyType = resolveKeyType(condition.getFieldName());
        TimeWindow window = resolveTimeWindowFromDays(days);
        BigDecimal avg = velocityService.getAggregation(request, keyType, window, AggregationType.AVG);

        BigDecimal threshold = parseBigDecimalThreshold(condition.getValueSingle());
        log.debug("AVG_LAST_N_DAYS: days={}, avg={}, threshold={}", days, avg, threshold);
        return avg.compareTo(threshold) > 0;
    }

    private boolean evaluateCountDistinctMerchantsLastNDays(String keyValue, RuleCondition condition, EvaluationContext context) {
        TransactionRequest request = context.getTransactionRequest();
        if (request == null) return false;

        int days = parseIntSafe(condition.getValueMin(), 1);
        KeyType keyType = resolveKeyType(condition.getFieldName());
        TimeWindow window = resolveTimeWindowFromDays(days);
        BigDecimal count = velocityService.getAggregation(request, keyType, window, AggregationType.DISTINCT_MERCHANTS);

        long threshold = parseThreshold(condition.getValueSingle());
        log.debug("COUNT_DISTINCT_MERCHANTS_LAST_N_DAYS: days={}, count={}, threshold={}", days, count, threshold);
        return count.longValue() > threshold;
    }

    private boolean evaluateCountDistinctCountriesLastNHours(String keyValue, RuleCondition condition, EvaluationContext context) {
        TransactionRequest request = context.getTransactionRequest();
        if (request == null) return false;

        int hours = parseIntSafe(condition.getValueMin(), 1);
        KeyType keyType = resolveKeyType(condition.getFieldName());
        TimeWindow window = resolveTimeWindowFromHours(hours);
        BigDecimal count = velocityService.getAggregation(request, keyType, window, AggregationType.DISTINCT_COUNTRIES);

        long threshold = parseThreshold(condition.getValueSingle());
        log.debug("COUNT_DISTINCT_COUNTRIES_LAST_N_HOURS: hours={}, count={}, threshold={}", hours, count, threshold);
        return count.longValue() > threshold;
    }

    private KeyType resolveKeyType(String fieldName) {
        if (fieldName == null) return KeyType.PAN;
        return switch (fieldName.toLowerCase()) {
            case "pan", "cardnumber", "card_number" -> KeyType.PAN;
            case "customerid", "customer_id", "customer" -> KeyType.CUSTOMER_ID;
            case "merchantid", "merchant_id", "merchant" -> KeyType.MERCHANT_ID;
            default -> KeyType.PAN;
        };
    }

    private TimeWindow resolveTimeWindow(RuleCondition condition) {
        String expression = condition.getValueExpression();
        if (expression != null && !expression.isEmpty()) {
            return parseTimeWindow(expression);
        }
        return TimeWindow.HOUR_1;
    }

    private TimeWindow resolveTimeWindowFromHours(int hours) {
        return switch (hours) {
            case 1 -> TimeWindow.HOUR_1;
            case 6 -> TimeWindow.HOUR_6;
            case 12 -> TimeWindow.HOUR_12;
            case 24 -> TimeWindow.HOUR_24;
            default -> hours <= 1 ? TimeWindow.HOUR_1 :
                       hours <= 6 ? TimeWindow.HOUR_6 :
                       hours <= 12 ? TimeWindow.HOUR_12 : TimeWindow.HOUR_24;
        };
    }

    private TimeWindow resolveTimeWindowFromDays(int days) {
        return switch (days) {
            case 1 -> TimeWindow.HOUR_24;
            case 7 -> TimeWindow.DAY_7;
            case 30 -> TimeWindow.DAY_30;
            default -> days <= 1 ? TimeWindow.HOUR_24 :
                       days <= 7 ? TimeWindow.DAY_7 : TimeWindow.DAY_30;
        };
    }

    private TimeWindow parseTimeWindow(String expression) {
        if (expression == null) return TimeWindow.HOUR_1;
        String lower = expression.toLowerCase();
        if (lower.contains("5m")) return TimeWindow.MINUTE_5;
        if (lower.contains("15m")) return TimeWindow.MINUTE_15;
        if (lower.contains("30m")) return TimeWindow.MINUTE_30;
        if (lower.contains("1h")) return TimeWindow.HOUR_1;
        if (lower.contains("6h")) return TimeWindow.HOUR_6;
        if (lower.contains("12h")) return TimeWindow.HOUR_12;
        if (lower.contains("24h") || lower.contains("1d")) return TimeWindow.HOUR_24;
        if (lower.contains("7d")) return TimeWindow.DAY_7;
        if (lower.contains("30d")) return TimeWindow.DAY_30;
        return TimeWindow.HOUR_1;
    }

    private long parseThreshold(String value) {
        try {
            return Long.parseLong(value);
        } catch (Exception e) {
            log.warn("Erro ao parsear threshold '{}', usando 0", value);
            return 0;
        }
    }

    private BigDecimal parseBigDecimalThreshold(String value) {
        try {
            return new BigDecimal(value);
        } catch (Exception e) {
            log.warn("Erro ao parsear threshold '{}', usando 0", value);
            return BigDecimal.ZERO;
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
        return "VELOCITY";
    }
}
