package com.rulex.service.complex.evaluator;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.RuleCondition.ConditionOperator;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.math.BigDecimal;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.Map;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Avaliador para operadores relacionados a conta e idade de conta.
 *
 * <p>Operadores suportados:
 * <ul>
 *   <li>ACCOUNT_AGE_LT_MINUTES - conta criada há menos de N minutos</li>
 *   <li>ACCOUNT_LINK_DEPTH - profundidade de links da conta</li>
 *   <li>ACCOUNT_TAKEOVER_PATTERN - padrão de tomada de conta</li>
 *   <li>DAYS_SINCE_LAST_ACTIVITY - dias desde última atividade</li>
 *   <li>CHARGEBACK_RATE_GT - taxa de chargeback maior que</li>
 *   <li>IS_NEW - conta é nova</li>
 *   <li>IS_FIRST - primeira transação</li>
 * </ul>
 */
@Component
@Slf4j
public class AccountOperatorEvaluator implements OperatorEvaluator {

    private static final Set<ConditionOperator> SUPPORTED = Set.of(
        ConditionOperator.ACCOUNT_AGE_LT_MINUTES,
        ConditionOperator.ACCOUNT_LINK_DEPTH,
        ConditionOperator.ACCOUNT_TAKEOVER_PATTERN,
        ConditionOperator.DAYS_SINCE_LAST_ACTIVITY,
        ConditionOperator.CHARGEBACK_RATE_GT,
        ConditionOperator.IS_NEW,
        ConditionOperator.IS_FIRST
    );

    @Override
    public Set<ConditionOperator> getSupportedOperators() {
        return SUPPORTED;
    }

    @Override
    public boolean evaluate(RuleCondition condition, EvaluationContext context) {
        ConditionOperator op = condition.getOperator();
        
        log.debug("AccountOperatorEvaluator: op={}, field={}", op, condition.getFieldName());

        return switch (op) {
            case ACCOUNT_AGE_LT_MINUTES -> evaluateAccountAgeLtMinutes(condition, context);
            case ACCOUNT_LINK_DEPTH -> evaluateAccountLinkDepth(condition, context);
            case ACCOUNT_TAKEOVER_PATTERN -> evaluateAccountTakeoverPattern(condition, context);
            case DAYS_SINCE_LAST_ACTIVITY -> evaluateDaysSinceLastActivity(condition, context);
            case CHARGEBACK_RATE_GT -> evaluateChargebackRateGt(condition, context);
            case IS_NEW -> evaluateIsNew(condition, context);
            case IS_FIRST -> evaluateIsFirst(condition, context);
            default -> false;
        };
    }

    private Object getFieldValue(EvaluationContext context, String fieldName) {
        if (context == null || fieldName == null) {
            return null;
        }

        Map<String, Object> payload = context.getPayload();
        if (payload != null && payload.containsKey(fieldName)) {
            return payload.get(fieldName);
        }

        if (context.getTransactionRequest() != null) {
            try {
                var request = context.getTransactionRequest();
                var field = request.getClass().getDeclaredField(fieldName);
                field.setAccessible(true);
                return field.get(request);
            } catch (Exception e) {
                log.trace("Campo {} não encontrado no TransactionRequest", fieldName);
            }
        }

        return null;
    }

    /**
     * ACCOUNT_AGE_LT_MINUTES - verifica se a conta foi criada há menos de N minutos.
     */
    private boolean evaluateAccountAgeLtMinutes(RuleCondition condition, EvaluationContext context) {
        Map<String, Object> payload = context.getPayload();
        if (payload == null) {
            return false;
        }

        // Tentar obter data de criação da conta
        Object accountCreatedAt = payload.get("accountCreatedAt");
        if (accountCreatedAt == null) {
            accountCreatedAt = payload.get("account_created_at");
        }
        if (accountCreatedAt == null) {
            accountCreatedAt = payload.get("createdAt");
        }

        if (accountCreatedAt == null) {
            log.debug("ACCOUNT_AGE_LT_MINUTES: data de criação não encontrada");
            return false;
        }

        // Parsear threshold
        int thresholdMinutes;
        try {
            thresholdMinutes = Integer.parseInt(condition.getValueSingle());
        } catch (NumberFormatException e) {
            log.warn("ACCOUNT_AGE_LT_MINUTES: threshold inválido: {}", condition.getValueSingle());
            return false;
        }

        // Calcular idade em minutos
        long ageMinutes = calculateAgeInMinutes(accountCreatedAt);
        
        log.debug("ACCOUNT_AGE_LT_MINUTES: ageMinutes={}, threshold={}", ageMinutes, thresholdMinutes);
        return ageMinutes < thresholdMinutes;
    }

    /**
     * ACCOUNT_LINK_DEPTH - verifica profundidade de links (relacionamentos) da conta.
     */
    private boolean evaluateAccountLinkDepth(RuleCondition condition, EvaluationContext context) {
        Map<String, Object> payload = context.getPayload();
        if (payload == null) {
            return false;
        }

        Object linkDepth = payload.get("accountLinkDepth");
        if (linkDepth == null) {
            linkDepth = payload.get("link_depth");
        }

        if (linkDepth == null) {
            return false;
        }

        int depth;
        try {
            depth = Integer.parseInt(String.valueOf(linkDepth));
        } catch (NumberFormatException e) {
            return false;
        }

        int threshold = parseIntSafe(condition.getValueSingle(), 0);
        
        // Se tem min/max, usar BETWEEN
        if (condition.getValueMin() != null && condition.getValueMax() != null) {
            int min = parseIntSafe(condition.getValueMin(), 0);
            int max = parseIntSafe(condition.getValueMax(), Integer.MAX_VALUE);
            return depth >= min && depth <= max;
        }

        return depth > threshold;
    }

    /**
     * ACCOUNT_TAKEOVER_PATTERN - detecta padrões de tomada de conta.
     */
    private boolean evaluateAccountTakeoverPattern(RuleCondition condition, EvaluationContext context) {
        Map<String, Object> payload = context.getPayload();
        if (payload == null) {
            return false;
        }

        // Verificar indicadores de ATO (Account Takeover)
        boolean passwordChanged = getBooleanValue(payload, "passwordChangedRecently");
        boolean emailChanged = getBooleanValue(payload, "emailChangedRecently");
        boolean phoneChanged = getBooleanValue(payload, "phoneChangedRecently");
        boolean newDevice = getBooleanValue(payload, "isNewDevice");
        boolean newLocation = getBooleanValue(payload, "isNewLocation");
        boolean unusualTime = getBooleanValue(payload, "isUnusualTime");

        // Contar indicadores positivos
        int indicators = 0;
        if (passwordChanged) indicators++;
        if (emailChanged) indicators++;
        if (phoneChanged) indicators++;
        if (newDevice) indicators++;
        if (newLocation) indicators++;
        if (unusualTime) indicators++;

        // Threshold padrão: 3 ou mais indicadores = padrão de ATO
        int threshold = parseIntSafe(condition.getValueSingle(), 3);
        
        log.debug("ACCOUNT_TAKEOVER_PATTERN: indicators={}, threshold={}", indicators, threshold);
        return indicators >= threshold;
    }

    /**
     * DAYS_SINCE_LAST_ACTIVITY - dias desde a última atividade.
     */
    private boolean evaluateDaysSinceLastActivity(RuleCondition condition, EvaluationContext context) {
        Map<String, Object> payload = context.getPayload();
        if (payload == null) {
            return false;
        }

        Object lastActivity = payload.get("lastActivityAt");
        if (lastActivity == null) {
            lastActivity = payload.get("last_activity_at");
        }
        if (lastActivity == null) {
            lastActivity = payload.get("lastTransactionAt");
        }

        if (lastActivity == null) {
            // Se não há última atividade, considerar como conta dormante
            return true;
        }

        long daysSince = calculateAgeInDays(lastActivity);
        int threshold = parseIntSafe(condition.getValueSingle(), 30);

        log.debug("DAYS_SINCE_LAST_ACTIVITY: daysSince={}, threshold={}", daysSince, threshold);
        return daysSince > threshold;
    }

    /**
     * CHARGEBACK_RATE_GT - taxa de chargeback maior que threshold.
     */
    private boolean evaluateChargebackRateGt(RuleCondition condition, EvaluationContext context) {
        Map<String, Object> payload = context.getPayload();
        if (payload == null) {
            return false;
        }

        Object chargebackRate = payload.get("chargebackRate");
        if (chargebackRate == null) {
            chargebackRate = payload.get("chargeback_rate");
        }

        // Se não há taxa, calcular se possível
        if (chargebackRate == null) {
            Object chargebacks = payload.get("chargebackCount");
            Object transactions = payload.get("transactionCount");
            
            if (chargebacks != null && transactions != null) {
                int cbCount = parseIntSafe(String.valueOf(chargebacks), 0);
                int txCount = parseIntSafe(String.valueOf(transactions), 1);
                if (txCount > 0) {
                    chargebackRate = (double) cbCount / txCount * 100;
                }
            }
        }

        if (chargebackRate == null) {
            return false;
        }

        BigDecimal rate;
        try {
            rate = new BigDecimal(String.valueOf(chargebackRate));
        } catch (NumberFormatException e) {
            return false;
        }

        BigDecimal threshold = parseBigDecimalSafe(condition.getValueSingle(), BigDecimal.ONE);
        
        log.debug("CHARGEBACK_RATE_GT: rate={}, threshold={}", rate, threshold);
        return rate.compareTo(threshold) > 0;
    }

    /**
     * IS_NEW - verifica se a conta é nova (criada recentemente).
     */
    private boolean evaluateIsNew(RuleCondition condition, EvaluationContext context) {
        Map<String, Object> payload = context.getPayload();
        if (payload == null) {
            return false;
        }

        // Verificar flag direta
        Object isNew = payload.get("isNewAccount");
        if (isNew == null) {
            isNew = payload.get("is_new_account");
        }
        if (isNew != null) {
            return getBooleanValue(payload, "isNewAccount") || getBooleanValue(payload, "is_new_account");
        }

        // Calcular baseado na idade da conta
        Object accountCreatedAt = payload.get("accountCreatedAt");
        if (accountCreatedAt == null) {
            accountCreatedAt = payload.get("account_created_at");
        }

        if (accountCreatedAt != null) {
            long ageDays = calculateAgeInDays(accountCreatedAt);
            int thresholdDays = parseIntSafe(condition.getValueSingle(), 30);
            return ageDays <= thresholdDays;
        }

        return false;
    }

    /**
     * IS_FIRST - verifica se é a primeira transação do cliente.
     */
    private boolean evaluateIsFirst(RuleCondition condition, EvaluationContext context) {
        Map<String, Object> payload = context.getPayload();
        if (payload == null) {
            return false;
        }

        // Verificar flag direta
        if (getBooleanValue(payload, "isFirstTransaction") || getBooleanValue(payload, "is_first_transaction")) {
            return true;
        }

        // Verificar contagem de transações
        Object txCount = payload.get("transactionCount");
        if (txCount == null) {
            txCount = payload.get("transaction_count");
        }
        if (txCount == null) {
            txCount = payload.get("previousTransactionCount");
        }

        if (txCount != null) {
            int count = parseIntSafe(String.valueOf(txCount), -1);
            return count == 0;
        }

        return false;
    }

    // Métodos auxiliares

    private long calculateAgeInMinutes(Object timestamp) {
        LocalDateTime createdAt = parseDateTime(timestamp);
        if (createdAt == null) {
            return Long.MAX_VALUE;
        }
        return ChronoUnit.MINUTES.between(createdAt, LocalDateTime.now());
    }

    private long calculateAgeInDays(Object timestamp) {
        LocalDateTime dateTime = parseDateTime(timestamp);
        if (dateTime == null) {
            return Long.MAX_VALUE;
        }
        return ChronoUnit.DAYS.between(dateTime, LocalDateTime.now());
    }

    private LocalDateTime parseDateTime(Object value) {
        if (value == null) {
            return null;
        }

        if (value instanceof LocalDateTime ldt) {
            return ldt;
        }

        if (value instanceof Instant instant) {
            return LocalDateTime.ofInstant(instant, ZoneId.systemDefault());
        }

        if (value instanceof Long epochMillis) {
            return LocalDateTime.ofInstant(Instant.ofEpochMilli(epochMillis), ZoneId.systemDefault());
        }

        // Tentar parsear string
        String str = String.valueOf(value);
        try {
            // Tentar ISO format
            return LocalDateTime.parse(str);
        } catch (Exception e1) {
            try {
                // Tentar epoch millis
                long millis = Long.parseLong(str);
                return LocalDateTime.ofInstant(Instant.ofEpochMilli(millis), ZoneId.systemDefault());
            } catch (Exception e2) {
                log.trace("Não foi possível parsear datetime: {}", str);
                return null;
            }
        }
    }

    private boolean getBooleanValue(Map<String, Object> payload, String key) {
        Object value = payload.get(key);
        if (value == null) {
            return false;
        }
        if (value instanceof Boolean b) {
            return b;
        }
        String str = String.valueOf(value).toLowerCase();
        return "true".equals(str) || "1".equals(str) || "yes".equals(str);
    }

    private int parseIntSafe(String value, int defaultValue) {
        try {
            return Integer.parseInt(value);
        } catch (Exception e) {
            return defaultValue;
        }
    }

    private BigDecimal parseBigDecimalSafe(String value, BigDecimal defaultValue) {
        try {
            return new BigDecimal(value);
        } catch (Exception e) {
            return defaultValue;
        }
    }

    @Override
    public String getCategory() {
        return "ACCOUNT";
    }
}
