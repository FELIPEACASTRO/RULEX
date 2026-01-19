package com.rulex.service.complex.evaluator;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.math.BigDecimal;
import java.util.Map;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Avaliador para operadores de AML (Anti-Money Laundering) e Fraude.
 *
 * <p>Operadores suportados:
 * <ul>
 *   <li>STRUCTURING_DETECTION - detecção de estruturação (smurfing)</li>
 *   <li>LAYERING_PATTERN - padrão de camadas</li>
 * </ul>
 */
@Component
@Slf4j
public class AmlFraudOperatorEvaluator implements OperatorEvaluator {

    // Limites comuns de AML
    private static final BigDecimal CTR_THRESHOLD = new BigDecimal("10000"); // Currency Transaction Report threshold
    private static final BigDecimal STRUCTURING_MARGIN = new BigDecimal("1000"); // Margem abaixo do CTR

    private static final Set<ConditionOperator> SUPPORTED = Set.of(
        ConditionOperator.STRUCTURING_DETECTION,
        ConditionOperator.LAYERING_PATTERN
    );

    @Override
    public Set<ConditionOperator> getSupportedOperators() {
        return SUPPORTED;
    }

    @Override
    public boolean evaluate(RuleCondition condition, EvaluationContext context) {
        ConditionOperator op = condition.getOperator();
        TransactionRequest request = context.getTransactionRequest();
        Map<String, Object> payload = context.getPayload();

        log.debug("AmlFraudOperatorEvaluator: op={}", op);

        try {
            return switch (op) {
                case STRUCTURING_DETECTION -> evaluateStructuring(request, payload, condition);
                case LAYERING_PATTERN -> evaluateLayering(request, payload, condition);
                default -> false;
            };
        } catch (Exception e) {
            log.error("Erro ao avaliar operador AML/Fraude {}: {}", op, e.getMessage());
            return false;
        }
    }

    private boolean evaluateStructuring(TransactionRequest request, Map<String, Object> payload, RuleCondition condition) {
        // Detectar estruturação (smurfing) - múltiplas transações abaixo do limite de reporte
        BigDecimal amount = request != null ? request.getTransactionAmount() : null;
        if (amount == null) {
            return false;
        }

        // Verificar se valor está na faixa de estruturação (logo abaixo do CTR threshold)
        BigDecimal lowerBound = CTR_THRESHOLD.subtract(STRUCTURING_MARGIN);
        boolean inStructuringRange = amount.compareTo(lowerBound) >= 0 &&
                                     amount.compareTo(CTR_THRESHOLD) < 0;

        // Verificar contagem de transações similares
        Integer similarTxnCount = getIntFromPayload(payload, "similarAmountTxnCount");
        Integer txnCountBelow10k = getIntFromPayload(payload, "txnCountBelow10k");

        int threshold = parseIntSafe(condition.getValueSingle(), 3);

        boolean hasMultipleSimilar = (similarTxnCount != null && similarTxnCount >= threshold) ||
                                     (txnCountBelow10k != null && txnCountBelow10k >= threshold);

        // Verificar padrão de round numbers
        Boolean isRoundNumber = isRoundAmount(amount);

        boolean isStructuring = inStructuringRange && (hasMultipleSimilar || isRoundNumber);

        log.debug("STRUCTURING_DETECTION: amount={}, inRange={}, similarCount={}, round={}, isStructuring={}",
            amount, inStructuringRange, similarTxnCount, isRoundNumber, isStructuring);

        return isStructuring;
    }

    private boolean evaluateLayering(TransactionRequest request, Map<String, Object> payload, RuleCondition condition) {
        // Detectar padrão de camadas (layering) - movimento rápido de fundos entre contas

        // Indicadores de layering
        Integer hopCount = getIntFromPayload(payload, "transactionHopCount");
        Integer intermediaryCount = getIntFromPayload(payload, "intermediaryAccountCount");
        Boolean rapidMovement = getBooleanFromPayload(payload, "rapidFundMovement");
        Boolean crossBorder = getBooleanFromPayload(payload, "crossBorderTransfer");
        Integer timeToNextTxn = getIntFromPayload(payload, "minutesToNextTransaction");

        int hopThreshold = parseIntSafe(condition.getValueSingle(), 3);
        int timeThreshold = parseIntSafe(condition.getValueMin(), 60); // 60 minutos

        // Verificar múltiplos hops
        boolean hasMultipleHops = (hopCount != null && hopCount >= hopThreshold) ||
                                  (intermediaryCount != null && intermediaryCount >= hopThreshold);

        // Verificar movimento rápido
        boolean isRapid = Boolean.TRUE.equals(rapidMovement) ||
                         (timeToNextTxn != null && timeToNextTxn < timeThreshold);

        // Verificar se é cross-border (aumenta suspeita)
        boolean isCrossBorder = Boolean.TRUE.equals(crossBorder);

        boolean isLayering = hasMultipleHops && (isRapid || isCrossBorder);

        log.debug("LAYERING_PATTERN: hops={}, rapid={}, crossBorder={}, isLayering={}",
            hopCount, isRapid, isCrossBorder, isLayering);

        return isLayering;
    }

    private boolean isRoundAmount(BigDecimal amount) {
        if (amount == null) return false;

        // Verificar se é múltiplo de 100, 500, 1000, etc.
        BigDecimal[] roundValues = {
            new BigDecimal("100"),
            new BigDecimal("500"),
            new BigDecimal("1000"),
            new BigDecimal("5000"),
            new BigDecimal("9000"),
            new BigDecimal("9500"),
            new BigDecimal("9900")
        };

        for (BigDecimal roundValue : roundValues) {
            if (amount.remainder(roundValue).compareTo(BigDecimal.ZERO) == 0) {
                return true;
            }
        }

        return false;
    }

    // Utility methods
    private Boolean getBooleanFromPayload(Map<String, Object> payload, String key) {
        if (payload == null) return null;
        Object value = payload.get(key);
        if (value instanceof Boolean) return (Boolean) value;
        if (value instanceof String) return Boolean.parseBoolean((String) value);
        return null;
    }

    private Integer getIntFromPayload(Map<String, Object> payload, String key) {
        if (payload == null) return null;
        Object value = payload.get(key);
        if (value instanceof Number) return ((Number) value).intValue();
        if (value instanceof String) {
            try {
                return Integer.parseInt((String) value);
            } catch (NumberFormatException e) {
                return null;
            }
        }
        return null;
    }

    private int parseIntSafe(String value, int defaultValue) {
        if (value == null || value.isEmpty()) return defaultValue;
        try {
            return Integer.parseInt(value);
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }

    @Override
    public String getCategory() {
        return "AML_FRAUD";
    }
}
