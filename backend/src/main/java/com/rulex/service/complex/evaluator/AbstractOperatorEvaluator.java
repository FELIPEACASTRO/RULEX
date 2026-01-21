package com.rulex.service.complex.evaluator;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.exception.RuleEvaluationException;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import com.rulex.service.complex.evaluator.util.EvaluatorUtils;
import java.math.BigDecimal;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;

/**
 * Classe base abstrata para todos os OperatorEvaluators.
 * 
 * <p>GAP-3 FIX: Padroniza tratamento de exceções.
 * <p>GAP-7 FIX: Elimina duplicação de código.
 * 
 * <p>Fornece:
 * <ul>
 *   <li>Template method para avaliação com tratamento de exceções</li>
 *   <li>Métodos utilitários para extração de valores</li>
 *   <li>Logging padronizado</li>
 *   <li>Métricas de avaliação</li>
 * </ul>
 * 
 * @version 1.0.0
 * @since 1.3.1
 */
@Slf4j
public abstract class AbstractOperatorEvaluator implements OperatorEvaluator {

    /**
     * Retorna o nome da categoria do evaluator para logging.
     */
    @Override
    public abstract String getCategory();

    /**
     * Retorna os operadores suportados por este evaluator.
     */
    @Override
    public abstract Set<ConditionOperator> getSupportedOperators();

    /**
     * Template method que implementa o padrão de avaliação com tratamento de exceções.
     * 
     * <p>GAP-3 FIX: Substitui os catch (Exception e) { return false; } por tratamento adequado.
     */
    @Override
    public final boolean evaluate(RuleCondition condition, EvaluationContext context) {
        ConditionOperator operator = condition.getOperator();
        String fieldName = condition.getFieldName();
        
        // Validação básica
        if (!getSupportedOperators().contains(operator)) {
            log.warn("Operador {} não suportado por {}", operator, getClass().getSimpleName());
            return false;
        }

        long startTime = System.nanoTime();
        try {
            // Delega para implementação específica
            boolean result = doEvaluate(condition, context);
            
            // Log de debug
            if (log.isDebugEnabled()) {
                long durationMicros = (System.nanoTime() - startTime) / 1000;
                log.debug("[{}] Operador {} em campo '{}': resultado={}, tempo={}µs",
                    getCategory(), operator, fieldName, result, durationMicros);
            }
            
            return result;
            
        } catch (RuleEvaluationException e) {
            // Exceções de avaliação são propagadas
            log.error("[{}] Erro de avaliação no operador {}: {}", 
                getCategory(), operator, e.getMessage());
            throw e;
            
        } catch (IllegalArgumentException e) {
            // Argumentos inválidos - log e retorna false
            log.warn("[{}] Argumento inválido no operador {} para campo '{}': {}",
                getCategory(), operator, fieldName, e.getMessage());
            return false;
            
        } catch (NullPointerException e) {
            // NPE geralmente indica campo ausente - log e retorna false
            log.debug("[{}] Valor nulo ao avaliar operador {} no campo '{}': {}",
                getCategory(), operator, fieldName, e.getMessage());
            return false;
            
        } catch (Exception e) {
            // Outras exceções - log detalhado e retorna false
            // GAP-3 FIX: Não engole mais a exceção silenciosamente
            log.error("[{}] Erro inesperado ao avaliar operador {} no campo '{}': {} - {}",
                getCategory(), operator, fieldName, e.getClass().getSimpleName(), e.getMessage());
            log.debug("[{}] Stack trace para operador {}", getCategory(), operator, e);
            return false;
        }
    }

    /**
     * Método abstrato que deve ser implementado pelas subclasses.
     * 
     * @param condition condição a avaliar
     * @param context contexto de avaliação
     * @return true se condição é satisfeita
     * @throws RuleEvaluationException se avaliação falhar de forma crítica
     */
    protected abstract boolean doEvaluate(RuleCondition condition, EvaluationContext context);

    // ========== MÉTODOS UTILITÁRIOS PARA SUBCLASSES ==========

    /**
     * Obtém valor do campo do contexto.
     */
    protected Optional<Object> getFieldValue(String fieldName, EvaluationContext context) {
        return EvaluatorUtils.getFieldValue(fieldName, context);
    }

    /**
     * Obtém valor do campo como String.
     */
    protected String getStringValue(String fieldName, EvaluationContext context) {
        return EvaluatorUtils.getStringValue(fieldName, context, null);
    }

    /**
     * Obtém valor do campo como Long.
     */
    protected long getLongValue(String fieldName, EvaluationContext context, long defaultValue) {
        return EvaluatorUtils.getLongValue(fieldName, context, defaultValue);
    }

    /**
     * Obtém valor do campo como Integer.
     */
    protected int getIntValue(String fieldName, EvaluationContext context, int defaultValue) {
        return EvaluatorUtils.getIntValue(fieldName, context, defaultValue);
    }

    /**
     * Obtém valor do campo como Double.
     */
    protected double getDoubleValue(String fieldName, EvaluationContext context, double defaultValue) {
        return EvaluatorUtils.getDoubleValue(fieldName, context, defaultValue);
    }

    /**
     * Obtém valor do campo como BigDecimal.
     */
    protected BigDecimal getBigDecimalValue(String fieldName, EvaluationContext context) {
        return EvaluatorUtils.getBigDecimalValue(fieldName, context, BigDecimal.ZERO);
    }

    /**
     * Obtém valor do campo como Boolean.
     */
    protected boolean getBooleanValue(String fieldName, EvaluationContext context) {
        return EvaluatorUtils.getBooleanValue(fieldName, context, false);
    }

    /**
     * Obtém payload do contexto.
     */
    protected Map<String, Object> getPayload(EvaluationContext context) {
        return context.getPayload() != null ? context.getPayload() : Map.of();
    }

    /**
     * Obtém TransactionRequest do contexto.
     */
    protected TransactionRequest getRequest(EvaluationContext context) {
        return context.getTransactionRequest();
    }

    /**
     * Parse seguro de Long.
     */
    protected long parseLongSafe(String value, long defaultValue) {
        return EvaluatorUtils.parseLongSafe(value, defaultValue);
    }

    /**
     * Parse seguro de Integer.
     */
    protected int parseIntSafe(String value, int defaultValue) {
        return EvaluatorUtils.parseIntSafe(value, defaultValue);
    }

    /**
     * Parse seguro de Double.
     */
    protected double parseDoubleSafe(String value, double defaultValue) {
        return EvaluatorUtils.parseDoubleSafe(value, defaultValue);
    }

    /**
     * Parse seguro de BigDecimal.
     */
    protected BigDecimal parseBigDecimalSafe(String value, BigDecimal defaultValue) {
        return EvaluatorUtils.parseBigDecimalSafe(value, defaultValue);
    }

    /**
     * Converte Object para Boolean.
     */
    protected boolean toBoolean(Object value) {
        return EvaluatorUtils.toBoolean(value);
    }

    /**
     * Converte Object para Long.
     */
    protected long toLong(Object value, long defaultValue) {
        return EvaluatorUtils.toLong(value, defaultValue);
    }

    /**
     * Converte Object para Double.
     */
    protected double toDouble(Object value, double defaultValue) {
        return EvaluatorUtils.toDouble(value, defaultValue);
    }

    /**
     * Verifica se valor é nulo ou vazio.
     */
    protected boolean isNullOrEmpty(Object value) {
        return EvaluatorUtils.isNullOrEmpty(value);
    }

    /**
     * Verifica se valor não é nulo nem vazio.
     */
    protected boolean isNotNullOrEmpty(Object value) {
        return EvaluatorUtils.isNotNullOrEmpty(value);
    }

    /**
     * Compara dois valores numéricos.
     */
    protected int compareNumeric(Object a, Object b) {
        return EvaluatorUtils.compareNumeric(a, b);
    }

    /**
     * Verifica se valor está entre min e max.
     */
    protected boolean isBetween(Object value, Object min, Object max) {
        return EvaluatorUtils.isBetween(value, min, max);
    }

    /**
     * Extrai threshold e window de valueSingle.
     */
    protected double[] parseThresholdAndWindow(String valueSingle, double defaultThreshold, double defaultWindow) {
        return EvaluatorUtils.parseThresholdAndWindow(valueSingle, defaultThreshold, defaultWindow);
    }

    /**
     * Log de avaliação para debug.
     */
    protected void logEvaluation(String operatorName, String fieldName, Object fieldValue, Object conditionValue, boolean result) {
        EvaluatorUtils.logEvaluation(operatorName, fieldName, fieldValue, conditionValue, result);
    }
}
