package com.rulex.service.complex.evaluator;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.RuleCondition.ConditionOperator;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.math.BigDecimal;
import java.util.Map;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Avaliador para operadores regulatórios IMPLEMENTADOS.
 * 
 * <p>NOTA: Operadores SCA_* que estavam como STUB foram movidos para StubOperatorEvaluator.
 * Apenas operadores com implementação real baseada em flags do payload ficam aqui.
 */
@Component
@Slf4j
public class RegulatoryOperatorEvaluator implements OperatorEvaluator {

    private static final BigDecimal SCA_LOW_VALUE_LIMIT = new BigDecimal("30");

    private static final Set<ConditionOperator> SUPPORTED = Set.of(
        ConditionOperator.SCA_EXEMPTION_TRA,
        ConditionOperator.SCA_EXEMPTION_LOW_VALUE,
        ConditionOperator.SCA_EXEMPTION_TRUSTED_BENEFICIARY,
        ConditionOperator.SCA_EXEMPTION_RECURRING,
        ConditionOperator.PSD3_COP_NAME_MATCH,
        ConditionOperator.DORA_INCIDENT_SEVERITY,
        ConditionOperator.GDPR_DATA_RETENTION_CHECK
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

        log.debug("RegulatoryOperatorEvaluator: op={}", op);

        try {
            return switch (op) {
                case SCA_EXEMPTION_TRA -> evaluateTraExemption(payload, condition);
                case SCA_EXEMPTION_LOW_VALUE -> evaluateLowValueExemption(request);
                case SCA_EXEMPTION_TRUSTED_BENEFICIARY -> evaluateTrustedBeneficiary(payload);
                case SCA_EXEMPTION_RECURRING -> evaluateRecurringExemption(payload);
                case PSD3_COP_NAME_MATCH -> evaluateCopNameMatch(payload);
                case DORA_INCIDENT_SEVERITY -> evaluateDoraIncidentSeverity(payload, condition);
                case GDPR_DATA_RETENTION_CHECK -> evaluateGdprRetention(payload, condition);
                default -> false;
            };
        } catch (Exception e) {
            log.error("Erro ao avaliar operador regulatório {}: {}", op, e.getMessage());
            return false;
        }
    }

    private boolean evaluateTraExemption(Map<String, Object> payload, RuleCondition condition) {
        Double riskScore = getDouble(payload, "riskScore");
        if (riskScore == null) return false;
        
        double threshold = parseDouble(condition.getValueSingle(), 0.5);
        boolean qualifies = riskScore < threshold;
        log.debug("SCA_EXEMPTION_TRA: riskScore={}, threshold={}, qualifies={}", riskScore, threshold, qualifies);
        return qualifies;
    }

    private boolean evaluateLowValueExemption(TransactionRequest request) {
        BigDecimal amount = request != null ? request.getTransactionAmount() : null;
        if (amount == null) return false;
        
        boolean qualifies = amount.compareTo(SCA_LOW_VALUE_LIMIT) <= 0;
        log.debug("SCA_EXEMPTION_LOW_VALUE: amount={}, limit={}, qualifies={}", amount, SCA_LOW_VALUE_LIMIT, qualifies);
        return qualifies;
    }

    private boolean evaluateTrustedBeneficiary(Map<String, Object> payload) {
        Boolean trusted = getBoolean(payload, "isTrustedBeneficiary");
        Boolean whitelisted = getBoolean(payload, "beneficiaryWhitelisted");
        
        boolean qualifies = Boolean.TRUE.equals(trusted) || Boolean.TRUE.equals(whitelisted);
        log.debug("SCA_EXEMPTION_TRUSTED_BENEFICIARY: trusted={}, whitelisted={}, qualifies={}", trusted, whitelisted, qualifies);
        return qualifies;
    }

    private boolean evaluateRecurringExemption(Map<String, Object> payload) {
        if (Boolean.TRUE.equals(getBoolean(payload, "isFirstRecurring"))) {
            log.debug("SCA_EXEMPTION_RECURRING: first recurring requires SCA");
            return false;
        }
        
        Boolean recurring = getBoolean(payload, "isRecurring");
        Boolean subscription = getBoolean(payload, "isSubscription");
        boolean qualifies = Boolean.TRUE.equals(recurring) || Boolean.TRUE.equals(subscription);
        log.debug("SCA_EXEMPTION_RECURRING: recurring={}, subscription={}, qualifies={}", recurring, subscription, qualifies);
        return qualifies;
    }

    private boolean evaluateCopNameMatch(Map<String, Object> payload) {
        Integer matchScore = getInt(payload, "copMatchScore");
        if (matchScore != null) {
            boolean matches = matchScore >= 90;
            log.debug("PSD3_COP_NAME_MATCH: score={}, matches={}", matchScore, matches);
            return matches;
        }
        
        String payeeName = getString(payload, "payeeName");
        String holderName = getString(payload, "accountHolderName");
        if (payeeName == null || holderName == null) return false;
        
        boolean matches = payeeName.equalsIgnoreCase(holderName);
        log.debug("PSD3_COP_NAME_MATCH: payee={}, holder={}, matches={}", payeeName, holderName, matches);
        return matches;
    }

    private boolean evaluateDoraIncidentSeverity(Map<String, Object> payload, RuleCondition condition) {
        String severity = getString(payload, "incidentSeverity");
        String required = condition.getValueSingle() != null ? condition.getValueSingle() : "HIGH";
        
        if (severity != null) {
            boolean matches = severity.equalsIgnoreCase(required);
            log.debug("DORA_INCIDENT_SEVERITY: severity={}, required={}, matches={}", severity, required, matches);
            return matches;
        }
        
        Integer level = getInt(payload, "incidentSeverityLevel");
        if (level != null) {
            int requiredLevel = parseInt(required, 3);
            boolean matches = level >= requiredLevel;
            log.debug("DORA_INCIDENT_SEVERITY: level={}, required={}, matches={}", level, requiredLevel, matches);
            return matches;
        }
        
        return false;
    }

    private boolean evaluateGdprRetention(Map<String, Object> payload, RuleCondition condition) {
        Integer dataAgeDays = getInt(payload, "dataAgeDays");
        if (dataAgeDays == null) return false;
        
        Integer configuredRetention = getInt(payload, "retentionPeriodDays");
        int maxRetention = configuredRetention != null ? configuredRetention : parseInt(condition.getValueSingle(), 2555);
        
        boolean withinRetention = dataAgeDays <= maxRetention;
        log.debug("GDPR_DATA_RETENTION: ageDays={}, maxRetention={}, withinRetention={}", dataAgeDays, maxRetention, withinRetention);
        return withinRetention;
    }

    // Utility methods
    private Boolean getBoolean(Map<String, Object> p, String k) {
        if (p == null) return null;
        Object v = p.get(k);
        if (v instanceof Boolean) return (Boolean) v;
        if (v instanceof String) return Boolean.parseBoolean((String) v);
        return null;
    }

    private Integer getInt(Map<String, Object> p, String k) {
        if (p == null) return null;
        Object v = p.get(k);
        if (v instanceof Number) return ((Number) v).intValue();
        if (v instanceof String) { try { return Integer.parseInt((String) v); } catch (Exception e) { return null; } }
        return null;
    }

    private Double getDouble(Map<String, Object> p, String k) {
        if (p == null) return null;
        Object v = p.get(k);
        if (v instanceof Number) return ((Number) v).doubleValue();
        if (v instanceof String) { try { return Double.parseDouble((String) v); } catch (Exception e) { return null; } }
        return null;
    }

    private String getString(Map<String, Object> p, String k) {
        if (p == null) return null;
        Object v = p.get(k);
        return v != null ? String.valueOf(v) : null;
    }

    private int parseInt(String v, int def) {
        if (v == null || v.isEmpty()) return def;
        try { return Integer.parseInt(v); } catch (Exception e) { return def; }
    }

    private double parseDouble(String v, double def) {
        if (v == null || v.isEmpty()) return def;
        try { return Double.parseDouble(v); } catch (Exception e) { return def; }
    }

    @Override
    public String getCategory() {
        return "REGULATORY";
    }
}
