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
 * Avaliador para operadores regulatórios.
 *
 * <p>Operadores suportados:
 * <ul>
 *   <li>SCA_EXEMPTION_* - isenções de Strong Customer Authentication</li>
 *   <li>PSD3_COP_NAME_MATCH - verificação de nome CoP PSD3</li>
 *   <li>DORA_INCIDENT_SEVERITY - severidade de incidente DORA</li>
 *   <li>GDPR_DATA_RETENTION_CHECK - verificação de retenção GDPR</li>
 * </ul>
 */
@Component
@Slf4j
public class RegulatoryOperatorEvaluator implements OperatorEvaluator {

    // Limites SCA conforme PSD2/PSD3
    private static final BigDecimal SCA_LOW_VALUE_LIMIT = new BigDecimal("30"); // €30
    private static final BigDecimal SCA_CONTACTLESS_LIMIT = new BigDecimal("50"); // €50
    private static final BigDecimal SCA_CUMULATIVE_LIMIT = new BigDecimal("150"); // €150

    private static final Set<ConditionOperator> SUPPORTED = Set.of(
        ConditionOperator.SCA_EXEMPTION_TRA,
        ConditionOperator.SCA_EXEMPTION_LOW_VALUE,
        ConditionOperator.SCA_EXEMPTION_TRUSTED_BENEFICIARY,
        ConditionOperator.SCA_EXEMPTION_RECURRING,
        ConditionOperator.PSD3_COP_NAME_MATCH,
        ConditionOperator.DORA_INCIDENT_SEVERITY,
        ConditionOperator.GDPR_DATA_RETENTION_CHECK,
        ConditionOperator.SCA_LOW_VALUE_EXEMPTION,
        ConditionOperator.SCA_CONTACTLESS_EXEMPTION,
        ConditionOperator.SCA_TRA_EXEMPTION,
        ConditionOperator.SCA_TRUSTED_BENEFICIARY,
        ConditionOperator.SCA_RECURRING_TRANSACTION,
        ConditionOperator.SCA_CHALLENGE_MANDATORY,
        ConditionOperator.SCA_CORPORATE_PAYMENT,
        ConditionOperator.SCA_DYNAMIC_3DS_ROUTING,
        ConditionOperator.SCA_FRAUD_RATE_MONITORING,
        ConditionOperator.SCA_LIABILITY_SHIFT,
        ConditionOperator.SCA_MERCHANT_INITIATED,
        ConditionOperator.SCA_SECURE_CORPORATE_PROTOCOL
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
                case SCA_EXEMPTION_TRA, SCA_TRA_EXEMPTION -> evaluateTraExemption(request, payload, condition);
                case SCA_EXEMPTION_LOW_VALUE, SCA_LOW_VALUE_EXEMPTION -> evaluateLowValueExemption(request, payload);
                case SCA_EXEMPTION_TRUSTED_BENEFICIARY, SCA_TRUSTED_BENEFICIARY -> evaluateTrustedBeneficiary(request, payload);
                case SCA_EXEMPTION_RECURRING, SCA_RECURRING_TRANSACTION -> evaluateRecurringExemption(request, payload);
                case SCA_CONTACTLESS_EXEMPTION -> evaluateContactlessExemption(request, payload);
                case PSD3_COP_NAME_MATCH -> evaluateCopNameMatch(request, payload);
                case DORA_INCIDENT_SEVERITY -> evaluateDoraIncidentSeverity(request, payload, condition);
                case GDPR_DATA_RETENTION_CHECK -> evaluateGdprRetention(request, payload, condition);
                case SCA_CHALLENGE_MANDATORY -> evaluateChallengeMandatory(request, payload);
                case SCA_CORPORATE_PAYMENT -> evaluateCorporatePayment(request, payload);
                case SCA_DYNAMIC_3DS_ROUTING -> evaluateDynamic3dsRouting(request, payload, condition);
                case SCA_FRAUD_RATE_MONITORING -> evaluateFraudRateMonitoring(request, payload, condition);
                case SCA_LIABILITY_SHIFT -> evaluateLiabilityShift(request, payload);
                case SCA_MERCHANT_INITIATED -> evaluateMerchantInitiated(request, payload);
                case SCA_SECURE_CORPORATE_PROTOCOL -> evaluateSecureCorporateProtocol(request, payload);
                default -> false;
            };
        } catch (Exception e) {
            log.error("Erro ao avaliar operador regulatório {}: {}", op, e.getMessage());
            return false;
        }
    }

    private boolean evaluateTraExemption(TransactionRequest request, Map<String, Object> payload, RuleCondition condition) {
        // Transaction Risk Analysis exemption
        // Verifica se a transação se qualifica para isenção TRA baseado no risco
        Double riskScore = getDoubleFromPayload(payload, "riskScore");
        BigDecimal amount = request != null ? request.getTransactionAmount() : null;

        if (riskScore == null || amount == null) {
            return false;
        }

        // Limites TRA conforme PSD2:
        // - €500 se fraud rate < 0.13%
        // - €250 se fraud rate < 0.06%
        // - €100 se fraud rate < 0.01%
        double threshold = parseDoubleSafe(condition.getValueSingle(), 0.5);
        boolean qualifies = riskScore < threshold;

        log.debug("SCA_TRA_EXEMPTION: riskScore={}, threshold={}, qualifies={}", riskScore, threshold, qualifies);
        return qualifies;
    }

    private boolean evaluateLowValueExemption(TransactionRequest request, Map<String, Object> payload) {
        // Isenção para transações de baixo valor (≤€30)
        BigDecimal amount = request != null ? request.getTransactionAmount() : null;
        if (amount == null) {
            return false;
        }

        boolean qualifies = amount.compareTo(SCA_LOW_VALUE_LIMIT) <= 0;
        log.debug("SCA_LOW_VALUE_EXEMPTION: amount={}, limit={}, qualifies={}",
            amount, SCA_LOW_VALUE_LIMIT, qualifies);
        return qualifies;
    }

    private boolean evaluateContactlessExemption(TransactionRequest request, Map<String, Object> payload) {
        // Isenção para transações contactless (≤€50)
        BigDecimal amount = request != null ? request.getTransactionAmount() : null;
        Boolean isContactless = getBooleanFromPayload(payload, "isContactless");

        if (amount == null || !Boolean.TRUE.equals(isContactless)) {
            return false;
        }

        // Verificar limite cumulativo também
        BigDecimal cumulativeAmount = getBigDecimalFromPayload(payload, "cumulativeContactlessAmount");
        if (cumulativeAmount != null && cumulativeAmount.compareTo(SCA_CUMULATIVE_LIMIT) > 0) {
            log.debug("SCA_CONTACTLESS_EXEMPTION: cumulative limit exceeded");
            return false;
        }

        boolean qualifies = amount.compareTo(SCA_CONTACTLESS_LIMIT) <= 0;
        log.debug("SCA_CONTACTLESS_EXEMPTION: amount={}, limit={}, qualifies={}",
            amount, SCA_CONTACTLESS_LIMIT, qualifies);
        return qualifies;
    }

    private boolean evaluateTrustedBeneficiary(TransactionRequest request, Map<String, Object> payload) {
        // Isenção para beneficiários de confiança
        Boolean isTrustedBeneficiary = getBooleanFromPayload(payload, "isTrustedBeneficiary");
        Boolean isWhitelisted = getBooleanFromPayload(payload, "beneficiaryWhitelisted");

        boolean qualifies = Boolean.TRUE.equals(isTrustedBeneficiary) || Boolean.TRUE.equals(isWhitelisted);
        log.debug("SCA_TRUSTED_BENEFICIARY: trusted={}, whitelisted={}, qualifies={}",
            isTrustedBeneficiary, isWhitelisted, qualifies);
        return qualifies;
    }

    private boolean evaluateRecurringExemption(TransactionRequest request, Map<String, Object> payload) {
        // Isenção para transações recorrentes
        Boolean isRecurring = getBooleanFromPayload(payload, "isRecurring");
        Boolean isSubscription = getBooleanFromPayload(payload, "isSubscription");
        Boolean firstRecurring = getBooleanFromPayload(payload, "isFirstRecurring");

        // Primeira transação recorrente requer SCA
        if (Boolean.TRUE.equals(firstRecurring)) {
            log.debug("SCA_RECURRING_EXEMPTION: first recurring requires SCA");
            return false;
        }

        boolean qualifies = Boolean.TRUE.equals(isRecurring) || Boolean.TRUE.equals(isSubscription);
        log.debug("SCA_RECURRING_EXEMPTION: recurring={}, subscription={}, qualifies={}",
            isRecurring, isSubscription, qualifies);
        return qualifies;
    }

    private boolean evaluateCopNameMatch(TransactionRequest request, Map<String, Object> payload) {
        // Confirmation of Payee - verificação de nome
        String payeeName = getStringFromPayload(payload, "payeeName");
        String accountHolderName = getStringFromPayload(payload, "accountHolderName");
        Integer matchScore = getIntFromPayload(payload, "copMatchScore");

        if (matchScore != null) {
            // Score >= 90 = match exato ou muito próximo
            boolean matches = matchScore >= 90;
            log.debug("PSD3_COP_NAME_MATCH: score={}, matches={}", matchScore, matches);
            return matches;
        }

        if (payeeName == null || accountHolderName == null) {
            return false;
        }

        // Comparação simples de nomes
        boolean matches = payeeName.equalsIgnoreCase(accountHolderName);
        log.debug("PSD3_COP_NAME_MATCH: payee={}, holder={}, matches={}",
            payeeName, accountHolderName, matches);
        return matches;
    }

    private boolean evaluateDoraIncidentSeverity(TransactionRequest request, Map<String, Object> payload, RuleCondition condition) {
        // DORA - Digital Operational Resilience Act
        String severity = getStringFromPayload(payload, "incidentSeverity");
        Integer severityLevel = getIntFromPayload(payload, "incidentSeverityLevel");

        String requiredSeverity = condition.getValueSingle();
        if (requiredSeverity == null) requiredSeverity = "HIGH";

        if (severity != null) {
            boolean matches = severity.equalsIgnoreCase(requiredSeverity);
            log.debug("DORA_INCIDENT_SEVERITY: severity={}, required={}, matches={}",
                severity, requiredSeverity, matches);
            return matches;
        }

        if (severityLevel != null) {
            int requiredLevel = parseIntSafe(requiredSeverity, 3);
            boolean matches = severityLevel >= requiredLevel;
            log.debug("DORA_INCIDENT_SEVERITY: level={}, required={}, matches={}",
                severityLevel, requiredLevel, matches);
            return matches;
        }

        return false;
    }

    private boolean evaluateGdprRetention(TransactionRequest request, Map<String, Object> payload, RuleCondition condition) {
        // Verificação de período de retenção GDPR
        Integer dataAgeDays = getIntFromPayload(payload, "dataAgeDays");
        Integer retentionPeriodDays = getIntFromPayload(payload, "retentionPeriodDays");

        if (dataAgeDays == null) {
            return false;
        }

        int maxRetention = retentionPeriodDays != null ? retentionPeriodDays :
                          parseIntSafe(condition.getValueSingle(), 365 * 7); // 7 anos padrão

        boolean withinRetention = dataAgeDays <= maxRetention;
        log.debug("GDPR_DATA_RETENTION: ageDays={}, maxRetention={}, withinRetention={}",
            dataAgeDays, maxRetention, withinRetention);
        return withinRetention;
    }

    private boolean evaluateChallengeMandatory(TransactionRequest request, Map<String, Object> payload) {
        // Verifica se challenge é obrigatório
        Boolean challengeRequired = getBooleanFromPayload(payload, "challengeRequired");
        Boolean highRisk = getBooleanFromPayload(payload, "highRisk");
        Boolean suspiciousActivity = getBooleanFromPayload(payload, "suspiciousActivity");

        boolean mandatory = Boolean.TRUE.equals(challengeRequired) ||
                           Boolean.TRUE.equals(highRisk) ||
                           Boolean.TRUE.equals(suspiciousActivity);
        log.debug("SCA_CHALLENGE_MANDATORY: required={}, highRisk={}, suspicious={}",
            challengeRequired, highRisk, suspiciousActivity);
        return mandatory;
    }

    private boolean evaluateCorporatePayment(TransactionRequest request, Map<String, Object> payload) {
        // Pagamento corporativo (pode ter isenções diferentes)
        Boolean isCorporate = getBooleanFromPayload(payload, "isCorporate");
        Boolean isB2B = getBooleanFromPayload(payload, "isB2B");
        String accountType = getStringFromPayload(payload, "accountType");

        boolean corporate = Boolean.TRUE.equals(isCorporate) ||
                           Boolean.TRUE.equals(isB2B) ||
                           "CORPORATE".equalsIgnoreCase(accountType);
        log.debug("SCA_CORPORATE_PAYMENT: corporate={}, b2b={}, type={}",
            isCorporate, isB2B, accountType);
        return corporate;
    }

    private boolean evaluateDynamic3dsRouting(TransactionRequest request, Map<String, Object> payload, RuleCondition condition) {
        // Roteamento dinâmico de 3DS baseado em risco
        Double riskScore = getDoubleFromPayload(payload, "riskScore");
        String recommendedFlow = getStringFromPayload(payload, "recommended3dsFlow");

        if (recommendedFlow != null) {
            String expectedFlow = condition.getValueSingle();
            if (expectedFlow == null) expectedFlow = "FRICTIONLESS";

            boolean matches = recommendedFlow.equalsIgnoreCase(expectedFlow);
            log.debug("SCA_DYNAMIC_3DS_ROUTING: recommended={}, expected={}, matches={}",
                recommendedFlow, expectedFlow, matches);
            return matches;
        }

        if (riskScore != null) {
            double threshold = parseDoubleSafe(condition.getValueSingle(), 0.3);
            boolean lowRisk = riskScore < threshold;
            log.debug("SCA_DYNAMIC_3DS_ROUTING: riskScore={}, threshold={}, lowRisk={}",
                riskScore, threshold, lowRisk);
            return lowRisk;
        }

        return false;
    }

    private boolean evaluateFraudRateMonitoring(TransactionRequest request, Map<String, Object> payload, RuleCondition condition) {
        // Monitoramento de taxa de fraude para TRA
        Double fraudRate = getDoubleFromPayload(payload, "merchantFraudRate");
        if (fraudRate == null) {
            fraudRate = getDoubleFromPayload(payload, "acquirerFraudRate");
        }

        if (fraudRate == null) {
            return false;
        }

        double threshold = parseDoubleSafe(condition.getValueSingle(), 0.13); // 0.13% = limite TRA €500
        boolean withinLimit = fraudRate <= threshold;
        log.debug("SCA_FRAUD_RATE_MONITORING: rate={}%, threshold={}%, withinLimit={}",
            fraudRate * 100, threshold * 100, withinLimit);
        return withinLimit;
    }

    private boolean evaluateLiabilityShift(TransactionRequest request, Map<String, Object> payload) {
        // Verificar se há liability shift
        Boolean liabilityShift = getBooleanFromPayload(payload, "liabilityShift");
        String liabilityHolder = getStringFromPayload(payload, "liabilityHolder");

        boolean hasShift = Boolean.TRUE.equals(liabilityShift) ||
                          "ISSUER".equalsIgnoreCase(liabilityHolder);
        log.debug("SCA_LIABILITY_SHIFT: shift={}, holder={}", liabilityShift, liabilityHolder);
        return hasShift;
    }

    private boolean evaluateMerchantInitiated(TransactionRequest request, Map<String, Object> payload) {
        // Transação iniciada pelo merchant (MIT)
        Boolean isMit = getBooleanFromPayload(payload, "isMerchantInitiated");
        Boolean isMoto = getBooleanFromPayload(payload, "isMoto");
        String initiator = getStringFromPayload(payload, "transactionInitiator");

        boolean merchantInitiated = Boolean.TRUE.equals(isMit) ||
                                   Boolean.TRUE.equals(isMoto) ||
                                   "MERCHANT".equalsIgnoreCase(initiator);
        log.debug("SCA_MERCHANT_INITIATED: mit={}, moto={}, initiator={}",
            isMit, isMoto, initiator);
        return merchantInitiated;
    }

    private boolean evaluateSecureCorporateProtocol(TransactionRequest request, Map<String, Object> payload) {
        // Protocolo corporativo seguro
        Boolean secureProtocol = getBooleanFromPayload(payload, "secureCorporateProtocol");
        String protocol = getStringFromPayload(payload, "corporateProtocol");

        boolean isSecure = Boolean.TRUE.equals(secureProtocol) ||
                          "EBICS".equalsIgnoreCase(protocol) ||
                          "SWIFT".equalsIgnoreCase(protocol);
        log.debug("SCA_SECURE_CORPORATE_PROTOCOL: secure={}, protocol={}", secureProtocol, protocol);
        return isSecure;
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

    private Double getDoubleFromPayload(Map<String, Object> payload, String key) {
        if (payload == null) return null;
        Object value = payload.get(key);
        if (value instanceof Number) return ((Number) value).doubleValue();
        if (value instanceof String) {
            try {
                return Double.parseDouble((String) value);
            } catch (NumberFormatException e) {
                return null;
            }
        }
        return null;
    }

    private String getStringFromPayload(Map<String, Object> payload, String key) {
        if (payload == null) return null;
        Object value = payload.get(key);
        return value != null ? String.valueOf(value) : null;
    }

    private BigDecimal getBigDecimalFromPayload(Map<String, Object> payload, String key) {
        if (payload == null) return null;
        Object value = payload.get(key);
        if (value instanceof BigDecimal) return (BigDecimal) value;
        if (value instanceof Number) return BigDecimal.valueOf(((Number) value).doubleValue());
        if (value instanceof String) {
            try {
                return new BigDecimal((String) value);
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

    private double parseDoubleSafe(String value, double defaultValue) {
        if (value == null || value.isEmpty()) return defaultValue;
        try {
            return Double.parseDouble(value);
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }

    @Override
    public String getCategory() {
        return "REGULATORY";
    }
}
