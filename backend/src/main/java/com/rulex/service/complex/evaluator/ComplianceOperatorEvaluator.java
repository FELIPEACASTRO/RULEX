package com.rulex.service.complex.evaluator;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.RuleCondition.ConditionOperator;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.math.BigDecimal;
import java.util.Map;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Avaliador para operadores de compliance e regulatórios.
 * Inclui verificações AML, KYC, sanções, PEP, etc.
 */
@Component
@Slf4j
public class ComplianceOperatorEvaluator implements OperatorEvaluator {

    private static final Set<ConditionOperator> SUPPORTED = Set.of(
        // Verificações de identidade
        ConditionOperator.ADDRESS_VERIFICATION,
        ConditionOperator.CPF_SSN_VALIDATION,
        ConditionOperator.CREDITOR_NAME_VALIDATION,
        ConditionOperator.DOCUMENT_FORGERY_DETECTION,
        ConditionOperator.ECBSV_SSN_VALIDATION,
        ConditionOperator.EIDAS_ASSURANCE_LEVEL,
        ConditionOperator.IDENTITY_VELOCITY,
        ConditionOperator.STRUCTURED_ADDRESS_CHECK,
        
        // Listas de sanções
        ConditionOperator.ADVERSE_MEDIA_CHECK,
        ConditionOperator.CONSORTIUM_NEGATIVE_FILE_CHECK,
        ConditionOperator.OFAC_LIST_CHECK,
        ConditionOperator.PEP_LIST_CHECK,
        ConditionOperator.SANCTIONS_COUNTRY_CHECK,
        
        // AML
        ConditionOperator.CASH_INTENSIVE_RATIO,
        ConditionOperator.CORRESPONDENT_ANOMALY,
        ConditionOperator.HIGH_RISK_CORRIDOR_CHECK,
        ConditionOperator.HIGH_RISK_JURISDICTION,
        ConditionOperator.NESTED_CORRESPONDENT_CHECK,
        ConditionOperator.PURPOSE_CODE_MISMATCH,
        ConditionOperator.REMITTANCE_INFO_ANALYSIS,
        ConditionOperator.SHELL_BANK_INDICATOR,
        ConditionOperator.SHELL_COMPANY_INDICATOR,
        ConditionOperator.TRADE_BASED_ML_INDICATOR,
        ConditionOperator.UETR_DUPLICATE_CHECK,
        
        // Synthetic Identity
        ConditionOperator.ALIAS_DETECTION,
        ConditionOperator.CREDIT_FILE_THIN,
        ConditionOperator.MULTI_LAYERED_SYNTHETIC_ID_CONTROLS,
        ConditionOperator.SYNTHETIC_FRAUD_SCORE,
        ConditionOperator.SYNTHETIC_IDENTITY_RING,
        ConditionOperator.SYNTHETIC_ID_LABEL_CORRECTION,
        
        // Scams
        ConditionOperator.APP_FRAUD_DETECTION,
        ConditionOperator.INVESTMENT_SCAM_PATTERN,
        ConditionOperator.ROMANCE_SCAM_INDICATOR
    );

    @Override
    public Set<ConditionOperator> getSupportedOperators() {
        return SUPPORTED;
    }

    @Override
    public boolean evaluate(RuleCondition condition, EvaluationContext context) {
        ConditionOperator op = condition.getOperator();
        log.debug("ComplianceOperatorEvaluator: op={}", op);

        return switch (op) {
            case ADDRESS_VERIFICATION -> evaluateAddressVerification(condition, context);
            case CPF_SSN_VALIDATION -> evaluateCpfSsnValidation(condition, context);
            case CREDITOR_NAME_VALIDATION -> evaluateCreditorNameValidation(condition, context);
            case DOCUMENT_FORGERY_DETECTION -> evaluateDocumentForgery(condition, context);
            case ECBSV_SSN_VALIDATION -> evaluateEcbsvSsnValidation(condition, context);
            case EIDAS_ASSURANCE_LEVEL -> evaluateEidasAssuranceLevel(condition, context);
            case IDENTITY_VELOCITY -> evaluateIdentityVelocity(condition, context);
            case STRUCTURED_ADDRESS_CHECK -> evaluateStructuredAddressCheck(condition, context);
            case ADVERSE_MEDIA_CHECK -> evaluateAdverseMediaCheck(condition, context);
            case CONSORTIUM_NEGATIVE_FILE_CHECK -> evaluateConsortiumNegativeFile(condition, context);
            case OFAC_LIST_CHECK -> evaluateOfacListCheck(condition, context);
            case PEP_LIST_CHECK -> evaluatePepListCheck(condition, context);
            case SANCTIONS_COUNTRY_CHECK -> evaluateSanctionsCountryCheck(condition, context);
            case CASH_INTENSIVE_RATIO -> evaluateCashIntensiveRatio(condition, context);
            case CORRESPONDENT_ANOMALY -> evaluateCorrespondentAnomaly(condition, context);
            case HIGH_RISK_CORRIDOR_CHECK -> evaluateHighRiskCorridor(condition, context);
            case HIGH_RISK_JURISDICTION -> evaluateHighRiskJurisdiction(condition, context);
            case NESTED_CORRESPONDENT_CHECK -> evaluateNestedCorrespondent(condition, context);
            case PURPOSE_CODE_MISMATCH -> evaluatePurposeCodeMismatch(condition, context);
            case REMITTANCE_INFO_ANALYSIS -> evaluateRemittanceInfoAnalysis(condition, context);
            case SHELL_BANK_INDICATOR -> evaluateShellBankIndicator(condition, context);
            case SHELL_COMPANY_INDICATOR -> evaluateShellCompanyIndicator(condition, context);
            case TRADE_BASED_ML_INDICATOR -> evaluateTradeBasedMl(condition, context);
            case UETR_DUPLICATE_CHECK -> evaluateUetrDuplicateCheck(condition, context);
            case ALIAS_DETECTION -> evaluateAliasDetection(condition, context);
            case CREDIT_FILE_THIN -> evaluateCreditFileThin(condition, context);
            case MULTI_LAYERED_SYNTHETIC_ID_CONTROLS -> evaluateMultiLayeredSyntheticId(condition, context);
            case SYNTHETIC_FRAUD_SCORE -> evaluateSyntheticFraudScore(condition, context);
            case SYNTHETIC_IDENTITY_RING -> evaluateSyntheticIdentityRing(condition, context);
            case SYNTHETIC_ID_LABEL_CORRECTION -> evaluateSyntheticIdLabelCorrection(condition, context);
            case APP_FRAUD_DETECTION -> evaluateAppFraudDetection(condition, context);
            case INVESTMENT_SCAM_PATTERN -> evaluateInvestmentScamPattern(condition, context);
            case ROMANCE_SCAM_INDICATOR -> evaluateRomanceScamIndicator(condition, context);
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

    private boolean evaluateBooleanPayload(EvaluationContext context, String... keys) {
        Object value = getPayloadValue(context, keys);
        if (value == null) return false;
        return Boolean.parseBoolean(String.valueOf(value));
    }

    private boolean evaluateAddressVerification(RuleCondition condition, EvaluationContext context) {
        Object verified = getPayloadValue(context, "addressVerified", "addressVerificationPassed");
        if (verified == null) return false;
        return !Boolean.parseBoolean(String.valueOf(verified)); // Retorna true se NÃO verificado
    }

    private boolean evaluateCpfSsnValidation(RuleCondition condition, EvaluationContext context) {
        Object valid = getPayloadValue(context, "cpfSsnValid", "ssnValid", "cpfValid");
        if (valid == null) return false;
        return !Boolean.parseBoolean(String.valueOf(valid));
    }

    private boolean evaluateCreditorNameValidation(RuleCondition condition, EvaluationContext context) {
        Object valid = getPayloadValue(context, "creditorNameValid", "nameValidationPassed");
        if (valid == null) return false;
        return !Boolean.parseBoolean(String.valueOf(valid));
    }

    private boolean evaluateDocumentForgery(RuleCondition condition, EvaluationContext context) {
        return evaluateBooleanPayload(context, "documentForgeryDetected", "forgeryDetected");
    }

    private boolean evaluateEcbsvSsnValidation(RuleCondition condition, EvaluationContext context) {
        Object valid = getPayloadValue(context, "ecbsvSsnValid", "ecbsvValidationPassed");
        if (valid == null) return false;
        return !Boolean.parseBoolean(String.valueOf(valid));
    }

    private boolean evaluateEidasAssuranceLevel(RuleCondition condition, EvaluationContext context) {
        Object level = getPayloadValue(context, "eidasAssuranceLevel", "assuranceLevel");
        if (level == null) return false;
        int currentLevel = parseIntSafe(String.valueOf(level), 0);
        int requiredLevel = parseIntSafe(condition.getValueSingle(), 2);
        return currentLevel < requiredLevel;
    }

    private boolean evaluateIdentityVelocity(RuleCondition condition, EvaluationContext context) {
        Object velocity = getPayloadValue(context, "identityVelocity", "idVelocity");
        if (velocity == null) return false;
        int v = parseIntSafe(String.valueOf(velocity), 0);
        int threshold = parseIntSafe(condition.getValueSingle(), 5);
        return v > threshold;
    }

    private boolean evaluateStructuredAddressCheck(RuleCondition condition, EvaluationContext context) {
        Object valid = getPayloadValue(context, "structuredAddressValid", "addressStructureValid");
        if (valid == null) return false;
        return !Boolean.parseBoolean(String.valueOf(valid));
    }

    private boolean evaluateAdverseMediaCheck(RuleCondition condition, EvaluationContext context) {
        return evaluateBooleanPayload(context, "adverseMediaFound", "negativeMediaHit");
    }

    private boolean evaluateConsortiumNegativeFile(RuleCondition condition, EvaluationContext context) {
        return evaluateBooleanPayload(context, "consortiumNegativeFileHit", "negativeFileMatch");
    }

    private boolean evaluateOfacListCheck(RuleCondition condition, EvaluationContext context) {
        return evaluateBooleanPayload(context, "ofacListHit", "ofacMatch");
    }

    private boolean evaluatePepListCheck(RuleCondition condition, EvaluationContext context) {
        return evaluateBooleanPayload(context, "pepListHit", "isPep", "pepMatch");
    }

    private boolean evaluateSanctionsCountryCheck(RuleCondition condition, EvaluationContext context) {
        return evaluateBooleanPayload(context, "sanctionsCountryHit", "sanctionedCountry");
    }

    private boolean evaluateCashIntensiveRatio(RuleCondition condition, EvaluationContext context) {
        Object ratio = getPayloadValue(context, "cashIntensiveRatio", "cashRatio");
        if (ratio == null) return false;
        BigDecimal r = parseBigDecimal(String.valueOf(ratio));
        BigDecimal threshold = parseBigDecimal(condition.getValueSingle(), new BigDecimal("70"));
        return r.compareTo(threshold) > 0;
    }

    private boolean evaluateCorrespondentAnomaly(RuleCondition condition, EvaluationContext context) {
        return evaluateBooleanPayload(context, "correspondentAnomaly", "correspondentBankAnomaly");
    }

    private boolean evaluateHighRiskCorridor(RuleCondition condition, EvaluationContext context) {
        return evaluateBooleanPayload(context, "highRiskCorridorHit", "riskCorridorMatch");
    }

    private boolean evaluateHighRiskJurisdiction(RuleCondition condition, EvaluationContext context) {
        return evaluateBooleanPayload(context, "highRiskJurisdiction", "jurisdictionRisk");
    }

    private boolean evaluateNestedCorrespondent(RuleCondition condition, EvaluationContext context) {
        return evaluateBooleanPayload(context, "nestedCorrespondentDetected", "nestedCorrespondent");
    }

    private boolean evaluatePurposeCodeMismatch(RuleCondition condition, EvaluationContext context) {
        return evaluateBooleanPayload(context, "purposeCodeMismatch", "purposeMismatch");
    }

    private boolean evaluateRemittanceInfoAnalysis(RuleCondition condition, EvaluationContext context) {
        return evaluateBooleanPayload(context, "remittanceInfoSuspicious", "suspiciousRemittance");
    }

    private boolean evaluateShellBankIndicator(RuleCondition condition, EvaluationContext context) {
        return evaluateBooleanPayload(context, "shellBankIndicator", "isShellBank");
    }

    private boolean evaluateShellCompanyIndicator(RuleCondition condition, EvaluationContext context) {
        return evaluateBooleanPayload(context, "shellCompanyIndicator", "isShellCompany");
    }

    private boolean evaluateTradeBasedMl(RuleCondition condition, EvaluationContext context) {
        return evaluateBooleanPayload(context, "tradeBasedMlIndicator", "tbmlDetected");
    }

    private boolean evaluateUetrDuplicateCheck(RuleCondition condition, EvaluationContext context) {
        return evaluateBooleanPayload(context, "uetrDuplicate", "duplicateUetr");
    }

    private boolean evaluateAliasDetection(RuleCondition condition, EvaluationContext context) {
        return evaluateBooleanPayload(context, "aliasDetected", "hasAlias");
    }

    private boolean evaluateCreditFileThin(RuleCondition condition, EvaluationContext context) {
        return evaluateBooleanPayload(context, "creditFileThin", "thinCreditFile");
    }

    private boolean evaluateMultiLayeredSyntheticId(RuleCondition condition, EvaluationContext context) {
        return evaluateBooleanPayload(context, "multiLayeredSyntheticIdDetected", "syntheticIdMultiLayer");
    }

    private boolean evaluateSyntheticFraudScore(RuleCondition condition, EvaluationContext context) {
        Object score = getPayloadValue(context, "syntheticFraudScore", "syntheticScore");
        if (score == null) return false;
        BigDecimal s = parseBigDecimal(String.valueOf(score));
        BigDecimal threshold = parseBigDecimal(condition.getValueSingle(), new BigDecimal("0.7"));
        return s.compareTo(threshold) > 0;
    }

    private boolean evaluateSyntheticIdentityRing(RuleCondition condition, EvaluationContext context) {
        return evaluateBooleanPayload(context, "syntheticIdentityRingDetected", "syntheticRing");
    }

    private boolean evaluateSyntheticIdLabelCorrection(RuleCondition condition, EvaluationContext context) {
        return evaluateBooleanPayload(context, "syntheticIdLabelCorrectionNeeded", "labelCorrectionRequired");
    }

    private boolean evaluateAppFraudDetection(RuleCondition condition, EvaluationContext context) {
        return evaluateBooleanPayload(context, "appFraudDetected", "authorizedPushPaymentFraud");
    }

    private boolean evaluateInvestmentScamPattern(RuleCondition condition, EvaluationContext context) {
        return evaluateBooleanPayload(context, "investmentScamPattern", "investmentScamDetected");
    }

    private boolean evaluateRomanceScamIndicator(RuleCondition condition, EvaluationContext context) {
        return evaluateBooleanPayload(context, "romanceScamIndicator", "romanceScamDetected");
    }

    private int parseIntSafe(String value, int defaultValue) {
        try {
            return Integer.parseInt(value);
        } catch (Exception e) {
            return defaultValue;
        }
    }

    private BigDecimal parseBigDecimal(String value) {
        return parseBigDecimal(value, BigDecimal.ZERO);
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
        return "COMPLIANCE";
    }
}
