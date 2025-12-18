package com.rulex.service;

import com.rulex.dto.TransactionRequest;
import com.rulex.repository.TransactionRepository;
import com.rulex.util.PanMaskingUtil;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

/**
 * Serviço avançado de motor de regras duras com as 28 novas regras identificadas no triple check.
 * Implementa regras determinísticas e configuráveis sem Machine Learning.
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class AdvancedRuleEngineService {

  private final TransactionRepository transactionRepository;
  private final AuditService auditService;

  public enum RuleCategory {
    EMV_SECURITY,
    TRANSACTION_CONTEXT,
    TERMINAL_NETWORK,
    PIN_CVV_VERIFICATION,
    CUSTOM_INDICATORS,
    TEMPORAL_ADVANCED,
    UNIQUE_IDENTIFIERS,
    CURRENCY_CONVERSION,
    AUTH_SEQUENCE,
    CONTEXT_COHERENCE,
    AUTHORIZATION_CONTRADICTION,
    ACQUIRER_PATTERN
  }

  public enum RuleResult {
    APPROVED,
    SUSPICIOUS,
    FRAUD
  }

  // ==================== GRUPO 1: EMV SECURITY (2 Regras) ====================

  /**
   * Regra 1: EMV_SECURITY_CHECK Valida indicadores de segurança EMV (cardAipStatic, cardAipDynamic,
   * etc)
   */
  public RuleResult checkEMVSecurity(TransactionRequest transaction) {
    log.debug("Executando regra: EMV_SECURITY_CHECK");

    boolean aipStaticValid = "Y".equals(transaction.getCardAipStatic());
    boolean aipDynamicValid = "Y".equals(transaction.getCardAipDynamic());
    boolean aipVerifyValid = "Y".equals(transaction.getCardAipVerify());

    if (!aipStaticValid || !aipDynamicValid || !aipVerifyValid) {
      if (transaction.getTransactionAmount().compareTo(new BigDecimal("1000")) > 0) {
        auditService.logRule("EMV_SECURITY_CHECK", transaction, "SUSPICIOUS");
        return RuleResult.SUSPICIOUS;
      }
    }

    return RuleResult.APPROVED;
  }

  /** Regra 2: TERMINAL_VERIFICATION_FAILED Detecta falhas em verificação do terminal ou cartão */
  public RuleResult checkTerminalVerificationFailed(TransactionRequest transaction) {
    log.debug("Executando regra: TERMINAL_VERIFICATION_FAILED");

    boolean terminalVerificationFailed =
        transaction.getTerminalVerificationResults() != null
            && transaction.getTerminalVerificationResults().contains("FAIL");
    boolean cardVerificationFailed =
        transaction.getCardVerificationResults() != null
            && transaction.getCardVerificationResults().contains("FAIL");

    if (terminalVerificationFailed || cardVerificationFailed) {
      auditService.logRule("TERMINAL_VERIFICATION_FAILED", transaction, "FRAUD");
      return RuleResult.FRAUD;
    }

    return RuleResult.APPROVED;
  }

  // ==================== GRUPO 2: TRANSACTION CONTEXT (3 Regras) ====================

  /** Regra 3: EXPIRED_CARD Valida se o cartão não está expirado */
  public RuleResult checkExpiredCard(TransactionRequest transaction) {
    log.debug("Executando regra: EXPIRED_CARD");

    if (transaction.getCardExpireDate() != null) {
      int expireDate = transaction.getCardExpireDate();
      int transactionDate = transaction.getTransactionDate();

      if (expireDate < transactionDate) {
        auditService.logRule("EXPIRED_CARD", transaction, "FRAUD");
        return RuleResult.FRAUD;
      }
    }

    return RuleResult.APPROVED;
  }

  /** Regra 4: SUSPICIOUS_TRANSACTION_TYPE Detecta tipos de transação suspeitos (Reversal, Void) */
  public RuleResult checkSuspiciousTransactionType(TransactionRequest transaction) {
    log.debug("Executando regra: SUSPICIOUS_TRANSACTION_TYPE");

    String transactionType = transaction.getTransactionType();
    if ("R".equals(transactionType) || "V".equals(transactionType)) {
      BigDecimal avgAmount = getCustomerAverageAmount(transaction.getCustomerIdFromHeader(), 30);
      if (transaction.getTransactionAmount().compareTo(avgAmount.multiply(new BigDecimal("2")))
          > 0) {
        auditService.logRule("SUSPICIOUS_TRANSACTION_TYPE", transaction, "SUSPICIOUS");
        return RuleResult.SUSPICIOUS;
      }
    }

    return RuleResult.APPROVED;
  }

  /** Regra 5: UNUSUAL_CARD_MEDIA Detecta mídia de cartão anômala (Chip em E-commerce) */
  public RuleResult checkUnusualCardMedia(TransactionRequest transaction) {
    log.debug("Executando regra: UNUSUAL_CARD_MEDIA");

    String cardMediaType = transaction.getCardMediaType();
    String posEntryMode = transaction.getPosEntryMode();

    // Chip (C) ou Magnetic (M) são válidos
    if (!"C".equals(cardMediaType) && !"M".equals(cardMediaType)) {
      if ("E".equals(posEntryMode) || "R".equals(posEntryMode)) {
        auditService.logRule("UNUSUAL_CARD_MEDIA", transaction, "SUSPICIOUS");
        return RuleResult.SUSPICIOUS;
      }
    }

    return RuleResult.APPROVED;
  }

  // ==================== GRUPO 3: TERMINAL & NETWORK (4 Regras) ====================

  /**
   * Regra 6: SUSPICIOUS_TERMINAL Detecta terminais suspeitos (ATM com transação fora do
   * estabelecimento)
   */
  public RuleResult checkSuspiciousTerminal(TransactionRequest transaction) {
    log.debug("Executando regra: SUSPICIOUS_TERMINAL");

    if ("A".equals(transaction.getTerminalType())
        && transaction.getPosOffPremises() == 1
        && transaction.getTransactionAmount().compareTo(new BigDecimal("5000")) > 0) {
      auditService.logRule("SUSPICIOUS_TERMINAL", transaction, "SUSPICIOUS");
      return RuleResult.SUSPICIOUS;
    }

    return RuleResult.APPROVED;
  }

  /** Regra 7: ECOMMERCE_NO_AVS Detecta E-commerce sem AVS */
  public RuleResult checkEcommerceNoAVS(TransactionRequest transaction) {
    log.debug("Executando regra: ECOMMERCE_NO_AVS");

    if (transaction.getEciIndicator() == 5
        && "N".equals(transaction.getAvsRequest())
        && transaction.getTransactionAmount().compareTo(new BigDecimal("1000")) > 0) {
      auditService.logRule("ECOMMERCE_NO_AVS", transaction, "SUSPICIOUS");
      return RuleResult.SUSPICIOUS;
    }

    return RuleResult.APPROVED;
  }

  /** Regra 8: POS_SECURITY_MISSING Detecta falta de segurança no POS */
  public RuleResult checkPOSSecurityMissing(TransactionRequest transaction) {
    log.debug("Executando regra: POS_SECURITY_MISSING");

    if (transaction.getPosSecurity() == 0
        && "C".equals(transaction.getPosEntryMode())
        && transaction.getTransactionAmount().compareTo(new BigDecimal("2000")) > 0) {
      auditService.logRule("POS_SECURITY_MISSING", transaction, "SUSPICIOUS");
      return RuleResult.SUSPICIOUS;
    }

    return RuleResult.APPROVED;
  }

  /** Regra 9: CARD_CAPTURE_FRAUD Detecta múltiplas capturas do mesmo cartão */
  public RuleResult checkCardCaptureFraud(TransactionRequest transaction) {
    log.debug("Executando regra: CARD_CAPTURE_FRAUD");

    if (transaction.getPosCardCapture() == 1) {
      String maskedPan = PanMaskingUtil.mask(transaction.getPan());
      long captureCount =
          transactionRepository.countCardCapturesSince(
              maskedPan, LocalDateTime.now().minusDays(30));
      if (captureCount > 2) {
        auditService.logRule("CARD_CAPTURE_FRAUD", transaction, "FRAUD");
        return RuleResult.FRAUD;
      }
    }

    return RuleResult.APPROVED;
  }

  // ==================== GRUPO 4: PIN/CVV VERIFICATION (3 Regras) ====================

  /** Regra 10: PIN_CVV_LIMIT_EXCEEDED Detecta limite de tentativas de PIN/CVV excedido */
  public RuleResult checkPinCvvLimitExceeded(TransactionRequest transaction) {
    log.debug("Executando regra: PIN_CVV_LIMIT_EXCEEDED");

    if (transaction.getCvvPinTryLimitExceeded() == 1) {
      auditService.logRule("PIN_CVV_LIMIT_EXCEEDED", transaction, "FRAUD");
      return RuleResult.FRAUD;
    }

    return RuleResult.APPROVED;
  }

  /** Regra 11: OFFLINE_PIN_FAILED Detecta falha de PIN offline */
  public RuleResult checkOfflinePinFailed(TransactionRequest transaction) {
    log.debug("Executando regra: OFFLINE_PIN_FAILED");

    if (transaction.getCvrofflinePinVerificationPerformed() == 1
        && transaction.getCvrofflinePinVerificationFailed() == 1) {
      auditService.logRule("OFFLINE_PIN_FAILED", transaction, "FRAUD");
      return RuleResult.FRAUD;
    }

    return RuleResult.APPROVED;
  }

  /** Regra 12: MISSING_CVV2_HIGH_RISK Detecta CVV2 ausente em transação de alto risco */
  public RuleResult checkMissingCvv2HighRisk(TransactionRequest transaction) {
    log.debug("Executando regra: MISSING_CVV2_HIGH_RISK");

    if ("0".equals(transaction.getCvv2Present())
        && isHighRiskMCC(transaction.getMcc())
        && transaction.getTransactionAmount().compareTo(new BigDecimal("1000")) > 0) {
      auditService.logRule("MISSING_CVV2_HIGH_RISK", transaction, "SUSPICIOUS");
      return RuleResult.SUSPICIOUS;
    }

    return RuleResult.APPROVED;
  }

  // ==================== GRUPO 5: CUSTOM INDICATORS (1 Regra) ====================

  /** Regra 13: CUSTOM_INDICATOR_FRAUD Detecta indicadores customizados como flags de fraude */
  public RuleResult checkCustomIndicatorFraud(TransactionRequest transaction) {
    log.debug("Executando regra: CUSTOM_INDICATOR_FRAUD");

    if ("F".equals(transaction.getUserIndicator01())
        || (transaction.getUserIndicator03() != null
            && transaction.getUserIndicator03().contains("BLOCKED"))
        || (transaction.getUserData04() != null && transaction.getUserData04().contains("FRAUD"))) {
      auditService.logRule("CUSTOM_INDICATOR_FRAUD", transaction, "FRAUD");
      return RuleResult.FRAUD;
    }

    return RuleResult.APPROVED;
  }

  // ==================== GRUPO 6: TEMPORAL ADVANCED (2 Regras) ====================

  /** Regra 14: PROCESSING_LAG_ANOMALY Detecta lag de processamento anômalo */
  public RuleResult checkProcessingLagAnomaly(TransactionRequest transaction) {
    log.debug("Executando regra: PROCESSING_LAG_ANOMALY");

    if (transaction.getRecordCreationTime() != null && transaction.getTransactionTime() != null) {
      long lagMinutes =
          calculateTimeLag(transaction.getTransactionTime(), transaction.getRecordCreationTime());
      if (lagMinutes > 60
          && transaction.getTransactionAmount().compareTo(new BigDecimal("5000")) > 0) {
        auditService.logRule("PROCESSING_LAG_ANOMALY", transaction, "SUSPICIOUS");
        return RuleResult.SUSPICIOUS;
      }
    }

    return RuleResult.APPROVED;
  }

  /** Regra 15: TIMEZONE_NORMALIZED_CHECK Normaliza transações por timezone */
  public RuleResult checkTimezoneNormalizedCheck(TransactionRequest transaction) {
    log.debug("Executando regra: TIMEZONE_NORMALIZED_CHECK");

    int hour = transaction.getTransactionTime() / 10000;
    if (hour >= 0 && hour < 6) { // Madrugada
      if (!"-03.00".equals(transaction.getGmtOffset())
          && !"-02.00".equals(transaction.getGmtOffset())) {
        if (transaction.getTransactionAmount().compareTo(new BigDecimal("2000")) > 0) {
          auditService.logRule("TIMEZONE_NORMALIZED_CHECK", transaction, "SUSPICIOUS");
          return RuleResult.SUSPICIOUS;
        }
      }
    }

    return RuleResult.APPROVED;
  }

  // ==================== GRUPO 7: UNIQUE IDENTIFIERS (3 Regras) ====================

  /** Regra 16: DUPLICATE_TRANSACTION Detecta transações duplicadas */
  public RuleResult checkDuplicateTransaction(TransactionRequest transaction) {
    log.debug("Executando regra: DUPLICATE_TRANSACTION");

    long duplicateCount =
        transactionRepository.countDuplicateTransactions(
            transaction.getExternalTransactionId(), transaction.getTransactionDate());

    if (duplicateCount > 0) {
      auditService.logRule("DUPLICATE_TRANSACTION", transaction, "FRAUD");
      return RuleResult.FRAUD;
    }

    return RuleResult.APPROVED;
  }

  /** Regra 17: SUSPICIOUS_MERCHANT_POSTAL Detecta CEP do merchant inválido */
  public RuleResult checkSuspiciousMerchantPostal(TransactionRequest transaction) {
    log.debug("Executando regra: SUSPICIOUS_MERCHANT_POSTAL");

    String postalCode = transaction.getMerchantPostalCode();
    if (postalCode == null || postalCode.startsWith("000000") || postalCode.isEmpty()) {
      auditService.logRule("SUSPICIOUS_MERCHANT_POSTAL", transaction, "SUSPICIOUS");
      return RuleResult.SUSPICIOUS;
    }

    return RuleResult.APPROVED;
  }

  /** Regra 18: SUSPICIOUS_TOKEN Detecta token suspeito (TEST, DEMO) */
  public RuleResult checkSuspiciousToken(TransactionRequest transaction) {
    log.debug("Executando regra: SUSPICIOUS_TOKEN");

    String tokenId = transaction.getTokenId();
    if (tokenId != null && (tokenId.contains("TEST") || tokenId.contains("DEMO"))) {
      if (transaction.getTransactionAmount().compareTo(new BigDecimal("1000")) > 0) {
        auditService.logRule("SUSPICIOUS_TOKEN", transaction, "SUSPICIOUS");
        return RuleResult.SUSPICIOUS;
      }
    }

    return RuleResult.APPROVED;
  }

  // ==================== GRUPO 8: CURRENCY & CONVERSION (2 Regras) ====================

  /** Regra 19: UNEXPECTED_CURRENCY Detecta moeda não esperada */
  public RuleResult checkUnexpectedCurrency(TransactionRequest transaction) {
    log.debug("Executando regra: UNEXPECTED_CURRENCY");

    if (transaction.getTransactionCurrencyCode() != 986
        && // 986 = BRL
        "076".equals(transaction.getMerchantCountryCode())
        && transaction.getTransactionAmount().compareTo(new BigDecimal("1000")) > 0) {
      auditService.logRule("UNEXPECTED_CURRENCY", transaction, "SUSPICIOUS");
      return RuleResult.SUSPICIOUS;
    }

    return RuleResult.APPROVED;
  }

  /** Regra 20: ANOMALOUS_CONVERSION_RATE Detecta taxa de conversão anômala */
  public RuleResult checkAnomalousConversionRate(TransactionRequest transaction) {
    log.debug("Executando regra: ANOMALOUS_CONVERSION_RATE");

    if (transaction.getTransactionCurrencyCode() != 986
        && transaction.getTransactionCurrencyConversionRate() != null) {
      BigDecimal avgRate =
          getAverageCurrencyConversionRate(transaction.getTransactionCurrencyCode(), 30);
      BigDecimal deviation =
          transaction.getTransactionCurrencyConversionRate().subtract(avgRate).abs();
      BigDecimal allowedDeviation = avgRate.multiply(new BigDecimal("0.1"));

      if (deviation.compareTo(allowedDeviation) > 0) {
        auditService.logRule("ANOMALOUS_CONVERSION_RATE", transaction, "SUSPICIOUS");
        return RuleResult.SUSPICIOUS;
      }
    }

    return RuleResult.APPROVED;
  }

  // ==================== GRUPO 9: AUTH SEQUENCE (1 Regra) ====================

  /** Regra 21: INCOHERENT_AUTH_SEQUENCE Detecta sequência de autenticação incoerente */
  public RuleResult checkIncoherentAuthSequence(TransactionRequest transaction) {
    log.debug("Executando regra: INCOHERENT_AUTH_SEQUENCE");

    boolean cryptogramValidButCvvInvalid =
        "V".equals(transaction.getCryptogramValid()) && "N".equals(transaction.getCvv2Response());
    boolean cavvValidButPinInvalid =
        transaction.getCavvResult() == 0 && "N".equals(transaction.getPinVerifyCode());
    boolean tokenSecureButScoreLow =
        transaction.getTokenAssuranceLevel() > 50
            && transaction.getConsumerAuthenticationScore() < 100;

    if (cryptogramValidButCvvInvalid || cavvValidButPinInvalid || tokenSecureButScoreLow) {
      auditService.logRule("INCOHERENT_AUTH_SEQUENCE", transaction, "SUSPICIOUS");
      return RuleResult.SUSPICIOUS;
    }

    return RuleResult.APPROVED;
  }

  // ==================== GRUPO 10: CONTEXT COHERENCE (1 Regra) ====================

  /** Regra 22: INCOHERENT_CONTEXT Detecta contexto incoerente */
  public RuleResult checkIncoherentContext(TransactionRequest transaction) {
    log.debug("Executando regra: INCOHERENT_CONTEXT");

    boolean ecommerceButCustomerPresent =
        "E".equals(transaction.getPosEntryMode()) && "Y".equals(transaction.getCustomerPresent());
    boolean atmButEcommerce =
        "A".equals(transaction.getTerminalType()) && "E".equals(transaction.getPosEntryMode());
    boolean chipButNoCryptogram =
        "C".equals(transaction.getCardMediaType()) && "N".equals(transaction.getCryptogramValid());

    if (ecommerceButCustomerPresent || atmButEcommerce || chipButNoCryptogram) {
      auditService.logRule("INCOHERENT_CONTEXT", transaction, "SUSPICIOUS");
      return RuleResult.SUSPICIOUS;
    }

    return RuleResult.APPROVED;
  }

  // ==================== GRUPO 11: AUTHORIZATION CONTRADICTION (1 Regra) ====================

  /** Regra 23: CONTRADICTORY_AUTHORIZATION Detecta autorização contraditória */
  public RuleResult checkContradictoryAuthorization(TransactionRequest transaction) {
    log.debug("Executando regra: CONTRADICTORY_AUTHORIZATION");

    boolean approvedButDifferentResponse =
        "A".equals(transaction.getAuthDecisionCode())
            && !"A".equals(transaction.getAuthResponseCode());
    boolean flagButZeroAmount =
        "A".equals(transaction.getAuthPostFlag())
            && transaction.getTransactionAmount().compareTo(BigDecimal.ZERO) == 0;
    boolean approvedButNoAuthId =
        "A".equals(transaction.getAuthDecisionCode()) && transaction.getAuthId() == null;

    if (approvedButDifferentResponse || flagButZeroAmount || approvedButNoAuthId) {
      auditService.logRule("CONTRADICTORY_AUTHORIZATION", transaction, "SUSPICIOUS");
      return RuleResult.SUSPICIOUS;
    }

    return RuleResult.APPROVED;
  }

  // ==================== GRUPO 12: ACQUIRER PATTERN (2 Regras) ====================

  /** Regra 24: SUSPICIOUS_ACQUIRER Detecta adquirente suspeito */
  public RuleResult checkSuspiciousAcquirer(TransactionRequest transaction) {
    log.debug("Executando regra: SUSPICIOUS_ACQUIRER");

    if (!"076".equals(transaction.getAcquirerCountry())
        && !"840".equals(transaction.getAcquirerCountry())
        && !"392".equals(transaction.getAcquirerCountry())
        && transaction.getTransactionAmount().compareTo(new BigDecimal("10000")) > 0) {
      auditService.logRule("SUSPICIOUS_ACQUIRER", transaction, "SUSPICIOUS");
      return RuleResult.SUSPICIOUS;
    }

    return RuleResult.APPROVED;
  }

  /** Regra 25: ACQUIRER_COUNTRY_MISMATCH Detecta mismatch entre país do adquirente e merchant */
  public RuleResult checkAcquirerCountryMismatch(TransactionRequest transaction) {
    log.debug("Executando regra: ACQUIRER_COUNTRY_MISMATCH");

    if (!transaction.getAcquirerCountry().equals(transaction.getMerchantCountryCode())
        && transaction.getTransactionAmount().compareTo(new BigDecimal("5000")) > 0) {
      auditService.logRule("ACQUIRER_COUNTRY_MISMATCH", transaction, "SUSPICIOUS");
      return RuleResult.SUSPICIOUS;
    }

    return RuleResult.APPROVED;
  }

  // ==================== REGRAS CONSOLIDADAS ====================

  /** Regra 26: COMBINED_SCORE_CHECK Consolidação de múltiplas regras de score */
  public RuleResult checkCombinedScore(TransactionRequest transaction) {
    log.debug("Executando regra: COMBINED_SCORE_CHECK");

    int combinedScore =
        (transaction.getConsumerAuthenticationScore() + transaction.getExternalScore3()) / 2;

    if (combinedScore < 100) {
      auditService.logRule("COMBINED_SCORE_CHECK", transaction, "FRAUD");
      return RuleResult.FRAUD;
    } else if (combinedScore < 200) {
      auditService.logRule("COMBINED_SCORE_CHECK", transaction, "SUSPICIOUS");
      return RuleResult.SUSPICIOUS;
    }

    return RuleResult.APPROVED;
  }

  /** Regra 27: VELOCITY_CHECK_CONSOLIDATED Consolidação de múltiplas regras de velocidade */
  public RuleResult checkVelocityConsolidated(TransactionRequest transaction) {
    log.debug("Executando regra: VELOCITY_CHECK_CONSOLIDATED");

    long count5min =
        Optional.ofNullable(
                transactionRepository.countTransactionsByCustomerSince(
                    transaction.getCustomerIdFromHeader(), LocalDateTime.now().minusMinutes(5)))
            .orElse(0L);

    if (count5min >= 3) {
      auditService.logRule("VELOCITY_CHECK_CONSOLIDATED", transaction, "FRAUD");
      return RuleResult.FRAUD;
    }

    long count1hour =
        Optional.ofNullable(
                transactionRepository.countTransactionsByCustomerSince(
                    transaction.getCustomerIdFromHeader(), LocalDateTime.now().minusMinutes(60)))
            .orElse(0L);

    if (count1hour >= 10) {
      auditService.logRule("VELOCITY_CHECK_CONSOLIDATED", transaction, "SUSPICIOUS");
      return RuleResult.SUSPICIOUS;
    }

    long countDaily =
        transactionRepository.countDailyTransactions(
            transaction.getCustomerIdFromHeader(), transaction.getTransactionDate());

    if (countDaily >= 50) {
      auditService.logRule("VELOCITY_CHECK_CONSOLIDATED", transaction, "SUSPICIOUS");
      return RuleResult.SUSPICIOUS;
    }

    return RuleResult.APPROVED;
  }

  /** Regra 28: CUSTOM_INDICATORS_COMPREHENSIVE Análise abrangente de indicadores customizados */
  public RuleResult checkCustomIndicatorsComprehensive(TransactionRequest transaction) {
    log.debug("Executando regra: CUSTOM_INDICATORS_COMPREHENSIVE");

    if ("F".equals(transaction.getUserIndicator01())
        || (transaction.getUserIndicator03() != null
            && transaction.getUserIndicator03().contains("BLOCK"))
        || (transaction.getUserIndicator04() != null
            && transaction.getUserIndicator04().contains("FRAUD"))
        || (transaction.getUserIndicator05() != null
            && transaction.getUserIndicator05().contains("ALERT"))
        || (transaction.getUserIndicator08() != null
            && transaction.getUserIndicator08().contains("RISK"))) {
      auditService.logRule("CUSTOM_INDICATORS_COMPREHENSIVE", transaction, "FRAUD");
      return RuleResult.FRAUD;
    }

    return RuleResult.APPROVED;
  }

  // ==================== MÉTODOS AUXILIARES ====================

  private boolean isHighRiskMCC(int mcc) {
    int[] highRiskMCCs = {7995, 6211, 6051, 7273, 7994, 7298, 6051, 6211, 6051};
    return Arrays.stream(highRiskMCCs).anyMatch(x -> x == mcc);
  }

  private BigDecimal getCustomerAverageAmount(String customerId, int days) {
    return transactionRepository
        .getCustomerAverageAmount(customerId, days)
        .orElse(new BigDecimal("1000"));
  }

  private BigDecimal getAverageCurrencyConversionRate(int currencyCode, int days) {
    return transactionRepository
        .getAverageCurrencyConversionRate(currencyCode, days)
        .orElse(new BigDecimal("1.0"));
  }

  private long calculateTimeLag(int transactionTime, int recordCreationTime) {
    // Implementação simplificada
    return Math.abs(recordCreationTime - transactionTime) / 100;
  }

  /** Executa todas as 28 regras e retorna o resultado mais severo */
  public RuleResult executeAllAdvancedRules(TransactionRequest transaction) {
    log.info(
        "Executando todas as 28 regras avançadas para transação: {}",
        transaction.getExternalTransactionId());

    RuleResult[] results = {
      checkEMVSecurity(transaction),
      checkTerminalVerificationFailed(transaction),
      checkExpiredCard(transaction),
      checkSuspiciousTransactionType(transaction),
      checkUnusualCardMedia(transaction),
      checkSuspiciousTerminal(transaction),
      checkEcommerceNoAVS(transaction),
      checkPOSSecurityMissing(transaction),
      checkCardCaptureFraud(transaction),
      checkPinCvvLimitExceeded(transaction),
      checkOfflinePinFailed(transaction),
      checkMissingCvv2HighRisk(transaction),
      checkCustomIndicatorFraud(transaction),
      checkProcessingLagAnomaly(transaction),
      checkTimezoneNormalizedCheck(transaction),
      checkDuplicateTransaction(transaction),
      checkSuspiciousMerchantPostal(transaction),
      checkSuspiciousToken(transaction),
      checkUnexpectedCurrency(transaction),
      checkAnomalousConversionRate(transaction),
      checkIncoherentAuthSequence(transaction),
      checkIncoherentContext(transaction),
      checkContradictoryAuthorization(transaction),
      checkSuspiciousAcquirer(transaction),
      checkAcquirerCountryMismatch(transaction),
      checkCombinedScore(transaction),
      checkVelocityConsolidated(transaction),
      checkCustomIndicatorsComprehensive(transaction)
    };

    // Retorna o resultado mais severo
    if (Arrays.stream(results).anyMatch(r -> r == RuleResult.FRAUD)) {
      return RuleResult.FRAUD;
    } else if (Arrays.stream(results).anyMatch(r -> r == RuleResult.SUSPICIOUS)) {
      return RuleResult.SUSPICIOUS;
    }

    return RuleResult.APPROVED;
  }
}
