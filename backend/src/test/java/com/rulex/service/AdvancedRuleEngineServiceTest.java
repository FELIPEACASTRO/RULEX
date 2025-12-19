package com.rulex.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.rulex.dto.TransactionRequest;
import com.rulex.repository.TransactionRepository;
import java.math.BigDecimal;
import java.time.Clock;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

class AdvancedRuleEngineServiceTest {

  private final TransactionRepository transactionRepository =
      Mockito.mock(TransactionRepository.class);
  private final AuditService auditService = Mockito.mock(AuditService.class);

  private final Clock clock = Clock.fixed(Instant.parse("2025-12-19T00:00:00Z"), ZoneOffset.UTC);

  private final AdvancedRuleEngineService service =
      new AdvancedRuleEngineService(transactionRepository, auditService, clock);

  @BeforeEach
  void defaultStubs() {
    doNothing().when(auditService).logRule(anyString(), any(TransactionRequest.class), anyString());
    when(transactionRepository.countCardCapturesSince(anyString(), any(LocalDateTime.class)))
        .thenReturn(0L);
    when(transactionRepository.countDuplicateTransactions(anyString(), anyInt())).thenReturn(0L);
    when(transactionRepository.getCustomerAverageAmount(anyString(), anyInt()))
        .thenReturn(Optional.of(new BigDecimal("1000")));
    when(transactionRepository.getAverageCurrencyConversionRate(anyInt(), anyInt()))
        .thenReturn(Optional.of(new BigDecimal("1.0")));
    when(transactionRepository.countTransactionsByCustomerSince(
            anyString(), any(LocalDateTime.class)))
        .thenReturn(0L);
    when(transactionRepository.countDailyTransactions(anyString(), anyInt())).thenReturn(0L);
  }

  @Test
  void rule01_emvSecurityCheck_suspiciousWhenAipInvalidAndHighAmount() {
    TransactionRequest req = baseline();
    req.setCardAipStatic("N");
    req.setTransactionAmount(new BigDecimal("1001"));
    assertThat(service.checkEMVSecurity(req))
        .isEqualTo(AdvancedRuleEngineService.RuleResult.SUSPICIOUS);
    verify(auditService).logRule(eq("EMV_SECURITY_CHECK"), eq(req), eq("SUSPICIOUS"));
  }

  @Test
  void rule02_terminalVerificationFailed_fraudWhenFailPresent() {
    TransactionRequest req = baseline();
    req.setTerminalVerificationResults("SOME_FAIL_CODE:FAIL");
    assertThat(service.checkTerminalVerificationFailed(req))
        .isEqualTo(AdvancedRuleEngineService.RuleResult.FRAUD);
  }

  @Test
  void rule03_expiredCard_fraudWhenExpireBeforeTransactionDate() {
    TransactionRequest req = baseline();
    req.setCardExpireDate(20240101);
    req.setTransactionDate(20250101);
    assertThat(service.checkExpiredCard(req)).isEqualTo(AdvancedRuleEngineService.RuleResult.FRAUD);
  }

  @Test
  void rule04_suspiciousTransactionType_suspiciousWhenReversalAndAmountAbove2xAvg() {
    TransactionRequest req = baseline();
    req.setTransactionType("R");
    when(transactionRepository.getCustomerAverageAmount(eq(req.getCustomerIdFromHeader()), eq(30)))
        .thenReturn(Optional.of(new BigDecimal("10")));
    req.setTransactionAmount(new BigDecimal("25"));
    assertThat(service.checkSuspiciousTransactionType(req))
        .isEqualTo(AdvancedRuleEngineService.RuleResult.SUSPICIOUS);
  }

  @Test
  void rule05_unusualCardMedia_suspiciousWhenNonChipOrMagneticInEcommerce() {
    TransactionRequest req = baseline();
    req.setCardMediaType("X");
    req.setPosEntryMode("E");
    assertThat(service.checkUnusualCardMedia(req))
        .isEqualTo(AdvancedRuleEngineService.RuleResult.SUSPICIOUS);
  }

  @Test
  void rule06_suspiciousTerminal_suspiciousWhenAtmOffPremisesHighAmount() {
    TransactionRequest req = baseline();
    req.setTerminalType("A");
    req.setPosOffPremises(1);
    req.setTransactionAmount(new BigDecimal("5000.01"));
    assertThat(service.checkSuspiciousTerminal(req))
        .isEqualTo(AdvancedRuleEngineService.RuleResult.SUSPICIOUS);
  }

  @Test
  void rule07_ecommerceNoAvs_suspiciousWhenEci5NoAvsHighAmount() {
    TransactionRequest req = baseline();
    req.setEciIndicator(5);
    req.setAvsRequest("N");
    req.setTransactionAmount(new BigDecimal("1000.01"));
    assertThat(service.checkEcommerceNoAVS(req))
        .isEqualTo(AdvancedRuleEngineService.RuleResult.SUSPICIOUS);
  }

  @Test
  void rule08_posSecurityMissing_suspiciousWhenPosSecurityZeroChipHighAmount() {
    TransactionRequest req = baseline();
    req.setPosSecurity(0);
    req.setPosEntryMode("C");
    req.setTransactionAmount(new BigDecimal("2000.01"));
    assertThat(service.checkPOSSecurityMissing(req))
        .isEqualTo(AdvancedRuleEngineService.RuleResult.SUSPICIOUS);
  }

  @Test
  void rule09_cardCaptureFraud_fraudWhenCaptureCountExceedsThreshold() {
    TransactionRequest req = baseline();
    req.setPosCardCapture(1);
    when(transactionRepository.countCardCapturesSince(anyString(), any(LocalDateTime.class)))
        .thenReturn(3L);
    assertThat(service.checkCardCaptureFraud(req))
        .isEqualTo(AdvancedRuleEngineService.RuleResult.FRAUD);
  }

  @Test
  void rule10_pinCvvLimitExceeded_fraudWhenFlagSet() {
    TransactionRequest req = baseline();
    req.setCvvPinTryLimitExceeded(1);
    assertThat(service.checkPinCvvLimitExceeded(req))
        .isEqualTo(AdvancedRuleEngineService.RuleResult.FRAUD);
  }

  @Test
  void rule11_offlinePinFailed_fraudWhenPerformedAndFailed() {
    TransactionRequest req = baseline();
    req.setCvrofflinePinVerificationPerformed(1);
    req.setCvrofflinePinVerificationFailed(1);
    assertThat(service.checkOfflinePinFailed(req))
        .isEqualTo(AdvancedRuleEngineService.RuleResult.FRAUD);
  }

  @Test
  void rule12_missingCvv2HighRisk_suspiciousWhenMissingCvv2HighRiskMccHighAmount() {
    TransactionRequest req = baseline();
    req.setCvv2Present("0");
    req.setMcc(7995);
    req.setTransactionAmount(new BigDecimal("1000.01"));
    assertThat(service.checkMissingCvv2HighRisk(req))
        .isEqualTo(AdvancedRuleEngineService.RuleResult.SUSPICIOUS);
  }

  @Test
  void rule13_customIndicatorFraud_fraudWhenUserIndicatorFlagsFraud() {
    TransactionRequest req = baseline();
    req.setUserIndicator01("F");
    assertThat(service.checkCustomIndicatorFraud(req))
        .isEqualTo(AdvancedRuleEngineService.RuleResult.FRAUD);
  }

  @Test
  void rule14_processingLagAnomaly_suspiciousWhenLagOver60MinutesHighAmount() {
    TransactionRequest req = baseline();
    req.setTransactionTime(100000);
    req.setRecordCreationTime(200000);
    req.setTransactionAmount(new BigDecimal("5000.01"));
    assertThat(service.checkProcessingLagAnomaly(req))
        .isEqualTo(AdvancedRuleEngineService.RuleResult.SUSPICIOUS);
  }

  @Test
  void rule15_timezoneNormalizedCheck_suspiciousWhenNightHourAndTimezoneMismatchHighAmount() {
    TransactionRequest req = baseline();
    req.setTransactionTime(10000); // 01:00
    req.setGmtOffset("+00.00");
    req.setTransactionAmount(new BigDecimal("2000.01"));
    assertThat(service.checkTimezoneNormalizedCheck(req))
        .isEqualTo(AdvancedRuleEngineService.RuleResult.SUSPICIOUS);
  }

  @Test
  void rule16_duplicateTransaction_fraudWhenDuplicateCountPositive() {
    TransactionRequest req = baseline();
    when(transactionRepository.countDuplicateTransactions(
            eq(req.getExternalTransactionId()), eq(req.getTransactionDate())))
        .thenReturn(1L);
    assertThat(service.checkDuplicateTransaction(req))
        .isEqualTo(AdvancedRuleEngineService.RuleResult.FRAUD);
  }

  @Test
  void rule17_suspiciousMerchantPostal_suspiciousWhenPostalCodeInvalid() {
    TransactionRequest req = baseline();
    req.setMerchantPostalCode("00000012");
    assertThat(service.checkSuspiciousMerchantPostal(req))
        .isEqualTo(AdvancedRuleEngineService.RuleResult.SUSPICIOUS);
  }

  @Test
  void rule18_suspiciousToken_suspiciousWhenTokenContainsTestAndHighAmount() {
    TransactionRequest req = baseline();
    req.setTokenId("MY_TEST_TOKEN");
    req.setTransactionAmount(new BigDecimal("1000.01"));
    assertThat(service.checkSuspiciousToken(req))
        .isEqualTo(AdvancedRuleEngineService.RuleResult.SUSPICIOUS);
  }

  @Test
  void rule19_unexpectedCurrency_suspiciousWhenBrazilCountryNonBrlHighAmount() {
    TransactionRequest req = baseline();
    req.setMerchantCountryCode("076");
    req.setTransactionCurrencyCode(840);
    req.setTransactionAmount(new BigDecimal("1000.01"));
    assertThat(service.checkUnexpectedCurrency(req))
        .isEqualTo(AdvancedRuleEngineService.RuleResult.SUSPICIOUS);
  }

  @Test
  void rule20_anomalousConversionRate_suspiciousWhenDeviationOver10Percent() {
    TransactionRequest req = baseline();
    req.setTransactionCurrencyCode(840);
    req.setTransactionCurrencyConversionRate(new BigDecimal("2.0"));
    when(transactionRepository.getAverageCurrencyConversionRate(eq(840), eq(30)))
        .thenReturn(Optional.of(new BigDecimal("1.0")));
    assertThat(service.checkAnomalousConversionRate(req))
        .isEqualTo(AdvancedRuleEngineService.RuleResult.SUSPICIOUS);
  }

  @Test
  void rule21_incoherentAuthSequence_suspiciousWhenCryptogramValidButCvvInvalid() {
    TransactionRequest req = baseline();
    req.setCryptogramValid("V");
    req.setCvv2Response("N");
    assertThat(service.checkIncoherentAuthSequence(req))
        .isEqualTo(AdvancedRuleEngineService.RuleResult.SUSPICIOUS);
  }

  @Test
  void rule22_incoherentContext_suspiciousWhenEcommerceButCustomerPresent() {
    TransactionRequest req = baseline();
    req.setPosEntryMode("E");
    req.setCustomerPresent("Y");
    assertThat(service.checkIncoherentContext(req))
        .isEqualTo(AdvancedRuleEngineService.RuleResult.SUSPICIOUS);
  }

  @Test
  void rule23_contradictoryAuthorization_suspiciousWhenDecisionApprovedButResponseDifferent() {
    TransactionRequest req = baseline();
    req.setAuthDecisionCode("A");
    req.setAuthResponseCode("D");
    assertThat(service.checkContradictoryAuthorization(req))
        .isEqualTo(AdvancedRuleEngineService.RuleResult.SUSPICIOUS);
  }

  @Test
  void rule24_suspiciousAcquirer_suspiciousWhenAcquirerCountryUnexpectedHighAmount() {
    TransactionRequest req = baseline();
    req.setAcquirerCountry("999");
    req.setTransactionAmount(new BigDecimal("10000.01"));
    assertThat(service.checkSuspiciousAcquirer(req))
        .isEqualTo(AdvancedRuleEngineService.RuleResult.SUSPICIOUS);
  }

  @Test
  void rule25_acquirerCountryMismatch_suspiciousWhenMismatchHighAmount() {
    TransactionRequest req = baseline();
    req.setAcquirerCountry("840");
    req.setMerchantCountryCode("076");
    req.setTransactionAmount(new BigDecimal("5000.01"));
    assertThat(service.checkAcquirerCountryMismatch(req))
        .isEqualTo(AdvancedRuleEngineService.RuleResult.SUSPICIOUS);
  }

  @Test
  void rule26_combinedScore_fraudWhenCombinedBelow100() {
    TransactionRequest req = baseline();
    req.setConsumerAuthenticationScore(10);
    req.setExternalScore3(10);
    assertThat(service.checkCombinedScore(req))
        .isEqualTo(AdvancedRuleEngineService.RuleResult.FRAUD);
  }

  @Test
  void rule27_velocityConsolidated_fraudWhenCount5MinAtLeast3() {
    TransactionRequest req = baseline();
    when(transactionRepository.countTransactionsByCustomerSince(
            eq(req.getCustomerIdFromHeader()), any(LocalDateTime.class)))
        .thenReturn(3L);
    assertThat(service.checkVelocityConsolidated(req))
        .isEqualTo(AdvancedRuleEngineService.RuleResult.FRAUD);
  }

  @Test
  void rule28_customIndicatorsComprehensive_fraudWhenAlertPresent() {
    TransactionRequest req = baseline();
    req.setUserIndicator05("ALERT");
    assertThat(service.checkCustomIndicatorsComprehensive(req))
        .isEqualTo(AdvancedRuleEngineService.RuleResult.FRAUD);
  }

  @Test
  void executeAllAdvancedRules_returnsMostSevere() {
    TransactionRequest req = baseline();
    req.setCvvPinTryLimitExceeded(1);
    assertThat(service.executeAllAdvancedRules(req))
        .isEqualTo(AdvancedRuleEngineService.RuleResult.FRAUD);
  }

  private static TransactionRequest baseline() {
    return TransactionRequest.builder()
        .externalTransactionId("tx-adv-1")
        .customerIdFromHeader("cust-adv")
        .customerAcctNumber(1234567890L)
        .pan("4111111111111111")
        .transactionAmount(new BigDecimal("10.00"))
        .transactionDate(20251218)
        .transactionTime(120000)
        .transactionCurrencyCode(986)
        .mcc(5411)
        .consumerAuthenticationScore(200)
        .externalScore3(200)
        .cavvResult(0)
        .cryptogramValid("V")
        .cvv2Response("M")
        .eciIndicator(5)
        .atcCard(1)
        .atcHost(1)
        .tokenAssuranceLevel(80)
        .availableCredit(new BigDecimal("1000.00"))
        .cardCashBalance(new BigDecimal("0.00"))
        .cardDelinquentAmount(new BigDecimal("0.00"))
        .merchantCountryCode("076")
        .merchantPostalCode("12345")
        .customerPresent("N")
        .cardAipStatic("Y")
        .cardAipDynamic("Y")
        .cardAipVerify("Y")
        .gmtOffset("-03.00")
        .build();
  }
}
