package com.rulex.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.dto.TransactionRequest;
import com.rulex.dto.TransactionResponse;
import com.rulex.entity.RuleConfiguration;
import com.rulex.entity.Transaction;
import com.rulex.entity.TransactionDecision;
import com.rulex.repository.RuleConfigurationRepository;
import com.rulex.repository.TransactionDecisionRepository;
import com.rulex.repository.TransactionRepository;
import com.rulex.v31.execlog.RuleExecutionLogService;
import java.math.BigDecimal;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;

@SuppressWarnings("null")
class RuleEngineServiceTest {

  private final TransactionRepository transactionRepository =
      Mockito.mock(TransactionRepository.class);
  private final TransactionDecisionRepository decisionRepository =
      Mockito.mock(TransactionDecisionRepository.class);
  private final RuleConfigurationRepository ruleConfigRepository =
      Mockito.mock(RuleConfigurationRepository.class);
  private final AuditService auditService = Mockito.mock(AuditService.class);
  private final ObjectMapper objectMapper = new ObjectMapper();

  private final Clock clock = Clock.fixed(Instant.parse("2025-12-19T00:00:00Z"), ZoneOffset.UTC);

  private final PayloadHashService payloadHashService = new PayloadHashService(objectMapper);
  private final TransactionRawStoreService rawStoreService =
      Mockito.mock(TransactionRawStoreService.class);

  private final RuleExecutionLogService ruleExecutionLogService =
      Mockito.mock(RuleExecutionLogService.class);

  private final RuleEngineService service =
      new RuleEngineService(
          transactionRepository,
          decisionRepository,
          ruleConfigRepository,
          auditService,
          objectMapper,
          clock,
          payloadHashService,
          rawStoreService,
          ruleExecutionLogService);

  @Test
  void returnsApproved_whenNoEnabledRules() {
    when(ruleConfigRepository.findByEnabled(true)).thenReturn(List.of());
    when(transactionRepository.save(any(Transaction.class)))
        .thenAnswer(
            invocation -> {
              Transaction t = invocation.getArgument(0, Transaction.class);
              t.setId(1L);
              return t;
            });
    when(decisionRepository.save(any(TransactionDecision.class)))
        .thenAnswer(invocation -> invocation.getArgument(0, TransactionDecision.class));
    doNothing().when(auditService).logTransactionProcessed(any(), any(), any());

    TransactionResponse response = service.analyzeTransaction(minimalRequest());

    assertThat(response.getClassification()).isEqualTo("APPROVED");
    assertThat(response.getRiskScore()).isEqualTo(0);
    assertThat(response.getTriggeredRules()).isNotNull();
    assertThat(response.getTriggeredRules()).isEmpty();
    verify(auditService, times(1)).logTransactionProcessed(any(), any(), any());
  }

  @ParameterizedTest
  @CsvSource({
    // ruleName, threshold, expectedClassification
    "LOW_AUTHENTICATION_SCORE,50,SUSPICIOUS",
    "LOW_EXTERNAL_SCORE,50,SUSPICIOUS",
    "INVALID_CAVV,0,FRAUD",
    "INVALID_CRYPTOGRAM,0,FRAUD",
    "CVV_MISMATCH,0,SUSPICIOUS",
    "HIGH_TRANSACTION_AMOUNT,1000,SUSPICIOUS",
    "HIGH_RISK_MCC,0,SUSPICIOUS",
    "INTERNATIONAL_TRANSACTION,0,SUSPICIOUS",
    "CARD_NOT_PRESENT,0,SUSPICIOUS",
    "PIN_VERIFICATION_FAILED,0,SUSPICIOUS",
    "CVV_PIN_LIMIT_EXCEEDED,0,FRAUD",
    "OFFLINE_PIN_FAILED,0,FRAUD"
  })
  void triggersLegacyRule_andImpactsDecision(
      String ruleName, int threshold, String expectedClassification) {
    RuleConfiguration rule =
        RuleConfiguration.builder()
            .ruleName(ruleName)
            .description("test")
            .ruleType(RuleConfiguration.RuleType.SECURITY)
            .threshold(threshold)
            .weight(60)
            .enabled(true)
            .classification(
                TransactionDecision.TransactionClassification.valueOf(expectedClassification))
            .build();

    when(ruleConfigRepository.findByEnabled(true)).thenReturn(List.of(rule));
    when(transactionRepository.save(any(Transaction.class)))
        .thenAnswer(
            invocation -> {
              Transaction t = invocation.getArgument(0, Transaction.class);
              t.setId(10L);
              return t;
            });
    when(decisionRepository.save(any(TransactionDecision.class)))
        .thenAnswer(invocation -> invocation.getArgument(0, TransactionDecision.class));
    doNothing().when(auditService).logTransactionProcessed(any(), any(), any());
    doNothing().when(auditService).logError(any(), any());

    TransactionRequest req = minimalRequest();
    // Configure request fields to trigger each legacy rule deterministically.
    switch (ruleName) {
      case "LOW_AUTHENTICATION_SCORE" -> req.setConsumerAuthenticationScore(threshold - 1);
      case "LOW_EXTERNAL_SCORE" -> req.setExternalScore3(threshold - 1);
      case "INVALID_CAVV" -> req.setCavvResult(1);
      case "INVALID_CRYPTOGRAM" -> req.setCryptogramValid("N");
      case "CVV_MISMATCH" -> req.setCvv2Response("N");
      case "HIGH_TRANSACTION_AMOUNT" -> req.setTransactionAmount(new BigDecimal(threshold + 1));
      case "HIGH_RISK_MCC" -> req.setMcc(7995);
      case "INTERNATIONAL_TRANSACTION" -> req.setMerchantCountryCode("840");
      case "CARD_NOT_PRESENT" -> req.setCustomerPresent("N");
      case "PIN_VERIFICATION_FAILED" -> req.setPinVerifyCode("I");
      case "CVV_PIN_LIMIT_EXCEEDED" -> req.setCvvPinTryLimitExceeded(1);
      case "OFFLINE_PIN_FAILED" -> {
        req.setCvrofflinePinVerificationPerformed(1);
        req.setCvrofflinePinVerificationFailed(1);
      }
      default -> throw new IllegalStateException("Unexpected rule in test: " + ruleName);
    }

    TransactionResponse response = service.analyzeTransaction(req);

    assertThat(response.getTriggeredRules()).hasSize(1);
    assertThat(response.getTriggeredRules().getFirst().getName()).isEqualTo(ruleName);
    assertThat(response.getClassification()).isEqualTo(expectedClassification);
    assertThat(response.getRiskScore()).isEqualTo(60);
  }

  @Test
  void evaluatesGenericConditions_andUsesAndEarlyExit() throws Exception {
    String conditionsJson =
        objectMapper.writeValueAsString(
            List.of(
                // mcc must equal 7995 AND transactionAmount must be > 100
                java.util.Map.of("field", "mcc", "operator", "==", "value", "7995"),
                java.util.Map.of("field", "transactionAmount", "operator", ">", "value", "100")));

    RuleConfiguration rule =
        RuleConfiguration.builder()
            .ruleName("GENERIC_AND")
            .ruleType(RuleConfiguration.RuleType.CONTEXT)
            .threshold(0)
            .weight(40)
            .enabled(true)
            .classification(TransactionDecision.TransactionClassification.SUSPICIOUS)
            .conditionsJson(conditionsJson)
            .logicOperator(RuleConfiguration.LogicOperator.AND)
            .build();

    when(ruleConfigRepository.findByEnabled(true)).thenReturn(List.of(rule));
    when(transactionRepository.save(any(Transaction.class)))
        .thenAnswer(
            invocation -> {
              Transaction t = invocation.getArgument(0, Transaction.class);
              t.setId(11L);
              return t;
            });
    when(decisionRepository.save(any(TransactionDecision.class)))
        .thenAnswer(invocation -> invocation.getArgument(0, TransactionDecision.class));
    doNothing().when(auditService).logTransactionProcessed(any(), any(), any());

    TransactionRequest req = minimalRequest();
    req.setMcc(7995);
    req.setTransactionAmount(new BigDecimal("101"));

    TransactionResponse response = service.analyzeTransaction(req);

    assertThat(response.getRiskScore()).isEqualTo(40);
    assertThat(response.getClassification()).isEqualTo("SUSPICIOUS");
    assertThat(response.getTriggeredRules()).hasSize(1);
    assertThat(response.getTriggeredRules().getFirst().getName()).isEqualTo("GENERIC_AND");
    assertThat(response.getTriggeredRules().getFirst().getDetail())
        .contains("mcc == 7995")
        .contains("transactionAmount > 100");
  }

  @Test
  void riskScoreIsClampedTo100() {
    RuleConfiguration r1 =
        RuleConfiguration.builder()
            .ruleName("R1")
            .ruleType(RuleConfiguration.RuleType.SECURITY)
            .threshold(0)
            .weight(80)
            .enabled(true)
            .classification(TransactionDecision.TransactionClassification.SUSPICIOUS)
            .conditionsJson("[{\"field\":\"mcc\",\"operator\":\"==\",\"value\":\"5411\"}]")
            .logicOperator(RuleConfiguration.LogicOperator.AND)
            .build();

    RuleConfiguration r2 =
        RuleConfiguration.builder()
            .ruleName("R2")
            .ruleType(RuleConfiguration.RuleType.SECURITY)
            .threshold(0)
            .weight(80)
            .enabled(true)
            .classification(TransactionDecision.TransactionClassification.FRAUD)
            .conditionsJson("[{\"field\":\"mcc\",\"operator\":\"==\",\"value\":\"5411\"}]")
            .logicOperator(RuleConfiguration.LogicOperator.AND)
            .build();

    when(ruleConfigRepository.findByEnabled(true)).thenReturn(List.of(r1, r2));
    when(transactionRepository.save(any(Transaction.class)))
        .thenAnswer(
            invocation -> {
              Transaction t = invocation.getArgument(0, Transaction.class);
              t.setId(12L);
              return t;
            });
    when(decisionRepository.save(any(TransactionDecision.class)))
        .thenAnswer(invocation -> invocation.getArgument(0, TransactionDecision.class));
    doNothing().when(auditService).logTransactionProcessed(any(), any(), any());

    TransactionRequest req = minimalRequest();
    req.setMcc(5411);

    TransactionResponse response = service.analyzeTransaction(req);

    assertThat(response.getRiskScore()).isEqualTo(100);
    assertThat(response.getClassification()).isEqualTo("FRAUD");
  }

  @Test
  void persistsDecisionLinkedToTransaction() {
    when(ruleConfigRepository.findByEnabled(true)).thenReturn(List.of());
    when(transactionRepository.save(any(Transaction.class)))
        .thenAnswer(
            invocation -> {
              Transaction t = invocation.getArgument(0, Transaction.class);
              t.setId(99L);
              return t;
            });
    ArgumentCaptor<TransactionDecision> decisionCaptor =
        ArgumentCaptor.forClass(TransactionDecision.class);
    when(decisionRepository.save(decisionCaptor.capture()))
        .thenAnswer(invocation -> invocation.getArgument(0, TransactionDecision.class));
    doNothing().when(auditService).logTransactionProcessed(any(), any(), any());

    TransactionResponse response = service.analyzeTransaction(minimalRequest());

    assertThat(response.getTransactionId()).isEqualTo("txn-1");
    assertThat(decisionCaptor.getValue().getTransaction()).isNotNull();
    assertThat(decisionCaptor.getValue().getTransaction().getId()).isEqualTo(99L);
  }

  private static TransactionRequest minimalRequest() {
    return TransactionRequest.builder()
        .externalTransactionId("txn-1")
        .customerIdFromHeader("cust-1")
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
        .customerPresent("Y")
        .build();
  }
}
