package com.rulex.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.dto.TransactionRequest;
import com.rulex.entity.RuleConfiguration;
import com.rulex.entity.TransactionDecision.TransactionClassification;
import com.rulex.repository.RuleConfigurationRepository;
import com.rulex.repository.TransactionRepository;
import java.math.BigDecimal;
import java.time.Clock;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

/**
 * Testes para o DatabaseRuleExecutorService.
 *
 * <p>Este serviço executa regras de fraude configuradas no banco de dados, substituindo a lógica
 * hardcoded que existia anteriormente no AdvancedRuleEngineService.
 */
class DatabaseRuleExecutorServiceTest {

  private final RuleConfigurationRepository ruleConfigRepository =
      Mockito.mock(RuleConfigurationRepository.class);
  private final TransactionRepository transactionRepository =
      Mockito.mock(TransactionRepository.class);
  private final AuditService auditService = Mockito.mock(AuditService.class);
  private final ObjectMapper objectMapper = new ObjectMapper();
  private final Clock clock = Clock.fixed(Instant.parse("2025-12-19T00:00:00Z"), ZoneOffset.UTC);

  private final DatabaseRuleExecutorService service =
      new DatabaseRuleExecutorService(
          ruleConfigRepository, transactionRepository, auditService, objectMapper, clock);

  @BeforeEach
  void defaultStubs() {
    doNothing().when(auditService).logRule(anyString(), any(TransactionRequest.class), anyString());
    when(transactionRepository.countDuplicateTransactions(anyString(), anyInt())).thenReturn(0L);
    when(transactionRepository.countTransactionsByCustomerSince(
            anyString(), any(LocalDateTime.class)))
        .thenReturn(0L);
    when(transactionRepository.countDailyTransactions(anyString(), anyInt())).thenReturn(0L);
    when(ruleConfigRepository.findByEnabledAndAdvanced(true, true)).thenReturn(List.of());
  }

  @Test
  void executeAdvancedRules_returnsApprovedWhenNoRulesConfigured() {
    TransactionRequest req = baseline();

    DatabaseRuleExecutorService.RuleExecution result = service.executeAdvancedRules(req);

    assertThat(result.result()).isEqualTo(DatabaseRuleExecutorService.RuleResult.APPROVED);
    assertThat(result.triggeredRules()).isEmpty();
  }

  @Test
  void executeAdvancedRules_returnsFraudForDuplicateTransaction() {
    TransactionRequest req = baseline();
    when(transactionRepository.countDuplicateTransactions(
            req.getExternalTransactionId(), req.getTransactionDate()))
        .thenReturn(1L);

    RuleConfiguration duplicateRule =
        RuleConfiguration.builder()
            .ruleName("DUPLICATE_TRANSACTION")
            .description("Detecta transações duplicadas")
            .ruleType(RuleConfiguration.RuleType.VELOCITY)
            .threshold(0)
            .weight(100)
            .enabled(true)
        .advanced(true)
            .classification(TransactionClassification.FRAUD)
            .build();

    when(ruleConfigRepository.findByEnabledAndAdvanced(true, true)).thenReturn(List.of(duplicateRule));

    DatabaseRuleExecutorService.RuleExecution result = service.executeAdvancedRules(req);

    assertThat(result.result()).isEqualTo(DatabaseRuleExecutorService.RuleResult.FRAUD);
    assertThat(result.triggeredRules()).hasSize(1);
    assertThat(result.triggeredRules().get(0).getName()).isEqualTo("DUPLICATE_TRANSACTION");
  }

  @Test
  void executeAdvancedRules_returnsFraudForVelocityExceeded() {
    TransactionRequest req = baseline();
    when(transactionRepository.countTransactionsByCustomerSince(
            anyString(), any(LocalDateTime.class)))
        .thenReturn(5L); // Mais de 3 em 5 minutos

    RuleConfiguration velocityRule =
        RuleConfiguration.builder()
            .ruleName("VELOCITY_CHECK_CONSOLIDATED")
            .description("Consolidação de múltiplas regras de velocidade")
            .ruleType(RuleConfiguration.RuleType.VELOCITY)
            .threshold(3)
            .weight(60)
            .enabled(true)
        .advanced(true)
            .classification(TransactionClassification.FRAUD)
            .build();

    when(ruleConfigRepository.findByEnabledAndAdvanced(true, true)).thenReturn(List.of(velocityRule));

    DatabaseRuleExecutorService.RuleExecution result = service.executeAdvancedRules(req);

    assertThat(result.result()).isEqualTo(DatabaseRuleExecutorService.RuleResult.FRAUD);
  }

  @Test
  void executeAdvancedRules_returnsSuspiciousForThresholdRule() {
    TransactionRequest req = baseline();
    req.setTransactionAmount(new BigDecimal("15000")); // Acima do threshold

    RuleConfiguration thresholdRule =
        RuleConfiguration.builder()
            .ruleName("SUSPICIOUS_ACQUIRER")
            .description("Detecta adquirente suspeito")
            .ruleType(RuleConfiguration.RuleType.CONTEXT)
            .threshold(10000)
            .weight(35)
            .enabled(true)
        .advanced(true)
            .classification(TransactionClassification.SUSPICIOUS)
            .build();

    when(ruleConfigRepository.findByEnabledAndAdvanced(true, true)).thenReturn(List.of(thresholdRule));

    DatabaseRuleExecutorService.RuleExecution result = service.executeAdvancedRules(req);

    assertThat(result.result()).isEqualTo(DatabaseRuleExecutorService.RuleResult.SUSPICIOUS);
  }

  @Test
  void executeAdvancedRules_ignoresDisabledRules() {
    TransactionRequest req = baseline();
    when(transactionRepository.countDuplicateTransactions(
            req.getExternalTransactionId(), req.getTransactionDate()))
        .thenReturn(1L);

    RuleConfiguration disabledRule =
        RuleConfiguration.builder()
            .ruleName("DUPLICATE_TRANSACTION")
            .description("Detecta transações duplicadas")
            .ruleType(RuleConfiguration.RuleType.VELOCITY)
            .threshold(0)
            .weight(100)
            .enabled(false) // Desabilitada
            .classification(TransactionClassification.FRAUD)
            .build();

    // findByEnabled(true) não retorna regras desabilitadas
    when(ruleConfigRepository.findByEnabledAndAdvanced(true, true)).thenReturn(List.of());

    DatabaseRuleExecutorService.RuleExecution result = service.executeAdvancedRules(req);

    assertThat(result.result()).isEqualTo(DatabaseRuleExecutorService.RuleResult.APPROVED);
  }

  @Test
  void executeAdvancedRules_returnsMostSevereResult() {
    TransactionRequest req = baseline();
    req.setTransactionAmount(new BigDecimal("15000"));
    when(transactionRepository.countDuplicateTransactions(
            req.getExternalTransactionId(), req.getTransactionDate()))
        .thenReturn(1L);

    RuleConfiguration fraudRule =
        RuleConfiguration.builder()
            .ruleName("DUPLICATE_TRANSACTION")
            .description("Detecta transações duplicadas")
            .ruleType(RuleConfiguration.RuleType.VELOCITY)
            .threshold(0)
            .weight(100)
            .enabled(true)
        .advanced(true)
            .classification(TransactionClassification.FRAUD)
            .build();

    RuleConfiguration suspiciousRule =
        RuleConfiguration.builder()
            .ruleName("SUSPICIOUS_ACQUIRER")
            .description("Detecta adquirente suspeito")
            .ruleType(RuleConfiguration.RuleType.CONTEXT)
            .threshold(10000)
            .weight(35)
            .enabled(true)
        .advanced(true)
            .classification(TransactionClassification.SUSPICIOUS)
            .build();

    when(ruleConfigRepository.findByEnabledAndAdvanced(true, true)).thenReturn(List.of(fraudRule, suspiciousRule));

    DatabaseRuleExecutorService.RuleExecution result = service.executeAdvancedRules(req);

    // FRAUD é mais severo que SUSPICIOUS
    assertThat(result.result()).isEqualTo(DatabaseRuleExecutorService.RuleResult.FRAUD);
    assertThat(result.triggeredRules()).hasSize(2);
  }

  @Test
  void executeAdvancedRules_filtersOnlyAdvancedRules() {
    TransactionRequest req = baseline();

    // Regra que NÃO está na lista de regras avançadas
    RuleConfiguration otherRule =
        RuleConfiguration.builder()
            .ruleName("SOME_OTHER_RULE")
            .description("Outra regra qualquer")
            .ruleType(RuleConfiguration.RuleType.CONTEXT)
            .threshold(100)
            .weight(50)
            .enabled(true)
        .advanced(false)
            .classification(TransactionClassification.SUSPICIOUS)
            .build();

    // Seleção do ruleset avançado é definida no banco: enabled=true AND advanced=true.
    when(ruleConfigRepository.findByEnabledAndAdvanced(true, true)).thenReturn(List.of());

    DatabaseRuleExecutorService.RuleExecution result = service.executeAdvancedRules(req);

    // Não deve processar regras que não pertencem ao ruleset advanced
    assertThat(result.result()).isEqualTo(DatabaseRuleExecutorService.RuleResult.APPROVED);
    assertThat(result.triggeredRules()).isEmpty();
  }

  private static TransactionRequest baseline() {
    return TransactionRequest.builder()
        .externalTransactionId("tx-db-1")
        .customerIdFromHeader("cust-db")
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
        .acquirerCountry("076")
        .build();
  }
}
