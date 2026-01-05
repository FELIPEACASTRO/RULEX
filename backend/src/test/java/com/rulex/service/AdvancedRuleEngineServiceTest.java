package com.rulex.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.rulex.dto.TransactionRequest;
import com.rulex.dto.TriggeredRuleDTO;
import java.math.BigDecimal;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

/**
 * Testes para o AdvancedRuleEngineService refatorado.
 *
 * <p>Após a refatoração GAP-FIX #1, o AdvancedRuleEngineService delega a execução das regras para o
 * DatabaseRuleExecutorService. Estes testes verificam que a delegação funciona corretamente.
 */
class AdvancedRuleEngineServiceTest {

  private final DatabaseRuleExecutorService databaseRuleExecutorService =
      Mockito.mock(DatabaseRuleExecutorService.class);

  private final AdvancedRuleEngineService service =
      new AdvancedRuleEngineService(databaseRuleExecutorService);

  @BeforeEach
  void defaultStubs() {
    // Por padrão, retorna APPROVED sem regras acionadas
    when(databaseRuleExecutorService.executeAdvancedRules(any(TransactionRequest.class)))
        .thenReturn(
            new DatabaseRuleExecutorService.RuleExecution(
                DatabaseRuleExecutorService.RuleResult.APPROVED, List.of()));
  }

  @Test
  void executeAllAdvancedRules_delegatesToDatabaseService() {
    TransactionRequest req = baseline();

    AdvancedRuleEngineService.RuleResult result = service.executeAllAdvancedRules(req);

    assertThat(result).isEqualTo(AdvancedRuleEngineService.RuleResult.APPROVED);
  }

  @Test
  void executeAllAdvancedRules_returnsFraudWhenDatabaseServiceReturnsFraud() {
    TransactionRequest req = baseline();
    when(databaseRuleExecutorService.executeAdvancedRules(any(TransactionRequest.class)))
        .thenReturn(
            new DatabaseRuleExecutorService.RuleExecution(
                DatabaseRuleExecutorService.RuleResult.FRAUD,
                List.of(TriggeredRuleDTO.builder().name("EXPIRED_CARD").detail("test").build())));

    AdvancedRuleEngineService.RuleResult result = service.executeAllAdvancedRules(req);

    assertThat(result).isEqualTo(AdvancedRuleEngineService.RuleResult.FRAUD);
  }

  @Test
  void executeAllAdvancedRules_returnsSuspiciousWhenDatabaseServiceReturnsSuspicious() {
    TransactionRequest req = baseline();
    when(databaseRuleExecutorService.executeAdvancedRules(any(TransactionRequest.class)))
        .thenReturn(
            new DatabaseRuleExecutorService.RuleExecution(
                DatabaseRuleExecutorService.RuleResult.SUSPICIOUS,
                List.of(
                    TriggeredRuleDTO.builder().name("EMV_SECURITY_CHECK").detail("test").build())));

    AdvancedRuleEngineService.RuleResult result = service.executeAllAdvancedRules(req);

    assertThat(result).isEqualTo(AdvancedRuleEngineService.RuleResult.SUSPICIOUS);
  }

  @Test
  void executeAllAdvancedRulesDetailed_returnsTriggeredRules() {
    TransactionRequest req = baseline();
    List<TriggeredRuleDTO> expectedRules =
        List.of(
            TriggeredRuleDTO.builder().name("EXPIRED_CARD").detail("test").build(),
            TriggeredRuleDTO.builder().name("DUPLICATE_TRANSACTION").detail("test").build());

    when(databaseRuleExecutorService.executeAdvancedRules(any(TransactionRequest.class)))
        .thenReturn(
            new DatabaseRuleExecutorService.RuleExecution(
                DatabaseRuleExecutorService.RuleResult.FRAUD, expectedRules));

    AdvancedRuleEngineService.AdvancedExecution execution =
        service.executeAllAdvancedRulesDetailed(req);

    assertThat(execution.result()).isEqualTo(AdvancedRuleEngineService.RuleResult.FRAUD);
    assertThat(execution.triggeredRules()).hasSize(2);
    assertThat(execution.triggeredRules().get(0).getName()).isEqualTo("EXPIRED_CARD");
    assertThat(execution.triggeredRules().get(1).getName()).isEqualTo("DUPLICATE_TRANSACTION");
  }

  @Test
  void executeAllAdvancedRulesDetailed_returnsEmptyListWhenNoRulesTriggered() {
    TransactionRequest req = baseline();

    AdvancedRuleEngineService.AdvancedExecution execution =
        service.executeAllAdvancedRulesDetailed(req);

    assertThat(execution.result()).isEqualTo(AdvancedRuleEngineService.RuleResult.APPROVED);
    assertThat(execution.triggeredRules()).isEmpty();
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
