package com.rulex.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.dto.RuleConditionDTO;
import com.rulex.dto.RuleConfigurationDTO;
import com.rulex.dto.TransactionRequest;
import com.rulex.core.simulation.port.RuleSimulationInputPort;
import com.rulex.core.simulation.usecase.RuleSimulationUseCase.BacktestResult;
import com.rulex.core.simulation.usecase.RuleSimulationUseCase.SimulationResult;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringBootConfiguration;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.flyway.FlywayAutoConfiguration;
import org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration;
import org.springframework.boot.autoconfigure.orm.jpa.HibernateJpaAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

@SpringBootTest(classes = RuleSimulationControllerTest.TestApplication.class)
@AutoConfigureMockMvc(addFilters = false)
class RuleSimulationControllerTest {

  @SpringBootConfiguration
  @EnableAutoConfiguration(
      exclude = {
        DataSourceAutoConfiguration.class,
        FlywayAutoConfiguration.class,
        HibernateJpaAutoConfiguration.class
      })
  @Import(RuleSimulationController.class)
  static class TestApplication {}

  @Autowired private MockMvc mockMvc;
  @Autowired private ObjectMapper objectMapper;

  @SuppressWarnings("removal")
  @MockBean private RuleSimulationInputPort simulationService;

  @Test
  void simulateRuleReturnsOk() throws Exception {
    SimulationResult result = SimulationResult.builder().ruleName("Rule 1").triggered(true).build();
    when(simulationService.simulateRule(any(), any())).thenReturn(result);

    Map<String, Object> request =
        Map.of(
            "rule", minimalRuleConfiguration(),
            "testPayload", minimalTransactionRequest());

    mockMvc
        .perform(
            post("/rules/simulation/test")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
        .andExpect(status().isOk());
  }

  @Test
  void backtestRuleReturnsOk() throws Exception {
    BacktestResult result = BacktestResult.builder().ruleId(1L).build();
    when(simulationService.backtestRule(eq(1L), any(), any(), eq(1000))).thenReturn(result);

    mockMvc
        .perform(
            post("/rules/simulation/backtest/1")
                .param("startDate", LocalDateTime.now().minusDays(1).toString())
                .param("endDate", LocalDateTime.now().toString())
                .param("sampleSize", "1000"))
        .andExpect(status().isOk());
  }

  private static RuleConfigurationDTO minimalRuleConfiguration() {
    return RuleConfigurationDTO.builder()
        .ruleName("Rule 1")
        .ruleType("SECURITY")
        .threshold(10)
        .weight(50)
        .enabled(true)
        .classification("FRAUD")
        .conditions(
            List.of(RuleConditionDTO.builder().field("amount").operator("GT").value("100").build()))
        .logicOperator("AND")
        .build();
  }

  private static TransactionRequest minimalTransactionRequest() {
    return TransactionRequest.builder()
        .externalTransactionId("tx-1")
        .customerIdFromHeader("cust")
        .customerAcctNumber(1234567890L)
        .pan("4111111111111111")
        .transactionAmount(new java.math.BigDecimal("10.00"))
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
        .availableCredit(new java.math.BigDecimal("1000.00"))
        .cardCashBalance(new java.math.BigDecimal("0.00"))
        .cardDelinquentAmount(new java.math.BigDecimal("0.00"))
        .merchantCountryCode("076")
        .customerPresent("Y")
        .build();
  }
}
