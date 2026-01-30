package com.rulex.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.api.RawPayloadCaptureFilter;
import com.rulex.dto.TransactionRequest;
import com.rulex.dto.TransactionResponse;
import com.rulex.service.AdvancedRuleEngineService;
import com.rulex.core.engine.port.RuleEngineInputPort;
import com.rulex.core.transaction.port.TransactionQueryInputPort;
import java.time.Clock;
import java.util.List;
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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

@SpringBootTest(classes = TransactionControllerTest.TestApplication.class)
@AutoConfigureMockMvc(addFilters = false)
class TransactionControllerTest {

  @SpringBootConfiguration
  @EnableAutoConfiguration(
      exclude = {
        DataSourceAutoConfiguration.class,
        FlywayAutoConfiguration.class,
        HibernateJpaAutoConfiguration.class
      })
  @Import(TransactionController.class)
  static class TestApplication {}

  @Autowired private MockMvc mockMvc;
  @Autowired private ObjectMapper objectMapper;

  @SuppressWarnings("removal")
  @MockBean private RuleEngineInputPort ruleEngineService;

  @SuppressWarnings("removal")
  @MockBean private TransactionQueryInputPort transactionQueryService;

  @SuppressWarnings("removal")
  @MockBean private AdvancedRuleEngineService advancedRuleEngineService;

  @SuppressWarnings("removal")
  @MockBean private Clock clock;

  @Test
  void analyzeTransactionReturnsOk() throws Exception {
    TransactionRequest request = minimalTransactionRequest();

    TransactionResponse response = TransactionResponse.builder().classification("APPROVED").build();
    when(ruleEngineService.analyzeTransaction(any(), any(), anyString())).thenReturn(response);

    mockMvc
        .perform(
            post("/transactions/analyze")
                .requestAttr(RawPayloadCaptureFilter.RAW_BYTES_ATTR, new byte[] {1, 2})
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
        .andExpect(status().isOk());
  }

  @Test
  void listTransactionsReturnsOk() throws Exception {
    Page<TransactionResponse> page = new PageImpl<>(List.of());
    when(transactionQueryService.findTransactions(any(), any(), any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(page);

    mockMvc.perform(get("/transactions")).andExpect(status().isOk());
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
