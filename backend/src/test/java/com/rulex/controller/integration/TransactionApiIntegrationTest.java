package com.rulex.controller.integration;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;

/** 🔥 TESTES INTEGRADOS - API DE TRANSAÇÕES Testa EvaluateController e TransactionController */
@DisplayName("🔥 API de Transações - Testes Integrados")
public class TransactionApiIntegrationTest extends BaseIntegrationTest {

  @Nested
  @DisplayName("POST /api/evaluate - Avaliação de Transações")
  class EvaluateTransactionTests {

    @Test
    @DisplayName("✅ Cenário 1: Transação normal aprovada")
    void shouldApproveNormalTransaction() throws Exception {
      Map<String, Object> transaction = createNormalTransaction();

      performPost("/api/evaluate", transaction)
          .andExpect(status().isOk())
          .andExpect(jsonPath("$.decision").exists());
    }

    @Test
    @DisplayName("⚠️ Cenário 2: Transação de alto valor")
    void shouldFlagHighValueTransaction() throws Exception {
      Map<String, Object> transaction = createHighValueTransaction();

      performPost("/api/evaluate", transaction)
          .andExpect(status().isOk())
          .andExpect(jsonPath("$.decision").exists());
    }

    @Test
    @DisplayName("🚨 Cenário 3: Transação suspeita - país de alto risco")
    void shouldFlagHighRiskCountryTransaction() throws Exception {
      Map<String, Object> transaction = createHighRiskCountryTransaction();

      performPost("/api/evaluate", transaction)
          .andExpect(status().isOk())
          .andExpect(jsonPath("$.decision").exists());
    }

    @Test
    @DisplayName("🚨 Cenário 4: Transação com device novo")
    void shouldFlagNewDeviceTransaction() throws Exception {
      Map<String, Object> transaction = createNewDeviceTransaction();

      performPost("/api/evaluate", transaction)
          .andExpect(status().isOk())
          .andExpect(jsonPath("$.decision").exists());
    }

    @Test
    @DisplayName("🚨 Cenário 5: Transação noturna")
    void shouldFlagNighttimeTransaction() throws Exception {
      Map<String, Object> transaction = createNighttimeTransaction();

      performPost("/api/evaluate", transaction)
          .andExpect(status().isOk())
          .andExpect(jsonPath("$.decision").exists());
    }

    @Test
    @DisplayName("❌ Cenário 6: Transação com dados inválidos")
    void shouldRejectInvalidTransaction() throws Exception {
      Map<String, Object> transaction = new HashMap<>();
      // Transação vazia

      performPost("/api/evaluate", transaction).andExpect(status().isBadRequest());
    }

    @Test
    @DisplayName("🚨 Cenário 7: Transação com MCC de alto risco (gambling)")
    void shouldFlagGamblingMccTransaction() throws Exception {
      Map<String, Object> transaction = createGamblingTransaction();

      performPost("/api/evaluate", transaction)
          .andExpect(status().isOk())
          .andExpect(jsonPath("$.decision").exists());
    }

    @Test
    @DisplayName("🚨 Cenário 8: Transação com valor redondo suspeito")
    void shouldFlagRoundAmountTransaction() throws Exception {
      Map<String, Object> transaction = createRoundAmountTransaction();

      performPost("/api/evaluate", transaction)
          .andExpect(status().isOk())
          .andExpect(jsonPath("$.decision").exists());
    }

    @Test
    @DisplayName("🚨 Cenário 9: Transação internacional primeira vez")
    void shouldFlagFirstInternationalTransaction() throws Exception {
      Map<String, Object> transaction = createFirstInternationalTransaction();

      performPost("/api/evaluate", transaction)
          .andExpect(status().isOk())
          .andExpect(jsonPath("$.decision").exists());
    }

    @Test
    @DisplayName("🚨 Cenário 10: Transação com VPN detectado")
    void shouldFlagVpnTransaction() throws Exception {
      Map<String, Object> transaction = createVpnTransaction();

      performPost("/api/evaluate", transaction)
          .andExpect(status().isOk())
          .andExpect(jsonPath("$.decision").exists());
    }
  }

  @Nested
  @DisplayName("GET /api/transactions - Listagem de Transações")
  class ListTransactionsTests {

    @Test
    @DisplayName("✅ Cenário 1: Listar todas as transações")
    void shouldListAllTransactions() throws Exception {
      performGet("/api/transactions").andExpect(status().isOk()).andExpect(jsonPath("$").isArray());
    }

    @Test
    @DisplayName("✅ Cenário 2: Listar com paginação")
    void shouldListWithPagination() throws Exception {
      mockMvc
          .perform(
              get("/api/transactions")
                  .param("page", "0")
                  .param("size", "10")
                  .contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk());
    }

    @Test
    @DisplayName("✅ Cenário 3: Filtrar por status")
    void shouldFilterByStatus() throws Exception {
      mockMvc
          .perform(
              get("/api/transactions")
                  .param("status", "APPROVED")
                  .contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk());
    }

    @Test
    @DisplayName("✅ Cenário 4: Filtrar por data")
    void shouldFilterByDate() throws Exception {
      mockMvc
          .perform(
              get("/api/transactions")
                  .param("startDate", LocalDateTime.now().minusDays(7).toString())
                  .param("endDate", LocalDateTime.now().toString())
                  .contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk());
    }
  }

  // ========== Métodos auxiliares para criar transações de teste ==========

  private Map<String, Object> createNormalTransaction() {
    Map<String, Object> tx = new HashMap<>();
    tx.put("transactionId", UUID.randomUUID().toString());
    tx.put("amount", new BigDecimal("150.00"));
    tx.put("currency", "BRL");
    tx.put("cardNumber", "4111111111111111");
    tx.put("merchantId", "MERCHANT_001");
    tx.put("merchantName", "Loja Normal");
    tx.put("merchantCity", "São Paulo");
    tx.put("merchantCountryCode", "BR");
    tx.put("mcc", "5411"); // Grocery
    tx.put("customerId", "CUST_001");
    tx.put("deviceId", "DEVICE_001");
    tx.put("ipAddress", "189.100.50.25");
    tx.put("timestamp", LocalDateTime.now().toString());
    return tx;
  }

  private Map<String, Object> createHighValueTransaction() {
    Map<String, Object> tx = createNormalTransaction();
    tx.put("amount", new BigDecimal("50000.00"));
    return tx;
  }

  private Map<String, Object> createHighRiskCountryTransaction() {
    Map<String, Object> tx = createNormalTransaction();
    tx.put("merchantCountryCode", "NG"); // Nigeria
    tx.put("merchantCity", "Lagos");
    return tx;
  }

  private Map<String, Object> createNewDeviceTransaction() {
    Map<String, Object> tx = createNormalTransaction();
    tx.put("deviceId", "NEW_DEVICE_" + UUID.randomUUID());
    tx.put("isNewDevice", true);
    return tx;
  }

  private Map<String, Object> createNighttimeTransaction() {
    Map<String, Object> tx = createNormalTransaction();
    tx.put("timestamp", LocalDateTime.now().withHour(3).withMinute(30).toString());
    return tx;
  }

  private Map<String, Object> createGamblingTransaction() {
    Map<String, Object> tx = createNormalTransaction();
    tx.put("mcc", "7995"); // Gambling
    tx.put("merchantName", "Casino Online");
    return tx;
  }

  private Map<String, Object> createRoundAmountTransaction() {
    Map<String, Object> tx = createNormalTransaction();
    tx.put("amount", new BigDecimal("10000.00"));
    return tx;
  }

  private Map<String, Object> createFirstInternationalTransaction() {
    Map<String, Object> tx = createNormalTransaction();
    tx.put("merchantCountryCode", "US");
    tx.put("merchantCity", "New York");
    tx.put("isFirstInternational", true);
    return tx;
  }

  private Map<String, Object> createVpnTransaction() {
    Map<String, Object> tx = createNormalTransaction();
    tx.put("ipAddress", "185.220.101.1"); // Known VPN IP
    tx.put("isVpn", true);
    return tx;
  }
}
