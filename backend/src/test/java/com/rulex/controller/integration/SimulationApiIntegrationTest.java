package com.rulex.controller.integration;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.*;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

/**
 * 🔥 TESTES INTEGRADOS - API DE SIMULAÇÃO
 * Testa RuleSimulationController, HomologSimulationController, RulesV31Controller
 */
@SuppressWarnings("unchecked")
@DisplayName("🔥 API de Simulação - Testes Integrados")
public class SimulationApiIntegrationTest extends BaseIntegrationTest {

    @Nested
    @DisplayName("POST /api/simulation/test - Teste de Regras")
    class TestRulesTests {

        @Test
        @DisplayName("✅ Cenário 1: Simular transação normal")
        void shouldSimulateNormalTransaction() throws Exception {
            Map<String, Object> request = createSimulationRequest();
            
            performPost("/api/simulation/test", request)
                .andExpect(status().isOk());
        }

        @Test
        @DisplayName("✅ Cenário 2: Simular transação de alto valor")
        void shouldSimulateHighValueTransaction() throws Exception {
            Map<String, Object> request = createHighValueSimulationRequest();
            
            performPost("/api/simulation/test", request)
                .andExpect(status().isOk());
        }

        @Test
        @DisplayName("✅ Cenário 3: Simular múltiplas transações em batch")
        void shouldSimulateBatchTransactions() throws Exception {
            List<Map<String, Object>> transactions = createBatchTransactions();
            Map<String, Object> request = new HashMap<>();
            request.put("transactions", transactions);
            
            performPost("/api/simulation/batch", request)
                .andExpect(status().isOk());
        }

        @Test
        @DisplayName("✅ Cenário 4: Comparar resultado de duas regras")
        void shouldCompareRules() throws Exception {
            Map<String, Object> request = createCompareRequest();
            
            performPost("/api/simulation/compare", request)
                .andExpect(status().isOk());
        }
    }

    @Nested
    @DisplayName("POST /api/rules-v31 - Validação e Lint")
    class RulesV31Tests {

        @Test
        @DisplayName("✅ Cenário 1: Validar regra válida")
        void shouldValidateValidRule() throws Exception {
            Map<String, Object> rule = createValidRule();
            
            performPost("/api/rules-v31/validate", rule)
                .andExpect(status().isOk());
        }

        @Test
        @DisplayName("✅ Cenário 2: Lint de regra")
        void shouldLintRule() throws Exception {
            Map<String, Object> rule = createValidRule();
            
            performPost("/api/rules-v31/lint", rule)
                .andExpect(status().isOk());
        }

        @Test
        @DisplayName("✅ Cenário 3: Simular regra V31")
        void shouldSimulateV31Rule() throws Exception {
            Map<String, Object> request = createV31SimulationRequest();
            
            performPost("/api/rules-v31/simulate", request)
                .andExpect(status().isOk());
        }
    }

    @Nested
    @DisplayName("POST /api/homolog/simulation - Simulação em Homologação")
    class HomologSimulationTests {

        @Test
        @DisplayName("✅ Cenário 1: Executar simulação em homologação")
        void shouldRunHomologSimulation() throws Exception {
            Map<String, Object> request = createHomologSimulationRequest();
            
            performPost("/api/homolog/simulation/run", request)
                .andExpect(status().isOk());
        }
    }

    // ========== Métodos auxiliares ==========

    private Map<String, Object> createSimulationRequest() {
        Map<String, Object> request = new HashMap<>();
        
        Map<String, Object> transaction = new HashMap<>();
        transaction.put("transactionId", UUID.randomUUID().toString());
        transaction.put("amount", new BigDecimal("500.00"));
        transaction.put("currency", "BRL");
        transaction.put("merchantCountryCode", "BR");
        transaction.put("mcc", "5411");
        transaction.put("customerId", "CUST_001");
        
        request.put("transaction", transaction);
        return request;
    }

    private Map<String, Object> createHighValueSimulationRequest() {
        Map<String, Object> request = createSimulationRequest();
        ((Map<String, Object>) request.get("transaction")).put("amount", new BigDecimal("100000.00"));
        return request;
    }

    private List<Map<String, Object>> createBatchTransactions() {
        List<Map<String, Object>> transactions = new ArrayList<>();
        for (int i = 0; i < 10; i++) {
            Map<String, Object> tx = new HashMap<>();
            tx.put("transactionId", UUID.randomUUID().toString());
            tx.put("amount", new BigDecimal(100 + i * 100));
            tx.put("currency", "BRL");
            tx.put("merchantCountryCode", "BR");
            tx.put("mcc", "5411");
            tx.put("customerId", "CUST_00" + i);
            transactions.add(tx);
        }
        return transactions;
    }

    private Map<String, Object> createCompareRequest() {
        Map<String, Object> request = new HashMap<>();
        request.put("ruleId1", UUID.randomUUID().toString());
        request.put("ruleId2", UUID.randomUUID().toString());
        request.put("transaction", createSimulationRequest().get("transaction"));
        return request;
    }

    private Map<String, Object> createValidRule() {
        Map<String, Object> rule = new HashMap<>();
        rule.put("name", "TEST_RULE_" + System.currentTimeMillis());
        rule.put("description", "Regra de teste para validação");
        rule.put("ruleKey", "TEST_" + System.currentTimeMillis());
        rule.put("priority", 100);
        
        Map<String, Object> condition = new HashMap<>();
        condition.put("fieldName", "amount");
        condition.put("operator", "GT");
        condition.put("value", "1000");
        
        rule.put("conditions", List.of(condition));
        return rule;
    }

    private Map<String, Object> createV31SimulationRequest() {
        Map<String, Object> request = new HashMap<>();
        request.put("rule", createValidRule());
        request.put("transaction", createSimulationRequest().get("transaction"));
        return request;
    }

    private Map<String, Object> createHomologSimulationRequest() {
        Map<String, Object> request = new HashMap<>();
        request.put("ruleSetId", UUID.randomUUID().toString());
        request.put("transactions", createBatchTransactions());
        return request;
    }
}
