package com.rulex.controller.integration;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;

import java.time.LocalDateTime;
import java.util.*;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

/**
 * 🔥 TESTES INTEGRADOS - API DE AUDITORIA E MÉTRICAS
 * Testa AuditController, MetricsController, RuleMetricsController
 */
@DisplayName("🔥 API de Auditoria e Métricas - Testes Integrados")
public class AuditAndMetricsApiIntegrationTest extends BaseIntegrationTest {

    @Nested
    @DisplayName("GET /api/audit - Logs de Auditoria")
    class AuditTests {

        @Test
        @DisplayName("✅ Cenário 1: Listar todos os logs de auditoria")
        void shouldListAllAuditLogs() throws Exception {
            performGet("/api/audit")
                .andExpect(status().isOk());
        }

        @Test
        @DisplayName("✅ Cenário 2: Listar com paginação")
        void shouldListWithPagination() throws Exception {
            mockMvc.perform(get("/api/audit")
                    .param("page", "0")
                    .param("size", "20")
                    .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        }

        @Test
        @DisplayName("✅ Cenário 3: Filtrar por período")
        void shouldFilterByDateRange() throws Exception {
            mockMvc.perform(get("/api/audit")
                    .param("startDate", LocalDateTime.now().minusDays(30).toString())
                    .param("endDate", LocalDateTime.now().toString())
                    .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        }

        @Test
        @DisplayName("✅ Cenário 4: Filtrar por ação")
        void shouldFilterByAction() throws Exception {
            mockMvc.perform(get("/api/audit")
                    .param("action", "CREATE")
                    .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        }

        @Test
        @DisplayName("✅ Cenário 5: Exportar auditoria em JSON")
        void shouldExportAuditJson() throws Exception {
            mockMvc.perform(get("/api/audit/export")
                    .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        }

        @Test
        @DisplayName("✅ Cenário 6: Exportar auditoria em CSV")
        void shouldExportAuditCsv() throws Exception {
            mockMvc.perform(get("/api/audit/export/csv")
                    .accept("text/csv"))
                .andExpect(status().isOk());
        }

        @Test
        @DisplayName("✅ Cenário 7: Buscar auditoria por transação")
        void shouldGetAuditByTransaction() throws Exception {
            String transactionId = UUID.randomUUID().toString();
            performGet("/api/audit/transaction/" + transactionId)
                .andExpect(status().isOk());
        }
    }

    @Nested
    @DisplayName("GET /api/metrics - Métricas Gerais")
    class MetricsTests {

        @Test
        @DisplayName("✅ Cenário 1: Obter métricas gerais")
        void shouldGetGeneralMetrics() throws Exception {
            performGet("/api/metrics")
                .andExpect(status().isOk());
        }

        @Test
        @DisplayName("✅ Cenário 2: Obter métricas por MCC")
        void shouldGetMccMetrics() throws Exception {
            performGet("/api/metrics/mcc")
                .andExpect(status().isOk());
        }

        @Test
        @DisplayName("✅ Cenário 3: Obter métricas por merchant")
        void shouldGetMerchantMetrics() throws Exception {
            performGet("/api/metrics/merchant")
                .andExpect(status().isOk());
        }

        @Test
        @DisplayName("✅ Cenário 4: Obter timeline de métricas")
        void shouldGetMetricsTimeline() throws Exception {
            performGet("/api/metrics/timeline")
                .andExpect(status().isOk());
        }
    }

    @Nested
    @DisplayName("GET /api/rule-metrics - Métricas de Regras")
    class RuleMetricsTests {

        @Test
        @DisplayName("✅ Cenário 1: Obter dashboard de métricas")
        void shouldGetDashboard() throws Exception {
            performGet("/api/rule-metrics/dashboard")
                .andExpect(status().isOk());
        }

        @Test
        @DisplayName("✅ Cenário 2: Obter métricas de todas as regras")
        void shouldGetAllRuleMetrics() throws Exception {
            performGet("/api/rule-metrics/all")
                .andExpect(status().isOk());
        }

        @Test
        @DisplayName("✅ Cenário 3: Obter métricas de regra específica")
        void shouldGetSpecificRuleMetrics() throws Exception {
            String ruleId = UUID.randomUUID().toString();
            performGet("/api/rule-metrics/" + ruleId)
                .andExpect(status().isOk());
        }

        @Test
        @DisplayName("✅ Cenário 4: Registrar falso positivo")
        void shouldRegisterFalsePositive() throws Exception {
            String ruleId = UUID.randomUUID().toString();
            Map<String, Object> request = new HashMap<>();
            request.put("transactionId", UUID.randomUUID().toString());
            request.put("reason", "Cliente confirmou transação legítima");
            
            performPost("/api/rule-metrics/" + ruleId + "/false-positive", request)
                .andExpect(status().isOk());
        }

        @Test
        @DisplayName("✅ Cenário 5: Registrar verdadeiro positivo")
        void shouldRegisterTruePositive() throws Exception {
            String ruleId = UUID.randomUUID().toString();
            Map<String, Object> request = new HashMap<>();
            request.put("transactionId", UUID.randomUUID().toString());
            request.put("reason", "Fraude confirmada");
            
            performPost("/api/rule-metrics/" + ruleId + "/true-positive", request)
                .andExpect(status().isOk());
        }
    }
}
