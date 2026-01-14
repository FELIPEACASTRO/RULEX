package com.rulex.controller.integration;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;

import java.util.*;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

/**
 * 🔥 TESTES INTEGRADOS - API DE EXPORT/IMPORT E APROVAÇÃO
 * Testa RuleExportImportController, RuleApprovalController
 */
@DisplayName("🔥 API de Export/Import e Aprovação - Testes Integrados")
public class ExportImportAndApprovalApiIntegrationTest extends BaseIntegrationTest {

    @Nested
    @DisplayName("GET/POST /api/rules/export - Exportação de Regras")
    class ExportTests {

        @Test
        @DisplayName("✅ Cenário 1: Exportar todas as regras")
        void shouldExportAllRules() throws Exception {
            performGet("/api/rules/export")
                .andExpect(status().isOk());
        }

        @Test
        @DisplayName("✅ Cenário 2: Exportar regras selecionadas")
        void shouldExportSelectedRules() throws Exception {
            Map<String, Object> request = new HashMap<>();
            request.put("ruleIds", List.of(
                UUID.randomUUID().toString(),
                UUID.randomUUID().toString()
            ));
            
            performPost("/api/rules/export/selective", request)
                .andExpect(status().isOk());
        }

        @Test
        @DisplayName("✅ Cenário 3: Exportar regras complexas")
        void shouldExportComplexRules() throws Exception {
            performGet("/api/rules/export/complex")
                .andExpect(status().isOk());
        }
    }

    @Nested
    @DisplayName("POST /api/rules/import - Importação de Regras")
    class ImportTests {

        @Test
        @DisplayName("✅ Cenário 1: Importar regras via JSON")
        void shouldImportRulesJson() throws Exception {
            Map<String, Object> importData = createImportData();
            
            performPost("/api/rules/import", importData)
                .andExpect(status().isOk());
        }

        @Test
        @DisplayName("✅ Cenário 2: Importar regras via arquivo")
        void shouldImportRulesFile() throws Exception {
            String jsonContent = objectMapper.writeValueAsString(createImportData());
            MockMultipartFile file = new MockMultipartFile(
                "file",
                "rules.json",
                MediaType.APPLICATION_JSON_VALUE,
                jsonContent.getBytes()
            );
            
            mockMvc.perform(multipart("/api/rules/import/file")
                    .file(file))
                .andExpect(status().isOk());
        }

        @Test
        @DisplayName("❌ Cenário 3: Importar arquivo inválido")
        void shouldRejectInvalidFile() throws Exception {
            MockMultipartFile file = new MockMultipartFile(
                "file",
                "invalid.txt",
                "text/plain",
                "invalid content".getBytes()
            );
            
            mockMvc.perform(multipart("/api/rules/import/file")
                    .file(file))
                .andExpect(status().isBadRequest());
        }
    }

    @Nested
    @DisplayName("POST /api/approval - Fluxo de Aprovação")
    class ApprovalTests {

        @Test
        @DisplayName("✅ Cenário 1: Criar solicitação de aprovação para nova regra")
        void shouldCreateApprovalForNewRule() throws Exception {
            Map<String, Object> request = createNewRuleApprovalRequest();
            
            performPost("/api/approval/create", request)
                .andExpect(status().isOk());
        }

        @Test
        @DisplayName("✅ Cenário 2: Criar solicitação de aprovação para atualização")
        void shouldCreateApprovalForUpdate() throws Exception {
            String ruleId = UUID.randomUUID().toString();
            Map<String, Object> request = createUpdateApprovalRequest();
            
            performPost("/api/approval/update/" + ruleId, request)
                .andExpect(status().isOk());
        }

        @Test
        @DisplayName("✅ Cenário 3: Criar solicitação de aprovação para exclusão")
        void shouldCreateApprovalForDelete() throws Exception {
            String ruleId = UUID.randomUUID().toString();
            Map<String, Object> request = new HashMap<>();
            request.put("reason", "Regra obsoleta");
            
            performPost("/api/approval/delete/" + ruleId, request)
                .andExpect(status().isOk());
        }

        @Test
        @DisplayName("✅ Cenário 4: Aprovar solicitação")
        void shouldApproveRequest() throws Exception {
            String approvalId = UUID.randomUUID().toString();
            Map<String, Object> request = new HashMap<>();
            request.put("comment", "Aprovado após revisão");
            
            performPost("/api/approval/" + approvalId + "/approve", request)
                .andExpect(status().isOk());
        }

        @Test
        @DisplayName("✅ Cenário 5: Rejeitar solicitação")
        void shouldRejectRequest() throws Exception {
            String approvalId = UUID.randomUUID().toString();
            Map<String, Object> request = new HashMap<>();
            request.put("comment", "Regra não atende aos requisitos");
            request.put("reason", "INVALID_LOGIC");
            
            performPost("/api/approval/" + approvalId + "/reject", request)
                .andExpect(status().isOk());
        }
    }

    @Nested
    @DisplayName("POST /api/homolog/rules - Regras em Homologação")
    class HomologRulesTests {

        @Test
        @DisplayName("✅ Cenário 1: Criar regra em homologação")
        void shouldCreateHomologRule() throws Exception {
            Map<String, Object> rule = createHomologRule();
            
            performPost("/api/homolog/rules", rule)
                .andExpect(status().isOk());
        }

        @Test
        @DisplayName("✅ Cenário 2: Obter última versão de regra")
        void shouldGetLatestRuleVersion() throws Exception {
            String ruleId = UUID.randomUUID().toString();
            performGet("/api/homolog/rules/" + ruleId + "/latest")
                .andExpect(status().isOk());
        }

        @Test
        @DisplayName("✅ Cenário 3: Publicar versão de regra")
        void shouldPublishRuleVersion() throws Exception {
            String ruleVersionId = UUID.randomUUID().toString();
            Map<String, Object> request = new HashMap<>();
            request.put("comment", "Publicando para produção");
            
            performPost("/api/homolog/rules/versions/" + ruleVersionId + "/publish", request)
                .andExpect(status().isOk());
        }

        @Test
        @DisplayName("✅ Cenário 4: Rollback de regra")
        void shouldRollbackRule() throws Exception {
            String ruleId = UUID.randomUUID().toString();
            Map<String, Object> request = new HashMap<>();
            request.put("reason", "Versão anterior era mais estável");
            
            performPost("/api/homolog/rules/" + ruleId + "/rollback/1", request)
                .andExpect(status().isOk());
        }
    }

    // ========== Métodos auxiliares ==========

    private Map<String, Object> createImportData() {
        Map<String, Object> data = new HashMap<>();
        
        List<Map<String, Object>> rules = new ArrayList<>();
        
        Map<String, Object> rule1 = new HashMap<>();
        rule1.put("name", "IMPORTED_RULE_1");
        rule1.put("description", "Regra importada 1");
        rule1.put("ruleKey", "IMPORT_1_" + System.currentTimeMillis());
        rule1.put("priority", 100);
        rule1.put("conditions", List.of(
            Map.of("fieldName", "amount", "operator", "GT", "value", "1000")
        ));
        rules.add(rule1);
        
        Map<String, Object> rule2 = new HashMap<>();
        rule2.put("name", "IMPORTED_RULE_2");
        rule2.put("description", "Regra importada 2");
        rule2.put("ruleKey", "IMPORT_2_" + System.currentTimeMillis());
        rule2.put("priority", 90);
        rule2.put("conditions", List.of(
            Map.of("fieldName", "merchantCountryCode", "operator", "IN", "value", List.of("NG", "KP"))
        ));
        rules.add(rule2);
        
        data.put("rules", rules);
        data.put("version", "1.0");
        data.put("exportedAt", java.time.LocalDateTime.now().toString());
        
        return data;
    }

    private Map<String, Object> createNewRuleApprovalRequest() {
        Map<String, Object> request = new HashMap<>();
        
        Map<String, Object> rule = new HashMap<>();
        rule.put("name", "NEW_RULE_FOR_APPROVAL");
        rule.put("description", "Nova regra aguardando aprovação");
        rule.put("ruleKey", "APPROVAL_" + System.currentTimeMillis());
        rule.put("priority", 100);
        rule.put("conditions", List.of(
            Map.of("fieldName", "amount", "operator", "GT", "value", "5000")
        ));
        
        request.put("rule", rule);
        request.put("requestedBy", "user@example.com");
        request.put("reason", "Nova regra para detecção de fraude");
        
        return request;
    }

    private Map<String, Object> createUpdateApprovalRequest() {
        Map<String, Object> request = new HashMap<>();
        
        Map<String, Object> changes = new HashMap<>();
        changes.put("priority", 95);
        changes.put("description", "Descrição atualizada");
        
        request.put("changes", changes);
        request.put("requestedBy", "user@example.com");
        request.put("reason", "Ajuste de prioridade");
        
        return request;
    }

    private Map<String, Object> createHomologRule() {
        Map<String, Object> rule = new HashMap<>();
        rule.put("name", "HOMOLOG_RULE_" + System.currentTimeMillis());
        rule.put("description", "Regra em homologação");
        rule.put("ruleKey", "HOMOLOG_" + System.currentTimeMillis());
        rule.put("priority", 100);
        rule.put("status", "TESTING");
        rule.put("conditions", List.of(
            Map.of("fieldName", "amount", "operator", "GT", "value", "10000")
        ));
        return rule;
    }
}
