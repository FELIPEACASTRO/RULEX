package com.rulex.controller.integration;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

import java.util.*;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.springframework.http.MediaType;

/**
 * 🔥 TESTES INTEGRADOS - API DE REGRAS Testa RuleController, ComplexRuleController,
 * ComplexRuleCrudController
 */
@DisplayName("🔥 API de Regras - Testes Integrados")
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class RuleApiIntegrationTest extends BaseIntegrationTest {

  @Nested
  @DisplayName("GET /api/rules - Listagem de Regras")
  class ListRulesTests {

    @Test
    @DisplayName("✅ Cenário 1: Listar todas as regras")
    void shouldListAllRules() throws Exception {
      performGet("/api/rules").andExpect(status().isOk());
    }

    @Test
    @DisplayName("✅ Cenário 2: Listar com paginação")
    void shouldListWithPagination() throws Exception {
      mockMvc
          .perform(
              get("/api/rules")
                  .param("page", "0")
                  .param("size", "10")
                  .contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk());
    }

    @Test
    @DisplayName("✅ Cenário 3: Filtrar por status PUBLISHED")
    void shouldFilterByPublishedStatus() throws Exception {
      mockMvc
          .perform(
              get("/api/rules")
                  .param("status", "PUBLISHED")
                  .contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk());
    }

    @Test
    @DisplayName("✅ Cenário 4: Filtrar por status DRAFT")
    void shouldFilterByDraftStatus() throws Exception {
      mockMvc
          .perform(
              get("/api/rules").param("status", "DRAFT").contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk());
    }
  }

  @Nested
  @DisplayName("POST /api/rules - Criação de Regras")
  @TestMethodOrder(MethodOrderer.OrderAnnotation.class)
  class CreateRuleTests {

    @Test
    @Order(1)
    @DisplayName("✅ Cenário 1: Criar regra simples de valor máximo")
    void shouldCreateMaxAmountRule() throws Exception {
      Map<String, Object> rule = createMaxAmountRule();

      performPost("/api/rules", rule)
          .andExpect(status().isOk())
          .andExpect(jsonPath("$.id").exists());
    }

    @Test
    @Order(2)
    @DisplayName("✅ Cenário 2: Criar regra de país de alto risco")
    void shouldCreateHighRiskCountryRule() throws Exception {
      Map<String, Object> rule = createHighRiskCountryRule();

      performPost("/api/rules", rule)
          .andExpect(status().isOk())
          .andExpect(jsonPath("$.id").exists());
    }

    @Test
    @Order(3)
    @DisplayName("✅ Cenário 3: Criar regra de velocity")
    void shouldCreateVelocityRule() throws Exception {
      Map<String, Object> rule = createVelocityRule();

      performPost("/api/rules", rule)
          .andExpect(status().isOk())
          .andExpect(jsonPath("$.id").exists());
    }

    @Test
    @Order(4)
    @DisplayName("✅ Cenário 4: Criar regra de MCC gambling")
    void shouldCreateGamblingMccRule() throws Exception {
      Map<String, Object> rule = createGamblingMccRule();

      performPost("/api/rules", rule)
          .andExpect(status().isOk())
          .andExpect(jsonPath("$.id").exists());
    }

    @Test
    @Order(5)
    @DisplayName("✅ Cenário 5: Criar regra de device novo")
    void shouldCreateNewDeviceRule() throws Exception {
      Map<String, Object> rule = createNewDeviceRule();

      performPost("/api/rules", rule)
          .andExpect(status().isOk())
          .andExpect(jsonPath("$.id").exists());
    }

    @Test
    @DisplayName("❌ Cenário 6: Criar regra sem nome (deve falhar)")
    void shouldFailWithoutName() throws Exception {
      Map<String, Object> rule = new HashMap<>();
      rule.put("description", "Regra sem nome");

      performPost("/api/rules", rule).andExpect(status().isBadRequest());
    }

    @Test
    @DisplayName("❌ Cenário 7: Criar regra com operador inválido")
    void shouldFailWithInvalidOperator() throws Exception {
      Map<String, Object> rule = createRuleWithInvalidOperator();

      performPost("/api/rules", rule).andExpect(status().isBadRequest());
    }
  }

  @Nested
  @DisplayName("GET /api/complex-rules - Regras Complexas")
  class ComplexRulesTests {

    @Test
    @DisplayName("✅ Cenário 1: Listar regras complexas")
    void shouldListComplexRules() throws Exception {
      performGet("/api/complex-rules").andExpect(status().isOk());
    }

    @Test
    @DisplayName("✅ Cenário 2: Criar regra complexa com múltiplas condições")
    void shouldCreateComplexRuleWithMultipleConditions() throws Exception {
      Map<String, Object> rule = createComplexRuleWithMultipleConditions();

      performPost("/api/complex-rules", rule)
          .andExpect(status().isOk())
          .andExpect(jsonPath("$.id").exists());
    }

    @Test
    @DisplayName("✅ Cenário 3: Criar regra com grupos aninhados (AND/OR)")
    void shouldCreateNestedGroupRule() throws Exception {
      Map<String, Object> rule = createNestedGroupRule();

      performPost("/api/complex-rules", rule)
          .andExpect(status().isOk())
          .andExpect(jsonPath("$.id").exists());
    }

    @Test
    @DisplayName("✅ Cenário 4: Criar regra com operador NEO4J")
    void shouldCreateNeo4jRule() throws Exception {
      Map<String, Object> rule = createNeo4jRule();

      performPost("/api/complex-rules", rule)
          .andExpect(status().isOk())
          .andExpect(jsonPath("$.id").exists());
    }

    @Test
    @DisplayName("✅ Cenário 5: Criar regra com operador VELOCITY")
    void shouldCreateVelocityComplexRule() throws Exception {
      Map<String, Object> rule = createVelocityComplexRule();

      performPost("/api/complex-rules", rule)
          .andExpect(status().isOk())
          .andExpect(jsonPath("$.id").exists());
    }
  }

  // ========== Métodos auxiliares para criar regras de teste ==========

  private Map<String, Object> createMaxAmountRule() {
    Map<String, Object> rule = new HashMap<>();
    rule.put("name", "MAX_AMOUNT_RULE_" + UUID.randomUUID().toString().substring(0, 8));
    rule.put("description", "Bloqueia transações acima de R$ 10.000");
    rule.put("ruleKey", "MAX_AMOUNT_" + System.currentTimeMillis());
    rule.put("priority", 100);
    rule.put("status", "DRAFT");

    Map<String, Object> condition = new HashMap<>();
    condition.put("fieldName", "amount");
    condition.put("operator", "GT");
    condition.put("value", "10000");

    rule.put("conditions", List.of(condition));
    return rule;
  }

  private Map<String, Object> createHighRiskCountryRule() {
    Map<String, Object> rule = new HashMap<>();
    rule.put("name", "HIGH_RISK_COUNTRY_" + UUID.randomUUID().toString().substring(0, 8));
    rule.put("description", "Alerta para países de alto risco");
    rule.put("ruleKey", "HIGH_RISK_COUNTRY_" + System.currentTimeMillis());
    rule.put("priority", 90);
    rule.put("status", "DRAFT");

    Map<String, Object> condition = new HashMap<>();
    condition.put("fieldName", "merchantCountryCode");
    condition.put("operator", "IN");
    condition.put("value", List.of("NG", "KP", "IR", "SY"));

    rule.put("conditions", List.of(condition));
    return rule;
  }

  private Map<String, Object> createVelocityRule() {
    Map<String, Object> rule = new HashMap<>();
    rule.put("name", "VELOCITY_RULE_" + UUID.randomUUID().toString().substring(0, 8));
    rule.put("description", "Mais de 5 transações em 1 hora");
    rule.put("ruleKey", "VELOCITY_" + System.currentTimeMillis());
    rule.put("priority", 80);
    rule.put("status", "DRAFT");

    Map<String, Object> condition = new HashMap<>();
    condition.put("fieldName", "transactionCount");
    condition.put("operator", "VELOCITY_COUNT_GT");
    condition.put("value", "5");
    condition.put("timeWindow", "1h");

    rule.put("conditions", List.of(condition));
    return rule;
  }

  private Map<String, Object> createGamblingMccRule() {
    Map<String, Object> rule = new HashMap<>();
    rule.put("name", "GAMBLING_MCC_" + UUID.randomUUID().toString().substring(0, 8));
    rule.put("description", "Alerta para MCC de gambling");
    rule.put("ruleKey", "GAMBLING_MCC_" + System.currentTimeMillis());
    rule.put("priority", 70);
    rule.put("status", "DRAFT");

    Map<String, Object> condition = new HashMap<>();
    condition.put("fieldName", "mcc");
    condition.put("operator", "MCC_GAMBLING");
    condition.put("value", "true");

    rule.put("conditions", List.of(condition));
    return rule;
  }

  private Map<String, Object> createNewDeviceRule() {
    Map<String, Object> rule = new HashMap<>();
    rule.put("name", "NEW_DEVICE_" + UUID.randomUUID().toString().substring(0, 8));
    rule.put("description", "Alerta para device novo");
    rule.put("ruleKey", "NEW_DEVICE_" + System.currentTimeMillis());
    rule.put("priority", 60);
    rule.put("status", "DRAFT");

    Map<String, Object> condition = new HashMap<>();
    condition.put("fieldName", "isNewDevice");
    condition.put("operator", "IS_TRUE");
    condition.put("value", "true");

    rule.put("conditions", List.of(condition));
    return rule;
  }

  private Map<String, Object> createRuleWithInvalidOperator() {
    Map<String, Object> rule = new HashMap<>();
    rule.put("name", "INVALID_RULE");
    rule.put("description", "Regra com operador inválido");
    rule.put("ruleKey", "INVALID_" + System.currentTimeMillis());

    Map<String, Object> condition = new HashMap<>();
    condition.put("fieldName", "amount");
    condition.put("operator", "INVALID_OPERATOR_XYZ");
    condition.put("value", "100");

    rule.put("conditions", List.of(condition));
    return rule;
  }

  private Map<String, Object> createComplexRuleWithMultipleConditions() {
    Map<String, Object> rule = new HashMap<>();
    rule.put("name", "COMPLEX_MULTI_" + UUID.randomUUID().toString().substring(0, 8));
    rule.put("description", "Regra complexa com múltiplas condições");
    rule.put("ruleKey", "COMPLEX_MULTI_" + System.currentTimeMillis());
    rule.put("priority", 100);
    rule.put("status", "DRAFT");

    List<Map<String, Object>> conditions = new ArrayList<>();

    // Condição 1: Valor alto
    Map<String, Object> cond1 = new HashMap<>();
    cond1.put("fieldName", "amount");
    cond1.put("operator", "GT");
    cond1.put("value", "5000");
    conditions.add(cond1);

    // Condição 2: País de alto risco
    Map<String, Object> cond2 = new HashMap<>();
    cond2.put("fieldName", "merchantCountryCode");
    cond2.put("operator", "IN");
    cond2.put("value", List.of("NG", "KP"));
    conditions.add(cond2);

    // Condição 3: Device novo
    Map<String, Object> cond3 = new HashMap<>();
    cond3.put("fieldName", "isNewDevice");
    cond3.put("operator", "IS_TRUE");
    cond3.put("value", "true");
    conditions.add(cond3);

    rule.put("conditions", conditions);
    rule.put("logicOperator", "AND");
    return rule;
  }

  private Map<String, Object> createNestedGroupRule() {
    Map<String, Object> rule = new HashMap<>();
    rule.put("name", "NESTED_GROUP_" + UUID.randomUUID().toString().substring(0, 8));
    rule.put("description", "Regra com grupos aninhados");
    rule.put("ruleKey", "NESTED_" + System.currentTimeMillis());
    rule.put("priority", 95);
    rule.put("status", "DRAFT");

    // Grupo principal (AND)
    Map<String, Object> rootGroup = new HashMap<>();
    rootGroup.put("logicOperator", "AND");

    // Sub-grupo 1 (OR)
    Map<String, Object> subGroup1 = new HashMap<>();
    subGroup1.put("logicOperator", "OR");
    subGroup1.put(
        "conditions",
        List.of(
            Map.of("fieldName", "amount", "operator", "GT", "value", "10000"),
            Map.of(
                "fieldName",
                "merchantCountryCode",
                "operator",
                "IN",
                "value",
                List.of("NG", "KP"))));

    // Sub-grupo 2 (AND)
    Map<String, Object> subGroup2 = new HashMap<>();
    subGroup2.put("logicOperator", "AND");
    subGroup2.put(
        "conditions",
        List.of(
            Map.of("fieldName", "isNewDevice", "operator", "IS_TRUE", "value", "true"),
            Map.of("fieldName", "mcc", "operator", "MCC_HIGH_RISK", "value", "true")));

    rootGroup.put("children", List.of(subGroup1, subGroup2));
    rule.put("rootGroup", rootGroup);

    return rule;
  }

  private Map<String, Object> createNeo4jRule() {
    Map<String, Object> rule = new HashMap<>();
    rule.put("name", "NEO4J_FRAUD_RING_" + UUID.randomUUID().toString().substring(0, 8));
    rule.put("description", "Detecta fraud ring via Neo4j");
    rule.put("ruleKey", "NEO4J_" + System.currentTimeMillis());
    rule.put("priority", 100);
    rule.put("status", "DRAFT");

    Map<String, Object> condition = new HashMap<>();
    condition.put("fieldName", "accountId");
    condition.put("operator", "NEO4J_FRAUD_RING_DETECTION");
    condition.put("value", "3"); // min ring size

    rule.put("conditions", List.of(condition));
    return rule;
  }

  private Map<String, Object> createVelocityComplexRule() {
    Map<String, Object> rule = new HashMap<>();
    rule.put("name", "VELOCITY_COMPLEX_" + UUID.randomUUID().toString().substring(0, 8));
    rule.put("description", "Regra de velocity complexa");
    rule.put("ruleKey", "VELOCITY_COMPLEX_" + System.currentTimeMillis());
    rule.put("priority", 85);
    rule.put("status", "DRAFT");

    List<Map<String, Object>> conditions = new ArrayList<>();

    // Velocity count
    Map<String, Object> cond1 = new HashMap<>();
    cond1.put("fieldName", "transactionCount");
    cond1.put("operator", "VELOCITY_COUNT_GT");
    cond1.put("value", "10");
    cond1.put("timeWindow", "24h");
    conditions.add(cond1);

    // Velocity sum
    Map<String, Object> cond2 = new HashMap<>();
    cond2.put("fieldName", "totalAmount");
    cond2.put("operator", "VELOCITY_SUM_GT");
    cond2.put("value", "50000");
    cond2.put("timeWindow", "24h");
    conditions.add(cond2);

    rule.put("conditions", conditions);
    rule.put("logicOperator", "OR");
    return rule;
  }
}
