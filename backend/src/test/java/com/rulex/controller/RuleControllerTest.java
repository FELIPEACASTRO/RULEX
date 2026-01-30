package com.rulex.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.patch;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.dto.RuleConditionDTO;
import com.rulex.dto.RuleConfigurationDTO;
import com.rulex.service.RuleConfigurationService;
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

@SpringBootTest(classes = RuleControllerTest.TestApplication.class)
@AutoConfigureMockMvc(addFilters = false)
class RuleControllerTest {

  @SpringBootConfiguration
  @EnableAutoConfiguration(
      exclude = {
        DataSourceAutoConfiguration.class,
        FlywayAutoConfiguration.class,
        HibernateJpaAutoConfiguration.class
      })
  @Import(RuleController.class)
  static class TestApplication {}

  @Autowired private MockMvc mockMvc;
  @Autowired private ObjectMapper objectMapper;

  @SuppressWarnings("removal")
  @MockBean private RuleConfigurationService ruleConfigurationService;

  @Test
  void listRulesReturnsOk() throws Exception {
    Page<RuleConfigurationDTO> page = new PageImpl<>(List.of());
    when(ruleConfigurationService.listRules(any())).thenReturn(page);

    mockMvc.perform(get("/rules").param("page", "0").param("size", "20"))
        .andExpect(status().isOk());
  }

  @Test
  void createRuleReturnsCreated() throws Exception {
    RuleConfigurationDTO dto = minimalRuleConfiguration();
    when(ruleConfigurationService.createRule(any(RuleConfigurationDTO.class))).thenReturn(dto);

    mockMvc
        .perform(
            post("/rules")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(dto)))
        .andExpect(status().isCreated());
  }

  @Test
  void updateRuleReturnsOk() throws Exception {
    RuleConfigurationDTO dto = minimalRuleConfiguration();
    when(ruleConfigurationService.updateRule(eq(1L), any(RuleConfigurationDTO.class)))
        .thenReturn(dto);

    mockMvc
        .perform(
            put("/rules/1")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(dto)))
        .andExpect(status().isOk());
  }

  @Test
  void deleteRuleReturnsNoContent() throws Exception {
    mockMvc.perform(delete("/rules/1")).andExpect(status().isNoContent());

    verify(ruleConfigurationService).deleteRule(1L);
  }

  @Test
  void toggleRuleReturnsOk() throws Exception {
    RuleConfigurationDTO dto = minimalRuleConfiguration();
    when(ruleConfigurationService.toggleRule(eq(1L))).thenReturn(dto);

    mockMvc.perform(patch("/rules/1/toggle")).andExpect(status().isOk());
  }

  @Test
  void listEnabledRulesReturnsOk() throws Exception {
    when(ruleConfigurationService.listRulesByEnabled(eq(true))).thenReturn(List.of());

    mockMvc.perform(get("/rules/enabled/true")).andExpect(status().isOk());
  }

  private static RuleConfigurationDTO minimalRuleConfiguration() {
    return RuleConfigurationDTO.builder()
        .ruleName("Rule 1")
        .ruleType("SECURITY")
        .threshold(10)
        .weight(50)
        .enabled(true)
        .classification("FRAUD")
        .conditions(List.of(RuleConditionDTO.builder().field("amount").operator("GT").value("100").build()))
        .logicOperator("AND")
        .build();
  }
}
