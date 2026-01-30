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
import com.rulex.dto.complex.ComplexRuleDTO;
import com.rulex.dto.complex.ConditionGroupDTO;
import com.rulex.dto.complex.ConditionGroupDTO.LogicOperatorType;
import com.rulex.service.complex.ComplexRuleCrudService;
import java.util.List;
import java.util.Map;
import java.util.UUID;
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

@SpringBootTest(classes = ComplexRuleCrudControllerTest.TestApplication.class)
@AutoConfigureMockMvc(addFilters = false)
class ComplexRuleCrudControllerTest {

  @SpringBootConfiguration
  @EnableAutoConfiguration(
      exclude = {
        DataSourceAutoConfiguration.class,
        FlywayAutoConfiguration.class,
        HibernateJpaAutoConfiguration.class
      })
  @Import(ComplexRuleCrudController.class)
  static class TestApplication {}

  @Autowired private MockMvc mockMvc;
  @Autowired private ObjectMapper objectMapper;

  @SuppressWarnings("removal")
  @MockBean private ComplexRuleCrudService complexRuleCrudService;

  @Test
  void listAllReturnsOk() throws Exception {
    when(complexRuleCrudService.listAll()).thenReturn(List.of());

    mockMvc.perform(get("/complex-rules")).andExpect(status().isOk());

    verify(complexRuleCrudService).listAll();
  }

  @Test
  void getByIdNotFoundReturns404() throws Exception {
    UUID id = UUID.randomUUID();
    when(complexRuleCrudService.getById(eq(id))).thenReturn(null);

    mockMvc.perform(get("/complex-rules/{id}", id)).andExpect(status().isNotFound());
  }

  @Test
  void createReturnsCreated() throws Exception {
    ComplexRuleDTO rule = minimalComplexRule();
    when(complexRuleCrudService.create(any(ComplexRuleDTO.class))).thenReturn(rule);

    mockMvc
        .perform(
            post("/complex-rules")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(rule)))
        .andExpect(status().isCreated());

    verify(complexRuleCrudService).create(any(ComplexRuleDTO.class));
  }

  @Test
  void updateNotFoundReturns404() throws Exception {
    UUID id = UUID.randomUUID();
    ComplexRuleDTO rule = minimalComplexRule();
    when(complexRuleCrudService.update(eq(id), any(ComplexRuleDTO.class))).thenReturn(null);

    mockMvc
        .perform(
            put("/complex-rules/{id}", id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(rule)))
        .andExpect(status().isNotFound());
  }

  @Test
  void deleteNotFoundReturns404() throws Exception {
    UUID id = UUID.randomUUID();
    when(complexRuleCrudService.delete(eq(id))).thenReturn(false);

    mockMvc.perform(delete("/complex-rules/{id}", id)).andExpect(status().isNotFound());
  }

  @Test
  void toggleNotFoundReturns404() throws Exception {
    UUID id = UUID.randomUUID();
    when(complexRuleCrudService.setEnabled(eq(id), eq(true))).thenReturn(null);

    mockMvc
        .perform(patch("/complex-rules/{id}/toggle", id).param("enabled", "true"))
        .andExpect(status().isNotFound());
  }

  @Test
  void duplicateNotFoundReturns404() throws Exception {
    UUID id = UUID.randomUUID();
    when(complexRuleCrudService.duplicate(eq(id), eq("NEW_KEY"))).thenReturn(null);

    mockMvc
        .perform(post("/complex-rules/{id}/duplicate", id).param("newKey", "NEW_KEY"))
        .andExpect(status().isNotFound());
  }

  @Test
  void validateReturnsOk() throws Exception {
    ComplexRuleDTO rule = minimalComplexRule();
    when(complexRuleCrudService.validate(any(ComplexRuleDTO.class)))
        .thenReturn(Map.of("valid", true));

    mockMvc
        .perform(
            post("/complex-rules/validate")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(rule)))
        .andExpect(status().isOk());
  }

  private static ComplexRuleDTO minimalComplexRule() {
    ConditionGroupDTO group = ConditionGroupDTO.builder().logicOperator(LogicOperatorType.AND).build();
    return ComplexRuleDTO.builder()
        .key("RULE_KEY")
        .title("Rule Title")
        .status(ComplexRuleDTO.RuleStatusType.DRAFT)
        .decision(ComplexRuleDTO.DecisionType.FRAUDE)
        .rootConditionGroup(group)
        .build();
  }
}
