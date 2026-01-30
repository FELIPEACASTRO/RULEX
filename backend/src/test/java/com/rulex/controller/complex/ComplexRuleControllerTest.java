package com.rulex.controller.complex;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.dto.complex.ConditionGroupDTO;
import com.rulex.dto.complex.ConditionGroupDTO.LogicOperatorType;
import com.rulex.dto.complex.ExpressionDTO;
import com.rulex.dto.complex.ConditionDTO.ValueType;
import com.rulex.service.complex.ComplexRuleEvaluator;
import com.rulex.service.complex.ComplexRuleService;
import java.util.List;
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

@SpringBootTest(classes = ComplexRuleControllerTest.TestApplication.class)
@AutoConfigureMockMvc(addFilters = false)
class ComplexRuleControllerTest {

  @SpringBootConfiguration
  @EnableAutoConfiguration(
      exclude = {
        DataSourceAutoConfiguration.class,
        FlywayAutoConfiguration.class,
        HibernateJpaAutoConfiguration.class
      })
  @Import(ComplexRuleController.class)
  static class TestApplication {}

  @Autowired private MockMvc mockMvc;
  @Autowired private ObjectMapper objectMapper;

  @SuppressWarnings("removal")
  @MockBean private ComplexRuleService complexRuleService;

  @SuppressWarnings("removal")
  @MockBean private ComplexRuleEvaluator complexRuleEvaluator;

  @Test
  void saveConditionGroupReturnsOk() throws Exception {
    UUID ruleVersionId = UUID.randomUUID();
    ConditionGroupDTO group = ConditionGroupDTO.builder().logicOperator(LogicOperatorType.AND).build();
    when(complexRuleService.saveConditionGroup(eq(ruleVersionId), any(ConditionGroupDTO.class)))
        .thenReturn(group);

    mockMvc
        .perform(
            post("/v1/complex-rules/{ruleVersionId}/conditions", ruleVersionId)
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(group)))
        .andExpect(status().isOk());
  }

  @Test
  void getConditionGroupNotFoundReturns404() throws Exception {
    UUID ruleVersionId = UUID.randomUUID();
    when(complexRuleService.getConditionGroup(eq(ruleVersionId))).thenReturn(null);

    mockMvc.perform(get("/v1/complex-rules/{ruleVersionId}/conditions", ruleVersionId))
        .andExpect(status().isNotFound());
  }

  @Test
  void saveExpressionsReturnsOk() throws Exception {
    UUID ruleVersionId = UUID.randomUUID();
    ExpressionDTO expression =
        ExpressionDTO.builder().name("expr").expression("1+1").resultType(ValueType.NUMBER).build();
    when(complexRuleService.saveExpressions(eq(ruleVersionId), any())).thenReturn(List.of(expression));

    mockMvc
        .perform(
            post("/v1/complex-rules/{ruleVersionId}/expressions", ruleVersionId)
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(List.of(expression))))
        .andExpect(status().isOk());
  }

  @Test
  void deleteRuleStructureReturnsNoContent() throws Exception {
    UUID ruleVersionId = UUID.randomUUID();
    mockMvc.perform(delete("/v1/complex-rules/{ruleVersionId}", ruleVersionId))
        .andExpect(status().isNoContent());
  }
}
