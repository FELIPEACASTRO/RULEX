package com.rulex.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.rulex.entity.complex.OperatorStatus;
import com.rulex.service.complex.evaluator.OperatorEvaluator;
import com.rulex.service.complex.evaluator.OperatorEvaluatorRegistry;
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
import org.springframework.test.web.servlet.MockMvc;

@SpringBootTest(classes = OperatorStatusControllerTest.TestApplication.class)
@AutoConfigureMockMvc(addFilters = false)
class OperatorStatusControllerTest {

  @SpringBootConfiguration
  @EnableAutoConfiguration(
      exclude = {
        DataSourceAutoConfiguration.class,
        FlywayAutoConfiguration.class,
        HibernateJpaAutoConfiguration.class
      })
  @Import(OperatorStatusController.class)
  static class TestApplication {}

  @Autowired private MockMvc mockMvc;

  @SuppressWarnings("removal")
  @MockBean private OperatorEvaluatorRegistry registry;

  @Test
  void getOperatorStatusReturnsOk() throws Exception {
    when(registry.getStatus(any())).thenReturn(OperatorStatus.STABLE);
    when(registry.isImplemented(any())).thenReturn(true);
    when(registry.getOperatorsByEvaluator()).thenReturn(Map.of());

    mockMvc.perform(get("/operators/status")).andExpect(status().isOk());
  }

  @Test
  void getOperatorDetailReturnsNotFoundForInvalidOperator() throws Exception {
    mockMvc.perform(get("/operators/status/invalid_op")).andExpect(status().isNotFound());
  }

  @Test
  void listOperatorsReturnsOk() throws Exception {
    OperatorEvaluator evaluator = org.mockito.Mockito.mock(OperatorEvaluator.class);
    when(evaluator.getCategory()).thenReturn("BASIC");
    when(registry.getStatus(any())).thenReturn(OperatorStatus.STABLE);
    when(registry.isImplemented(any())).thenReturn(true);
    when(registry.getEvaluator(any())).thenReturn(evaluator);

    mockMvc.perform(get("/operators/list")).andExpect(status().isOk());
  }
}
