package com.rulex.controller;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.rulex.core.metrics.port.RuleMetricsInputPort;
import com.rulex.core.metrics.usecase.RuleMetricsUseCase.MetricsDashboard;
import com.rulex.core.metrics.usecase.RuleMetricsUseCase.RuleMetricsSummary;
import java.time.LocalDate;
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
import org.springframework.test.web.servlet.MockMvc;

@SpringBootTest(classes = RuleMetricsControllerTest.TestApplication.class)
@AutoConfigureMockMvc(addFilters = false)
class RuleMetricsControllerTest {

  @SpringBootConfiguration
  @EnableAutoConfiguration(
      exclude = {
        DataSourceAutoConfiguration.class,
        FlywayAutoConfiguration.class,
        HibernateJpaAutoConfiguration.class
      })
  @Import(RuleMetricsController.class)
  static class TestApplication {}

  @Autowired private MockMvc mockMvc;

  @SuppressWarnings("removal")
  @MockBean private RuleMetricsInputPort metricsService;

  @Test
  void getDashboardReturnsOk() throws Exception {
    LocalDate startDate = LocalDate.of(2024, 1, 1);
    LocalDate endDate = LocalDate.of(2024, 1, 31);
    MetricsDashboard dashboard =
        MetricsDashboard.builder().startDate(startDate).endDate(endDate).build();

    when(metricsService.getDashboard(eq(startDate), eq(endDate))).thenReturn(dashboard);

    mockMvc
        .perform(
            get("/rules/metrics/dashboard")
                .param("startDate", "2024-01-01")
                .param("endDate", "2024-01-31"))
        .andExpect(status().isOk());

    verify(metricsService).getDashboard(startDate, endDate);
  }

  @Test
  void getRuleMetricsReturnsOk() throws Exception {
    LocalDate startDate = LocalDate.of(2024, 1, 1);
    LocalDate endDate = LocalDate.of(2024, 1, 31);
    RuleMetricsSummary summary = RuleMetricsSummary.builder().ruleId(1L).build();

    when(metricsService.getRuleMetrics(eq(1L), eq(startDate), eq(endDate))).thenReturn(summary);

    mockMvc
        .perform(
            get("/rules/metrics/1")
                .param("startDate", "2024-01-01")
                .param("endDate", "2024-01-31"))
        .andExpect(status().isOk());

    verify(metricsService).getRuleMetrics(1L, startDate, endDate);
  }

  @Test
  void getAllRulesMetricsReturnsOk() throws Exception {
    LocalDate startDate = LocalDate.of(2024, 2, 1);
    LocalDate endDate = LocalDate.of(2024, 2, 28);
    when(metricsService.getAllRulesMetrics(eq(startDate), eq(endDate)))
        .thenReturn(List.of(RuleMetricsSummary.builder().ruleId(1L).build()));

    mockMvc
        .perform(
            get("/rules/metrics/all")
                .param("startDate", "2024-02-01")
                .param("endDate", "2024-02-28"))
        .andExpect(status().isOk());

    verify(metricsService).getAllRulesMetrics(startDate, endDate);
  }

  @Test
  void recordFalsePositiveReturnsOk() throws Exception {
    mockMvc.perform(post("/rules/metrics/10/false-positive")).andExpect(status().isOk());

    verify(metricsService).recordFalsePositive(10L);
  }

  @Test
  void recordTruePositiveReturnsOk() throws Exception {
    mockMvc.perform(post("/rules/metrics/11/true-positive")).andExpect(status().isOk());

    verify(metricsService).recordTruePositive(11L);
  }
}
