package com.rulex.controller;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.rulex.dto.MetricsDTO;
import com.rulex.service.MetricsService;
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

@SpringBootTest(classes = MetricsControllerTest.TestApplication.class)
@AutoConfigureMockMvc(addFilters = false)
class MetricsControllerTest {

  @SpringBootConfiguration
  @EnableAutoConfiguration(
      exclude = {
        DataSourceAutoConfiguration.class,
        FlywayAutoConfiguration.class,
        HibernateJpaAutoConfiguration.class
      })
  @Import(MetricsController.class)
  static class TestApplication {}

  @Autowired private MockMvc mockMvc;

  @SuppressWarnings("removal")
  @MockBean private MetricsService metricsService;

  @Test
  void getMetricsReturnsOk() throws Exception {
    when(metricsService.getMetrics(eq("7d"))).thenReturn(new MetricsDTO());

    mockMvc.perform(get("/metrics").param("period", "7d"))
        .andExpect(status().isOk());

    verify(metricsService).getMetrics("7d");
  }

  @Test
  void getMetricsByMccReturnsOk() throws Exception {
    when(metricsService.getMetricsByMcc(eq("30d"))).thenReturn(java.util.Map.of());

    mockMvc.perform(get("/metrics/mcc").param("period", "30d"))
        .andExpect(status().isOk());

    verify(metricsService).getMetricsByMcc("30d");
  }

  @Test
  void getMetricsByMerchantReturnsOk() throws Exception {
    when(metricsService.getMetricsByMerchant(eq("7d"))).thenReturn(java.util.Map.of());

    mockMvc.perform(get("/metrics/merchant").param("period", "7d"))
        .andExpect(status().isOk());

    verify(metricsService).getMetricsByMerchant("7d");
  }

  @Test
  void getMetricsTimelineReturnsOk() throws Exception {
    when(metricsService.getMetricsTimeline(eq("day"))).thenReturn(java.util.Map.of());

    mockMvc.perform(get("/metrics/timeline").param("granularity", "day"))
        .andExpect(status().isOk());

    verify(metricsService).getMetricsTimeline("day");
  }
}
