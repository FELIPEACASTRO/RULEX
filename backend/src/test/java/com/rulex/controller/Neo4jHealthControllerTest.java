package com.rulex.controller;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.rulex.service.Neo4jGraphService;
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

@SpringBootTest(classes = Neo4jHealthControllerTest.TestApplication.class)
@AutoConfigureMockMvc(addFilters = false)
class Neo4jHealthControllerTest {

  @SpringBootConfiguration
  @EnableAutoConfiguration(
      exclude = {
        DataSourceAutoConfiguration.class,
        FlywayAutoConfiguration.class,
        HibernateJpaAutoConfiguration.class
      })
  @Import(Neo4jHealthController.class)
  static class TestApplication {}

  @Autowired private MockMvc mockMvc;

  @SuppressWarnings("removal")
  @MockBean private Neo4jGraphService neo4jGraphService;

  @Test
  void getStatusReturnsOk() throws Exception {
    when(neo4jGraphService.isCurrentlyAvailable()).thenReturn(true);
    when(neo4jGraphService.isAvailable()).thenReturn(false);

    mockMvc.perform(get("/admin/neo4j/status"))
        .andExpect(status().isOk());
  }

  @Test
  void getGraphStatsReturnsOkWhenUnavailable() throws Exception {
    when(neo4jGraphService.isAvailable()).thenReturn(false);

    mockMvc.perform(get("/admin/neo4j/graph-stats"))
        .andExpect(status().isOk());
  }

  @Test
  void testWriteReturnsOkWhenUnavailable() throws Exception {
    when(neo4jGraphService.isAvailable()).thenReturn(false);

    mockMvc.perform(post("/admin/neo4j/test-write"))
        .andExpect(status().isOk());
  }

  @Test
  void getGdsStatusReturnsOkWhenUnavailable() throws Exception {
    when(neo4jGraphService.isAvailable()).thenReturn(false);

    mockMvc.perform(get("/admin/neo4j/gds-status"))
        .andExpect(status().isOk());
  }
}
