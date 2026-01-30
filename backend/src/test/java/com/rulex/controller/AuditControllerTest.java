package com.rulex.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.rulex.dto.AuditLogDTO;
import com.rulex.core.audit.port.AuditQueryInputPort;
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
import org.springframework.test.web.servlet.MockMvc;

@SpringBootTest(classes = AuditControllerTest.TestApplication.class)
@AutoConfigureMockMvc(addFilters = false)
class AuditControllerTest {

  @SpringBootConfiguration
  @EnableAutoConfiguration(
      exclude = {
        DataSourceAutoConfiguration.class,
        FlywayAutoConfiguration.class,
        HibernateJpaAutoConfiguration.class
      })
  @Import(AuditController.class)
  static class TestApplication {}

  @Autowired private MockMvc mockMvc;

  @SuppressWarnings("removal")
  @MockBean private AuditQueryInputPort auditQueryService;

  @Test
  void listAuditLogsReturnsOk() throws Exception {
    Page<AuditLogDTO> page = new PageImpl<>(List.of());
    when(auditQueryService.findAuditLogs(any(), any(), any(), any(), any())).thenReturn(page);

    mockMvc.perform(get("/audit")).andExpect(status().isOk());
  }

  @Test
  void getTransactionAuditLogsReturnsOk() throws Exception {
    Page<AuditLogDTO> page = new PageImpl<>(List.of());
    when(auditQueryService.findAuditLogsByTransactionId(any(), any())).thenReturn(page);

    mockMvc.perform(get("/audit/transaction/10")).andExpect(status().isOk());
  }
}
