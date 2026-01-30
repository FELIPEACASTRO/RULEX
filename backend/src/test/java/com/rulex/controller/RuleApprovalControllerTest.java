package com.rulex.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.dto.RuleConditionDTO;
import com.rulex.dto.RuleConfigurationDTO;
import com.rulex.entity.RuleApproval;
import com.rulex.core.approval.port.RuleApprovalInputPort;
import com.rulex.core.approval.usecase.RuleApprovalUseCase.ApprovalResult;
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
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

@SpringBootTest(classes = RuleApprovalControllerTest.TestApplication.class)
@AutoConfigureMockMvc(addFilters = false)
class RuleApprovalControllerTest {

  @SpringBootConfiguration
  @EnableAutoConfiguration(
      exclude = {
        DataSourceAutoConfiguration.class,
        FlywayAutoConfiguration.class,
        HibernateJpaAutoConfiguration.class
      })
  @Import(RuleApprovalController.class)
  static class TestApplication {}

  @Autowired private MockMvc mockMvc;
  @Autowired private ObjectMapper objectMapper;

  @SuppressWarnings("removal")
  @MockBean private RuleApprovalInputPort approvalService;

  @Test
  void requestCreateApprovalReturnsCreated() throws Exception {
    RuleApproval approval = RuleApproval.builder().id(1L).ruleId(0L).ruleName("Rule 1").build();
    when(approvalService.requestCreateApproval(any(RuleConfigurationDTO.class))).thenReturn(approval);

    mockMvc
        .perform(
            post("/rules/approvals/create")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(minimalRuleConfiguration())))
        .andExpect(status().isCreated());
  }

  @Test
  void approveReturnsOk() throws Exception {
    ApprovalResult result = ApprovalResult.builder().success(true).build();
    when(approvalService.approve(eq(10L), eq("ok"))).thenReturn(result);

    mockMvc
        .perform(
            post("/rules/approvals/10/approve")
                .contentType(MediaType.APPLICATION_JSON)
                .content("{\"comments\":\"ok\"}"))
        .andExpect(status().isOk());
  }

  @Test
  void listPendingApprovalsReturnsOk() throws Exception {
    when(approvalService.listPendingApprovals()).thenReturn(List.of());

    mockMvc.perform(get("/rules/approvals/pending")).andExpect(status().isOk());
  }

  private static RuleConfigurationDTO minimalRuleConfiguration() {
    return RuleConfigurationDTO.builder()
        .ruleName("Rule 1")
        .ruleType("SECURITY")
        .threshold(10)
        .weight(50)
        .enabled(true)
        .classification("FRAUD")
        .conditions(
            List.of(RuleConditionDTO.builder().field("amount").operator("GT").value("100").build()))
        .logicOperator("AND")
        .build();
  }
}
