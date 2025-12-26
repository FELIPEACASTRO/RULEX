package com.rulex.security;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.httpBasic;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.patch;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.rulex.dto.EvaluateResponse;
import com.rulex.dto.RuleConfigurationDTO;
import com.rulex.service.RuleConfigurationService;
import com.rulex.service.RuleEngineService;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.MediaType;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.servlet.MockMvc;
import rulex.testconfig.SecurityRbacTestApplication;

/**
 * Security RBAC tests using WebMvcTest slice with security enabled. Tests HTTP Basic authentication
 * with ADMIN and ANALYST roles.
 */
@SpringBootTest(
    classes = SecurityRbacTestApplication.class,
    webEnvironment = SpringBootTest.WebEnvironment.MOCK)
@AutoConfigureMockMvc
@DirtiesContext(classMode = DirtiesContext.ClassMode.AFTER_CLASS)
@TestPropertySource(
    properties = {
      "rulex.security.enabled=true",
      "rulex.security.admin.username=admin",
      "rulex.security.admin.password=adminpw",
      "rulex.security.analyst.username=analyst",
      "rulex.security.analyst.password=analystpw"
    })
class SecurityRbacTest {

  @Autowired private MockMvc mockMvc;

  @MockBean private RuleConfigurationService ruleConfigurationService;

  @MockBean private RuleEngineService ruleEngineService;

  @Test
  void evaluate_isPublic() throws Exception {
    when(ruleEngineService.evaluateRaw(any(), any(), any())).thenReturn(new EvaluateResponse());

    mockMvc
        .perform(post("/evaluate").contentType(MediaType.APPLICATION_JSON).content("{}"))
        .andExpect(status().isOk());
  }

  @Test
  void rulesList_requiresAuth() throws Exception {
    mockMvc.perform(get("/rules")).andExpect(status().isUnauthorized());
  }

  @Test
  void rulesList_allowsAnalyst() throws Exception {
    Page<RuleConfigurationDTO> emptyPage =
        new PageImpl<>(new ArrayList<>(), PageRequest.of(0, 20), 0);
    when(ruleConfigurationService.listRules(any())).thenReturn(emptyPage);

    mockMvc
        .perform(get("/rules").with(httpBasic("analyst", "analystpw")))
        .andExpect(status().isOk());
  }

  @Test
  void rulesToggle_forbidsAnalyst() throws Exception {
    mockMvc
        .perform(
            patch("/rules/1/toggle")
                .contentType(MediaType.APPLICATION_JSON)
                .content("{\"enabled\":true}")
                .with(httpBasic("analyst", "analystpw")))
        .andExpect(status().isForbidden());
  }

  @Test
  void rulesToggle_allowsAdmin() throws Exception {
    when(ruleConfigurationService.setRuleEnabled(anyLong(), anyBoolean()))
        .thenReturn(
            RuleConfigurationDTO.builder()
                .id(1L)
                .ruleName("R")
                .ruleType("SECURITY")
                .threshold(0)
                .weight(0)
                .enabled(true)
                .classification("APPROVED")
                .conditions(List.of())
                .logicOperator("AND")
                .build());

    mockMvc
        .perform(
            patch("/rules/1/toggle")
                .contentType(MediaType.APPLICATION_JSON)
                .content("{\"enabled\":true}")
                .with(httpBasic("admin", "adminpw")))
        .andExpect(status().isOk());
  }
}
