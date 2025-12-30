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

import com.rulex.config.SecurityConfig;
import com.rulex.controller.EvaluateController;
import com.rulex.controller.RuleController;
import com.rulex.dto.EvaluateResponse;
import com.rulex.dto.RuleConfigurationDTO;
import com.rulex.service.RuleConfigurationService;
import com.rulex.service.RuleEngineService;
import java.util.ArrayList;
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
import org.springframework.data.domain.PageRequest;
import org.springframework.http.MediaType;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.servlet.MockMvc;

@SpringBootTest(classes = SecurityRbacTest.TestApplication.class)
@AutoConfigureMockMvc
@TestPropertySource(
    properties = {
      "rulex.security.enabled=true",
      "rulex.security.admin.username=admin",
      "rulex.security.admin.password=adminpw",
      "rulex.security.analyst.username=analyst",
      "rulex.security.analyst.password=analystpw",
      "server.servlet.context-path=/api"
    })
class SecurityRbacTest {

  @SpringBootConfiguration
  @EnableAutoConfiguration(
      exclude = {
        DataSourceAutoConfiguration.class,
        FlywayAutoConfiguration.class,
        HibernateJpaAutoConfiguration.class
      })
  @Import({SecurityConfig.class, RuleController.class, EvaluateController.class})
  static class TestApplication {}

  @Autowired private MockMvc mockMvc;

  @SuppressWarnings("removal")
  @MockBean
  private RuleConfigurationService ruleConfigurationService;

  @SuppressWarnings("removal")
  @MockBean
  private RuleEngineService ruleEngineService;

  @Test
  void actuatorHealth_isPublicBySecurityMatcher() throws Exception {
    mockMvc.perform(get("/api/actuator/health").contextPath("/api")).andExpect(status().isOk());
  }

  @Test
  void evaluate_isPublic() throws Exception {
    when(ruleEngineService.evaluateRaw(any(), any(), any())).thenReturn(new EvaluateResponse());

    mockMvc
        .perform(
            post("/api/evaluate")
                .contextPath("/api")
                .contentType(MediaType.APPLICATION_JSON)
                .content("{}"))
        .andExpect(status().isOk());
  }

  @Test
  void rulesList_requiresAuth() throws Exception {
    mockMvc.perform(get("/api/rules").contextPath("/api")).andExpect(status().isUnauthorized());
  }

  @Test
  void rulesList_allowsAnalyst() throws Exception {
    Page<RuleConfigurationDTO> emptyPage =
        new PageImpl<>(new ArrayList<>(), PageRequest.of(0, 20), 0);
    when(ruleConfigurationService.listRules(any())).thenReturn(emptyPage);

    mockMvc
        .perform(get("/api/rules").contextPath("/api").with(httpBasic("analyst", "analystpw")))
        .andExpect(status().isOk());
  }

  @Test
  void rulesToggle_forbidsAnalyst() throws Exception {
    mockMvc
        .perform(
            patch("/api/rules/1/toggle")
                .contextPath("/api")
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
            patch("/api/rules/1/toggle")
                .contextPath("/api")
                .contentType(MediaType.APPLICATION_JSON)
                .content("{\"enabled\":true}")
                .with(httpBasic("admin", "adminpw")))
        .andExpect(status().isOk());
  }
}
