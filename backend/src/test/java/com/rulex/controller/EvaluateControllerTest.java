package com.rulex.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.api.RawPayloadCaptureFilter;
import com.rulex.dto.EvaluateRequestDTO;
import com.rulex.dto.EvaluateResponse;
import com.rulex.core.engine.port.RuleEngineInputPort;
import java.time.LocalDateTime;
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
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

@SpringBootTest(classes = EvaluateControllerTest.TestApplication.class)
@AutoConfigureMockMvc(addFilters = false)
class EvaluateControllerTest {

  @SpringBootConfiguration
  @EnableAutoConfiguration(
      exclude = {
        DataSourceAutoConfiguration.class,
        FlywayAutoConfiguration.class,
        HibernateJpaAutoConfiguration.class
      })
  @Import(EvaluateController.class)
  static class TestApplication {}

  @Autowired private MockMvc mockMvc;
  @Autowired private ObjectMapper objectMapper;

  @SuppressWarnings("removal")
  @MockBean private RuleEngineInputPort ruleEngineService;

  @Test
  void evaluateReturnsOk() throws Exception {
    EvaluateRequestDTO request =
        EvaluateRequestDTO.builder().payload(Map.of("amount", 100)).build();
    EvaluateResponse response =
        EvaluateResponse.builder().classification("APPROVED").timestamp(LocalDateTime.now()).build();

    when(ruleEngineService.evaluateRaw(anyString(), any(), anyString())).thenReturn(response);

    mockMvc
        .perform(
            post("/evaluate")
                .requestAttr(RawPayloadCaptureFilter.RAW_BYTES_ATTR, new byte[] {1, 2, 3})
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
        .andExpect(status().isOk());
  }

  @Test
  void evaluateRawEmptyReturnsBadRequest() throws Exception {
    mockMvc
        .perform(post("/evaluate/raw").contentType(MediaType.APPLICATION_JSON).content(""))
        .andExpect(status().isBadRequest());
  }
}
