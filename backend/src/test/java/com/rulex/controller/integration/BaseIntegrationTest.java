package com.rulex.controller.integration;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.rulex.config.TestContainersConfig;
import org.junit.jupiter.api.BeforeEach;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;
import org.springframework.http.MediaType;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;

/**
 * 🔥 Base para testes integrados de APIs Configura MockMvc, ObjectMapper e Testcontainers com
 * PostgreSQL
 */
@SpringBootTest
@AutoConfigureMockMvc
@ActiveProfiles("test")
@Import(TestContainersConfig.class)
public abstract class BaseIntegrationTest {

  @Autowired protected MockMvc mockMvc;

  protected ObjectMapper objectMapper;

  @BeforeEach
  void setUp() {
    objectMapper = new ObjectMapper();
    objectMapper.registerModule(new JavaTimeModule());
  }

  protected String toJson(Object obj) throws Exception {
    return objectMapper.writeValueAsString(obj);
  }

  protected <T> T fromJson(String json, Class<T> clazz) throws Exception {
    return objectMapper.readValue(json, clazz);
  }

  protected ResultActions performGet(String url) throws Exception {
    return mockMvc.perform(get(url).contentType(MediaType.APPLICATION_JSON));
  }

  protected ResultActions performPost(String url, Object body) throws Exception {
    return mockMvc.perform(post(url).contentType(MediaType.APPLICATION_JSON).content(toJson(body)));
  }

  protected ResultActions performPut(String url, Object body) throws Exception {
    return mockMvc.perform(put(url).contentType(MediaType.APPLICATION_JSON).content(toJson(body)));
  }

  protected ResultActions performDelete(String url) throws Exception {
    return mockMvc.perform(delete(url).contentType(MediaType.APPLICATION_JSON));
  }
}
