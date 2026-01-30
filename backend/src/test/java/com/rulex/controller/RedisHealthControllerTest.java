package com.rulex.controller;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.rulex.service.RedisVelocityCacheService;
import com.rulex.service.VelocityServiceFacade;
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
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.test.web.servlet.MockMvc;

@SpringBootTest(classes = RedisHealthControllerTest.TestApplication.class)
@AutoConfigureMockMvc(addFilters = false)
class RedisHealthControllerTest {

  @SpringBootConfiguration
  @EnableAutoConfiguration(
      exclude = {
        DataSourceAutoConfiguration.class,
        FlywayAutoConfiguration.class,
        HibernateJpaAutoConfiguration.class
      })
  @Import(RedisHealthController.class)
  static class TestApplication {}

  @Autowired private MockMvc mockMvc;

  @SuppressWarnings("removal")
  @MockBean private StringRedisTemplate stringRedisTemplate;

  @SuppressWarnings("removal")
  @MockBean private VelocityServiceFacade velocityServiceFacade;

  @SuppressWarnings("removal")
  @MockBean private RedisVelocityCacheService redisVelocityCacheService;

  @Test
  void getCacheStatsReturnsOkWhenNoCacheManager() throws Exception {
    mockMvc.perform(get("/admin/redis/cache-stats")).andExpect(status().isOk());
  }
}
