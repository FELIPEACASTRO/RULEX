package com.rulex.config.health;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.actuate.health.Health;
import org.springframework.boot.actuate.health.HealthIndicator;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

/**
 * Health Indicator para Redis.
 *
 * <p>Verifica se o serviço Redis está disponível e responsivo.
 */
@Component("redisHealth")
@RequiredArgsConstructor
@Slf4j
public class RedisHealthIndicator implements HealthIndicator {

  private final StringRedisTemplate redisTemplate;

  @Override
  public Health health() {
    long start = System.currentTimeMillis();
    try {
      RedisConnectionFactory factory = redisTemplate.getConnectionFactory();
      if (factory == null) {
        return Health.down()
            .withDetail("status", "no_connection_factory")
            .withDetail("service", "redis")
            .build();
      }

      String pong = factory.getConnection().ping();
      long latency = System.currentTimeMillis() - start;

      if ("PONG".equals(pong)) {
        log.debug("Redis health check OK - latency: {}ms", latency);
        return Health.up()
            .withDetail("latencyMs", latency)
            .withDetail("status", "connected")
            .withDetail("service", "redis")
            .withDetail("response", pong)
            .build();
      } else {
        log.warn("Redis health check FAILED - unexpected response: {}", pong);
        return Health.down()
            .withDetail("response", pong)
            .withDetail("status", "unexpected_response")
            .withDetail("service", "redis")
            .build();
      }
    } catch (Exception e) {
      long latency = System.currentTimeMillis() - start;
      log.error("Redis health check ERROR: {}", e.getMessage());
      return Health.down()
          .withDetail("error", e.getMessage())
          .withDetail("errorType", e.getClass().getSimpleName())
          .withDetail("status", "error")
          .withDetail("service", "redis")
          .withDetail("latencyMs", latency)
          .build();
    }
  }
}
