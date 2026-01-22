package com.rulex.config.health;

import com.rulex.service.Neo4jGraphService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.actuate.health.Health;
import org.springframework.boot.actuate.health.HealthIndicator;
import org.springframework.stereotype.Component;

/**
 * Health Indicator para Neo4j.
 *
 * <p>Verifica se o serviço Neo4j está disponível e responsivo.
 */
@Component("neo4jHealth")
@RequiredArgsConstructor
@Slf4j
public class Neo4jHealthIndicator implements HealthIndicator {

  private final Neo4jGraphService neo4jService;

  @Override
  public Health health() {
    long start = System.currentTimeMillis();
    try {
      boolean available = neo4jService.isAvailable();
      long latency = System.currentTimeMillis() - start;

      if (available) {
        log.debug("Neo4j health check OK - latency: {}ms", latency);
        return Health.up()
            .withDetail("latencyMs", latency)
            .withDetail("status", "connected")
            .withDetail("service", "neo4j")
            .build();
      } else {
        log.warn("Neo4j health check FAILED - service unavailable");
        return Health.down()
            .withDetail("status", "disconnected")
            .withDetail("service", "neo4j")
            .withDetail("message", "Neo4j service is not available")
            .build();
      }
    } catch (Exception e) {
      long latency = System.currentTimeMillis() - start;
      log.error("Neo4j health check ERROR: {}", e.getMessage());
      return Health.down()
          .withDetail("error", e.getMessage())
          .withDetail("errorType", e.getClass().getSimpleName())
          .withDetail("status", "error")
          .withDetail("service", "neo4j")
          .withDetail("latencyMs", latency)
          .build();
    }
  }
}
