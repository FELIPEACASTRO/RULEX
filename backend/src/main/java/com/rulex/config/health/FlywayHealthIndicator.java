package com.rulex.config.health;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.flywaydb.core.Flyway;
import org.flywaydb.core.api.MigrationInfo;
import org.flywaydb.core.api.MigrationInfoService;
import io.micrometer.core.instrument.Gauge;
import io.micrometer.core.instrument.MeterRegistry;
import jakarta.annotation.PostConstruct;
import org.springframework.boot.actuate.health.Health;
import org.springframework.boot.actuate.health.HealthIndicator;
import org.springframework.stereotype.Component;

/**
 * OPS-004 FIX: Health Indicator para status das migrations Flyway.
 *
 * <p>Verifica:
 * - Se há migrations pendentes
 * - Se há migrations falhadas
 * - Versão atual do schema
 * - Total de migrations aplicadas
 */
@Component("flywayMigrationHealth")
@RequiredArgsConstructor
@Slf4j
public class FlywayHealthIndicator implements HealthIndicator {

  private final Flyway flyway;
  private final MeterRegistry meterRegistry;
  private final AtomicInteger failedMigrations = new AtomicInteger(0);
  private final AtomicInteger pendingMigrations = new AtomicInteger(0);

  @PostConstruct
  void registerMetrics() {
    Gauge.builder("rulex.flyway.migrations.failed", failedMigrations, AtomicInteger::get)
        .description("Quantidade de migrations Flyway falhadas")
        .register(meterRegistry);
    Gauge.builder("rulex.flyway.migrations.pending", pendingMigrations, AtomicInteger::get)
        .description("Quantidade de migrations Flyway pendentes")
        .register(meterRegistry);
  }

  @Override
  public Health health() {
    try {
      MigrationInfoService infoService = flyway.info();
      MigrationInfo current = infoService.current();
      MigrationInfo[] pending = infoService.pending();
      MigrationInfo[] applied = infoService.applied();

      // Verificar migrations falhadas através do estado
      MigrationInfo[] failed = java.util.Arrays.stream(infoService.all())
          .filter(m -> m.getState() != null && m.getState().isFailed())
          .toArray(MigrationInfo[]::new);

      Map<String, Object> details = new HashMap<>();
      details.put("service", "flyway-migrations");

      // Versão atual
      if (current != null) {
        details.put("currentVersion", current.getVersion().toString());
        details.put("currentDescription", current.getDescription());
      } else {
        details.put("currentVersion", "none");
      }

      // Contagens
      details.put("appliedMigrations", applied != null ? applied.length : 0);
      details.put("pendingMigrations", pending != null ? pending.length : 0);
      details.put("failedMigrations", failed != null ? failed.length : 0);

      failedMigrations.set(failed != null ? failed.length : 0);
      pendingMigrations.set(pending != null ? pending.length : 0);

      // Se há migrations falhadas, status DOWN
      if (failed != null && failed.length > 0) {
        details.put("status", "failed_migrations");
        details.put(
            "failedVersions",
            java.util.Arrays.stream(failed)
                .map(m -> m.getVersion().toString())
                .toList());

        log.error("Flyway health check FAILED: {} failed migrations", failed.length);
        return Health.down().withDetails(details).build();
      }

      // Se há migrations pendentes, status WARNING (mas UP)
      if (pending != null && pending.length > 0) {
        details.put("status", "pending_migrations");
        details.put(
            "pendingVersions",
            java.util.Arrays.stream(pending)
                .map(m -> m.getVersion().toString())
                .toList());

        log.warn("Flyway health check WARNING: {} pending migrations", pending.length);
        return Health.up().withDetails(details).build();
      }

      // Tudo OK
      details.put("status", "ok");
      return Health.up().withDetails(details).build();

    } catch (Exception e) {
      log.error("Flyway health check ERROR: {}", e.getMessage());
      return Health.down()
          .withDetail("service", "flyway-migrations")
          .withDetail("status", "error")
          .withDetail("error", e.getMessage())
          .build();
    }
  }
}
