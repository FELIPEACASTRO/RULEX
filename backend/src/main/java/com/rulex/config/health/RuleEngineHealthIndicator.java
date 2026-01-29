package com.rulex.config.health;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.OperatorStatus;
import com.rulex.service.complex.evaluator.OperatorEvaluatorRegistry;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.actuate.health.Health;
import org.springframework.boot.actuate.health.HealthIndicator;
import org.springframework.stereotype.Component;

/**
 * Health Indicator do Rule Engine.
 *
 * <p>Expõe métricas básicas do registry de operadores.
 */
@Component("ruleEngineHealth")
@RequiredArgsConstructor
@Slf4j
public class RuleEngineHealthIndicator implements HealthIndicator {

  private final OperatorEvaluatorRegistry operatorRegistry;

  @Override
  public Health health() {
    try {
      int totalOperators = ConditionOperator.values().length;
      int registered = operatorRegistry.getAllSupportedOperators().size();
      int stable =
          (int)
              operatorRegistry.getAllSupportedOperators().stream()
                  .filter(op -> operatorRegistry.getStatus(op) == OperatorStatus.STABLE)
                  .count();
      int planned =
          (int)
              operatorRegistry.getAllSupportedOperators().stream()
                  .filter(op -> operatorRegistry.getStatus(op) == OperatorStatus.PLANNED)
                  .count();
      int missing = Math.max(0, totalOperators - registered);

      Health.Builder builder = Health.up();
      builder
          .withDetail("service", "rule-engine")
          .withDetail("totalOperators", totalOperators)
          .withDetail("registeredOperators", registered)
          .withDetail("stableOperators", stable)
          .withDetail("plannedOperators", planned)
          .withDetail("missingOperators", missing);

      if (missing > 0) {
        builder.withDetail("status", "missing_operators");
      } else {
        builder.withDetail("status", "ok");
      }

      return builder.build();
    } catch (Exception e) {
      log.error("RuleEngine health check ERROR: {}", e.getMessage());
      return Health.down()
          .withDetail("service", "rule-engine")
          .withDetail("status", "error")
          .withDetail("error", e.getMessage())
          .build();
    }
  }
}