package com.rulex.config;

import io.micrometer.observation.ObservationRegistry;
import io.micrometer.observation.aop.ObservedAspect;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * OpenTelemetry configuration for distributed tracing.
 *
 * <p>This configuration enables:
 * <ul>
 *   <li>Automatic HTTP request tracing via Micrometer</li>
 *   <li>@Observed annotation support for custom spans</li>
 *   <li>W3C trace context propagation</li>
 *   <li>OTLP export to observability backends (Jaeger, Tempo, etc.)</li>
 * </ul>
 *
 * <p>Environment variables:
 * <ul>
 *   <li>OTEL_EXPORTER_OTLP_ENDPOINT: OTLP collector endpoint (default: http://localhost:4318)</li>
 *   <li>OTEL_SAMPLING_PROBABILITY: Sampling rate 0.0-1.0 (default: 1.0)</li>
 * </ul>
 *
 * @see <a href="https://opentelemetry.io/">OpenTelemetry</a>
 */
@Configuration
public class OpenTelemetryConfig {

  /**
   * Enables the @Observed annotation for creating custom spans.
   *
   * <p>Usage example:
   * <pre>
   * {@literal @}Observed(name = "process-transaction", contextualName = "processTransaction")
   * public TransactionResult processTransaction(Transaction tx) {
   *     // Method execution is automatically traced
   * }
   * </pre>
   *
   * @param observationRegistry the Micrometer observation registry
   * @return the ObservedAspect bean
   */
  @Bean
  public ObservedAspect observedAspect(ObservationRegistry observationRegistry) {
    return new ObservedAspect(observationRegistry);
  }
}
