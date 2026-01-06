package com.rulex.resilience;

import io.github.resilience4j.circuitbreaker.CircuitBreaker;
import io.github.resilience4j.circuitbreaker.CircuitBreakerConfig;
import io.github.resilience4j.circuitbreaker.CircuitBreakerRegistry;
import io.github.resilience4j.retry.Retry;
import io.github.resilience4j.retry.RetryConfig;
import io.github.resilience4j.retry.RetryRegistry;
import java.time.Duration;
import java.util.function.Supplier;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.dao.DataAccessException;

/**
 * Circuit Breaker para resiliência de conexão com banco de dados.
 *
 * <p>Implementa padrão Circuit Breaker para prevenir cascata de falhas quando o banco de dados
 * está indisponível.
 *
 * <p>Estados do Circuit Breaker: - CLOSED: Funcionamento normal - OPEN: Banco indisponível,
 * requisições falham imediatamente - HALF_OPEN: Testando recuperação
 *
 * <p>FIX: Recuperação lenta após perda de conexão com DB (30s → <10s)
 *
 * @version 1.0.0
 */
@Configuration
@Slf4j
public class DatabaseCircuitBreaker {

  /**
   * Configura Circuit Breaker para banco de dados.
   *
   * @return CircuitBreaker configurado
   */
  @Bean
  public CircuitBreaker databaseCircuitBreaker() {
    CircuitBreakerConfig config =
        CircuitBreakerConfig.custom()
            // Threshold: 50% de falhas em 10 requisições
            .failureRateThreshold(50)
            .minimumNumberOfCalls(10)
            // Tempo em OPEN antes de tentar HALF_OPEN: 5 segundos
            .waitDurationInOpenState(Duration.ofSeconds(5))
            // Número de chamadas permitidas em HALF_OPEN: 3
            .permittedNumberOfCallsInHalfOpenState(3)
            // Slow call threshold: 2 segundos
            .slowCallDurationThreshold(Duration.ofSeconds(2))
            .slowCallRateThreshold(50)
            // Tamanho do sliding window: 20 chamadas
            .slidingWindowSize(20)
            .slidingWindowType(CircuitBreakerConfig.SlidingWindowType.COUNT_BASED)
            // Exceções que devem abrir o circuit
            .recordExceptions(
                DataAccessException.class,
                org.springframework.dao.QueryTimeoutException.class,
                org.springframework.jdbc.CannotGetJdbcConnectionException.class)
            // Exceções que devem ser ignoradas
            .ignoreExceptions(IllegalArgumentException.class)
            .build();

    CircuitBreakerRegistry registry = CircuitBreakerRegistry.of(config);
    CircuitBreaker circuitBreaker = registry.circuitBreaker("database");

    // Event listeners para logging
    circuitBreaker
        .getEventPublisher()
        .onSuccess(event -> log.debug("Database call succeeded: {}", event))
        .onError(
            event ->
                log.warn(
                    "Database call failed: {} - {}",
                    event.getThrowable().getClass().getSimpleName(),
                    event.getThrowable().getMessage()))
        .onStateTransition(
            event ->
                log.warn(
                    "Circuit Breaker state transition: {} -> {}",
                    event.getStateTransition().getFromState(),
                    event.getStateTransition().getToState()))
        .onSlowCallRateExceeded(
            event -> log.warn("Slow call rate exceeded: {}%", event.getSlowCallRate()))
        .onFailureRateExceeded(
            event -> log.error("Failure rate exceeded: {}%", event.getFailureRate()));

    return circuitBreaker;
  }

  /**
   * Configura Retry para banco de dados.
   *
   * @return Retry configurado
   */
  @Bean
  public Retry databaseRetry() {
    RetryConfig config =
        RetryConfig.custom()
            // Máximo de 3 tentativas
            .maxAttempts(3)
            // Intervalo entre tentativas: 500ms
            .waitDuration(Duration.ofMillis(500))
            // Exponential backoff: 2x a cada tentativa
            .intervalFunction(
                attempt -> {
                  long interval = 500L * (long) Math.pow(2, attempt - 1);
                  return Math.min(interval, 5000L); // Max 5 segundos
                })
            // Exceções que devem ser retentadas
            .retryExceptions(
                org.springframework.dao.QueryTimeoutException.class,
                org.springframework.jdbc.CannotGetJdbcConnectionException.class,
                org.springframework.dao.TransientDataAccessException.class)
            // Exceções que não devem ser retentadas
            .ignoreExceptions(
                IllegalArgumentException.class,
                org.springframework.dao.DuplicateKeyException.class)
            .build();

    RetryRegistry registry = RetryRegistry.of(config);
    Retry retry = registry.retry("database");

    // Event listeners
    retry
        .getEventPublisher()
        .onRetry(
            event ->
                log.warn(
                    "Retrying database call (attempt {}/{}): {}",
                    event.getNumberOfRetryAttempts(),
                    config.getMaxAttempts(),
                    event.getLastThrowable().getMessage()))
        .onSuccess(
            event ->
                log.info(
                    "Database call succeeded after {} retries",
                    event.getNumberOfRetryAttempts()))
        .onError(
            event ->
                log.error(
                    "Database call failed after {} retries: {}",
                    event.getNumberOfRetryAttempts(),
                    event.getLastThrowable().getMessage()));

    return retry;
  }

  /**
   * Executa operação com Circuit Breaker e Retry.
   *
   * @param <T> Tipo de retorno
   * @param supplier Operação a ser executada
   * @param circuitBreaker Circuit Breaker
   * @param retry Retry
   * @return Resultado da operação
   */
  public static <T> T executeWithResilience(
      Supplier<T> supplier, CircuitBreaker circuitBreaker, Retry retry) {
    // Combina Circuit Breaker + Retry
    Supplier<T> decoratedSupplier =
        CircuitBreaker.decorateSupplier(circuitBreaker, Retry.decorateSupplier(retry, supplier));

    try {
      return decoratedSupplier.get();
    } catch (Exception e) {
      log.error("Operation failed after circuit breaker and retry: {}", e.getMessage());
      throw e;
    }
  }

  /**
   * Executa operação com Circuit Breaker, Retry e Fallback.
   *
   * @param <T> Tipo de retorno
   * @param supplier Operação a ser executada
   * @param fallback Fallback em caso de falha
   * @param circuitBreaker Circuit Breaker
   * @param retry Retry
   * @return Resultado da operação ou fallback
   */
  public static <T> T executeWithFallback(
      Supplier<T> supplier, Supplier<T> fallback, CircuitBreaker circuitBreaker, Retry retry) {
    try {
      return executeWithResilience(supplier, circuitBreaker, retry);
    } catch (Exception e) {
      log.warn("Using fallback due to: {}", e.getMessage());
      return fallback.get();
    }
  }
}
