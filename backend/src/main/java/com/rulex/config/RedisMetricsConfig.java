package com.rulex.config;

import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.Tag;
import io.micrometer.core.instrument.Tags;
import io.micrometer.core.instrument.Timer;
import io.micrometer.core.instrument.binder.MeterBinder;
import jakarta.annotation.PostConstruct;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.core.RedisCallback;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

/**
 * Métricas de Redis para Micrometer/Prometheus.
 *
 * <p>Expõe métricas de:
 *
 * <ul>
 *   <li>Conexão Redis (status, latência)
 *   <li>Operações (comandos executados, erros)
 *   <li>Memória utilizada pelo Redis
 *   <li>Chaves por prefixo (velocity, cache, etc.)
 *   <li>Cache hit/miss ratio
 * </ul>
 */
@Component
@ConditionalOnProperty(
    name = "rulex.engine.velocity.redis.enabled",
    havingValue = "true",
    matchIfMissing = false)
@RequiredArgsConstructor
@Slf4j
public class RedisMetricsConfig implements MeterBinder {

  private final RedisConnectionFactory connectionFactory;
  private final StringRedisTemplate redisTemplate;

  private MeterRegistry meterRegistry;

  // Contadores atômicos para métricas
  private final AtomicLong totalKeys = new AtomicLong(0);
  private final AtomicLong velocityKeys = new AtomicLong(0);
  private final AtomicLong cacheKeys = new AtomicLong(0);
  private final AtomicLong usedMemoryBytes = new AtomicLong(0);
  private final AtomicLong connectedClients = new AtomicLong(0);
  private final AtomicLong commandsProcessed = new AtomicLong(0);
  private final AtomicLong lastPingLatencyMs = new AtomicLong(0);

  // Contadores de operações
  private final AtomicLong operationsSuccess = new AtomicLong(0);
  private final AtomicLong operationsError = new AtomicLong(0);

  @PostConstruct
  public void init() {
    log.info("============================================================");
    log.info("RedisMetricsConfig INICIALIZANDO");
    log.info("Métricas Redis serão expostas em /actuator/prometheus");
    log.info("============================================================");
  }

  @Override
  public void bindTo(MeterRegistry registry) {
    this.meterRegistry = registry;

    Tags commonTags = Tags.of(Tag.of("application", "rulex"));

    // Gauge para total de chaves
    registry.gauge(
        "rulex.redis.keys.total", commonTags, totalKeys, AtomicLong::doubleValue);

    // Gauge para chaves de velocity
    registry.gauge(
        "rulex.redis.keys.velocity", commonTags, velocityKeys, AtomicLong::doubleValue);

    // Gauge para chaves de cache
    registry.gauge(
        "rulex.redis.keys.cache", commonTags, cacheKeys, AtomicLong::doubleValue);

    // Gauge para memória utilizada
    registry.gauge(
        "rulex.redis.memory.used.bytes", commonTags, usedMemoryBytes, AtomicLong::doubleValue);

    // Gauge para clientes conectados
    registry.gauge(
        "rulex.redis.clients.connected", commonTags, connectedClients, AtomicLong::doubleValue);

    // Gauge para comandos processados
    registry.gauge(
        "rulex.redis.commands.processed.total",
        commonTags,
        commandsProcessed,
        AtomicLong::doubleValue);

    // Gauge para latência do ping
    registry.gauge(
        "rulex.redis.ping.latency.ms", commonTags, lastPingLatencyMs, AtomicLong::doubleValue);

    // Counter para operações bem-sucedidas
    registry.gauge(
        "rulex.redis.operations.success.total",
        commonTags,
        operationsSuccess,
        AtomicLong::doubleValue);

    // Counter para operações com erro
    registry.gauge(
        "rulex.redis.operations.error.total",
        commonTags,
        operationsError,
        AtomicLong::doubleValue);

    log.info("Métricas Redis registradas no MeterRegistry");
  }

  /** Atualiza métricas a cada 30 segundos. */
  @Scheduled(fixedRate = 30000)
  public void updateMetrics() {
    try {
      updateKeyMetrics();
      updateInfoMetrics();
      updateLatencyMetric();
      operationsSuccess.incrementAndGet();
    } catch (Exception e) {
      log.warn("Erro ao atualizar métricas Redis: {}", e.getMessage());
      operationsError.incrementAndGet();
    }
  }

  private void updateKeyMetrics() {
    try {
      // Total de chaves
      Long dbSize =
          redisTemplate.execute(
              (RedisCallback<Long>) connection -> connection.serverCommands().dbSize());
      if (dbSize != null) {
        totalKeys.set(dbSize);
      }

      // Chaves de velocity
      var velocityPatternKeys = redisTemplate.keys("velocity:*");
      var rulexVelocityKeys = redisTemplate.keys("rulex:velocity:*");
      long velocityCount =
          (velocityPatternKeys != null ? velocityPatternKeys.size() : 0)
              + (rulexVelocityKeys != null ? rulexVelocityKeys.size() : 0);
      velocityKeys.set(velocityCount);

      // Chaves de cache
      var cachePatternKeys = redisTemplate.keys("rulex:cache:*");
      cacheKeys.set(cachePatternKeys != null ? cachePatternKeys.size() : 0);

    } catch (Exception e) {
      log.debug("Erro ao contar chaves Redis: {}", e.getMessage());
    }
  }

  private void updateInfoMetrics() {
    try {
      String info =
          redisTemplate.execute(
              (RedisCallback<String>)
                  connection -> {
                    var props = connection.serverCommands().info("all");
                    return props != null ? props.toString() : "";
                  });

      if (info != null) {
        // Extrair used_memory
        String usedMemory = extractValue(info, "used_memory");
        if (usedMemory != null) {
          usedMemoryBytes.set(Long.parseLong(usedMemory));
        }

        // Extrair connected_clients
        String clients = extractValue(info, "connected_clients");
        if (clients != null) {
          connectedClients.set(Long.parseLong(clients));
        }

        // Extrair total_commands_processed
        String commands = extractValue(info, "total_commands_processed");
        if (commands != null) {
          commandsProcessed.set(Long.parseLong(commands));
        }
      }
    } catch (Exception e) {
      log.debug("Erro ao obter INFO do Redis: {}", e.getMessage());
    }
  }

  private void updateLatencyMetric() {
    long start = System.nanoTime();
    try {
      String pong =
          redisTemplate.execute(
              (RedisCallback<String>) connection -> connection.ping());
      long latencyMs = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - start);
      lastPingLatencyMs.set(latencyMs);

      // Registrar no Timer também
      if (meterRegistry != null) {
        Timer timer =
            meterRegistry.timer("rulex.redis.ping.duration", Tags.of("status", "success"));
        timer.record(latencyMs, TimeUnit.MILLISECONDS);
      }
    } catch (Exception e) {
      log.debug("Erro no PING Redis: {}", e.getMessage());
      lastPingLatencyMs.set(-1);
    }
  }

  private String extractValue(String info, String key) {
    if (info == null) return null;
    String search = key + "=";
    int start = info.indexOf(search);
    if (start == -1) {
      search = key + ":";
      start = info.indexOf(search);
    }
    if (start == -1) return null;
    start += search.length();
    int end = info.indexOf('\n', start);
    if (end == -1) end = info.indexOf(',', start);
    if (end == -1) end = info.length();
    return info.substring(start, end).trim();
  }

  /** Registra uma operação bem-sucedida. */
  public void recordSuccess() {
    operationsSuccess.incrementAndGet();
  }

  /** Registra uma operação com erro. */
  public void recordError() {
    operationsError.incrementAndGet();
  }

  /** Retorna as métricas atuais como Map. */
  public java.util.Map<String, Object> getMetricsSnapshot() {
    return java.util.Map.of(
        "totalKeys", totalKeys.get(),
        "velocityKeys", velocityKeys.get(),
        "cacheKeys", cacheKeys.get(),
        "usedMemoryBytes", usedMemoryBytes.get(),
        "connectedClients", connectedClients.get(),
        "commandsProcessed", commandsProcessed.get(),
        "lastPingLatencyMs", lastPingLatencyMs.get(),
        "operationsSuccess", operationsSuccess.get(),
        "operationsError", operationsError.get());
  }
}
