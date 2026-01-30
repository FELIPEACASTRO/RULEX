package com.rulex.controller;

import com.rulex.config.RedisMetricsConfig;
import com.rulex.service.RedisVelocityCacheService;
import com.rulex.service.VelocityServiceFacade;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import java.time.Instant;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.cache.CacheManager;
import org.springframework.data.redis.connection.RedisConnection;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * Controller para diagnóstico e monitoramento da integração Redis.
 *
 * <p>FASE 1 - GAP-FIX: Endpoint de diagnóstico para verificar status Redis em runtime.
 */
@RestController
@RequestMapping("/admin/redis")
@Slf4j
@Tag(name = "Redis Admin", description = "Endpoints de administração e diagnóstico do Redis")
public class RedisHealthController {

  private final StringRedisTemplate redisTemplate;
  private final VelocityServiceFacade velocityServiceFacade;
  private final RedisVelocityCacheService redisVelocityCacheService;
  private final RedisMetricsConfig redisMetricsConfig;
  private final CacheManager cacheManager;

    public RedisHealthController(
      StringRedisTemplate redisTemplate,
      VelocityServiceFacade velocityServiceFacade,
      ObjectProvider<RedisVelocityCacheService> redisVelocityCacheServiceProvider,
      ObjectProvider<RedisMetricsConfig> redisMetricsConfigProvider,
      ObjectProvider<CacheManager> cacheManagerProvider) {
    this.redisTemplate = redisTemplate;
    this.velocityServiceFacade = velocityServiceFacade;
    this.redisVelocityCacheService =
      redisVelocityCacheServiceProvider != null
        ? redisVelocityCacheServiceProvider.getIfAvailable()
        : null;
    this.redisMetricsConfig =
      redisMetricsConfigProvider != null ? redisMetricsConfigProvider.getIfAvailable() : null;
    this.cacheManager =
      cacheManagerProvider != null ? cacheManagerProvider.getIfAvailable() : null;
    }

  @Value("${rulex.engine.velocity.redis.enabled:false}")
  private boolean redisEnabled;

  @GetMapping("/status")
  @PreAuthorize("hasRole('ADMIN')")
  @Operation(summary = "Status completo do Redis", description = "Retorna diagnóstico detalhado")
  public ResponseEntity<Map<String, Object>> getRedisStatus() {
    Map<String, Object> status = new HashMap<>();
    status.put("timestamp", Instant.now().toString());

    // Configuração
    Map<String, Object> config = new HashMap<>();
    config.put("redisEnabled", redisEnabled);
    config.put("velocityCacheServiceAvailable", redisVelocityCacheService != null);
    config.put("cacheStatus", velocityServiceFacade.getCacheStatus());
    status.put("configuration", config);

    // Conexão
    Map<String, Object> connection = new HashMap<>();
    try {
      RedisConnectionFactory factory = redisTemplate.getConnectionFactory();
      if (factory != null) {
        long startTime = System.currentTimeMillis();
        RedisConnection conn = factory.getConnection();
        String pong = conn.ping();
        long latency = System.currentTimeMillis() - startTime;

        connection.put("status", "CONNECTED");
        connection.put("ping", pong);
        connection.put("latencyMs", latency);

        // Info do servidor
        try {
          var serverInfo = conn.serverCommands().info("server");
          if (serverInfo != null) {
            connection.put("redisVersion", serverInfo.getProperty("redis_version"));
            connection.put("uptimeSeconds", serverInfo.getProperty("uptime_in_seconds"));
          }
        } catch (Exception e) {
          connection.put("serverInfoError", e.getMessage());
        }

        // Info dos clientes
        try {
          var clientInfo = conn.serverCommands().info("clients");
          if (clientInfo != null) {
            connection.put("connectedClients", clientInfo.getProperty("connected_clients"));
          }
        } catch (Exception e) {
          connection.put("clientInfoError", e.getMessage());
        }

        conn.close();
      } else {
        connection.put("status", "NO_FACTORY");
      }
    } catch (Exception e) {
      connection.put("status", "ERROR");
      connection.put("error", e.getMessage());
      connection.put("errorType", e.getClass().getSimpleName());
    }
    status.put("connection", connection);

    // Estatísticas de chaves
    Map<String, Object> keys = new HashMap<>();
    try {
      Long dbSize = redisTemplate.getConnectionFactory().getConnection().serverCommands().dbSize();
      keys.put("totalKeys", dbSize);

      // Contar chaves por prefixo (amostragem)
      Set<String> velocityKeys = redisTemplate.keys("velocity:*");
      Set<String> rulexKeys = redisTemplate.keys("rulex:*");
      keys.put("velocityKeyCount", velocityKeys != null ? velocityKeys.size() : 0);
      keys.put("rulexKeyCount", rulexKeys != null ? rulexKeys.size() : 0);
    } catch (Exception e) {
      keys.put("error", e.getMessage());
    }
    status.put("keys", keys);

    // Diagnóstico
    Map<String, Object> diagnosis = new HashMap<>();
    if (!redisEnabled) {
      diagnosis.put("issue", "Redis DESABILITADO na configuração");
      diagnosis.put("action", "Definir rulex.engine.velocity.redis.enabled=true");
    } else if (redisVelocityCacheService == null) {
      diagnosis.put("issue", "RedisVelocityCacheService NÃO foi inicializado");
      diagnosis.put("action", "Verificar @ConditionalOnProperty e logs de startup");
    } else if (connection.get("status").equals("ERROR")) {
      diagnosis.put("issue", "Falha na conexão Redis");
      diagnosis.put("action", "Verificar SPRING_DATA_REDIS_HOST e SPRING_DATA_REDIS_PORT");
    } else {
      Long totalKeys = (Long) keys.get("totalKeys");
      if (totalKeys != null && totalKeys == 0) {
        diagnosis.put("warning", "Redis conectado mas sem chaves - verificar se transações estão sendo registradas");
        diagnosis.put("action", "Enviar transação de teste e verificar logs");
      } else {
        diagnosis.put("status", "HEALTHY");
        diagnosis.put("message", "Integração Redis funcionando corretamente");
      }
    }
    status.put("diagnosis", diagnosis);

    return ResponseEntity.ok(status);
  }

  @PostMapping("/test-write")
  @PreAuthorize("hasRole('ADMIN')")
  @Operation(summary = "Testa gravação no Redis", description = "Grava uma chave de teste")
  public ResponseEntity<Map<String, Object>> testWrite() {
    Map<String, Object> result = new HashMap<>();
    String testKey = "rulex:test:" + System.currentTimeMillis();
    String testValue = "test-" + Instant.now();

    try {
      // Escrever
      redisTemplate.opsForValue().set(testKey, testValue, java.time.Duration.ofMinutes(1));
      result.put("writeStatus", "SUCCESS");
      result.put("key", testKey);
      result.put("value", testValue);

      // Ler de volta
      String readValue = redisTemplate.opsForValue().get(testKey);
      result.put("readStatus", testValue.equals(readValue) ? "SUCCESS" : "MISMATCH");
      result.put("readValue", readValue);

      // Deletar
      redisTemplate.delete(testKey);
      result.put("deleteStatus", "SUCCESS");

      result.put("overallStatus", "REDIS_WRITE_READ_DELETE_OK");

    } catch (Exception e) {
      result.put("status", "ERROR");
      result.put("error", e.getMessage());
      result.put("errorType", e.getClass().getSimpleName());
      log.error("Falha no teste de escrita Redis", e);
    }

    return ResponseEntity.ok(result);
  }

  @GetMapping("/velocity-keys")
  @PreAuthorize("hasRole('ADMIN')")
  @Operation(summary = "Lista chaves de velocity", description = "Amostra de chaves velocity:*")
  public ResponseEntity<Map<String, Object>> getVelocityKeys() {
    Map<String, Object> result = new HashMap<>();

    try {
      Set<String> velocityKeys = redisTemplate.keys("velocity:*");
      Set<String> rulexVelocityKeys = redisTemplate.keys("rulex:velocity:*");

      result.put("velocityPrefixCount", velocityKeys != null ? velocityKeys.size() : 0);
      result.put("rulexVelocityPrefixCount", rulexVelocityKeys != null ? rulexVelocityKeys.size() : 0);

      // Amostra de até 20 chaves
      if (velocityKeys != null && !velocityKeys.isEmpty()) {
        result.put("velocitySample", velocityKeys.stream().limit(20).toList());
      }
      if (rulexVelocityKeys != null && !rulexVelocityKeys.isEmpty()) {
        result.put("rulexVelocitySample", rulexVelocityKeys.stream().limit(20).toList());
      }

      result.put("status", "SUCCESS");

    } catch (Exception e) {
      result.put("status", "ERROR");
      result.put("error", e.getMessage());
    }

    return ResponseEntity.ok(result);
  }

  @GetMapping("/metrics")
  @PreAuthorize("hasRole('ADMIN')")
  @Operation(summary = "Métricas Redis", description = "Retorna métricas coletadas do Redis")
  public ResponseEntity<Map<String, Object>> getMetrics() {
    Map<String, Object> result = new HashMap<>();
    result.put("timestamp", Instant.now().toString());

    // CacheManager info
    Map<String, Object> cacheInfo = new HashMap<>();
    if (cacheManager != null) {
      cacheInfo.put("type", cacheManager.getClass().getSimpleName());
      cacheInfo.put("cacheNames", cacheManager.getCacheNames());
    } else {
      cacheInfo.put("type", "NONE");
    }
    result.put("cacheManager", cacheInfo);

    // Métricas do RedisMetricsConfig
    if (redisMetricsConfig != null) {
      result.put("redisMetrics", redisMetricsConfig.getMetricsSnapshot());
    } else {
      result.put("redisMetrics", Map.of("status", "METRICS_CONFIG_NOT_AVAILABLE"));
    }

    // Info de memória do Redis
    try {
      RedisConnection conn = redisTemplate.getConnectionFactory().getConnection();
      var memoryInfo = conn.serverCommands().info("memory");
      if (memoryInfo != null) {
        Map<String, Object> memory = new HashMap<>();
        memory.put("usedMemoryHuman", memoryInfo.getProperty("used_memory_human"));
        memory.put("usedMemoryPeak", memoryInfo.getProperty("used_memory_peak_human"));
        memory.put("maxMemory", memoryInfo.getProperty("maxmemory_human"));
        memory.put("fragmentation", memoryInfo.getProperty("mem_fragmentation_ratio"));
        result.put("memoryInfo", memory);
      }
      conn.close();
    } catch (Exception e) {
      result.put("memoryInfoError", e.getMessage());
    }

    return ResponseEntity.ok(result);
  }

  @GetMapping("/cache-stats")
  @PreAuthorize("hasRole('ADMIN')")
  @Operation(summary = "Estatísticas de Cache", description = "Retorna estatísticas dos caches @Cacheable")
  public ResponseEntity<Map<String, Object>> getCacheStats() {
    Map<String, Object> result = new HashMap<>();
    result.put("timestamp", Instant.now().toString());

    if (cacheManager == null) {
      result.put("status", "NO_CACHE_MANAGER");
      return ResponseEntity.ok(result);
    }

    result.put("cacheManagerType", cacheManager.getClass().getSimpleName());
    result.put("cacheNames", cacheManager.getCacheNames());

    // Contar chaves em caches Redis
    Map<String, Object> cacheDetails = new HashMap<>();
    try {
      Set<String> cacheKeys = redisTemplate.keys("rulex:cache:*");
      if (cacheKeys != null) {
        cacheDetails.put("totalCacheKeys", cacheKeys.size());

        // Agrupar por nome de cache
        Map<String, Long> keysPerCache = new HashMap<>();
        for (String key : cacheKeys) {
          String[] parts = key.split(":");
          if (parts.length >= 3) {
            String cacheName = parts[2];
            keysPerCache.merge(cacheName, 1L, Long::sum);
          }
        }
        cacheDetails.put("keysPerCache", keysPerCache);
      }
    } catch (Exception e) {
      cacheDetails.put("error", e.getMessage());
    }
    result.put("cacheDetails", cacheDetails);

    return ResponseEntity.ok(result);
  }
}
