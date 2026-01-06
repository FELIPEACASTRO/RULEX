package com.rulex.infrastructure.cache;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.application.port.out.RuleCachePort;
import java.time.Duration;
import java.util.Optional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Primary;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

/**
 * Adapter de cache Redis para regras.
 *
 * <p>Implementação distribuída usando Redis. Ativada quando redis.enabled=true.
 *
 * <p>Este adapter é marcado como @Primary para substituir o InMemoryCacheAdapter quando Redis está
 * disponível.
 */
@Component
@ConditionalOnProperty(name = "spring.data.redis.enabled", havingValue = "true", matchIfMissing = false)
@Primary
@RequiredArgsConstructor
@Slf4j
public class RedisCacheAdapter implements RuleCachePort {

  private final StringRedisTemplate redisTemplate;
  private final ObjectMapper objectMapper;

  private static final String KEY_PREFIX = "rulex:rules:";
  private static final Duration DEFAULT_TTL = Duration.ofMinutes(5);

  @Override
  public <T> Optional<T> get(String key, Class<T> type) {
    try {
      String redisKey = buildKey(key);
      String json = redisTemplate.opsForValue().get(redisKey);

      if (json == null) {
        log.debug("Cache miss: key={}", key);
        return Optional.empty();
      }

      T value = deserialize(json, type);
      log.debug("Cache hit: key={}", key);
      return Optional.ofNullable(value);
    } catch (Exception e) {
      log.warn("Cache get error for key {}: {}", key, e.getMessage());
      return Optional.empty();
    }
  }

  @Override
  public void put(String key, Object value) {
    put(key, value, DEFAULT_TTL);
  }

  @Override
  public void put(String key, Object value, Duration ttl) {
    try {
      String redisKey = buildKey(key);
      String json = serialize(value);
      redisTemplate.opsForValue().set(redisKey, json, ttl);
      log.debug("Cache put: key={}, ttl={}s", key, ttl.toSeconds());
    } catch (Exception e) {
      log.warn("Cache put error for key {}: {}", key, e.getMessage());
    }
  }

  @Override
  public void evict(String key) {
    try {
      String redisKey = buildKey(key);
      redisTemplate.delete(redisKey);
      log.debug("Cache evict: key={}", key);
    } catch (Exception e) {
      log.warn("Cache evict error for key {}: {}", key, e.getMessage());
    }
  }

  /** Constrói a chave Redis com prefixo do namespace. */
  private String buildKey(String key) {
    return KEY_PREFIX + key;
  }

  /** Serializa objeto para JSON. */
  private String serialize(Object value) throws JsonProcessingException {
    return objectMapper.writeValueAsString(value);
  }

  /** Deserializa JSON para objeto do tipo especificado. */
  @SuppressWarnings("unchecked")
  private <T> T deserialize(String json, Class<T> type) throws JsonProcessingException {
    // Para listas genéricas (List<Rule>), precisamos usar TypeReference
    if (type == java.util.List.class) {
      // Assumimos que é List<Rule>
      return (T) objectMapper.readValue(json, 
          objectMapper.getTypeFactory().constructCollectionType(java.util.List.class, 
              com.rulex.domain.model.Rule.class));
    }
    return objectMapper.readValue(json, type);
  }

  /** Limpa todas as chaves do namespace RULEX. */
  public void clearNamespace() {
    try {
      var keys = redisTemplate.keys(KEY_PREFIX + "*");
      if (keys != null && !keys.isEmpty()) {
        redisTemplate.delete(keys);
        log.info("Cleared {} keys from Redis namespace", keys.size());
      }
    } catch (Exception e) {
      log.warn("Error clearing Redis namespace: {}", e.getMessage());
    }
  }

  /** Verifica se a conexão Redis está ativa. */
  public boolean isConnected() {
    try {
      String pong = redisTemplate.getConnectionFactory().getConnection().ping();
      return "PONG".equals(pong);
    } catch (Exception e) {
      log.warn("Redis connection check failed: {}", e.getMessage());
      return false;
    }
  }
}
