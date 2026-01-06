package com.rulex.infrastructure.cache;

import com.rulex.application.port.out.RuleCachePort;
import java.time.Duration;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Adapter de cache in-memory para regras.
 *
 * <p>Implementação simples usando ConcurrentHashMap. Será substituído por Redis na Fase 5.
 */
@Component
@Slf4j
public class InMemoryCacheAdapter implements RuleCachePort {

  private final ConcurrentMap<String, CacheEntry> cache = new ConcurrentHashMap<>();

  @Override
  @SuppressWarnings("unchecked")
  public <T> Optional<T> get(String key, Class<T> type) {
    CacheEntry entry = cache.get(key);
    if (entry == null) {
      return Optional.empty();
    }

    // Verificar TTL
    if (entry.isExpired()) {
      cache.remove(key);
      return Optional.empty();
    }

    try {
      return Optional.ofNullable((T) entry.value());
    } catch (ClassCastException e) {
      log.warn("Cache type mismatch for key {}: expected {}", key, type.getSimpleName());
      return Optional.empty();
    }
  }

  @Override
  public void put(String key, Object value) {
    put(key, value, Duration.ofMinutes(5)); // TTL default de 5 minutos
  }

  @Override
  public void put(String key, Object value, Duration ttl) {
    long expireAt = System.currentTimeMillis() + ttl.toMillis();
    cache.put(key, new CacheEntry(value, expireAt));
    log.debug("Cache put: key={}, ttl={}ms", key, ttl.toMillis());
  }

  @Override
  public void evict(String key) {
    cache.remove(key);
    log.debug("Cache evict: key={}", key);
  }

  /** Limpa entradas expiradas (pode ser chamado periodicamente) */
  public void cleanupExpired() {
    long now = System.currentTimeMillis();
    cache.entrySet().removeIf(entry -> entry.getValue().expireAt() < now);
  }

  /** Retorna tamanho atual do cache */
  public int size() {
    return cache.size();
  }

  /** Limpa todo o cache */
  public void clear() {
    cache.clear();
    log.info("Cache cleared");
  }

  private record CacheEntry(Object value, long expireAt) {
    boolean isExpired() {
      return System.currentTimeMillis() > expireAt;
    }
  }
}
