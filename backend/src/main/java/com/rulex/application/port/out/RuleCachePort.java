package com.rulex.application.port.out;

import java.time.Duration;
import java.util.Optional;

/**
 * Port de cache para regras.
 *
 * <p>Define como a camada de aplicação acessa cache. Implementação pode ser Redis, Caffeine, etc.
 */
public interface RuleCachePort {

  /** Chave para lista de regras ativas */
  String ACTIVE_RULES_KEY = "rules:active";

  /** Chave para regras shadow */
  String SHADOW_RULES_KEY = "rules:shadow";

  /**
   * Busca valor no cache.
   *
   * @param key chave do cache
   * @param type tipo do valor
   * @return valor ou empty
   */
  <T> Optional<T> get(String key, Class<T> type);

  /**
   * Armazena valor no cache.
   *
   * @param key chave do cache
   * @param value valor a armazenar
   */
  void put(String key, Object value);

  /**
   * Armazena valor com TTL.
   *
   * @param key chave do cache
   * @param value valor a armazenar
   * @param ttl tempo de vida
   */
  void put(String key, Object value, Duration ttl);

  /**
   * Remove valor do cache.
   *
   * @param key chave do cache
   */
  void evict(String key);

  /** Invalida cache de regras ativas. */
  default void evictActiveRules() {
    evict(ACTIVE_RULES_KEY);
  }

  /** Invalida cache de regras shadow. */
  default void evictShadowRules() {
    evict(SHADOW_RULES_KEY);
  }

  /** Invalida todo o cache de regras. */
  default void evictAllRules() {
    evictActiveRules();
    evictShadowRules();
  }
}
