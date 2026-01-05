package com.rulex.config;

import java.time.Duration;
import java.util.Arrays;
import org.springframework.cache.CacheManager;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.cache.concurrent.ConcurrentMapCacheManager;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;

/**
 * Configuração de cache para otimização de performance.
 *
 * <p>Caches configurados:
 *
 * <ul>
 *   <li><b>enabledRules</b>: Cache de regras habilitadas (evita query DB por request)
 *   <li><b>advancedRules</b>: Cache de regras avançadas/complexas
 *   <li><b>ruleOrder</b>: Cache da ordenação otimizada de regras
 * </ul>
 *
 * <p>IMPORTANTE: Os caches são invalidados automaticamente em operações de CRUD de regras via
 * {@code @CacheEvict} nos controllers/services correspondentes.
 *
 * <p>Para ambiente de produção com múltiplas instâncias, considerar usar Redis como cache
 * distribuído em vez de ConcurrentMapCache.
 */
@Configuration
@EnableCaching
public class CacheConfig {

  /** Nomes dos caches disponíveis. */
  public static final String CACHE_ENABLED_RULES = "enabledRules";

  public static final String CACHE_ADVANCED_RULES = "advancedRules";
  public static final String CACHE_RULE_ORDER = "ruleOrder";

  /**
   * Cache manager principal usando ConcurrentMapCache (in-memory). Adequado para ambiente single
   * instance. Para múltiplas instâncias, substituir por RedisCacheManager.
   */
  @Bean
  @Primary
  public CacheManager cacheManager() {
    ConcurrentMapCacheManager cacheManager =
        new ConcurrentMapCacheManager(CACHE_ENABLED_RULES, CACHE_ADVANCED_RULES, CACHE_RULE_ORDER);

    // Permitir criação dinâmica de caches adicionais se necessário
    cacheManager.setAllowNullValues(false);

    return cacheManager;
  }
}
