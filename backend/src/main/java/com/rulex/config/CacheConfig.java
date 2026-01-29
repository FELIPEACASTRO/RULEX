package com.rulex.config;

import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.cache.CacheManager;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.cache.concurrent.ConcurrentMapCacheManager;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Configuração de cache em memória para o RULEX (fallback).
 *
 * <p>Esta configuração é usada quando o Redis NÃO está habilitado
 * (rulex.engine.velocity.redis.enabled=false ou ausente).
 *
 * <p>Usa ConcurrentMapCacheManager como implementação de cache em memória. Caches configurados:
 *
 * <ul>
 *   <li>binLookup - Cache de BIN lookup
 *   <li>mccCategory - Cache de categorias MCC
 *   <li>geoReference - Cache de referências geográficas
 *   <li>geoPolygon - Cache de polígonos geográficos
 *   <li>ruleConfigurations - Cache de configurações de regras
 *   <li>advancedRules - Cache de regras avançadas
 *   <li>highRiskMcc - Cache de MCCs de alto risco
 * </ul>
 *
 * <p>Nota: Quando rulex.engine.velocity.redis.enabled=true, o RedisCacheConfig é usado.
 */
@Configuration
@EnableCaching
@ConditionalOnProperty(
    name = "rulex.engine.velocity.redis.enabled",
    havingValue = "false",
    matchIfMissing = true)
public class CacheConfig {

  @Bean
  public CacheManager cacheManager() {
    ConcurrentMapCacheManager cacheManager =
        new ConcurrentMapCacheManager(
            "binLookup",
            "mccCategory",
            "geoReference",
            "geoPolygon",
            "ruleConfigurations",
            "advancedRules",
            "highRiskMcc");

    // Permitir criação dinâmica de caches
    cacheManager.setAllowNullValues(false);

    return cacheManager;
  }
}
