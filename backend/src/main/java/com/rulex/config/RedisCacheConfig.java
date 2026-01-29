package com.rulex.config;

import java.time.Duration;
import java.util.HashMap;
import java.util.Map;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.cache.CacheManager;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.data.redis.cache.RedisCacheConfiguration;
import org.springframework.data.redis.cache.RedisCacheManager;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.serializer.GenericJackson2JsonRedisSerializer;
import org.springframework.data.redis.serializer.RedisSerializationContext;
import org.springframework.data.redis.serializer.StringRedisSerializer;

/**
 * Configuração de RedisCacheManager para uso com @Cacheable.
 *
 * <p>Esta configuração é ativada quando rulex.engine.velocity.redis.enabled=true e substitui o
 * ConcurrentMapCacheManager padrão, permitindo que os caches do sistema (@Cacheable) usem Redis.
 *
 * <p>Caches configurados com TTLs específicos:
 *
 * <ul>
 *   <li>binLookup - 24h (dados estáticos de BIN)
 *   <li>mccCategory - 24h (categorias MCC)
 *   <li>geoReference - 24h (dados geográficos)
 *   <li>geoPolygon - 24h (polígonos geográficos)
 *   <li>ruleConfigurations - 5min (regras podem mudar)
 *   <li>advancedRules - 5min (regras avançadas)
 *   <li>highRiskMcc - 12h (MCCs de alto risco)
 * </ul>
 */
@Configuration
@EnableCaching
@ConditionalOnProperty(
    name = "rulex.engine.velocity.redis.enabled",
    havingValue = "true",
    matchIfMissing = false)
@Slf4j
public class RedisCacheConfig {

  @Value("${rulex.redis-cache.key-prefix:rulex:cache:}")
  private String keyPrefix;

  @Value("${rulex.redis-cache.ttl.default:3600}")
  private long defaultTtlSeconds;

  @Value("${rulex.redis-cache.ttl.bin-lookup:86400}")
  private long binLookupTtlSeconds;

  @Value("${rulex.redis-cache.ttl.mcc-category:86400}")
  private long mccCategoryTtlSeconds;

  @Value("${rulex.redis-cache.ttl.geo-reference:86400}")
  private long geoReferenceTtlSeconds;

  @Value("${rulex.redis-cache.ttl.rules:300}")
  private long rulesTtlSeconds;

  @Value("${rulex.redis-cache.ttl.high-risk-mcc:43200}")
  private long highRiskMccTtlSeconds;

  /**
   * Cria o RedisCacheManager como CacheManager primário.
   *
   * @param connectionFactory Factory de conexão Redis
   * @return RedisCacheManager configurado
   */
  @Bean
  @Primary
  public CacheManager redisCacheManager(RedisConnectionFactory connectionFactory) {
    log.info("============================================================");
    log.info("RedisCacheManager INICIALIZANDO - Redis como backend de @Cacheable");
    log.info("Key prefix: {}", keyPrefix);
    log.info("TTLs configurados:");
    log.info("  - default: {}s", defaultTtlSeconds);
    log.info("  - binLookup: {}s", binLookupTtlSeconds);
    log.info("  - mccCategory: {}s", mccCategoryTtlSeconds);
    log.info("  - geoReference: {}s", geoReferenceTtlSeconds);
    log.info("  - ruleConfigurations/advancedRules: {}s", rulesTtlSeconds);
    log.info("  - highRiskMcc: {}s", highRiskMccTtlSeconds);
    log.info("============================================================");

    // Configuração padrão para caches
    RedisCacheConfiguration defaultConfig =
        RedisCacheConfiguration.defaultCacheConfig()
            .prefixCacheNameWith(keyPrefix)
            .entryTtl(Duration.ofSeconds(defaultTtlSeconds))
            .serializeKeysWith(
                RedisSerializationContext.SerializationPair.fromSerializer(
                    new StringRedisSerializer()))
            .serializeValuesWith(
                RedisSerializationContext.SerializationPair.fromSerializer(
                    new GenericJackson2JsonRedisSerializer()))
            .disableCachingNullValues();

    // Configurações específicas por cache
    Map<String, RedisCacheConfiguration> cacheConfigurations = new HashMap<>();

    // Dados estáticos - TTL longo (24h)
    RedisCacheConfiguration staticDataConfig =
        defaultConfig.entryTtl(Duration.ofSeconds(binLookupTtlSeconds));
    cacheConfigurations.put("binLookup", staticDataConfig);
    cacheConfigurations.put(
        "mccCategory", defaultConfig.entryTtl(Duration.ofSeconds(mccCategoryTtlSeconds)));
    cacheConfigurations.put(
        "geoReference", defaultConfig.entryTtl(Duration.ofSeconds(geoReferenceTtlSeconds)));
    cacheConfigurations.put(
        "geoPolygon", defaultConfig.entryTtl(Duration.ofSeconds(geoReferenceTtlSeconds)));

    // Regras - TTL curto (5min) para refletir mudanças rapidamente
    RedisCacheConfiguration rulesConfig =
        defaultConfig.entryTtl(Duration.ofSeconds(rulesTtlSeconds));
    cacheConfigurations.put("ruleConfigurations", rulesConfig);
    cacheConfigurations.put("advancedRules", rulesConfig);

    // MCC alto risco - TTL médio (12h)
    cacheConfigurations.put(
        "highRiskMcc", defaultConfig.entryTtl(Duration.ofSeconds(highRiskMccTtlSeconds)));

    RedisCacheManager cacheManager =
        RedisCacheManager.builder(connectionFactory)
            .cacheDefaults(defaultConfig)
            .withInitialCacheConfigurations(cacheConfigurations)
            .transactionAware()
            .build();

    log.info("RedisCacheManager INICIALIZADO com {} caches pré-configurados", 
        cacheConfigurations.size());

    return cacheManager;
  }
}
