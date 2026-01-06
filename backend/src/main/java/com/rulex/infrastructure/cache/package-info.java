/**
 * Cache infrastructure adapters.
 *
 * <p>Provides implementations of the {@link com.rulex.application.port.out.RuleCachePort} interface:
 *
 * <ul>
 *   <li>{@link InMemoryCacheAdapter} - Simple in-memory cache using ConcurrentHashMap (default)
 *   <li>{@link RedisCacheAdapter} - Distributed cache using Redis (when enabled)
 * </ul>
 *
 * <p>The RedisCacheAdapter is marked as @Primary and activates when spring.data.redis.enabled=true.
 */
package com.rulex.infrastructure.cache;
