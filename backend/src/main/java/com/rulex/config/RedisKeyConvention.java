package com.rulex.config;

import java.time.Duration;

/**
 * Convenção de nomenclatura para chaves Redis.
 * 
 * <p>Padrão: rulex:{module}:{entity}:{identifier}:{suffix}
 * 
 * <p>Exemplos:
 * <ul>
 *   <li>rulex:velocity:count:pan:abc123:5min</li>
 *   <li>rulex:cache:rules:rule-001</li>
 *   <li>rulex:session:user-123</li>
 * </ul>
 */
public final class RedisKeyConvention {

    private RedisKeyConvention() {
        // Utility class
    }

    // ========== PREFIXOS ==========
    
    /** Prefixo base para todas as chaves RULEX */
    public static final String PREFIX = "rulex:";
    
    // Velocity
    public static final String VELOCITY_COUNT = PREFIX + "velocity:count:";
    public static final String VELOCITY_SUM = PREFIX + "velocity:sum:";
    public static final String VELOCITY_DISTINCT = PREFIX + "velocity:distinct:";
    public static final String VELOCITY_AVG = PREFIX + "velocity:avg:";
    public static final String VELOCITY_MAX = PREFIX + "velocity:max:";
    public static final String VELOCITY_MIN = PREFIX + "velocity:min:";
    
    // Cache
    public static final String CACHE_RULES = PREFIX + "cache:rules:";
    public static final String CACHE_GEO = PREFIX + "cache:geo:";
    public static final String CACHE_BIN = PREFIX + "cache:bin:";
    public static final String CACHE_MERCHANT = PREFIX + "cache:merchant:";
    
    // Session
    public static final String SESSION = PREFIX + "session:";
    
    // Rate Limiting
    public static final String RATE_LIMIT = PREFIX + "ratelimit:";
    
    // Bloom Filters
    public static final String BLOOM = PREFIX + "bloom:";
    
    // Locks
    public static final String LOCK = PREFIX + "lock:";

    // ========== TTLs PADRÃO ==========
    
    /** TTL para contadores de 5 minutos (6 min para margem) */
    public static final Duration TTL_VELOCITY_5MIN = Duration.ofMinutes(6);
    
    /** TTL para contadores de 15 minutos (16 min para margem) */
    public static final Duration TTL_VELOCITY_15MIN = Duration.ofMinutes(16);
    
    /** TTL para contadores de 30 minutos (32 min para margem) */
    public static final Duration TTL_VELOCITY_30MIN = Duration.ofMinutes(32);
    
    /** TTL para contadores de 1 hora (65 min para margem) */
    public static final Duration TTL_VELOCITY_1H = Duration.ofMinutes(65);
    
    /** TTL para contadores de 6 horas */
    public static final Duration TTL_VELOCITY_6H = Duration.ofHours(7);
    
    /** TTL para contadores de 12 horas */
    public static final Duration TTL_VELOCITY_12H = Duration.ofHours(13);
    
    /** TTL para contadores de 24 horas (25h para margem) */
    public static final Duration TTL_VELOCITY_24H = Duration.ofHours(25);
    
    /** TTL para cache de regras */
    public static final Duration TTL_CACHE_RULES = Duration.ofMinutes(5);
    
    /** TTL para cache de geolocalização */
    public static final Duration TTL_CACHE_GEO = Duration.ofHours(24);
    
    /** TTL para cache de BIN */
    public static final Duration TTL_CACHE_BIN = Duration.ofHours(24);
    
    /** TTL para sessão */
    public static final Duration TTL_SESSION = Duration.ofHours(8);
    
    /** TTL para rate limiting */
    public static final Duration TTL_RATE_LIMIT = Duration.ofMinutes(1);
    
    /** TTL para locks distribuídos */
    public static final Duration TTL_LOCK = Duration.ofSeconds(30);

    // ========== MÉTODOS UTILITÁRIOS ==========

    /**
     * Gera chave para contador de velocidade.
     * @param keyType Tipo da chave (PAN, CUSTOMER, MERCHANT)
     * @param identifier Identificador (hash do PAN, customerId, etc.)
     * @param window Janela temporal (5min, 1h, 24h, etc.)
     * @return Chave formatada
     */
    public static String velocityCountKey(String keyType, String identifier, String window) {
        return VELOCITY_COUNT + keyType.toLowerCase() + ":" + identifier + ":" + window;
    }

    /**
     * Gera chave para soma de velocidade.
     */
    public static String velocitySumKey(String keyType, String identifier, String window) {
        return VELOCITY_SUM + keyType.toLowerCase() + ":" + identifier + ":" + window;
    }

    /**
     * Gera chave para contador de distintos (HyperLogLog).
     */
    public static String velocityDistinctKey(String keyType, String identifier, String distinctType) {
        return VELOCITY_DISTINCT + keyType.toLowerCase() + ":" + identifier + ":" + distinctType;
    }

    /**
     * Gera chave para cache de regra.
     */
    public static String cacheRuleKey(String ruleId) {
        return CACHE_RULES + ruleId;
    }

    /**
     * Gera chave para cache de geolocalização.
     */
    public static String cacheGeoKey(String location) {
        return CACHE_GEO + sanitizeKey(location);
    }

    /**
     * Gera chave para cache de BIN.
     */
    public static String cacheBinKey(String bin) {
        return CACHE_BIN + bin;
    }

    /**
     * Gera chave para sessão de usuário.
     */
    public static String sessionKey(String userId) {
        return SESSION + userId;
    }

    /**
     * Gera chave para rate limiting.
     */
    public static String rateLimitKey(String identifier) {
        return RATE_LIMIT + identifier;
    }

    /**
     * Gera chave para bloom filter.
     */
    public static String bloomKey(String filterName) {
        return BLOOM + filterName;
    }

    /**
     * Gera chave para lock distribuído.
     */
    public static String lockKey(String resource) {
        return LOCK + resource;
    }

    /**
     * Sanitiza uma string para uso como parte de chave Redis.
     * Remove caracteres especiais e converte para lowercase.
     */
    public static String sanitizeKey(String input) {
        if (input == null) return "null";
        return input.toLowerCase()
            .replaceAll("[^a-z0-9_-]", "_")
            .replaceAll("_+", "_");
    }

    /**
     * Retorna o TTL apropriado para uma janela de velocidade.
     */
    public static Duration getTtlForWindow(String window) {
        return switch (window.toLowerCase()) {
            case "5min", "5m" -> TTL_VELOCITY_5MIN;
            case "15min", "15m" -> TTL_VELOCITY_15MIN;
            case "30min", "30m" -> TTL_VELOCITY_30MIN;
            case "1h", "1hour" -> TTL_VELOCITY_1H;
            case "6h", "6hour" -> TTL_VELOCITY_6H;
            case "12h", "12hour" -> TTL_VELOCITY_12H;
            case "24h", "24hour", "1d" -> TTL_VELOCITY_24H;
            default -> TTL_VELOCITY_24H;
        };
    }
}
