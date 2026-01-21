package com.rulex.config;

/**
 * Prefixos padronizados para chaves Redis.
 * GAP-007: Padronizar prefixos Redis
 * 
 * <p>Convenção de nomenclatura:
 * <ul>
 *   <li>rulex:velocity:* - Contadores de velocity
 *   <li>rulex:cache:* - Cache geral
 *   <li>rulex:session:* - Sessões de usuário
 *   <li>rulex:rule:* - Cache de regras
 *   <li>rulex:bloom:* - Bloom filters
 *   <li>rulex:lock:* - Locks distribuídos
 * </ul>
 */
public final class RedisKeyPrefixes {

    private RedisKeyPrefixes() {
        // Utility class
    }

    // ========== PREFIXOS BASE ==========
    
    /** Prefixo raiz para todas as chaves RULEX */
    public static final String ROOT = "rulex:";
    
    // ========== VELOCITY ==========
    
    /** Prefixo para contadores de velocity */
    public static final String VELOCITY = ROOT + "velocity:";
    
    /** Prefixo para contagem por customer */
    public static final String VELOCITY_CUSTOMER = VELOCITY + "customer:";
    
    /** Prefixo para contagem por account */
    public static final String VELOCITY_ACCOUNT = VELOCITY + "account:";
    
    /** Prefixo para contagem por device */
    public static final String VELOCITY_DEVICE = VELOCITY + "device:";
    
    /** Prefixo para contagem por IP */
    public static final String VELOCITY_IP = VELOCITY + "ip:";
    
    /** Prefixo para contagem por merchant */
    public static final String VELOCITY_MERCHANT = VELOCITY + "merchant:";
    
    /** Prefixo para contagem por PAN (hash) */
    public static final String VELOCITY_PAN = VELOCITY + "pan:";
    
    /** Prefixo para somas de valores */
    public static final String VELOCITY_SUM = VELOCITY + "sum:";
    
    /** Prefixo para contagem de distintos */
    public static final String VELOCITY_DISTINCT = VELOCITY + "distinct:";
    
    // ========== CACHE ==========
    
    /** Prefixo para cache geral */
    public static final String CACHE = ROOT + "cache:";
    
    /** Prefixo para cache de regras */
    public static final String CACHE_RULES = CACHE + "rules:";
    
    /** Prefixo para cache de configurações */
    public static final String CACHE_CONFIG = CACHE + "config:";
    
    /** Prefixo para cache de listas (blacklist, whitelist) */
    public static final String CACHE_LISTS = CACHE + "lists:";
    
    // ========== SESSÕES ==========
    
    /** Prefixo para sessões de usuário */
    public static final String SESSION = ROOT + "session:";
    
    /** Prefixo para sessões de API */
    public static final String SESSION_API = SESSION + "api:";
    
    // ========== REGRAS ==========
    
    /** Prefixo para dados de regras */
    public static final String RULE = ROOT + "rule:";
    
    /** Prefixo para resultados de avaliação */
    public static final String RULE_RESULT = RULE + "result:";
    
    /** Prefixo para métricas de regras */
    public static final String RULE_METRICS = RULE + "metrics:";
    
    // ========== BLOOM FILTERS ==========
    
    /** Prefixo para bloom filters */
    public static final String BLOOM = ROOT + "bloom:";
    
    /** Prefixo para bloom filter de PANs vistos */
    public static final String BLOOM_PAN = BLOOM + "pan:";
    
    /** Prefixo para bloom filter de devices vistos */
    public static final String BLOOM_DEVICE = BLOOM + "device:";
    
    // ========== LOCKS ==========
    
    /** Prefixo para locks distribuídos */
    public static final String LOCK = ROOT + "lock:";
    
    /** Prefixo para locks de processamento */
    public static final String LOCK_PROCESSING = LOCK + "processing:";
    
    // ========== MÉTODOS UTILITÁRIOS ==========
    
    /**
     * Gera chave de velocity para customer.
     * @param customerId ID do customer
     * @param windowMinutes Janela de tempo em minutos
     * @return Chave formatada
     */
    public static String velocityCustomerKey(String customerId, int windowMinutes) {
        return VELOCITY_CUSTOMER + customerId + ":" + windowMinutes + "m";
    }
    
    /**
     * Gera chave de velocity para account.
     * @param accountId ID da conta
     * @param windowMinutes Janela de tempo em minutos
     * @return Chave formatada
     */
    public static String velocityAccountKey(String accountId, int windowMinutes) {
        return VELOCITY_ACCOUNT + accountId + ":" + windowMinutes + "m";
    }
    
    /**
     * Gera chave de velocity para device.
     * @param deviceId ID do device
     * @param windowMinutes Janela de tempo em minutos
     * @return Chave formatada
     */
    public static String velocityDeviceKey(String deviceId, int windowMinutes) {
        return VELOCITY_DEVICE + deviceId + ":" + windowMinutes + "m";
    }
    
    /**
     * Gera chave de velocity para IP.
     * @param ipAddress Endereço IP
     * @param windowMinutes Janela de tempo em minutos
     * @return Chave formatada
     */
    public static String velocityIpKey(String ipAddress, int windowMinutes) {
        return VELOCITY_IP + ipAddress + ":" + windowMinutes + "m";
    }
    
    /**
     * Gera chave de cache para regra.
     * @param ruleId ID da regra
     * @return Chave formatada
     */
    public static String cacheRuleKey(String ruleId) {
        return CACHE_RULES + ruleId;
    }
    
    /**
     * Gera chave de bloom filter para PAN.
     * @param panHash Hash do PAN
     * @return Chave formatada
     */
    public static String bloomPanKey(String panHash) {
        return BLOOM_PAN + panHash;
    }
    
    /**
     * Gera chave de lock para processamento.
     * @param transactionId ID da transação
     * @return Chave formatada
     */
    public static String lockProcessingKey(String transactionId) {
        return LOCK_PROCESSING + transactionId;
    }
}
