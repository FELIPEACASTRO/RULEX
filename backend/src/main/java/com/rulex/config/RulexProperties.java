package com.rulex.config;

import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;
import org.springframework.validation.annotation.Validated;

/**
 * QUAL-002 FIX: Propriedades centralizadas do RULEX.
 * Substitui magic numbers hardcoded por configurações externalizáveis.
 * 
 * Uso: @Autowired RulexProperties props;
 * Exemplo: props.getLimits().getMaxRequestBodySize()
 */
@Configuration
@ConfigurationProperties(prefix = "rulex")
@Validated
@Data
public class RulexProperties {

    @NotNull
    private Limits limits = new Limits();

    @NotNull
    private Velocity velocity = new Velocity();

    @NotNull
    private Fraud fraud = new Fraud();

    @NotNull
    private Cache cache = new Cache();

    /**
     * Limites gerais do sistema.
     */
    @Data
    public static class Limits {
        /** Tamanho máximo do body de requisição em bytes (default: 1MB) */
        @Min(1024)
        @Max(104857600) // 100MB
        private long maxRequestBodySize = 1_000_000;

        /** Número máximo de regras avaliadas por transação */
        @Min(1)
        @Max(1000)
        private int maxRulesPerTransaction = 100;

        /** Número máximo de condições por regra */
        @Min(1)
        @Max(100)
        private int maxConditionsPerRule = 50;

        /** Número máximo de grupos aninhados por regra */
        @Min(1)
        @Max(10)
        private int maxNestedGroups = 5;

        /** Timeout de avaliação de regra em milissegundos */
        @Min(100)
        @Max(60000)
        private long ruleEvaluationTimeoutMs = 5000;
    }

    /**
     * Thresholds de velocidade para detecção de fraude.
     */
    @Data
    public static class Velocity {
        /** Threshold de transações em 5 minutos */
        @Min(1)
        private int threshold5min = 3;

        /** Threshold de transações em 1 hora */
        @Min(1)
        private int threshold1hour = 10;

        /** Threshold de transações diárias */
        @Min(1)
        private int thresholdDaily = 50;

        /** Threshold de merchants distintos em 24h */
        @Min(1)
        private int distinctMerchantsThreshold = 10;

        /** Threshold de países distintos em 24h */
        @Min(1)
        private int distinctCountriesThreshold = 5;

        /** Threshold de dispositivos distintos por cartão */
        @Min(1)
        private int maxDevicesPerCard = 5;
    }

    /**
     * Configurações de detecção de fraude.
     */
    @Data
    public static class Fraud {
        /** Valor mínimo considerado alto risco (BRL) */
        @Min(0)
        private double highRiskAmountThreshold = 10000.0;

        /** Distância máxima para viagem impossível (km) */
        @Min(1)
        private int impossibleTravelDistanceKm = 500;

        /** Tempo mínimo para viagem impossível (minutos) */
        @Min(1)
        private int impossibleTravelTimeMinutes = 60;

        /** Score mínimo de confiança do dispositivo */
        @Min(0)
        @Max(100)
        private int minDeviceTrustScore = 50;

        /** Percentual de desvio do valor médio para alerta */
        @Min(1)
        @Max(1000)
        private int amountDeviationPercentThreshold = 300;
    }

    /**
     * Configurações de cache.
     */
    @Data
    public static class Cache {
        /** TTL do cache de regras em segundos */
        @Min(1)
        private int rulesCacheTtlSeconds = 300;

        /** TTL do cache de velocity em segundos */
        @Min(1)
        private int velocityCacheTtlSeconds = 30;

        /** Tamanho máximo do cache de velocity */
        @Min(100)
        private int velocityCacheMaxSize = 10000;

        /** TTL do cache de BIN lookup em segundos */
        @Min(1)
        private int binCacheTtlSeconds = 3600;
    }
}
