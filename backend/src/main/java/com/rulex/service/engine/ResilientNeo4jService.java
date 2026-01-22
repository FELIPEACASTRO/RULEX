package com.rulex.service.engine;

import com.rulex.service.Neo4jGraphService;
import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import io.github.resilience4j.retry.annotation.Retry;
import io.github.resilience4j.timelimiter.annotation.TimeLimiter;
import io.micrometer.core.instrument.Counter;
import io.micrometer.core.instrument.MeterRegistry;
import jakarta.annotation.PostConstruct;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

/**
 * Wrapper resiliente para Neo4jGraphService com Circuit Breaker, Retry e Timeout.
 *
 * <p>Este serviço encapsula todas as chamadas ao Neo4j com padrões de resiliência:
 * <ul>
 *   <li>Circuit Breaker - evita chamadas quando Neo4j está indisponível</li>
 *   <li>Retry - tenta novamente em caso de falhas transientes</li>
 *   <li>Time Limiter - timeout para evitar bloqueio</li>
 *   <li>Fallback - valores conservadores quando Neo4j falha</li>
 * </ul>
 *
 * <p>Configuração via application.yml:
 * <pre>
 * resilience4j:
 *   circuitbreaker:
 *     instances:
 *       neo4j:
 *         slidingWindowSize: 10
 *         failureRateThreshold: 50
 *         waitDurationInOpenState: 30s
 *   retry:
 *     instances:
 *       neo4j:
 *         maxAttempts: 3
 *         waitDuration: 500ms
 *   timelimiter:
 *     instances:
 *       neo4j:
 *         timeoutDuration: 2s
 * </pre>
 *
 * @author Refactoring - RULEX Team
 * @since 2.0.0
 */
@Service
@Slf4j
public class ResilientNeo4jService {

    private static final String CIRCUIT_BREAKER_NAME = "neo4j";
    private static final int FALLBACK_COMPONENT_ID = -1;
    private static final int FALLBACK_DEGREE = 0;
    private static final double FALLBACK_PAGERANK = 0.0;
    private static final boolean FALLBACK_IN_FRAUD_RING = false;

    private final Neo4jGraphService neo4jGraphService;
    private final MeterRegistry meterRegistry;

    private Counter fallbackCounter;
    private Counter successCounter;
    private Counter errorCounter;

    @Value("${rulex.neo4j.enabled:true}")
    private boolean neo4jEnabled;

    public ResilientNeo4jService(Neo4jGraphService neo4jGraphService, MeterRegistry meterRegistry) {
        this.neo4jGraphService = neo4jGraphService;
        this.meterRegistry = meterRegistry;
    }

    @PostConstruct
    public void initMetrics() {
        fallbackCounter = Counter.builder("rulex.neo4j.fallback")
            .description("Number of Neo4j fallback invocations")
            .register(meterRegistry);

        successCounter = Counter.builder("rulex.neo4j.success")
            .description("Number of successful Neo4j calls")
            .register(meterRegistry);

        errorCounter = Counter.builder("rulex.neo4j.error")
            .description("Number of Neo4j errors")
            .register(meterRegistry);
    }

    /**
     * NEO001: Weakly Connected Components - Detecta clusters de contas conectadas.
     */
    @CircuitBreaker(name = CIRCUIT_BREAKER_NAME, fallbackMethod = "getComponentIdFallback")
    @Retry(name = CIRCUIT_BREAKER_NAME)
    @TimeLimiter(name = CIRCUIT_BREAKER_NAME)
    public CompletableFuture<Integer> getWeaklyConnectedComponentId(String accountId) {
        return CompletableFuture.supplyAsync(() -> {
            if (!neo4jEnabled) {
                return FALLBACK_COMPONENT_ID;
            }
            int result = neo4jGraphService.getWeaklyConnectedComponentId(accountId);
            successCounter.increment();
            return result;
        });
    }

    public CompletableFuture<Integer> getComponentIdFallback(String accountId, Throwable t) {
        logFallback("getWeaklyConnectedComponentId", accountId, t);
        return CompletableFuture.completedFuture(FALLBACK_COMPONENT_ID);
    }

    /**
     * NEO002: Degree Centrality - Conta conexões de uma conta.
     */
    @CircuitBreaker(name = CIRCUIT_BREAKER_NAME, fallbackMethod = "getDegreeFallback")
    @Retry(name = CIRCUIT_BREAKER_NAME)
    @TimeLimiter(name = CIRCUIT_BREAKER_NAME)
    public CompletableFuture<Integer> getDegreeCentrality(String accountId) {
        return CompletableFuture.supplyAsync(() -> {
            if (!neo4jEnabled) {
                return FALLBACK_DEGREE;
            }
            int result = neo4jGraphService.getDegreeCentrality(accountId);
            successCounter.increment();
            return result;
        });
    }

    public CompletableFuture<Integer> getDegreeFallback(String accountId, Throwable t) {
        logFallback("getDegreeCentrality", accountId, t);
        return CompletableFuture.completedFuture(FALLBACK_DEGREE);
    }

    /**
     * NEO003: PageRank - Importância de uma conta na rede.
     */
    @CircuitBreaker(name = CIRCUIT_BREAKER_NAME, fallbackMethod = "getPageRankFallback")
    @Retry(name = CIRCUIT_BREAKER_NAME)
    @TimeLimiter(name = CIRCUIT_BREAKER_NAME)
    public CompletableFuture<Double> getPageRank(String accountId) {
        return CompletableFuture.supplyAsync(() -> {
            if (!neo4jEnabled) {
                return FALLBACK_PAGERANK;
            }
            double result = neo4jGraphService.getPageRankScore(accountId);
            successCounter.increment();
            return result;
        });
    }

    public CompletableFuture<Double> getPageRankFallback(String accountId, Throwable t) {
        logFallback("getPageRank", accountId, t);
        return CompletableFuture.completedFuture(FALLBACK_PAGERANK);
    }

    /**
     * NEO004: Fraud Ring Detection - Verifica se conta está em anel de fraude.
     */
    @CircuitBreaker(name = CIRCUIT_BREAKER_NAME, fallbackMethod = "isInFraudRingFallback")
    @Retry(name = CIRCUIT_BREAKER_NAME)
    @TimeLimiter(name = CIRCUIT_BREAKER_NAME)
    public CompletableFuture<Boolean> isInFraudRing(String accountId) {
        return CompletableFuture.supplyAsync(() -> {
            if (!neo4jEnabled) {
                return FALLBACK_IN_FRAUD_RING;
            }
            boolean result = neo4jGraphService.isInFraudRing(accountId, 3); // min ring size = 3
            successCounter.increment();
            return result;
        });
    }

    public CompletableFuture<Boolean> isInFraudRingFallback(String accountId, Throwable t) {
        logFallback("isInFraudRing", accountId, t);
        return CompletableFuture.completedFuture(FALLBACK_IN_FRAUD_RING);
    }

    /**
     * NEO005: Second Level Fraudster Detection - Detecta fraudadores de segundo nível.
     */
    @CircuitBreaker(name = CIRCUIT_BREAKER_NAME, fallbackMethod = "isSecondLevelFraudsterFallback")
    @Retry(name = CIRCUIT_BREAKER_NAME)
    @TimeLimiter(name = CIRCUIT_BREAKER_NAME)
    public CompletableFuture<Boolean> isSecondLevelFraudster(String accountId) {
        return CompletableFuture.supplyAsync(() -> {
            if (!neo4jEnabled) {
                return false;
            }
            boolean result = neo4jGraphService.isSecondLevelFraudster(accountId);
            successCounter.increment();
            return result;
        });
    }

    public CompletableFuture<Boolean> isSecondLevelFraudsterFallback(String accountId, Throwable t) {
        logFallback("isSecondLevelFraudster", accountId, t);
        return CompletableFuture.completedFuture(false);
    }

    /**
     * NEO006: Betweenness Centrality - Mede importância de um nó como ponte.
     */
    @CircuitBreaker(name = CIRCUIT_BREAKER_NAME, fallbackMethod = "getBetweennessCentralityFallback")
    @Retry(name = CIRCUIT_BREAKER_NAME)
    @TimeLimiter(name = CIRCUIT_BREAKER_NAME)
    public CompletableFuture<Double> getBetweennessCentrality(String accountId) {
        return CompletableFuture.supplyAsync(() -> {
            if (!neo4jEnabled) {
                return 0.0;
            }
            double result = neo4jGraphService.getBetweennessCentrality(accountId);
            successCounter.increment();
            return result;
        });
    }

    public CompletableFuture<Double> getBetweennessCentralityFallback(String accountId, Throwable t) {
        logFallback("getBetweennessCentrality", accountId, t);
        return CompletableFuture.completedFuture(0.0);
    }

    /**
     * Verifica se Neo4j está disponível.
     */
    public boolean isAvailable() {
        if (!neo4jEnabled) {
            return false;
        }
        return neo4jGraphService.isAvailable();
    }

    /**
     * Verifica se Neo4j está habilitado.
     */
    public boolean isEnabled() {
        return neo4jEnabled;
    }

    /**
     * Registra fallback e métricas.
     */
    private void logFallback(String method, String identifier, Throwable t) {
        fallbackCounter.increment();
        errorCounter.increment();

        if (t != null) {
            log.warn("Neo4j {} fallback triggered for {}: {} - {}",
                method, identifier, t.getClass().getSimpleName(), t.getMessage());
        } else {
            log.debug("Neo4j {} fallback triggered for {} (no error)", method, identifier);
        }
    }

    /**
     * Retorna estatísticas do serviço.
     */
    public ServiceStats getStats() {
        return new ServiceStats(
            neo4jEnabled,
            isAvailable(),
            (long) successCounter.count(),
            (long) errorCounter.count(),
            (long) fallbackCounter.count()
        );
    }

    /**
     * Estatísticas do serviço.
     */
    public record ServiceStats(
        boolean enabled,
        boolean available,
        long successCount,
        long errorCount,
        long fallbackCount
    ) {
        public double successRate() {
            long total = successCount + errorCount;
            return total > 0 ? (successCount * 100.0 / total) : 100.0;
        }
    }
}
