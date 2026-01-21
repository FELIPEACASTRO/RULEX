package com.rulex.config;

import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.SchedulingConfigurer;
import org.springframework.scheduling.config.ScheduledTaskRegistrar;

/**
 * Configuração do scheduler para jobs agendados.
 *
 * <p>Jobs agendados incluem:
 * <ul>
 *   <li>BloomFilterService - rebuild de filtros</li>
 *   <li>DeviceFingerprintService - cleanup de fingerprints</li>
 *   <li>ImpossibleTravelService - cleanup de histórico de localização</li>
 *   <li>RedisVelocityService - manutenção de contadores</li>
 *   <li>RuleOrderingService - recompute de ordenação</li>
 *   <li>ShadowModeService - reset de estatísticas</li>
 * </ul>
 */
@Configuration
@Slf4j
public class SchedulingConfig implements SchedulingConfigurer {

    private static final int POOL_SIZE = 4;

    @Override
    public void configureTasks(ScheduledTaskRegistrar taskRegistrar) {
        taskRegistrar.setScheduler(scheduledTaskExecutor());
        log.info("Scheduler configurado com pool de {} threads", POOL_SIZE);
    }

    @Bean(destroyMethod = "shutdown")
    public ScheduledExecutorService scheduledTaskExecutor() {
        java.util.concurrent.atomic.AtomicInteger counter = new java.util.concurrent.atomic.AtomicInteger(0);
        return Executors.newScheduledThreadPool(POOL_SIZE, r -> {
            Thread t = new Thread(r);
            t.setName("rulex-scheduler-" + counter.incrementAndGet());
            t.setDaemon(true);
            return t;
        });
    }
}
