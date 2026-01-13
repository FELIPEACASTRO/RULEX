package com.rulex.config;

import java.util.concurrent.Executor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.AsyncConfigurer;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

/**
 * Configuração de execução assíncrona para operações I/O-bound.
 *
 * <p>Define pools de threads otimizados para: - Operações de enriquecimento (BIN lookup, MCC
 * lookup) - Operações de geolocalização - Operações de auditoria
 *
 * <p>Com virtual threads habilitadas no Spring Boot 3.x, o executor padrão já utiliza virtual
 * threads. Esta configuração fornece executores nomeados para melhor observabilidade.
 */
@Configuration
public class AsyncConfig implements AsyncConfigurer {

  /** Executor padrão para operações assíncronas. Usado quando @Async não especifica um executor. */
  @Override
  @Bean("taskExecutor")
  public Executor getAsyncExecutor() {
    ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
    executor.setCorePoolSize(4);
    executor.setMaxPoolSize(10);
    executor.setQueueCapacity(100);
    executor.setThreadNamePrefix("rulex-async-");
    executor.setWaitForTasksToCompleteOnShutdown(true);
    executor.setAwaitTerminationSeconds(30);
    executor.initialize();
    return executor;
  }

  /** Executor dedicado para operações de enriquecimento. BIN lookup, MCC lookup, etc. */
  @Bean("enrichmentExecutor")
  public Executor enrichmentExecutor() {
    ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
    executor.setCorePoolSize(2);
    executor.setMaxPoolSize(8);
    executor.setQueueCapacity(50);
    executor.setThreadNamePrefix("rulex-enrichment-");
    executor.setWaitForTasksToCompleteOnShutdown(true);
    executor.setAwaitTerminationSeconds(15);
    executor.initialize();
    return executor;
  }

  /**
   * Executor dedicado para operações de geolocalização. Cálculos de distância, verificação de
   * polígonos, etc.
   */
  @Bean("geoExecutor")
  public Executor geoExecutor() {
    ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
    executor.setCorePoolSize(2);
    executor.setMaxPoolSize(6);
    executor.setQueueCapacity(50);
    executor.setThreadNamePrefix("rulex-geo-");
    executor.setWaitForTasksToCompleteOnShutdown(true);
    executor.setAwaitTerminationSeconds(15);
    executor.initialize();
    return executor;
  }

  /** Executor dedicado para operações de auditoria. Logging assíncrono de eventos de auditoria. */
  @Bean("auditExecutor")
  public Executor auditExecutor() {
    ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
    executor.setCorePoolSize(2);
    executor.setMaxPoolSize(4);
    executor.setQueueCapacity(200);
    executor.setThreadNamePrefix("rulex-audit-");
    executor.setWaitForTasksToCompleteOnShutdown(true);
    executor.setAwaitTerminationSeconds(60); // Mais tempo para auditoria
    executor.initialize();
    return executor;
  }
}
