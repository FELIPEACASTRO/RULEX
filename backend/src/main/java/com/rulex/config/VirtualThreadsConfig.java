package com.rulex.config;

import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import org.springframework.boot.web.embedded.tomcat.TomcatProtocolHandlerCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class VirtualThreadsConfig {

  @Bean(destroyMethod = "shutdown")
  public Executor virtualThreadPerTaskExecutor() {
    return Executors.newVirtualThreadPerTaskExecutor();
  }

  @Bean
  public TomcatProtocolHandlerCustomizer<?> tomcatVirtualThreadsCustomizer(
      Executor virtualThreadPerTaskExecutor) {
    return protocolHandler -> protocolHandler.setExecutor(virtualThreadPerTaskExecutor);
  }
}
