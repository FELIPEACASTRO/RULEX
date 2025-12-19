package com.rulex.config;

import java.util.Arrays;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.lang.NonNull;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

/** Configuração de CORS para permitir requisições do frontend. */
@Configuration
public class CorsConfig implements WebMvcConfigurer {

  /** Lista CSV de origens permitidas. Ex.: http://localhost:5173,http://localhost:3000 */
  @Value("${rulex.cors.allowed-origins:http://localhost:5173,http://localhost:3000}")
  private String allowedOriginsCsv;

  @Override
  public void addCorsMappings(@NonNull CorsRegistry registry) {
    String csv = allowedOriginsCsv == null ? "" : allowedOriginsCsv;
    String[] allowedOrigins =
        Arrays.stream(csv.split(","))
            .map(String::trim)
            .filter(s -> !s.isBlank())
            .toArray(String[]::new);

    registry
        .addMapping("/**")
        .allowedOrigins(allowedOrigins)
        .allowedMethods("GET", "POST", "PUT", "DELETE", "PATCH", "OPTIONS")
        .allowedHeaders("*")
        .maxAge(3600);
  }
}
