package com.rulex.testsupport;

import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

@Testcontainers
public abstract class CorePostgresITSupport {

  @Container
  @SuppressWarnings("resource")
  static final PostgreSQLContainer<?> postgres =
      new PostgreSQLContainer<>("postgres:16-alpine")
          .withDatabaseName("rulex_db")
          .withUsername("postgres")
          .withPassword("postgres");

  @DynamicPropertySource
  static void coreProps(DynamicPropertyRegistry r) {
    if (!postgres.isRunning()) {
      postgres.start();
    }
    r.add("spring.datasource.url", postgres::getJdbcUrl);
    r.add("spring.datasource.username", postgres::getUsername);
    r.add("spring.datasource.password", postgres::getPassword);
    r.add("spring.datasource.driver-class-name", () -> "org.postgresql.Driver");
    // Temporariamente usar update para depuração (retornar a validate depois)
    r.add("spring.jpa.hibernate.ddl-auto", () -> "none");
    r.add("spring.flyway.enabled", () -> "true");
    // Ensure HikariCP creates the datasource
    r.add("spring.datasource.hikari.connection-timeout", () -> "30000");
  }
}
