package com.rulex.db;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;
import org.junit.jupiter.api.Test;
import org.springframework.boot.jdbc.DataSourceBuilder;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.flywaydb.core.Flyway;

@Testcontainers
class FlywayMigrationsTest {

  @Container
  static final PostgreSQLContainer<?> POSTGRES =
      new PostgreSQLContainer<>("postgres:16-alpine")
          .withDatabaseName("rulex_test")
          .withUsername("rulex")
          .withPassword("rulex");

  @Test
  void migrations_apply_and_expected_tables_exist() throws Exception {
    var dataSource =
        DataSourceBuilder.create()
            .url(POSTGRES.getJdbcUrl())
            .username(POSTGRES.getUsername())
            .password(POSTGRES.getPassword())
            .driverClassName("org.postgresql.Driver")
            .build();

    Flyway.configure().dataSource(dataSource).locations("classpath:db/migration").load().migrate();

    try (Connection c = dataSource.getConnection();
        Statement s = c.createStatement()) {
      assertTrue(tableExists(s, "transactions"));
      assertTrue(tableExists(s, "rule_configurations"));
      assertTrue(tableExists(s, "transaction_raw_store"));
    }
  }

  private static boolean tableExists(Statement s, String tableName) throws Exception {
    try (ResultSet rs =
        s.executeQuery(
            "select 1 from information_schema.tables where table_schema='public' and table_name='"
                + tableName
                + "'")) {
      return rs.next();
    }
  }
}
