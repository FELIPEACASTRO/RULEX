package com.rulex.v31.ast;

import static org.junit.jupiter.api.Assertions.*;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.v31.features.DbFeatureProvider;
import com.zaxxer.hikari.HikariDataSource;
import org.flywaydb.core.Flyway;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.jdbc.core.JdbcTemplate;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

@Testcontainers
class EnterpriseFunctionsIT {

  @Container
  @SuppressWarnings("resource")
  static final PostgreSQLContainer<?> postgres =
      new PostgreSQLContainer<>("postgres:16-alpine")
          .withDatabaseName("rulex_db")
          .withUsername("postgres")
          .withPassword("postgres");

  static HikariDataSource dataSource;
  static JdbcTemplate jdbc;
  static final ObjectMapper om = new ObjectMapper();
  static DbFeatureProvider featureProvider;

  @BeforeAll
  static void initDb() {
    dataSource = new HikariDataSource();
    dataSource.setJdbcUrl(postgres.getJdbcUrl());
    dataSource.setUsername(postgres.getUsername());
    dataSource.setPassword(postgres.getPassword());

    Flyway.configure().dataSource(dataSource).locations("classpath:db/migration").load().migrate();

    jdbc = new JdbcTemplate(dataSource);
    featureProvider = new DbFeatureProvider(jdbc, om);
  }

  @AfterAll
  static void shutdown() {
    if (dataSource != null) {
      dataSource.close();
    }
  }

  @BeforeEach
  void clean() {
    jdbc.execute("TRUNCATE TABLE holidays RESTART IDENTITY CASCADE");
    jdbc.execute("TRUNCATE TABLE feature_store RESTART IDENTITY CASCADE");
  }

  private boolean eval(String astJson, String payloadJson) throws Exception {
    AstEvaluator e = new AstEvaluator(featureProvider);
    return e.evaluate(om.readTree(astJson), om.readTree(payloadJson));
  }

  @Test
  void isHolidayReadsDb() throws Exception {
    jdbc.update(
        "INSERT INTO holidays(country, uf, date, name) VALUES (?,?,?,?)",
        "BR",
        "SP",
        java.sql.Date.valueOf("2025-01-01"),
        "Confraternização Universal");

    String ast =
        """
        {
          "type":"CONDITION",
          "left":{
            "type":"FUNC",
            "name":"IS_HOLIDAY",
            "returnType":"boolean",
            "args":["BR","SP",20250101]
          },
          "operator":"IS_TRUE"
        }
        """;

    assertTrue(eval(ast, "{}"));
  }

  @Test
  void isBusinessDayFalseOnHoliday() throws Exception {
    jdbc.update(
        "INSERT INTO holidays(country, uf, date, name) VALUES (?,?,?,?)",
        "BR",
        null,
        java.sql.Date.valueOf("2025-05-01"),
        "Dia do Trabalho");

    String ast =
        """
        {
          "type":"CONDITION",
          "left":{
            "type":"FUNC",
            "name":"IS_BUSINESS_DAY",
            "returnType":"boolean",
            "args":["BR",20250501]
          },
          "operator":"IS_FALSE"
        }
        """;

    assertTrue(eval(ast, "{}"));
  }

  @Test
  void timeSinceLastUsesFeatureStore() throws Exception {
    jdbc.update(
        "INSERT INTO feature_store(entity_key, feature_name, value_jsonb) VALUES (?,?,?::jsonb)",
        "user-1",
        "last_event_ts:LOGIN",
        "{\"ts\":\"2025-01-01T00:00:00Z\"}");

    String ast =
        """
        {
          "type":"CONDITION",
          "left":{
            "type":"FUNC",
            "name":"TIME_SINCE_LAST",
            "returnType":"number",
            "args":["LOGIN","user-1","2025-01-01T00:00:10Z"]
          },
          "operator":"EQ",
          "right": 10
        }
        """;

    assertTrue(eval(ast, "{}"));
  }

  @Test
  void similarityJaroHighForIdenticalStrings() throws Exception {
    String ast =
        """
        {
          "type":"CONDITION",
          "left":{
            "type":"FUNC",
            "name":"SIMILARITY_JARO",
            "returnType":"number",
            "args":["abc","abc"]
          },
          "operator":"GTE",
          "right": 0.99
        }
        """;

    assertTrue(eval(ast, "{}"));
  }

  @Test
  void jsonPathExistsResolvesAgainstPayload() throws Exception {
    String ast =
        """
        {
          "type":"CONDITION",
          "left":{
            "type":"FUNC",
            "name":"JSON_PATH_EXISTS",
            "returnType":"boolean",
            "args":["$.pan"]
          },
          "operator":"IS_TRUE"
        }
        """;

    assertTrue(eval(ast, "{\"pan\":\"411111\"}"));
  }

  @Test
  void unknownFieldsPresentDetectsUnexpectedRootKeys() throws Exception {
    String ast =
        """
        {
          "type":"CONDITION",
          "left":{
            "type":"FUNC",
            "name":"UNKNOWN_FIELDS_PRESENT",
            "returnType":"boolean",
            "args":[]
          },
          "operator":"IS_TRUE"
        }
        """;

    assertTrue(eval(ast, "{\"unknownRoot\":123}"));
  }
}
