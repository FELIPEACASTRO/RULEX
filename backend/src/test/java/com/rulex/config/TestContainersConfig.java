package com.rulex.config;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import java.util.HashMap;
import java.util.Map;
import javax.sql.DataSource;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Primary;
import org.springframework.orm.jpa.JpaTransactionManager;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;
import org.springframework.orm.jpa.vendor.HibernateJpaVendorAdapter;
import org.springframework.transaction.PlatformTransactionManager;
import org.testcontainers.containers.PostgreSQLContainer;

/**
 * Configuração de teste que cria um DataSource usando Testcontainers. Esta configuração é usada
 * para garantir que o DataSource seja criado antes dos repositórios JPA.
 */
@TestConfiguration
public class TestContainersConfig {

  private static final PostgreSQLContainer<?> postgres;

  static {
    postgres =
        new PostgreSQLContainer<>("postgres:16-alpine")
            .withDatabaseName("rulex_db")
            .withUsername("postgres")
            .withPassword("postgres");
    postgres.start();
  }

  @Bean
  @Primary
  public DataSource dataSource() {
    HikariConfig config = new HikariConfig();
    config.setJdbcUrl(postgres.getJdbcUrl());
    config.setUsername(postgres.getUsername());
    config.setPassword(postgres.getPassword());
    config.setDriverClassName("org.postgresql.Driver");
    config.setPoolName("TestHikariPool");
    config.setMinimumIdle(2);
    config.setMaximumPoolSize(5);
    config.setConnectionTimeout(30000);
    config.setAutoCommit(true);
    return new HikariDataSource(config);
  }

  @Bean
  @Primary
  public LocalContainerEntityManagerFactoryBean entityManagerFactory(DataSource dataSource) {
    LocalContainerEntityManagerFactoryBean em = new LocalContainerEntityManagerFactoryBean();
    em.setDataSource(dataSource);
    em.setPackagesToScan(
        "com.rulex.entity",
        "com.rulex.entity.complex",
        "com.rulex.entity.homolog",
        "com.rulex.v31.execlog",
        "com.rulex.v31.field");

    HibernateJpaVendorAdapter vendorAdapter = new HibernateJpaVendorAdapter();
    vendorAdapter.setGenerateDdl(true);
    vendorAdapter.setShowSql(false);
    em.setJpaVendorAdapter(vendorAdapter);

    Map<String, Object> properties = new HashMap<>();
    properties.put("hibernate.hbm2ddl.auto", "create-drop");
    properties.put("hibernate.dialect", "org.hibernate.dialect.PostgreSQLDialect");
    properties.put("hibernate.format_sql", false);
    em.setJpaPropertyMap(properties);

    return em;
  }

  @Bean
  @Primary
  public PlatformTransactionManager transactionManager(
      LocalContainerEntityManagerFactoryBean entityManagerFactory) {
    JpaTransactionManager transactionManager = new JpaTransactionManager();
    transactionManager.setEntityManagerFactory(entityManagerFactory.getObject());
    return transactionManager;
  }

  public static String getJdbcUrl() {
    return postgres.getJdbcUrl();
  }

  public static String getUsername() {
    return postgres.getUsername();
  }

  public static String getPassword() {
    return postgres.getPassword();
  }
}
