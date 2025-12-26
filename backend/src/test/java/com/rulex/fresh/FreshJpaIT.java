package com.rulex.fresh;

import static org.assertj.core.api.Assertions.assertThat;

import javax.sql.DataSource;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.ApplicationContext;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

/** Completely fresh test class in separate package to verify JPA initialization. */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Testcontainers
@DirtiesContext(classMode = DirtiesContext.ClassMode.AFTER_CLASS)
class FreshJpaIT {

  @Container
  @SuppressWarnings("resource")
  static final PostgreSQLContainer<?> postgres =
      new PostgreSQLContainer<>("postgres:16-alpine")
          .withDatabaseName("rulex_db")
          .withUsername("postgres")
          .withPassword("postgres");

  @DynamicPropertySource
  static void props(DynamicPropertyRegistry r) {
    if (!postgres.isRunning()) {
      postgres.start();
    }
    r.add("spring.datasource.url", postgres::getJdbcUrl);
    r.add("spring.datasource.username", postgres::getUsername);
    r.add("spring.datasource.password", postgres::getPassword);
    r.add("spring.datasource.driver-class-name", () -> "org.postgresql.Driver");
    r.add("spring.jpa.hibernate.ddl-auto", () -> "none");
    r.add("spring.flyway.enabled", () -> "true");
  }

  @Autowired ApplicationContext context;

  @Test
  void dataSourceBeanExists() {
    boolean hasDataSource = context.containsBean("dataSource");
    System.out.println("===> DataSource bean exists: " + hasDataSource);

    // Just check what beans we have
    String[] beanNames = context.getBeanNamesForType(DataSource.class);
    System.out.println("===> DataSource beans: " + String.join(", ", beanNames));

    assertThat(hasDataSource).isTrue();
  }
}
