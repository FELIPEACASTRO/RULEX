package com.rulex.contract;

import io.restassured.module.mockmvc.RestAssuredMockMvc;
import org.junit.jupiter.api.BeforeEach;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.web.context.WebApplicationContext;

/**
 * Base class for Spring Cloud Contract tests.
 *
 * <p>This class sets up the test environment for contract verification, including: - Full Spring
 * application context - MockMvc configuration with RestAssured - Security context for
 * authenticated requests
 *
 * <p>Contract tests verify that the API implementation adheres to the contracts defined in
 * src/test/resources/contracts/.
 */
@SpringBootTest(webEnvironment = WebEnvironment.MOCK)
@ActiveProfiles("test")
public abstract class ContractTestBase {

  @Autowired private WebApplicationContext context;

  @BeforeEach
  public void setup() {
    RestAssuredMockMvc.webAppContextSetup(context);
  }
}
