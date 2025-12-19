package com.rulex.homolog;

import static org.assertj.core.api.Assertions.assertThat;

import com.rulex.dto.RuleConditionDTO;
import com.rulex.dto.TransactionRequest;
import com.rulex.dto.homolog.ActivateRuleSetRequest;
import com.rulex.dto.homolog.CreateRuleRequest;
import com.rulex.dto.homolog.CreateRuleSetRequest;
import com.rulex.dto.homolog.SimulationRequest;
import com.rulex.entity.homolog.DecisionOutcome;
import com.rulex.entity.homolog.LogicOperator;
import com.rulex.entity.homolog.RuleStatus;
import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.*;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

@Testcontainers
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class HomologSimulationIT {

  private static final ParameterizedTypeReference<Map<String, Object>> MAP_RESPONSE =
      new ParameterizedTypeReference<>() {};

  @SuppressWarnings("resource")
  @Container
  static final PostgreSQLContainer<?> postgres =
      new PostgreSQLContainer<>("postgres:16-alpine")
          .withDatabaseName("rulex_db")
          .withUsername("postgres")
          .withPassword("postgres");

  @DynamicPropertySource
  static void props(DynamicPropertyRegistry r) {
    r.add("spring.datasource.url", postgres::getJdbcUrl);
    r.add("spring.datasource.username", postgres::getUsername);
    r.add("spring.datasource.password", postgres::getPassword);
    r.add("spring.jpa.hibernate.ddl-auto", () -> "none");
    r.add("spring.flyway.enabled", () -> "true");
  }

  @LocalServerPort int port;

  @Autowired TestRestTemplate http;

  @Test
  void createsRule_publishes_ruleset_activates_and_simulates() {
    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);
    headers.set("X-Actor-Email", "admin@rulex.local");

    CreateRuleRequest createRule =
        new CreateRuleRequest(
            "RISK_MCC_7995",
            "MCC alto risco",
            10,
            40,
            DecisionOutcome.SUSPEITA_DE_FRAUDE,
            "MCC em lista de alto risco",
            List.of("mcc"),
            LogicOperator.AND,
            List.of(new RuleConditionDTO("mcc", "EQ", "7995")),
            true);

    ResponseEntity<Map<String, Object>> ruleResp =
        http.exchange(
            url("/api/homolog/rules"),
            HttpMethod.POST,
            new HttpEntity<>(createRule, headers),
            MAP_RESPONSE);
    assertThat(ruleResp.getStatusCode()).isEqualTo(HttpStatus.OK);
    UUID ruleVersionId =
        UUID.fromString(String.valueOf(Objects.requireNonNull(ruleResp.getBody()).get("id")));

    ResponseEntity<Map<String, Object>> publishResp =
        http.exchange(
            url("/api/homolog/rules/versions/" + ruleVersionId + "/publish"),
            HttpMethod.POST,
            new HttpEntity<>(null, headers),
            MAP_RESPONSE);
    assertThat(publishResp.getStatusCode()).isEqualTo(HttpStatus.OK);
    assertThat(Objects.requireNonNull(publishResp.getBody()).get("status"))
        .isEqualTo(RuleStatus.PUBLISHED.name());

    CreateRuleSetRequest createRuleSet =
        new CreateRuleSetRequest("DEFAULT", "RuleSet padrão", List.of(ruleVersionId), "seed");

    ResponseEntity<Map<String, Object>> rsResp =
        http.exchange(
            url("/api/homolog/rulesets"),
            HttpMethod.POST,
            new HttpEntity<>(createRuleSet, headers),
            MAP_RESPONSE);
    assertThat(rsResp.getStatusCode()).isEqualTo(HttpStatus.OK);
    UUID ruleSetVersionId =
        UUID.fromString(String.valueOf(Objects.requireNonNull(rsResp.getBody()).get("id")));

    ResponseEntity<Map<String, Object>> rsPub =
        http.exchange(
            url("/api/homolog/rulesets/versions/" + ruleSetVersionId + "/publish"),
            HttpMethod.POST,
            new HttpEntity<>(null, headers),
            MAP_RESPONSE);
    assertThat(rsPub.getStatusCode()).isEqualTo(HttpStatus.OK);

    ResponseEntity<Void> act =
        http.exchange(
            url("/api/homolog/rulesets/activate"),
            HttpMethod.POST,
            new HttpEntity<>(new ActivateRuleSetRequest(ruleSetVersionId), headers),
            Void.class);
    assertThat(act.getStatusCode()).isEqualTo(HttpStatus.OK);

    TransactionRequest payload =
        TransactionRequest.builder()
            .externalTransactionId("tx-1")
            .customerIdFromHeader("c-1")
            .customerAcctNumber(1234567890123456L)
            .pan("4111111111111111")
            .transactionAmount(new BigDecimal("10.00"))
            .transactionDate(20251218)
            .transactionTime(120000)
            .transactionCurrencyCode(986)
            .mcc(7995)
            .consumerAuthenticationScore(1)
            .externalScore3(1)
            .cavvResult(1)
            .eciIndicator(5)
            .atcCard(1)
            .atcHost(1)
            .tokenAssuranceLevel(1)
            .availableCredit(new BigDecimal("1000.00"))
            .cardCashBalance(new BigDecimal("0.00"))
            .cardDelinquentAmount(new BigDecimal("0.00"))
            .build();

    SimulationRequest simReq = new SimulationRequest("it", ruleSetVersionId, payload);
    ResponseEntity<Map<String, Object>> simResp =
        http.exchange(
            url("/api/homolog/simulations/run"),
            HttpMethod.POST,
            new HttpEntity<>(simReq, headers),
            MAP_RESPONSE);

    assertThat(simResp.getStatusCode()).isEqualTo(HttpStatus.OK);
    Map<String, Object> simBody = Objects.requireNonNull(simResp.getBody());
    assertThat(simBody.get("decision")).isEqualTo(DecisionOutcome.SUSPEITA_DE_FRAUDE.name());
    assertThat(Integer.parseInt(String.valueOf(simBody.get("riskScore"))))
        .isGreaterThanOrEqualTo(40);
  }

  private String url(String path) {
    return "http://localhost:" + port + path;
  }
}
