package com.rulex.controller;

import static org.assertj.core.api.Assertions.assertThat;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.dto.RuleConfigurationDTO;
import com.rulex.dto.TransactionRequest;
import com.rulex.dto.TransactionResponse;
import com.rulex.entity.TransactionDecision;
import com.rulex.repository.AuditLogRepository;
import com.rulex.repository.RuleConfigurationHistoryRepository;
import com.rulex.repository.RuleConfigurationRepository;
import com.rulex.repository.TransactionDecisionRepository;
import com.rulex.repository.TransactionRepository;
import com.rulex.testsupport.CorePostgresITSupport;
import java.math.BigDecimal;
import java.util.Objects;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.JdkClientHttpRequestFactory;
import org.springframework.test.annotation.DirtiesContext;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@DirtiesContext(classMode = DirtiesContext.ClassMode.AFTER_CLASS)
class RulePopupE2EIT extends CorePostgresITSupport {

  @LocalServerPort int port;

  @Autowired TestRestTemplate http;
  @Autowired ObjectMapper objectMapper;

  @Autowired RuleConfigurationRepository ruleConfigurationRepository;
  @Autowired RuleConfigurationHistoryRepository ruleHistoryRepository;
  @Autowired TransactionRepository transactionRepository;
  @Autowired TransactionDecisionRepository transactionDecisionRepository;
  @Autowired AuditLogRepository auditLogRepository;

  @BeforeEach
  void clean() {
    // Default RestTemplate request factory (HttpURLConnection) não suporta PATCH.
    http.getRestTemplate().setRequestFactory(new JdkClientHttpRequestFactory());

    auditLogRepository.deleteAll();
    ruleHistoryRepository.deleteAll();
    transactionDecisionRepository.deleteAll();
    transactionRepository.deleteAll();
    ruleConfigurationRepository.deleteAll();
  }

  @Test
  void popupCreate_thenAnalyze_usesThresholdAndWeight_andDisabledRulesAreNotEvaluated() {
    // 1) Criar regra via API (simulando o popup do FE)
    RuleConfigurationDTO create =
        RuleConfigurationDTO.builder()
            .ruleName("LOW_AUTHENTICATION_SCORE")
            .description("Score de autenticação baixo")
            .ruleType("SECURITY")
            .classification("SUSPICIOUS")
            .threshold(100)
            .weight(70)
            .enabled(true)
            .logicOperator("AND")
            .conditions(java.util.List.of())
            .build();

    ResponseEntity<RuleConfigurationDTO> createdResp =
        http.postForEntity(url("/api/rules"), json(create), RuleConfigurationDTO.class);
    assertThat(createdResp.getStatusCode()).isEqualTo(HttpStatus.CREATED);
    RuleConfigurationDTO created = Objects.requireNonNull(createdResp.getBody());
    assertThat(created.getId()).isNotNull();
    assertThat(created.getRuleName()).isEqualTo("LOW_AUTHENTICATION_SCORE");
    assertThat(ruleHistoryRepository.findByRuleIdOrderByCreatedAtDesc(created.getId()))
        .isNotEmpty();

    // 2) Analisar transação que dispara a regra (consumerAuthenticationScore < threshold)
    TransactionRequest req = minimalRequest("tx-pop-1");
    req.setConsumerAuthenticationScore(50);
    ResponseEntity<TransactionResponse> analyze1 =
        http.postForEntity(url("/api/transactions/analyze"), json(req), TransactionResponse.class);
    assertThat(analyze1.getStatusCode()).isEqualTo(HttpStatus.OK);
    TransactionResponse b1 = Objects.requireNonNull(analyze1.getBody());
    // V3.1: classificação é derivada da severidade das regras (riskScore é telemetria)
    assertThat(b1.getClassification()).isEqualTo("SUSPICIOUS");
    assertThat(b1.getRiskScore()).isEqualTo(70);
    assertThat(b1.getTriggeredRules()).hasSize(1);
    assertThat(b1.getTriggeredRules().getFirst().getName()).isEqualTo("LOW_AUTHENTICATION_SCORE");

    // 3) Desabilitar regra e validar que não é avaliada
    ResponseEntity<RuleConfigurationDTO> toggled =
        http.exchange(
            url("/api/rules/" + created.getId() + "/toggle"),
            org.springframework.http.HttpMethod.PATCH,
            new HttpEntity<>(null, jsonHeaders()),
            RuleConfigurationDTO.class);
    assertThat(toggled.getStatusCode()).isEqualTo(HttpStatus.OK);
    assertThat(Objects.requireNonNull(toggled.getBody()).getEnabled()).isFalse();

    TransactionRequest req2 = minimalRequest("tx-pop-2");
    req2.setConsumerAuthenticationScore(50);
    ResponseEntity<TransactionResponse> analyze2 =
        http.postForEntity(url("/api/transactions/analyze"), json(req2), TransactionResponse.class);
    assertThat(analyze2.getStatusCode()).isEqualTo(HttpStatus.OK);
    TransactionResponse b2 = Objects.requireNonNull(analyze2.getBody());
    assertThat(b2.getTriggeredRules()).isEmpty();
    assertThat(b2.getRiskScore()).isLessThan(30);
    assertThat(b2.getClassification()).isEqualTo("APPROVED");
  }

  @Test
  void popupEdit_thenAnalyze_riskScoreClampedTo100() {
    RuleConfigurationDTO createExternalScoreRule =
        RuleConfigurationDTO.builder()
            .ruleName("LOW_EXTERNAL_SCORE")
            .description("Score externo baixo")
            .ruleType("SECURITY")
            .classification("FRAUD")
            .threshold(10)
            .weight(10)
            .enabled(true)
            .logicOperator("AND")
            .conditions(java.util.List.of())
            .build();

    RuleConfigurationDTO createdExternalScoreRule =
        Objects.requireNonNull(
            http.postForEntity(
                    url("/api/rules"), json(createExternalScoreRule), RuleConfigurationDTO.class)
                .getBody());

    RuleConfigurationDTO createInvalidCavvRule =
        RuleConfigurationDTO.builder()
            .ruleName("INVALID_CAVV")
            .description("CAVV inválido")
            .ruleType("SECURITY")
            .classification("SUSPICIOUS")
            .threshold(0)
            .weight(50)
            .enabled(true)
            .logicOperator("AND")
            .conditions(java.util.List.of())
            .build();

    http.postForEntity(url("/api/rules"), json(createInvalidCavvRule), RuleConfigurationDTO.class);

    // editar peso para 100 e validar clamp do score quando múltiplas regras disparam
    RuleConfigurationDTO update =
        RuleConfigurationDTO.builder()
            .ruleName(createdExternalScoreRule.getRuleName())
            .description(createdExternalScoreRule.getDescription())
            .ruleType(createdExternalScoreRule.getRuleType())
            .classification(createdExternalScoreRule.getClassification())
            .threshold(createdExternalScoreRule.getThreshold())
            .weight(100)
            .enabled(true)
            .logicOperator("AND")
            .conditions(java.util.List.of())
            .build();

    ResponseEntity<RuleConfigurationDTO> updatedResp =
        http.exchange(
            url("/api/rules/" + createdExternalScoreRule.getId()),
            org.springframework.http.HttpMethod.PUT,
            json(update),
            RuleConfigurationDTO.class);
    assertThat(updatedResp.getStatusCode()).isEqualTo(HttpStatus.OK);

    TransactionRequest req = minimalRequest("tx-pop-3");
    req.setExternalScore3(0);
    req.setCavvResult(1);
    ResponseEntity<TransactionResponse> analyze =
        http.postForEntity(url("/api/transactions/analyze"), json(req), TransactionResponse.class);
    assertThat(analyze.getStatusCode()).isEqualTo(HttpStatus.OK);
    TransactionResponse body = Objects.requireNonNull(analyze.getBody());
    assertThat(body.getClassification()).isEqualTo("FRAUD");
    assertThat(body.getTriggeredRules()).hasSize(2);
    assertThat(body.getRiskScore()).isEqualTo(100); // 100 + 50 => clamp

    var savedTx = transactionRepository.findByExternalTransactionId("tx-pop-3");
    assertThat(savedTx).isPresent();
    var savedDecision = transactionDecisionRepository.findByTransactionId(savedTx.get().getId());
    assertThat(savedDecision).isPresent();
    assertThat(savedDecision.get().getClassification())
        .isEqualTo(TransactionDecision.TransactionClassification.FRAUD);
  }

  @Test
  void ruleNameMustBeUnique_andIsAuditable() {
    RuleConfigurationDTO create =
        RuleConfigurationDTO.builder()
            .ruleName("UNIQUE_RULE")
            .description("x")
            .ruleType("SECURITY")
            .classification("SUSPICIOUS")
            .threshold(0)
            .weight(10)
            .enabled(true)
            .logicOperator("AND")
            .conditions(java.util.List.of())
            .build();

    ResponseEntity<RuleConfigurationDTO> first =
        http.postForEntity(url("/api/rules"), json(create), RuleConfigurationDTO.class);
    assertThat(first.getStatusCode()).isEqualTo(HttpStatus.CREATED);
    Long id = Objects.requireNonNull(first.getBody()).getId();
    assertThat(ruleHistoryRepository.findByRuleIdOrderByCreatedAtDesc(id)).isNotEmpty();

    ResponseEntity<String> second =
        http.postForEntity(url("/api/rules"), json(create), String.class);
    assertThat(second.getStatusCode()).isEqualTo(HttpStatus.CONFLICT);
  }

  @Test
  void evaluate_returnsDecision_ruleHits_andAggregatedPopups() throws Exception {
    ruleConfigurationRepository.save(
        Objects.requireNonNull(
            com.rulex.entity.RuleConfiguration.builder()
                .ruleName("RISK_MCC_7995")
                .description("MCC alto risco")
                .ruleType(com.rulex.entity.RuleConfiguration.RuleType.SECURITY)
                .threshold(0)
                .weight(60)
                .enabled(true)
                .classification(TransactionDecision.TransactionClassification.FRAUD)
                .conditionsJson("[{\"field\":\"mcc\",\"operator\":\"==\",\"value\":\"7995\"}]")
                .logicOperator(com.rulex.entity.RuleConfiguration.LogicOperator.AND)
                .build()));

    TransactionRequest req = minimalRequest("tx-eval-1");
    req.setMcc(7995);

    ResponseEntity<String> resp = http.postForEntity(url("/api/evaluate"), json(req), String.class);
    assertThat(resp.getStatusCode()).isEqualTo(HttpStatus.OK);

    JsonNode root = objectMapper.readTree(Objects.requireNonNull(resp.getBody()));
    assertThat(root.get("transactionId").asText()).isEqualTo("tx-eval-1");
    assertThat(root.get("classification").asText()).isEqualTo("FRAUD");
    assertThat(root.get("ruleHits").isArray()).isTrue();
    assertThat(root.get("ruleHits").size()).isEqualTo(1);
    assertThat(root.get("ruleHits").get(0).get("ruleName").asText()).isEqualTo("RISK_MCC_7995");
    assertThat(root.get("popups").isArray()).isTrue();
    assertThat(root.get("popups").size()).isGreaterThanOrEqualTo(1);
    assertThat(root.get("popups").get(0).get("rules").size()).isEqualTo(1);
  }

  private TransactionRequest minimalRequest(String txId) {
    return TransactionRequest.builder()
        .externalTransactionId(txId)
        .customerIdFromHeader("cust-1")
        .customerAcctNumber(1234567890L)
        .pan("4111111111111111")
        .merchantId("m-1")
        .merchantName("Merchant")
        .transactionAmount(new BigDecimal("10.00"))
        .transactionDate(20251218)
        .transactionTime(120000)
        .transactionCurrencyCode(986)
        .mcc(5411)
        .consumerAuthenticationScore(200)
        .externalScore3(200)
        .cavvResult(0)
        .cryptogramValid("V")
        .cvv2Response("M")
        .eciIndicator(5)
        .atcCard(1)
        .atcHost(1)
        .tokenAssuranceLevel(80)
        .availableCredit(new BigDecimal("1000.00"))
        .cardCashBalance(new BigDecimal("0.00"))
        .cardDelinquentAmount(new BigDecimal("0.00"))
        .merchantCountryCode("076")
        .customerPresent("Y")
        .build();
  }

  private HttpEntity<Object> json(Object body) {
    return new HttpEntity<>(body, jsonHeaders());
  }

  private HttpHeaders jsonHeaders() {
    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);
    return headers;
  }

  private String url(String path) {
    return "http://localhost:" + port + path;
  }
}
