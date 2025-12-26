package com.rulex.controller;

import static org.assertj.core.api.Assertions.assertThat;

import com.rulex.dto.TransactionRequest;
import com.rulex.dto.TransactionResponse;
import com.rulex.entity.AuditLog;
import com.rulex.entity.RuleConfiguration;
import com.rulex.entity.TransactionDecision;
import com.rulex.repository.AuditLogRepository;
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
import org.springframework.test.annotation.DirtiesContext;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@DirtiesContext(classMode = DirtiesContext.ClassMode.AFTER_CLASS)
class TransactionAnalyzeIT extends CorePostgresITSupport {

  @LocalServerPort int port;

  @Autowired TestRestTemplate http;

  @Autowired RuleConfigurationRepository ruleConfigurationRepository;
  @Autowired TransactionRepository transactionRepository;
  @Autowired TransactionDecisionRepository transactionDecisionRepository;
  @Autowired AuditLogRepository auditLogRepository;

  @BeforeEach
  void clean() {
    auditLogRepository.deleteAll();
    transactionDecisionRepository.deleteAll();
    transactionRepository.deleteAll();
    ruleConfigurationRepository.deleteAll();
  }

  @Test
  void analyzeTransaction_persistsDecisionAndAudit_andMasksPan() {
    ruleConfigurationRepository.save(
        Objects.requireNonNull(
            RuleConfiguration.builder()
                .ruleName("RISK_MCC_7995")
                .description("MCC alto risco")
                .ruleType(RuleConfiguration.RuleType.SECURITY)
                .threshold(0)
                .weight(60)
                .enabled(true)
                .classification(TransactionDecision.TransactionClassification.FRAUD)
                .conditionsJson("[{\"field\":\"mcc\",\"operator\":\"==\",\"value\":\"7995\"}]")
                .logicOperator(RuleConfiguration.LogicOperator.AND)
                .build()));

    TransactionRequest req =
        TransactionRequest.builder()
            .externalTransactionId("tx-it-1")
            .customerIdFromHeader("cust-1")
            .customerAcctNumber(1234567890L)
            .pan("4111111111111111")
            .merchantId("m-1")
            .merchantName("Merchant")
            .transactionAmount(new BigDecimal("10.00"))
            .transactionDate(20251218)
            .transactionTime(120000)
            .transactionCurrencyCode(986)
            .mcc(7995)
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

    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);

    ResponseEntity<TransactionResponse> resp =
        http.postForEntity(
            url("/api/transactions/analyze"),
            new HttpEntity<>(req, headers),
            TransactionResponse.class);

    assertThat(resp.getStatusCode()).isEqualTo(HttpStatus.OK);
    TransactionResponse body = Objects.requireNonNull(resp.getBody());
    assertThat(body.getClassification()).isEqualTo("FRAUD");
    assertThat(body.getRiskScore()).isEqualTo(60);
    assertThat(body.getTriggeredRules()).hasSize(1);
    assertThat(body.getTriggeredRules().getFirst().getName()).isEqualTo("RISK_MCC_7995");

    var savedTx = transactionRepository.findByExternalTransactionId("tx-it-1");
    assertThat(savedTx).isPresent();
    assertThat(savedTx.get().getPan()).doesNotContain("4111111111111111");
    assertThat(savedTx.get().getPan()).contains("411111").contains("1111");

    var savedDecision = transactionDecisionRepository.findByTransactionId(savedTx.get().getId());
    assertThat(savedDecision).isPresent();
    assertThat(savedDecision.get().getClassification())
        .isEqualTo(TransactionDecision.TransactionClassification.FRAUD);
    assertThat(savedDecision.get().getRiskScore()).isEqualTo(60);

    var audits =
        auditLogRepository.findByActionType(
            AuditLog.AuditActionType.TRANSACTION_PROCESSED,
            org.springframework.data.domain.Pageable.ofSize(50));
    assertThat(audits.getTotalElements()).isGreaterThanOrEqualTo(1);
  }

  private String url(String path) {
    return "http://localhost:" + port + path;
  }
}
