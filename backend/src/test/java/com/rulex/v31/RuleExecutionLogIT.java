package com.rulex.v31;

import static org.assertj.core.api.Assertions.assertThat;

import com.rulex.testsupport.CorePostgresITSupport;
import com.rulex.v31.execlog.ExecutionEventType;
import com.rulex.v31.execlog.RuleExecutionLogRepository;
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

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class RuleExecutionLogIT extends CorePostgresITSupport {

  @LocalServerPort int port;

  @Autowired TestRestTemplate http;
  @Autowired RuleExecutionLogRepository repository;

  @BeforeEach
  void clean() {
    repository.deleteAll();
  }

  @Test
  void evaluate_writesEvaluateAndIdempotentReplayLogs() {
    String extId = "tx-log-1";

    String rawJson =
        "{"
            + "\"externalTransactionId\":\""
            + extId
            + "\","
            + "\"customerIdFromHeader\":\"cust-1\","
            + "\"customerAcctNumber\":1234567890,"
            + "\"pan\":\"4111111111111111\","
            + "\"merchantId\":\"m-1\","
            + "\"merchantName\":\"Merchant\","
            + "\"transactionAmount\":10.00,"
            + "\"transactionDate\":20251218,"
            + "\"transactionTime\":120000,"
            + "\"transactionCurrencyCode\":986,"
            + "\"mcc\":5999,"
            + "\"consumerAuthenticationScore\":200,"
            + "\"externalScore3\":200,"
            + "\"cavvResult\":0,"
            + "\"cryptogramValid\":\"V\","
            + "\"cvv2Response\":\"M\","
            + "\"eciIndicator\":5,"
            + "\"atcCard\":1,"
            + "\"atcHost\":1,"
            + "\"tokenAssuranceLevel\":80,"
            + "\"availableCredit\":1000.00,"
            + "\"cardCashBalance\":0.00,"
            + "\"cardDelinquentAmount\":0.00,"
            + "\"merchantCountryCode\":\"076\","
            + "\"customerPresent\":\"Y\""
            + "}";

    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);

    ResponseEntity<String> resp1 =
        http.postForEntity(url("/evaluate"), new HttpEntity<>(rawJson, headers), String.class);
    assertThat(resp1.getStatusCode()).isEqualTo(HttpStatus.OK);

    var logsAfter1 = repository.findByExternalTransactionIdOrderByCreatedAtAsc(extId);
    assertThat(logsAfter1).hasSize(1);
    assertThat(logsAfter1.getFirst().getEventType()).isEqualTo(ExecutionEventType.EVALUATE);

    ResponseEntity<String> resp2 =
        http.postForEntity(url("/evaluate"), new HttpEntity<>(rawJson, headers), String.class);
    assertThat(resp2.getStatusCode()).isEqualTo(HttpStatus.OK);

    var logsAfter2 = repository.findByExternalTransactionIdOrderByCreatedAtAsc(extId);
    assertThat(logsAfter2).hasSize(2);
    assertThat(logsAfter2.get(0).getEventType()).isEqualTo(ExecutionEventType.EVALUATE);
    assertThat(logsAfter2.get(1).getEventType()).isEqualTo(ExecutionEventType.IDEMPOTENT_REPLAY);
  }

  private String url(String path) {
    return "http://localhost:" + port + path;
  }
}
