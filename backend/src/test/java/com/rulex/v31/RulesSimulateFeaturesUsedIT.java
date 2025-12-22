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
class RulesSimulateFeaturesUsedIT extends CorePostgresITSupport {

  @LocalServerPort int port;

  @Autowired TestRestTemplate http;
  @Autowired RuleExecutionLogRepository repository;

  @BeforeEach
  void clean() {
    repository.deleteAll();
  }

  @Test
  void simulate_returnsFeaturesUsed_andWritesAuditRow() {
    String body =
        "{"
            + "\"payload\":{\"merchantName\":\"Acme\"},"
            + "\"rules\":[{\"ruleName\":\"r1\",\"decision\":\"FRAUDE\",\"ast\":{\"type\":\"CONDITION\",\"left\":{\"type\":\"FIELD\",\"jsonPath\":\"$.merchantName\"},\"operator\":\"EQ\",\"right\":\"Acme\"}}]"
            + "}";

    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);

    ResponseEntity<String> resp =
        http.postForEntity(url("/rules/simulate"), new HttpEntity<>(body, headers), String.class);

    assertThat(resp.getStatusCode()).isEqualTo(HttpStatus.OK);
    assertThat(resp.getBody()).contains("\"featuresUsed\"");
    assertThat(resp.getBody()).contains("$.merchantName");

    var logs = repository.findAll();
    assertThat(logs).hasSize(1);
    assertThat(logs.getFirst().getEventType()).isEqualTo(ExecutionEventType.SIMULATE);
    assertThat(logs.getFirst().getFeaturesUsedJson().toString()).contains("$.merchantName");
  }

  private String url(String path) {
    return "http://localhost:" + port + "/api" + path;
  }
}
