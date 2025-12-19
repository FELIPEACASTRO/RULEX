package com.rulex.controller;

import static org.assertj.core.api.Assertions.assertThat;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.dto.TransactionRequest;
import com.rulex.dto.TransactionResponse;
import com.rulex.testsupport.CorePostgresITSupport;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Objects;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.http.*;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class CrtranBaselineIT extends CorePostgresITSupport {

  @LocalServerPort int port;

  @Autowired TestRestTemplate http;

  @Autowired ObjectMapper objectMapper;

  @Test
  void baselinePayload_crtranJson_isAcceptedByAnalyzeAndAdvanced() throws Exception {
    String json = Files.readString(resolveCrtranPath());
    TransactionRequest req = objectMapper.readValue(json, TransactionRequest.class);

    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);

    ResponseEntity<TransactionResponse> analyze =
        http.postForEntity(
            url("/api/transactions/analyze"),
            new HttpEntity<>(req, headers),
            TransactionResponse.class);

    assertThat(analyze.getStatusCode()).isEqualTo(HttpStatus.OK);
    TransactionResponse analyzeBody = Objects.requireNonNull(analyze.getBody());
    assertThat(analyzeBody.getTransactionId()).isEqualTo(req.getExternalTransactionId());
    assertThat(analyzeBody.getSuccess()).isTrue();

    ResponseEntity<TransactionResponse> advanced =
        http.postForEntity(
            url("/api/transactions/analyze-advanced"),
            new HttpEntity<>(req, headers),
            TransactionResponse.class);

    assertThat(advanced.getStatusCode()).isEqualTo(HttpStatus.OK);
    TransactionResponse advancedBody = Objects.requireNonNull(advanced.getBody());
    assertThat(advancedBody.getTransactionId()).isEqualTo(req.getExternalTransactionId());
    assertThat(advancedBody.getSuccess()).isTrue();
    assertThat(advancedBody.getTriggeredRules()).isNotNull();
  }

  private String url(String path) {
    return "http://localhost:" + port + path;
  }

  private Path resolveCrtranPath() {
    // Preferir o payload oficial na raiz do repo.
    Path[] candidates = {
      Path.of("..", "fixtures", "crtran.json"), // quando cwd=backend
      Path.of("fixtures", "crtran.json"),
      Path.of("..", "..", "fixtures", "crtran.json")
    };

    for (Path p : candidates) {
      if (Files.exists(p)) {
        return p.normalize();
      }
    }

    throw new IllegalStateException(
        "fixtures/crtran.json não encontrado. Candidates: "
            + java.util.Arrays.toString(candidates));
  }
}
