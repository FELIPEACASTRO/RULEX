package com.rulex.service;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import lombok.Builder;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * Cliente HTTP para chamadas de webhook e serviços externos.
 * Utiliza HttpClient nativo do Java para requisições assíncronas.
 */
@Service
@Slf4j
public class WebhookClient {

  private final HttpClient httpClient;
  private final ObjectMapper objectMapper;

  @Value("${rulex.webhook.timeout-seconds:10}")
  private int timeoutSeconds;

  @Value("${rulex.webhook.enabled:true}")
  private boolean webhooksEnabled;

  @Value("${rulex.webhook.max-retries:0}")
  private int maxRetries;

  @Value("${rulex.webhook.retry-backoff-ms:250}")
  private long retryBackoffMs;

  public WebhookClient(ObjectMapper objectMapper) {
    this.objectMapper = objectMapper;
    this.httpClient = HttpClient.newBuilder()
        .connectTimeout(Duration.ofSeconds(5))
        .build();
  }

  /** Resultado de uma chamada de webhook */
  @Data
  @Builder
  public static class WebhookResult {
    private boolean success;
    private int statusCode;
    private String responseBody;
    private String errorMessage;
    private long durationMs;
  }

  /**
   * Executa uma chamada de webhook de forma síncrona.
   *
   * @param url URL do webhook
   * @param payload Dados a enviar (serão serializados como JSON)
   * @param headers Headers HTTP adicionais
   * @return Resultado da chamada
   */
  public WebhookResult callWebhook(String url, Map<String, Object> payload, Map<String, String> headers) {
    if (!webhooksEnabled) {
      log.info("Webhooks desabilitados. Chamada para {} ignorada.", url);
      return WebhookResult.builder()
          .success(false)
          .errorMessage("Webhooks desabilitados")
          .build();
    }
    return executeWithRetry(
        () -> executeWebhookPost(url, payload, headers),
        "webhook",
        url);
  }

  /**
   * Executa uma chamada de webhook de forma assíncrona.
   *
   * @param url URL do webhook
   * @param payload Dados a enviar
   * @param headers Headers HTTP adicionais
   * @return CompletableFuture com o resultado
   */
  public CompletableFuture<WebhookResult> callWebhookAsync(String url, Map<String, Object> payload, Map<String, String> headers) {
    return CompletableFuture.supplyAsync(() -> callWebhook(url, payload, headers));
  }

  /**
   * Executa uma chamada GET para serviço externo.
   *
   * @param url URL do serviço
   * @param headers Headers HTTP adicionais
   * @return Resultado da chamada
   */
  public WebhookResult callExternalService(String url, Map<String, String> headers) {
    if (!webhooksEnabled) {
      log.info("Serviços externos desabilitados. Chamada para {} ignorada.", url);
      return WebhookResult.builder()
          .success(false)
          .errorMessage("Serviços externos desabilitados")
          .build();
    }
    return executeWithRetry(
        () -> executeExternalGet(url, headers),
        "external",
        url);
  }

  private WebhookResult executeWebhookPost(
      String url, Map<String, Object> payload, Map<String, String> headers) throws Exception {
    String jsonBody = objectMapper.writeValueAsString(payload);

    HttpRequest.Builder requestBuilder = HttpRequest.newBuilder()
        .uri(URI.create(url))
        .timeout(Duration.ofSeconds(timeoutSeconds))
        .header("Content-Type", "application/json")
        .POST(HttpRequest.BodyPublishers.ofString(jsonBody));

    if (headers != null) {
      headers.forEach(requestBuilder::header);
    }

    HttpRequest request = requestBuilder.build();

    log.info("Executando webhook POST para: {}", url);
    HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());

    boolean success = response.statusCode() >= 200 && response.statusCode() < 300;

    return WebhookResult.builder()
        .success(success)
        .statusCode(response.statusCode())
        .responseBody(response.body())
        .build();
  }

  private WebhookResult executeExternalGet(String url, Map<String, String> headers) throws Exception {
    HttpRequest.Builder requestBuilder = HttpRequest.newBuilder()
        .uri(URI.create(url))
        .timeout(Duration.ofSeconds(timeoutSeconds))
        .header("Accept", "application/json")
        .GET();

    if (headers != null) {
      headers.forEach(requestBuilder::header);
    }

    HttpRequest request = requestBuilder.build();

    log.info("Chamando serviço externo GET: {}", url);
    HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());

    boolean success = response.statusCode() >= 200 && response.statusCode() < 300;

    return WebhookResult.builder()
        .success(success)
        .statusCode(response.statusCode())
        .responseBody(response.body())
        .build();
  }

  private WebhookResult executeWithRetry(
      WebhookCall call,
      String callType,
      String url) {
    int attempts = Math.max(0, maxRetries) + 1;
    long startTime = System.currentTimeMillis();
    WebhookResult lastResult = null;

    for (int attempt = 1; attempt <= attempts; attempt++) {
      try {
        WebhookResult result = call.execute();
        if (result.isSuccess() || result.getStatusCode() < 500 || attempt == attempts) {
          long durationMs = System.currentTimeMillis() - startTime;
          log.info("{} {} respondeu com status {} em {}ms", callType, url, result.getStatusCode(), durationMs);
          return WebhookResult.builder()
              .success(result.isSuccess())
              .statusCode(result.getStatusCode())
              .responseBody(result.getResponseBody())
              .errorMessage(result.getErrorMessage())
              .durationMs(durationMs)
              .build();
        }
        log.warn("{} {} falhou com status {} (tentativa {}/{})", callType, url, result.getStatusCode(), attempt, attempts);
        lastResult = result;
      } catch (Exception e) {
        log.error("Erro ao chamar {} {} (tentativa {}/{}): {}", callType, url, attempt, attempts, e.getMessage());
        lastResult = WebhookResult.builder()
            .success(false)
            .errorMessage(e.getMessage())
            .build();
      }

      if (attempt < attempts && retryBackoffMs > 0) {
        try {
          Thread.sleep(retryBackoffMs * attempt);
        } catch (InterruptedException ie) {
          Thread.currentThread().interrupt();
          break;
        }
      }
    }

    long durationMs = System.currentTimeMillis() - startTime;
    return WebhookResult.builder()
        .success(false)
        .statusCode(lastResult != null ? lastResult.getStatusCode() : 0)
        .responseBody(lastResult != null ? lastResult.getResponseBody() : null)
        .errorMessage(lastResult != null ? lastResult.getErrorMessage() : "Erro desconhecido")
        .durationMs(durationMs)
        .build();
  }

  @FunctionalInterface
  private interface WebhookCall {
    WebhookResult execute() throws Exception;
  }

  /**
   * Verifica se um URL de webhook está acessível.
   *
   * @param url URL para verificar
   * @return true se acessível
   */
  public boolean isWebhookReachable(String url) {
    try {
      HttpRequest request = HttpRequest.newBuilder()
          .uri(URI.create(url))
          .timeout(Duration.ofSeconds(5))
          .method("HEAD", HttpRequest.BodyPublishers.noBody())
          .build();

      HttpResponse<Void> response = httpClient.send(request, HttpResponse.BodyHandlers.discarding());
      return response.statusCode() < 500;
    } catch (Exception e) {
      log.debug("Webhook {} não está acessível: {}", url, e.getMessage());
      return false;
    }
  }
}
