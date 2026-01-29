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

    long startTime = System.currentTimeMillis();

    try {
      String jsonBody = objectMapper.writeValueAsString(payload);

      HttpRequest.Builder requestBuilder = HttpRequest.newBuilder()
          .uri(URI.create(url))
          .timeout(Duration.ofSeconds(timeoutSeconds))
          .header("Content-Type", "application/json")
          .POST(HttpRequest.BodyPublishers.ofString(jsonBody));

      // Adicionar headers customizados
      if (headers != null) {
        headers.forEach(requestBuilder::header);
      }

      HttpRequest request = requestBuilder.build();

      log.info("Executando webhook POST para: {}", url);
      HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());

      long durationMs = System.currentTimeMillis() - startTime;
      boolean success = response.statusCode() >= 200 && response.statusCode() < 300;

      log.info("Webhook {} respondeu com status {} em {}ms", url, response.statusCode(), durationMs);

      return WebhookResult.builder()
          .success(success)
          .statusCode(response.statusCode())
          .responseBody(response.body())
          .durationMs(durationMs)
          .build();

    } catch (Exception e) {
      long durationMs = System.currentTimeMillis() - startTime;
      log.error("Erro ao chamar webhook {}: {}", url, e.getMessage());

      return WebhookResult.builder()
          .success(false)
          .errorMessage(e.getMessage())
          .durationMs(durationMs)
          .build();
    }
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

    long startTime = System.currentTimeMillis();

    try {
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

      long durationMs = System.currentTimeMillis() - startTime;
      boolean success = response.statusCode() >= 200 && response.statusCode() < 300;

      log.info("Serviço externo {} respondeu com status {} em {}ms", url, response.statusCode(), durationMs);

      return WebhookResult.builder()
          .success(success)
          .statusCode(response.statusCode())
          .responseBody(response.body())
          .durationMs(durationMs)
          .build();

    } catch (Exception e) {
      long durationMs = System.currentTimeMillis() - startTime;
      log.error("Erro ao chamar serviço externo {}: {}", url, e.getMessage());

      return WebhookResult.builder()
          .success(false)
          .errorMessage(e.getMessage())
          .durationMs(durationMs)
          .build();
    }
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
