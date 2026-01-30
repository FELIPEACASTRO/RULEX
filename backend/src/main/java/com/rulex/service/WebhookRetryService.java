package com.rulex.service;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.entity.WebhookDeliveryFailure;
import com.rulex.repository.WebhookDeliveryFailureRepository;
import java.time.OffsetDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * RES-003 FIX: Serviço para reprocessamento de webhooks falhados (DLQ).
 *
 * <p>Funcionalidades:
 * - Registra falhas de webhook na DLQ
 * - Reprocessa webhooks pendentes com backoff exponencial
 * - Expira webhooks antigos
 * - Limpa registros resolvidos
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class WebhookRetryService {

  private final WebhookDeliveryFailureRepository dlqRepository;
  private final WebhookClient webhookClient;
  private final ObjectMapper objectMapper;

  @Value("${rulex.webhook.dlq.enabled:true}")
  private boolean dlqEnabled;

  @Value("${rulex.webhook.dlq.retry-backoff-ms:1000}")
  private long retryBackoffMs;

  @Value("${rulex.webhook.dlq.max-retries:5}")
  private int maxRetries;

  @Value("${rulex.webhook.dlq.expire-after-hours:72}")
  private int expireAfterHours;

  @Value("${rulex.webhook.dlq.cleanup-after-days:30}")
  private int cleanupAfterDays;

  /**
   * Registra uma falha de webhook na DLQ.
   *
   * @param url URL do webhook
   * @param payload Payload que foi enviado
   * @param headers Headers que foram enviados
   * @param ruleId ID da regra (opcional)
   * @param transactionId ID da transação (opcional)
   * @param error Mensagem de erro
   * @param statusCode Código HTTP retornado (opcional)
   */
  @Transactional
  public void recordFailure(
      String url,
      Map<String, Object> payload,
      Map<String, String> headers,
      UUID ruleId,
      String transactionId,
      String error,
      Integer statusCode) {

    if (!dlqEnabled) {
      log.debug("DLQ desabilitada, falha de webhook não registrada");
      return;
    }

    try {
      WebhookDeliveryFailure failure =
          WebhookDeliveryFailure.builder()
              .webhookUrl(url)
              .payload(objectMapper.writeValueAsString(payload))
              .headers(headers != null ? objectMapper.writeValueAsString(headers) : null)
              .ruleId(ruleId)
              .transactionId(transactionId)
              .status(WebhookDeliveryFailure.STATUS_PENDING)
              .retryCount(0)
              .maxRetries(maxRetries)
              .lastError(truncate(error, 2000))
              .lastStatusCode(statusCode)
              .nextRetryAt(OffsetDateTime.now().plusSeconds(retryBackoffMs / 1000))
              .build();

      dlqRepository.save(failure);
      log.info(
          "RES-003: Webhook failure recorded in DLQ - URL: {}, Transaction: {}, Error: {}",
          url,
          transactionId,
          error);

    } catch (Exception e) {
      log.error("Failed to record webhook failure in DLQ: {}", e.getMessage());
    }
  }

  /**
   * Processa webhooks pendentes de retry.
   * Executado a cada 30 segundos.
   */
  @Scheduled(fixedDelayString = "${rulex.webhook.dlq.retry-interval-ms:30000}")
  @Transactional
  public void processRetries() {
    if (!dlqEnabled) {
      return;
    }

    List<WebhookDeliveryFailure> pending =
        dlqRepository.findPendingRetries(OffsetDateTime.now());

    if (pending.isEmpty()) {
      return;
    }

    log.info("RES-003: Processing {} pending webhook retries", pending.size());

    for (WebhookDeliveryFailure failure : pending) {
      processRetry(failure);
    }
  }

  private void processRetry(WebhookDeliveryFailure failure) {
    try {
      Map<String, Object> payload =
          objectMapper.readValue(failure.getPayload(), new TypeReference<>() {});

      Map<String, String> headers = null;
      if (failure.getHeaders() != null) {
        headers = objectMapper.readValue(failure.getHeaders(), new TypeReference<>() {});
      }

      log.debug(
          "Retrying webhook - ID: {}, URL: {}, Attempt: {}",
          failure.getId(),
          failure.getWebhookUrl(),
          failure.getRetryCount() + 1);

      WebhookClient.WebhookResult result =
          webhookClient.callWebhook(failure.getWebhookUrl(), payload, headers);

      if (result.isSuccess()) {
        failure.markSuccess();
        dlqRepository.save(failure);
        log.info(
            "RES-003: Webhook retry SUCCESS - ID: {}, URL: {}",
            failure.getId(),
            failure.getWebhookUrl());
      } else {
        failure.setLastError(result.getErrorMessage());
        failure.setLastStatusCode(result.getStatusCode());
        failure.scheduleNextRetry(retryBackoffMs);
        dlqRepository.save(failure);

        if (WebhookDeliveryFailure.STATUS_FAILED.equals(failure.getStatus())) {
          log.error(
              "RES-003: Webhook permanently FAILED after {} retries - ID: {}, URL: {}",
              failure.getRetryCount(),
              failure.getId(),
              failure.getWebhookUrl());
        } else {
          log.warn(
              "RES-003: Webhook retry {} FAILED, next retry at {} - ID: {}",
              failure.getRetryCount(),
              failure.getNextRetryAt(),
              failure.getId());
        }
      }

    } catch (Exception e) {
      log.error("Error processing webhook retry {}: {}", failure.getId(), e.getMessage());
      failure.setLastError("Processing error: " + e.getMessage());
      failure.scheduleNextRetry(retryBackoffMs);
      dlqRepository.save(failure);
    }
  }

  /**
   * Expira webhooks antigos que não foram processados.
   * Executado a cada hora.
   */
  @Scheduled(cron = "0 0 * * * *")
  @Transactional
  public void expireOldFailures() {
    if (!dlqEnabled) {
      return;
    }

    OffsetDateTime expireBefore = OffsetDateTime.now().minusHours(expireAfterHours);
    int expired = dlqRepository.expireOldFailures(expireBefore);

    if (expired > 0) {
      log.info("RES-003: Expired {} old webhook failures", expired);
    }
  }

  /**
   * Limpa registros resolvidos antigos.
   * Executado diariamente às 3h.
   */
  @Scheduled(cron = "0 0 3 * * *")
  @Transactional
  public void cleanupResolvedFailures() {
    if (!dlqEnabled) {
      return;
    }

    OffsetDateTime cleanupBefore = OffsetDateTime.now().minusDays(cleanupAfterDays);
    int deleted = dlqRepository.deleteResolvedBefore(cleanupBefore);

    if (deleted > 0) {
      log.info("RES-003: Cleaned up {} resolved webhook failures", deleted);
    }
  }

  /** Retorna estatísticas da DLQ */
  public Map<String, Long> getStats() {
    return Map.of(
        "pending", dlqRepository.countByStatus(WebhookDeliveryFailure.STATUS_PENDING),
        "retrying", dlqRepository.countByStatus(WebhookDeliveryFailure.STATUS_RETRYING),
        "success", dlqRepository.countByStatus(WebhookDeliveryFailure.STATUS_SUCCESS),
        "failed", dlqRepository.countByStatus(WebhookDeliveryFailure.STATUS_FAILED),
        "expired", dlqRepository.countByStatus(WebhookDeliveryFailure.STATUS_EXPIRED));
  }

  private String truncate(String value, int maxLength) {
    if (value == null) return null;
    return value.length() > maxLength ? value.substring(0, maxLength) : value;
  }
}
