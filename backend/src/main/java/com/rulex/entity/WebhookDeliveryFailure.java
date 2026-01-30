package com.rulex.entity;

import jakarta.persistence.*;
import java.time.OffsetDateTime;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;

/**
 * RES-003 FIX: Entidade para Dead Letter Queue de webhooks falhados.
 *
 * <p>Armazena webhooks que falharam para reprocessamento posterior.
 */
@Entity
@Table(
    name = "webhook_delivery_failures",
    indexes = {
      @Index(name = "idx_webhook_dlq_status", columnList = "status"),
      @Index(name = "idx_webhook_dlq_next_retry", columnList = "next_retry_at"),
      @Index(name = "idx_webhook_dlq_created", columnList = "created_at")
    })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class WebhookDeliveryFailure {

  @Id
  @GeneratedValue(strategy = GenerationType.UUID)
  private UUID id;

  @Column(name = "webhook_url", nullable = false, length = 2048)
  private String webhookUrl;

  @Column(name = "payload", columnDefinition = "jsonb")
  @JdbcTypeCode(SqlTypes.JSON)
  private String payload;

  @Column(name = "headers", columnDefinition = "jsonb")
  @JdbcTypeCode(SqlTypes.JSON)
  private String headers;

  @Column(name = "http_method", nullable = false, length = 10)
  @Builder.Default
  private String httpMethod = "POST";

  /** ID da regra raiz (ex: complex_rule.id) responsável pelo webhook */
  @Column(name = "rule_id")
  private UUID ruleId;

  @Column(name = "transaction_id", length = 100)
  private String transactionId;

  @Column(name = "status", nullable = false, length = 20)
  @Builder.Default
  private String status = "PENDING";

  @Column(name = "retry_count", nullable = false)
  @Builder.Default
  private int retryCount = 0;

  @Column(name = "max_retries", nullable = false)
  @Builder.Default
  private int maxRetries = 5;

  @Column(name = "last_error", columnDefinition = "TEXT")
  private String lastError;

  @Column(name = "last_status_code")
  private Integer lastStatusCode;

  @Column(name = "next_retry_at")
  private OffsetDateTime nextRetryAt;

  @Column(name = "last_attempt_at")
  private OffsetDateTime lastAttemptAt;

  @Column(name = "resolved_at")
  private OffsetDateTime resolvedAt;

  @CreationTimestamp
  @Column(name = "created_at", nullable = false, updatable = false)
  private OffsetDateTime createdAt;

  /** Status possíveis */
  public static final String STATUS_PENDING = "PENDING";
  public static final String STATUS_RETRYING = "RETRYING";
  public static final String STATUS_SUCCESS = "SUCCESS";
  public static final String STATUS_FAILED = "FAILED";
  public static final String STATUS_EXPIRED = "EXPIRED";

  /** Calcula o próximo tempo de retry com backoff exponencial */
  public void scheduleNextRetry(long baseBackoffMs) {
    this.retryCount++;
    this.lastAttemptAt = OffsetDateTime.now();

    if (this.retryCount >= this.maxRetries) {
      this.status = STATUS_FAILED;
      this.nextRetryAt = null;
    } else {
      this.status = STATUS_RETRYING;
      // Backoff exponencial: base * 2^(retryCount-1)
      long delayMs = baseBackoffMs * (long) Math.pow(2, this.retryCount - 1);
      this.nextRetryAt = OffsetDateTime.now().plusNanos(delayMs * 1_000_000);
    }
  }

  /** Marca como sucesso */
  public void markSuccess() {
    this.status = STATUS_SUCCESS;
    this.resolvedAt = OffsetDateTime.now();
    this.nextRetryAt = null;
  }
}
