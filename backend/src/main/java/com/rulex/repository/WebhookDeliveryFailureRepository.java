package com.rulex.repository;

import com.rulex.entity.WebhookDeliveryFailure;
import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.stereotype.Repository;
import jakarta.persistence.LockModeType;

/**
 * RES-003 FIX: Repositório para Dead Letter Queue de webhooks.
 */
@Repository
public interface WebhookDeliveryFailureRepository
    extends JpaRepository<WebhookDeliveryFailure, UUID> {

  /** Busca webhooks pendentes de retry */
  @Query(
      "SELECT w FROM WebhookDeliveryFailure w "
          + "WHERE w.status IN ('PENDING', 'RETRYING') "
          + "AND (w.nextRetryAt IS NULL OR w.nextRetryAt <= :now) "
          + "ORDER BY w.createdAt ASC")
  List<WebhookDeliveryFailure> findPendingRetries(@Param("now") OffsetDateTime now);

  /** Busca webhooks pendentes com lock pessimista para evitar duplicidade entre instâncias */
  @Lock(LockModeType.PESSIMISTIC_WRITE)
  @Query(
      "SELECT w FROM WebhookDeliveryFailure w "
          + "WHERE w.status IN ('PENDING', 'RETRYING') "
          + "AND (w.nextRetryAt IS NULL OR w.nextRetryAt <= :now) "
          + "ORDER BY w.createdAt ASC")
  List<WebhookDeliveryFailure> findPendingRetriesLocked(
      @Param("now") OffsetDateTime now, Pageable pageable);

  /** Conta webhooks por status */
  @Query("SELECT COUNT(w) FROM WebhookDeliveryFailure w WHERE w.status = :status")
  long countByStatus(@Param("status") String status);

  /** Busca webhooks falhados para uma transação */
  List<WebhookDeliveryFailure> findByTransactionId(String transactionId);

  /** Busca webhooks falhados para uma regra */
  List<WebhookDeliveryFailure> findByRuleId(UUID ruleId);

  /** Expira webhooks antigos que excederam max retries */
  @Modifying
  @Query(
      "UPDATE WebhookDeliveryFailure w SET w.status = 'EXPIRED' "
          + "WHERE w.status IN ('PENDING', 'RETRYING') "
          + "AND w.createdAt < :before")
  int expireOldFailures(@Param("before") OffsetDateTime before);

  /** Limpa webhooks resolvidos antigos */
  @Modifying
  @Query(
      "DELETE FROM WebhookDeliveryFailure w "
          + "WHERE w.status IN ('SUCCESS', 'EXPIRED') "
          + "AND w.createdAt < :before")
  int deleteResolvedBefore(@Param("before") OffsetDateTime before);
}
