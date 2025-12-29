package com.rulex.repository;

import com.rulex.entity.TransactionDecision;
import java.time.LocalDateTime;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

/** Repositório para acesso aos dados de Decisões de Transações. */
@Repository
public interface TransactionDecisionRepository extends JpaRepository<TransactionDecision, Long> {

  Optional<TransactionDecision> findByTransactionId(Long transactionId);

  List<TransactionDecision> findByTransactionIdIn(Collection<Long> transactionIds);

  Optional<TransactionDecision> findByExternalTransactionIdAndPayloadRawHash(
      String externalTransactionId, String payloadRawHash);

  Page<TransactionDecision> findByClassification(
      TransactionDecision.TransactionClassification classification, Pageable pageable);

  @Query(
      "SELECT COUNT(d) FROM TransactionDecision d WHERE "
          + "d.classification = :classification AND "
          + "d.createdAt >= :since")
  Long countByClassificationSince(
      @Param("classification") TransactionDecision.TransactionClassification classification,
      @Param("since") LocalDateTime since);

  @Query(
      "SELECT d FROM TransactionDecision d WHERE "
          + "d.createdAt >= :startDate AND "
          + "d.createdAt <= :endDate AND "
          + "(:classification IS NULL OR d.classification = :classification)")
  Page<TransactionDecision> findByDateRangeAndClassification(
      @Param("startDate") LocalDateTime startDate,
      @Param("endDate") LocalDateTime endDate,
      @Param("classification") TransactionDecision.TransactionClassification classification,
      Pageable pageable);

  @Query(
      value =
          "SELECT t.mcc AS mcc, COUNT(*) AS total, "
              + "SUM(CASE WHEN d.classification = 'FRAUD' THEN 1 ELSE 0 END) AS fraud, "
              + "SUM(CASE WHEN d.classification = 'SUSPICIOUS' THEN 1 ELSE 0 END) AS suspicious, "
              + "SUM(CASE WHEN d.classification = 'APPROVED' THEN 1 ELSE 0 END) AS approved "
              + "FROM transaction_decisions d "
              + "JOIN transactions t ON t.id = d.transaction_id "
              + "WHERE d.created_at >= :since "
              + "GROUP BY t.mcc",
      nativeQuery = true)
  List<Object[]> aggregateByMccSince(@Param("since") LocalDateTime since);

  @Query(
      value =
          "SELECT t.merchant_id AS merchantId, MAX(t.merchant_name) AS merchantName, COUNT(*) AS total, "
              + "SUM(CASE WHEN d.classification = 'FRAUD' THEN 1 ELSE 0 END) AS fraud "
              + "FROM transaction_decisions d "
              + "JOIN transactions t ON t.id = d.transaction_id "
              + "WHERE d.created_at >= :since "
              + "GROUP BY t.merchant_id",
      nativeQuery = true)
  List<Object[]> aggregateByMerchantSince(@Param("since") LocalDateTime since);

  @Query(
      value =
          "SELECT date_trunc('hour', d.created_at) AS bucket, COUNT(*) AS total, "
              + "SUM(CASE WHEN d.classification = 'FRAUD' THEN 1 ELSE 0 END) AS fraud "
              + "FROM transaction_decisions d "
              + "WHERE d.created_at >= :since "
              + "GROUP BY bucket ORDER BY bucket",
      nativeQuery = true)
  List<Object[]> timelineHourlySince(@Param("since") LocalDateTime since);

  @Query(
      value =
          "SELECT date_trunc('day', d.created_at) AS bucket, COUNT(*) AS total, "
              + "SUM(CASE WHEN d.classification = 'FRAUD' THEN 1 ELSE 0 END) AS fraud "
              + "FROM transaction_decisions d "
              + "WHERE d.created_at >= :since "
              + "GROUP BY bucket ORDER BY bucket",
      nativeQuery = true)
  List<Object[]> timelineDailySince(@Param("since") LocalDateTime since);
}
