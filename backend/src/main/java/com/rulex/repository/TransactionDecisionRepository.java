package com.rulex.repository;

import com.rulex.entity.TransactionDecision;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.Optional;

/**
 * Repositório para acesso aos dados de Decisões de Transações.
 */
@Repository
public interface TransactionDecisionRepository extends JpaRepository<TransactionDecision, Long> {

    Optional<TransactionDecision> findByTransactionId(Long transactionId);

    Page<TransactionDecision> findByClassification(
            TransactionDecision.TransactionClassification classification,
            Pageable pageable);

    @Query("SELECT COUNT(d) FROM TransactionDecision d WHERE " +
           "d.classification = :classification AND " +
           "d.createdAt >= :since")
    Long countByClassificationSince(
            @Param("classification") TransactionDecision.TransactionClassification classification,
            @Param("since") LocalDateTime since);

    @Query("SELECT d FROM TransactionDecision d WHERE " +
           "d.createdAt >= :startDate AND " +
           "d.createdAt <= :endDate AND " +
           "(:classification IS NULL OR d.classification = :classification)")
    Page<TransactionDecision> findByDateRangeAndClassification(
            @Param("startDate") LocalDateTime startDate,
            @Param("endDate") LocalDateTime endDate,
            @Param("classification") TransactionDecision.TransactionClassification classification,
            Pageable pageable);

}
