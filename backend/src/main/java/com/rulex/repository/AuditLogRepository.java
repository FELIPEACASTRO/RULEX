package com.rulex.repository;

import com.rulex.entity.AuditLog;
import java.time.LocalDateTime;
import java.util.List;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

/** Repositório para acesso aos dados de Auditoria. */
@Repository
public interface AuditLogRepository extends JpaRepository<AuditLog, Long> {

  List<AuditLog> findByTransactionId(Long transactionId);

  Page<AuditLog> findByActionType(AuditLog.AuditActionType actionType, Pageable pageable);

  @Query(
      "SELECT a FROM AuditLog a WHERE "
          + "a.createdAt >= :startDate AND "
          + "a.createdAt <= :endDate AND "
          + "(:actionType IS NULL OR a.actionType = :actionType) AND "
          + "(:result IS NULL OR a.result = :result)")
  Page<AuditLog> findByDateRangeAndFilters(
      @Param("startDate") LocalDateTime startDate,
      @Param("endDate") LocalDateTime endDate,
      @Param("actionType") AuditLog.AuditActionType actionType,
      @Param("result") AuditLog.AuditResult result,
      Pageable pageable);
}
