package com.rulex.adapter.audit;

import com.rulex.core.audit.port.AuditQueryRepositoryPort;
import com.rulex.entity.AuditLog;
import com.rulex.repository.AuditLogRepository;
import java.time.LocalDateTime;
import java.util.List;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Component;

@Component
public class AuditQueryRepositoryAdapter implements AuditQueryRepositoryPort {

  private final AuditLogRepository auditLogRepository;

  public AuditQueryRepositoryAdapter(AuditLogRepository auditLogRepository) {
    this.auditLogRepository = auditLogRepository;
  }

  @Override
  public Page<AuditLog> findByDateRangeAndFilters(
      LocalDateTime start,
      LocalDateTime end,
      AuditLog.AuditActionType actionType,
      AuditLog.AuditResult result,
      Pageable pageable) {
    return auditLogRepository.findByDateRangeAndFilters(start, end, actionType, result, pageable);
  }

  @Override
  public List<AuditLog> findByTransactionId(Long transactionId) {
    return auditLogRepository.findByTransactionId(transactionId);
  }
}
