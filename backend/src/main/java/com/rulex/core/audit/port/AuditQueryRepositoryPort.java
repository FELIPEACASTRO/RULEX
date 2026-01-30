package com.rulex.core.audit.port;

import com.rulex.entity.AuditLog;
import java.time.LocalDateTime;
import java.util.List;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

public interface AuditQueryRepositoryPort {

  Page<AuditLog> findByDateRangeAndFilters(
      LocalDateTime start,
      LocalDateTime end,
      AuditLog.AuditActionType actionType,
      AuditLog.AuditResult result,
      Pageable pageable);

  List<AuditLog> findByTransactionId(Long transactionId);
}
