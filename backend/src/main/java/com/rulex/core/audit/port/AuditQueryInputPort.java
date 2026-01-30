package com.rulex.core.audit.port;

import com.rulex.dto.AuditLogDTO;
import java.io.IOException;
import java.io.Writer;
import java.time.LocalDateTime;
import java.util.List;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

public interface AuditQueryInputPort {

  Page<AuditLogDTO> findAuditLogs(
      String actionType,
      String result,
      LocalDateTime startDate,
      LocalDateTime endDate,
      Pageable pageable);

  Page<AuditLogDTO> findAuditLogsByTransactionId(Long transactionId, Pageable pageable);

  List<AuditLogDTO> exportAsList(
      String actionType,
      String result,
      LocalDateTime startDate,
      LocalDateTime endDate,
      Pageable firstPage,
      int limit);

  void exportAsCsv(
      Writer w,
      String actionType,
      String result,
      LocalDateTime startDate,
      LocalDateTime endDate,
      Pageable firstPage,
      int limit)
      throws IOException;
}
