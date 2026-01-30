package com.rulex.service;

import com.rulex.core.audit.port.AuditQueryInputPort;
import com.rulex.core.audit.port.AuditQueryRepositoryPort;
import com.rulex.core.audit.usecase.AuditQueryUseCase;
import com.rulex.dto.AuditLogDTO;
import java.io.IOException;
import java.io.Writer;
import java.time.LocalDateTime;
import java.util.List;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/** Adapter Spring para consulta de logs de auditoria. */
@Service
@Transactional(readOnly = true)
public class AuditQueryService implements AuditQueryInputPort {

  private final AuditQueryUseCase useCase;

  public AuditQueryService(AuditQueryRepositoryPort repositoryPort) {
    this.useCase = new AuditQueryUseCase(repositoryPort);
  }

  @Override
  public Page<AuditLogDTO> findAuditLogs(
      String actionType,
      String result,
      LocalDateTime startDate,
      LocalDateTime endDate,
      Pageable pageable) {
    return useCase.findAuditLogs(actionType, result, startDate, endDate, pageable);
  }

  @Override
  public Page<AuditLogDTO> findAuditLogsByTransactionId(Long transactionId, Pageable pageable) {
    return useCase.findAuditLogsByTransactionId(transactionId, pageable);
  }

  @Override
  public List<AuditLogDTO> exportAsList(
      String actionType,
      String result,
      LocalDateTime startDate,
      LocalDateTime endDate,
      Pageable firstPage,
      int limit) {
    return useCase.exportAsList(actionType, result, startDate, endDate, firstPage, limit);
  }

  @Override
  public void exportAsCsv(
      Writer w,
      String actionType,
      String result,
      LocalDateTime startDate,
      LocalDateTime endDate,
      Pageable firstPage,
      int limit)
      throws IOException {
    useCase.exportAsCsv(w, actionType, result, startDate, endDate, firstPage, limit);
  }
}

