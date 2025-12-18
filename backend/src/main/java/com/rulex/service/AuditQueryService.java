package com.rulex.service;

import com.rulex.dto.AuditLogDTO;
import com.rulex.entity.AuditLog;
import com.rulex.repository.AuditLogRepository;
import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/** Serviço para consulta de logs de auditoria. */
@Service
@RequiredArgsConstructor
@Slf4j
@Transactional(readOnly = true)
public class AuditQueryService {

  private final AuditLogRepository auditLogRepository;

  /** Busca logs de auditoria com filtros. */
  public Page<AuditLogDTO> findAuditLogs(
      String actionType,
      String result,
      LocalDateTime startDate,
      LocalDateTime endDate,
      Pageable pageable) {

    AuditLog.AuditActionType actionTypeEnum = null;
    AuditLog.AuditResult resultEnum = null;

    if (actionType != null && !actionType.isEmpty()) {
      try {
        actionTypeEnum = AuditLog.AuditActionType.valueOf(actionType);
      } catch (IllegalArgumentException e) {
        log.warn("Tipo de ação inválido: {}", actionType);
      }
    }

    if (result != null && !result.isEmpty()) {
      try {
        resultEnum = AuditLog.AuditResult.valueOf(result);
      } catch (IllegalArgumentException e) {
        log.warn("Resultado inválido: {}", result);
      }
    }

    LocalDateTime start = startDate != null ? startDate : LocalDateTime.now().minusDays(30);
    LocalDateTime end = endDate != null ? endDate : LocalDateTime.now();

    Page<AuditLog> logs =
        auditLogRepository.findByDateRangeAndFilters(
            start, end, actionTypeEnum, resultEnum, pageable);

    return logs.map(this::convertToDTO);
  }

  /** Busca logs de auditoria para uma transação específica. */
  public Page<AuditLogDTO> findAuditLogsByTransactionId(Long transactionId, Pageable pageable) {
    List<AuditLog> logs = auditLogRepository.findByTransactionId(transactionId);

    List<AuditLogDTO> dtos = logs.stream().map(this::convertToDTO).collect(Collectors.toList());

    int start = (int) pageable.getOffset();
    int end = Math.min((start + pageable.getPageSize()), dtos.size());

    return new PageImpl<>(dtos.subList(start, end), pageable, dtos.size());
  }

  /** Converte AuditLog para DTO. */
  private AuditLogDTO convertToDTO(AuditLog log) {
    return AuditLogDTO.builder()
        .id(log.getId())
        .transactionId(log.getTransactionId())
        .actionType(log.getActionType().name())
        .description(log.getDescription())
        .details(log.getDetails())
        .performedBy(log.getPerformedBy())
        .result(log.getResult().name())
        .errorMessage(log.getErrorMessage())
        .sourceIp(log.getSourceIp())
        .createdAt(log.getCreatedAt())
        .build();
  }
}
