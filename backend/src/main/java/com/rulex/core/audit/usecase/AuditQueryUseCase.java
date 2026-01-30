package com.rulex.core.audit.usecase;

import com.rulex.core.audit.port.AuditQueryInputPort;
import com.rulex.core.audit.port.AuditQueryRepositoryPort;
import com.rulex.dto.AuditLogDTO;
import com.rulex.entity.AuditLog;
import java.io.IOException;
import java.io.Writer;
import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Collectors;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;

/** Caso de uso para consulta de logs de auditoria. */
public class AuditQueryUseCase implements AuditQueryInputPort {

  private final AuditQueryRepositoryPort repositoryPort;

  public AuditQueryUseCase(AuditQueryRepositoryPort repositoryPort) {
    this.repositoryPort = repositoryPort;
  }

  /** Busca logs de auditoria com filtros. */
  @Override
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
      } catch (IllegalArgumentException ignored) {
        actionTypeEnum = null;
      }
    }

    if (result != null && !result.isEmpty()) {
      try {
        resultEnum = AuditLog.AuditResult.valueOf(result);
      } catch (IllegalArgumentException ignored) {
        resultEnum = null;
      }
    }

    LocalDateTime start = startDate != null ? startDate : LocalDateTime.now().minusDays(30);
    LocalDateTime end = endDate != null ? endDate : LocalDateTime.now();

    Page<AuditLog> logs =
        repositoryPort.findByDateRangeAndFilters(
            start, end, actionTypeEnum, resultEnum, pageable);

    return logs.map(this::convertToDTO);
  }

  /** Busca logs de auditoria para uma transação específica. */
  @Override
  public Page<AuditLogDTO> findAuditLogsByTransactionId(Long transactionId, Pageable pageable) {
    List<AuditLog> logs = repositoryPort.findByTransactionId(transactionId);

    List<AuditLogDTO> dtos = logs.stream().map(this::convertToDTO).collect(Collectors.toList());

    int start = (int) pageable.getOffset();
    int end = Math.min((start + pageable.getPageSize()), dtos.size());

    return new PageImpl<>(dtos.subList(start, end), pageable, dtos.size());
  }

  @Override
  public List<AuditLogDTO> exportAsList(
      String actionType,
      String result,
      LocalDateTime startDate,
      LocalDateTime endDate,
      Pageable firstPage,
      int limit) {
    java.util.ArrayList<AuditLogDTO> out = new java.util.ArrayList<>();
    Pageable pageable =
        firstPage != null
            ? firstPage
            : PageRequest.of(0, Math.min(1000, limit), Sort.by(Sort.Direction.DESC, "createdAt"));

    while (out.size() < limit) {
      Page<AuditLogDTO> page = findAuditLogs(actionType, result, startDate, endDate, pageable);
      out.addAll(page.getContent());
      if (!page.hasNext()) break;
      pageable =
          PageRequest.of(pageable.getPageNumber() + 1, pageable.getPageSize(), pageable.getSort());
    }
    if (out.size() > limit) {
      return out.subList(0, limit);
    }
    return out;
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
    int written = 0;
    Pageable pageable =
        firstPage != null
            ? firstPage
            : PageRequest.of(0, Math.min(1000, limit), Sort.by(Sort.Direction.DESC, "createdAt"));

    while (written < limit) {
      Page<AuditLogDTO> page = findAuditLogs(actionType, result, startDate, endDate, pageable);
      for (AuditLogDTO row : page.getContent()) {
        if (written >= limit) break;
        w.write(toCsvLine(row));
        w.write("\n");
        written++;
      }
      if (!page.hasNext()) break;
      pageable =
          PageRequest.of(pageable.getPageNumber() + 1, pageable.getPageSize(), pageable.getSort());
    }
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

  private String toCsvLine(AuditLogDTO r) {
    return csv(r.getId())
        + ","
        + csv(r.getTransactionId())
        + ","
        + csv(r.getActionType())
        + ","
        + csv(r.getResult())
        + ","
        + csv(r.getPerformedBy())
        + ","
        + csv(r.getCreatedAt())
        + ","
        + csv(r.getDescription())
        + ","
        + csv(r.getErrorMessage())
        + ","
        + csv(r.getSourceIp())
        + ","
        + csv(r.getDetails());
  }

  private static String csv(Object v) {
    if (v == null) return "";
    String s = String.valueOf(v).replace("\r", " ").replace("\n", " ");
    if (s.contains(",") || s.contains("\"")) {
      s = "\"" + s.replace("\"", "\"\"") + "\"";
    }
    return s;
  }
}
