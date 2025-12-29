package com.rulex.controller;

import com.rulex.dto.AuditLogDTO;
import com.rulex.service.AuditQueryService;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.method.annotation.StreamingResponseBody;

/** Controller REST para consulta de logs de auditoria. */
@RestController
@RequestMapping("/audit")
@RequiredArgsConstructor
@Slf4j
public class AuditController {

  private final AuditQueryService auditQueryService;

  /**
   * Lista logs de auditoria com filtros. GET /api/audit?actionType=...&result=...&page=0&size=20
   */
  @GetMapping
  public ResponseEntity<Page<AuditLogDTO>> listAuditLogs(
      @RequestParam(required = false) String actionType,
      @RequestParam(required = false) String result,
      @RequestParam(required = false) String startDate,
      @RequestParam(required = false) String endDate,
      @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "20") int size) {

    log.info(
        "Listando logs de auditoria: actionType={}, result={}, page={}, size={}",
        actionType,
        result,
        page,
        size);

    Pageable pageable = PageRequest.of(page, size);

    LocalDateTime startDateTime = null;
    LocalDateTime endDateTime = null;

    if (startDate != null && !startDate.isEmpty()) {
      startDateTime = LocalDateTime.parse(startDate, DateTimeFormatter.ISO_DATE_TIME);
    }
    if (endDate != null && !endDate.isEmpty()) {
      endDateTime = LocalDateTime.parse(endDate, DateTimeFormatter.ISO_DATE_TIME);
    }

    Page<AuditLogDTO> logs =
        auditQueryService.findAuditLogs(actionType, result, startDateTime, endDateTime, pageable);

    return ResponseEntity.ok(logs);
  }

  /** Exporta logs de auditoria. GET /api/audit/export?format=csv|json&... */
  @GetMapping("/export")
  public ResponseEntity<?> exportAudit(
      @RequestParam(defaultValue = "csv") String format,
      @RequestParam(required = false) String actionType,
      @RequestParam(required = false) String result,
      @RequestParam(required = false) String startDate,
      @RequestParam(required = false) String endDate,
      @RequestParam(defaultValue = "10000") int limit) {

    if (limit <= 0 || limit > 50000) {
      throw new IllegalArgumentException("limit deve estar entre 1 e 50000");
    }

    LocalDateTime startDateTime = null;
    LocalDateTime endDateTime = null;
    if (startDate != null && !startDate.isEmpty()) {
      startDateTime = LocalDateTime.parse(startDate, DateTimeFormatter.ISO_DATE_TIME);
    }
    if (endDate != null && !endDate.isEmpty()) {
      endDateTime = LocalDateTime.parse(endDate, DateTimeFormatter.ISO_DATE_TIME);
    }

    Pageable firstPage =
        PageRequest.of(0, Math.min(1000, limit), Sort.by(Sort.Direction.DESC, "createdAt"));

    if ("json".equalsIgnoreCase(format)) {
      List<AuditLogDTO> out =
          auditQueryService.exportAsList(actionType, result, startDateTime, endDateTime, firstPage, limit);
      return ResponseEntity.ok(out);
    }

    if (!"csv".equalsIgnoreCase(format)) {
      throw new IllegalArgumentException("format inválido (use csv ou json)");
    }

    final LocalDateTime startDateFinal = startDateTime;
    final LocalDateTime endDateFinal = endDateTime;

    StreamingResponseBody body =
        outputStream -> {
          try (Writer w = new OutputStreamWriter(outputStream, StandardCharsets.UTF_8)) {
            w.write(
                "id,transactionId,actionType,result,performedBy,createdAt,description,errorMessage,sourceIp,details\n");
            auditQueryService.exportAsCsv(
                w, actionType, result, startDateFinal, endDateFinal, firstPage, limit);
            w.flush();
          }
        };

    String filename = "rulex-audit.csv";
    return ResponseEntity.ok()
        .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + filename + "\"")
        .contentType(new MediaType("text", "csv", StandardCharsets.UTF_8))
        .body(body);
  }

  /**
   * Obtém logs de auditoria para uma transação específica. GET
   * /api/audit/transaction/{transactionId}
   */
  @GetMapping("/transaction/{transactionId}")
  public ResponseEntity<Page<AuditLogDTO>> getTransactionAuditLogs(
      @PathVariable Long transactionId,
      @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "20") int size) {

    log.info("Obtendo logs de auditoria para transação: {}", transactionId);

    Pageable pageable = PageRequest.of(page, size);
    Page<AuditLogDTO> logs =
        auditQueryService.findAuditLogsByTransactionId(transactionId, pageable);
    return ResponseEntity.ok(logs);
  }
}
