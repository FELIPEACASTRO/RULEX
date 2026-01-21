package com.rulex.controller;

import com.rulex.dto.AuditLogDTO;
import com.rulex.service.AuditQueryService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
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

/** Controller REST para consulta de logs de auditoria. */
@RestController
@RequestMapping("/audit")
@RequiredArgsConstructor
@Slf4j
@Tag(name = "Auditoria", description = "Consulta e exportação de logs de auditoria")
public class AuditController {

  private final AuditQueryService auditQueryService;

  /**
   * Lista logs de auditoria com filtros. GET /api/audit?actionType=...&result=...&page=0&size=20
   */
  @Operation(summary = "Listar logs", description = "Lista logs de auditoria com filtros opcionais")
  @ApiResponse(responseCode = "200", description = "Lista de logs retornada")
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

  /** Exporta logs de auditoria em JSON. GET /api/audit/export?format=json&... */
  @Operation(summary = "Exportar logs JSON", description = "Exporta logs em JSON")
  @ApiResponse(responseCode = "200", description = "Arquivo exportado")
  @GetMapping(value = "/export", produces = MediaType.APPLICATION_JSON_VALUE)
  public ResponseEntity<List<AuditLogDTO>> exportAuditJson(
      @RequestParam(defaultValue = "json") String format,
      @RequestParam(required = false) String actionType,
      @RequestParam(required = false) String result,
      @RequestParam(required = false) String startDate,
      @RequestParam(required = false) String endDate,
      @RequestParam(defaultValue = "10000") int limit) {

    if (!"json".equalsIgnoreCase(format)) {
      throw new IllegalArgumentException("Use /export/csv para exportar em CSV");
    }

    if (limit <= 0 || limit > 50000) {
      throw new IllegalArgumentException("limit deve estar entre 1 e 50000");
    }

    LocalDateTime startDateTime = parseIsoToLocalDateTime(startDate);
    LocalDateTime endDateTime = parseIsoToLocalDateTime(endDate);

    Pageable firstPage =
        PageRequest.of(0, Math.min(1000, limit), Sort.by(Sort.Direction.DESC, "createdAt"));

    List<AuditLogDTO> out =
        auditQueryService.exportAsList(
            actionType, result, startDateTime, endDateTime, firstPage, limit);
    return ResponseEntity.ok(out);
  }

  /** Exporta logs de auditoria em CSV. GET /api/audit/export/csv */
  @Operation(summary = "Exportar logs CSV", description = "Exporta logs em CSV")
  @ApiResponse(responseCode = "200", description = "Arquivo CSV exportado")
  @GetMapping(value = "/export/csv", produces = "text/csv")
  public void exportAuditCsv(
      @RequestParam(required = false) String actionType,
      @RequestParam(required = false) String result,
      @RequestParam(required = false) String startDate,
      @RequestParam(required = false) String endDate,
      @RequestParam(defaultValue = "10000") int limit,
      HttpServletResponse response)
      throws IOException {

    if (limit <= 0 || limit > 50000) {
      throw new IllegalArgumentException("limit deve estar entre 1 e 50000");
    }

    LocalDateTime startDateTime = parseIsoToLocalDateTime(startDate);
    LocalDateTime endDateTime = parseIsoToLocalDateTime(endDate);

    Pageable firstPage =
        PageRequest.of(0, Math.min(1000, limit), Sort.by(Sort.Direction.DESC, "createdAt"));

    response.setContentType("text/csv; charset=UTF-8");
    response.setHeader(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"rulex-audit.csv\"");

    try (Writer w = new OutputStreamWriter(response.getOutputStream(), StandardCharsets.UTF_8)) {
      w.write(
          "id,transactionId,actionType,result,performedBy,createdAt,description,errorMessage,sourceIp,details\n");
      auditQueryService.exportAsCsv(
          w, actionType, result, startDateTime, endDateTime, firstPage, limit);
      w.flush();
    }
  }

  private static LocalDateTime parseIsoToLocalDateTime(String raw) {
    if (raw == null || raw.isBlank()) {
      return null;
    }
    try {
      return OffsetDateTime.parse(raw, DateTimeFormatter.ISO_DATE_TIME).toLocalDateTime();
    } catch (Exception ignored) {
      return LocalDateTime.parse(raw, DateTimeFormatter.ISO_DATE_TIME);
    }
  }

  /**
   * Obtém logs de auditoria para uma transação específica. GET
   * /api/audit/transaction/{transactionId}
   */
  @Operation(summary = "Logs por transação", description = "Obtém logs de uma transação específica")
  @ApiResponse(responseCode = "200", description = "Logs da transação")
  @GetMapping("/transaction/{transactionId}")
  public ResponseEntity<Page<AuditLogDTO>> getTransactionAuditLogs(
      @Parameter(description = "ID da transação") @PathVariable Long transactionId,
      @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "20") int size) {

    log.info("Obtendo logs de auditoria para transação: {}", transactionId);

    Pageable pageable = PageRequest.of(page, size);
    Page<AuditLogDTO> logs =
        auditQueryService.findAuditLogsByTransactionId(transactionId, pageable);
    return ResponseEntity.ok(logs);
  }
}
