package com.rulex.controller;

import com.rulex.dto.AuditLogDTO;
import com.rulex.service.AuditQueryService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

/**
 * Controller REST para consulta de logs de auditoria.
 */
@RestController
@RequestMapping("/audit")
@RequiredArgsConstructor
@Slf4j
@CrossOrigin(origins = "*", maxAge = 3600)
public class AuditController {

    private final AuditQueryService auditQueryService;

    /**
     * Lista logs de auditoria com filtros.
     * GET /api/audit?actionType=...&result=...&page=0&size=20
     */
    @GetMapping
    public ResponseEntity<Page<AuditLogDTO>> listAuditLogs(
            @RequestParam(required = false) String actionType,
            @RequestParam(required = false) String result,
            @RequestParam(required = false) String startDate,
            @RequestParam(required = false) String endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size) {
        
        log.info("Listando logs de auditoria: actionType={}, result={}, page={}, size={}", 
            actionType, result, page, size);
        
        try {
            Pageable pageable = PageRequest.of(page, size);
            
            LocalDateTime startDateTime = null;
            LocalDateTime endDateTime = null;
            
            if (startDate != null && !startDate.isEmpty()) {
                startDateTime = LocalDateTime.parse(startDate, DateTimeFormatter.ISO_DATE_TIME);
            }
            if (endDate != null && !endDate.isEmpty()) {
                endDateTime = LocalDateTime.parse(endDate, DateTimeFormatter.ISO_DATE_TIME);
            }
            
            Page<AuditLogDTO> logs = auditQueryService.findAuditLogs(
                actionType, result, startDateTime, endDateTime, pageable);
            
            return ResponseEntity.ok(logs);
        } catch (Exception e) {
            log.error("Erro ao listar logs de auditoria", e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }

    /**
     * Obtém logs de auditoria para uma transação específica.
     * GET /api/audit/transaction/{transactionId}
     */
    @GetMapping("/transaction/{transactionId}")
    public ResponseEntity<Page<AuditLogDTO>> getTransactionAuditLogs(
            @PathVariable Long transactionId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size) {
        
        log.info("Obtendo logs de auditoria para transação: {}", transactionId);
        
        try {
            Pageable pageable = PageRequest.of(page, size);
            Page<AuditLogDTO> logs = auditQueryService.findAuditLogsByTransactionId(transactionId, pageable);
            return ResponseEntity.ok(logs);
        } catch (Exception e) {
            log.error("Erro ao obter logs de auditoria", e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }

}
