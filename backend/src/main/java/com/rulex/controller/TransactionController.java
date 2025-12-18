package com.rulex.controller;

import com.rulex.dto.TransactionRequest;
import com.rulex.dto.TransactionResponse;
import com.rulex.service.RuleEngineService;
import com.rulex.service.TransactionQueryService;
import com.rulex.service.AdvancedRuleEngineService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

/**
 * Controller REST para processamento e consulta de transações.
 */
@RestController
@RequestMapping("/transactions")
@RequiredArgsConstructor
@Slf4j
public class TransactionController {

    private final RuleEngineService ruleEngineService;
    private final TransactionQueryService transactionQueryService;
    private final AdvancedRuleEngineService advancedRuleEngineService;

    /**
     * Analisa uma transação e retorna a classificação de fraude.
     * POST /api/transactions/analyze
     */
    @PostMapping("/analyze")
    public ResponseEntity<TransactionResponse> analyzeTransaction(
            @Valid @RequestBody TransactionRequest request) {
        
        log.info("Analisando transação: {}", request.getExternalTransactionId());
        
        try {
            TransactionResponse response = ruleEngineService.analyzeTransaction(request);
            return ResponseEntity.ok(response);
        } catch (Exception e) {
            log.error("Erro ao analisar transação: {}", request.getExternalTransactionId(), e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }

    /**
     * Lista transações com filtros opcionais.
     * GET /api/transactions?customerId=...&merchantId=...&page=0&size=20
     */
    @GetMapping
    public ResponseEntity<Page<TransactionResponse>> listTransactions(
            @RequestParam(required = false) String customerId,
            @RequestParam(required = false) String merchantId,
            @RequestParam(required = false) Integer mcc,
            @RequestParam(required = false) BigDecimal minAmount,
            @RequestParam(required = false) BigDecimal maxAmount,
            @RequestParam(required = false) String startDate,
            @RequestParam(required = false) String endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size) {
        
        log.info("Listando transações com filtros: customerId={}, merchantId={}, page={}, size={}", 
            customerId, merchantId, page, size);
        
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
            
            Page<TransactionResponse> transactions = transactionQueryService.findTransactions(
                customerId, merchantId, mcc, minAmount, maxAmount, startDateTime, endDateTime, pageable);
            
            return ResponseEntity.ok(transactions);
        } catch (Exception e) {
            log.error("Erro ao listar transações", e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }

    /**
     * Obtém detalhes de uma transação específica.
     * GET /api/transactions/{id}
     */
    @GetMapping("/{id}")
    public ResponseEntity<TransactionResponse> getTransaction(@PathVariable Long id) {
        log.info("Obtendo detalhes da transação: {}", id);
        
        try {
            TransactionResponse transaction = transactionQueryService.getTransactionById(id);
            return ResponseEntity.ok(transaction);
        } catch (Exception e) {
            log.error("Erro ao obter transação: {}", id, e);
            return ResponseEntity.status(HttpStatus.NOT_FOUND).build();
        }
    }

    /**
     * Obtém detalhes de uma transação pelo ID externo.
     * GET /api/transactions/external/{externalId}
     */
    @GetMapping("/external/{externalId}")
    public ResponseEntity<TransactionResponse> getTransactionByExternalId(@PathVariable String externalId) {
        log.info("Obtendo transação pelo ID externo: {}", externalId);
        
        try {
            TransactionResponse transaction = transactionQueryService.getTransactionByExternalId(externalId);
            return ResponseEntity.ok(transaction);
        } catch (Exception e) {
            log.error("Erro ao obter transação: {}", externalId, e);
            return ResponseEntity.status(HttpStatus.NOT_FOUND).build();
        }
    }

    /**
     * Analisa uma transação com as 28 novas regras avançadas.
     * POST /api/transactions/analyze-advanced
     */
    @PostMapping("/analyze-advanced")
    public ResponseEntity<TransactionResponse> analyzeTransactionAdvanced(
            @Valid @RequestBody TransactionRequest request) {
        
        log.info("Analisando transação com regras avançadas: {}", request.getExternalTransactionId());
        
        try {
            AdvancedRuleEngineService.RuleResult result = advancedRuleEngineService.executeAllAdvancedRules(request);
            return ResponseEntity.ok(TransactionResponse.builder()
                .transactionId(request.getExternalTransactionId())
                .classification(result.name())
                .riskScore(result == AdvancedRuleEngineService.RuleResult.FRAUD ? 90 : (result == AdvancedRuleEngineService.RuleResult.SUSPICIOUS ? 60 : 10))
                .triggeredRules(java.util.List.of())
                .reason("Resultado de regras avançadas")
                .rulesetVersion("advanced")
                .processingTimeMs(0L)
                .timestamp(LocalDateTime.now())
                .success(true)
                .build());
        } catch (Exception e) {
            log.error("Erro ao analisar transação com regras avançadas: {}", request.getExternalTransactionId(), e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }

}
