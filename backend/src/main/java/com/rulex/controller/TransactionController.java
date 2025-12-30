package com.rulex.controller;

import com.rulex.api.RawPayloadCaptureFilter;
import com.rulex.dto.TransactionRequest;
import com.rulex.dto.TransactionResponse;
import com.rulex.dto.TriggeredRuleDTO;
import com.rulex.service.AdvancedRuleEngineService;
import com.rulex.service.RuleEngineService;
import com.rulex.service.TransactionQueryService;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.time.Clock;
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
import org.springframework.web.servlet.mvc.method.annotation.StreamingResponseBody;

/** Controller REST para processamento e consulta de transações. */
@RestController
@RequestMapping("/transactions")
@RequiredArgsConstructor
@Slf4j
public class TransactionController {

  private final RuleEngineService ruleEngineService;
  private final TransactionQueryService transactionQueryService;
  private final AdvancedRuleEngineService advancedRuleEngineService;
  private final Clock clock;

  /** Analisa uma transação e retorna a classificação de fraude. POST /api/transactions/analyze */
  @PostMapping("/analyze")
  public ResponseEntity<TransactionResponse> analyzeTransaction(
      @Valid @RequestBody TransactionRequest request, HttpServletRequest httpRequest) {

    log.info("Analisando transação: {}", request.getExternalTransactionId());

    byte[] rawBytes = (byte[]) httpRequest.getAttribute(RawPayloadCaptureFilter.RAW_BYTES_ATTR);
    String contentType = httpRequest.getContentType();
    TransactionResponse response =
        ruleEngineService.analyzeTransaction(request, rawBytes, contentType);
    return ResponseEntity.ok(response);
  }

  /**
   * Lista transações com filtros opcionais. GET
   * /api/transactions?customerId=...&merchantId=...&page=0&size=20
   */
  @GetMapping
  public ResponseEntity<Page<TransactionResponse>> listTransactions(
      @RequestParam(required = false) String customerId,
      @RequestParam(required = false) String merchantId,
      @RequestParam(required = false) String classification,
      @RequestParam(required = false) Integer mcc,
      @RequestParam(required = false) BigDecimal minAmount,
      @RequestParam(required = false) BigDecimal maxAmount,
      @RequestParam(required = false) String startDate,
      @RequestParam(required = false) String endDate,
      @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "20") int size) {

    log.info(
        "Listando transações com filtros: customerId={}, merchantId={}, page={}, size={}",
        customerId,
        merchantId,
        page,
        size);

    Pageable pageable = PageRequest.of(page, size);

    LocalDateTime startDateTime = null;
    LocalDateTime endDateTime = null;

    if (startDate != null && !startDate.isEmpty()) {
      // Frontend can send ISO with timezone (e.g. "...Z"); accept both.
      startDateTime = parseIsoToLocalDateTime(startDate);
    }
    if (endDate != null && !endDate.isEmpty()) {
      endDateTime = parseIsoToLocalDateTime(endDate);
    }

    Page<TransactionResponse> transactions =
        transactionQueryService.findTransactions(
            customerId,
            merchantId,
            classification,
            mcc,
            minAmount,
            maxAmount,
            startDateTime,
            endDateTime,
            pageable);

    return ResponseEntity.ok(transactions);
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
   * Exporta transações com os mesmos filtros da listagem.
   *
   * <p>GET /api/transactions/export?format=csv|json&...filtros...
   */
  @GetMapping("/export")
  public ResponseEntity<?> exportTransactions(
      @RequestParam(defaultValue = "csv") String format,
      @RequestParam(required = false) String customerId,
      @RequestParam(required = false) String merchantId,
      @RequestParam(required = false) String classification,
      @RequestParam(required = false) Integer mcc,
      @RequestParam(required = false) BigDecimal minAmount,
      @RequestParam(required = false) BigDecimal maxAmount,
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

    // Ordenar por mais recentes
    Pageable firstPage =
        PageRequest.of(0, Math.min(1000, limit), Sort.by(Sort.Direction.DESC, "createdAt"));

    if ("json".equalsIgnoreCase(format)) {
      List<TransactionResponse> out =
          transactionQueryService.exportAsList(
              customerId,
              merchantId,
              classification,
              mcc,
              minAmount,
              maxAmount,
              startDateTime,
              endDateTime,
              firstPage,
              limit);
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
            // Header
            w.write(
                "id,transactionId,customerIdFromHeader,merchantId,merchantName,transactionAmount,transactionDate,transactionTime,classification,riskScore,timestamp,reason\n");
            transactionQueryService.exportAsCsv(
                w,
                customerId,
                merchantId,
                classification,
                mcc,
                minAmount,
                maxAmount,
                startDateFinal,
                endDateFinal,
                firstPage,
                limit);
            w.flush();
          }
        };

    String filename = "rulex-transactions.csv";
    return ResponseEntity.ok()
        .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + filename + "\"")
        .contentType(new MediaType("text", "csv", StandardCharsets.UTF_8))
        .body(body);
  }

  /** Obtém detalhes de uma transação específica. GET /api/transactions/{id} */
  @GetMapping("/{id}")
  public ResponseEntity<TransactionResponse> getTransaction(@PathVariable Long id) {
    log.info("Obtendo detalhes da transação: {}", id);

    TransactionResponse transaction = transactionQueryService.getTransactionById(id);
    return ResponseEntity.ok(transaction);
  }

  /**
   * Obtém detalhes de uma transação pelo ID externo. GET /api/transactions/external/{externalId}
   */
  @GetMapping("/external/{externalId}")
  public ResponseEntity<TransactionResponse> getTransactionByExternalId(
      @PathVariable String externalId) {
    log.info("Obtendo transação pelo ID externo: {}", externalId);

    TransactionResponse transaction =
        transactionQueryService.getTransactionByExternalId(externalId);
    return ResponseEntity.ok(transaction);
  }

  /**
   * Analisa uma transação com as 28 novas regras avançadas. POST /api/transactions/analyze-advanced
   */
  @PostMapping("/analyze-advanced")
  public ResponseEntity<TransactionResponse> analyzeTransactionAdvanced(
      @Valid @RequestBody TransactionRequest request) {

    log.info("Analisando transação com regras avançadas: {}", request.getExternalTransactionId());

    long startTime = System.currentTimeMillis();

    AdvancedRuleEngineService.AdvancedExecution execution =
        advancedRuleEngineService.executeAllAdvancedRulesDetailed(request);
    AdvancedRuleEngineService.RuleResult result = execution.result();

    long processingTime = System.currentTimeMillis() - startTime;
    java.util.List<TriggeredRuleDTO> triggeredRules =
        execution.triggeredRules() != null ? execution.triggeredRules() : java.util.List.of();

    return ResponseEntity.ok(
        TransactionResponse.builder()
            .customerIdFromHeader(request.getCustomerIdFromHeader())
            .merchantId(request.getMerchantId())
            .merchantName(request.getMerchantName())
            .transactionAmount(request.getTransactionAmount())
            .transactionDate(request.getTransactionDate())
            .transactionTime(request.getTransactionTime())
            .transactionId(request.getExternalTransactionId())
            .classification(result.name())
            .riskScore(
                result == AdvancedRuleEngineService.RuleResult.FRAUD
                    ? 90
                    : (result == AdvancedRuleEngineService.RuleResult.SUSPICIOUS ? 60 : 10))
            .triggeredRules(triggeredRules)
            .reason(
                triggeredRules.isEmpty()
                    ? "Resultado de regras avançadas"
                    : ("Resultado de regras avançadas. Regras acionadas: "
                        + String.join(
                            ", ", triggeredRules.stream().map(TriggeredRuleDTO::getName).toList())))
            .rulesetVersion("advanced")
            .processingTimeMs(processingTime)
            .timestamp(OffsetDateTime.now(clock))
            .success(true)
            .build());
  }
}
