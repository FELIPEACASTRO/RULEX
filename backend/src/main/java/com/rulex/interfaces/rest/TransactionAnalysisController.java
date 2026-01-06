package com.rulex.interfaces.rest;

import com.rulex.application.port.in.AnalyzeTransactionUseCase;
import com.rulex.application.port.in.AnalyzeTransactionUseCase.AnalysisResult;
import com.rulex.domain.model.TransactionData;
import com.rulex.interfaces.rest.dto.AnalyzeTransactionRequest;
import com.rulex.interfaces.rest.dto.AnalyzeTransactionResponse;
import com.rulex.interfaces.rest.dto.AnalyzeTransactionResponse.TriggeredRuleInfo;
import io.micrometer.core.annotation.Timed;
import jakarta.validation.Valid;
import java.time.Instant;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * REST Controller for transaction analysis using hexagonal architecture.
 *
 * <p>This controller serves as a primary adapter (driving adapter) that:
 *
 * <ul>
 *   <li>Receives HTTP requests
 *   <li>Transforms DTOs to domain objects
 *   <li>Delegates to application use cases
 *   <li>Transforms domain results back to DTOs
 * </ul>
 *
 * <p>It does NOT contain business logic - all fraud detection logic resides in the domain layer.
 */
@RestController
@RequestMapping("/api/v2/transactions")
@RequiredArgsConstructor
@Slf4j
public class TransactionAnalysisController {

  private final AnalyzeTransactionUseCase analyzeTransactionUseCase;

  /**
   * Analyzes a transaction for fraud.
   *
   * <p>POST /api/v2/transactions/analyze
   *
   * @param request Transaction data to analyze
   * @return Analysis result with classification, score, and triggered rules
   */
  @PostMapping("/analyze")
  @Timed(
      value = "rulex.transaction.analyze.v2",
      description = "Transaction analysis latency (hexagonal)")
  public ResponseEntity<AnalyzeTransactionResponse> analyze(
      @Valid @RequestBody AnalyzeTransactionRequest request) {

    log.debug("Analyzing transaction: {}", request.getExternalTransactionId());

    // Transform DTO to Domain object
    TransactionData transactionData = toDomainModel(request);

    // Delegate to use case
    AnalysisResult result = analyzeTransactionUseCase.analyze(transactionData);

    // Transform result to response DTO
    AnalyzeTransactionResponse response = toResponseDto(result, request.getExternalTransactionId());

    log.info(
        "Transaction {} analyzed: classification={}, score={}, rules={}, time={}ms",
        request.getExternalTransactionId(),
        response.getClassification(),
        response.getRiskScore(),
        response.getTriggeredRules() != null ? response.getTriggeredRules().size() : 0,
        response.getProcessingTimeMs());

    return ResponseEntity.ok(response);
  }

  /**
   * Health check for the analysis endpoint.
   *
   * <p>GET /api/v2/transactions/health
   */
  @GetMapping("/health")
  public ResponseEntity<Map<String, Object>> health() {
    Map<String, Object> health = new HashMap<>();
    health.put("status", "UP");
    health.put("version", "2.0");
    health.put("architecture", "hexagonal");
    health.put("timestamp", Instant.now());
    return ResponseEntity.ok(health);
  }

  /** Transforms request DTO to domain TransactionData. */
  private TransactionData toDomainModel(AnalyzeTransactionRequest request) {
    Map<String, Object> customFields = request.getCustomFields();
    if (customFields == null) {
      customFields = new HashMap<>();
    }

    // Add standard fields to the data map for rule evaluation
    Map<String, Object> allFields = new HashMap<>(customFields);
    addIfNotNull(allFields, "merchantId", request.getMerchantId());
    addIfNotNull(allFields, "merchantName", request.getMerchantName());
    addIfNotNull(allFields, "mcc", request.getMcc());
    addIfNotNull(allFields, "currency", request.getCurrency());
    addIfNotNull(allFields, "country", request.getCountry());
    addIfNotNull(allFields, "channel", request.getChannel());
    addIfNotNull(allFields, "deviceId", request.getDeviceId());
    addIfNotNull(allFields, "deviceFingerprint", request.getDeviceFingerprint());
    addIfNotNull(allFields, "ipAddress", request.getIpAddress());
    addIfNotNull(allFields, "latitude", request.getLatitude());
    addIfNotNull(allFields, "longitude", request.getLongitude());
    addIfNotNull(allFields, "email", request.getEmail());
    addIfNotNull(allFields, "phone", request.getPhone());
    addIfNotNull(allFields, "cardBin", request.getCardBin());
    addIfNotNull(allFields, "cardType", request.getCardType());
    addIfNotNull(allFields, "isRecurring", request.getIsRecurring());
    addIfNotNull(allFields, "is3dsAuthenticated", request.getIs3dsAuthenticated());

    return TransactionData.builder()
        .transactionId(request.getExternalTransactionId())
        .customerId(request.getCustomerId())
        .amount(request.getAmount())
        .timestamp(Instant.now())
        .data(allFields)
        .build();
  }

  private void addIfNotNull(Map<String, Object> map, String key, Object value) {
    if (value != null) {
      map.put(key, value);
    }
  }

  /** Transforms domain AnalysisResult to response DTO. */
  private AnalyzeTransactionResponse toResponseDto(AnalysisResult result, String transactionId) {
    List<TriggeredRuleInfo> triggeredRules = Collections.emptyList();

    if (result.triggeredRules() != null && !result.triggeredRules().isEmpty()) {
      triggeredRules =
          result.triggeredRules().stream()
              .map(
                  tr ->
                      TriggeredRuleInfo.builder()
                          .ruleId(tr.ruleId())
                          .ruleName(tr.ruleName())
                          .ruleType(tr.ruleType())
                          .score(tr.score())
                          .isShadowMode(tr.isShadowMode())
                          .message(tr.message())
                          .build())
              .toList();
    }

    boolean hasShadowModeRules =
        triggeredRules.stream().anyMatch(r -> Boolean.TRUE.equals(r.getIsShadowMode()));

    return AnalyzeTransactionResponse.builder()
        .decisionId(UUID.randomUUID().toString())
        .transactionId(transactionId)
        .classification(result.classification().name())
        .riskScore(result.totalScore())
        .reason(buildReason(result))
        .triggeredRules(triggeredRules)
        .processingTimeMs(result.processingTimeMs())
        .timestamp(Instant.now())
        .hasShadowModeRules(hasShadowModeRules)
        .build();
  }

  /** Builds a human-readable reason string. */
  private String buildReason(AnalysisResult result) {
    if (result.triggeredRules() == null || result.triggeredRules().isEmpty()) {
      return "Nenhuma regra disparada - transação aprovada";
    }

    int activeRules =
        (int)
            result.triggeredRules().stream()
                .filter(r -> !Boolean.TRUE.equals(r.isShadowMode()))
                .count();

    if (activeRules == 0) {
      return "Apenas regras em shadow mode disparadas";
    }

    String ruleNames =
        result.triggeredRules().stream()
            .filter(r -> !Boolean.TRUE.equals(r.isShadowMode()))
            .map(AnalyzeTransactionUseCase.TriggeredRule::ruleName)
            .limit(3)
            .reduce((a, b) -> a + ", " + b)
            .orElse("");

    return String.format(
        "%d regra(s) disparada(s): %s%s",
        activeRules, ruleNames, activeRules > 3 ? " e outras" : "");
  }
}
