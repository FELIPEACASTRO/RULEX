package com.rulex.service.engine;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.dto.EvaluateResponse;
import com.rulex.dto.PopupDTO;
import com.rulex.dto.RuleHitDTO;
import com.rulex.dto.TriggeredRuleDTO;
import com.rulex.dto.TransactionResponse;
import com.rulex.entity.RuleConfiguration;
import com.rulex.entity.Transaction;
import com.rulex.entity.TransactionDecision;
import com.rulex.repository.RuleConfigurationRepository;
import com.rulex.repository.TransactionDecisionRepository;
import com.rulex.core.engine.model.RuleEvaluationResult;
import com.rulex.v31.execlog.ExecutionEventType;
import com.rulex.v31.execlog.RuleExecutionLogService;
import java.time.Clock;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DataAccessException;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Slf4j
public class RuleEngineResponseBuilder {

  private final TransactionDecisionRepository decisionRepository;
  private final RuleConfigurationRepository ruleConfigRepository;
  private final RuleExecutionLogService ruleExecutionLogService;
  private final ObjectMapper objectMapper;
  private final Clock clock;

  public TransactionResponse buildResponseFromTamperDecision(
      TransactionDecision decision, long startTime) {
    long processingTime = System.currentTimeMillis() - startTime;
    return TransactionResponse.builder()
        .id(decision.getTransaction() != null ? decision.getTransaction().getId() : null)
        .transactionId(decision.getExternalTransactionId())
        .customerIdFromHeader(
            decision.getTransaction() != null
                ? decision.getTransaction().getCustomerIdFromHeader()
                : null)
        .merchantId(
            decision.getTransaction() != null ? decision.getTransaction().getMerchantId() : null)
        .merchantName(
            decision.getTransaction() != null ? decision.getTransaction().getMerchantName() : null)
        .transactionAmount(
            decision.getTransaction() != null
                ? decision.getTransaction().getTransactionAmount()
                : null)
        .transactionDate(
            decision.getTransaction() != null
                ? decision.getTransaction().getTransactionDate()
                : null)
        .transactionTime(
            decision.getTransaction() != null
                ? decision.getTransaction().getTransactionTime()
                : null)
        .classification(decision.getClassification().name())
        .riskScore(decision.getRiskScore())
        .triggeredRules(List.of())
        .reason(decision.getReason())
        .rulesetVersion(decision.getRulesVersion())
        .processingTimeMs(processingTime)
        .timestamp(OffsetDateTime.now(clock))
        .success(true)
        .build();
  }

  public EvaluateResponse buildEvaluateResponseFromTamperDecision(
      TransactionDecision decision, long startTime) {
    long processingTime = System.currentTimeMillis() - startTime;
    return EvaluateResponse.builder()
        .transactionId(decision.getExternalTransactionId())
        .classification(decision.getClassification().name())
        .riskScore(decision.getRiskScore())
        .reason(decision.getReason())
        .rulesetVersion(decision.getRulesVersion())
        .processingTimeMs(processingTime)
        .timestamp(LocalDateTime.now(clock))
        .ruleHits(List.of())
        .popups(List.of())
        .build();
  }

  public EvaluateResponse buildEvaluateResponse(
      Transaction transaction,
      TransactionDecision decision,
      RuleEvaluationResult result,
      long processingTime) {
    List<RuleHitDTO> ruleHits = enrichRuleHits(result.getTriggeredRules());
    List<PopupDTO> popups = aggregatePopups(ruleHits);

    return EvaluateResponse.builder()
        .transactionId(transaction.getExternalTransactionId())
        .classification(decision.getClassification().name())
        .riskScore(decision.getRiskScore())
        .reason(decision.getReason())
        .rulesetVersion(decision.getRulesVersion())
        .processingTimeMs(processingTime)
        .timestamp(
            decision.getCreatedAt() != null ? decision.getCreatedAt() : transaction.getCreatedAt())
        .ruleHits(ruleHits)
        .popups(popups)
        .build();
  }

        public TransactionResponse buildResponse(
        Transaction transaction,
        TransactionDecision decision,
        RuleEvaluationResult result,
        long processingTime) {
          return TransactionResponse.builder()
          .id(transaction.getId())
          .transactionId(transaction.getExternalTransactionId())
          .customerIdFromHeader(transaction.getCustomerIdFromHeader())
          .merchantId(transaction.getMerchantId())
          .merchantName(transaction.getMerchantName())
          .transactionAmount(transaction.getTransactionAmount())
          .transactionDate(transaction.getTransactionDate())
          .transactionTime(transaction.getTransactionTime())
          .classification(decision.getClassification().name())
          .riskScore(decision.getRiskScore())
          .triggeredRules(result.getTriggeredRules())
          .reason(decision.getReason())
          .rulesetVersion(decision.getRulesVersion())
          .processingTimeMs(processingTime)
          .timestamp(OffsetDateTime.now(clock))
          .success(true)
          .build();
        }

  public EvaluateResponse buildEvaluateResponseFromExisting(
      Transaction transaction, long startTime) {
    TransactionDecision decision =
        decisionRepository
            .findByTransactionId(transaction.getId())
            .orElseThrow(
                () -> new IllegalStateException("Transação existe sem decisão registrada"));

    long processingTime = System.currentTimeMillis() - startTime;
    List<TriggeredRuleDTO> triggeredRules = readTriggeredRules(decision.getRulesApplied());
    List<RuleHitDTO> ruleHits = enrichRuleHits(triggeredRules);
    List<PopupDTO> popups = aggregatePopups(ruleHits);

    safeLogEvaluate(
        ExecutionEventType.IDEMPOTENT_REPLAY,
        transaction.getExternalTransactionId(),
        decision.getPayloadRawHash() != null
            ? decision.getPayloadRawHash()
            : transaction.getPayloadRawHash(),
        decision,
        triggeredRules,
        null);

    return EvaluateResponse.builder()
        .transactionId(transaction.getExternalTransactionId())
        .classification(decision.getClassification().name())
        .riskScore(decision.getRiskScore())
        .reason(decision.getReason())
        .rulesetVersion(decision.getRulesVersion())
        .processingTimeMs(processingTime)
        .timestamp(
            decision.getCreatedAt() != null ? decision.getCreatedAt() : transaction.getCreatedAt())
        .ruleHits(ruleHits)
        .popups(popups)
        .build();
  }

  public TransactionResponse buildResponseFromExisting(Transaction transaction, long startTime) {
    TransactionDecision decision =
        decisionRepository
            .findByTransactionId(transaction.getId())
            .orElseThrow(
                () -> new IllegalStateException("Transação existe sem decisão registrada"));

    long processingTime = System.currentTimeMillis() - startTime;
    List<TriggeredRuleDTO> triggeredRules = readTriggeredRules(decision.getRulesApplied());

    safeLogEvaluate(
        ExecutionEventType.IDEMPOTENT_REPLAY,
        transaction.getExternalTransactionId(),
        decision.getPayloadRawHash() != null
            ? decision.getPayloadRawHash()
            : transaction.getPayloadRawHash(),
        decision,
        triggeredRules,
        null);

    return TransactionResponse.builder()
        .id(transaction.getId())
        .transactionId(transaction.getExternalTransactionId())
        .customerIdFromHeader(transaction.getCustomerIdFromHeader())
        .merchantId(transaction.getMerchantId())
        .merchantName(transaction.getMerchantName())
        .transactionAmount(transaction.getTransactionAmount())
        .transactionDate(transaction.getTransactionDate())
        .transactionTime(transaction.getTransactionTime())
        .classification(decision.getClassification().name())
        .riskScore(decision.getRiskScore())
        .triggeredRules(triggeredRules)
        .reason(decision.getReason())
        .rulesetVersion(decision.getRulesVersion())
        .processingTimeMs(processingTime)
        .timestamp(
            toOffsetDateTime(
                decision.getCreatedAt() != null
                    ? decision.getCreatedAt()
                    : transaction.getCreatedAt()))
        .success(true)
        .build();
  }

  public TransactionResponse buildResponseFromExistingDecision(
      TransactionDecision decision, long startTime) {
    Transaction transaction = decision.getTransaction();
    long processingTime = System.currentTimeMillis() - startTime;
    List<TriggeredRuleDTO> triggeredRules = readTriggeredRules(decision.getRulesApplied());

    return TransactionResponse.builder()
        .id(transaction != null ? transaction.getId() : null)
        .transactionId(transaction.getExternalTransactionId())
        .customerIdFromHeader(transaction.getCustomerIdFromHeader())
        .merchantId(transaction.getMerchantId())
        .merchantName(transaction.getMerchantName())
        .transactionAmount(transaction.getTransactionAmount())
        .transactionDate(transaction.getTransactionDate())
        .transactionTime(transaction.getTransactionTime())
        .classification(decision.getClassification().name())
        .riskScore(decision.getRiskScore())
        .triggeredRules(triggeredRules)
        .reason(decision.getReason())
        .rulesetVersion(decision.getRulesVersion())
        .processingTimeMs(processingTime)
        .timestamp(
            toOffsetDateTime(
                decision.getCreatedAt() != null
                    ? decision.getCreatedAt()
                    : transaction.getCreatedAt()))
        .success(true)
        .build();
  }

  public EvaluateResponse buildEvaluateResponseFromExistingDecision(
      TransactionDecision decision, long startTime) {
    Transaction transaction = decision.getTransaction();
    long processingTime = System.currentTimeMillis() - startTime;
    List<TriggeredRuleDTO> triggeredRules = readTriggeredRules(decision.getRulesApplied());
    List<RuleHitDTO> ruleHits = enrichRuleHits(triggeredRules);
    List<PopupDTO> popups = aggregatePopups(ruleHits);

    return EvaluateResponse.builder()
        .transactionId(transaction.getExternalTransactionId())
        .classification(decision.getClassification().name())
        .riskScore(decision.getRiskScore())
        .reason(decision.getReason())
        .rulesetVersion(decision.getRulesVersion())
        .processingTimeMs(processingTime)
        .timestamp(
            decision.getCreatedAt() != null ? decision.getCreatedAt() : transaction.getCreatedAt())
        .ruleHits(ruleHits)
        .popups(popups)
        .build();
  }

  private List<RuleHitDTO> enrichRuleHits(List<TriggeredRuleDTO> triggeredRules) {
    if (triggeredRules == null || triggeredRules.isEmpty()) {
      return List.of();
    }

    List<String> names =
        triggeredRules.stream()
            .map(TriggeredRuleDTO::getName)
            .filter(Objects::nonNull)
            .distinct()
            .toList();
    Map<String, RuleConfiguration> byName =
        ruleConfigRepository.findByRuleNameIn(names).stream()
            .collect(
                java.util.stream.Collectors.toMap(
                    RuleConfiguration::getRuleName, Function.identity(), (a, b) -> a));

    List<RuleHitDTO> hits = new ArrayList<>();
    for (TriggeredRuleDTO tr : triggeredRules) {
      RuleConfiguration rc = tr.getName() != null ? byName.get(tr.getName()) : null;
      hits.add(
          RuleHitDTO.builder()
              .ruleName(tr.getName())
              .description(rc != null ? rc.getDescription() : null)
              .ruleType(rc != null && rc.getRuleType() != null ? rc.getRuleType().name() : null)
              .classification(
                  rc != null && rc.getClassification() != null
                      ? rc.getClassification().name()
                      : null)
              .threshold(rc != null ? rc.getThreshold() : null)
              .weight(tr.getWeight())
              .contribution(tr.getContribution())
              .detail(tr.getDetail())
              .build());
    }
    return hits;
  }

  private List<PopupDTO> aggregatePopups(List<RuleHitDTO> ruleHits) {
    if (ruleHits == null || ruleHits.isEmpty()) {
      return List.of();
    }

    Map<String, List<RuleHitDTO>> grouped = new LinkedHashMap<>();
    for (RuleHitDTO hit : ruleHits) {
      String key = hit.getClassification() != null ? hit.getClassification() : "UNKNOWN";
      grouped.computeIfAbsent(key, k -> new ArrayList<>()).add(hit);
    }

    List<String> order = List.of("FRAUD", "SUSPICIOUS", "APPROVED", "UNKNOWN");
    List<PopupDTO> popups = new ArrayList<>();
    for (String key : order) {
      List<RuleHitDTO> hits = grouped.get(key);
      if (hits == null || hits.isEmpty()) continue;

      int total =
          hits.stream()
              .map(RuleHitDTO::getContribution)
              .filter(Objects::nonNull)
              .mapToInt(Integer::intValue)
              .sum();
      String title =
          switch (key) {
            case "FRAUD" -> "Bloqueio";
            case "SUSPICIOUS" -> "Suspeita";
            case "APPROVED" -> "Aprovada";
            default -> "Atenção";
          };

      String reason =
          hits.stream()
              .map(h -> h.getDetail() != null ? h.getDetail() : h.getDescription())
              .filter(s -> s != null && !s.isBlank())
              .distinct()
              .reduce((a, b) -> a + " | " + b)
              .orElse(null);

      popups.add(
          PopupDTO.builder()
              .key(key)
              .title(title)
              .classification(key)
              .totalContribution(total)
              .rules(hits)
              .reason(reason)
              .build());
    }

    for (Map.Entry<String, List<RuleHitDTO>> e : grouped.entrySet()) {
      if (order.contains(e.getKey())) continue;
      List<RuleHitDTO> hits = e.getValue();
      int total =
          hits.stream()
              .map(RuleHitDTO::getContribution)
              .filter(Objects::nonNull)
              .mapToInt(Integer::intValue)
              .sum();
      String reason =
          hits.stream()
              .map(h -> h.getDetail() != null ? h.getDetail() : h.getDescription())
              .filter(s -> s != null && !s.isBlank())
              .distinct()
              .reduce((a, b) -> a + " | " + b)
              .orElse(null);
      popups.add(
          PopupDTO.builder()
              .key(e.getKey())
              .title("Atenção")
              .classification(e.getKey())
              .totalContribution(total)
              .rules(hits)
              .reason(reason)
              .build());
    }

    return popups;
  }

  private List<TriggeredRuleDTO> readTriggeredRules(String rulesApplied) {
    if (rulesApplied == null || rulesApplied.isBlank()) {
      return List.of();
    }
    try {
      return objectMapper.readValue(
          rulesApplied,
          objectMapper
              .getTypeFactory()
              .constructCollectionType(List.class, TriggeredRuleDTO.class));
    } catch (Exception e) {
      return List.of(rulesApplied.split(",")).stream()
          .map(String::trim)
          .filter(s -> !s.isBlank())
          .map(name -> TriggeredRuleDTO.builder().name(name).weight(0).contribution(0).build())
          .toList();
    }
  }

  private OffsetDateTime toOffsetDateTime(LocalDateTime dt) {
    if (dt == null) {
      return null;
    }
    return dt.atZone(ZoneId.systemDefault()).toOffsetDateTime();
  }

  public void safeLogEvaluate(
      ExecutionEventType eventType,
      String externalTransactionId,
      String payloadHash,
      TransactionDecision decision,
      List<?> rulesFired,
      com.fasterxml.jackson.databind.JsonNode errorJson) {
    try {
      ruleExecutionLogService.logEvaluate(
          eventType,
          externalTransactionId,
          payloadHash,
          decision != null ? decision.getClassification() : null,
          decision != null ? decision.getRiskScore() : 0,
          rulesFired,
          errorJson);
    } catch (DataAccessException e) {
      log.debug("Falha ao registrar rule_execution_log (ignorado)", e);
    }
  }

  private String writeJson(Object value) {
    try {
      return objectMapper.writeValueAsString(value);
    } catch (Exception e) {
      log.error("Erro ao serializar JSON", e);
      return "{}";
    }
  }
}
