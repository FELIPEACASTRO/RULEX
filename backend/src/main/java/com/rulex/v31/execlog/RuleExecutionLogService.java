package com.rulex.v31.execlog;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.rulex.entity.TransactionDecision;
import com.rulex.entity.homolog.DecisionOutcome;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.slf4j.MDC;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class RuleExecutionLogService {

  private final RuleExecutionLogRepository repository;
  private final ObjectMapper objectMapper;

  @Transactional(propagation = Propagation.REQUIRES_NEW)
  public void logEvaluate(
      ExecutionEventType eventType,
      String externalTransactionId,
      String payloadRawHash,
      TransactionDecision.TransactionClassification classification,
      int riskScore,
      List<?> rulesFired,
      JsonNode errorJson) {

    DecisionOutcome decision = toDecisionOutcome(classification);

    ArrayNode rulesFiredJson = objectMapper.createArrayNode();
    if (rulesFired != null) {
      for (Object o : rulesFired) {
        rulesFiredJson.add(objectMapper.valueToTree(o));
      }
    }

    ObjectNode decisionPathJson = objectMapper.createObjectNode();
    decisionPathJson.put("classification", classification != null ? classification.name() : null);
    decisionPathJson.put("riskScore", riskScore);

    String correlationId = MDC.get("correlationId");

    RuleExecutionLogEntity row =
        RuleExecutionLogEntity.builder()
            .eventType(eventType)
            .correlationId(correlationId)
            .externalTransactionId(externalTransactionId)
            .payloadRawHash(payloadRawHash)
            .attemptedPayloadHash(null)
            .tamper(false)
            .decision(decision)
            .riskScore(riskScore)
            .rulesFiredJson(rulesFiredJson)
            .decisionPathJson(decisionPathJson)
            .whyNotFiredJson(null)
            .contextFlagsJson(null)
            .errorJson(errorJson)
            .build();

    try {
      repository.save(row);
    } catch (DataIntegrityViolationException ignored) {
      // Dedupe index: ignore identical repeats.
    }
  }

  @Transactional(propagation = Propagation.REQUIRES_NEW)
  public void logAntiTamper(
      String externalTransactionId,
      String storedPayloadHash,
      String attemptedPayloadHash,
      JsonNode errorJson) {

    String correlationId = MDC.get("correlationId");

    ObjectNode decisionPathJson = objectMapper.createObjectNode();
    decisionPathJson.put("reason", "ANTI_TAMPER");

    RuleExecutionLogEntity row =
        RuleExecutionLogEntity.builder()
            .eventType(ExecutionEventType.ANTI_TAMPER)
            .correlationId(correlationId)
            .externalTransactionId(externalTransactionId)
            .payloadRawHash(storedPayloadHash)
            .attemptedPayloadHash(attemptedPayloadHash)
            .tamper(true)
            .decision(DecisionOutcome.FRAUDE)
            .riskScore(100)
            .rulesFiredJson(objectMapper.createArrayNode())
            .decisionPathJson(decisionPathJson)
            .whyNotFiredJson(null)
            .contextFlagsJson(null)
            .errorJson(errorJson)
            .build();

    try {
      repository.save(row);
    } catch (DataIntegrityViolationException ignored) {
      // Dedupe index: ignore identical repeats.
    }
  }

  private DecisionOutcome toDecisionOutcome(
      TransactionDecision.TransactionClassification classification) {
    if (classification == null) {
      return DecisionOutcome.SUSPEITA_DE_FRAUDE;
    }
    return switch (classification) {
      case APPROVED -> DecisionOutcome.APROVADO;
      case SUSPICIOUS -> DecisionOutcome.SUSPEITA_DE_FRAUDE;
      case FRAUD -> DecisionOutcome.FRAUDE;
    };
  }
}
