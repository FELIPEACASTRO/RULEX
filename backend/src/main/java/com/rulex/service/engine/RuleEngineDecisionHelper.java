package com.rulex.service.engine;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.entity.Transaction;
import com.rulex.entity.TransactionDecision;
import com.rulex.repository.TransactionRepository;
import com.rulex.service.RuleEvaluationResult;
import java.time.Clock;
import java.time.LocalDateTime;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
@Slf4j
public class RuleEngineDecisionHelper {

  private final TransactionRepository transactionRepository;
  private final ObjectMapper objectMapper;
  private final Clock clock;

  public TransactionDecision buildTamperDecision(
      String externalTransactionId, String newPayloadHash) {
    return TransactionDecision.builder()
        .transaction(
            transactionRepository
                .findByExternalTransactionId(externalTransactionId)
                .orElseThrow(
                    () ->
                        new IllegalStateException(
                            "Tamper detected but transaction row is missing for externalTransactionId="
                                + externalTransactionId)))
        .externalTransactionId(externalTransactionId)
        .payloadRawHash(newPayloadHash)
        .classification(TransactionDecision.TransactionClassification.FRAUD)
        .riskScore(100)
        .rulesApplied(writeJson(java.util.List.of()))
        .scoreDetails(writeJson(java.util.Map.of()))
        .reason("ANTI_TAMPER: externalTransactionId reutilizado com payload diferente")
        .rulesVersion("ANTI_TAMPER_V1")
        .createdAt(LocalDateTime.now(clock))
        .build();
  }

  public TransactionDecision createDecision(Transaction transaction, RuleEvaluationResult result) {
    return TransactionDecision.builder()
        .transaction(transaction)
        .externalTransactionId(transaction.getExternalTransactionId())
        .payloadRawHash(transaction.getPayloadRawHash())
        .classification(result.getClassification())
        .riskScore(result.getRiskScore())
        .rulesApplied(writeJson(result.getTriggeredRules()))
        .scoreDetails(writeJson(result.getScoreDetails()))
        .reason(result.getReason())
        .rulesVersion("1")
        .createdAt(LocalDateTime.now(clock))
        .build();
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
