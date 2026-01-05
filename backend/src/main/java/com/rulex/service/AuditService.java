package com.rulex.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.dto.TransactionRequest;
import com.rulex.entity.AuditLog;
import com.rulex.entity.Transaction;
import com.rulex.entity.TransactionDecision;
import com.rulex.repository.AuditLogRepository;
import com.rulex.service.RuleEngineService.RuleEvaluationResult;
import java.time.Clock;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

/**
 * Serviço de auditoria que registra todas as ações para compliance.
 *
 * <p>IMPORTANTE: Todos os métodos são assíncronos (@Async) para não bloquear o processamento de
 * transações. Utiliza pool dedicado "auditExecutor" (2-4 threads, queue=200).
 */
@Service
@RequiredArgsConstructor
@Slf4j
@SuppressWarnings("null")
public class AuditService {

  private final AuditLogRepository auditLogRepository;
  private final ObjectMapper objectMapper;
  private final Clock clock;

  /** Registra o processamento de uma transação. */
  @Async("auditExecutor")
  public void logTransactionProcessed(
      Transaction transaction, TransactionDecision decision, RuleEvaluationResult result) {
    try {
      Map<String, Object> details = new HashMap<>();
      details.put("transactionId", transaction.getExternalTransactionId());
      details.put("customerId", transaction.getCustomerIdFromHeader());
      details.put("merchantId", transaction.getMerchantId());
      details.put("amount", transaction.getTransactionAmount());
      details.put("classification", decision.getClassification().name());
      details.put("riskScore", decision.getRiskScore());
      details.put("rulesApplied", extractAppliedRuleNames(result));

      AuditLog auditLog =
          AuditLog.builder()
              .transactionId(transaction.getId())
              .actionType(AuditLog.AuditActionType.TRANSACTION_PROCESSED)
              .description(
                  String.format(
                      "Transação %s processada com classificação %s",
                      transaction.getExternalTransactionId(),
                      decision.getClassification().getLabel()))
              .details(objectMapper.writeValueAsString(details))
              .result(AuditLog.AuditResult.SUCCESS)
              .createdAt(LocalDateTime.now(clock))
              .build();

      auditLogRepository.save(auditLog);
      log.info("Auditoria registrada para transação: {}", transaction.getExternalTransactionId());
    } catch (Exception e) {
      log.error("Erro ao registrar auditoria de transação", e);
    }
  }

  private Object extractAppliedRuleNames(RuleEvaluationResult result) {
    if (result == null
        || result.getTriggeredRules() == null
        || result.getTriggeredRules().isEmpty()) {
      return java.util.List.of();
    }

    java.util.List<String> names = new java.util.ArrayList<>(result.getTriggeredRules().size());
    for (var rule : result.getTriggeredRules()) {
      if (rule != null && rule.getName() != null && !rule.getName().isBlank()) {
        names.add(rule.getName());
      }
    }
    return names;
  }

  /** Registra um erro no processamento. */
  @Async("auditExecutor")
  public void logError(String transactionId, Exception exception) {
    try {
      AuditLog auditLog =
          AuditLog.builder()
              .actionType(AuditLog.AuditActionType.TRANSACTION_PROCESSED)
              .description(String.format("Erro ao processar transação %s", transactionId))
              .result(AuditLog.AuditResult.FAILURE)
              .errorMessage(exception.getMessage())
              .createdAt(LocalDateTime.now(clock))
              .build();

      auditLogRepository.save(auditLog);
    } catch (Exception e) {
      log.error("Erro ao registrar auditoria de erro", e);
    }
  }

  /** Registra a criação de uma regra. */
  @Async("auditExecutor")
  public void logRuleCreated(String ruleName, String performedBy) {
    try {
      AuditLog auditLog =
          AuditLog.builder()
              .actionType(AuditLog.AuditActionType.RULE_CREATED)
              .description(String.format("Regra '%s' criada", ruleName))
              .performedBy(performedBy)
              .result(AuditLog.AuditResult.SUCCESS)
              .createdAt(LocalDateTime.now(clock))
              .build();

      auditLogRepository.save(auditLog);
    } catch (Exception e) {
      log.error("Erro ao registrar auditoria de criação de regra", e);
    }
  }

  /** Registra a atualização de uma regra. */
  @Async("auditExecutor")
  public void logRuleUpdated(String ruleName, Map<String, Object> changes, String performedBy) {
    try {
      AuditLog auditLog =
          AuditLog.builder()
              .actionType(AuditLog.AuditActionType.RULE_UPDATED)
              .description(String.format("Regra '%s' atualizada", ruleName))
              .details(objectMapper.writeValueAsString(changes))
              .performedBy(performedBy)
              .result(AuditLog.AuditResult.SUCCESS)
              .createdAt(LocalDateTime.now(clock))
              .build();

      auditLogRepository.save(auditLog);
    } catch (Exception e) {
      log.error("Erro ao registrar auditoria de atualização de regra", e);
    }
  }

  /** Registra a deleção de uma regra. */
  @Async("auditExecutor")
  public void logRuleDeleted(String ruleName, String performedBy) {
    try {
      AuditLog auditLog =
          AuditLog.builder()
              .actionType(AuditLog.AuditActionType.RULE_DELETED)
              .description(String.format("Regra '%s' deletada", ruleName))
              .performedBy(performedBy)
              .result(AuditLog.AuditResult.SUCCESS)
              .createdAt(LocalDateTime.now(clock))
              .build();

      auditLogRepository.save(auditLog);
    } catch (Exception e) {
      log.error("Erro ao registrar auditoria de deleção de regra", e);
    }
  }

  /** Registra a execução de uma regra específica (motor avançado). */
  @Async("auditExecutor")
  public void logRule(String ruleName, TransactionRequest transaction, String outcome) {
    try {
      Map<String, Object> details = new HashMap<>();
      details.put("ruleName", ruleName);
      details.put("externalTransactionId", transaction.getExternalTransactionId());
      details.put("outcome", outcome);

      AuditLog auditLog =
          AuditLog.builder()
              .actionType(AuditLog.AuditActionType.DECISION_MADE)
              .description(
                  String.format("Regra '%s' executada com resultado %s", ruleName, outcome))
              .details(objectMapper.writeValueAsString(details))
              .performedBy("SYSTEM")
              .result(AuditLog.AuditResult.SUCCESS)
              .createdAt(LocalDateTime.now(clock))
              .build();

      auditLogRepository.save(auditLog);
    } catch (Exception e) {
      log.error("Erro ao registrar auditoria da regra", e);
    }
  }

  /**
   * V3.1: append-only audit for anti-tamper (externalTransactionId reused with different raw hash).
   */
  @Async("auditExecutor")
  public void logTamperAttempt(String externalTransactionId, String oldHash, String newHash) {
    try {
      Map<String, Object> details = new HashMap<>();
      details.put("externalTransactionId", externalTransactionId);
      details.put("oldHash", oldHash);
      details.put("newHash", newHash);
      details.put("severity", "CRITICAL");

      AuditLog auditLog =
          AuditLog.builder()
              .actionType(AuditLog.AuditActionType.DECISION_MADE)
              .description(
                  String.format(
                      "ANTI_TAMPER_VIOLATION: externalTransactionId %s reutilizado com payload diferente",
                      externalTransactionId))
              .details(objectMapper.writeValueAsString(details))
              .performedBy("SYSTEM")
              .result(AuditLog.AuditResult.SUCCESS)
              .createdAt(LocalDateTime.now(clock))
              .build();

      auditLogRepository.save(auditLog);
    } catch (Exception e) {
      log.error("Erro ao registrar auditoria de anti-tamper", e);
    }
  }
}
