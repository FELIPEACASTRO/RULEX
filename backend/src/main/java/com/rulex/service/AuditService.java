package com.rulex.service;

import com.rulex.entity.AuditLog;
import com.rulex.entity.Transaction;
import com.rulex.entity.TransactionDecision;
import com.rulex.repository.AuditLogRepository;
import com.rulex.service.RuleEngineService.RuleEvaluationResult;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.HashMap;
import java.util.Map;

/**
 * Serviço de auditoria que registra todas as ações para compliance.
 */
@Service
@RequiredArgsConstructor
@Slf4j
@Transactional
public class AuditService {

    private final AuditLogRepository auditLogRepository;
    private final ObjectMapper objectMapper;

    /**
     * Registra o processamento de uma transação.
     */
    public void logTransactionProcessed(Transaction transaction, TransactionDecision decision, 
                                       RuleEvaluationResult result) {
        try {
            Map<String, Object> details = new HashMap<>();
            details.put("transactionId", transaction.getExternalTransactionId());
            details.put("customerId", transaction.getCustomerIdFromHeader());
            details.put("merchantId", transaction.getMerchantId());
            details.put("amount", transaction.getTransactionAmount());
            details.put("classification", decision.getClassification().name());
            details.put("riskScore", decision.getRiskScore());
            details.put("rulesApplied", result.getAppliedRules());

            AuditLog log = AuditLog.builder()
                .transactionId(transaction.getId())
                .actionType(AuditLog.AuditActionType.TRANSACTION_PROCESSED)
                .description(String.format("Transação %s processada com classificação %s", 
                    transaction.getExternalTransactionId(), decision.getClassification().getLabel()))
                .details(objectMapper.writeValueAsString(details))
                .result(AuditLog.AuditResult.SUCCESS)
                .build();

            auditLogRepository.save(log);
            log.info("Auditoria registrada para transação: {}", transaction.getExternalTransactionId());
        } catch (Exception e) {
            log.error("Erro ao registrar auditoria de transação", e);
        }
    }

    /**
     * Registra um erro no processamento.
     */
    public void logError(String transactionId, Exception exception) {
        try {
            AuditLog log = AuditLog.builder()
                .actionType(AuditLog.AuditActionType.TRANSACTION_PROCESSED)
                .description(String.format("Erro ao processar transação %s", transactionId))
                .result(AuditLog.AuditResult.FAILURE)
                .errorMessage(exception.getMessage())
                .build();

            auditLogRepository.save(log);
        } catch (Exception e) {
            log.error("Erro ao registrar auditoria de erro", e);
        }
    }

    /**
     * Registra a criação de uma regra.
     */
    public void logRuleCreated(String ruleName, String performedBy) {
        try {
            AuditLog log = AuditLog.builder()
                .actionType(AuditLog.AuditActionType.RULE_CREATED)
                .description(String.format("Regra '%s' criada", ruleName))
                .performedBy(performedBy)
                .result(AuditLog.AuditResult.SUCCESS)
                .build();

            auditLogRepository.save(log);
        } catch (Exception e) {
            log.error("Erro ao registrar auditoria de criação de regra", e);
        }
    }

    /**
     * Registra a atualização de uma regra.
     */
    public void logRuleUpdated(String ruleName, Map<String, Object> changes, String performedBy) {
        try {
            AuditLog log = AuditLog.builder()
                .actionType(AuditLog.AuditActionType.RULE_UPDATED)
                .description(String.format("Regra '%s' atualizada", ruleName))
                .details(objectMapper.writeValueAsString(changes))
                .performedBy(performedBy)
                .result(AuditLog.AuditResult.SUCCESS)
                .build();

            auditLogRepository.save(log);
        } catch (Exception e) {
            log.error("Erro ao registrar auditoria de atualização de regra", e);
        }
    }

    /**
     * Registra a deleção de uma regra.
     */
    public void logRuleDeleted(String ruleName, String performedBy) {
        try {
            AuditLog log = AuditLog.builder()
                .actionType(AuditLog.AuditActionType.RULE_DELETED)
                .description(String.format("Regra '%s' deletada", ruleName))
                .performedBy(performedBy)
                .result(AuditLog.AuditResult.SUCCESS)
                .build();

            auditLogRepository.save(log);
        } catch (Exception e) {
            log.error("Erro ao registrar auditoria de deleção de regra", e);
        }
    }

}
