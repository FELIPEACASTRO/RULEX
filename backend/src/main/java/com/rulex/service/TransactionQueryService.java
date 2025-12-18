package com.rulex.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.dto.TransactionResponse;
import com.rulex.dto.TriggeredRuleDTO;
import com.rulex.entity.Transaction;
import com.rulex.entity.TransactionDecision;
import com.rulex.repository.TransactionDecisionRepository;
import com.rulex.repository.TransactionRepository;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/** Serviço para consulta de transações e decisões. */
@Service
@RequiredArgsConstructor
@Slf4j
@Transactional(readOnly = true)
public class TransactionQueryService {

  private final TransactionRepository transactionRepository;
  private final TransactionDecisionRepository decisionRepository;
  private final ObjectMapper objectMapper;

  /** Busca transações com filtros. */
  public Page<TransactionResponse> findTransactions(
      String customerId,
      String merchantId,
      Integer mcc,
      BigDecimal minAmount,
      BigDecimal maxAmount,
      LocalDateTime startDate,
      LocalDateTime endDate,
      Pageable pageable) {

    Page<Transaction> transactions =
        transactionRepository.findByFilters(
            customerId, merchantId, mcc, minAmount, maxAmount, startDate, endDate, pageable);

    return transactions.map(this::convertToResponse);
  }

  /** Obtém uma transação pelo ID. */
  public TransactionResponse getTransactionById(Long id) {
    Transaction transaction =
        transactionRepository
            .findById(id)
            .orElseThrow(() -> new RuntimeException("Transação não encontrada"));

    return convertToResponse(transaction);
  }

  /** Obtém uma transação pelo ID externo. */
  public TransactionResponse getTransactionByExternalId(String externalId) {
    Transaction transaction =
        transactionRepository
            .findByExternalTransactionId(externalId)
            .orElseThrow(() -> new RuntimeException("Transação não encontrada"));

    return convertToResponse(transaction);
  }

  /** Converte uma entidade Transaction para TransactionResponse. */
  private TransactionResponse convertToResponse(Transaction transaction) {
    TransactionDecision decision =
        decisionRepository.findByTransactionId(transaction.getId()).orElse(null);

    return TransactionResponse.builder()
        .transactionId(transaction.getExternalTransactionId())
        .classification(decision != null ? decision.getClassification().name() : "UNKNOWN")
        .riskScore(decision != null ? decision.getRiskScore() : 0)
        .triggeredRules(
            decision != null ? readTriggeredRules(decision.getRulesApplied()) : List.of())
        .reason(decision != null ? decision.getReason() : "Sem decisão registrada")
        .rulesetVersion(decision != null ? decision.getRulesVersion() : "1")
        .processingTimeMs(0L)
        .timestamp(decision != null ? decision.getCreatedAt() : transaction.getCreatedAt())
        .success(true)
        .build();
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
      // fallback legado: string CSV
      return List.of(rulesApplied.split(",")).stream()
          .map(String::trim)
          .filter(s -> !s.isBlank())
          .map(name -> TriggeredRuleDTO.builder().name(name).weight(0).contribution(0).build())
          .toList();
    }
  }
}
