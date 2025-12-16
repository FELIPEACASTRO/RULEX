package com.rulex.service;

import com.rulex.dto.TransactionResponse;
import com.rulex.entity.Transaction;
import com.rulex.entity.TransactionDecision;
import com.rulex.repository.TransactionDecisionRepository;
import com.rulex.repository.TransactionRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Serviço para consulta de transações e decisões.
 */
@Service
@RequiredArgsConstructor
@Slf4j
@Transactional(readOnly = true)
public class TransactionQueryService {

    private final TransactionRepository transactionRepository;
    private final TransactionDecisionRepository decisionRepository;

    /**
     * Busca transações com filtros.
     */
    public Page<TransactionResponse> findTransactions(
            String customerId,
            String merchantId,
            Integer mcc,
            BigDecimal minAmount,
            BigDecimal maxAmount,
            LocalDateTime startDate,
            LocalDateTime endDate,
            Pageable pageable) {
        
        Page<Transaction> transactions = transactionRepository.findByFilters(
            customerId, merchantId, mcc, minAmount, maxAmount, startDate, endDate, pageable);
        
        return transactions.map(this::convertToResponse);
    }

    /**
     * Obtém uma transação pelo ID.
     */
    public TransactionResponse getTransactionById(Long id) {
        Transaction transaction = transactionRepository.findById(id)
            .orElseThrow(() -> new RuntimeException("Transação não encontrada"));
        
        return convertToResponse(transaction);
    }

    /**
     * Obtém uma transação pelo ID externo.
     */
    public TransactionResponse getTransactionByExternalId(String externalId) {
        Transaction transaction = transactionRepository.findByExternalTransactionId(externalId)
            .orElseThrow(() -> new RuntimeException("Transação não encontrada"));
        
        return convertToResponse(transaction);
    }

    /**
     * Converte uma entidade Transaction para TransactionResponse.
     */
    private TransactionResponse convertToResponse(Transaction transaction) {
        TransactionDecision decision = decisionRepository.findByTransactionId(transaction.getId())
            .orElse(null);
        
        return TransactionResponse.builder()
            .externalTransactionId(transaction.getExternalTransactionId())
            .classification(decision != null ? decision.getClassification().name() : "UNKNOWN")
            .riskScore(decision != null ? decision.getRiskScore() : 0)
            .rulesApplied(decision != null ? 
                List.of(decision.getRulesApplied().split(", ")) : List.of())
            .reason(decision != null ? decision.getReason() : "Sem decisão registrada")
            .rulesVersion(decision != null ? decision.getRulesVersion() : "1.0.0")
            .timestamp(transaction.getCreatedAt())
            .success(true)
            .build();
    }

}
