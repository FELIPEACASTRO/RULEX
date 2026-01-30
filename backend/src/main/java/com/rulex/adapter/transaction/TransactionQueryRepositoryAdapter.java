package com.rulex.adapter.transaction;

import com.rulex.core.transaction.port.TransactionQueryRepositoryPort;
import com.rulex.entity.Transaction;
import com.rulex.entity.TransactionDecision;
import com.rulex.repository.TransactionDecisionRepository;
import com.rulex.repository.TransactionRepository;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Component;

@Component
public class TransactionQueryRepositoryAdapter implements TransactionQueryRepositoryPort {

  private final TransactionRepository transactionRepository;
  private final TransactionDecisionRepository decisionRepository;

  public TransactionQueryRepositoryAdapter(
      TransactionRepository transactionRepository, TransactionDecisionRepository decisionRepository) {
    this.transactionRepository = transactionRepository;
    this.decisionRepository = decisionRepository;
  }

  @Override
  public Page<Transaction> findByFiltersWithClassification(
      String customerId,
      String merchantId,
      Integer mcc,
      BigDecimal minAmount,
      BigDecimal maxAmount,
      LocalDateTime startDate,
      LocalDateTime endDate,
      TransactionDecision.TransactionClassification classification,
      Pageable pageable) {
    return transactionRepository.findByFiltersWithClassification(
        customerId,
        merchantId,
        mcc,
        minAmount,
        maxAmount,
        startDate,
        endDate,
        classification,
        pageable);
  }

  @Override
  public Optional<Transaction> findById(Long id) {
    return transactionRepository.findById(id);
  }

  @Override
  public Optional<Transaction> findByExternalTransactionId(String externalId) {
    return transactionRepository.findByExternalTransactionId(externalId);
  }

  @Override
  public Optional<TransactionDecision> findDecisionByTransactionId(Long transactionId) {
    return decisionRepository.findByTransactionId(transactionId);
  }

  @Override
  public List<TransactionDecision> findDecisionsByTransactionIdIn(List<Long> ids) {
    return decisionRepository.findByTransactionIdIn(ids);
  }
}
