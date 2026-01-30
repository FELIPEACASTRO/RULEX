package com.rulex.core.transaction.port;

import com.rulex.entity.Transaction;
import com.rulex.entity.TransactionDecision;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

public interface TransactionQueryRepositoryPort {

  Page<Transaction> findByFiltersWithClassification(
      String customerId,
      String merchantId,
      Integer mcc,
      BigDecimal minAmount,
      BigDecimal maxAmount,
      LocalDateTime startDate,
      LocalDateTime endDate,
      TransactionDecision.TransactionClassification classification,
      Pageable pageable);

  Optional<Transaction> findById(Long id);

  Optional<Transaction> findByExternalTransactionId(String externalId);

  Optional<TransactionDecision> findDecisionByTransactionId(Long transactionId);

  List<TransactionDecision> findDecisionsByTransactionIdIn(List<Long> ids);
}
