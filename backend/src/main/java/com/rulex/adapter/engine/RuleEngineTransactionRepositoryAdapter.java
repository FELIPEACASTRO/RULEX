package com.rulex.adapter.engine;

import com.rulex.core.engine.port.RuleEngineTransactionRepositoryPort;
import com.rulex.entity.Transaction;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import com.rulex.repository.TransactionRepository;
import java.util.Optional;
import org.springframework.stereotype.Component;

@Component
public class RuleEngineTransactionRepositoryAdapter implements RuleEngineTransactionRepositoryPort {

  private final TransactionRepository transactionRepository;

  public RuleEngineTransactionRepositoryAdapter(TransactionRepository transactionRepository) {
    this.transactionRepository = transactionRepository;
  }

  @Override
  public Optional<Transaction> findByExternalTransactionId(String externalTransactionId) {
    return transactionRepository.findByExternalTransactionId(externalTransactionId);
  }

  @Override
  public Transaction save(Transaction transaction) {
    return transactionRepository.save(transaction);
  }

  @Override
  public Long countTransactionsByCustomerSince(String customerId, LocalDateTime since) {
    return transactionRepository.countTransactionsByCustomerSince(customerId, since);
  }

  @Override
  public Long countTransactionsByMerchantSince(String merchantId, LocalDateTime since) {
    return transactionRepository.countTransactionsByMerchantSince(merchantId, since);
  }

  @Override
  public Long countSince(LocalDateTime since) {
    return transactionRepository.countSince(since);
  }

  @Override
  public BigDecimal sumAmountByCustomerSince(String customerId, LocalDateTime since) {
    return transactionRepository.sumAmountByCustomerSince(customerId, since);
  }

  @Override
  public BigDecimal sumAmountByMerchantSince(String merchantId, LocalDateTime since) {
    return transactionRepository.sumAmountByMerchantSince(merchantId, since);
  }

  @Override
  public BigDecimal sumAmountSince(LocalDateTime since) {
    return transactionRepository.sumAmountSince(since);
  }
}
