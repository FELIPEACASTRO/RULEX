package com.rulex.core.engine.port;

import com.rulex.entity.Transaction;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Optional;

public interface RuleEngineTransactionRepositoryPort {

  Optional<Transaction> findByExternalTransactionId(String externalTransactionId);

  Transaction save(Transaction transaction);

  Long countTransactionsByCustomerSince(String customerId, LocalDateTime since);

  Long countTransactionsByMerchantSince(String merchantId, LocalDateTime since);

  Long countSince(LocalDateTime since);

  BigDecimal sumAmountByCustomerSince(String customerId, LocalDateTime since);

  BigDecimal sumAmountByMerchantSince(String merchantId, LocalDateTime since);

  BigDecimal sumAmountSince(LocalDateTime since);
}
