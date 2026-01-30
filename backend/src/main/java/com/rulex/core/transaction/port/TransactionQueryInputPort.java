package com.rulex.core.transaction.port;

import com.rulex.dto.TransactionResponse;
import java.io.IOException;
import java.io.Writer;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

public interface TransactionQueryInputPort {

  Page<TransactionResponse> findTransactions(
      String customerId,
      String merchantId,
      String classification,
      Integer mcc,
      BigDecimal minAmount,
      BigDecimal maxAmount,
      LocalDateTime startDate,
      LocalDateTime endDate,
      Pageable pageable);

  Optional<TransactionResponse> getTransactionById(Long id);

  Optional<TransactionResponse> getTransactionByExternalId(String externalId);

  List<TransactionResponse> exportAsList(
      String customerId,
      String merchantId,
      String classification,
      Integer mcc,
      BigDecimal minAmount,
      BigDecimal maxAmount,
      LocalDateTime startDate,
      LocalDateTime endDate,
      Pageable firstPage,
      int limit);

  void exportAsCsv(
      Writer w,
      String customerId,
      String merchantId,
      String classification,
      Integer mcc,
      BigDecimal minAmount,
      BigDecimal maxAmount,
      LocalDateTime startDate,
      LocalDateTime endDate,
      Pageable firstPage,
      int limit)
      throws IOException;
}
