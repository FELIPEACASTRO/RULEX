package com.rulex.service;

import com.rulex.api.NotFoundException;
import com.rulex.core.transaction.port.TransactionQueryInputPort;
import com.rulex.core.transaction.port.TransactionQueryRepositoryPort;
import com.rulex.core.transaction.port.TriggeredRuleParserPort;
import com.rulex.core.transaction.usecase.TransactionQueryUseCase;
import com.rulex.dto.TransactionResponse;
import java.io.IOException;
import java.io.Writer;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/** Adapter Spring para consulta de transações e decisões. */
@Service
@Transactional(readOnly = true)
public class TransactionQueryService implements TransactionQueryInputPort {

  private final TransactionQueryUseCase useCase;

  public TransactionQueryService(
      TransactionQueryRepositoryPort repositoryPort, TriggeredRuleParserPort triggeredRuleParser) {
    this.useCase = new TransactionQueryUseCase(repositoryPort, triggeredRuleParser);
  }

  @Override
  public Page<TransactionResponse> findTransactions(
      String customerId,
      String merchantId,
      String classification,
      Integer mcc,
      BigDecimal minAmount,
      BigDecimal maxAmount,
      LocalDateTime startDate,
      LocalDateTime endDate,
      Pageable pageable) {
    return useCase.findTransactions(
        customerId,
        merchantId,
        classification,
        mcc,
        minAmount,
        maxAmount,
        startDate,
        endDate,
        pageable);
  }

  @Override
  public Optional<TransactionResponse> getTransactionById(Long id) {
    return useCase.getTransactionById(id);
  }

  @Override
  public Optional<TransactionResponse> getTransactionByExternalId(String externalId) {
    return useCase.getTransactionByExternalId(externalId);
  }

  public TransactionResponse getTransactionByIdOrThrow(Long id) {
    return getTransactionById(id)
        .orElseThrow(() -> new NotFoundException("Transação não encontrada"));
  }

  public TransactionResponse getTransactionByExternalIdOrThrow(String externalId) {
    return getTransactionByExternalId(externalId)
        .orElseThrow(() -> new NotFoundException("Transação não encontrada"));
  }

  @Override
  public List<TransactionResponse> exportAsList(
      String customerId,
      String merchantId,
      String classification,
      Integer mcc,
      BigDecimal minAmount,
      BigDecimal maxAmount,
      LocalDateTime startDate,
      LocalDateTime endDate,
      Pageable firstPage,
      int limit) {
    return useCase.exportAsList(
        customerId,
        merchantId,
        classification,
        mcc,
        minAmount,
        maxAmount,
        startDate,
        endDate,
        firstPage,
        limit);
  }

  @Override
  public void exportAsCsv(
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
      throws IOException {
    useCase.exportAsCsv(
        w,
        customerId,
        merchantId,
        classification,
        mcc,
        minAmount,
        maxAmount,
        startDate,
        endDate,
        firstPage,
        limit);
  }
}

