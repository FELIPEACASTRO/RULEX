package com.rulex.infrastructure.persistence;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.application.port.out.TransactionPersistencePort;
import com.rulex.domain.model.TransactionData;
import com.rulex.entity.Transaction;
import com.rulex.entity.TransactionRawStore;
import com.rulex.repository.TransactionRawStoreRepository;
import com.rulex.repository.TransactionRepository;
import java.nio.charset.StandardCharsets;
import java.time.Clock;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Adapter JPA para persistência/consulta mínima de transações.
 *
 * <p>Para o fluxo hexagonal atual, este adapter prioriza o armazenamento de hash do payload
 * (anti-tamper) em {@code transaction_raw_store}. Quando disponível, também faz fallback para a
 * tabela {@code transactions}.
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class TransactionPersistenceAdapter implements TransactionPersistencePort {

  private final TransactionRawStoreRepository rawStoreRepository;
  private final TransactionRepository transactionRepository;
  private final ObjectMapper objectMapper;
  private final Clock clock;

  @Override
  public Optional<TransactionData> findByExternalId(String externalTransactionId) {
    Optional<Transaction> tx = transactionRepository.findByExternalTransactionId(externalTransactionId);
    if (tx.isPresent()) {
      return Optional.of(toDomain(tx.get()));
    }

    return rawStoreRepository
        .findById(externalTransactionId)
        .map(
            row ->
                TransactionData.builder()
                    .transactionId(externalTransactionId)
                    .externalTransactionId(externalTransactionId)
                    .payloadRawHash(row.getPayloadRawHash())
                    .data(Map.of())
                    .build());
  }

  @Override
  public Optional<String> findPayloadHashByExternalId(String externalTransactionId) {
    Optional<String> fromRaw =
        rawStoreRepository.findById(externalTransactionId).map(TransactionRawStore::getPayloadRawHash);
    if (fromRaw.isPresent()) {
      return fromRaw;
    }

    return transactionRepository
        .findByExternalTransactionId(externalTransactionId)
        .map(Transaction::getPayloadRawHash);
  }

  @Override
  public TransactionData save(TransactionData transaction, String payloadHash) {
    String externalId = resolveExternalId(transaction);

    Optional<TransactionRawStore> existing = rawStoreRepository.findById(externalId);
    if (existing.isPresent()) {
      // Não sobrescreve hash existente; o anti-tamper deve tratar divergências antes.
      return transaction.withPayloadRawHash(existing.get().getPayloadRawHash());
    }

    TransactionRawStore row =
        TransactionRawStore.builder()
            .externalTransactionId(externalId)
            .payloadRawHash(payloadHash)
            .payloadRawBytes(serializePayloadBytes(transaction.getData(), payloadHash))
            .contentType("application/json")
            .createdAt(LocalDateTime.now(clock))
            .build();

    Objects.requireNonNull(rawStoreRepository.save(row));
    return transaction.withPayloadRawHash(payloadHash);
  }

  @Override
  public boolean existsByExternalId(String externalTransactionId) {
    return rawStoreRepository.existsById(externalTransactionId)
        || transactionRepository.findByExternalTransactionId(externalTransactionId).isPresent();
  }

  private String resolveExternalId(TransactionData transaction) {
    if (transaction == null) {
      throw new IllegalArgumentException("TransactionData is null");
    }
    if (transaction.getTransactionId() != null && !transaction.getTransactionId().isBlank()) {
      return transaction.getTransactionId();
    }
    if (transaction.getExternalTransactionId() != null && !transaction.getExternalTransactionId().isBlank()) {
      return transaction.getExternalTransactionId();
    }
    throw new IllegalArgumentException("externalTransactionId is required");
  }

  private byte[] serializePayloadBytes(Map<String, Object> payload, String fallback) {
    try {
      if (payload == null) {
        return "{}".getBytes(StandardCharsets.UTF_8);
      }
      return objectMapper.writeValueAsBytes(payload);
    } catch (JsonProcessingException e) {
      log.warn("Erro ao serializar payload para raw store: {}", e.getMessage());
      return (fallback == null ? "" : fallback).getBytes(StandardCharsets.UTF_8);
    }
  }

  private TransactionData toDomain(Transaction entity) {
    Instant timestamp = null;
    if (entity.getCreatedAt() != null) {
      timestamp = entity.getCreatedAt().toInstant(ZoneOffset.UTC);
    }

    return TransactionData.builder()
        .id(entity.getId())
        .transactionId(entity.getExternalTransactionId())
        .externalTransactionId(entity.getExternalTransactionId())
        .payloadRawHash(entity.getPayloadRawHash())
        .customerId(entity.getCustomerIdFromHeader())
        .customerIdFromHeader(entity.getCustomerIdFromHeader())
        .customerAcctNumber(entity.getCustomerAcctNumber())
        .pan(entity.getPan())
        .merchantId(entity.getMerchantId())
        .merchantName(entity.getMerchantName())
        .amount(entity.getTransactionAmount())
        .transactionAmount(entity.getTransactionAmount())
        .transactionDate(entity.getTransactionDate())
        .transactionTime(entity.getTransactionTime())
        .gmtOffset(entity.getGmtOffset())
        .transactionCurrencyCode(entity.getTransactionCurrencyCode())
        .transactionCurrencyConversionRate(entity.getTransactionCurrencyConversionRate())
        .merchantCountryCode(entity.getMerchantCountryCode())
        .merchantCity(entity.getMerchantCity())
        .merchantState(entity.getMerchantState())
        .merchantPostalCode(entity.getMerchantPostalCode())
        .mcc(entity.getMcc())
        .posEntryMode(entity.getPosEntryMode())
        .customerPresent(entity.getCustomerPresent())
        .consumerAuthenticationScore(entity.getConsumerAuthenticationScore())
        .externalScore3(entity.getExternalScore3())
        .cavvResult(entity.getCavvResult())
        .cryptogramValid(entity.getCryptogramValid())
        .cvv2Response(entity.getCvv2Response())
        .cvv2Present(entity.getCvv2Present())
        .pinVerifyCode(entity.getPinVerifyCode())
        .cvvVerifyCode(entity.getCvvVerifyCode())
        .eciIndicator(entity.getEciIndicator())
        .atcCard(entity.getAtcCard())
        .atcHost(entity.getAtcHost())
        .tokenAssuranceLevel(entity.getTokenAssuranceLevel())
        .tokenizationIndicator(entity.getTokenizationIndicator())
        .availableCredit(entity.getAvailableCredit())
        .cardCashBalance(entity.getCardCashBalance())
        .cardDelinquentAmount(entity.getCardDelinquentAmount())
        .workflow(entity.getWorkflow())
        .recordType(entity.getRecordType())
        .clientIdFromHeader(entity.getClientIdFromHeader())
        .createdAt(entity.getCreatedAt())
        .updatedAt(entity.getUpdatedAt())
        .timestamp(timestamp)
        .data(Map.of())
        .build();
  }
}
