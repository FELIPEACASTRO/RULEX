package com.rulex.service;

import com.rulex.entity.TransactionRawStore;
import com.rulex.repository.TransactionRawStoreRepository;
import java.time.Clock;
import java.time.LocalDateTime;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class TransactionRawStoreService {

  private final TransactionRawStoreRepository repository;
  private final Clock clock;

  @Transactional(propagation = Propagation.REQUIRES_NEW)
  public void store(
      @NonNull String externalTransactionId,
      @NonNull String payloadRawHash,
      @NonNull byte[] payloadRawBytes,
      String contentType) {
    TransactionRawStore row =
        TransactionRawStore.builder()
            .externalTransactionId(externalTransactionId)
            .payloadRawHash(payloadRawHash)
            .payloadRawBytes(payloadRawBytes)
            .contentType(contentType)
            .createdAt(LocalDateTime.now(clock))
            .build();
    Objects.requireNonNull(repository.save(row));
  }

  @Transactional(readOnly = true)
  public java.util.Optional<TransactionRawStore> findByExternalTransactionId(
      @NonNull String externalTransactionId) {
    return repository.findById(externalTransactionId);
  }
}
