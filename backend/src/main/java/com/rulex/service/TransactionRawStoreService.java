package com.rulex.service;

import com.rulex.entity.TransactionRawStore;
import com.rulex.repository.TransactionRawStoreRepository;
import java.time.Clock;
import java.time.LocalDateTime;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.lang.NonNull;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

/**
 * Serviço de armazenamento de payload raw para anti-tamper e auditoria.
 *
 * <p>IMPORTANTE: O método store() é assíncrono (@Async) para não bloquear o processamento de
 * transações. Utiliza pool dedicado "auditExecutor" (2-4 threads, queue=200).
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class TransactionRawStoreService {

  private final TransactionRawStoreRepository repository;
  private final Clock clock;

  /**
   * Armazena o payload raw da transação de forma assíncrona. Executa em thread separada para não
   * bloquear o processamento principal.
   */
  @Async("auditExecutor")
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
    log.debug(
        "Raw payload stored for transaction: {}, hash: {}",
        externalTransactionId,
        payloadRawHash);
  }

  @Transactional(readOnly = true)
  public java.util.Optional<TransactionRawStore> findByExternalTransactionId(
      @NonNull String externalTransactionId) {
    return repository.findById(externalTransactionId);
  }
}
