package com.rulex.service;

import com.rulex.entity.TransactionRawStore;
import com.rulex.repository.TransactionRawStoreRepository;
import java.time.Clock;
import java.time.LocalDateTime;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class TransactionRawStoreService {

  private final TransactionRawStoreRepository repository;
  private final Clock clock;

  @Transactional(propagation = Propagation.REQUIRES_NEW)
  public void store(String externalTransactionId, String payloadRawHash, String payloadRawJson) {
    TransactionRawStore row =
        TransactionRawStore.builder()
            .externalTransactionId(externalTransactionId)
            .payloadRawHash(payloadRawHash)
            .payloadRawJson(payloadRawJson)
            .createdAt(LocalDateTime.now(clock))
            .build();
    repository.save(row);
  }
}
