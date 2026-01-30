package com.rulex.adapter.engine;

import com.rulex.core.engine.port.RuleEngineRawStorePort;
import com.rulex.entity.TransactionRawStore;
import com.rulex.service.TransactionRawStoreService;
import java.util.Optional;
import org.springframework.stereotype.Component;

@Component
public class RuleEngineRawStoreAdapter implements RuleEngineRawStorePort {

  private final TransactionRawStoreService rawStoreService;

  public RuleEngineRawStoreAdapter(TransactionRawStoreService rawStoreService) {
    this.rawStoreService = rawStoreService;
  }

  @Override
  public Optional<TransactionRawStore> findByExternalTransactionId(String externalTransactionId) {
    return rawStoreService.findByExternalTransactionId(externalTransactionId);
  }

  @Override
  public void store(
      String externalTransactionId, String payloadHash, byte[] rawBytes, String contentType) {
    rawStoreService.store(externalTransactionId, payloadHash, rawBytes, contentType);
  }
}
