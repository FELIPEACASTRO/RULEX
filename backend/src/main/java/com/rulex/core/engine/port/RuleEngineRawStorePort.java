package com.rulex.core.engine.port;

import com.rulex.entity.TransactionRawStore;
import java.util.Optional;

public interface RuleEngineRawStorePort {

  Optional<TransactionRawStore> findByExternalTransactionId(String externalTransactionId);

  void store(
      String externalTransactionId, String payloadHash, byte[] rawBytes, String contentType);
}
