package com.rulex.application.port.out;

import com.rulex.domain.model.TransactionData;
import java.util.Optional;

/**
 * Port de persistência de transações.
 *
 * <p>Define como a camada de aplicação acessa transações. Implementação em
 * infrastructure/persistence.
 */
public interface TransactionPersistencePort {

  /**
   * Busca transação por ID externo.
   *
   * @param externalTransactionId identificador externo
   * @return transação encontrada ou empty
   */
  Optional<TransactionData> findByExternalId(String externalTransactionId);

  /**
   * Busca hash de payload por ID externo.
   *
   * @param externalTransactionId identificador externo
   * @return hash do payload original ou empty
   */
  Optional<String> findPayloadHashByExternalId(String externalTransactionId);

  /**
   * Persiste uma nova transação.
   *
   * @param transaction transação a persistir
   * @param payloadHash hash do payload original
   * @return transação persistida
   */
  TransactionData save(TransactionData transaction, String payloadHash);

  /**
   * Verifica se transação existe.
   *
   * @param externalTransactionId identificador externo
   * @return true se existe
   */
  boolean existsByExternalId(String externalTransactionId);
}
