package com.rulex.application.port.out;

import com.rulex.domain.model.Decision;
import java.util.Optional;

/**
 * Port de persistência de decisões.
 *
 * <p>Define como a camada de aplicação persiste decisões. Implementação em
 * infrastructure/persistence.
 */
public interface DecisionPersistencePort {

  /**
   * Busca decisão por ID externo da transação.
   *
   * @param externalTransactionId identificador externo da transação
   * @return decisão encontrada ou empty
   */
  Optional<Decision> findByExternalTransactionId(String externalTransactionId);

  /**
   * Persiste uma decisão.
   *
   * @param decision decisão a persistir
   * @param transactionId ID interno da transação (FK)
   * @return decisão persistida
   */
  Decision save(Decision decision, Long transactionId);

  /**
   * Verifica se já existe decisão para a transação.
   *
   * @param externalTransactionId identificador externo
   * @return true se já existe decisão
   */
  boolean existsByExternalTransactionId(String externalTransactionId);
}
