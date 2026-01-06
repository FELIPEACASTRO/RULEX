package com.rulex.domain.exception;

import java.io.Serial;

/**
 * Exceção lançada quando ocorre erro durante processamento de transação.
 *
 * <p>Encapsula erros de infraestrutura (DB, Redis, etc.) em exceção de domínio para não vazar
 * detalhes técnicos.
 */
public class TransactionProcessingException extends DomainException {

  @Serial private static final long serialVersionUID = 1L;

  private final String externalTransactionId;

  public TransactionProcessingException(String externalTransactionId, String message) {
    super("TRANSACTION_PROCESSING_ERROR", message);
    this.externalTransactionId = externalTransactionId;
  }

  public TransactionProcessingException(
      String externalTransactionId, String message, Throwable cause) {
    super("TRANSACTION_PROCESSING_ERROR", message, cause);
    this.externalTransactionId = externalTransactionId;
  }

  public String getExternalTransactionId() {
    return externalTransactionId;
  }

  /** Cria exceção para erro de serialização */
  public static TransactionProcessingException serializationError(
      String externalTransactionId, Throwable cause) {
    return new TransactionProcessingException(
        externalTransactionId, "Erro ao serializar/deserializar payload", cause);
  }

  /** Cria exceção para erro de persistência */
  public static TransactionProcessingException persistenceError(
      String externalTransactionId, Throwable cause) {
    return new TransactionProcessingException(
        externalTransactionId, "Erro ao persistir transação", cause);
  }

  /** Cria exceção para erro de avaliação de regras */
  public static TransactionProcessingException ruleEvaluationError(
      String externalTransactionId, Throwable cause) {
    return new TransactionProcessingException(
        externalTransactionId, "Erro ao avaliar regras", cause);
  }
}
