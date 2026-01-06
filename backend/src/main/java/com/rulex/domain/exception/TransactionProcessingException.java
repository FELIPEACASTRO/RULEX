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

  private final String transactionId;

  public TransactionProcessingException(String transactionId, String message) {
    super("TRANSACTION_PROCESSING_ERROR", message);
    this.transactionId = transactionId;
  }

  public TransactionProcessingException(String transactionId, String message, Throwable cause) {
    super("TRANSACTION_PROCESSING_ERROR", message, cause);
    this.transactionId = transactionId;
  }

  public String getTransactionId() {
    return transactionId;
  }

  /** Alias para compatibilidade */
  public String getExternalTransactionId() {
    return transactionId;
  }

  /** Cria exceção para erro de serialização */
  public static TransactionProcessingException serializationError(
      String transactionId, Throwable cause) {
    return new TransactionProcessingException(
        transactionId, "Erro ao serializar/deserializar payload", cause);
  }

  /** Cria exceção para erro de persistência */
  public static TransactionProcessingException persistenceError(
      String transactionId, Throwable cause) {
    return new TransactionProcessingException(transactionId, "Erro ao persistir transação", cause);
  }

  /** Cria exceção para erro de avaliação de regras */
  public static TransactionProcessingException ruleEvaluationError(
      String transactionId, Throwable cause) {
    return new TransactionProcessingException(transactionId, "Erro ao avaliar regras", cause);
  }
}
