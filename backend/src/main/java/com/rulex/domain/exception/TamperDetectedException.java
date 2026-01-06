package com.rulex.domain.exception;

import java.io.Serial;

/**
 * Exceção lançada quando é detectada tentativa de adulteração do payload.
 *
 * <p>Ocorre quando o hash do payload atual não confere com o hash original armazenado para o mesmo
 * externalTransactionId.
 *
 * <p>Esta é uma violação de segurança grave - a transação deve ser classificada como FRAUD.
 */
public class TamperDetectedException extends DomainException {

  @Serial private static final long serialVersionUID = 1L;

  private final String externalTransactionId;
  private final String originalHash;
  private final String receivedHash;

  public TamperDetectedException(
      String externalTransactionId, String originalHash, String receivedHash) {
    super(
        "TAMPER_DETECTED",
        String.format(
            "Tentativa de adulteração detectada para transação %s. "
                + "Hash original: %s, Hash recebido: %s",
            externalTransactionId, originalHash, receivedHash));
    this.externalTransactionId = externalTransactionId;
    this.originalHash = originalHash;
    this.receivedHash = receivedHash;
  }

  public String getExternalTransactionId() {
    return externalTransactionId;
  }

  public String getOriginalHash() {
    return originalHash;
  }

  public String getReceivedHash() {
    return receivedHash;
  }
}
