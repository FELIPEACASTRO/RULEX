package com.rulex.domain.exception;

import java.io.Serial;

/**
 * Exceção base para erros de domínio RULEX.
 *
 * <p>Exceções de domínio são checked para forçar tratamento explícito. Não devem conter detalhes de
 * infraestrutura (SQL, HTTP, etc.).
 */
public abstract class DomainException extends RuntimeException {

  @Serial private static final long serialVersionUID = 1L;

  private final String errorCode;

  protected DomainException(String errorCode, String message) {
    super(message);
    this.errorCode = errorCode;
  }

  protected DomainException(String errorCode, String message, Throwable cause) {
    super(message, cause);
    this.errorCode = errorCode;
  }

  public String getErrorCode() {
    return errorCode;
  }
}
