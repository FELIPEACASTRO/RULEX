package com.rulex.exception;

/** Exceção lançada quando ocorre erro no serviço de geolocalização. */
public class GeoServiceException extends RulexException {

  private static final long serialVersionUID = 1L;

  private final String operation;

  public GeoServiceException(String message) {
    super(message);
    this.operation = null;
  }

  public GeoServiceException(String message, Throwable cause) {
    super(message, cause);
    this.operation = null;
  }

  public GeoServiceException(String operation, String message, Throwable cause) {
    super(String.format("Erro em operação geo '%s': %s", operation, message), cause);
    this.operation = operation;
  }

  public String getOperation() {
    return operation;
  }
}
