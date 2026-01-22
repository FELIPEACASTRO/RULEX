package com.rulex.exception;

/** Exceção lançada quando ocorre erro de conexão com Neo4j. */
public class Neo4jConnectionException extends RulexException {

  private static final long serialVersionUID = 1L;

  private final String uri;

  public Neo4jConnectionException(String message) {
    super(message);
    this.uri = null;
  }

  public Neo4jConnectionException(String message, Throwable cause) {
    super(message, cause);
    this.uri = null;
  }

  public Neo4jConnectionException(String uri, String message, Throwable cause) {
    super(String.format("Erro de conexão Neo4j [%s]: %s", uri, message), cause);
    this.uri = uri;
  }

  public String getUri() {
    return uri;
  }
}
