package com.rulex.exception;

/** Exceção lançada quando ocorre erro de conexão com Redis. */
public class RedisConnectionException extends RulexException {

  private static final long serialVersionUID = 1L;

  private final String host;
  private final int port;

  public RedisConnectionException(String message) {
    super(message);
    this.host = null;
    this.port = 0;
  }

  public RedisConnectionException(String message, Throwable cause) {
    super(message, cause);
    this.host = null;
    this.port = 0;
  }

  public RedisConnectionException(String host, int port, String message, Throwable cause) {
    super(String.format("Erro de conexão Redis [%s:%d]: %s", host, port, message), cause);
    this.host = host;
    this.port = port;
  }

  public String getHost() {
    return host;
  }

  public int getPort() {
    return port;
  }
}
