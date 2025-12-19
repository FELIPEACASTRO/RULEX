package com.rulex.api;

/** Constants for correlation-id propagation across the request lifecycle. */
public final class CorrelationId {
  public static final String HEADER = "X-Correlation-Id";
  public static final String REQUEST_ATTR = "CORRELATION_ID";

  private CorrelationId() {}
}
