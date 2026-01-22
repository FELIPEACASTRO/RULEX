package com.rulex.service.complex.parsing;

public final class BooleanParser {

  private BooleanParser() {}

  public static Boolean toBoolean(Object value) {
    if (value == null) return null;
    if (value instanceof Boolean) return (Boolean) value;
    String str = String.valueOf(value).toLowerCase();
    return "true".equals(str) || "1".equals(str) || "yes".equals(str);
  }
}
