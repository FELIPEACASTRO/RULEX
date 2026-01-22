package com.rulex.service.complex.parsing;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class NumericParser {

  private NumericParser() {}

  public static int parseIntSafe(String value, int defaultValue) {
    if (value == null || value.isBlank()) {
      return defaultValue;
    }
    try {
      return Integer.parseInt(value.trim());
    } catch (NumberFormatException e) {
      log.warn(
          "Valor inválido para parse int: '{}', usando default: {}",
          maskSensitiveData(value),
          defaultValue);
      return defaultValue;
    }
  }

  public static double parseDoubleSafe(String value, double defaultValue) {
    if (value == null || value.isBlank()) {
      return defaultValue;
    }
    try {
      return Double.parseDouble(value.trim());
    } catch (NumberFormatException e) {
      log.warn(
          "Valor inválido para parse double: '{}', usando default: {}",
          maskSensitiveData(value),
          defaultValue);
      return defaultValue;
    }
  }

  private static String maskSensitiveData(String data) {
    if (data == null || data.length() < 4) {
      return "****";
    }
    return "****" + data.substring(Math.max(0, data.length() - 4));
  }
}
