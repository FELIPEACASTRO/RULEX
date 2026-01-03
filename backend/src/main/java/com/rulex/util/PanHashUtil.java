package com.rulex.util;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;

/** Utility for generating stable, irreversible hashes (e.g., PAN correlation). */
public final class PanHashUtil {

  private PanHashUtil() {}

  public static String sha256Hex(String value) {
    if (value == null) {
      return null;
    }
    try {
      MessageDigest md = MessageDigest.getInstance("SHA-256");
      byte[] bytes = md.digest(value.getBytes(StandardCharsets.UTF_8));
      StringBuilder sb = new StringBuilder(bytes.length * 2);
      for (byte b : bytes) {
        sb.append(Character.forDigit((b >>> 4) & 0xF, 16));
        sb.append(Character.forDigit(b & 0xF, 16));
      }
      return sb.toString();
    } catch (Exception e) {
      throw new IllegalStateException("SHA-256 not available", e);
    }
  }
}
