package com.rulex.v31.trace;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class FeatureUsageCollector {

  private final Map<String, FeatureUsed> dedupe = new LinkedHashMap<>();

  public void record(
      String source,
      String name,
      String entityKey,
      String featureVersion,
      String windowName,
      Object rawValue) {
    String valueHash = sha256Hex(rawValue == null ? "null" : String.valueOf(rawValue));

    FeatureUsed item =
        new FeatureUsed(source, name, entityKey, featureVersion, windowName, valueHash, null);

    // Stable dedupe key (keeps insertion order)
    String key =
        source
            + "|"
            + (name == null ? "" : name)
            + "|"
            + (entityKey == null ? "" : entityKey)
            + "|"
            + (featureVersion == null ? "" : featureVersion)
            + "|"
            + (windowName == null ? "" : windowName)
            + "|"
            + (valueHash == null ? "" : valueHash);

    dedupe.putIfAbsent(key, item);
  }

  public List<FeatureUsed> snapshot() {
    return new ArrayList<>(dedupe.values());
  }

  private static String sha256Hex(String s) {
    try {
      MessageDigest digest = MessageDigest.getInstance("SHA-256");
      byte[] hash = digest.digest(s.getBytes(StandardCharsets.UTF_8));
      StringBuilder sb = new StringBuilder(hash.length * 2);
      for (byte b : hash) {
        sb.append(String.format("%02x", b));
      }
      return sb.toString();
    } catch (Exception e) {
      // Deterministic fallback: never fail evaluation because of audit.
      return "sha256_error";
    }
  }
}
