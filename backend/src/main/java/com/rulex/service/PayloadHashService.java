package com.rulex.service;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.json.JsonMapper;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class PayloadHashService {

  private final ObjectMapper objectMapper;

  // Cached canonical mapper built once using non-deprecated builder API
  private volatile ObjectMapper cachedCanonicalMapper;

  public CanonicalPayload canonicalize(Object payload) {
    try {
      JsonNode node = objectMapper.valueToTree(payload);
      String canonicalJson = canonicalMapper().writeValueAsString(node);
      return new CanonicalPayload(canonicalJson, sha256Hex(canonicalJson));
    } catch (JsonProcessingException e) {
      throw new IllegalArgumentException("Falha ao canonizar payload", e);
    }
  }

  /** V3.1: SHA-256 of the raw HTTP body bytes, exactly as received. */
  public String sha256Hex(byte[] rawBytes) {
    try {
      MessageDigest digest = MessageDigest.getInstance("SHA-256");
      byte[] hashed = digest.digest(rawBytes);
      return toHex(hashed);
    } catch (NoSuchAlgorithmException e) {
      throw new IllegalStateException("SHA-256 indisponível no runtime", e);
    }
  }

  /** Convenience for callers that only have a raw string (already as-received) and charset. */
  public String sha256Hex(String raw, Charset charset) {
    return sha256Hex(raw.getBytes(charset));
  }

  /**
   * Creates a canonical ObjectMapper using the non-deprecated JsonMapper.builder() API. Uses
   * double-checked locking for thread-safe lazy initialization.
   */
  private ObjectMapper canonicalMapper() {
    ObjectMapper result = cachedCanonicalMapper;
    if (result == null) {
      synchronized (this) {
        result = cachedCanonicalMapper;
        if (result == null) {
          result =
              JsonMapper.builder()
                  .serializationInclusion(JsonInclude.Include.NON_NULL)
                  .disable(SerializationFeature.INDENT_OUTPUT)
                  .enable(SerializationFeature.ORDER_MAP_ENTRIES_BY_KEYS)
                  .enable(MapperFeature.SORT_PROPERTIES_ALPHABETICALLY)
                  .build();
          cachedCanonicalMapper = result;
        }
      }
    }
    return result;
  }

  private String sha256Hex(String value) {
    try {
      MessageDigest digest = MessageDigest.getInstance("SHA-256");
      byte[] hashed = digest.digest(value.getBytes(StandardCharsets.UTF_8));
      return toHex(hashed);
    } catch (NoSuchAlgorithmException e) {
      throw new IllegalStateException("SHA-256 indisponível no runtime", e);
    }
  }

  private String toHex(byte[] bytes) {
    StringBuilder sb = new StringBuilder(bytes.length * 2);
    for (byte b : bytes) {
      sb.append(Character.forDigit((b >> 4) & 0xF, 16));
      sb.append(Character.forDigit((b & 0xF), 16));
    }
    return sb.toString();
  }

  public record CanonicalPayload(String json, String sha256Hex) {}
}
