package com.rulex.service;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class PayloadHashService {

  private final ObjectMapper objectMapper;

  public CanonicalPayload canonicalize(Object payload) {
    try {
      JsonNode node = objectMapper.valueToTree(payload);
      String canonicalJson = canonicalMapper().writeValueAsString(node);
      return new CanonicalPayload(canonicalJson, sha256Hex(canonicalJson));
    } catch (JsonProcessingException e) {
      throw new IllegalArgumentException("Falha ao canonizar payload", e);
    }
  }

  @SuppressWarnings("deprecation")
  private ObjectMapper canonicalMapper() {
    return objectMapper
        .copy()
        .setSerializationInclusion(JsonInclude.Include.NON_NULL)
        .configure(SerializationFeature.INDENT_OUTPUT, false)
        .configure(SerializationFeature.ORDER_MAP_ENTRIES_BY_KEYS, true)
        .configure(MapperFeature.SORT_PROPERTIES_ALPHABETICALLY, true);
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
