package com.rulex.service;

import static org.junit.jupiter.api.Assertions.*;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("PayloadHashService Tests")
class PayloadHashServiceTest {

  private PayloadHashService payloadHashService;
  private ObjectMapper objectMapper;

  @BeforeEach
  void setUp() {
    objectMapper = new ObjectMapper();
    payloadHashService = new PayloadHashService(objectMapper);
  }

  @Test
  @DisplayName("Should generate consistent hash for same payload bytes")
  void shouldGenerateConsistentHash() {
    byte[] payload = "{\"amount\": 1000, \"customerId\": \"123\"}".getBytes(StandardCharsets.UTF_8);

    String hash1 = payloadHashService.sha256Hex(payload);
    String hash2 = payloadHashService.sha256Hex(payload);

    assertEquals(hash1, hash2, "Same payload should produce same hash");
  }

  @Test
  @DisplayName("Should generate different hash for different payloads")
  void shouldGenerateDifferentHashForDifferentPayloads() {
    byte[] payload1 = "{\"amount\": 1000}".getBytes(StandardCharsets.UTF_8);
    byte[] payload2 = "{\"amount\": 2000}".getBytes(StandardCharsets.UTF_8);

    String hash1 = payloadHashService.sha256Hex(payload1);
    String hash2 = payloadHashService.sha256Hex(payload2);

    assertNotEquals(hash1, hash2, "Different payloads should produce different hashes");
  }

  @Test
  @DisplayName("Should handle empty payload")
  void shouldHandleEmptyPayload() {
    byte[] emptyPayload = "".getBytes(StandardCharsets.UTF_8);

    String hash = payloadHashService.sha256Hex(emptyPayload);

    assertNotNull(hash, "Hash should not be null for empty payload");
    assertFalse(hash.isEmpty(), "Hash should not be empty");
    assertEquals(64, hash.length(), "SHA-256 should produce 64 hex characters");
  }

  @Test
  @DisplayName("Hash should be deterministic across calls")
  void hashShouldBeDeterministic() {
    byte[] payload =
        "{\"transactionId\": \"TXN-001\", \"amount\": 5000.00}".getBytes(StandardCharsets.UTF_8);

    String hash1 = payloadHashService.sha256Hex(payload);
    String hash2 = payloadHashService.sha256Hex(payload);
    String hash3 = payloadHashService.sha256Hex(payload);

    assertEquals(hash1, hash2);
    assertEquals(hash2, hash3);
  }

  @Test
  @DisplayName("Hash should have expected SHA-256 format (64 hex chars)")
  void hashShouldHaveExpectedFormat() {
    byte[] payload = "{\"test\": true}".getBytes(StandardCharsets.UTF_8);

    String hash = payloadHashService.sha256Hex(payload);

    assertNotNull(hash);
    assertEquals(64, hash.length(), "SHA-256 should produce 64 hex characters");
    assertTrue(hash.matches("[0-9a-f]+"), "Hash should be lowercase hex");
  }

  @Test
  @DisplayName("Should canonicalize and hash object payload")
  void shouldCanonicalizeAndHashObject() {
    Map<String, Object> payload = Map.of("amount", 1000, "customerId", "123");

    PayloadHashService.CanonicalPayload result = payloadHashService.canonicalize(payload);

    assertNotNull(result);
    assertNotNull(result.json());
    assertNotNull(result.sha256Hex());
    assertEquals(64, result.sha256Hex().length());
  }

  @Test
  @DisplayName("Canonicalization should produce consistent results")
  void canonicalizationShouldBeConsistent() {
    Map<String, Object> payload =
        Map.of(
            "b", 2,
            "a", 1);

    PayloadHashService.CanonicalPayload result1 = payloadHashService.canonicalize(payload);
    PayloadHashService.CanonicalPayload result2 = payloadHashService.canonicalize(payload);

    assertEquals(result1.sha256Hex(), result2.sha256Hex());
  }

  @Test
  @DisplayName("Should compute hash from string with charset")
  void shouldComputeHashFromStringWithCharset() {
    String raw = "{\"test\": true}";

    String hash = payloadHashService.sha256Hex(raw, StandardCharsets.UTF_8);

    assertNotNull(hash);
    assertEquals(64, hash.length());
  }
}
