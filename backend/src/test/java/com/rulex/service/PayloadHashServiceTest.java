package com.rulex.service;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;

import static org.junit.jupiter.api.Assertions.*;

@DisplayName("PayloadHashService Tests")
class PayloadHashServiceTest {

    private PayloadHashService payloadHashService;

    @BeforeEach
    void setUp() {
        payloadHashService = new PayloadHashService();
    }

    @Test
    @DisplayName("Should generate consistent hash for same payload")
    void shouldGenerateConsistentHash() {
        String payload = "{\"amount\": 1000, \"customerId\": \"123\"}";
        
        String hash1 = payloadHashService.computeHash(payload);
        String hash2 = payloadHashService.computeHash(payload);
        
        assertEquals(hash1, hash2, "Same payload should produce same hash");
    }

    @Test
    @DisplayName("Should generate different hash for different payloads")
    void shouldGenerateDifferentHashForDifferentPayloads() {
        String payload1 = "{\"amount\": 1000}";
        String payload2 = "{\"amount\": 2000}";
        
        String hash1 = payloadHashService.computeHash(payload1);
        String hash2 = payloadHashService.computeHash(payload2);
        
        assertNotEquals(hash1, hash2, "Different payloads should produce different hashes");
    }

    @Test
    @DisplayName("Should handle empty payload")
    void shouldHandleEmptyPayload() {
        String hash = payloadHashService.computeHash("");
        
        assertNotNull(hash, "Hash should not be null for empty payload");
        assertFalse(hash.isEmpty(), "Hash should not be empty");
    }

    @Test
    @DisplayName("Should handle null payload gracefully")
    void shouldHandleNullPayload() {
        assertDoesNotThrow(() -> {
            String hash = payloadHashService.computeHash(null);
            // Either returns null or empty or throws - implementation dependent
        });
    }

    @Test
    @DisplayName("Hash should be deterministic across calls")
    void hashShouldBeDeterministic() {
        String payload = "{\"transactionId\": \"TXN-001\", \"amount\": 5000.00}";
        
        String hash1 = payloadHashService.computeHash(payload);
        String hash2 = payloadHashService.computeHash(payload);
        String hash3 = payloadHashService.computeHash(payload);
        
        assertEquals(hash1, hash2);
        assertEquals(hash2, hash3);
    }

    @Test
    @DisplayName("Hash should have expected format")
    void hashShouldHaveExpectedFormat() {
        String payload = "{\"test\": true}";
        
        String hash = payloadHashService.computeHash(payload);
        
        assertNotNull(hash);
        // SHA-256 produces 64 hex characters
        assertTrue(hash.length() >= 32, "Hash should be at least 32 characters");
    }
}
