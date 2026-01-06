package com.rulex.domain.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.rulex.domain.exception.TamperDetectedException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

@DisplayName("TamperDetector - Domain Service")
class TamperDetectorTest {

  private TamperDetector detector;

  @BeforeEach
  void setUp() {
    detector = new TamperDetector();
  }

  @Nested
  @DisplayName("checkTamper() - Exception-based Detection")
  class CheckTamper {

    @Test
    @DisplayName("Should not throw when hashes match")
    void testCheckTamper_MatchingHashes() {
      String hash = "abc123def456";

      detector.checkTamper("TXN-001", hash, hash);

      // No exception thrown - test passes
    }

    @Test
    @DisplayName("Should throw TamperDetectedException when hashes differ")
    void testCheckTamper_DifferentHashes() {
      String originalHash = "abc123def456";
      String receivedHash = "xyz789uvw012";

      assertThatThrownBy(() -> detector.checkTamper("TXN-002", originalHash, receivedHash))
          .isInstanceOf(TamperDetectedException.class)
          .hasMessageContaining("TXN-002")
          .hasMessageContaining(originalHash)
          .hasMessageContaining(receivedHash);
    }

    @Test
    @DisplayName("Should not throw when original hash is null")
    void testCheckTamper_NullOriginalHash() {
      String receivedHash = "abc123def456";

      detector.checkTamper("TXN-003", null, receivedHash);

      // No exception - unable to verify without original hash
    }

    @Test
    @DisplayName("Should not throw when received hash is null")
    void testCheckTamper_NullReceivedHash() {
      String originalHash = "abc123def456";

      detector.checkTamper("TXN-004", originalHash, null);

      // No exception - unable to verify without received hash
    }

    @Test
    @DisplayName("Should not throw when both hashes are null")
    void testCheckTamper_BothHashesNull() {
      detector.checkTamper("TXN-005", null, null);

      // No exception - no hashes to compare
    }

    @Test
    @DisplayName("Should be case-sensitive in hash comparison")
    void testCheckTamper_CaseSensitive() {
      String originalHash = "AbC123DeF456";
      String receivedHash = "abc123def456"; // Different case

      assertThatThrownBy(() -> detector.checkTamper("TXN-006", originalHash, receivedHash))
          .isInstanceOf(TamperDetectedException.class);
    }

    @Test
    @DisplayName("Should detect even minor differences in hash")
    void testCheckTamper_MinorDifference() {
      String originalHash = "abc123def456";
      String receivedHash = "abc123def457"; // Last digit different

      assertThatThrownBy(() -> detector.checkTamper("TXN-007", originalHash, receivedHash))
          .isInstanceOf(TamperDetectedException.class);
    }

    @Test
    @DisplayName("Should handle empty string hashes")
    void testCheckTamper_EmptyStrings() {
      detector.checkTamper("TXN-008", "", "");

      // Both empty strings match - no exception
    }

    @Test
    @DisplayName("Should throw when one hash is empty and other is not")
    void testCheckTamper_EmptyVsNonEmpty() {
      assertThatThrownBy(() -> detector.checkTamper("TXN-009", "", "abc123"))
          .isInstanceOf(TamperDetectedException.class);

      assertThatThrownBy(() -> detector.checkTamper("TXN-010", "abc123", ""))
          .isInstanceOf(TamperDetectedException.class);
    }

    @Test
    @DisplayName("Should handle very long hashes (SHA-256/SHA-512)")
    void testCheckTamper_LongHashes() {
      String sha256Hash =
          "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"; // SHA-256
      String differentHash =
          "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b856"; // Last digit
      // different

      detector.checkTamper("TXN-011", sha256Hash, sha256Hash); // No exception

      assertThatThrownBy(() -> detector.checkTamper("TXN-012", sha256Hash, differentHash))
          .isInstanceOf(TamperDetectedException.class);
    }

    @Test
    @DisplayName("Should handle whitespace in hashes")
    void testCheckTamper_Whitespace() {
      String hashWithSpace = "abc 123 def";
      String hashWithoutSpace = "abc123def";

      // Hashes are different if whitespace differs
      assertThatThrownBy(() -> detector.checkTamper("TXN-013", hashWithSpace, hashWithoutSpace))
          .isInstanceOf(TamperDetectedException.class);
    }
  }

  @Nested
  @DisplayName("isTampered() - Boolean-based Detection")
  class IsTampered {

    @Test
    @DisplayName("Should return false when hashes match")
    void testIsTampered_MatchingHashes() {
      String hash = "abc123def456";

      boolean result = detector.isTampered(hash, hash);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("Should return true when hashes differ")
    void testIsTampered_DifferentHashes() {
      String originalHash = "abc123def456";
      String receivedHash = "xyz789uvw012";

      boolean result = detector.isTampered(originalHash, receivedHash);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Should return false when original hash is null")
    void testIsTampered_NullOriginalHash() {
      String receivedHash = "abc123def456";

      boolean result = detector.isTampered(null, receivedHash);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("Should return false when received hash is null")
    void testIsTampered_NullReceivedHash() {
      String originalHash = "abc123def456";

      boolean result = detector.isTampered(originalHash, null);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("Should return false when both hashes are null")
    void testIsTampered_BothHashesNull() {
      boolean result = detector.isTampered(null, null);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("Should be case-sensitive")
    void testIsTampered_CaseSensitive() {
      String originalHash = "AbC123DeF456";
      String receivedHash = "abc123def456";

      boolean result = detector.isTampered(originalHash, receivedHash);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Should detect minor differences")
    void testIsTampered_MinorDifference() {
      String originalHash = "abc123def456";
      String receivedHash = "abc123def457";

      boolean result = detector.isTampered(originalHash, receivedHash);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Should return false for matching empty strings")
    void testIsTampered_EmptyStrings() {
      boolean result = detector.isTampered("", "");

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("Should return true when one hash is empty and other is not")
    void testIsTampered_EmptyVsNonEmpty() {
      assertThat(detector.isTampered("", "abc123")).isTrue();
      assertThat(detector.isTampered("abc123", "")).isTrue();
    }
  }

  @Nested
  @DisplayName("Integration Scenarios")
  class IntegrationScenarios {

    @Test
    @DisplayName("Replay attack detection: same externalTxnId, different payload")
    void testReplayAttackDetection() {
      String externalTxnId = "TXN-REPLAY-001";
      String originalPayloadHash = "hash-of-legitimate-transaction";
      String replayedPayloadHash = "hash-of-modified-transaction";

      // First transaction - legitimate
      detector.checkTamper(externalTxnId, originalPayloadHash, originalPayloadHash);

      // Replay attempt - should throw
      assertThatThrownBy(
              () -> detector.checkTamper(externalTxnId, originalPayloadHash, replayedPayloadHash))
          .isInstanceOf(TamperDetectedException.class)
          .hasMessageContaining(externalTxnId);
    }

    @Test
    @DisplayName("Idempotency check: same externalTxnId, same payload (legitimate)")
    void testLegitimateIdempotency() {
      String externalTxnId = "TXN-IDEMPOTENT-001";
      String payloadHash = "hash-of-same-transaction";

      // First request
      detector.checkTamper(externalTxnId, payloadHash, payloadHash);

      // Idempotent retry (same payload) - should pass
      detector.checkTamper(externalTxnId, payloadHash, payloadHash);

      // No exception thrown
    }

    @Test
    @DisplayName("Should handle real SHA-256 hash comparison")
    void testRealHashComparison() {
      // Example SHA-256 hashes
      String hash1 =
          "2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae"; // Hash of "foo"
      String hash2 =
          "fcde2b2edba56bf408601fb721fe9b5c338d10ee429ea04fae5511b68fbf8fb9"; // Hash of "bar"

      assertThat(detector.isTampered(hash1, hash1)).isFalse();
      assertThat(detector.isTampered(hash1, hash2)).isTrue();

      detector.checkTamper("TXN-HASH-001", hash1, hash1); // No exception

      assertThatThrownBy(() -> detector.checkTamper("TXN-HASH-002", hash1, hash2))
          .isInstanceOf(TamperDetectedException.class);
    }

    @Test
    @DisplayName("First transaction with new externalTxnId (no original hash)")
    void testFirstTransaction() {
      String externalTxnId = "TXN-FIRST-001";
      String receivedHash = "hash-of-new-transaction";

      // First transaction - no original hash to compare
      detector.checkTamper(externalTxnId, null, receivedHash);

      // Should not throw - this is the first occurrence
    }
  }

  @Nested
  @DisplayName("Edge Cases")
  class EdgeCases {

    @Test
    @DisplayName("Should handle Unicode characters in hashes")
    void testUnicodeHashes() {
      String unicodeHash = "abc123äöü€";

      detector.checkTamper("TXN-UNICODE-001", unicodeHash, unicodeHash);
      assertThat(detector.isTampered(unicodeHash, unicodeHash)).isFalse();

      assertThatThrownBy(() -> detector.checkTamper("TXN-UNICODE-002", unicodeHash, "abc123"))
          .isInstanceOf(TamperDetectedException.class);
    }

    @Test
    @DisplayName("Should handle special characters in hashes")
    void testSpecialCharactersInHashes() {
      String specialHash = "!@#$%^&*()_+-=[]{}|;':\"<>?,./";

      detector.checkTamper("TXN-SPECIAL-001", specialHash, specialHash);
      assertThat(detector.isTampered(specialHash, specialHash)).isFalse();
    }

    @Test
    @DisplayName("Should handle very long externalTransactionId")
    void testLongExternalTxnId() {
      String longTxnId = "TXN-" + "A".repeat(1000);
      String hash = "abc123";
      String differentHash = "xyz789";

      assertThatThrownBy(() -> detector.checkTamper(longTxnId, hash, differentHash))
          .isInstanceOf(TamperDetectedException.class)
          .hasMessageContaining(longTxnId);
    }

    @Test
    @DisplayName("Should handle hashes with leading/trailing spaces")
    void testHashesWithSpaces() {
      String hash = "abc123";
      String hashWithSpaces = " abc123 ";

      // Spaces make hashes different
      assertThat(detector.isTampered(hash, hashWithSpaces)).isTrue();

      assertThatThrownBy(() -> detector.checkTamper("TXN-SPACE-001", hash, hashWithSpaces))
          .isInstanceOf(TamperDetectedException.class);
    }
  }
}
