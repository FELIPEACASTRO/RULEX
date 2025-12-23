package com.rulex.util;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

/**
 * P0 unit test for PAN masking compliance (LGPD/PCI).
 *
 * <p>Validates that PANs are masked to pattern: first6 + "******" + last4.
 */
class PanMaskingUtilTest {

  @Test
  void mask_16digitPan_returnsFirst6AsteriskLast4() {
    // Given
    String pan = "4111111111111111";

    // When
    String masked = PanMaskingUtil.mask(pan);

    // Then
    assertThat(masked).isEqualTo("411111******1111");
    assertThat(masked).matches("^\\d{6}\\*{6}\\d{4}$");
  }

  @Test
  void mask_19digitPan_returnsFirst6AsteriskLast4() {
    // Given
    String pan = "6011000990139424123";

    // When
    String masked = PanMaskingUtil.mask(pan);

    // Then
    assertThat(masked).isEqualTo("601100******4123");
    assertThat(masked).matches("^\\d{6}\\*{6}\\d{4}$");
  }

  @Test
  void mask_shortPan_returnsAsterisks() {
    // Given (edge case: PAN too short to mask properly)
    String pan = "12345678";

    // When
    String masked = PanMaskingUtil.mask(pan);

    // Then (implementation returns "******" for short PANs)
    assertThat(masked).isEqualTo("******");
  }

  @Test
  void mask_nullPan_returnsEmptyString() {
    // Given
    String pan = null;

    // When
    String masked = PanMaskingUtil.mask(pan);

    // Then
    assertThat(masked).isEmpty();
  }

  @Test
  void mask_blankPan_returnsEmptyString() {
    // Given
    String pan = "   ";

    // When
    String masked = PanMaskingUtil.mask(pan);

    // Then
    assertThat(masked).isEmpty();
  }
}
