package com.rulex.service;

import static org.assertj.core.api.Assertions.assertThat;

import com.rulex.dto.TransactionRequest;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.Map;
import java.util.Optional;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Testes unitários para DerivedContext. Valida parsing de tipos derivados sem alterar o payload
 * original.
 */
class DerivedContextTest {

  @Nested
  @DisplayName("parseDate - Integer YYYYMMDD → LocalDate")
  class ParseDateTests {

    @Test
    @DisplayName("Deve parsear data válida 20251231")
    void shouldParseValidDate() {
      Optional<LocalDate> result = DerivedContext.parseDate(20251231);

      assertThat(result).isPresent();
      assertThat(result.get()).isEqualTo(LocalDate.of(2025, 12, 31));
    }

    @Test
    @DisplayName("Deve parsear data com zeros à esquerda 20250101")
    void shouldParseDateWithLeadingZeros() {
      Optional<LocalDate> result = DerivedContext.parseDate(20250101);

      assertThat(result).isPresent();
      assertThat(result.get()).isEqualTo(LocalDate.of(2025, 1, 1));
    }

    @Test
    @DisplayName("Deve retornar empty para null")
    void shouldReturnEmptyForNull() {
      Optional<LocalDate> result = DerivedContext.parseDate(null);

      assertThat(result).isEmpty();
    }

    @ParameterizedTest
    @ValueSource(ints = {99999999, 20251301, 20251232, 0, -1})
    @DisplayName("Deve retornar empty para datas inválidas")
    void shouldReturnEmptyForInvalidDates(int invalidDate) {
      Optional<LocalDate> result = DerivedContext.parseDate(invalidDate);

      assertThat(result).isEmpty();
    }
  }

  @Nested
  @DisplayName("parseTime - Integer HHMMSS → LocalTime")
  class ParseTimeTests {

    @Test
    @DisplayName("Deve parsear hora válida 143052")
    void shouldParseValidTime() {
      Optional<LocalTime> result = DerivedContext.parseTime(143052);

      assertThat(result).isPresent();
      assertThat(result.get()).isEqualTo(LocalTime.of(14, 30, 52));
    }

    @Test
    @DisplayName("Deve parsear meia-noite 000000")
    void shouldParseMidnight() {
      Optional<LocalTime> result = DerivedContext.parseTime(0);

      assertThat(result).isPresent();
      assertThat(result.get()).isEqualTo(LocalTime.MIDNIGHT);
    }

    @Test
    @DisplayName("Deve parsear 23:59:59")
    void shouldParseEndOfDay() {
      Optional<LocalTime> result = DerivedContext.parseTime(235959);

      assertThat(result).isPresent();
      assertThat(result.get()).isEqualTo(LocalTime.of(23, 59, 59));
    }

    @Test
    @DisplayName("Deve retornar empty para null")
    void shouldReturnEmptyForNull() {
      Optional<LocalTime> result = DerivedContext.parseTime(null);

      assertThat(result).isEmpty();
    }

    @ParameterizedTest
    @ValueSource(ints = {250000, 126000, 120060, -1})
    @DisplayName("Deve retornar empty para horas inválidas")
    void shouldReturnEmptyForInvalidTimes(int invalidTime) {
      Optional<LocalTime> result = DerivedContext.parseTime(invalidTime);

      assertThat(result).isEmpty();
    }
  }

  @Nested
  @DisplayName("parseGmtOffset - String → ZoneOffset")
  class ParseGmtOffsetTests {

    @ParameterizedTest
    @CsvSource({"-0300, -3", "+0000, 0", "+0530, 5.5", "-1200, -12", "+1400, 14"})
    @DisplayName("Deve parsear offsets válidos no formato -HHMM")
    void shouldParseValidOffsets(String offset, double expectedHours) {
      Optional<ZoneOffset> result = DerivedContext.parseGmtOffset(offset);

      assertThat(result).isPresent();
      int expectedSeconds = (int) (expectedHours * 3600);
      assertThat(result.get().getTotalSeconds()).isEqualTo(expectedSeconds);
    }

    @Test
    @DisplayName("Deve retornar empty para null")
    void shouldReturnEmptyForNull() {
      Optional<ZoneOffset> result = DerivedContext.parseGmtOffset(null);

      assertThat(result).isEmpty();
    }

    @Test
    @DisplayName("Deve retornar empty para string vazia")
    void shouldReturnEmptyForBlank() {
      Optional<ZoneOffset> result = DerivedContext.parseGmtOffset("   ");

      assertThat(result).isEmpty();
    }
  }

  @Nested
  @DisplayName("deriveTimestamp - LocalDate + LocalTime + gmtOffset → OffsetDateTime")
  class DeriveTimestampTests {

    @Test
    @DisplayName("Deve combinar date + time + offset corretamente")
    void shouldCombineDateTimeOffset() {
      LocalDate date = LocalDate.of(2025, 12, 31);
      LocalTime time = LocalTime.of(14, 30, 0);
      String offset = "-0300";

      Optional<OffsetDateTime> result = DerivedContext.deriveTimestamp(date, time, offset);

      assertThat(result).isPresent();
      assertThat(result.get().getYear()).isEqualTo(2025);
      assertThat(result.get().getMonthValue()).isEqualTo(12);
      assertThat(result.get().getDayOfMonth()).isEqualTo(31);
      assertThat(result.get().getHour()).isEqualTo(14);
      assertThat(result.get().getMinute()).isEqualTo(30);
      assertThat(result.get().getOffset()).isEqualTo(ZoneOffset.ofHours(-3));
    }

    @Test
    @DisplayName("Deve usar UTC como fallback quando offset é null")
    void shouldUseUtcWhenOffsetIsNull() {
      LocalDate date = LocalDate.of(2025, 1, 1);
      LocalTime time = LocalTime.of(0, 0, 0);

      Optional<OffsetDateTime> result = DerivedContext.deriveTimestamp(date, time, null);

      assertThat(result).isPresent();
      assertThat(result.get().getOffset()).isEqualTo(ZoneOffset.UTC);
    }

    @Test
    @DisplayName("Deve retornar empty quando date é null")
    void shouldReturnEmptyWhenDateIsNull() {
      Optional<OffsetDateTime> result =
          DerivedContext.deriveTimestamp(null, LocalTime.NOON, "-0300");

      assertThat(result).isEmpty();
    }

    @Test
    @DisplayName("Deve retornar empty quando time é null")
    void shouldReturnEmptyWhenTimeIsNull() {
      Optional<OffsetDateTime> result =
          DerivedContext.deriveTimestamp(LocalDate.now(), null, "-0300");

      assertThat(result).isEmpty();
    }
  }

  @Nested
  @DisplayName("maskPan - Mascaramento de PAN")
  class MaskPanTests {

    @Test
    @DisplayName("Deve mascarar PAN de 16 dígitos")
    void shouldMask16DigitPan() {
      String result = DerivedContext.maskPan("4111111111111111");

      assertThat(result).isEqualTo("411111******1111");
    }

    @Test
    @DisplayName("Deve mascarar PAN de 15 dígitos (Amex)")
    void shouldMask15DigitPan() {
      String result = DerivedContext.maskPan("378282246310005");

      assertThat(result).isEqualTo("378282*****0005");
    }

    @Test
    @DisplayName("Deve mascarar PAN com espaços")
    void shouldMaskPanWithSpaces() {
      String result = DerivedContext.maskPan("4111 1111 1111 1111");

      assertThat(result).isEqualTo("411111******1111");
    }

    @Test
    @DisplayName("Deve retornar original para PAN muito curto")
    void shouldReturnOriginalForShortPan() {
      String result = DerivedContext.maskPan("411111");

      assertThat(result).isEqualTo("411111");
    }

    @Test
    @DisplayName("Deve retornar null para null")
    void shouldReturnNullForNull() {
      String result = DerivedContext.maskPan(null);

      assertThat(result).isNull();
    }
  }

  @Nested
  @DisplayName("extractBin - Extração de BIN")
  class ExtractBinTests {

    @Test
    @DisplayName("Deve extrair BIN de 6 dígitos")
    void shouldExtract6DigitBin() {
      String result = DerivedContext.extractBin("4111111111111111");

      assertThat(result).isEqualTo("411111");
    }

    @Test
    @DisplayName("Deve retornar null para PAN muito curto")
    void shouldReturnNullForShortPan() {
      String result = DerivedContext.extractBin("41111");

      assertThat(result).isNull();
    }
  }

  @Nested
  @DisplayName("extractLast4 - Extração dos últimos 4 dígitos")
  class ExtractLast4Tests {

    @Test
    @DisplayName("Deve extrair últimos 4 dígitos")
    void shouldExtractLast4() {
      String result = DerivedContext.extractLast4("4111111111111111");

      assertThat(result).isEqualTo("1111");
    }

    @Test
    @DisplayName("Deve extrair de PAN com espaços")
    void shouldExtractFromPanWithSpaces() {
      String result = DerivedContext.extractLast4("4111 1111 1111 9999");

      assertThat(result).isEqualTo("9999");
    }
  }

  @Nested
  @DisplayName("normalizePostalCode - Normalização de CEP")
  class NormalizePostalCodeTests {

    @ParameterizedTest
    @CsvSource({
      "01310-100, 01310100",
      "01310100, 01310100",
      "01310 100, 01310100",
      "12345-678, 12345678"
    })
    @DisplayName("Deve normalizar CEPs brasileiros")
    void shouldNormalizeBrazilianPostalCodes(String input, String expected) {
      String result = DerivedContext.normalizePostalCode(input);

      assertThat(result).isEqualTo(expected);
    }

    @Test
    @DisplayName("Deve retornar null para null")
    void shouldReturnNullForNull() {
      String result = DerivedContext.normalizePostalCode(null);

      assertThat(result).isNull();
    }
  }

  @Nested
  @DisplayName("normalizeState - Normalização de Estado")
  class NormalizeStateTests {

    @ParameterizedTest
    @CsvSource({"sp, SP", "SP, SP", " sp , SP", "rj, RJ"})
    @DisplayName("Deve normalizar estados para uppercase")
    void shouldNormalizeStates(String input, String expected) {
      String result = DerivedContext.normalizeState(input);

      assertThat(result).isEqualTo(expected);
    }
  }

  @Nested
  @DisplayName("normalizeCountryCode - Normalização de Código de País")
  class NormalizeCountryCodeTests {

    @ParameterizedTest
    @CsvSource({"76, 076", "076, 076", "1, 001", "840, 840"})
    @DisplayName("Deve normalizar códigos de país para 3 dígitos")
    void shouldNormalizeCountryCodes(String input, String expected) {
      String result = DerivedContext.normalizeCountryCode(input);

      assertThat(result).isEqualTo(expected);
    }

    @Test
    @DisplayName("Deve manter código alpha inalterado")
    void shouldKeepAlphaCodeUnchanged() {
      String result = DerivedContext.normalizeCountryCode("BRA");

      assertThat(result).isEqualTo("BRA");
    }
  }

  @Nested
  @DisplayName("from - Criação completa do DerivedContext")
  class FromTests {

    @Test
    @DisplayName("Deve criar contexto completo a partir de TransactionRequest")
    void shouldCreateCompleteContext() {
      TransactionRequest request =
          TransactionRequest.builder()
              .externalTransactionId("TX-001")
              .customerIdFromHeader("CUST-001")
              .customerAcctNumber(123456789L)
              .pan("4111111111111111")
              .transactionDate(20251231)
              .transactionTime(143052)
              .gmtOffset("-0300")
              .transactionAmount(BigDecimal.valueOf(1000.00))
              .transactionCurrencyCode(986)
              .mcc(5411)
              .consumerAuthenticationScore(100)
              .externalScore3(100)
              .cavvResult(0)
              .eciIndicator(5)
              .atcCard(1)
              .atcHost(1)
              .tokenAssuranceLevel(0)
              .availableCredit(BigDecimal.valueOf(5000))
              .cardCashBalance(BigDecimal.ZERO)
              .cardDelinquentAmount(BigDecimal.ZERO)
              .merchantCountryCode("76")
              .merchantState("sp")
              .merchantPostalCode("01310-100")
              .build();

      DerivedContext context = DerivedContext.from(request);

      assertThat(context.getOriginalPayload()).isEqualTo(request);
      assertThat(context.getTransactionLocalDate()).isEqualTo(LocalDate.of(2025, 12, 31));
      assertThat(context.getTransactionLocalTime()).isEqualTo(LocalTime.of(14, 30, 52));
      assertThat(context.getTransactionTimestamp()).isNotNull();
      assertThat(context.getTransactionTimestamp().getOffset()).isEqualTo(ZoneOffset.ofHours(-3));
      assertThat(context.getMaskedPan()).isEqualTo("411111******1111");
      assertThat(context.getBin()).isEqualTo("411111");
      assertThat(context.getLast4()).isEqualTo("1111");
      assertThat(context.getNormalizedCountryCode()).isEqualTo("076");
      assertThat(context.getNormalizedState()).isEqualTo("SP");
      assertThat(context.getNormalizedPostalCode()).isEqualTo("01310100");
    }

    @Test
    @DisplayName("Deve criar contexto vazio para request null")
    void shouldCreateEmptyContextForNullRequest() {
      DerivedContext context = DerivedContext.from(null);

      assertThat(context.getOriginalPayload()).isNull();
      assertThat(context.getTransactionLocalDate()).isNull();
      assertThat(context.getTransactionTimestamp()).isNull();
    }

    @Test
    @DisplayName("Deve criar contexto parcial com campos faltantes")
    void shouldCreatePartialContextWithMissingFields() {
      TransactionRequest request =
          TransactionRequest.builder()
              .externalTransactionId("TX-002")
              .customerIdFromHeader("CUST-002")
              .customerAcctNumber(123456789L)
              .pan("4111111111111111")
              .transactionAmount(BigDecimal.valueOf(100))
              .transactionCurrencyCode(986)
              .mcc(5411)
              .consumerAuthenticationScore(100)
              .externalScore3(100)
              .cavvResult(0)
              .eciIndicator(5)
              .atcCard(1)
              .atcHost(1)
              .tokenAssuranceLevel(0)
              .availableCredit(BigDecimal.valueOf(5000))
              .cardCashBalance(BigDecimal.ZERO)
              .cardDelinquentAmount(BigDecimal.ZERO)
              // transactionDate e transactionTime são null
              .build();

      DerivedContext context = DerivedContext.from(request);

      assertThat(context.getOriginalPayload()).isEqualTo(request);
      assertThat(context.getTransactionLocalDate()).isNull();
      assertThat(context.getTransactionLocalTime()).isNull();
      assertThat(context.getTransactionTimestamp()).isNull();
      assertThat(context.getMaskedPan()).isEqualTo("411111******1111"); // PAN ainda funciona
    }
  }

  @Nested
  @DisplayName("toMap - Conversão para Map")
  class ToMapTests {

    @Test
    @DisplayName("Deve converter contexto completo para Map")
    void shouldConvertToMap() {
      TransactionRequest request =
          TransactionRequest.builder()
              .externalTransactionId("TX-003")
              .customerIdFromHeader("CUST-003")
              .customerAcctNumber(123456789L)
              .pan("4111111111111111")
              .transactionDate(20251231)
              .transactionTime(120000)
              .gmtOffset("-0300")
              .transactionAmount(BigDecimal.valueOf(100))
              .transactionCurrencyCode(986)
              .mcc(5411)
              .consumerAuthenticationScore(100)
              .externalScore3(100)
              .cavvResult(0)
              .eciIndicator(5)
              .atcCard(1)
              .atcHost(1)
              .tokenAssuranceLevel(0)
              .availableCredit(BigDecimal.valueOf(5000))
              .cardCashBalance(BigDecimal.ZERO)
              .cardDelinquentAmount(BigDecimal.ZERO)
              .build();

      DerivedContext context = DerivedContext.from(request);
      Map<String, Object> map = context.toMap();

      assertThat(map).containsKey("transactionLocalDate");
      assertThat(map).containsKey("transactionLocalTime");
      assertThat(map).containsKey("transactionTimestamp");
      assertThat(map).containsKey("transaction_ts"); // Alias
      assertThat(map).containsKey("maskedPan");
      assertThat(map).containsKey("masked_pan"); // Alias
      assertThat(map).containsKey("bin");
      assertThat(map).containsKey("cardBin"); // Alias
      assertThat(map).containsKey("last4");
      assertThat(map).containsKey("panLast4"); // Alias
    }

    @Test
    @DisplayName("Deve omitir campos null do Map")
    void shouldOmitNullFieldsFromMap() {
      DerivedContext context = DerivedContext.from(null);
      Map<String, Object> map = context.toMap();

      assertThat(map).isEmpty();
    }
  }
}
