package com.rulex.service;

import com.rulex.dto.TransactionRequest;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import lombok.Builder;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

/**
 * Contexto derivado que extrai tipos ricos do payload sem alterar o payload de entrada.
 *
 * <p>Deriva:
 *
 * <ul>
 *   <li>transactionDate (Integer YYYYMMDD) → LocalDate
 *   <li>transactionTime (Integer HHMMSS) → LocalTime
 *   <li>(date + time + gmtOffset) → OffsetDateTime (transaction_ts)
 *   <li>pan → masked_pan, bin (primeiros 6-8 dígitos), last4
 *   <li>merchantPostalCode → normalized (apenas dígitos)
 *   <li>merchantState → normalized (uppercase)
 *   <li>merchantCountryCode → normalized (3 dígitos)
 * </ul>
 *
 * <p>Todos os métodos têm fallback seguro (retornam Optional.empty() ou valor padrão) para não
 * quebrar o fluxo em caso de erro de parse.
 */
@Data
@Builder
@Slf4j
public class DerivedContext {

  private static final DateTimeFormatter DATE_FORMATTER =
      DateTimeFormatter.BASIC_ISO_DATE; // YYYYMMDD
  private static final DateTimeFormatter TIME_FORMATTER = DateTimeFormatter.ofPattern("HHmmss");

  // Campos derivados
  private final LocalDate transactionLocalDate;
  private final LocalTime transactionLocalTime;
  private final OffsetDateTime transactionTimestamp;
  private final String maskedPan;
  private final String bin;
  private final String last4;
  private final String normalizedPostalCode;
  private final String normalizedState;
  private final String normalizedCountryCode;

  // Payload original (imutável)
  private final TransactionRequest originalPayload;

  /**
   * Cria um DerivedContext a partir do payload original. Erros de parse são logados como WARN e não
   * interrompem o fluxo.
   */
  public static DerivedContext from(TransactionRequest request) {
    if (request == null) {
      log.warn("TransactionRequest é null, retornando contexto vazio");
      return DerivedContext.builder().originalPayload(null).build();
    }

    LocalDate localDate = parseDate(request.getTransactionDate()).orElse(null);
    LocalTime localTime = parseTime(request.getTransactionTime()).orElse(null);
    OffsetDateTime timestamp =
        deriveTimestamp(localDate, localTime, request.getGmtOffset()).orElse(null);

    String pan = request.getPan();
    String masked = maskPan(pan);
    String bin = extractBin(pan);
    String last4 = extractLast4(pan);

    String normalizedPostal = normalizePostalCode(request.getMerchantPostalCode());
    String normalizedState = normalizeState(request.getMerchantState());
    String normalizedCountry = normalizeCountryCode(request.getMerchantCountryCode());

    return DerivedContext.builder()
        .originalPayload(request)
        .transactionLocalDate(localDate)
        .transactionLocalTime(localTime)
        .transactionTimestamp(timestamp)
        .maskedPan(masked)
        .bin(bin)
        .last4(last4)
        .normalizedPostalCode(normalizedPostal)
        .normalizedState(normalizedState)
        .normalizedCountryCode(normalizedCountry)
        .build();
  }

  /**
   * Converte Integer YYYYMMDD para LocalDate.
   *
   * @return Optional.empty() se null ou formato inválido
   */
  public static Optional<LocalDate> parseDate(Integer dateInt) {
    if (dateInt == null) {
      return Optional.empty();
    }
    try {
      String dateStr = String.format("%08d", dateInt);
      return Optional.of(LocalDate.parse(dateStr, DATE_FORMATTER));
    } catch (DateTimeParseException e) {
      log.warn("Falha ao parsear data {}: {}", dateInt, e.getMessage());
      return Optional.empty();
    }
  }

  /**
   * Converte Integer HHMMSS para LocalTime.
   *
   * @return Optional.empty() se null ou formato inválido
   */
  public static Optional<LocalTime> parseTime(Integer timeInt) {
    if (timeInt == null) {
      return Optional.empty();
    }
    try {
      String timeStr = String.format("%06d", timeInt);
      return Optional.of(LocalTime.parse(timeStr, TIME_FORMATTER));
    } catch (DateTimeParseException e) {
      log.warn("Falha ao parsear hora {}: {}", timeInt, e.getMessage());
      return Optional.empty();
    }
  }

  /**
   * Combina LocalDate + LocalTime + gmtOffset para criar OffsetDateTime. gmtOffset esperado no
   * formato "-0300" ou "+0530".
   *
   * @return Optional.empty() se qualquer componente for inválido
   */
  public static Optional<OffsetDateTime> deriveTimestamp(
      LocalDate date, LocalTime time, String gmtOffset) {
    if (date == null || time == null) {
      return Optional.empty();
    }

    ZoneOffset offset = parseGmtOffset(gmtOffset).orElse(ZoneOffset.UTC);

    try {
      return Optional.of(OffsetDateTime.of(date, time, offset));
    } catch (Exception e) {
      log.warn("Falha ao criar OffsetDateTime: {}", e.getMessage());
      return Optional.empty();
    }
  }

  /**
   * Parseia gmtOffset no formato "-0300" ou "+0530" para ZoneOffset.
   *
   * @return Optional.empty() se formato inválido (fallback para UTC)
   */
  public static Optional<ZoneOffset> parseGmtOffset(String gmtOffset) {
    if (gmtOffset == null || gmtOffset.isBlank()) {
      return Optional.empty();
    }

    try {
      // Formato esperado: "-0300" ou "+0530"
      String normalized = gmtOffset.trim();
      if (normalized.length() == 5 && (normalized.startsWith("-") || normalized.startsWith("+"))) {
        int hours = Integer.parseInt(normalized.substring(1, 3));
        int minutes = Integer.parseInt(normalized.substring(3, 5));
        int totalSeconds = (hours * 3600 + minutes * 60) * (normalized.startsWith("-") ? -1 : 1);
        return Optional.of(ZoneOffset.ofTotalSeconds(totalSeconds));
      }
      // Tentar formato ISO (+03:00)
      return Optional.of(ZoneOffset.of(normalized));
    } catch (Exception e) {
      log.warn("Falha ao parsear gmtOffset '{}': {}", gmtOffset, e.getMessage());
      return Optional.empty();
    }
  }

  /**
   * Mascara o PAN mantendo apenas primeiros 6 e últimos 4 dígitos. Ex: "4111111111111111" →
   * "411111******1111"
   */
  public static String maskPan(String pan) {
    if (pan == null || pan.length() < 13) {
      return pan; // Retorna original se inválido
    }
    String digitsOnly = pan.replaceAll("[^0-9]", "");
    if (digitsOnly.length() < 13) {
      return pan;
    }
    int maskLength = digitsOnly.length() - 10; // 6 início + 4 fim
    return digitsOnly.substring(0, 6)
        + "*".repeat(Math.max(0, maskLength))
        + digitsOnly.substring(digitsOnly.length() - 4);
  }

  /**
   * Extrai o BIN (Bank Identification Number) do PAN. Retorna os primeiros 6-8 dígitos dependendo
   * do comprimento.
   */
  public static String extractBin(String pan) {
    if (pan == null) {
      return null;
    }
    String digitsOnly = pan.replaceAll("[^0-9]", "");
    if (digitsOnly.length() < 6) {
      return null;
    }
    // BIN moderno pode ter 8 dígitos, mas 6 é o padrão
    return digitsOnly.substring(0, Math.min(6, digitsOnly.length()));
  }

  /** Extrai os últimos 4 dígitos do PAN. */
  public static String extractLast4(String pan) {
    if (pan == null) {
      return null;
    }
    String digitsOnly = pan.replaceAll("[^0-9]", "");
    if (digitsOnly.length() < 4) {
      return null;
    }
    return digitsOnly.substring(digitsOnly.length() - 4);
  }

  /** Normaliza CEP removendo caracteres não numéricos. Ex: "01310-100" → "01310100" */
  public static String normalizePostalCode(String postalCode) {
    if (postalCode == null) {
      return null;
    }
    return postalCode.replaceAll("[^0-9]", "");
  }

  /** Normaliza estado para uppercase. Ex: "sp" → "SP" */
  public static String normalizeState(String state) {
    if (state == null) {
      return null;
    }
    return state.trim().toUpperCase();
  }

  /** Normaliza código de país para 3 dígitos. Ex: "76" → "076", "076" → "076" */
  public static String normalizeCountryCode(String countryCode) {
    if (countryCode == null) {
      return null;
    }
    String digitsOnly = countryCode.replaceAll("[^0-9]", "");
    if (digitsOnly.isEmpty()) {
      return countryCode; // Pode ser código ISO alpha
    }
    return String.format("%03d", Integer.parseInt(digitsOnly));
  }

  /** Retorna todos os campos derivados como Map para uso no evaluator. */
  public Map<String, Object> toMap() {
    Map<String, Object> map = new HashMap<>();

    if (transactionLocalDate != null) {
      map.put("transactionLocalDate", transactionLocalDate);
    }
    if (transactionLocalTime != null) {
      map.put("transactionLocalTime", transactionLocalTime);
    }
    if (transactionTimestamp != null) {
      map.put("transactionTimestamp", transactionTimestamp);
      map.put("transaction_ts", transactionTimestamp); // Alias
    }
    if (maskedPan != null) {
      map.put("maskedPan", maskedPan);
      map.put("masked_pan", maskedPan); // Alias snake_case
    }
    if (bin != null) {
      map.put("bin", bin);
      map.put("cardBin", bin); // Alias
    }
    if (last4 != null) {
      map.put("last4", last4);
      map.put("panLast4", last4); // Alias
    }
    if (normalizedPostalCode != null) {
      map.put("normalizedPostalCode", normalizedPostalCode);
    }
    if (normalizedState != null) {
      map.put("normalizedState", normalizedState);
    }
    if (normalizedCountryCode != null) {
      map.put("normalizedCountryCode", normalizedCountryCode);
    }

    return map;
  }
}
