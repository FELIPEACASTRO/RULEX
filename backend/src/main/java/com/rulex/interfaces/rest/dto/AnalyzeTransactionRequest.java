package com.rulex.interfaces.rest.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import jakarta.validation.constraints.DecimalMin;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import java.math.BigDecimal;
import java.util.Map;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Request DTO for transaction analysis.
 *
 * <p>This DTO captures all fields needed for fraud analysis while maintaining compatibility with
 * the existing API contract.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude(JsonInclude.Include.NON_NULL)
public class AnalyzeTransactionRequest {

  /** External transaction identifier from the source system. */
  @NotBlank(message = "externalTransactionId é obrigatório")
  private String externalTransactionId;

  /** Customer identifier. */
  @NotBlank(message = "customerId é obrigatório")
  private String customerId;

  /** Merchant identifier. */
  private String merchantId;

  /** Merchant name. */
  private String merchantName;

  /** Merchant Category Code. */
  private Integer mcc;

  /** Transaction amount. */
  @NotNull(message = "amount é obrigatório")
  @DecimalMin(value = "0.01", message = "amount deve ser maior que zero")
  private BigDecimal amount;

  /** Currency code (ISO 4217). */
  @Builder.Default private String currency = "BRL";

  /** Source country code (ISO 3166-1 alpha-2). */
  private String country;

  /** Channel through which transaction was initiated. */
  private String channel;

  /** Device identifier (for digital channels). */
  private String deviceId;

  /** Device fingerprint hash. */
  private String deviceFingerprint;

  /** IP address of the transaction origin. */
  private String ipAddress;

  /** Geographic coordinates. */
  private Double latitude;

  private Double longitude;

  /** Email associated with the transaction. */
  private String email;

  /** Phone number. */
  private String phone;

  /** Card BIN (first 6-8 digits). */
  private String cardBin;

  /** Card type (credit, debit, prepaid). */
  private String cardType;

  /** Is this a recurring transaction? */
  @Builder.Default private Boolean isRecurring = false;

  /** Is this a 3DS authenticated transaction? */
  @Builder.Default private Boolean is3dsAuthenticated = false;

  /** Additional custom fields for rule evaluation. */
  private Map<String, Object> customFields;
}
