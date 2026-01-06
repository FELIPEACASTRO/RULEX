package com.rulex.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.constraints.*;
import java.math.BigDecimal;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * DTO para requisição de análise de transação. Mapeia todos os parâmetros do JSON de transação de
 * crédito.
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class TransactionRequest {

  @NotBlank(message = "externalTransactionId é obrigatório")
  @JsonProperty("externalTransactionId")
  private String externalTransactionId;

  @NotBlank(message = "customerIdFromHeader é obrigatório")
  @JsonProperty("customerIdFromHeader")
  private String customerIdFromHeader;

  @NotNull(message = "customerAcctNumber é obrigatório")
  @JsonProperty("customerAcctNumber")
  private Long customerAcctNumber;

  @NotBlank(message = "pan é obrigatório")
  @JsonProperty("pan")
  private String pan;

  @JsonProperty("merchantId")
  private String merchantId;

  @JsonProperty("merchantName")
  private String merchantName;

  @NotNull(message = "transactionCurrencyCode é obrigatório")
  @JsonProperty("transactionCurrencyCode")
  private Integer transactionCurrencyCode;

  @JsonProperty("dataSpecificationVersion")
  private BigDecimal dataSpecificationVersion;

  @JsonProperty("authPostFlag")
  private String authPostFlag;

  @JsonProperty("cardSeqNum")
  private Integer cardSeqNum;

  @JsonProperty("cardExpireDate")
  private Integer cardExpireDate;

  @JsonProperty("authDecisionCode")
  private String authDecisionCode;

  @JsonProperty("transactionType")
  private String transactionType;

  @JsonProperty("transactionCategory")
  private String transactionCategory;

  @JsonProperty("atmOwner")
  private String atmOwner;

  @JsonProperty("acquirerBin")
  private String acquirerBin;

  @JsonProperty("userIndicator01")
  private String userIndicator01;

  @JsonProperty("userIndicator03")
  private String userIndicator03;

  @JsonProperty("userIndicator04")
  private String userIndicator04;

  @JsonProperty("userIndicator05")
  private String userIndicator05;

  @JsonProperty("userIndicator08")
  private String userIndicator08;

  @JsonProperty("idMethod")
  private Integer idMethod;

  @JsonProperty("tokenId")
  private String tokenId;

  @JsonProperty("cardAipStatic")
  private String cardAipStatic;

  @JsonProperty("cardAipDynamic")
  private String cardAipDynamic;

  @JsonProperty("cardAipVerify")
  private String cardAipVerify;

  @JsonProperty("cardAipRisk")
  private String cardAipRisk;

  @JsonProperty("cardAipIssuerAuthentication")
  private String cardAipIssuerAuthentication;

  @JsonProperty("cardAipCombined")
  private String cardAipCombined;

  @JsonProperty("cardMediaType")
  private String cardMediaType;

  @JsonProperty("tokenRequestorId")
  private String tokenRequestorId;

  @JsonProperty("tokenizationIndicator")
  private String tokenizationIndicator;

  @JsonProperty("paymentInstrumentId")
  private String paymentInstrumentId;

  @JsonProperty("acquirerId")
  private String acquirerId;

  @JsonProperty("acquirerCountry")
  private String acquirerCountry;

  @JsonProperty("terminalId")
  private String terminalId;

  @JsonProperty("terminalType")
  private String terminalType;

  @JsonProperty("terminalEntryCapability")
  private String terminalEntryCapability;

  @JsonProperty("posConditionCode")
  private String posConditionCode;

  @JsonProperty("networkId")
  private String networkId;

  @JsonProperty("terminalVerificationResults")
  private String terminalVerificationResults;

  @JsonProperty("cardVerificationResults")
  private String cardVerificationResults;

  @JsonProperty("authIndicator")
  private Integer authIndicator;

  @JsonProperty("secondFactorAuthCode")
  private String secondFactorAuthCode;

  @JsonProperty("cavvKeyIndicator")
  private Integer cavvKeyIndicator;

  @JsonProperty("processorAuthReasonCode")
  private String processorAuthReasonCode;

  @JsonProperty("standinAdvice")
  private String standinAdvice;

  @JsonProperty("avsRequest")
  private String avsRequest;

  @JsonProperty("cvrofflinePinVerificationPerformed")
  private Integer cvrofflinePinVerificationPerformed;

  @JsonProperty("cvrofflinePinVerificationFailed")
  private Integer cvrofflinePinVerificationFailed;

  @JsonProperty("cvvPinTryLimitExceeded")
  private Integer cvvPinTryLimitExceeded;

  @JsonProperty("posOffPremises")
  private Integer posOffPremises;

  @JsonProperty("posCardCapture")
  private Integer posCardCapture;

  @JsonProperty("posSecurity")
  private Integer posSecurity;

  @JsonProperty("authResponseCode")
  private String authResponseCode;

  @JsonProperty("authId")
  private String authId;

  @JsonProperty("checkNumber")
  private String checkNumber;

  @JsonProperty("recordCreationDate")
  private Integer recordCreationDate;

  @JsonProperty("recordCreationTime")
  private Integer recordCreationTime;

  @JsonProperty("recordCreationMilliseconds")
  private Integer recordCreationMilliseconds;

  @JsonProperty("userData01")
  private String userData01;

  @JsonProperty("userData02")
  private String userData02;

  @JsonProperty("userData03")
  private String userData03;

  @JsonProperty("userData04")
  private String userData04;

  @JsonProperty("userData05")
  private String userData05;

  @JsonProperty("userData06")
  private String userData06;

  @JsonProperty("userData06_2")
  private String userData06_2;

  @JsonProperty("userData09")
  private String userData09;

  @JsonProperty("portfolio")
  private String portfolio;

  @JsonProperty("onUsMerchantId")
  private String onUsMerchantId;

  @JsonProperty("expandedBIN")
  private String expandedBIN;

  @JsonProperty("tranCode")
  private String tranCode;

  @NotNull(message = "transactionAmount é obrigatório")
  @DecimalMin(value = "0.0", inclusive = false, message = "transactionAmount deve ser maior que 0")
  @JsonProperty("transactionAmount")
  private BigDecimal transactionAmount;

  @NotNull(message = "transactionDate é obrigatório")
  @JsonProperty("transactionDate")
  private Integer transactionDate;

  @NotNull(message = "transactionTime é obrigatório")
  @JsonProperty("transactionTime")
  private Integer transactionTime;

  @JsonProperty("gmtOffset")
  private String gmtOffset;

  @JsonProperty("transactionCurrencyConversionRate")
  private BigDecimal transactionCurrencyConversionRate;

  @JsonProperty("merchantCountryCode")
  private String merchantCountryCode;

  @JsonProperty("merchantCity")
  private String merchantCity;

  @JsonProperty("merchantState")
  private String merchantState;

  @JsonProperty("merchantPostalCode")
  private String merchantPostalCode;

  @NotNull(message = "mcc é obrigatório")
  @JsonProperty("mcc")
  private Integer mcc;

  @JsonProperty("posEntryMode")
  private String posEntryMode;

  @JsonProperty("customerPresent")
  private String customerPresent;

  @NotNull(message = "consumerAuthenticationScore é obrigatório")
  @Min(value = 0, message = "consumerAuthenticationScore deve ser >= 0")
  @Max(value = 999, message = "consumerAuthenticationScore deve ser <= 999")
  @JsonProperty("consumerAuthenticationScore")
  private Integer consumerAuthenticationScore;

  @NotNull(message = "externalScore3 é obrigatório")
  @Min(value = 0, message = "externalScore3 deve ser >= 0")
  @Max(value = 999, message = "externalScore3 deve ser <= 999")
  @JsonProperty("externalScore3")
  private Integer externalScore3;

  @NotNull(message = "cavvResult é obrigatório")
  @JsonProperty("cavvResult")
  private Integer cavvResult;

  @JsonProperty("cryptogramValid")
  private String cryptogramValid;

  @JsonProperty("cvv2Response")
  private String cvv2Response;

  @JsonProperty("cvv2Present")
  private String cvv2Present;

  @JsonProperty("pinVerifyCode")
  private String pinVerifyCode;

  @JsonProperty("cvvVerifyCode")
  private String cvvVerifyCode;

  @NotNull(message = "eciIndicator é obrigatório")
  @JsonProperty("eciIndicator")
  private Integer eciIndicator;

  @NotNull(message = "atcCard é obrigatório")
  @JsonProperty("atcCard")
  private Integer atcCard;

  @NotNull(message = "atcHost é obrigatório")
  @JsonProperty("atcHost")
  private Integer atcHost;

  @NotNull(message = "tokenAssuranceLevel é obrigatório")
  @JsonProperty("tokenAssuranceLevel")
  private Integer tokenAssuranceLevel;

  @NotNull(message = "availableCredit é obrigatório")
  @JsonProperty("availableCredit")
  private BigDecimal availableCredit;

  @NotNull(message = "cardCashBalance é obrigatório")
  @JsonProperty("cardCashBalance")
  private BigDecimal cardCashBalance;

  @NotNull(message = "cardDelinquentAmount é obrigatório")
  @JsonProperty("cardDelinquentAmount")
  private BigDecimal cardDelinquentAmount;

  @JsonProperty("workflow")
  private String workflow;

  @JsonProperty("recordType")
  private String recordType;

  @JsonProperty("clientIdFromHeader")
  private String clientIdFromHeader;

  // =========================================================================
  // CAMPOS ADICIONAIS PARA REGRAS DE FRAUDE AVANÇADAS
  // =========================================================================

  // --- Dados do Cliente/Beneficiário ---

  /** País do cliente (código ISO 2 ou 3 letras) */
  @JsonProperty("customerCountry")
  private String customerCountry;

  /** Email do cliente */
  @JsonProperty("customerEmail")
  private String customerEmail;

  /** Telefone do cliente */
  @JsonProperty("customerPhone")
  private String customerPhone;

  /** País do beneficiário em transferências (código ISO) */
  @JsonProperty("beneficiaryCountry")
  private String beneficiaryCountry;

  /** ID do beneficiário em transferências */
  @JsonProperty("beneficiaryId")
  private String beneficiaryId;

  /** Nome impresso no cartão */
  @JsonProperty("cardholderName")
  private String cardholderName;

  /** Endereço de cobrança completo */
  @JsonProperty("billingAddress")
  private String billingAddress;

  /** Renda declarada do cliente */
  @JsonProperty("declaredIncome")
  private BigDecimal declaredIncome;

  // --- Dados de Dispositivo/Sessão ---

  /** ID único do dispositivo (fingerprint) */
  @JsonProperty("deviceId")
  private String deviceId;

  /** User Agent do navegador/app */
  @JsonProperty("userAgent")
  private String userAgent;

  /** Score de reputação do IP (0-100, maior = melhor) */
  @JsonProperty("ipReputationScore")
  private Integer ipReputationScore;

  // --- Dados de Autenticação/Verificação ---

  /** Se MFA foi solicitado nesta transação */
  @JsonProperty("mfaRequested")
  private Boolean mfaRequested;

  /** Se MFA foi completado com sucesso */
  @JsonProperty("mfaCompleted")
  private Boolean mfaCompleted;

  /** Se passou na verificação de vivacidade (liveness check) */
  @JsonProperty("livenessCheckPassed")
  private Boolean livenessCheckPassed;

  /** Score de verificação de identidade (0-100) */
  @JsonProperty("identityVerificationScore")
  private Integer identityVerificationScore;

  /** Score de detecção de deepfake (0-100, maior = mais provável ser fake) */
  @JsonProperty("deepfakeScore")
  private Integer deepfakeScore;

  /** Score de correspondência de nome (0-100) */
  @JsonProperty("nameMatchScore")
  private Integer nameMatchScore;

  // --- Dados de Entrega ---

  /** Método de envio (STANDARD, EXPRESS, OVERNIGHT, SAME_DAY, PICKUP) */
  @JsonProperty("shippingMethod")
  private String shippingMethod;

  /** Nome do destinatário no endereço de entrega */
  @JsonProperty("shippingName")
  private String shippingName;

  /** Endereço de entrega completo */
  @JsonProperty("shippingAddress")
  private String shippingAddress;

  // --- Dados Derivados/Calculados ---

  /** Diferença de idade entre dados (ex: idade do cartão vs idade da conta) */
  @JsonProperty("ageDifference")
  private Integer ageDifference;
}
