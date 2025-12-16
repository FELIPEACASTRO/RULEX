package com.rulex.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.constraints.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.math.BigDecimal;

/**
 * DTO para requisição de análise de transação.
 * Mapeia todos os parâmetros do JSON de transação de crédito.
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

    @NotNull(message = "transactionCurrencyCode é obrigatório")
    @JsonProperty("transactionCurrencyCode")
    private Integer transactionCurrencyCode;

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

    @JsonProperty("tokenizationIndicator")
    private String tokenizationIndicator;

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

}
