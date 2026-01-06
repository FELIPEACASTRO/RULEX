package com.rulex.domain.model;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import lombok.Builder;
import lombok.Value;
import lombok.With;

/**
 * Domain model para dados de Transação.
 *
 * <p>POJO puro sem anotações JPA/Spring. Representa os dados de uma transação financeira para
 * análise de fraude.
 *
 * <p>Imutável por design (usando @Value do Lombok).
 */
@Value
@Builder(toBuilder = true)
@With
public class TransactionData {

  // Identificação
  Long id;
  String externalTransactionId;
  String payloadRawHash;
  String customerIdFromHeader;
  Long customerAcctNumber;
  String pan;
  String merchantId;
  String merchantName;

  // Valores
  BigDecimal transactionAmount;
  Integer transactionDate; // YYYYMMDD
  Integer transactionTime; // HHMMSS
  String gmtOffset;
  Integer transactionCurrencyCode;
  BigDecimal transactionCurrencyConversionRate;

  // Localização
  String merchantCountryCode;
  String merchantCity;
  String merchantState;
  String merchantPostalCode;

  // Categoria
  Integer mcc;
  String posEntryMode;
  String customerPresent;

  // Segurança
  Integer consumerAuthenticationScore;
  Integer externalScore3;
  Integer cavvResult;
  String cryptogramValid;
  String cvv2Response;
  String cvv2Present;
  String pinVerifyCode;
  String cvvVerifyCode;
  Integer eciIndicator;
  Integer atcCard;
  Integer atcHost;
  Integer tokenAssuranceLevel;
  String tokenizationIndicator;

  // Crédito
  BigDecimal availableCredit;
  BigDecimal cardCashBalance;
  BigDecimal cardDelinquentAmount;

  // Metadata
  String workflow;
  String recordType;
  String clientIdFromHeader;
  LocalDateTime createdAt;
  LocalDateTime updatedAt;

  /** Extrai os primeiros 6 dígitos do PAN (BIN) */
  public String getBin() {
    if (pan == null || pan.length() < 6) {
      return null;
    }
    // Remove caracteres não numéricos e pega primeiros 6
    String cleaned = pan.replaceAll("[^0-9]", "");
    return cleaned.length() >= 6 ? cleaned.substring(0, 6) : null;
  }

  /** Retorna PAN mascarado para exibição */
  public String getMaskedPan() {
    if (pan == null || pan.length() < 4) {
      return "****";
    }
    String cleaned = pan.replaceAll("[^0-9*]", "");
    if (cleaned.length() <= 4) {
      return cleaned;
    }
    String last4 = cleaned.substring(cleaned.length() - 4);
    return "****" + last4;
  }

  /** Verifica se é transação de alto valor (> R$ 5.000) */
  public boolean isHighValue() {
    return transactionAmount != null && transactionAmount.compareTo(new BigDecimal("5000")) > 0;
  }

  /** Verifica se é transação internacional */
  public boolean isInternational() {
    return merchantCountryCode != null && !merchantCountryCode.equals("076"); // 076 = Brasil
  }

  /** Verifica se cliente estava presente */
  public boolean isCustomerPresent() {
    return "Y".equalsIgnoreCase(customerPresent);
  }

  /** Verifica se CVV foi validado com sucesso */
  public boolean isCvvValid() {
    return "M".equalsIgnoreCase(cvv2Response); // M = Match
  }
}
