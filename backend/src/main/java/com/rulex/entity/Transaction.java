package com.rulex.entity;

import jakarta.persistence.*;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Entidade que representa uma transação de crédito a ser analisada. Mapeia todos os parâmetros
 * relevantes do JSON de transação.
 */
@Entity
@Table(
    name = "transactions",
    indexes = {
      @Index(name = "idx_customer_id", columnList = "customer_id_from_header"),
      @Index(name = "idx_merchant_id", columnList = "merchant_id"),
      @Index(name = "idx_transaction_date", columnList = "transaction_date"),
      @Index(
          name = "idx_external_transaction_id",
          columnList = "external_transaction_id",
          unique = true)
    })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Transaction {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  // Identificação
  @Column(nullable = false, unique = true, length = 64)
  private String externalTransactionId;

  @Column(nullable = false, length = 64)
  private String customerIdFromHeader;

  @Column(nullable = false)
  private Long customerAcctNumber;

  @Column(nullable = false, length = 64)
  private String pan; // Tokenizado ou mascarado

  @Column(length = 64)
  private String merchantId;

  @Column(length = 255)
  private String merchantName;

  // Valores e Datas
  @Column(nullable = false)
  private BigDecimal transactionAmount;

  @Column(nullable = false)
  private Integer transactionDate; // YYYYMMDD

  @Column(nullable = false)
  private Integer transactionTime; // HHMMSS

  @Column(length = 10)
  private String gmtOffset;

  @Column(nullable = false)
  private Integer transactionCurrencyCode; // 986 = BRL

  @Column(precision = 10, scale = 4)
  private BigDecimal transactionCurrencyConversionRate;

  // Localização
  @Column(length = 10)
  private String merchantCountryCode;

  @Column(length = 255)
  private String merchantCity;

  @Column(length = 2)
  private String merchantState;

  @Column(length = 20)
  private String merchantPostalCode;

  // Categoria e Tipo
  @Column(nullable = false)
  private Integer mcc; // Merchant Category Code

  @Column(length = 1)
  private String posEntryMode; // E, C, etc.

  @Column(length = 1)
  private String customerPresent; // Y/N

  // POS / Terminal (CRTRAN25)
  @Column private Integer posOffPremises;

  @Column private Integer posCardCapture;

  @Column private Integer posSecurity;

  // PIN/CVV (CRTRAN25)
  @Column private Integer cvvPinTryLimitExceeded;

  @Column private Integer cvrofflinePinVerificationPerformed;

  @Column private Integer cvrofflinePinVerificationFailed;

  // Card media
  @Column(length = 1)
  private String cardMediaType;

  // Segurança e Autenticação
  @Column(nullable = false)
  private Integer consumerAuthenticationScore;

  @Column(nullable = false)
  private Integer externalScore3;

  @Column(nullable = false)
  private Integer cavvResult;

  @Column(length = 1)
  private String cryptogramValid; // V = Válido

  @Column(length = 1)
  private String cvv2Response; // M = Match, N = No Match

  @Column(length = 1)
  private String cvv2Present;

  @Column(length = 1)
  private String pinVerifyCode;

  @Column(length = 1)
  private String cvvVerifyCode;

  @Column(nullable = false)
  private Integer eciIndicator;

  @Column(nullable = false)
  private Integer atcCard;

  @Column(nullable = false)
  private Integer atcHost;

  @Column(nullable = false)
  private Integer tokenAssuranceLevel;

  @Column(length = 1)
  private String tokenizationIndicator;

  // Crédito
  @Column(nullable = false)
  private BigDecimal availableCredit;

  @Column(nullable = false)
  private BigDecimal cardCashBalance;

  @Column(nullable = false)
  private BigDecimal cardDelinquentAmount;

  // Metadata
  @Column(length = 1)
  private String workflow;

  @Column(length = 20)
  private String recordType;

  @Column(length = 64)
  private String clientIdFromHeader;

  // Timestamps
  @Column(nullable = false, updatable = false)
  private LocalDateTime createdAt;

  @Column(nullable = false)
  private LocalDateTime updatedAt;

  @PrePersist
  protected void onCreate() {
    createdAt = LocalDateTime.now();
    updatedAt = LocalDateTime.now();
  }

  @PreUpdate
  protected void onUpdate() {
    updatedAt = LocalDateTime.now();
  }
}
