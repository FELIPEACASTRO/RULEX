package com.rulex.domain.model;

import java.time.LocalDateTime;
import java.util.List;
import lombok.Builder;
import lombok.Value;
import lombok.With;

/**
 * Domain model para Decisão de Análise de Fraude.
 *
 * <p>POJO puro sem anotações JPA/Spring. Representa o resultado da análise de uma transação.
 *
 * <p>Imutável por design (usando @Value do Lombok).
 */
@Value
@Builder(toBuilder = true)
@With
public class Decision {

  Long id;
  String externalTransactionId;
  String payloadRawHash;
  Classification classification;
  Integer riskScore;
  List<TriggeredRule> triggeredRules;
  String reason;
  String rulesVersion;
  Long processingTimeMs;
  LocalDateTime createdAt;

  /** Regra que foi acionada durante a análise */
  @Value
  @Builder
  public static class TriggeredRule {
    String ruleName;
    String ruleType;
    Integer weight;
    Integer contribution;
    String detail;
    Classification classification;
  }

  /** Verifica se a decisão é de bloqueio (FRAUD) */
  public boolean isBlocked() {
    return classification == Classification.FRAUD;
  }

  /** Verifica se a decisão requer análise manual */
  public boolean requiresManualReview() {
    return classification == Classification.SUSPICIOUS;
  }

  /** Verifica se foi aprovada */
  public boolean isApproved() {
    return classification == Classification.APPROVED;
  }

  /** Retorna quantidade de regras acionadas */
  public int getTriggeredRulesCount() {
    return triggeredRules != null ? triggeredRules.size() : 0;
  }

  /** Verifica se processamento foi rápido (< 200ms) */
  public boolean isFastProcessing() {
    return processingTimeMs != null && processingTimeMs < 200;
  }

  /** Cria decisão de aprovação */
  public static Decision approved(String externalTransactionId, long processingTimeMs) {
    return Decision.builder()
        .externalTransactionId(externalTransactionId)
        .classification(Classification.APPROVED)
        .riskScore(0)
        .triggeredRules(List.of())
        .reason("Nenhuma regra acionada")
        .processingTimeMs(processingTimeMs)
        .createdAt(LocalDateTime.now())
        .build();
  }

  /** Cria decisão de fraude (anti-tamper) */
  public static Decision tamperDetected(String externalTransactionId, String payloadHash) {
    return Decision.builder()
        .externalTransactionId(externalTransactionId)
        .payloadRawHash(payloadHash)
        .classification(Classification.FRAUD)
        .riskScore(100)
        .triggeredRules(
            List.of(
                TriggeredRule.builder()
                    .ruleName("ANTI_TAMPER")
                    .ruleType("SECURITY")
                    .weight(100)
                    .contribution(100)
                    .detail("Hash do payload não confere com original")
                    .classification(Classification.FRAUD)
                    .build()))
        .reason("Tentativa de adulteração detectada")
        .createdAt(LocalDateTime.now())
        .build();
  }
}
