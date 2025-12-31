package com.rulex.entity;

import jakarta.persistence.*;
import java.time.LocalDateTime;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Entidade que armazena configurações dinâmicas de regras. Permite ajustar pesos, thresholds e
 * flags sem necessidade de redeploy.
 */
@Entity
@Table(
    name = "rule_configurations",
    indexes = {
      @Index(name = "idx_rule_name", columnList = "rule_name", unique = true),
      @Index(name = "idx_enabled", columnList = "enabled")
    })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RuleConfiguration {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  /** Nome único da regra */
  @Column(nullable = false, unique = true, length = 100)
  private String ruleName;

  /** Descrição da regra */
  @Column(columnDefinition = "TEXT")
  private String description;

  /** Tipo de regra: SECURITY, CONTEXT, VELOCITY, ANOMALY */
  @Column(nullable = false, length = 20)
  @Enumerated(EnumType.STRING)
  private RuleType ruleType;

  /** Threshold para ativação da regra */
  @Column(nullable = false)
  private Integer threshold;

  /** Peso da regra no cálculo do score (0-100) */
  @Column(nullable = false)
  private Integer weight;

  /** Se a regra está habilitada */
  @Column(nullable = false)
  private Boolean enabled;

  /** Classificação que a regra pode gerar: APPROVED, SUSPICIOUS, FRAUD */
  @Column(nullable = false, length = 20)
  @Enumerated(EnumType.STRING)
  private TransactionDecision.TransactionClassification classification;

  /** Parâmetros adicionais em JSON */
  @Column(columnDefinition = "TEXT")
  private String parameters;

  /** Condições genéricas da regra (JSON array de {field, operator, value}). */
  @Column(columnDefinition = "jsonb")
  private String conditionsJson;

  /** Operador lógico para combinar condições. */
  @Column(length = 3)
  @Enumerated(EnumType.STRING)
  private LogicOperator logicOperator;

  /** Versão da configuração */
  @Column(nullable = false)
  private Integer version;

  /** Timestamps */
  @Column(nullable = false, updatable = false)
  private LocalDateTime createdAt;

  @Column(nullable = false)
  private LocalDateTime updatedAt;

  @PrePersist
  protected void onCreate() {
    if (createdAt == null) {
      createdAt = LocalDateTime.now();
    }
    if (updatedAt == null) {
      updatedAt = createdAt;
    }
    version = 1;
    if (logicOperator == null) {
      logicOperator = LogicOperator.AND;
    }
  }

  @PreUpdate
  protected void onUpdate() {
    if (updatedAt == null) {
      updatedAt = LocalDateTime.now();
    }
  }

  public enum RuleType {
    SECURITY("Segurança"),
    CONTEXT("Contexto"),
    VELOCITY("Velocidade"),
    ANOMALY("Anomalia");

    private final String label;

    RuleType(String label) {
      this.label = label;
    }

    public String getLabel() {
      return label;
    }
  }

  public enum LogicOperator {
    AND,
    OR
  }
}
