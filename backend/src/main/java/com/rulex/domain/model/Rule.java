package com.rulex.domain.model;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;
import lombok.Builder;
import lombok.Value;
import lombok.With;

/**
 * Domain model para Regra de Fraude.
 *
 * <p>POJO puro sem anotações JPA/Spring. Representa uma regra de negócio para detecção de fraude.
 *
 * <p>Imutável por design (usando @Value do Lombok). Use @With para criar cópias modificadas.
 */
@Value
@Builder(toBuilder = true)
@With
public class Rule {

  UUID id;
  String name;
  String description;
  RuleType type;
  Integer threshold;
  Integer weight;
  Boolean enabled;
  Classification classification;
  List<RuleCondition> conditions;
  LogicOperator logicOperator;
  Boolean advanced;
  ShadowMode shadowMode;
  Integer canaryPercentage;
  Integer version;
  LocalDateTime createdAt;
  LocalDateTime updatedAt;

  /** Tipo de regra */
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

  /** Operador lógico para combinar condições */
  public enum LogicOperator {
    AND,
    OR
  }

  /** Modo shadow para testes seguros em produção */
  public enum ShadowMode {
    /** Regra ativa e afeta decisões */
    DISABLED,
    /** Regra avaliada mas não afeta decisões */
    SHADOW,
    /** Regra avaliada para percentual do tráfego */
    CANARY
  }

  /** Verifica se a regra está ativa (não em shadow mode) */
  public boolean isActive() {
    return enabled != null
        && enabled
        && (shadowMode == null || shadowMode == ShadowMode.DISABLED);
  }

  /** Verifica se a regra deve ser avaliada em modo shadow */
  public boolean isShadowOnly() {
    return shadowMode == ShadowMode.SHADOW;
  }

  /** Verifica se a regra está em modo canary */
  public boolean isCanary() {
    return shadowMode == ShadowMode.CANARY;
  }

  /** Retorna percentual do canary (0-100) */
  public int getEffectiveCanaryPercentage() {
    return canaryPercentage != null ? canaryPercentage : 0;
  }
}
