package com.rulex.infrastructure.persistence;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.application.port.out.RulePersistencePort;
import com.rulex.domain.model.Classification;
import com.rulex.domain.model.Rule;
import com.rulex.domain.model.RuleCondition;
import com.rulex.entity.RuleConfiguration;
import com.rulex.entity.TransactionDecision;
import com.rulex.repository.RuleConfigurationRepository;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Adapter JPA para persistência de regras.
 *
 * <p>Implementa RulePersistencePort convertendo entre Domain Model (Rule) e JPA Entity
 * (RuleConfiguration).
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class RulePersistenceAdapter implements RulePersistencePort {

  private final RuleConfigurationRepository repository;
  private final ObjectMapper objectMapper;

  @Override
  public Optional<Rule> findById(UUID id) {
    // RuleConfiguration usa Long como ID - mapeamos UUID para Long
    try {
      Long longId = Math.abs(id.getLeastSignificantBits() % Long.MAX_VALUE);
      return repository.findById(longId).map(this::toDomain);
    } catch (Exception e) {
      log.warn("Erro ao buscar regra por ID {}: {}", id, e.getMessage());
      return Optional.empty();
    }
  }

  @Override
  public Optional<Rule> findByName(String name) {
    return repository.findByRuleName(name).map(this::toDomain);
  }

  @Override
  public List<Rule> findAllActiveOrderedByPriority() {
    return repository.findByEnabled(true).stream()
        .filter(r -> r.getShadowMode() == RuleConfiguration.ShadowMode.DISABLED)
        .sorted((a, b) -> compareByWeight(b, a)) // Maior peso primeiro
        .map(this::toDomain)
        .toList();
  }

  @Override
  public List<Rule> findActiveByType(Rule.RuleType type) {
    RuleConfiguration.RuleType entityType = toEntityRuleType(type);
    if (entityType == null) return Collections.emptyList();

    return repository.findByEnabledAndRuleType(true, entityType).stream()
        .filter(r -> r.getShadowMode() == RuleConfiguration.ShadowMode.DISABLED)
        .map(this::toDomain)
        .toList();
  }

  @Override
  public List<Rule> findShadowRules() {
    return repository.findByEnabled(true).stream()
        .filter(
            r ->
                r.getShadowMode() == RuleConfiguration.ShadowMode.SHADOW
                    || r.getShadowMode() == RuleConfiguration.ShadowMode.CANARY)
        .map(this::toDomain)
        .toList();
  }

  @Override
  public Rule save(Rule rule) {
    RuleConfiguration entity = toEntity(rule);
    RuleConfiguration saved = repository.save(entity);
    return toDomain(saved);
  }

  @Override
  public void deleteById(UUID id) {
    Long longId = Math.abs(id.getLeastSignificantBits() % Long.MAX_VALUE);
    repository.deleteById(longId);
  }

  @Override
  public long countActive() {
    return repository.findByEnabled(true).stream()
        .filter(r -> r.getShadowMode() == RuleConfiguration.ShadowMode.DISABLED)
        .count();
  }

  // ========== Mapeamento Entity <-> Domain ==========

  private Rule toDomain(RuleConfiguration entity) {
    return Rule.builder()
        .id(UUID.randomUUID()) // Entity usa Long, geramos UUID para domain
        .name(entity.getRuleName())
        .description(entity.getDescription())
        .type(toDomainRuleType(entity.getRuleType()))
        .weight(entity.getWeight())
        .threshold(entity.getThreshold())
        .enabled(entity.getEnabled())
        .classification(toDomainClassification(entity.getClassification()))
        .conditions(parseConditions(entity.getConditionsJson()))
        .logicOperator(toDomainLogicOperator(entity.getLogicOperator()))
        .shadowMode(toDomainShadowMode(entity.getShadowMode()))
        .canaryPercentage(entity.getCanaryPercentage())
        .advanced(entity.getAdvanced())
        .createdAt(entity.getCreatedAt())
        .updatedAt(entity.getUpdatedAt())
        .build();
  }

  private RuleConfiguration toEntity(Rule rule) {
    return RuleConfiguration.builder()
        .ruleName(rule.getName())
        .description(rule.getDescription())
        .ruleType(toEntityRuleType(rule.getType()))
        .weight(rule.getWeight())
        .threshold(rule.getThreshold())
        .enabled(rule.getEnabled())
        .classification(toEntityClassification(rule.getClassification()))
        .conditionsJson(serializeConditions(rule.getConditions()))
        .logicOperator(toEntityLogicOperator(rule.getLogicOperator()))
        .shadowMode(toEntityShadowMode(rule.getShadowMode()))
        .canaryPercentage(rule.getCanaryPercentage())
        .advanced(rule.getAdvanced())
        .build();
  }

  // ========== Conversões de Enum ==========

  private Rule.RuleType toDomainRuleType(RuleConfiguration.RuleType type) {
    if (type == null) return Rule.RuleType.SECURITY;
    return switch (type) {
      case SECURITY -> Rule.RuleType.SECURITY;
      case VELOCITY -> Rule.RuleType.VELOCITY;
      case ANOMALY -> Rule.RuleType.ANOMALY;
      case CONTEXT -> Rule.RuleType.CONTEXT;
    };
  }

  private RuleConfiguration.RuleType toEntityRuleType(Rule.RuleType type) {
    if (type == null) return RuleConfiguration.RuleType.SECURITY;
    return switch (type) {
      case SECURITY -> RuleConfiguration.RuleType.SECURITY;
      case VELOCITY -> RuleConfiguration.RuleType.VELOCITY;
      case ANOMALY -> RuleConfiguration.RuleType.ANOMALY;
      case CONTEXT -> RuleConfiguration.RuleType.CONTEXT;
    };
  }

  private Classification toDomainClassification(
      TransactionDecision.TransactionClassification classification) {
    if (classification == null) return Classification.APPROVED;
    return switch (classification) {
      case APPROVED -> Classification.APPROVED;
      case SUSPICIOUS -> Classification.SUSPICIOUS;
      case FRAUD -> Classification.FRAUD;
    };
  }

  private TransactionDecision.TransactionClassification toEntityClassification(
      Classification classification) {
    if (classification == null) return TransactionDecision.TransactionClassification.APPROVED;
    return switch (classification) {
      case APPROVED -> TransactionDecision.TransactionClassification.APPROVED;
      case SUSPICIOUS -> TransactionDecision.TransactionClassification.SUSPICIOUS;
      case FRAUD -> TransactionDecision.TransactionClassification.FRAUD;
    };
  }

  private Rule.LogicOperator toDomainLogicOperator(RuleConfiguration.LogicOperator op) {
    if (op == null) return Rule.LogicOperator.AND;
    return switch (op) {
      case AND -> Rule.LogicOperator.AND;
      case OR -> Rule.LogicOperator.OR;
    };
  }

  private RuleConfiguration.LogicOperator toEntityLogicOperator(Rule.LogicOperator op) {
    if (op == null) return RuleConfiguration.LogicOperator.AND;
    return switch (op) {
      case AND -> RuleConfiguration.LogicOperator.AND;
      case OR -> RuleConfiguration.LogicOperator.OR;
    };
  }

  private Rule.ShadowMode toDomainShadowMode(RuleConfiguration.ShadowMode mode) {
    if (mode == null) return Rule.ShadowMode.DISABLED;
    return switch (mode) {
      case DISABLED -> Rule.ShadowMode.DISABLED;
      case SHADOW -> Rule.ShadowMode.SHADOW;
      case CANARY -> Rule.ShadowMode.CANARY;
    };
  }

  private RuleConfiguration.ShadowMode toEntityShadowMode(Rule.ShadowMode mode) {
    if (mode == null) return RuleConfiguration.ShadowMode.DISABLED;
    return switch (mode) {
      case DISABLED -> RuleConfiguration.ShadowMode.DISABLED;
      case SHADOW -> RuleConfiguration.ShadowMode.SHADOW;
      case CANARY -> RuleConfiguration.ShadowMode.CANARY;
    };
  }

  // ========== JSON Parsing ==========

  private List<RuleCondition> parseConditions(String json) {
    if (json == null || json.isBlank()) return Collections.emptyList();
    try {
      List<ConditionDto> dtos =
          objectMapper.readValue(json, new TypeReference<List<ConditionDto>>() {});
      return dtos.stream()
          .map(dto -> new RuleCondition(dto.field(), dto.operator(), dto.value()))
          .toList();
    } catch (Exception e) {
      log.warn("Erro ao parsear condições JSON: {}", e.getMessage());
      return Collections.emptyList();
    }
  }

  private String serializeConditions(List<RuleCondition> conditions) {
    if (conditions == null || conditions.isEmpty()) return null;
    try {
      List<ConditionDto> dtos =
          conditions.stream()
              .map(c -> new ConditionDto(c.getField(), c.getOperator(), c.getValue()))
              .toList();
      return objectMapper.writeValueAsString(dtos);
    } catch (Exception e) {
      log.warn("Erro ao serializar condições: {}", e.getMessage());
      return null;
    }
  }

  private record ConditionDto(String field, String operator, String value) {}

  private int compareByWeight(RuleConfiguration a, RuleConfiguration b) {
    int wa = a.getWeight() != null ? a.getWeight() : 0;
    int wb = b.getWeight() != null ? b.getWeight() : 0;
    return Integer.compare(wa, wb);
  }
}
