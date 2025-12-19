package com.rulex.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.api.NotFoundException;
import com.rulex.dto.RuleConditionDTO;
import com.rulex.dto.RuleConfigurationDTO;
import com.rulex.dto.TransactionRequest;
import com.rulex.entity.RuleConfiguration;
import com.rulex.entity.RuleConfigurationHistory;
import com.rulex.entity.TransactionDecision;
import com.rulex.repository.RuleConfigurationHistoryRepository;
import com.rulex.repository.RuleConfigurationRepository;
import java.beans.Introspector;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/** Serviço para gerenciamento de configurações de regras. */
@Service
@RequiredArgsConstructor
@Slf4j
@Transactional
@SuppressWarnings("null")
public class RuleConfigurationService {

  private final RuleConfigurationRepository ruleConfigRepository;
  private final RuleConfigurationHistoryRepository historyRepository;
  private final AuditService auditService;
  private final ObjectMapper objectMapper;

  /** Lista todas as regras. */
  public Page<RuleConfigurationDTO> listRules(Pageable pageable) {
    Page<RuleConfiguration> rules = ruleConfigRepository.findAll(pageable);
    return rules.map(this::convertToDTO);
  }

  /** Obtém uma regra pelo ID. */
  public RuleConfigurationDTO getRuleById(Long id) {
    RuleConfiguration rule =
        ruleConfigRepository
            .findById(id)
            .orElseThrow(() -> new NotFoundException("Regra não encontrada"));
    return convertToDTO(rule);
  }

  /** Cria uma nova regra. */
  public RuleConfigurationDTO createRule(RuleConfigurationDTO dto) {
    // Verificar se já existe uma regra com este nome
    if (ruleConfigRepository.findByRuleName(dto.getRuleName()).isPresent()) {
      throw new IllegalStateException("Já existe uma regra com este nome");
    }

    validateConditionFields(dto.getConditions());

    RuleConfiguration rule =
        RuleConfiguration.builder()
            .ruleName(dto.getRuleName())
            .description(dto.getDescription())
            .ruleType(RuleConfiguration.RuleType.valueOf(dto.getRuleType()))
            .threshold(dto.getThreshold())
            .weight(dto.getWeight())
            .enabled(dto.getEnabled())
            .classification(
                TransactionDecision.TransactionClassification.valueOf(dto.getClassification()))
            .parameters(dto.getParameters())
            .conditionsJson(writeConditionsJson(dto.getConditions()))
            .logicOperator(parseLogicOperator(dto.getLogicOperator()))
            .build();

    rule = ruleConfigRepository.save(rule);

    historyRepository.save(
        RuleConfigurationHistory.builder()
            .ruleId(rule.getId())
            .ruleName(rule.getRuleName())
            .version(rule.getVersion())
            .previousJson(null)
            .currentJson(serializeRule(rule))
            .performedBy("SYSTEM")
            .build());

    auditService.logRuleCreated(dto.getRuleName(), "SYSTEM");
    log.info("Regra criada: {}", dto.getRuleName());

    return convertToDTO(rule);
  }

  /** Atualiza uma regra existente. */
  public RuleConfigurationDTO updateRule(Long id, RuleConfigurationDTO dto) {
    RuleConfiguration rule =
        ruleConfigRepository
            .findById(id)
            .orElseThrow(() -> new NotFoundException("Regra não encontrada"));

    String previous = serializeRule(rule);

    validateConditionFields(dto.getConditions());

    rule.setDescription(dto.getDescription());
    rule.setThreshold(dto.getThreshold());
    rule.setWeight(dto.getWeight());
    rule.setEnabled(dto.getEnabled());
    rule.setClassification(
        TransactionDecision.TransactionClassification.valueOf(dto.getClassification()));
    rule.setParameters(dto.getParameters());
    rule.setConditionsJson(writeConditionsJson(dto.getConditions()));
    rule.setLogicOperator(parseLogicOperator(dto.getLogicOperator()));
    rule.setVersion(rule.getVersion() + 1);

    rule = ruleConfigRepository.save(rule);

    historyRepository.save(
        RuleConfigurationHistory.builder()
            .ruleId(rule.getId())
            .ruleName(rule.getRuleName())
            .version(rule.getVersion())
            .previousJson(previous)
            .currentJson(serializeRule(rule))
            .performedBy("SYSTEM")
            .build());

    auditService.logRuleUpdated(
        rule.getRuleName(),
        java.util.Map.of(
            "threshold", dto.getThreshold(),
            "weight", dto.getWeight(),
            "enabled", dto.getEnabled()),
        "SYSTEM");

    log.info("Regra atualizada: {}", rule.getRuleName());

    return convertToDTO(rule);
  }

  /** Deleta uma regra. */
  public void deleteRule(Long id) {
    RuleConfiguration rule =
        ruleConfigRepository
            .findById(id)
            .orElseThrow(() -> new NotFoundException("Regra não encontrada"));

    String previous = serializeRule(rule);

    ruleConfigRepository.delete(rule);

    historyRepository.save(
        RuleConfigurationHistory.builder()
            .ruleId(id)
            .ruleName(rule.getRuleName())
            .version(rule.getVersion())
            .previousJson(previous)
            .currentJson(null)
            .performedBy("SYSTEM")
            .build());

    auditService.logRuleDeleted(rule.getRuleName(), "SYSTEM");
    log.info("Regra deletada: {}", rule.getRuleName());
  }

  /** Ativa/desativa uma regra. */
  public RuleConfigurationDTO toggleRule(Long id) {
    RuleConfiguration rule =
        ruleConfigRepository
            .findById(id)
            .orElseThrow(() -> new NotFoundException("Regra não encontrada"));

    String previous = serializeRule(rule);

    rule.setEnabled(!rule.getEnabled());
    rule.setVersion(rule.getVersion() + 1);
    rule = ruleConfigRepository.save(rule);

    historyRepository.save(
        RuleConfigurationHistory.builder()
            .ruleId(rule.getId())
            .ruleName(rule.getRuleName())
            .version(rule.getVersion())
            .previousJson(previous)
            .currentJson(serializeRule(rule))
            .performedBy("SYSTEM")
            .build());

    auditService.logRuleUpdated(
        rule.getRuleName(), java.util.Map.of("enabled", rule.getEnabled()), "SYSTEM");

    log.info("Regra alternada: {} - Habilitada: {}", rule.getRuleName(), rule.getEnabled());

    return convertToDTO(rule);
  }

  /** Lista regras por status de habilitação. */
  public List<RuleConfigurationDTO> listRulesByEnabled(Boolean enabled) {
    return ruleConfigRepository.findByEnabled(enabled).stream()
        .map(this::convertToDTO)
        .collect(Collectors.toList());
  }

  /** Converte RuleConfiguration para DTO. */
  private RuleConfigurationDTO convertToDTO(RuleConfiguration rule) {
    return RuleConfigurationDTO.builder()
        .id(rule.getId())
        .ruleName(rule.getRuleName())
        .description(rule.getDescription())
        .ruleType(rule.getRuleType().name())
        .threshold(rule.getThreshold())
        .weight(rule.getWeight())
        .enabled(rule.getEnabled())
        .classification(rule.getClassification().name())
        .parameters(rule.getParameters())
        .conditions(readConditions(rule.getConditionsJson()))
        .logicOperator(
            (rule.getLogicOperator() != null
                    ? rule.getLogicOperator()
                    : RuleConfiguration.LogicOperator.AND)
                .name())
        .version(rule.getVersion())
        .build();
  }

  private String writeConditionsJson(List<RuleConditionDTO> conditions) {
    try {
      if (conditions == null) {
        return "[]";
      }
      return objectMapper.writeValueAsString(conditions);
    } catch (Exception e) {
      throw new RuntimeException("Condições inválidas", e);
    }
  }

  private void validateConditionFields(List<RuleConditionDTO> conditions) {
    if (conditions == null || conditions.isEmpty()) {
      return;
    }

    final Set<String> allowedFields = getTransactionRequestFieldNames();

    List<String> invalidFields =
        conditions.stream()
            .map(RuleConditionDTO::getField)
            .filter(f -> f != null && !f.isBlank())
            .filter(f -> !allowedFields.contains(f))
            .distinct()
            .toList();

    if (!invalidFields.isEmpty()) {
      throw new IllegalArgumentException(
          "Campos inválidos em conditions.field (não existem no payload TransactionRequest): "
              + String.join(", ", invalidFields));
    }
  }

  private Set<String> getTransactionRequestFieldNames() {
    try {
      return Arrays.stream(
              Introspector.getBeanInfo(TransactionRequest.class).getPropertyDescriptors())
          .map(pd -> pd.getName())
          .filter(name -> !"class".equals(name))
          .collect(Collectors.toUnmodifiableSet());
    } catch (Exception e) {
      throw new IllegalStateException("Falha ao introspectar TransactionRequest", e);
    }
  }

  private List<RuleConditionDTO> readConditions(String conditionsJson) {
    try {
      if (conditionsJson == null || conditionsJson.isBlank()) {
        return List.of();
      }
      return objectMapper.readValue(
          conditionsJson,
          objectMapper
              .getTypeFactory()
              .constructCollectionType(List.class, RuleConditionDTO.class));
    } catch (Exception e) {
      // manter comportamento tolerante para dados legados
      return List.of();
    }
  }

  private RuleConfiguration.LogicOperator parseLogicOperator(String logicOperator) {
    if (logicOperator == null || logicOperator.isBlank()) {
      return RuleConfiguration.LogicOperator.AND;
    }
    return RuleConfiguration.LogicOperator.valueOf(logicOperator);
  }

  private String serializeRule(RuleConfiguration rule) {
    try {
      return objectMapper.writeValueAsString(rule);
    } catch (Exception e) {
      return "{}";
    }
  }

  /** Histórico append-only (mais recente primeiro). */
  @Transactional(readOnly = true)
  public List<RuleConfigurationHistory> getRuleHistory(Long ruleId) {
    return historyRepository.findByRuleIdOrderByCreatedAtDesc(ruleId);
  }
}
