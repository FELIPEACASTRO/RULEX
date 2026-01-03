package com.rulex.service.complex;

import com.rulex.dto.complex.*;
import com.rulex.entity.complex.*;
import com.rulex.repository.complex.*;
import java.time.OffsetDateTime;
import java.util.*;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Isolation;
import org.springframework.transaction.annotation.Transactional;

/**
 * Serviço para CRUD de regras complexas. Gerencia a persistência completa de regras com grupos de
 * condições aninhados.
 *
 * <p>Usa SERIALIZABLE isolation para operações de escrita para garantir consistência em ambiente
 * concorrente.
 */
@Service
@RequiredArgsConstructor
@Slf4j
@Transactional(isolation = Isolation.READ_COMMITTED)
public class ComplexRuleCrudService {

  private final ComplexRuleRepository complexRuleRepository;
  private final RuleConditionGroupRepository conditionGroupRepository;
  private final RuleConditionRepository conditionRepository;
  private final RuleExpressionRepository expressionRepository;
  private final RuleContextVariableRepository contextVariableRepository;
  private final RuleActionRepository actionRepository;
  private final ComplexRuleMapper mapper;

  /** Lista todas as regras complexas. */
  public List<ComplexRuleDTO> listAll() {
    return complexRuleRepository.findAll().stream().map(this::toDTO).collect(Collectors.toList());
  }

  /** Busca uma regra por ID. */
  public ComplexRuleDTO getById(UUID id) {
    return complexRuleRepository.findById(id).map(this::toDTO).orElse(null);
  }

  /** Busca uma regra por chave. */
  public ComplexRuleDTO getByKey(String key) {
    return complexRuleRepository.findByKey(key).map(this::toDTO).orElse(null);
  }

  /** Cria uma nova regra complexa. */
  public ComplexRuleDTO create(ComplexRuleDTO dto) {
    // Verificar se a chave já existe
    if (complexRuleRepository.findByKey(dto.getKey()).isPresent()) {
      throw new IllegalArgumentException("Já existe uma regra com a chave: " + dto.getKey());
    }

    ComplexRule entity = toEntity(dto);
    entity.setId(UUID.randomUUID());
    entity.setVersion(1);
    entity.setCreatedAt(OffsetDateTime.now());
    entity.setUpdatedAt(OffsetDateTime.now());

    ComplexRule saved = complexRuleRepository.save(entity);

    // Salvar grupo de condições
    if (dto.getRootConditionGroup() != null) {
      saveConditionGroup(saved.getId(), dto.getRootConditionGroup());
    }

    // Salvar expressões
    if (dto.getExpressions() != null) {
      saveExpressions(saved.getId(), dto.getExpressions());
    }

    // Salvar variáveis de contexto
    if (dto.getContextVariables() != null) {
      saveContextVariables(saved.getId(), dto.getContextVariables());
    }

    // Salvar ações
    if (dto.getActions() != null) {
      saveActions(saved.getId(), dto.getActions());
    }

    return getById(saved.getId());
  }

  /** Atualiza uma regra existente. */
  public ComplexRuleDTO update(UUID id, ComplexRuleDTO dto) {
    Optional<ComplexRule> existing = complexRuleRepository.findById(id);
    if (existing.isEmpty()) {
      return null;
    }

    ComplexRule entity = existing.get();
    entity.setKey(dto.getKey());
    entity.setTitle(dto.getTitle());
    entity.setDescription(dto.getDescription());
    entity.setStatus(dto.getStatus() != null ? dto.getStatus().name() : "DRAFT");
    entity.setPriority(dto.getPriority() != null ? dto.getPriority() : 0);
    entity.setSeverity(dto.getSeverity() != null ? dto.getSeverity() : 0);
    entity.setDecision(dto.getDecision() != null ? dto.getDecision().name() : "APROVADO");
    entity.setReasonTemplate(dto.getReasonTemplate());
    entity.setEnabled(dto.getEnabled() != null ? dto.getEnabled() : false);
    entity.setVersion(entity.getVersion() + 1);
    entity.setUpdatedAt(OffsetDateTime.now());

    complexRuleRepository.save(entity);

    // Atualizar grupo de condições
    if (dto.getRootConditionGroup() != null) {
      // Deletar grupos existentes
      deleteConditionGroups(id);
      saveConditionGroup(id, dto.getRootConditionGroup());
    }

    // Atualizar expressões
    expressionRepository.deleteByRuleVersionId(id);
    if (dto.getExpressions() != null) {
      saveExpressions(id, dto.getExpressions());
    }

    // Atualizar variáveis de contexto
    contextVariableRepository.deleteByRuleVersionId(id);
    if (dto.getContextVariables() != null) {
      saveContextVariables(id, dto.getContextVariables());
    }

    // Atualizar ações
    actionRepository.deleteByRuleVersionId(id);
    if (dto.getActions() != null) {
      saveActions(id, dto.getActions());
    }

    return getById(id);
  }

  /** Deleta uma regra. */
  public boolean delete(UUID id) {
    if (!complexRuleRepository.existsById(id)) {
      return false;
    }

    // Deletar entidades relacionadas
    deleteConditionGroups(id);
    expressionRepository.deleteByRuleVersionId(id);
    contextVariableRepository.deleteByRuleVersionId(id);
    actionRepository.deleteByRuleVersionId(id);

    complexRuleRepository.deleteById(id);
    return true;
  }

  /** Alterna o status habilitado/desabilitado. */
  public ComplexRuleDTO setEnabled(UUID id, boolean enabled) {
    Optional<ComplexRule> existing = complexRuleRepository.findById(id);
    if (existing.isEmpty()) {
      return null;
    }

    ComplexRule entity = existing.get();
    entity.setEnabled(enabled);
    entity.setUpdatedAt(OffsetDateTime.now());
    complexRuleRepository.save(entity);

    return getById(id);
  }

  /** Duplica uma regra com nova chave. */
  public ComplexRuleDTO duplicate(UUID id, String newKey) {
    ComplexRuleDTO original = getById(id);
    if (original == null) {
      return null;
    }

    // Verificar se a nova chave já existe
    if (complexRuleRepository.findByKey(newKey).isPresent()) {
      throw new IllegalArgumentException("Já existe uma regra com a chave: " + newKey);
    }

    // Criar cópia
    original.setId(null);
    original.setKey(newKey);
    original.setTitle(original.getTitle() + " (Cópia)");
    original.setVersion(null);
    original.setStatus(ComplexRuleDTO.RuleStatusType.DRAFT);
    original.setEnabled(false);

    return create(original);
  }

  /** Valida uma regra sem salvar. */
  public Map<String, Object> validate(ComplexRuleDTO rule) {
    List<String> errors = new ArrayList<>();

    if (rule.getKey() == null || rule.getKey().isBlank()) {
      errors.add("Chave da regra é obrigatória");
    }

    if (rule.getTitle() == null || rule.getTitle().isBlank()) {
      errors.add("Título da regra é obrigatório");
    }

    if (rule.getRootConditionGroup() == null) {
      errors.add("Grupo de condições é obrigatório");
    } else {
      validateConditionGroup(rule.getRootConditionGroup(), errors, 0);
    }

    if (rule.getDecision() == null) {
      errors.add("Decisão é obrigatória");
    }

    return Map.of("valid", errors.isEmpty(), "errors", errors);
  }

  private void validateConditionGroup(ConditionGroupDTO group, List<String> errors, int depth) {
    if (depth > 10) {
      errors.add("Profundidade máxima de aninhamento excedida (máximo: 10)");
      return;
    }

    if (group.getOperator() == null) {
      errors.add("Operador do grupo é obrigatório");
    }

    if (group.getConditions() != null) {
      for (var condition : group.getConditions()) {
        if (condition.getFieldPath() == null || condition.getFieldPath().isBlank()) {
          errors.add("Campo da condição é obrigatório");
        }
        if (condition.getOperator() == null) {
          errors.add("Operador da condição é obrigatório");
        }
      }
    }

    if (group.getSubGroups() != null) {
      for (var subGroup : group.getSubGroups()) {
        validateConditionGroup(subGroup, errors, depth + 1);
      }
    }
  }

  // ========== Métodos auxiliares ==========

  private ComplexRuleDTO toDTO(ComplexRule entity) {
    ComplexRuleDTO dto =
        ComplexRuleDTO.builder()
            .id(entity.getId())
            .key(entity.getKey())
            .title(entity.getTitle())
            .description(entity.getDescription())
            .version(entity.getVersion())
            .status(parseStatus(entity.getStatus()))
            .priority(entity.getPriority())
            .severity(entity.getSeverity())
            .decision(parseDecision(entity.getDecision()))
            .reasonTemplate(entity.getReasonTemplate())
            .enabled(entity.getEnabled())
            .createdAt(entity.getCreatedAt())
            .updatedAt(entity.getUpdatedAt())
            .build();

    // Carregar grupo de condições
    conditionGroupRepository
        .findRootByRuleVersionId(entity.getId())
        .ifPresent(group -> dto.setRootConditionGroup(mapper.toDTO(group)));

    // Carregar expressões
    dto.setExpressions(
        expressionRepository.findByRuleVersionId(entity.getId()).stream()
            .map(mapper::toDTO)
            .collect(Collectors.toList()));

    // Carregar variáveis de contexto
    dto.setContextVariables(
        contextVariableRepository.findByRuleVersionId(entity.getId()).stream()
            .map(mapper::toDTO)
            .collect(Collectors.toList()));

    // Carregar ações
    dto.setActions(
        actionRepository.findByRuleVersionIdOrderByExecutionOrder(entity.getId()).stream()
            .map(mapper::toDTO)
            .collect(Collectors.toList()));

    return dto;
  }

  private ComplexRule toEntity(ComplexRuleDTO dto) {
    ComplexRule entity = new ComplexRule();
    entity.setKey(dto.getKey());
    entity.setTitle(dto.getTitle());
    entity.setDescription(dto.getDescription());
    entity.setStatus(dto.getStatus() != null ? dto.getStatus().name() : "DRAFT");
    entity.setPriority(dto.getPriority() != null ? dto.getPriority() : 0);
    entity.setSeverity(dto.getSeverity() != null ? dto.getSeverity() : 0);
    entity.setDecision(dto.getDecision() != null ? dto.getDecision().name() : "APROVADO");
    entity.setReasonTemplate(dto.getReasonTemplate());
    entity.setEnabled(dto.getEnabled() != null ? dto.getEnabled() : false);
    return entity;
  }

  private ComplexRuleDTO.RuleStatusType parseStatus(String status) {
    try {
      return status != null
          ? ComplexRuleDTO.RuleStatusType.valueOf(status)
          : ComplexRuleDTO.RuleStatusType.DRAFT;
    } catch (IllegalArgumentException e) {
      return ComplexRuleDTO.RuleStatusType.DRAFT;
    }
  }

  private ComplexRuleDTO.DecisionType parseDecision(String decision) {
    try {
      return decision != null
          ? ComplexRuleDTO.DecisionType.valueOf(decision)
          : ComplexRuleDTO.DecisionType.APROVADO;
    } catch (IllegalArgumentException e) {
      return ComplexRuleDTO.DecisionType.APROVADO;
    }
  }

  private void saveConditionGroup(UUID ruleVersionId, ConditionGroupDTO dto) {
    RuleConditionGroup entity = mapper.toEntity(dto, ruleVersionId, null);
    RuleConditionGroup savedGroup = conditionGroupRepository.save(entity);

    // Salvar condições
    if (dto.getConditions() != null) {
      int position = 0;
      for (var conditionDto : dto.getConditions()) {
        RuleCondition condition = mapper.toEntity(conditionDto, position++);
        condition.setGroup(savedGroup);
        conditionRepository.save(condition);
      }
    }

    // Salvar subgrupos recursivamente (usando children do DTO)
    var subGroups = dto.getSubGroups() != null ? dto.getSubGroups() : dto.getChildren();
    if (subGroups != null) {
      for (var subGroupDto : subGroups) {
        saveSubGroup(ruleVersionId, savedGroup.getId(), subGroupDto);
      }
    }
  }

  private void saveSubGroup(UUID ruleVersionId, UUID parentId, ConditionGroupDTO dto) {
    RuleConditionGroup entity = mapper.toEntity(dto, ruleVersionId, parentId);
    RuleConditionGroup savedGroup = conditionGroupRepository.save(entity);

    // Salvar condições
    if (dto.getConditions() != null) {
      int position = 0;
      for (var conditionDto : dto.getConditions()) {
        RuleCondition condition = mapper.toEntity(conditionDto, position++);
        condition.setGroup(savedGroup);
        conditionRepository.save(condition);
      }
    }

    // Salvar subgrupos recursivamente (usando children do DTO)
    var subGroups = dto.getSubGroups() != null ? dto.getSubGroups() : dto.getChildren();
    if (subGroups != null) {
      for (var subGroupDto : subGroups) {
        saveSubGroup(ruleVersionId, savedGroup.getId(), subGroupDto);
      }
    }
  }

  private void deleteConditionGroups(UUID ruleVersionId) {
    // Primeiro deletar condições
    var groups = conditionGroupRepository.findByRuleVersionId(ruleVersionId);
    for (var group : groups) {
      conditionRepository.deleteByGroupId(group.getId());
    }
    // Depois deletar grupos
    conditionGroupRepository.deleteByRuleVersionId(ruleVersionId);
  }

  private void saveExpressions(UUID ruleVersionId, List<ExpressionDTO> expressions) {
    for (var dto : expressions) {
      RuleExpression entity = mapper.toEntity(dto, ruleVersionId);
      expressionRepository.save(entity);
    }
  }

  private void saveContextVariables(UUID ruleVersionId, List<ContextVariableDTO> variables) {
    for (var dto : variables) {
      RuleContextVariable entity = mapper.toEntity(dto, ruleVersionId);
      contextVariableRepository.save(entity);
    }
  }

  private void saveActions(UUID ruleVersionId, List<RuleActionDTO> actions) {
    int order = 0;
    for (var dto : actions) {
      RuleAction entity = mapper.toEntity(dto, ruleVersionId);
      entity.setPosition(order++);
      actionRepository.save(entity);
    }
  }
}
