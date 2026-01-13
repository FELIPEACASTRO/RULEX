package com.rulex.service.complex;

import com.rulex.dto.complex.*;
import com.rulex.entity.complex.*;
import com.rulex.repository.complex.*;
import jakarta.transaction.Transactional;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

/** Serviço para gerenciamento de regras complexas. */
@Service
@RequiredArgsConstructor
@Slf4j
public class ComplexRuleService {

  private final RuleConditionGroupRepository conditionGroupRepository;
  private final RuleConditionRepository conditionRepository;
  private final RuleExpressionRepository expressionRepository;
  private final RuleContextVariableRepository contextVariableRepository;
  private final RuleActionRepository actionRepository;
  private final RuleTemplateRepository templateRepository;
  private final ComplexRuleMapper mapper;
  private final RuleValidationService validationService;

  /** Salva a estrutura completa de condições de uma regra */
  @Transactional
  public ConditionGroupDTO saveConditionGroup(UUID ruleVersionId, ConditionGroupDTO dto) {
    log.info("Salvando grupo de condições para ruleVersionId: {}", ruleVersionId);

    // Validar regra antes de persistir
    RuleValidationService.ValidationResult validation =
        validationService.validateConditionGroup(dto);
    if (!validation.valid()) {
      String errorMsg = String.join("; ", validation.errors());
      log.error("Validação de regra falhou: {}", errorMsg);
      throw new IllegalArgumentException("Regra inválida: " + errorMsg);
    }

    // Deletar grupos existentes
    conditionGroupRepository.deleteByRuleVersionId(ruleVersionId);

    // Criar novo grupo raiz
    RuleConditionGroup rootGroup = mapper.toEntity(dto, ruleVersionId, null);
    rootGroup = saveGroupRecursively(rootGroup);

    log.info("Grupo de condições salvo com sucesso. ID: {}", rootGroup.getId());
    return mapper.toDTO(rootGroup);
  }

  /** Salva um grupo e seus filhos recursivamente */
  private RuleConditionGroup saveGroupRecursively(RuleConditionGroup group) {
    // Salvar o grupo primeiro
    group = conditionGroupRepository.save(group);

    // Salvar condições
    if (group.getConditions() != null) {
      for (RuleCondition condition : group.getConditions()) {
        condition.setGroup(group);
      }
      conditionRepository.saveAll(group.getConditions());
    }

    // Salvar grupos filhos recursivamente
    if (group.getChildren() != null) {
      List<RuleConditionGroup> savedChildren = new ArrayList<>();
      for (RuleConditionGroup child : group.getChildren()) {
        child.setParentGroupId(group.getId());
        child.setRuleVersionId(group.getRuleVersionId());
        savedChildren.add(saveGroupRecursively(child));
      }
      group.setChildren(savedChildren);
    }

    return group;
  }

  /** Busca o grupo raiz de condições de uma regra */
  public ConditionGroupDTO getConditionGroup(UUID ruleVersionId) {
    return conditionGroupRepository
        .findByRuleVersionIdAndParentGroupIdIsNull(ruleVersionId)
        .map(mapper::toDTO)
        .orElse(null);
  }

  /** Salva expressões de uma regra */
  @Transactional
  public List<ExpressionDTO> saveExpressions(UUID ruleVersionId, List<ExpressionDTO> dtos) {
    log.info("Salvando {} expressões para ruleVersionId: {}", dtos.size(), ruleVersionId);

    // Deletar expressões existentes
    expressionRepository.deleteByRuleVersionId(ruleVersionId);

    // Criar novas expressões
    List<RuleExpression> expressions =
        dtos.stream().map(dto -> mapper.toEntity(dto, ruleVersionId)).collect(Collectors.toList());

    expressions = expressionRepository.saveAll(expressions);

    return expressions.stream().map(mapper::toDTO).collect(Collectors.toList());
  }

  /** Busca expressões de uma regra */
  public List<ExpressionDTO> getExpressions(UUID ruleVersionId) {
    return expressionRepository.findByRuleVersionId(ruleVersionId).stream()
        .map(mapper::toDTO)
        .collect(Collectors.toList());
  }

  /** Salva variáveis de contexto de uma regra */
  @Transactional
  public List<ContextVariableDTO> saveContextVariables(
      UUID ruleVersionId, List<ContextVariableDTO> dtos) {
    log.info(
        "Salvando {} variáveis de contexto para ruleVersionId: {}", dtos.size(), ruleVersionId);

    // Deletar variáveis existentes
    contextVariableRepository.deleteByRuleVersionId(ruleVersionId);

    // Criar novas variáveis
    List<RuleContextVariable> variables =
        dtos.stream().map(dto -> mapper.toEntity(dto, ruleVersionId)).collect(Collectors.toList());

    variables = contextVariableRepository.saveAll(variables);

    return variables.stream().map(mapper::toDTO).collect(Collectors.toList());
  }

  /** Busca variáveis de contexto de uma regra */
  public List<ContextVariableDTO> getContextVariables(UUID ruleVersionId) {
    return contextVariableRepository.findByRuleVersionId(ruleVersionId).stream()
        .map(mapper::toDTO)
        .collect(Collectors.toList());
  }

  /** Salva ações de uma regra */
  @Transactional
  public List<RuleActionDTO> saveActions(UUID ruleVersionId, List<RuleActionDTO> dtos) {
    log.info("Salvando {} ações para ruleVersionId: {}", dtos.size(), ruleVersionId);

    // Deletar ações existentes
    actionRepository.deleteByRuleVersionId(ruleVersionId);

    // Criar novas ações
    List<RuleAction> actions = new ArrayList<>();
    for (int i = 0; i < dtos.size(); i++) {
      actions.add(mapper.toEntity(dtos.get(i), ruleVersionId, i));
    }

    actions = actionRepository.saveAll(actions);

    return actions.stream().map(mapper::toDTO).collect(Collectors.toList());
  }

  /** Busca ações de uma regra */
  public List<RuleActionDTO> getActions(UUID ruleVersionId) {
    return actionRepository.findByRuleVersionIdOrderByPositionAsc(ruleVersionId).stream()
        .map(mapper::toDTO)
        .collect(Collectors.toList());
  }

  /** Busca campos usados em uma regra */
  public List<String> getFieldsUsed(UUID ruleVersionId) {
    return conditionRepository.findDistinctFieldNamesByRuleVersionId(ruleVersionId);
  }

  /** Valida a profundidade máxima de aninhamento */
  public boolean validateMaxDepth(UUID ruleVersionId, int maxAllowedDepth) {
    int currentDepth = conditionGroupRepository.findMaxDepthByRuleVersionId(ruleVersionId);
    return currentDepth <= maxAllowedDepth;
  }

  // ========== Templates ==========

  /** Lista todos os templates */
  public List<RuleTemplate> getAllTemplates() {
    return templateRepository.findAll();
  }

  /** Lista templates por categoria */
  public List<RuleTemplate> getTemplatesByCategory(String category) {
    return templateRepository.findByCategoryOrderByNameAsc(category);
  }

  /** Lista templates do sistema */
  public List<RuleTemplate> getSystemTemplates() {
    return templateRepository.findByIsSystemTrueOrderByNameAsc();
  }

  /** Busca template por nome */
  public RuleTemplate getTemplateByName(String name) {
    return templateRepository.findByName(name).orElse(null);
  }

  /** Cria um novo template */
  @Transactional
  public RuleTemplate createTemplate(RuleTemplate template) {
    log.info("Criando template: {}", template.getName());
    return templateRepository.save(template);
  }

  /** Deleta estrutura completa de uma regra */
  @Transactional
  public void deleteRuleStructure(UUID ruleVersionId) {
    log.info("Deletando estrutura da regra: {}", ruleVersionId);
    actionRepository.deleteByRuleVersionId(ruleVersionId);
    contextVariableRepository.deleteByRuleVersionId(ruleVersionId);
    expressionRepository.deleteByRuleVersionId(ruleVersionId);
    conditionGroupRepository.deleteByRuleVersionId(ruleVersionId);
  }
}
