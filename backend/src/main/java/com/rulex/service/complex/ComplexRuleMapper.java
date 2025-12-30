package com.rulex.service.complex;

import com.rulex.dto.complex.*;
import com.rulex.entity.complex.*;
import java.util.ArrayList;
import java.util.stream.Collectors;
import org.springframework.stereotype.Component;

/** Mapper para conversão entre DTOs e Entidades de regras complexas. */
@Component
public class ComplexRuleMapper {

  /** Converte ConditionGroupDTO para entidade */
  public RuleConditionGroup toEntity(
      ConditionGroupDTO dto, java.util.UUID ruleVersionId, java.util.UUID parentGroupId) {
    if (dto == null) return null;

    RuleConditionGroup group =
        RuleConditionGroup.builder()
            .id(dto.getId())
            .ruleVersionId(ruleVersionId)
            .parentGroupId(parentGroupId)
            .logicOperator(mapLogicOperator(dto.getLogicOperator()))
            .name(dto.getName())
            .description(dto.getDescription())
            .position(dto.getPosition() != null ? dto.getPosition() : 0)
            .enabled(dto.getEnabled() != null ? dto.getEnabled() : true)
            .conditions(new ArrayList<>())
            .children(new ArrayList<>())
            .build();

    // Mapear condições
    if (dto.getConditions() != null) {
      for (int i = 0; i < dto.getConditions().size(); i++) {
        ConditionDTO condDto = dto.getConditions().get(i);
        RuleCondition condition = toEntity(condDto, i);
        condition.setGroup(group);
        group.getConditions().add(condition);
      }
    }

    // Mapear grupos filhos recursivamente
    if (dto.getChildren() != null) {
      for (int i = 0; i < dto.getChildren().size(); i++) {
        ConditionGroupDTO childDto = dto.getChildren().get(i);
        if (childDto.getPosition() == null) {
          childDto.setPosition(i);
        }
        RuleConditionGroup child = toEntity(childDto, ruleVersionId, group.getId());
        group.getChildren().add(child);
      }
    }

    return group;
  }

  /** Converte ConditionDTO para entidade */
  public RuleCondition toEntity(ConditionDTO dto, int position) {
    if (dto == null) return null;

    return RuleCondition.builder()
        .id(dto.getId())
        .position(dto.getPosition() != null ? dto.getPosition() : position)
        .fieldName(dto.getFieldName())
        .fieldPath(dto.getFieldPath())
        .operator(mapOperator(dto.getOperator()))
        .valueType(mapValueType(dto.getValueType()))
        .valueSingle(dto.getValueSingle())
        .valueArray(dto.getValueArray())
        .valueMin(dto.getValueMin())
        .valueMax(dto.getValueMax())
        .valueFieldRef(dto.getValueFieldRef())
        .valueExpression(dto.getValueExpression())
        .caseSensitive(dto.getCaseSensitive() != null ? dto.getCaseSensitive() : true)
        .negate(dto.getNegate() != null ? dto.getNegate() : false)
        .enabled(dto.getEnabled() != null ? dto.getEnabled() : true)
        .description(dto.getDescription())
        .errorMessage(dto.getErrorMessage())
        .build();
  }

  /** Converte ExpressionDTO para entidade */
  public RuleExpression toEntity(ExpressionDTO dto, java.util.UUID ruleVersionId) {
    if (dto == null) return null;

    return RuleExpression.builder()
        .id(dto.getId())
        .ruleVersionId(ruleVersionId)
        .name(dto.getName())
        .expression(dto.getExpression())
        .resultType(mapValueType(dto.getResultType()))
        .description(dto.getDescription())
        .build();
  }

  /** Converte ContextVariableDTO para entidade */
  public RuleContextVariable toEntity(ContextVariableDTO dto, java.util.UUID ruleVersionId) {
    if (dto == null) return null;

    return RuleContextVariable.builder()
        .id(dto.getId())
        .ruleVersionId(ruleVersionId)
        .name(dto.getName())
        .sourceType(mapSourceType(dto.getSourceType()))
        .sourceConfig(dto.getSourceConfig())
        .defaultValue(dto.getDefaultValue())
        .description(dto.getDescription())
        .build();
  }

  /** Converte RuleActionDTO para entidade */
  public RuleAction toEntity(RuleActionDTO dto, java.util.UUID ruleVersionId, int position) {
    if (dto == null) return null;

    return RuleAction.builder()
        .id(dto.getId())
        .ruleVersionId(ruleVersionId)
        .actionType(mapActionType(dto.getActionType()))
        .actionConfig(dto.getActionConfig())
        .position(dto.getPosition() != null ? dto.getPosition() : position)
        .conditionGroupId(dto.getConditionGroupId())
        .enabled(dto.getEnabled() != null ? dto.getEnabled() : true)
        .description(dto.getDescription())
        .build();
  }

  // ========== Conversão de Entidade para DTO ==========

  /** Converte entidade RuleConditionGroup para DTO */
  public ConditionGroupDTO toDTO(RuleConditionGroup entity) {
    if (entity == null) return null;

    return ConditionGroupDTO.builder()
        .id(entity.getId())
        .logicOperator(mapLogicOperatorToDTO(entity.getLogicOperator()))
        .name(entity.getName())
        .description(entity.getDescription())
        .position(entity.getPosition())
        .enabled(entity.getEnabled())
        .conditions(
            entity.getConditions() != null
                ? entity.getConditions().stream().map(this::toDTO).collect(Collectors.toList())
                : new ArrayList<>())
        .children(
            entity.getChildren() != null
                ? entity.getChildren().stream().map(this::toDTO).collect(Collectors.toList())
                : new ArrayList<>())
        .build();
  }

  /** Converte entidade RuleCondition para DTO */
  public ConditionDTO toDTO(RuleCondition entity) {
    if (entity == null) return null;

    return ConditionDTO.builder()
        .id(entity.getId())
        .fieldName(entity.getFieldName())
        .fieldPath(entity.getFieldPath())
        .operator(mapOperatorToDTO(entity.getOperator()))
        .valueType(mapValueTypeToDTO(entity.getValueType()))
        .valueSingle(entity.getValueSingle())
        .valueArray(entity.getValueArray())
        .valueMin(entity.getValueMin())
        .valueMax(entity.getValueMax())
        .valueFieldRef(entity.getValueFieldRef())
        .valueExpression(entity.getValueExpression())
        .caseSensitive(entity.getCaseSensitive())
        .negate(entity.getNegate())
        .enabled(entity.getEnabled())
        .position(entity.getPosition())
        .description(entity.getDescription())
        .errorMessage(entity.getErrorMessage())
        .build();
  }

  /** Converte entidade RuleExpression para DTO */
  public ExpressionDTO toDTO(RuleExpression entity) {
    if (entity == null) return null;

    return ExpressionDTO.builder()
        .id(entity.getId())
        .name(entity.getName())
        .expression(entity.getExpression())
        .resultType(mapValueTypeToDTO(entity.getResultType()))
        .description(entity.getDescription())
        .build();
  }

  /** Converte entidade RuleContextVariable para DTO */
  public ContextVariableDTO toDTO(RuleContextVariable entity) {
    if (entity == null) return null;

    return ContextVariableDTO.builder()
        .id(entity.getId())
        .name(entity.getName())
        .sourceType(mapSourceTypeToDTO(entity.getSourceType()))
        .sourceConfig(entity.getSourceConfig())
        .defaultValue(entity.getDefaultValue())
        .description(entity.getDescription())
        .build();
  }

  /** Converte entidade RuleAction para DTO */
  public RuleActionDTO toDTO(RuleAction entity) {
    if (entity == null) return null;

    return RuleActionDTO.builder()
        .id(entity.getId())
        .actionType(mapActionTypeToDTO(entity.getActionType()))
        .actionConfig(entity.getActionConfig())
        .position(entity.getPosition())
        .conditionGroupId(entity.getConditionGroupId())
        .enabled(entity.getEnabled())
        .description(entity.getDescription())
        .build();
  }

  // ========== Métodos auxiliares de mapeamento de enums ==========

  private RuleConditionGroup.GroupLogicOperator mapLogicOperator(
      ConditionGroupDTO.LogicOperatorType type) {
    if (type == null) return RuleConditionGroup.GroupLogicOperator.AND;
    return RuleConditionGroup.GroupLogicOperator.valueOf(type.name());
  }

  private ConditionGroupDTO.LogicOperatorType mapLogicOperatorToDTO(
      RuleConditionGroup.GroupLogicOperator type) {
    if (type == null) return ConditionGroupDTO.LogicOperatorType.AND;
    return ConditionGroupDTO.LogicOperatorType.valueOf(type.name());
  }

  private RuleCondition.ConditionOperator mapOperator(ConditionDTO.OperatorType type) {
    if (type == null) return RuleCondition.ConditionOperator.EQ;
    return RuleCondition.ConditionOperator.valueOf(type.name());
  }

  private ConditionDTO.OperatorType mapOperatorToDTO(RuleCondition.ConditionOperator type) {
    if (type == null) return ConditionDTO.OperatorType.EQ;
    return ConditionDTO.OperatorType.valueOf(type.name());
  }

  private RuleCondition.ConditionValueType mapValueType(ConditionDTO.ValueType type) {
    if (type == null) return RuleCondition.ConditionValueType.STRING;
    return RuleCondition.ConditionValueType.valueOf(type.name());
  }

  private ConditionDTO.ValueType mapValueTypeToDTO(RuleCondition.ConditionValueType type) {
    if (type == null) return ConditionDTO.ValueType.STRING;
    return ConditionDTO.ValueType.valueOf(type.name());
  }

  private RuleContextVariable.SourceType mapSourceType(ContextVariableDTO.SourceType type) {
    if (type == null) return RuleContextVariable.SourceType.PAYLOAD;
    return RuleContextVariable.SourceType.valueOf(type.name());
  }

  private ContextVariableDTO.SourceType mapSourceTypeToDTO(RuleContextVariable.SourceType type) {
    if (type == null) return ContextVariableDTO.SourceType.PAYLOAD;
    return ContextVariableDTO.SourceType.valueOf(type.name());
  }

  private RuleAction.ActionType mapActionType(RuleActionDTO.ActionType type) {
    if (type == null) return RuleAction.ActionType.SET_DECISION;
    return RuleAction.ActionType.valueOf(type.name());
  }

  private RuleActionDTO.ActionType mapActionTypeToDTO(RuleAction.ActionType type) {
    if (type == null) return RuleActionDTO.ActionType.SET_DECISION;
    return RuleActionDTO.ActionType.valueOf(type.name());
  }
}
