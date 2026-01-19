package com.rulex.service.complex;

import com.rulex.dto.complex.ConditionDTO;
import com.rulex.dto.complex.ConditionGroupDTO;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

/**
 * Serviço de validação de regras. Valida regras antes de persistir para garantir que apenas
 * operadores implementados sejam usados. Também aplica limites anti-abuso para evitar regras
 * monstruosas.
 */
@Service
@Slf4j
public class RuleValidationService {

  // ========== Limites Anti-Abuso ==========
  /** Profundidade máxima de aninhamento de grupos */
  public static final int MAX_NESTING_DEPTH = 10;

  /** Número máximo de condições por grupo */
  public static final int MAX_CONDITIONS_PER_GROUP = 50;

  /** Número máximo total de condições em uma regra */
  public static final int MAX_TOTAL_CONDITIONS = 200;

  /** Tamanho máximo de listas IN/NOT_IN */
  public static final int MAX_LIST_SIZE = 1000;

  /** Tamanho máximo de padrão REGEX */
  public static final int MAX_REGEX_LENGTH = 500;

  /** Tamanho máximo do JSON da regra (em bytes) */
  public static final int MAX_RULE_JSON_SIZE = 100_000;

  /**
   * Operadores que estão declarados mas NÃO implementados. Regras com estes operadores devem ser
   * rejeitadas na criação.
   *
   * <p>NOTA: GEO_DISTANCE_LT, GEO_DISTANCE_GT, GEO_IN_POLYGON foram implementados via GeoService e
   * agora estão disponíveis.
   */
  private static final Set<String> UNSUPPORTED_OPERATORS = Set.of();

  /**
   * Tipos de valor que não estão suportados.
   *
   * <p>NOTA: GEO_POINT e GEO_POLYGON agora são suportados via GeoService.
   */
  private static final Set<String> UNSUPPORTED_VALUE_TYPES = Set.of();

  /** Resultado da validação de uma regra. */
  public record ValidationResult(boolean valid, List<String> errors, List<String> warnings) {
    public static ValidationResult success() {
      return new ValidationResult(true, List.of(), List.of());
    }

    public static ValidationResult failure(List<String> errors) {
      return new ValidationResult(false, errors, List.of());
    }

    public static ValidationResult withWarnings(List<String> warnings) {
      return new ValidationResult(true, List.of(), warnings);
    }
  }

  /**
   * Valida um grupo de condições (DTO) antes de persistir.
   *
   * @param group O grupo de condições a validar
   * @return Resultado da validação com erros se houver
   */
  public ValidationResult validateConditionGroup(ConditionGroupDTO group) {
    List<String> errors = new ArrayList<>();
    List<String> warnings = new ArrayList<>();

    if (group == null) {
      return ValidationResult.success();
    }

    // Validar contagem total de condições
    int totalConditions = countTotalConditions(group);
    if (totalConditions > MAX_TOTAL_CONDITIONS) {
      errors.add(
          String.format(
              "Número total de condições excede o máximo permitido (máximo: %d, encontrado: %d)",
              MAX_TOTAL_CONDITIONS, totalConditions));
    }

    validateGroupRecursively(group, errors, warnings, 0);

    if (!errors.isEmpty()) {
      log.warn("Validação de regra falhou com {} erros: {}", errors.size(), errors);
      return ValidationResult.failure(errors);
    }

    if (!warnings.isEmpty()) {
      log.info("Validação de regra passou com {} avisos: {}", warnings.size(), warnings);
      return ValidationResult.withWarnings(warnings);
    }

    return ValidationResult.success();
  }

  /** Valida um grupo de condições recursivamente. */
  private void validateGroupRecursively(
      ConditionGroupDTO group, List<String> errors, List<String> warnings, int depth) {

    // Validar profundidade máxima
    if (depth > MAX_NESTING_DEPTH) {
      errors.add(
          String.format(
              "Profundidade máxima de aninhamento excedida (máximo: %d níveis)",
              MAX_NESTING_DEPTH));
      return;
    }

    // Validar número de condições por grupo
    if (group.getConditions() != null && group.getConditions().size() > MAX_CONDITIONS_PER_GROUP) {
      errors.add(
          String.format(
              "Número máximo de condições por grupo excedido (máximo: %d, encontrado: %d)",
              MAX_CONDITIONS_PER_GROUP, group.getConditions().size()));
    }

    // Validar condições do grupo
    if (group.getConditions() != null) {
      for (ConditionDTO condition : group.getConditions()) {
        validateCondition(condition, errors, warnings);
      }
    }

    // Validar grupos filhos recursivamente
    if (group.getChildren() != null) {
      for (ConditionGroupDTO child : group.getChildren()) {
        validateGroupRecursively(child, errors, warnings, depth + 1);
      }
    }
  }

  /** Valida uma condição individual. */
  private void validateCondition(
      ConditionDTO condition, List<String> errors, List<String> warnings) {
    if (condition == null) {
      return;
    }

    String fieldName = condition.getFieldName();
    ConditionDTO.OperatorType operator = condition.getOperator();
    ConditionDTO.ValueType valueType = condition.getValueType();

    // Validar operador
    if (operator != null) {
      String operatorName = operator.name();
      if (UNSUPPORTED_OPERATORS.contains(operatorName)) {
        errors.add(
            String.format(
                "Operador '%s' no campo '%s' não está implementado. "
                    + "Operadores de geolocalização (GEO_DISTANCE_LT, GEO_DISTANCE_GT, GEO_IN_POLYGON) "
                    + "requerem integração com serviço externo que ainda não está disponível.",
                operatorName, fieldName != null ? fieldName : "desconhecido"));
      }
    }

    // Validar tipo de valor
    if (valueType != null) {
      String valueTypeName = valueType.name();
      if (UNSUPPORTED_VALUE_TYPES.contains(valueTypeName)) {
        errors.add(
            String.format(
                "Tipo de valor '%s' no campo '%s' não está implementado. "
                    + "Tipos de geolocalização (GEO_POINT, GEO_POLYGON) requerem integração com serviço externo.",
                valueTypeName, fieldName != null ? fieldName : "desconhecido"));
      }
    }

    // Validar campo obrigatório
    if (fieldName == null || fieldName.isBlank()) {
      errors.add("Nome do campo é obrigatório em todas as condições");
    }

    // Validar operador obrigatório
    if (operator == null) {
      errors.add(
          String.format(
              "Operador é obrigatório para o campo '%s'",
              fieldName != null ? fieldName : "desconhecido"));
    }

    // Validar valor para operadores que requerem
    if (operator != null
        && requiresValue(operator)
        && condition.getValueSingle() == null
        && (condition.getValueArray() == null || condition.getValueArray().isEmpty())) {
      warnings.add(
          String.format(
              "Operador '%s' no campo '%s' geralmente requer um valor",
              operator.name(), fieldName != null ? fieldName : "desconhecido"));
    }

    // Validar BETWEEN requer min e max
    if (operator != null && isBetweenOperator(operator)) {
      if (condition.getValueMin() == null || condition.getValueMax() == null) {
        errors.add(
            String.format(
                "Operador '%s' no campo '%s' requer valores mínimo e máximo",
                operator.name(), fieldName != null ? fieldName : "desconhecido"));
      }
    }

    // Validar tamanho de listas IN/NOT_IN
    if (operator != null && isListOperator(operator) && condition.getValueArray() != null) {
      if (condition.getValueArray().size() > MAX_LIST_SIZE) {
        errors.add(
            String.format(
                "Lista no campo '%s' excede o tamanho máximo (máximo: %d, encontrado: %d)",
                fieldName != null ? fieldName : "desconhecido",
                MAX_LIST_SIZE,
                condition.getValueArray().size()));
      }
    }

    // Validar tamanho de REGEX
    if (operator != null && isRegexOperator(operator) && condition.getValueSingle() != null) {
      if (condition.getValueSingle().length() > MAX_REGEX_LENGTH) {
        errors.add(
            String.format(
                "Expressão regular no campo '%s' excede o tamanho máximo (máximo: %d caracteres)",
                fieldName != null ? fieldName : "desconhecido", MAX_REGEX_LENGTH));
      }
    }
  }

  /** Verifica se é um operador de lista. */
  private boolean isListOperator(ConditionDTO.OperatorType operator) {
    return switch (operator) {
      case IN, NOT_IN -> true;
      default -> false;
    };
  }

  /** Verifica se é um operador de regex. */
  private boolean isRegexOperator(ConditionDTO.OperatorType operator) {
    return switch (operator) {
      case REGEX, NOT_REGEX -> true;
      default -> false;
    };
  }

  /** Conta o número total de condições em um grupo (recursivamente). */
  private int countTotalConditions(ConditionGroupDTO group) {
    if (group == null) {
      return 0;
    }
    int count = group.getConditions() != null ? group.getConditions().size() : 0;
    if (group.getChildren() != null) {
      for (ConditionGroupDTO child : group.getChildren()) {
        count += countTotalConditions(child);
      }
    }
    return count;
  }

  /** Verifica se o operador requer um valor. */
  private boolean requiresValue(ConditionDTO.OperatorType operator) {
    return switch (operator) {
      case IS_NULL, NOT_NULL, IS_TRUE, IS_FALSE -> false;
      default -> true;
    };
  }

  /** Verifica se é um operador BETWEEN. */
  private boolean isBetweenOperator(ConditionDTO.OperatorType operator) {
    return switch (operator) {
      case BETWEEN, NOT_BETWEEN, DATE_BETWEEN, TIME_BETWEEN -> true;
      default -> false;
    };
  }

  /** Valida uma condição de entidade (RuleCondition). */
  public ValidationResult validateCondition(RuleCondition condition) {
    List<String> errors = new ArrayList<>();

    if (condition == null) {
      return ValidationResult.success();
    }

    ConditionOperator operator = condition.getOperator();
    if (operator != null && UNSUPPORTED_OPERATORS.contains(operator.name())) {
      errors.add(String.format("Operador '%s' não está implementado", operator.name()));
    }

    RuleCondition.ConditionValueType valueType = condition.getValueType();
    if (valueType != null && UNSUPPORTED_VALUE_TYPES.contains(valueType.name())) {
      errors.add(String.format("Tipo de valor '%s' não está implementado", valueType.name()));
    }

    return errors.isEmpty() ? ValidationResult.success() : ValidationResult.failure(errors);
  }

  /** Retorna a lista de operadores não suportados. */
  public Set<String> getUnsupportedOperators() {
    return UNSUPPORTED_OPERATORS;
  }

  /** Retorna a lista de tipos de valor não suportados. */
  public Set<String> getUnsupportedValueTypes() {
    return UNSUPPORTED_VALUE_TYPES;
  }
}
