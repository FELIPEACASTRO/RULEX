package com.rulex.service.complex;

import static org.assertj.core.api.Assertions.assertThat;

import com.rulex.dto.complex.ConditionDTO;
import com.rulex.dto.complex.ConditionGroupDTO;
import com.rulex.dto.complex.ConditionGroupDTO.LogicOperatorType;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

/**
 * Testes unitários para RuleValidationService. Valida a validação de regras e operadores.
 *
 * <p>NOTA: Operadores GEO (GEO_DISTANCE_LT, GEO_DISTANCE_GT, GEO_IN_POLYGON) foram implementados
 * via GeoService e agora são aceitos.
 */
class RuleValidationServiceTest {

  private RuleValidationService validationService;

  @BeforeEach
  void setUp() {
    validationService = new RuleValidationService();
  }

  @Nested
  @DisplayName("validateConditionGroup - Validação de Grupos de Condições")
  class ValidateConditionGroupTests {

    @Test
    @DisplayName("Deve aceitar grupo com operadores válidos")
    void shouldAcceptValidOperators() {
      ConditionDTO condition = new ConditionDTO();
      condition.setFieldName("transactionAmount");
      condition.setOperator(ConditionDTO.OperatorType.GT);
      condition.setValueSingle("1000");

      ConditionGroupDTO group = new ConditionGroupDTO();
      group.setLogicOperator(LogicOperatorType.AND);
      group.setConditions(List.of(condition));

      RuleValidationService.ValidationResult result =
          validationService.validateConditionGroup(group);

      assertThat(result.valid()).isTrue();
      assertThat(result.errors()).isEmpty();
    }

    @Test
    @DisplayName("Deve aceitar grupo nulo")
    void shouldAcceptNullGroup() {
      RuleValidationService.ValidationResult result =
          validationService.validateConditionGroup(null);

      assertThat(result.valid()).isTrue();
    }

    @Test
    @DisplayName("Deve rejeitar condição sem campo")
    void shouldRejectConditionWithoutField() {
      ConditionDTO condition = new ConditionDTO();
      condition.setOperator(ConditionDTO.OperatorType.EQ);
      condition.setValueSingle("test");

      ConditionGroupDTO group = new ConditionGroupDTO();
      group.setLogicOperator(LogicOperatorType.AND);
      group.setConditions(List.of(condition));

      RuleValidationService.ValidationResult result =
          validationService.validateConditionGroup(group);

      assertThat(result.valid()).isFalse();
      assertThat(result.errors()).anyMatch(e -> e.contains("campo"));
    }

    @Test
    @DisplayName("Deve rejeitar condição sem operador")
    void shouldRejectConditionWithoutOperator() {
      ConditionDTO condition = new ConditionDTO();
      condition.setFieldName("transactionAmount");
      condition.setValueSingle("1000");

      ConditionGroupDTO group = new ConditionGroupDTO();
      group.setLogicOperator(LogicOperatorType.AND);
      group.setConditions(List.of(condition));

      RuleValidationService.ValidationResult result =
          validationService.validateConditionGroup(group);

      assertThat(result.valid()).isFalse();
      assertThat(result.errors()).anyMatch(e -> e.contains("Operador"));
    }

    @Test
    @DisplayName("Deve aceitar operador GEO_DISTANCE_LT (agora implementado)")
    void shouldAcceptGeoDistanceLt() {
      ConditionDTO condition = new ConditionDTO();
      condition.setFieldName("merchantLocation");
      condition.setOperator(ConditionDTO.OperatorType.GEO_DISTANCE_LT);
      condition.setValueSingle("-23.55,-46.63,100");
      condition.setValueType(ConditionDTO.ValueType.GEO_POINT);

      ConditionGroupDTO group = new ConditionGroupDTO();
      group.setLogicOperator(LogicOperatorType.AND);
      group.setConditions(List.of(condition));

      RuleValidationService.ValidationResult result =
          validationService.validateConditionGroup(group);

      assertThat(result.valid()).isTrue();
    }

    @Test
    @DisplayName("Deve aceitar operador GEO_DISTANCE_GT (agora implementado)")
    void shouldAcceptGeoDistanceGt() {
      ConditionDTO condition = new ConditionDTO();
      condition.setFieldName("location");
      condition.setOperator(ConditionDTO.OperatorType.GEO_DISTANCE_GT);
      condition.setValueSingle("-23.55,-46.63,50");

      ConditionGroupDTO group = new ConditionGroupDTO();
      group.setLogicOperator(LogicOperatorType.AND);
      group.setConditions(List.of(condition));

      RuleValidationService.ValidationResult result =
          validationService.validateConditionGroup(group);

      assertThat(result.valid()).isTrue();
    }

    @Test
    @DisplayName("Deve aceitar operador GEO_IN_POLYGON (agora implementado)")
    void shouldAcceptGeoInPolygon() {
      ConditionDTO condition = new ConditionDTO();
      condition.setFieldName("merchantLocation");
      condition.setOperator(ConditionDTO.OperatorType.GEO_IN_POLYGON);
      condition.setValueSingle("BRASIL");

      ConditionGroupDTO group = new ConditionGroupDTO();
      group.setLogicOperator(LogicOperatorType.AND);
      group.setConditions(List.of(condition));

      RuleValidationService.ValidationResult result =
          validationService.validateConditionGroup(group);

      assertThat(result.valid()).isTrue();
    }

    @Test
    @DisplayName("Deve aceitar tipo de valor GEO_POINT (agora implementado)")
    void shouldAcceptGeoPointValueType() {
      ConditionDTO condition = new ConditionDTO();
      condition.setFieldName("location");
      condition.setOperator(ConditionDTO.OperatorType.GEO_DISTANCE_LT);
      condition.setValueType(ConditionDTO.ValueType.GEO_POINT);
      condition.setValueSingle("-23.55,-46.63,100");

      ConditionGroupDTO group = new ConditionGroupDTO();
      group.setLogicOperator(LogicOperatorType.AND);
      group.setConditions(List.of(condition));

      RuleValidationService.ValidationResult result =
          validationService.validateConditionGroup(group);

      assertThat(result.valid()).isTrue();
    }

    @Test
    @DisplayName("Deve aceitar tipo de valor GEO_POLYGON (agora implementado)")
    void shouldAcceptGeoPolygonValueType() {
      ConditionDTO condition = new ConditionDTO();
      condition.setFieldName("area");
      condition.setOperator(ConditionDTO.OperatorType.GEO_IN_POLYGON);
      condition.setValueType(ConditionDTO.ValueType.GEO_POLYGON);
      condition.setValueSingle("SAO_PAULO_ESTADO");

      ConditionGroupDTO group = new ConditionGroupDTO();
      group.setLogicOperator(LogicOperatorType.AND);
      group.setConditions(List.of(condition));

      RuleValidationService.ValidationResult result =
          validationService.validateConditionGroup(group);

      assertThat(result.valid()).isTrue();
    }

    @Test
    @DisplayName("Deve aceitar múltiplos operadores GEO (agora implementados)")
    void shouldAcceptMultipleGeoOperators() {
      ConditionDTO condition1 = new ConditionDTO();
      condition1.setFieldName("location1");
      condition1.setOperator(ConditionDTO.OperatorType.GEO_DISTANCE_LT);
      condition1.setValueSingle("-23.55,-46.63,100");

      ConditionDTO condition2 = new ConditionDTO();
      condition2.setFieldName("location2");
      condition2.setOperator(ConditionDTO.OperatorType.GEO_IN_POLYGON);
      condition2.setValueSingle("BRASIL");

      ConditionGroupDTO group = new ConditionGroupDTO();
      group.setLogicOperator(LogicOperatorType.AND);
      group.setConditions(List.of(condition1, condition2));

      RuleValidationService.ValidationResult result =
          validationService.validateConditionGroup(group);

      assertThat(result.valid()).isTrue();
    }

    @Test
    @DisplayName("Deve validar grupos aninhados")
    void shouldValidateNestedGroups() {
      ConditionDTO condition1 = new ConditionDTO();
      condition1.setFieldName("amount");
      condition1.setOperator(ConditionDTO.OperatorType.GT);
      condition1.setValueSingle("1000");

      ConditionDTO condition2 = new ConditionDTO();
      condition2.setFieldName("mcc");
      condition2.setOperator(ConditionDTO.OperatorType.IN);
      condition2.setValueArray(List.of("7995", "6211"));

      ConditionGroupDTO childGroup = new ConditionGroupDTO();
      childGroup.setLogicOperator(LogicOperatorType.OR);
      childGroup.setConditions(List.of(condition2));

      ConditionGroupDTO rootGroup = new ConditionGroupDTO();
      rootGroup.setLogicOperator(LogicOperatorType.AND);
      rootGroup.setConditions(List.of(condition1));
      rootGroup.setChildren(List.of(childGroup));

      RuleValidationService.ValidationResult result =
          validationService.validateConditionGroup(rootGroup);

      assertThat(result.valid()).isTrue();
    }

    @Test
    @DisplayName("Deve rejeitar profundidade excessiva de aninhamento")
    void shouldRejectExcessiveNesting() {
      // Criar 12 níveis de aninhamento (limite é 10)
      ConditionGroupDTO current = new ConditionGroupDTO();
      current.setLogicOperator(LogicOperatorType.AND);

      ConditionDTO condition = new ConditionDTO();
      condition.setFieldName("test");
      condition.setOperator(ConditionDTO.OperatorType.EQ);
      condition.setValueSingle("value");
      current.setConditions(List.of(condition));

      for (int i = 0; i < 12; i++) {
        ConditionGroupDTO parent = new ConditionGroupDTO();
        parent.setLogicOperator(LogicOperatorType.AND);
        parent.setChildren(List.of(current));
        current = parent;
      }

      RuleValidationService.ValidationResult result =
          validationService.validateConditionGroup(current);

      assertThat(result.valid()).isFalse();
      assertThat(result.errors()).anyMatch(e -> e.contains("Profundidade"));
    }
  }

  @Nested
  @DisplayName("validateCondition - Validação de Condição de Entidade")
  class ValidateConditionTests {

    @Test
    @DisplayName("Deve aceitar condição válida")
    void shouldAcceptValidCondition() {
      RuleCondition condition =
          RuleCondition.builder()
              .fieldName("transactionAmount")
              .operator(ConditionOperator.GT)
              .valueSingle("1000")
              .build();

      RuleValidationService.ValidationResult result =
          validationService.validateCondition(condition);

      assertThat(result.valid()).isTrue();
    }

    @Test
    @DisplayName("Deve aceitar condição nula")
    void shouldAcceptNullCondition() {
      RuleValidationService.ValidationResult result = validationService.validateCondition(null);

      assertThat(result.valid()).isTrue();
    }

    @Test
    @DisplayName("Deve aceitar operador GEO_DISTANCE_LT (agora implementado)")
    void shouldAcceptGeoCondition() {
      RuleCondition condition =
          RuleCondition.builder()
              .fieldName("location")
              .operator(ConditionOperator.GEO_DISTANCE_LT)
              .valueSingle("-23.55,-46.63,100")
              .build();

      RuleValidationService.ValidationResult result =
          validationService.validateCondition(condition);

      assertThat(result.valid()).isTrue();
    }
  }

  @Nested
  @DisplayName("getUnsupportedOperators - Lista de Operadores Não Suportados")
  class GetUnsupportedOperatorsTests {

    @Test
    @DisplayName("Deve retornar lista vazia (todos operadores agora suportados)")
    void shouldReturnEmptyList() {
      var unsupported = validationService.getUnsupportedOperators();

      assertThat(unsupported).isEmpty();
    }
  }

  @Nested
  @DisplayName("getUnsupportedValueTypes - Lista de Tipos de Valor Não Suportados")
  class GetUnsupportedValueTypesTests {

    @Test
    @DisplayName("Deve retornar lista vazia (todos tipos agora suportados)")
    void shouldReturnEmptyList() {
      var unsupported = validationService.getUnsupportedValueTypes();

      assertThat(unsupported).isEmpty();
    }
  }
}
