package com.rulex.service.complex;

import static org.assertj.core.api.Assertions.assertThat;

import com.rulex.dto.complex.ConditionDTO;
import com.rulex.dto.complex.ConditionGroupDTO;
import com.rulex.dto.complex.ConditionGroupDTO.LogicOperatorType;
import com.rulex.entity.complex.RuleCondition;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

/**
 * Testes unitários para RuleValidationService.
 * Valida que operadores GEO não implementados são bloqueados na criação de regras.
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

            RuleValidationService.ValidationResult result = validationService.validateConditionGroup(group);

            assertThat(result.valid()).isTrue();
            assertThat(result.errors()).isEmpty();
        }

        @Test
        @DisplayName("Deve rejeitar operador GEO_DISTANCE_LT")
        void shouldRejectGeoDistanceLt() {
            ConditionDTO condition = new ConditionDTO();
            condition.setFieldName("location");
            condition.setOperator(ConditionDTO.OperatorType.GEO_DISTANCE_LT);
            condition.setValueSingle("100");

            ConditionGroupDTO group = new ConditionGroupDTO();
            group.setLogicOperator(LogicOperatorType.AND);
            group.setConditions(List.of(condition));

            RuleValidationService.ValidationResult result = validationService.validateConditionGroup(group);

            assertThat(result.valid()).isFalse();
            assertThat(result.errors()).hasSize(1);
            assertThat(result.errors().get(0)).contains("GEO_DISTANCE_LT");
            assertThat(result.errors().get(0)).contains("não está implementado");
        }

        @Test
        @DisplayName("Deve rejeitar operador GEO_DISTANCE_GT")
        void shouldRejectGeoDistanceGt() {
            ConditionDTO condition = new ConditionDTO();
            condition.setFieldName("location");
            condition.setOperator(ConditionDTO.OperatorType.GEO_DISTANCE_GT);
            condition.setValueSingle("50");

            ConditionGroupDTO group = new ConditionGroupDTO();
            group.setLogicOperator(LogicOperatorType.AND);
            group.setConditions(List.of(condition));

            RuleValidationService.ValidationResult result = validationService.validateConditionGroup(group);

            assertThat(result.valid()).isFalse();
            assertThat(result.errors()).hasSize(1);
            assertThat(result.errors().get(0)).contains("GEO_DISTANCE_GT");
        }

        @Test
        @DisplayName("Deve rejeitar operador GEO_IN_POLYGON")
        void shouldRejectGeoInPolygon() {
            ConditionDTO condition = new ConditionDTO();
            condition.setFieldName("merchantLocation");
            condition.setOperator(ConditionDTO.OperatorType.GEO_IN_POLYGON);

            ConditionGroupDTO group = new ConditionGroupDTO();
            group.setLogicOperator(LogicOperatorType.AND);
            group.setConditions(List.of(condition));

            RuleValidationService.ValidationResult result = validationService.validateConditionGroup(group);

            assertThat(result.valid()).isFalse();
            assertThat(result.errors()).hasSize(1);
            assertThat(result.errors().get(0)).contains("GEO_IN_POLYGON");
        }

        @Test
        @DisplayName("Deve rejeitar tipo de valor GEO_POINT")
        void shouldRejectGeoPointValueType() {
            ConditionDTO condition = new ConditionDTO();
            condition.setFieldName("location");
            condition.setOperator(ConditionDTO.OperatorType.EQ);
            condition.setValueType(ConditionDTO.ValueType.GEO_POINT);

            ConditionGroupDTO group = new ConditionGroupDTO();
            group.setLogicOperator(LogicOperatorType.AND);
            group.setConditions(List.of(condition));

            RuleValidationService.ValidationResult result = validationService.validateConditionGroup(group);

            assertThat(result.valid()).isFalse();
            assertThat(result.errors()).anyMatch(e -> e.contains("GEO_POINT"));
        }

        @Test
        @DisplayName("Deve rejeitar tipo de valor GEO_POLYGON")
        void shouldRejectGeoPolygonValueType() {
            ConditionDTO condition = new ConditionDTO();
            condition.setFieldName("area");
            condition.setOperator(ConditionDTO.OperatorType.EQ);
            condition.setValueType(ConditionDTO.ValueType.GEO_POLYGON);

            ConditionGroupDTO group = new ConditionGroupDTO();
            group.setLogicOperator(LogicOperatorType.AND);
            group.setConditions(List.of(condition));

            RuleValidationService.ValidationResult result = validationService.validateConditionGroup(group);

            assertThat(result.valid()).isFalse();
            assertThat(result.errors()).anyMatch(e -> e.contains("GEO_POLYGON"));
        }

        @Test
        @DisplayName("Deve validar grupos aninhados recursivamente")
        void shouldValidateNestedGroups() {
            // Condição válida no grupo pai
            ConditionDTO validCondition = new ConditionDTO();
            validCondition.setFieldName("amount");
            validCondition.setOperator(ConditionDTO.OperatorType.GT);
            validCondition.setValueSingle("100");

            // Condição inválida no grupo filho
            ConditionDTO invalidCondition = new ConditionDTO();
            invalidCondition.setFieldName("location");
            invalidCondition.setOperator(ConditionDTO.OperatorType.GEO_DISTANCE_LT);

            ConditionGroupDTO childGroup = new ConditionGroupDTO();
            childGroup.setLogicOperator(LogicOperatorType.AND);
            childGroup.setConditions(List.of(invalidCondition));

            ConditionGroupDTO parentGroup = new ConditionGroupDTO();
            parentGroup.setLogicOperator(LogicOperatorType.OR);
            parentGroup.setConditions(List.of(validCondition));
            parentGroup.setChildren(List.of(childGroup));

            RuleValidationService.ValidationResult result = validationService.validateConditionGroup(parentGroup);

            assertThat(result.valid()).isFalse();
            assertThat(result.errors()).hasSize(1);
            assertThat(result.errors().get(0)).contains("GEO_DISTANCE_LT");
        }

        @Test
        @DisplayName("Deve aceitar grupo null")
        void shouldAcceptNullGroup() {
            RuleValidationService.ValidationResult result = validationService.validateConditionGroup(null);

            assertThat(result.valid()).isTrue();
        }

        @Test
        @DisplayName("Deve aceitar grupo vazio")
        void shouldAcceptEmptyGroup() {
            ConditionGroupDTO group = new ConditionGroupDTO();
            group.setLogicOperator(LogicOperatorType.AND);

            RuleValidationService.ValidationResult result = validationService.validateConditionGroup(group);

            assertThat(result.valid()).isTrue();
        }

        @Test
        @DisplayName("Deve rejeitar múltiplos operadores GEO")
        void shouldRejectMultipleGeoOperators() {
            ConditionDTO condition1 = new ConditionDTO();
            condition1.setFieldName("location1");
            condition1.setOperator(ConditionDTO.OperatorType.GEO_DISTANCE_LT);

            ConditionDTO condition2 = new ConditionDTO();
            condition2.setFieldName("location2");
            condition2.setOperator(ConditionDTO.OperatorType.GEO_IN_POLYGON);

            ConditionGroupDTO group = new ConditionGroupDTO();
            group.setLogicOperator(LogicOperatorType.AND);
            group.setConditions(List.of(condition1, condition2));

            RuleValidationService.ValidationResult result = validationService.validateConditionGroup(group);

            assertThat(result.valid()).isFalse();
            assertThat(result.errors()).hasSize(2);
        }

        @Test
        @DisplayName("Deve gerar erro para campo sem nome")
        void shouldErrorOnMissingFieldName() {
            ConditionDTO condition = new ConditionDTO();
            condition.setOperator(ConditionDTO.OperatorType.EQ);
            condition.setValueSingle("test");

            ConditionGroupDTO group = new ConditionGroupDTO();
            group.setLogicOperator(LogicOperatorType.AND);
            group.setConditions(List.of(condition));

            RuleValidationService.ValidationResult result = validationService.validateConditionGroup(group);

            assertThat(result.valid()).isFalse();
            assertThat(result.errors()).anyMatch(e -> e.contains("Nome do campo é obrigatório"));
        }

        @Test
        @DisplayName("Deve gerar erro para BETWEEN sem min/max")
        void shouldErrorOnBetweenWithoutMinMax() {
            ConditionDTO condition = new ConditionDTO();
            condition.setFieldName("amount");
            condition.setOperator(ConditionDTO.OperatorType.BETWEEN);
            // Sem valueMin e valueMax

            ConditionGroupDTO group = new ConditionGroupDTO();
            group.setLogicOperator(LogicOperatorType.AND);
            group.setConditions(List.of(condition));

            RuleValidationService.ValidationResult result = validationService.validateConditionGroup(group);

            assertThat(result.valid()).isFalse();
            assertThat(result.errors()).anyMatch(e -> e.contains("BETWEEN") && e.contains("mínimo e máximo"));
        }
    }

    @Nested
    @DisplayName("validateCondition - Validação de Condição Individual")
    class ValidateConditionTests {

        @Test
        @DisplayName("Deve aceitar condição com operador válido")
        void shouldAcceptValidCondition() {
            RuleCondition condition = RuleCondition.builder()
                    .fieldName("amount")
                    .operator(RuleCondition.ConditionOperator.GT)
                    .valueSingle("1000")
                    .build();

            RuleValidationService.ValidationResult result = validationService.validateCondition(condition);

            assertThat(result.valid()).isTrue();
        }

        @Test
        @DisplayName("Deve rejeitar condição com operador GEO")
        void shouldRejectGeoCondition() {
            RuleCondition condition = RuleCondition.builder()
                    .fieldName("location")
                    .operator(RuleCondition.ConditionOperator.GEO_DISTANCE_LT)
                    .build();

            RuleValidationService.ValidationResult result = validationService.validateCondition(condition);

            assertThat(result.valid()).isFalse();
            assertThat(result.errors()).anyMatch(e -> e.contains("GEO_DISTANCE_LT"));
        }
    }

    @Nested
    @DisplayName("getUnsupportedOperators - Lista de Operadores Não Suportados")
    class GetUnsupportedOperatorsTests {

        @Test
        @DisplayName("Deve retornar lista de operadores GEO")
        void shouldReturnGeoOperators() {
            var unsupported = validationService.getUnsupportedOperators();

            assertThat(unsupported).containsExactlyInAnyOrder(
                    "GEO_DISTANCE_LT",
                    "GEO_DISTANCE_GT",
                    "GEO_IN_POLYGON"
            );
        }
    }

    @Nested
    @DisplayName("getUnsupportedValueTypes - Lista de Tipos de Valor Não Suportados")
    class GetUnsupportedValueTypesTests {

        @Test
        @DisplayName("Deve retornar lista de tipos GEO")
        void shouldReturnGeoValueTypes() {
            var unsupported = validationService.getUnsupportedValueTypes();

            assertThat(unsupported).containsExactlyInAnyOrder(
                    "GEO_POINT",
                    "GEO_POLYGON"
            );
        }
    }
}
