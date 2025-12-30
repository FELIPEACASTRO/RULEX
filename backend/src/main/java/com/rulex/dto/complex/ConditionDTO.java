package com.rulex.dto.complex;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import java.util.List;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/** DTO para condições individuais com suporte a operadores avançados. */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ConditionDTO {

  private UUID id;

  @NotBlank(message = "Nome do campo é obrigatório")
  private String fieldName;

  /** Caminho para campos aninhados (ex: "customer.address.city") */
  private String fieldPath;

  @NotNull(message = "Operador é obrigatório")
  private OperatorType operator;

  @NotNull(message = "Tipo do valor é obrigatório")
  private ValueType valueType;

  /** Valor único para comparação */
  private String valueSingle;

  /** Lista de valores para operadores IN, NOT_IN, etc */
  private List<String> valueArray;

  /** Valor mínimo para operadores BETWEEN */
  private String valueMin;

  /** Valor máximo para operadores BETWEEN */
  private String valueMax;

  /** Referência a outro campo para comparação entre campos */
  private String valueFieldRef;

  /** Expressão calculada (ex: "transactionAmount * 1.1") */
  private String valueExpression;

  /** Se a comparação deve ser case-sensitive (para strings) */
  private Boolean caseSensitive;

  /** Inverte o resultado da condição */
  private Boolean negate;

  private Boolean enabled;

  private Integer position;

  private String description;

  /** Mensagem de erro customizada quando a condição falha */
  private String errorMessage;

  /** Operadores de comparação suportados */
  public enum OperatorType {
    // Comparação básica
    EQ("Igual"),
    NEQ("Diferente"),
    GT("Maior que"),
    GTE("Maior ou igual"),
    LT("Menor que"),
    LTE("Menor ou igual"),

    // Listas
    IN("Em lista"),
    NOT_IN("Não em lista"),

    // Strings
    CONTAINS("Contém"),
    NOT_CONTAINS("Não contém"),
    STARTS_WITH("Começa com"),
    ENDS_WITH("Termina com"),
    REGEX("Expressão regular"),
    NOT_REGEX("Não corresponde à regex"),

    // Nulos
    IS_NULL("É nulo"),
    NOT_NULL("Não é nulo"),

    // Booleanos
    IS_TRUE("É verdadeiro"),
    IS_FALSE("É falso"),

    // Range
    BETWEEN("Entre"),
    NOT_BETWEEN("Não entre"),

    // Comparação entre campos
    FIELD_EQ("Campo igual a"),
    FIELD_NEQ("Campo diferente de"),
    FIELD_GT("Campo maior que"),
    FIELD_GTE("Campo maior ou igual a"),
    FIELD_LT("Campo menor que"),
    FIELD_LTE("Campo menor ou igual a"),

    // Funções de data/tempo
    DATE_BEFORE("Data anterior a"),
    DATE_AFTER("Data posterior a"),
    DATE_BETWEEN("Data entre"),
    TIME_BEFORE("Hora anterior a"),
    TIME_AFTER("Hora posterior a"),
    TIME_BETWEEN("Hora entre"),

    // Funções de lista/array
    ARRAY_CONTAINS("Array contém"),
    ARRAY_NOT_CONTAINS("Array não contém"),
    ARRAY_SIZE_EQ("Tamanho do array igual"),
    ARRAY_SIZE_GT("Tamanho do array maior que"),
    ARRAY_SIZE_LT("Tamanho do array menor que"),

    // Funções matemáticas
    MOD_EQ("Módulo igual"),
    MOD_NEQ("Módulo diferente"),

    // Geolocalização
    GEO_DISTANCE_LT("Distância menor que"),
    GEO_DISTANCE_GT("Distância maior que"),
    GEO_IN_POLYGON("Dentro do polígono");

    private final String description;

    OperatorType(String description) {
      this.description = description;
    }

    public String getDescription() {
      return description;
    }
  }

  /** Tipos de valor suportados */
  public enum ValueType {
    STRING("Texto"),
    NUMBER("Número"),
    BOOLEAN("Booleano"),
    DATE("Data"),
    TIME("Hora"),
    DATETIME("Data e Hora"),
    ARRAY_STRING("Lista de Textos"),
    ARRAY_NUMBER("Lista de Números"),
    FIELD_REFERENCE("Referência a Campo"),
    EXPRESSION("Expressão Calculada"),
    GEO_POINT("Ponto Geográfico"),
    GEO_POLYGON("Polígono Geográfico");

    private final String description;

    ValueType(String description) {
      this.description = description;
    }

    public String getDescription() {
      return description;
    }
  }
}
