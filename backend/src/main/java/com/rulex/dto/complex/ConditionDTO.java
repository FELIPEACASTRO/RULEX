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
    GEO_IN_POLYGON("Dentro do polígono"),

    // Velocity (agregações temporais)
    VELOCITY_COUNT_GT("Contagem de velocidade maior que"),
    VELOCITY_COUNT_LT("Contagem de velocidade menor que"),
    VELOCITY_SUM_GT("Soma de velocidade maior que"),
    VELOCITY_SUM_LT("Soma de velocidade menor que"),
    VELOCITY_AVG_GT("Média de velocidade maior que"),
    VELOCITY_AVG_LT("Média de velocidade menor que"),
    VELOCITY_DISTINCT_GT("Distintos de velocidade maior que"),
    VELOCITY_DISTINCT_LT("Distintos de velocidade menor que"),

    // Agregações temporais avançadas
    SUM_LAST_N_DAYS("Soma nos últimos N dias"),
    COUNT_LAST_N_HOURS("Contagem nas últimas N horas"),
    AVG_LAST_N_DAYS("Média nos últimos N dias"),
    COUNT_DISTINCT_MERCHANTS_LAST_N_DAYS("Merchants distintos nos últimos N dias"),
    COUNT_DISTINCT_COUNTRIES_LAST_N_HOURS("Países distintos nas últimas N horas"),
    MAX_AMOUNT_LAST_N_DAYS("Valor máximo nos últimos N dias"),
    MIN_AMOUNT_LAST_N_DAYS("Valor mínimo nos últimos N dias"),

    // Operadores avançados de fraude
    IN_LIST("Em lista (alias)"),
    NOT_IN_HISTORICAL("Não está no histórico"),
    NAME_SIMILARITY_LT("Similaridade de nome menor que"),
    GTE_PERCENT_OF_LAST_INCOMING("Maior ou igual a % do último crédito"),
    DOMAIN_IN_LIST("Domínio em lista"),
    CHARGEBACK_RATE_GT("Taxa de chargeback maior que"),
    ACCOUNT_AGE_LT_MINUTES("Idade da conta menor que N minutos"),
    IS_VOIP("É telefone VoIP"),
    COUNT_DISTINCT_PANS_LAST_N_HOURS("PANs distintos nas últimas N horas"),
    COUNT_DISTINCT_ACCOUNTS("Contas distintas"),
    HAS_FAILED_3DS_LAST_N_MINUTES("Falha 3DS nos últimos N minutos"),
    COUNT_MFA_ABANDONMENTS("Abandonos de MFA"),
    HAS_INCOMING_TRANSFER_LAST_N_HOURS("Transferência recebida nas últimas N horas"),
    IS_IMPOSSIBLE_COMBINATION("Combinação impossível"),
    PIX_KEY_CHANGED_LAST_N_DAYS("Chave PIX alterada nos últimos N dias"),
    CONTAINS_SUSPICIOUS_KEYWORDS("Contém palavras suspeitas"),
    COUNT_CRYPTO_TXN_LAST_N_DAYS("Transações crypto nos últimos N dias"),
    COUNT_DISTINCT_INSTRUMENTS_LAST_N_DAYS("Instrumentos distintos nos últimos N dias"),
    COUNT_DISTINCT_PAYERS_LAST_N_DAYS("Pagadores distintos nos últimos N dias"),
    COUNT_DISTINCT_USER_AGENTS_LAST_N_HOURS("User agents distintos nas últimas N horas"),
    COUNT_LAST_N_DAYS("Contagem nos últimos N dias"),
    COUNT_MFA_DENIALS_LAST_N_HOURS("Negações MFA nas últimas N horas"),
    DAYS_SINCE_LAST_ACTIVITY("Dias desde última atividade"),
    DEVICE_CHANGED_IN_SESSION("Device mudou na sessão"),
    IS_CRYPTO_RANSOM_AMOUNT("Valor típico de ransom"),
    OUTFLOW_RATE_LAST_N_DAYS("Taxa de saída nos últimos N dias");

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
