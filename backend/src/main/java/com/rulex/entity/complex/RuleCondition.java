package com.rulex.entity.complex;

import jakarta.persistence.*;
import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;

/** Entidade para condições individuais com suporte a operadores avançados. */
@Entity
@Table(
    name = "rule_conditions",
    indexes = {
      @Index(name = "idx_conditions_group", columnList = "group_id"),
      @Index(name = "idx_conditions_field", columnList = "field_name")
    })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RuleCondition {

  @Id
  @GeneratedValue(strategy = GenerationType.UUID)
  private UUID id;

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "group_id", nullable = false)
  private RuleConditionGroup group;

  @Column(nullable = false)
  @Builder.Default
  private Integer position = 0;

  @Column(name = "field_name", nullable = false)
  private String fieldName;

  @Column(name = "field_path")
  private String fieldPath;

  @Enumerated(EnumType.STRING)
  @Column(nullable = false, columnDefinition = "condition_operator")
  private ConditionOperator operator;

  @Enumerated(EnumType.STRING)
  @Column(name = "value_type", nullable = false, columnDefinition = "condition_value_type")
  @Builder.Default
  private ConditionValueType valueType = ConditionValueType.STRING;

  @Column(name = "value_single")
  private String valueSingle;

  @Column(name = "value_array", columnDefinition = "TEXT[]")
  @JdbcTypeCode(SqlTypes.ARRAY)
  private List<String> valueArray;

  @Column(name = "value_min")
  private String valueMin;

  @Column(name = "value_max")
  private String valueMax;

  @Column(name = "value_field_ref")
  private String valueFieldRef;

  @Column(name = "value_expression")
  private String valueExpression;

  @Column(name = "case_sensitive", nullable = false)
  @Builder.Default
  private Boolean caseSensitive = true;

  @Column(nullable = false)
  @Builder.Default
  private Boolean negate = false;

  @Column(nullable = false)
  @Builder.Default
  private Boolean enabled = true;

  private String description;

  @Column(name = "error_message")
  private String errorMessage;

  @CreationTimestamp
  @Column(name = "created_at", nullable = false, updatable = false)
  private OffsetDateTime createdAt;

  /** Operadores de comparação suportados */
  public enum ConditionOperator {
    // Comparação básica
    EQ,
    NEQ,
    GT,
    GTE,
    LT,
    LTE,
    // Listas
    IN,
    NOT_IN,
    // Strings
    CONTAINS,
    NOT_CONTAINS,
    STARTS_WITH,
    ENDS_WITH,
    REGEX,
    NOT_REGEX,
    // Nulos
    IS_NULL,
    NOT_NULL,
    // Booleanos
    IS_TRUE,
    IS_FALSE,
    // Range
    BETWEEN,
    NOT_BETWEEN,
    // Comparação entre campos
    FIELD_EQ,
    FIELD_NEQ,
    FIELD_GT,
    FIELD_GTE,
    FIELD_LT,
    FIELD_LTE,
    // Funções de data/tempo
    DATE_BEFORE,
    DATE_AFTER,
    DATE_BETWEEN,
    TIME_BEFORE,
    TIME_AFTER,
    TIME_BETWEEN,
    // Funções de lista/array
    ARRAY_CONTAINS,
    ARRAY_NOT_CONTAINS,
    ARRAY_SIZE_EQ,
    ARRAY_SIZE_GT,
    ARRAY_SIZE_LT,
    // Funções matemáticas
    MOD_EQ,
    MOD_NEQ,
    // Geolocalização
    GEO_DISTANCE_LT,
    GEO_DISTANCE_GT,
    GEO_IN_POLYGON,
    // Velocity (agregações temporais)
    VELOCITY_COUNT_GT,
    VELOCITY_COUNT_LT,
    VELOCITY_SUM_GT,
    VELOCITY_SUM_LT,
    VELOCITY_AVG_GT,
    VELOCITY_AVG_LT,
    VELOCITY_DISTINCT_GT,
    VELOCITY_DISTINCT_LT,
    // Agregações temporais avançadas (DSL expandida)
    SUM_LAST_N_DAYS,
    COUNT_LAST_N_HOURS,
    AVG_LAST_N_DAYS,
    COUNT_DISTINCT_MERCHANTS_LAST_N_DAYS,
    COUNT_DISTINCT_COUNTRIES_LAST_N_HOURS,
    MAX_AMOUNT_LAST_N_DAYS,
    MIN_AMOUNT_LAST_N_DAYS,
    // Operadores avançados de fraude (Triple Check V36)
    GT_FIELD_MULTIPLIER, // Comparar com campo multiplicado (ex: amount > avgAmount * 5)
    DECIMAL_PLACES_GT, // Verificar casas decimais (ex: amount tem mais de 2 casas)
    EXPIRES_WITHIN_DAYS, // Verificar expiração (ex: cartão expira em N dias)
    IS_NEW, // Verificar se é novo (ex: deviceId é novo)
    IS_FIRST, // Verificar se é primeira ocorrência
    LT_CURRENT_DATE, // Comparar com data atual
    GT_CURRENT_DATE, // Comparar com data atual (futuro)
    NOT_IN_CUSTOMER_HISTORY, // Verificar se não está no histórico do cliente
    IN_CUSTOMER_HISTORY, // Verificar se está no histórico do cliente
    NOT_IN_CUSTOMER_USUAL_HOURS, // Verificar se não está no horário habitual
    IN_CUSTOMER_USUAL_HOURS, // Verificar se está no horário habitual
    IN_CUSTOMER_CHARGEBACK_MERCHANTS, // Verificar se merchant teve chargeback
    PERCENTAGE_OF_FIELD, // Calcular percentual de campo
    HOUR_BETWEEN, // Verificar se hora está entre valores
    DAY_OF_WEEK_IN, // Verificar dia da semana
    IS_WEEKEND, // Verificar se é fim de semana
    IS_HOLIDAY, // Verificar se é feriado
    DISTANCE_FROM_LAST_GT, // Distância da última transação maior que
    TIME_SINCE_LAST_LT, // Tempo desde última transação menor que
    COUNT_FAILURES_LAST_N_HOURS, // Contagem de falhas nas últimas N horas
    SUM_LAST_N_HOURS, // Soma nas últimas N horas
    COUNT_DISTINCT_MERCHANTS_LAST_N_HOURS, // Merchants distintos nas últimas N horas
    VELOCITY_SPIKE, // Spike de velocidade comparado com média
    AMOUNT_SPIKE, // Spike de valor comparado com média
    PATTERN_ESCALATION, // Padrão de escada (valores crescentes)
    PATTERN_ROUND_NUMBERS, // Padrão de valores redondos
    PATTERN_SPLIT_TRANSACTION, // Padrão de split de transação

    // Operadores críticos para regras de fraude avançadas
    NOT_IN_HISTORICAL, // Verificar se valor não está no histórico
    NAME_SIMILARITY_LT, // Similaridade de nome menor que threshold
    GTE_PERCENT_OF_LAST_INCOMING, // Valor >= X% do último crédito recebido
    DOMAIN_IN_LIST, // Domínio do email está em lista
    CHARGEBACK_RATE_GT, // Taxa de chargeback do merchant maior que
    ACCOUNT_AGE_LT_MINUTES, // Idade da conta menor que N minutos
    IS_VOIP, // Telefone é VoIP
    COUNT_DISTINCT_PANS_LAST_N_HOURS, // PANs distintos nas últimas N horas
    COUNT_DISTINCT_ACCOUNTS, // Contas distintas associadas ao campo

    // Operadores adicionais para migrações V28-V30
    IN_LIST, // Alias para IN (compatibilidade com migrações)
    HAS_FAILED_3DS_LAST_N_MINUTES, // Houve falha 3DS nos últimos N minutos
    COUNT_MFA_ABANDONMENTS, // Contagem de abandonos de MFA
    HAS_INCOMING_TRANSFER_LAST_N_HOURS, // Houve transferência de entrada nas últimas N horas
    IS_IMPOSSIBLE_COMBINATION, // Combinação impossível de dados
    PIX_KEY_CHANGED_LAST_N_DAYS, // Chave PIX alterada nos últimos N dias
    CONTAINS_SUSPICIOUS_KEYWORDS, // Contém palavras-chave suspeitas
    COUNT_CRYPTO_TXN_LAST_N_DAYS, // Contagem de transações crypto nos últimos N dias
    COUNT_DISTINCT_INSTRUMENTS_LAST_N_DAYS, // Instrumentos distintos nos últimos N dias
    COUNT_DISTINCT_PAYERS_LAST_N_DAYS, // Pagadores distintos nos últimos N dias
    COUNT_DISTINCT_USER_AGENTS_LAST_N_HOURS, // User agents distintos nas últimas N horas
    COUNT_LAST_N_DAYS, // Contagem nos últimos N dias
    COUNT_MFA_DENIALS_LAST_N_HOURS, // Contagem de negações MFA nas últimas N horas
    DAYS_SINCE_LAST_ACTIVITY, // Dias desde última atividade
    DEVICE_CHANGED_IN_SESSION, // Device mudou na sessão
    IS_CRYPTO_RANSOM_AMOUNT, // Valor típico de ransom crypto
    OUTFLOW_RATE_LAST_N_DAYS, // Taxa de saída nos últimos N dias

    // ========== OPERADORES V31+ (82 novos) - Regras Parametrizáveis ==========
    // CATEGORIA A: Velocity Avançado (10)
    VELOCITY_CROSS_CHANNEL, // Velocidade entre canais diferentes
    VELOCITY_ROLLING_WINDOW, // Janela deslizante de velocidade
    VELOCITY_PERCENTILE, // Valor no percentil de velocidade
    VELOCITY_RATIO_GT, // Razão de velocidade maior que
    VELOCITY_TREND, // Tendência de velocidade (crescente/decrescente)
    COUNT_UNIQUE_BENEFICIARIES_LAST_N_DAYS, // Beneficiários únicos nos últimos N dias
    COUNT_UNIQUE_IPS_LAST_N_HOURS, // IPs únicos nas últimas N horas
    SUM_BY_CHANNEL_LAST_N_DAYS, // Soma por canal nos últimos N dias
    AVG_INTERVAL_BETWEEN_TXN, // Intervalo médio entre transações
    VELOCITY_ACCELERATION, // Aceleração de velocidade

    // CATEGORIA B: Behavioral Rules (8)
    DORMANCY_REVIVAL, // Conta dormiente reativada
    AMOUNT_DEVIATION_FROM_AVG, // Desvio do valor médio
    TIME_DEVIATION_FROM_USUAL, // Desvio do horário usual
    MERCHANT_DEVIATION, // Desvio de merchant usual
    MICRO_TRANSACTION_TEST, // Teste com micro-transação
    LOCATION_DEVIATION, // Desvio de localização usual
    CHANNEL_SWITCH_PATTERN, // Padrão de troca de canal
    BENEFICIARY_REUSE_PATTERN, // Padrão de reutilização de beneficiário

    // CATEGORIA C: Graph/Network (8)
    FAN_OUT_COUNT, // Contagem de fan-out (N beneficiários)
    FAN_IN_COUNT, // Contagem de fan-in (N origens)
    SHARED_DEVICE_COUNT, // Dispositivos compartilhados
    SHARED_IP_COUNT, // IPs compartilhados
    ACCOUNT_LINK_DEPTH, // Profundidade de links de conta
    CIRCULAR_TRANSFER_DETECTION, // Detecção de transferência circular
    RAPID_MULTI_HOP, // Múltiplos saltos rápidos
    BENEFICIARY_CONCENTRATION, // Concentração de beneficiários

    // CATEGORIA D: Sanctions & Name Matching (7)
    OFAC_LIST_CHECK, // Verificação lista OFAC
    PEP_LIST_CHECK, // Verificação lista PEP
    ADVERSE_MEDIA_CHECK, // Verificação mídia adversa
    SANCTIONS_COUNTRY_CHECK, // País sancionado
    HIGH_RISK_JURISDICTION, // Jurisdição de alto risco
    NAME_TRANSLITERATION_MATCH, // Match de transliteração de nome
    ALIAS_DETECTION, // Detecção de alias

    // CATEGORIA E: Synthetic ID Detection (8)
    CPF_SSN_VALIDATION, // Validação de CPF/SSN
    PHONE_CARRIER_CHECK, // Verificação de operadora
    EMAIL_DOMAIN_AGE, // Idade do domínio do email
    ADDRESS_VERIFICATION, // Verificação de endereço
    IDENTITY_VELOCITY, // Velocidade de identidade
    DEVICE_ACCOUNT_RATIO, // Razão dispositivo/conta
    EMAIL_PHONE_MISMATCH, // Incompatibilidade email/telefone
    CREDIT_FILE_THIN, // Arquivo de crédito fino

    // CATEGORIA F: AML Typology (8)
    STRUCTURING_DETECTION, // Detecção de estruturação (smurfing)
    LAYERING_PATTERN, // Padrão de camadas
    RAPID_MOVEMENT, // Movimento rápido de fundos
    INTEGRATION_PATTERN, // Padrão de integração
    CASH_INTENSIVE_RATIO, // Razão de intensidade de cash
    UNUSUAL_BUSINESS_PATTERN, // Padrão de negócio incomum
    SHELL_COMPANY_INDICATOR, // Indicador de empresa de fachada
    TRADE_BASED_ML_INDICATOR, // Indicador de lavagem baseada em comércio

    // CATEGORIA G: Regulatory (8)
    SCA_EXEMPTION_TRA, // Isenção SCA por TRA
    SCA_EXEMPTION_LOW_VALUE, // Isenção SCA por baixo valor
    SCA_EXEMPTION_TRUSTED_BENEFICIARY, // Isenção SCA por beneficiário confiável
    SCA_EXEMPTION_RECURRING, // Isenção SCA por recorrência
    PSD3_COP_NAME_MATCH, // Match de nome CoP PSD3
    DORA_INCIDENT_SEVERITY, // Severidade de incidente DORA
    EIDAS_ASSURANCE_LEVEL, // Nível de assurance eIDAS
    GDPR_DATA_RETENTION_CHECK, // Verificação retenção GDPR

    // CATEGORIA H: Device (7)
    DEVICE_JAILBREAK_ROOTED, // Device jailbroken/rooted
    EMULATOR_DETECTION, // Detecção de emulador
    VPN_PROXY_DETECTION, // Detecção de VPN/Proxy
    TOR_EXIT_NODE, // Nó de saída Tor
    BROWSER_INCONSISTENCY, // Inconsistência de browser
    TIMEZONE_MISMATCH, // Incompatibilidade de timezone
    LANGUAGE_MISMATCH, // Incompatibilidade de idioma

    // CATEGORIA I: Merchant & MCC (7)
    MCC_HIGH_RISK, // MCC de alto risco
    MCC_GAMBLING, // MCC de apostas
    MCC_CRYPTO, // MCC de crypto
    MERCHANT_FIRST_SEEN, // Merchant visto pela primeira vez
    MERCHANT_COUNTRY_MISMATCH, // Incompatibilidade país do merchant
    MERCHANT_CATEGORY_CHANGE, // Mudança de categoria do merchant
    MERCHANT_VELOCITY_SPIKE, // Spike de velocidade do merchant

    // CATEGORIA J: ISO 20022 (6)
    PACS008_FIELD_VALIDATION, // Validação campos PACS008
    REMITTANCE_INFO_ANALYSIS, // Análise de remittance info
    PURPOSE_CODE_MISMATCH, // Incompatibilidade de código de propósito
    UETR_DUPLICATE_CHECK, // Verificação UETR duplicado
    CREDITOR_NAME_VALIDATION, // Validação nome do credor
    STRUCTURED_ADDRESS_CHECK, // Verificação endereço estruturado

    // CATEGORIA K: Estatísticos Simples (5)
    BENFORD_LAW_DEVIATION, // Desvio da Lei de Benford
    Z_SCORE_GT, // Z-Score maior que
    STANDARD_DEVIATION_GT, // Desvio padrão maior que
    PERCENTILE_GT, // Percentil maior que
    COEFFICIENT_VARIATION_GT, // Coeficiente de variação maior que

    // ========== OPERADORES V4.0 PHASE 1 (40 novos) - Velocity + Device ==========
    // CATEGORIA L: Transaction Count Velocity Avançado (12)
    TRANSACTION_COUNT_PER_CARD_HOUR, // Contagem de transações por cartão por hora
    TRANSACTION_COUNT_PER_IP_HOUR, // Contagem de transações por IP por hora
    TRANSACTION_COUNT_PER_DEVICE_DAY, // Contagem de transações por dispositivo por dia
    TRANSACTION_COUNT_PER_MERCHANT_HOUR, // Contagem de transações por merchant por hora
    TRANSACTION_COUNT_PER_CUSTOMER_HOUR, // Contagem de transações por cliente por hora
    UNIQUE_CARD_COUNT_PER_IP_HOUR, // Cartões únicos por IP por hora
    UNIQUE_MERCHANT_COUNT_PER_CARD_DAY, // Merchants únicos por cartão por dia
    TRANSACTION_ATTEMPT_COUNT_PER_CARD, // Tentativas de transação por cartão
    CVV_FAILURE_VELOCITY, // Velocidade de falhas CVV
    ADDRESS_CHANGE_VELOCITY, // Velocidade de alteração de endereço
    BENEFICIARY_ADD_VELOCITY, // Velocidade de adição de beneficiários
    CARD_ADD_VELOCITY, // Velocidade de adição de cartões

    // CATEGORIA M: Amount Velocity Avançado (10)
    AMOUNT_SUM_PER_CARD_HOUR, // Soma de valores por cartão por hora
    AMOUNT_SUM_PER_CUSTOMER_DAY, // Soma de valores por cliente por dia
    AVG_TRANSACTION_SPIKE, // Spike de valor médio de transação
    LARGE_AMOUNT_FREQUENCY, // Frequência de valores altos
    SMALL_AMOUNT_VELOCITY, // Velocidade de valores pequenos (smurfing)
    ROUND_AMOUNT_FREQUENCY, // Frequência de valores redondos
    SEQUENTIAL_AMOUNT_PATTERN, // Padrão sequencial de valores
    AMOUNT_VARIANCE_ANOMALY, // Anomalia de variância de valores
    DAILY_LIMIT_PROXIMITY, // Proximidade do limite diário
    WEEKLY_LIMIT_PROXIMITY, // Proximidade do limite semanal

    // CATEGORIA N: Temporal Velocity Avançado (8)
    TIME_BETWEEN_CONSECUTIVE_TX, // Tempo entre transações consecutivas
    TRANSACTION_FREQUENCY_ANOMALY, // Anomalia de frequência de transações
    TIME_OF_DAY_ANOMALY, // Anomalia de horário do dia
    DORMANCY_ALERT_VELOCITY, // Alerta de velocidade pós-dormência
    WEEKEND_VS_WEEKDAY_PATTERN, // Padrão fim de semana vs dia útil
    HOLIDAY_TRANSACTION_SPIKE, // Spike de transações em feriados
    NIGHTTIME_TRANSACTION_RATIO, // Razão de transações noturnas
    BUSINESS_HOURS_DEVIATION, // Desvio do horário comercial

    // CATEGORIA O: Device Fingerprint Avançado (10)
    DEVICE_TRUST_SCORE, // Score de confiança do dispositivo
    CANVAS_FINGERPRINT_MISMATCH, // Incompatibilidade de fingerprint canvas
    WEBGL_FINGERPRINT_ANOMALY, // Anomalia de fingerprint WebGL
    AUDIO_FINGERPRINT_NEW, // Fingerprint de áudio novo
    FONTS_FINGERPRINT_ANOMALY, // Anomalia de fingerprint de fontes
    SCREEN_RESOLUTION_CHANGE, // Mudança de resolução de tela
    BATTERY_LEVEL_ANOMALY, // Anomalia de nível de bateria
    HARDWARE_CONCURRENCY_MISMATCH, // Incompatibilidade de concorrência de hardware
    TOUCH_SUPPORT_INCONSISTENCY, // Inconsistência de suporte touch
    DEVICE_MEMORY_ANOMALY // Anomalia de memória do dispositivo
  }

  /** Tipos de valor suportados */
  public enum ConditionValueType {
    STRING,
    NUMBER,
    BOOLEAN,
    DATE,
    TIME,
    DATETIME,
    ARRAY_STRING,
    ARRAY_NUMBER,
    FIELD_REFERENCE,
    EXPRESSION,
    GEO_POINT,
    GEO_POLYGON
  }
}
