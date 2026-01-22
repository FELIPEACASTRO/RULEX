package com.rulex.entity.complex;

/**
 * ARCH-002: Enum de operadores extraído de RuleCondition.java.
 *
 * <p>Este enum define todos os operadores suportados pelo motor de regras RULEX. Foi extraído para
 * facilitar manutenção e evitar que mudanças no enum afetem a entity JPA RuleCondition.
 *
 * <p>Categorias de operadores:
 *
 * <ul>
 *   <li>Comparação básica (EQ, NEQ, GT, GTE, LT, LTE)
 *   <li>Listas (IN, NOT_IN)
 *   <li>Strings (CONTAINS, STARTS_WITH, REGEX, etc.)
 *   <li>Nulos e Booleanos
 *   <li>Range (BETWEEN)
 *   <li>Data/Tempo
 *   <li>Velocity
 *   <li>Geo/Localização
 *   <li>Graph/Neo4j
 *   <li>Estatísticos
 *   <li>AML/Regulatory
 *   <li>Behavioral
 * </ul>
 */
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
  DEVICE_MEMORY_ANOMALY, // Anomalia de memória do dispositivo

  // ========== OPERADORES V4.0 PHASE 1B (25 novos) - Behavioral ==========
  // CATEGORIA P: Behavioral Patterns (15)
  BEHAVIORAL_BASELINE_DEVIATION, // Desvio do baseline comportamental
  SPENDING_CATEGORY_SHIFT, // Mudança de categoria de gastos
  TRANSACTION_SIZE_ESCALATION, // Escalada de tamanho de transação
  FREQUENCY_PATTERN_CHANGE, // Mudança de padrão de frequência
  TIME_PREFERENCE_SHIFT, // Mudança de preferência de horário
  CHANNEL_USAGE_ANOMALY, // Anomalia de uso de canal
  PAYMENT_METHOD_SWITCH, // Troca de método de pagamento
  RECIPIENT_DIVERSITY_CHANGE, // Mudança de diversidade de destinatários
  GEOGRAPHIC_BEHAVIOR_SHIFT, // Mudança de comportamento geográfico
  SESSION_BEHAVIOR_ANOMALY, // Anomalia de comportamento de sessão
  LOGIN_PATTERN_DEVIATION, // Desvio de padrão de login
  NAVIGATION_PATTERN_ANOMALY, // Anomalia de padrão de navegação
  TRANSACTION_TIMING_CLUSTER, // Cluster de timing de transações
  AMOUNT_ROUNDING_BEHAVIOR, // Comportamento de arredondamento de valores
  SPLIT_PAYMENT_PATTERN, // Padrão de pagamento dividido

  // CATEGORIA Q: Statistical Behavioral (10)
  CHI_SQUARE_DISTRIBUTION_TEST, // Teste Chi-Square de distribuição
  KOLMOGOROV_SMIRNOV_TEST, // Teste Kolmogorov-Smirnov
  ANDERSON_DARLING_TEST, // Teste Anderson-Darling
  T_TEST_AMOUNT_DEVIATION, // T-Test de desvio de valor
  MANN_WHITNEY_U_TEST, // Teste Mann-Whitney U
  CORRELATION_ANOMALY, // Anomalia de correlação
  REGRESSION_RESIDUAL_OUTLIER, // Outlier de resíduo de regressão
  VARIANCE_RATIO_TEST, // Teste de razão de variância
  ENTROPY_SCORE_ANOMALY, // Anomalia de score de entropia
  SKEWNESS_KURTOSIS_ANOMALY, // Anomalia de skewness/kurtosis

  // ========== OPERADORES V4.0 PHASE 1C (18 novos) - MCC & Merchant ==========
  // CATEGORIA R: MCC & Merchant Advanced (18)
  MCC_CATEGORY_VELOCITY, // Velocidade por categoria MCC
  MCC_SPENDING_LIMIT_CHECK, // Verificação de limite por MCC
  MCC_CROSS_CATEGORY_PATTERN, // Padrão cross-category MCC
  MERCHANT_REPUTATION_SCORE, // Score de reputação do merchant
  MERCHANT_AGE_CHECK, // Verificação de idade do merchant
  MERCHANT_TRANSACTION_VOLUME, // Volume de transações do merchant
  MERCHANT_CHARGEBACK_HISTORY, // Histórico de chargeback do merchant
  MERCHANT_FRAUD_RATE_CHECK, // Verificação de taxa de fraude do merchant
  MERCHANT_GEOGRAPHIC_SPREAD, // Dispersão geográfica do merchant
  MERCHANT_CUSTOMER_CONCENTRATION, // Concentração de clientes do merchant
  MERCHANT_AMOUNT_DISTRIBUTION, // Distribuição de valores do merchant
  MERCHANT_TIME_PATTERN, // Padrão temporal do merchant
  MERCHANT_DEVICE_DIVERSITY, // Diversidade de dispositivos do merchant
  MERCHANT_REFUND_RATIO, // Razão de reembolso do merchant
  MERCHANT_NEW_CUSTOMER_RATIO, // Razão de novos clientes do merchant
  MERCHANT_DORMANT_REACTIVATION, // Reativação de merchant dormentes
  MERCHANT_CROSS_BORDER_RATIO, // Razão cross-border do merchant
  MERCHANT_HIGH_VALUE_FREQUENCY, // Frequência de alto valor do merchant

  // ========== OPERADORES V4.0 PHASE 2A (28 novos) - FATF AML Typologies ==========
  // CATEGORIA S: FATF AML Typologies (28) - 100% Rule-Based
  FATF_PLACEMENT_CASH_INTENSIVE, // FATF001: Placement via cash-intensive business
  FATF_PLACEMENT_STRUCTURING, // FATF002: Structuring deposits below CTR threshold
  FATF_PLACEMENT_SMURFING, // FATF003: Coordinated smurfing network
  FATF_PLACEMENT_CURRENCY_EXCHANGE, // FATF004: Large cash currency exchange
  FATF_PLACEMENT_CASINO_GAMBLING, // FATF005: Casino chip placement
  FATF_LAYERING_RAPID_MOVEMENT, // FATF006: Rapid movement of funds
  FATF_LAYERING_SHELL_COMPANY, // FATF007: Shell company transfers
  FATF_LAYERING_OFFSHORE, // FATF008: Offshore account transactions
  FATF_LAYERING_WIRE_CHAINS, // FATF009: Wire transfer chains
  FATF_LAYERING_CONVERTIBLE_INSTRUMENTS, // FATF010: Convertible instruments
  FATF_INTEGRATION_REAL_ESTATE, // FATF011: Real estate purchase integration
  FATF_INTEGRATION_LUXURY_GOODS, // FATF012: Luxury goods purchase
  FATF_INTEGRATION_BUSINESS_INVESTMENT, // FATF013: Business investment
  FATF_INTEGRATION_LOAN_REPAYMENT, // FATF014: Loan repayment with illicit funds
  FATF_TBML_OVER_INVOICING, // FATF015: Trade-based ML over-invoicing
  FATF_TBML_UNDER_INVOICING, // FATF016: Trade-based ML under-invoicing
  FATF_TBML_PHANTOM_SHIPPING, // FATF017: Phantom shipping
  FATF_TBML_MULTIPLE_INVOICING, // FATF018: Multiple invoicing same shipment
  FATF_TBML_FALSE_DESCRIPTION, // FATF019: False description of goods
  FATF_HAWALA_INFORMAL, // FATF020: Hawala/informal value transfer
  FATF_NEW_PAYMENT_EXPLOITATION, // FATF021: New payment method exploitation
  FATF_CRYPTO_MIXING, // FATF022: Cryptocurrency mixing services
  FATF_CRYPTO_ATM_CASHOUT, // FATF023: Crypto ATM cash-out
  FATF_PEP_TRANSACTION, // FATF024: PEP relationship transaction
  FATF_CORRESPONDENT_LAYERING, // FATF025: Correspondent banking layering
  FATF_ROUND_TRIPPING, // FATF026: Round-tripping investment
  FATF_BLACK_MARKET_EXCHANGE, // FATF027: Black market currency exchange
  FATF_INSURANCE_CASH_VALUE, // FATF028: Insurance policy cash value extraction

  // ========== OPERADORES V4.0 PHASE 2B (12 novos) - PSD2 SCA Exemptions ==========
  // CATEGORIA T: PSD2 SCA Exemptions (12) - 100% Rule-Based
  SCA_LOW_VALUE_EXEMPTION, // SCA001: Low value exemption (≤€30)
  SCA_CONTACTLESS_EXEMPTION, // SCA002: Contactless exemption (≤€50)
  SCA_TRA_EXEMPTION, // SCA003: Transaction risk analysis exemption
  SCA_TRUSTED_BENEFICIARY, // SCA004: Trusted beneficiary exemption
  SCA_RECURRING_TRANSACTION, // SCA005: Recurring transaction exemption
  SCA_MERCHANT_INITIATED, // SCA006: Merchant initiated transaction
  SCA_CORPORATE_PAYMENT, // SCA007: Corporate payment exemption
  SCA_SECURE_CORPORATE_PROTOCOL, // SCA008: Secure corporate payment protocol
  SCA_LIABILITY_SHIFT, // SCA009: SCA exemption liability shift check
  SCA_DYNAMIC_3DS_ROUTING, // SCA010: Dynamic 3DS exemption routing
  SCA_FRAUD_RATE_MONITORING, // SCA011: PSP fraud rate threshold monitoring
  SCA_CHALLENGE_MANDATORY, // SCA012: SCA challenge mandatory triggers

  // ========== OPERADORES V4.0 PHASE 2C (28 novos) - Platform Best Practices ==========
  // CATEGORIA U: Platform Best Practices (28) - 100% Rule-Based
  PLT_BEHAVIOR_SORTED_LISTS, // PLT001: FICO behavior sorted lists tracking
  PLT_BUSINESS_RULES_SCENARIO, // PLT002: FICO business rules scenario editor
  PLT_IDENTITY_RESOLUTION, // PLT003: FICO identity resolution engine
  PLT_COMPROMISE_MANAGER, // PLT004: FICO compromise manager
  PLT_INTELLIGENCE_NETWORK, // PLT005: FICO intelligence network
  PLT_RULES_MODELS_HYBRID, // PLT006: Feedzai rules+models hybrid
  PLT_BEHAVIORAL_PROFILING, // PLT007: Feedzai behavioral profiling
  PLT_NETWORK_ANALYTICS, // PLT008: Feedzai network analytics
  PLT_SAR_AUTOMATED, // PLT009: Feedzai SAR automated submission
  PLT_DS2_RULE_ENGINE, // PLT010: SAS DS2 rule engine
  PLT_REAL_TIME_DETECTION, // PLT011: SAS real-time detection engine
  PLT_NETWORK_ENTITY_RESOLUTION, // PLT012: SAS network entity resolution
  PLT_SCENARIO_SCORECARD, // PLT013: SAS scenario contribution scorecard
  PLT_RADAR_RULE_BACKTESTING, // PLT014: Stripe radar rule backtesting
  PLT_RADAR_METADATA_MATCHING, // PLT015: Stripe radar metadata matching
  PLT_RADAR_INLINE_LISTS, // PLT016: Stripe radar inline lists
  PLT_RADAR_COMPLEX_CONDITIONS, // PLT017: Stripe radar complex conditions
  PLT_RISK_PROFILE_ASSIGNMENT, // PLT018: Adyen risk profile assignment
  PLT_CUSTOM_RULE_BUILDER, // PLT019: Adyen custom rule builder
  PLT_RISK_LIST_COMPARISON, // PLT020: Adyen risk list comparison
  PLT_BACKTESTING_LABELING, // PLT021: Adyen backtesting labeling
  PLT_ML_FRAUD_RISK_OUTCOME, // PLT022: Adyen ML fraud risk outcome
  PLT_RISK_SCORE_CALCULATION, // PLT023: PayPal FPA risk score calculation
  PLT_VELOCITY_FILTERS, // PLT024: PayPal FPA velocity filters
  PLT_LINKING_VELOCITY, // PLT025: PayPal FPA linking velocity
  PLT_BAD_ENTITY_NETWORK, // PLT026: PayPal FPA bad entity network
  PLT_REVIEWLIST_QUEUE, // PLT027: PayPal FPA reviewlist queue
  PLT_CONSORTIUM_DATA_CHECK, // PLT028: Consortium data cross-check

  // ========== OPERADORES V4.0 PHASE 2D (14 novos) - Basel III Operational Risk ==========
  BSL_BUSINESS_INDICATOR, // BSL001: Business Indicator calculation (BI = ILDC + SC + FC)
  BSL_BUSINESS_INDICATOR_COMPONENT, // BSL002: BI component extraction (ILDC, SC, FC)
  BSL_INTERNAL_LOSS_MULTIPLIER, // BSL003: Internal Loss Multiplier (ILM) calculation
  BSL_LOSS_DATA_COLLECTION, // BSL004: Operational loss data collection validation
  BSL_LOSS_EXCLUSION_APPROVAL, // BSL005: Loss event exclusion approval workflow
  BSL_BUCKET_CLASSIFICATION, // BSL006: BI bucket classification (€1bn, €3bn, €30bn)
  BSL_MARGINAL_COEFFICIENT, // BSL007: Marginal coefficient application (12%/15%/18%)
  BSL_LOSS_THRESHOLD_SETTING, // BSL008: Loss data threshold setting (€20k default)
  BSL_RETENTION_PERIOD, // BSL009: Loss data retention period validation (10 years)
  BSL_RISK_GOVERNANCE, // BSL010: Operational risk governance framework check
  BSL_LOSS_EVENT_REPORTING, // BSL011: Operational loss event reporting compliance
  BSL_CONTROL_DEFICIENCY, // BSL012: Control deficiency tracking and remediation
  BSL_KRI_MONITORING, // BSL013: Key Risk Indicator monitoring thresholds
  BSL_SCENARIO_ANALYSIS, // BSL014: Scenario analysis capital calculation

  // ========== OPERADORES V4.0 PHASE 2E (3 novos) - Rule Mining Determinístico ==========
  APRIORI_ASSOCIATION, // APRIORI001: Apriori association rule mining (support/confidence)
  FPGROWTH_FREQUENT_PATTERNS, // FPGROWTH001: FP-Growth frequent pattern detection
  ECLAT_ITEMSET, // ECLAT001: Eclat itemset detection (vertical format)

  // ========== OPERADORES V4.0 PHASE 2F (2 novos) - Fuzzy Logic ==========
  FUZZY_MEMBERSHIP, // FUZZY001: Fuzzy membership function evaluation
  FUZZY_ADAPTIVE_THRESHOLD, // FUZZY002: Adaptive fuzzy threshold adjustment

  // ==========
  // natural

  // ========== OPERADORES V4.0 PHASE 5B (18 novos) - Neo4j Graph Fraud Detection ==========
  NEO4J_WEAKLY_CONNECTED_COMPONENTS, // NEO001: Componentes fracamente conectados (WCC)
  NEO4J_DEGREE_CENTRALITY, // NEO002: Centralidade de grau
  NEO4J_PAGERANK_FRAUD_SCORE, // NEO003: Score de fraude via PageRank
  NEO4J_LOUVAIN_COMMUNITY_DETECTION, // NEO004: Detecção de comunidade Louvain
  NEO4J_PAIRWISE_SIMILARITY_PII, // NEO005: Similaridade de PII entre pares
  NEO4J_ENTITY_RESOLUTION_SHARED_PII, // NEO006: Resolução de entidade por PII compartilhado
  NEO4J_FRAUD_RING_DETECTION, // NEO007: Detecção de anel de fraude
  NEO4J_MONEY_MULE_NETWORK_ANALYSIS, // NEO008: Análise de rede de money mules
  NEO4J_CIRCULAR_TRANSACTION_DETECTION, // NEO009: Detecção de transação circular
  NEO4J_FIRST_PARTY_FRAUD_CLUSTERING, // NEO010: Clustering de fraude de primeira parte
  NEO4J_SECOND_LEVEL_FRAUDSTER_ID, // NEO011: Identificação de fraudador de segundo nível
  NEO4J_BETWEENNESS_CENTRALITY_MULE, // NEO012: Centralidade de intermediação para mules
  NEO4J_LABEL_PROPAGATION_FRAUD_SPREAD, // NEO013: Propagação de label de fraude
  NEO4J_SHORTEST_PATH_AML_TRACKING, // NEO014: Rastreamento AML via caminho mais curto
  NEO4J_TRIANGLE_COUNT_COLLUSION, // NEO015: Contagem de triângulos para colusão
  NEO4J_NODE_SIMILARITY_SYNTHETIC_ID, // NEO016: Similaridade de nó para ID sintético
  NEO4J_GRAPH_EMBEDDING_FRAUD_PREDICTION, // NEO017: Predição de fraude via embedding de grafo
  NEO4J_TEMPORAL_MOTIF_PATTERN, // NEO018: Padrão de motif temporal

}
