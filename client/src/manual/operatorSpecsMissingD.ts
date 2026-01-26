/**
 * OPERATOR_SPECS_COMPLETE - PARTE 8 (MISSING NOT-SYNTHETIC)
 */

import type { OperatorSpec } from './operatorSpecs';

export const MISSING_SPECS_NOT_S: Record<string, OperatorSpec> = {
  NOT_BETWEEN: {
    name: "NOT_BETWEEN",
    summary: "Verifica se valor NÃO está entre dois limites",
    syntax: "amount NOT_BETWEEN 100 AND 1000",
    syntaxExplanation: "Retorna true se valor estiver fora do intervalo.",
    story: "Valores fora da faixa normal do cliente.",
    problem: "Como detectar valores fora de faixa?",
    goldenTip: "💎 Use com BETWEEN para regras espelhadas."
  },

  NOT_CONTAINS: {
    name: "NOT_CONTAINS",
    summary: "Verifica se string NÃO contém substring",
    syntax: "email NOT_CONTAINS '@company.com'",
    syntaxExplanation: "Retorna true quando substring não aparece.",
    story: "Email não corporativo em cadastro corporativo.",
    problem: "Como validar ausência de conteúdo?",
    goldenTip: "💎 Combine com CONTAINS para filtros completos."
  },

  NOT_IN_CUSTOMER_HISTORY: {
    name: "NOT_IN_CUSTOMER_HISTORY",
    summary: "Verifica se entidade não está no histórico do cliente",
    syntax: "NOT_IN_CUSTOMER_HISTORY(customerId, merchantId)",
    syntaxExplanation: "Merchant nunca visto pelo cliente.",
    story: "Primeira compra em merchant desconhecido.",
    problem: "Como tratar entidades inéditas?",
    goldenTip: "💎 Inédito + valor alto = alerta."
  },

  NOT_IN_LIST: {
    name: "NOT_IN_LIST",
    summary: "Verifica se valor NÃO está em uma lista",
    syntax: "NOT_IN_LIST(ip, 'blocked_ips') IS_TRUE",
    syntaxExplanation: "Retorna true se não estiver na lista.",
    story: "IP não consta na blacklist.",
    problem: "Como validar ausência em listas?",
    goldenTip: "💎 Use com IN_LIST para regras complementares."
  },

  NOT_NULL: {
    name: "NOT_NULL",
    summary: "Verifica se campo não é nulo",
    syntax: "NOT_NULL(field)",
    syntaxExplanation: "Retorna true quando campo existe e não é null.",
    story: "Campo obrigatório informado.",
    problem: "Como validar preenchimento?",
    goldenTip: "💎 Use com IS_EMPTY para diferenciar null vs vazio."
  },

  NOT_REGEX: {
    name: "NOT_REGEX",
    summary: "Verifica se string NÃO corresponde ao regex",
    syntax: "NOT_REGEX(email, '^[^@]+@[^@]+$')",
    syntaxExplanation: "Retorna true quando regex não casa.",
    story: "Email inválido detectado.",
    problem: "Como validar formato negativo?",
    goldenTip: "💎 Regex muito estrito pode rejeitar casos válidos."
  },

  OFFLINE_PIN_FAILED: {
    name: "OFFLINE_PIN_FAILED",
    summary: "Detecta falha de PIN offline",
    syntax: "OFFLINE_PIN_FAILED(transaction) IS_TRUE",
    syntaxExplanation: "Falha na verificação de PIN do chip.",
    story: "Tentativas de PIN errado em POS.",
    problem: "Como detectar tentativas de PIN?",
    goldenTip: "💎 PIN offline falhado + fallback = suspeito."
  },

  OUTFLOW_RATE_LAST_N_DAYS: {
    name: "OUTFLOW_RATE_LAST_N_DAYS",
    summary: "Mede taxa de saída de fundos nos últimos N dias",
    syntax: "OUTFLOW_RATE_LAST_N_DAYS(accountId, 30) GT 0.8",
    syntaxExplanation: "Percentual de saída vs entrada.",
    story: "Sai 90% do que entra rapidamente.",
    problem: "Como detectar drenagem de conta?",
    goldenTip: "💎 OUTFLOW alto + entradas recentes = risco AML."
  },

  PACS008_FIELD_VALIDATION: {
    name: "PACS008_FIELD_VALIDATION",
    summary: "Valida campos ISO20022 pacs.008",
    syntax: "PACS008_FIELD_VALIDATION(message) VALID",
    syntaxExplanation: "Verifica formatos e campos obrigatórios.",
    story: "Mensagem pacs.008 inválida é rejeitada.",
    problem: "Como validar mensagens de pagamento?",
    goldenTip: "💎 Campos inválidos podem indicar fraude ou erro sistêmico."
  },

  PATTERN_ESCALATION: {
    name: "PATTERN_ESCALATION",
    summary: "Detecta escalada de padrão (intensificação)",
    syntax: "PATTERN_ESCALATION(customerId) IS_TRUE",
    syntaxExplanation: "Frequência e valores aumentam progressivamente.",
    story: "R$ 100 → 300 → 900 em sequência.",
    problem: "Como detectar escalada?",
    goldenTip: "💎 Escalada é típica de fraude em progressão."
  },

  PATTERN_ROUND_NUMBERS: {
    name: "PATTERN_ROUND_NUMBERS",
    summary: "Detecta padrão de valores redondos",
    syntax: "PATTERN_ROUND_NUMBERS(customerId) IS_TRUE",
    syntaxExplanation: "Valores sem centavos repetidamente.",
    story: "R$ 100, R$ 200, R$ 300...",
    problem: "Como detectar valores artificiais?",
    goldenTip: "💎 Valores reais geralmente têm centavos."
  },

  PATTERN_SPLIT_TRANSACTION: {
    name: "PATTERN_SPLIT_TRANSACTION",
    summary: "Detecta divisão de transações para burlar limites",
    syntax: "PATTERN_SPLIT_TRANSACTION(customerId) IS_TRUE",
    syntaxExplanation: "Múltiplas transações próximas abaixo do limite.",
    story: "5 transações de R$ 1.990 com limite de R$ 2.000.",
    problem: "Como detectar splitting?",
    goldenTip: "💎 Combine com ROUND_AMOUNT_FREQUENCY."
  },

  PAYMENT_METHOD_SWITCH: {
    name: "PAYMENT_METHOD_SWITCH",
    summary: "Detecta mudança de método de pagamento",
    syntax: "PAYMENT_METHOD_SWITCH(customerId) IS_TRUE",
    syntaxExplanation: "Ex: cartão → PIX → boleto.",
    story: "Cliente que só usava cartão agora usa PIX alto.",
    problem: "Como detectar mudança de método?",
    goldenTip: "💎 Mudança + valor alto = risco."
  },

  PEER_GROUP_DEVIATION_SCORE: {
    name: "PEER_GROUP_DEVIATION_SCORE",
    summary: "Score de desvio em relação a grupo de pares",
    syntax: "PEER_GROUP_DEVIATION_SCORE(customerId, peerGroup) GT 0.8",
    syntaxExplanation: "Compara cliente com similares.",
    story: "Cliente foge muito do grupo de renda/idade.",
    problem: "Como comparar com peers?",
    goldenTip: "💎 Use segmentação correta para evitar viés."
  },

  PERCENTAGE_OF_FIELD: {
    name: "PERCENTAGE_OF_FIELD",
    summary: "Calcula porcentagem de um campo",
    syntax: "PERCENTAGE_OF_FIELD(amount, balance) GT 80",
    syntaxExplanation: "amount/balance*100.",
    story: "Transferência de 90% do saldo.",
    problem: "Como avaliar proporção?",
    goldenTip: "💎 Valores altos indicam possível drenagem."
  },

  PHONE_CARRIER_CHECK: {
    name: "PHONE_CARRIER_CHECK",
    summary: "Verifica operadora do telefone",
    syntax: "PHONE_CARRIER_CHECK(phone) IN ['CarrierX','CarrierY']",
    syntaxExplanation: "Operadora pode indicar VoIP ou números descartáveis.",
    story: "Número associado a operadora VoIP.",
    problem: "Como validar telefonia?",
    goldenTip: "💎 Combine com IS_VOIP."
  },

  PHONE_COUNTRY_MISMATCH: {
    name: "PHONE_COUNTRY_MISMATCH",
    summary: "Detecta mismatch entre país do telefone e da conta",
    syntax: "PHONE_COUNTRY_MISMATCH(phone, customerCountry) IS_TRUE",
    syntaxExplanation: "DDI não condiz com país declarado.",
    story: "Cliente BR com telefone +91.",
    problem: "Como detectar inconsistências geográficas?",
    goldenTip: "💎 Pode ser expatriado. Use contexto adicional."
  },

  PIN_CVV_LIMIT_EXCEEDED: {
    name: "PIN_CVV_LIMIT_EXCEEDED",
    summary: "Detecta excesso de tentativas de PIN/CVV",
    syntax: "PIN_CVV_LIMIT_EXCEEDED(cardId, HOUR_1) IS_TRUE",
    syntaxExplanation: "Muitas tentativas falhas.",
    story: "5 tentativas de CVV em 30 minutos.",
    problem: "Como detectar brute force?",
    goldenTip: "💎 Bloqueie cartão após limite."
  },

  PIX_KEY_CHANGED_LAST_N_DAYS: {
    name: "PIX_KEY_CHANGED_LAST_N_DAYS",
    summary: "Detecta mudança de chave PIX recente",
    syntax: "PIX_KEY_CHANGED_LAST_N_DAYS(customerId, 7) IS_TRUE",
    syntaxExplanation: "Chave PIX alterada recentemente.",
    story: "Troca de chave seguida de transferência alta.",
    problem: "Como detectar fraude em PIX?",
    goldenTip: "💎 Chave nova + valor alto = alerta."
  },

  PLT_BACKTESTING_LABELING: {
    name: "PLT_BACKTESTING_LABELING",
    summary: "Suporte a labeling para backtesting",
    syntax: "PLT_BACKTESTING_LABELING(ruleId) ENABLED",
    syntaxExplanation: "Marca eventos para análise retroativa.",
    story: "Regra nova testada em histórico sem impactar produção.",
    problem: "Como validar regras antes de ativar?",
    goldenTip: "💎 Sempre use backtest para calibrar thresholds."
  },

  PLT_BAD_ENTITY_NETWORK: {
    name: "PLT_BAD_ENTITY_NETWORK",
    summary: "Detecta rede de entidades maliciosas",
    syntax: "PLT_BAD_ENTITY_NETWORK(graph) FOUND",
    syntaxExplanation: "Detecta clusters de entidades suspeitas.",
    story: "Rede de contas com shared PII.",
    problem: "Como identificar redes de fraude?",
    goldenTip: "💎 Use graph analytics e entity resolution."
  },

  PLT_BEHAVIOR_SORTED_LISTS: {
    name: "PLT_BEHAVIOR_SORTED_LISTS",
    summary: "Listas ordenadas por comportamento",
    syntax: "PLT_BEHAVIOR_SORTED_LISTS(metric) GENERATED",
    syntaxExplanation: "Rankeia entidades por risco/comportamento.",
    story: "Top 100 contas mais arriscadas do dia.",
    problem: "Como priorizar revisões?",
    goldenTip: "💎 Ordene por risco e impacto financeiro."
  },

  PLT_BEHAVIORAL_PROFILING: {
    name: "PLT_BEHAVIORAL_PROFILING",
    summary: "Perfil comportamental de usuários",
    syntax: "PLT_BEHAVIORAL_PROFILING(customerId) PROFILE",
    syntaxExplanation: "Constrói baseline de horários, valores e canais.",
    story: "Perfil mostra que cliente compra só de manhã.",
    problem: "Como criar baseline individual?",
    goldenTip: "💎 Perfis reduzem falsos positivos."
  },

  PLT_BUSINESS_RULES_SCENARIO: {
    name: "PLT_BUSINESS_RULES_SCENARIO",
    summary: "Cenário de regras de negócio",
    syntax: "PLT_BUSINESS_RULES_SCENARIO(scenario) RUN",
    syntaxExplanation: "Simula regras em cenários predefinidos.",
    story: "Cenário: Black Friday com pico de volume.",
    problem: "Como testar regras em situações extremas?",
    goldenTip: "💎 Simulações evitam surpresas em produção."
  },

  PLT_COMPROMISE_MANAGER: {
    name: "PLT_COMPROMISE_MANAGER",
    summary: "Gerenciador de comprometimento de contas",
    syntax: "PLT_COMPROMISE_MANAGER(accountId) FLAGGED",
    syntaxExplanation: "Centraliza flags de ATO e ações.",
    story: "Conta sinalizada e bloqueada automaticamente.",
    problem: "Como orquestrar resposta a ATO?",
    goldenTip: "💎 Integre com workflows de recuperação de conta."
  },

  PLT_CONSORTIUM_DATA_CHECK: {
    name: "PLT_CONSORTIUM_DATA_CHECK",
    summary: "Consulta dados de consórcio",
    syntax: "PLT_CONSORTIUM_DATA_CHECK(entity) HIT",
    syntaxExplanation: "Busca sinais reconhecidos pelo consórcio.",
    story: "Device visto em outra instituição.",
    problem: "Como usar inteligência coletiva?",
    goldenTip: "💎 Hits de consórcio têm alta confiança."
  },

  PLT_CUSTOM_RULE_BUILDER: {
    name: "PLT_CUSTOM_RULE_BUILDER",
    summary: "Suporte a criação de regras customizadas",
    syntax: "PLT_CUSTOM_RULE_BUILDER() AVAILABLE",
    syntaxExplanation: "Permite montar regras por DSL.",
    story: "Time de risco cria regra sem dev.",
    problem: "Como permitir autonomia?",
    goldenTip: "💎 Use versionamento e aprovação antes de publicar."
  },

  PLT_DS2_RULE_ENGINE: {
    name: "PLT_DS2_RULE_ENGINE",
    summary: "Integração com rule engine DS2",
    syntax: "PLT_DS2_RULE_ENGINE(rule) EXECUTED",
    syntaxExplanation: "Executa regras complexas em tempo real.",
    story: "Regra DS2 processa evento com baixa latência.",
    problem: "Como escalar execução de regras?",
    goldenTip: "💎 Use caching e pré-cálculo de features."
  },

  PLT_IDENTITY_RESOLUTION: {
    name: "PLT_IDENTITY_RESOLUTION",
    summary: "Resolução de identidade entre entidades",
    syntax: "PLT_IDENTITY_RESOLUTION(profile) RESOLVED",
    syntaxExplanation: "Une múltiplos registros da mesma pessoa.",
    story: "Dois emails e telefones apontam para mesma pessoa.",
    problem: "Como evitar duplicidade?",
    goldenTip: "💎 Melhor resolução = menos fraudes de identidade."
  },

  PLT_INTELLIGENCE_NETWORK: {
    name: "PLT_INTELLIGENCE_NETWORK",
    summary: "Rede de inteligência de fraude",
    syntax: "PLT_INTELLIGENCE_NETWORK() ACTIVE",
    syntaxExplanation: "Compartilha sinais de risco internamente.",
    story: "Detecção em um canal ativa alerta em outro.",
    problem: "Como unificar sinais?",
    goldenTip: "💎 Centralize sinais para decisões consistentes."
  },

  PLT_LINKING_VELOCITY: {
    name: "PLT_LINKING_VELOCITY",
    summary: "Velocidade de criação de links entre entidades",
    syntax: "PLT_LINKING_VELOCITY(graph, HOUR_24) GT 50",
    syntaxExplanation: "Muitos links novos em pouco tempo.",
    story: "Centenas de dispositivos ligados a contas novas.",
    problem: "Como detectar linking suspeito?",
    goldenTip: "💎 Linking rápido indica fraude organizada."
  },

  PLT_ML_FRAUD_RISK_OUTCOME: {
    name: "PLT_ML_FRAUD_RISK_OUTCOME",
    summary: "Score de risco gerado por modelo ML",
    syntax: "PLT_ML_FRAUD_RISK_OUTCOME(event) GT 0.8",
    syntaxExplanation: "Score 0-1 do modelo.",
    story: "Modelo ML indica 0.92 de fraude.",
    problem: "Como integrar ML com regras?",
    goldenTip: "💎 Use ML como um sinal ponderado."
  },

  PLT_NETWORK_ANALYTICS: {
    name: "PLT_NETWORK_ANALYTICS",
    summary: "Análise de rede para detectar padrões",
    syntax: "PLT_NETWORK_ANALYTICS(graph) RESULT",
    syntaxExplanation: "Gera métricas de centralidade, comunidades etc.",
    story: "Detecção de cluster suspeito.",
    problem: "Como usar grafos para fraude?",
    goldenTip: "💎 Combine com NEO4J_* métricas."
  },

  PLT_NETWORK_ENTITY_RESOLUTION: {
    name: "PLT_NETWORK_ENTITY_RESOLUTION",
    summary: "Resolução de entidades em rede",
    syntax: "PLT_NETWORK_ENTITY_RESOLUTION(graph) RESOLVED",
    syntaxExplanation: "Identifica duplicidades e conexões ocultas.",
    story: "Várias contas vinculadas ao mesmo device.",
    problem: "Como consolidar identidades?",
    goldenTip: "💎 Reduz fraude por múltiplas contas."
  },

  PLT_RADAR_COMPLEX_CONDITIONS: {
    name: "PLT_RADAR_COMPLEX_CONDITIONS",
    summary: "Suporte a condições complexas no Radar",
    syntax: "PLT_RADAR_COMPLEX_CONDITIONS(rule) SUPPORTED",
    syntaxExplanation: "Permite combinações avançadas de sinais.",
    story: "Regra com múltiplos AND/OR e subcondições.",
    problem: "Como criar regras complexas?",
    goldenTip: "💎 Documente regras para evitar ambiguidade."
  },

  PLT_RADAR_INLINE_LISTS: {
    name: "PLT_RADAR_INLINE_LISTS",
    summary: "Listas inline para regras Radar",
    syntax: "PLT_RADAR_INLINE_LISTS(list) AVAILABLE",
    syntaxExplanation: "Listas embutidas na regra.",
    story: "Lista rápida de MCCs de risco.",
    problem: "Como usar listas rápidas?",
    goldenTip: "💎 Prefira listas centralizadas para manutenção."
  },

  PLT_RADAR_METADATA_MATCHING: {
    name: "PLT_RADAR_METADATA_MATCHING",
    summary: "Matching por metadados",
    syntax: "PLT_RADAR_METADATA_MATCHING(event) MATCH",
    syntaxExplanation: "Compara metadados de requests e transações.",
    story: "Meta indica origem suspeita.",
    problem: "Como usar metadados para detecção?",
    goldenTip: "💎 Metadados inconsistentes indicam spoofing."
  },

  PLT_RADAR_RULE_BACKTESTING: {
    name: "PLT_RADAR_RULE_BACKTESTING",
    summary: "Backtesting de regras",
    syntax: "PLT_RADAR_RULE_BACKTESTING(ruleId) RESULT",
    syntaxExplanation: "Executa regra em histórico.",
    story: "Regra teria bloqueado 12% das fraudes.",
    problem: "Como avaliar eficácia de regra?",
    goldenTip: "💎 Use métricas de precisão/recall."
  },

  PLT_REAL_TIME_DETECTION: {
    name: "PLT_REAL_TIME_DETECTION",
    summary: "Detecção em tempo real",
    syntax: "PLT_REAL_TIME_DETECTION(event) EXECUTED",
    syntaxExplanation: "Processamento com baixa latência.",
    story: "Regra bloqueia transação em 80ms.",
    problem: "Como garantir baixa latência?",
    goldenTip: "💎 Pré-compute features e use caches."
  },

  PLT_REVIEWLIST_QUEUE: {
    name: "PLT_REVIEWLIST_QUEUE",
    summary: "Fila de revisão manual",
    syntax: "PLT_REVIEWLIST_QUEUE(case) ENQUEUED",
    syntaxExplanation: "Cases enviados para analistas.",
    story: "Transações suspeitas vão para review.",
    problem: "Como priorizar investigação?",
    goldenTip: "💎 Priorize por risco e impacto financeiro."
  },

  PLT_RISK_LIST_COMPARISON: {
    name: "PLT_RISK_LIST_COMPARISON",
    summary: "Comparação com listas de risco",
    syntax: "PLT_RISK_LIST_COMPARISON(entity) HIT",
    syntaxExplanation: "Consulta listas internas/externas.",
    story: "Email aparece em blacklist.",
    problem: "Como usar listas para decisão?",
    goldenTip: "💎 Combine com scoring para reduzir falsos positivos."
  },

  PLT_RISK_PROFILE_ASSIGNMENT: {
    name: "PLT_RISK_PROFILE_ASSIGNMENT",
    summary: "Atribui perfil de risco",
    syntax: "PLT_RISK_PROFILE_ASSIGNMENT(customerId) PROFILE",
    syntaxExplanation: "Classifica cliente em baixo/médio/alto risco.",
    story: "Cliente PJ com alto volume = high risk.",
    problem: "Como segmentar risco?",
    goldenTip: "💎 Perfis dinâmicos evitam desatualização."
  },

  PLT_RISK_SCORE_CALCULATION: {
    name: "PLT_RISK_SCORE_CALCULATION",
    summary: "Calcula score de risco consolidado",
    syntax: "PLT_RISK_SCORE_CALCULATION(event) SCORE",
    syntaxExplanation: "Agrega múltiplos sinais.",
    story: "Score 82 = revisão manual.",
    problem: "Como ter score único?",
    goldenTip: "💎 Documente pesos para auditabilidade."
  },

  PLT_RULES_MODELS_HYBRID: {
    name: "PLT_RULES_MODELS_HYBRID",
    summary: "Combina regras e modelos",
    syntax: "PLT_RULES_MODELS_HYBRID(event) DECISION",
    syntaxExplanation: "Híbrido de regras determinísticas + ML.",
    story: "Regra filtra e ML refina decisão.",
    problem: "Como equilibrar regras e ML?",
    goldenTip: "💎 Use regras para explicabilidade."
  },

  PLT_SAR_AUTOMATED: {
    name: "PLT_SAR_AUTOMATED",
    summary: "Automatiza geração de SAR",
    syntax: "PLT_SAR_AUTOMATED(case) GENERATED",
    syntaxExplanation: "Cria relatório de atividade suspeita.",
    story: "SAR gerado automaticamente para caso crítico.",
    problem: "Como automatizar compliance?",
    goldenTip: "💎 Sempre revise SARs gerados automaticamente."
  },

  PLT_SCENARIO_SCORECARD: {
    name: "PLT_SCENARIO_SCORECARD",
    summary: "Scorecard por cenário",
    syntax: "PLT_SCENARIO_SCORECARD(scenario) SCORE",
    syntaxExplanation: "Mede performance de regras por cenário.",
    story: "Scorecard mostra alta detecção em phishing.",
    problem: "Como avaliar regras por cenário?",
    goldenTip: "💎 Ajuda a priorizar melhorias."
  },

  PLT_VELOCITY_FILTERS: {
    name: "PLT_VELOCITY_FILTERS",
    summary: "Filtros de velocidade pré-configurados",
    syntax: "PLT_VELOCITY_FILTERS(profile) APPLY",
    syntaxExplanation: "Aplicação rápida de limites padrão.",
    story: "Template de velocity para contas novas.",
    problem: "Como agilizar configuração de regras?",
    goldenTip: "💎 Use templates por segmento."
  },

  POS_SECURITY_MISSING: {
    name: "POS_SECURITY_MISSING",
    summary: "Detecta ausência de segurança no POS",
    syntax: "POS_SECURITY_MISSING(transaction) IS_TRUE",
    syntaxExplanation: "POS sem EMV/sem PIN/sem criptografia.",
    story: "POS sem chip em transação presencial.",
    problem: "Como detectar POS inseguros?",
    goldenTip: "💎 POS inseguro aumenta risco de skimming."
  },

  PSD3_COP_NAME_MATCH: {
    name: "PSD3_COP_NAME_MATCH",
    summary: "Confirmation of Payee (PSD3): match de nome",
    syntax: "PSD3_COP_NAME_MATCH(payerName, payeeName) MATCH",
    syntaxExplanation: "Confirma que o nome do beneficiário confere.",
    story: "Nome informado não confere com conta destino.",
    problem: "Como reduzir APP fraud?",
    goldenTip: "💎 CoP é obrigatório em PSD3 para alguns fluxos."
  },

  PURPOSE_CODE_MISMATCH: {
    name: "PURPOSE_CODE_MISMATCH",
    summary: "Detecta incompatibilidade de purpose code",
    syntax: "PURPOSE_CODE_MISMATCH(transaction) IS_TRUE",
    syntaxExplanation: "Purpose code não condiz com produto/segmento.",
    story: "Purpose code de importação em transação doméstica.",
    problem: "Como validar purpose codes?",
    goldenTip: "💎 Mismatch é sinal de tentativa de mascarar finalidade."
  },

  RAPID_MOVEMENT: {
    name: "RAPID_MOVEMENT",
    summary: "Detecta movimentação rápida de fundos",
    syntax: "RAPID_MOVEMENT(accountId, HOUR_1) IS_TRUE",
    syntaxExplanation: "Entrada e saída em poucos minutos.",
    story: "Recebe e transfere em 5 minutos.",
    problem: "Como detectar layering rápido?",
    goldenTip: "💎 Rapid movement é típico de lavagem."
  },

  RAPID_MULTI_HOP: {
    name: "RAPID_MULTI_HOP",
    summary: "Detecta múltiplos hops em sequência rápida",
    syntax: "RAPID_MULTI_HOP(network, HOUR_1) IS_TRUE",
    syntaxExplanation: "Funds passam por várias contas rapidamente.",
    story: "A→B→C→D em 30 minutos.",
    problem: "Como detectar multi-hop?",
    goldenTip: "💎 Use graph analytics para detectar cadeias."
  },

  RAPID_SUCCESSION_PATTERN: {
    name: "RAPID_SUCCESSION_PATTERN",
    summary: "Detecta transações em sucessão rápida",
    syntax: "RAPID_SUCCESSION_PATTERN(accountId, MINUTE_5) IS_TRUE",
    syntaxExplanation: "Muitas transações em poucos minutos.",
    story: "10 transações em 2 minutos.",
    problem: "Como detectar rajadas?",
    goldenTip: "💎 Combine com velocity e valor."
  },

  RECIPIENT_DIVERSITY_CHANGE: {
    name: "RECIPIENT_DIVERSITY_CHANGE",
    summary: "Detecta mudança na diversidade de destinatários",
    syntax: "RECIPIENT_DIVERSITY_CHANGE(accountId) IS_TRUE",
    syntaxExplanation: "Passa de poucos para muitos destinatários.",
    story: "De 2 destinatários para 20 em um mês.",
    problem: "Como detectar expansão suspeita?",
    goldenTip: "💎 Diversidade alta indica distribuição para mulas."
  },

  RECIPIENT_IN_WATCHLIST: {
    name: "RECIPIENT_IN_WATCHLIST",
    summary: "Verifica se destinatário está em watchlist",
    syntax: "RECIPIENT_IN_WATCHLIST(recipientId) IS_TRUE",
    syntaxExplanation: "Lista de contas suspeitas.",
    story: "Destinatário já ligado a fraude.",
    problem: "Como bloquear destinatários de risco?",
    goldenTip: "💎 Hits em watchlist devem acionar bloqueio."
  },

  RECIPIENT_IS_NEW: {
    name: "RECIPIENT_IS_NEW",
    summary: "Verifica se destinatário é novo para o cliente",
    syntax: "RECIPIENT_IS_NEW(customerId, recipientId) IS_TRUE",
    syntaxExplanation: "Primeira transferência para este destinatário.",
    story: "Primeiro envio para nova conta.",
    problem: "Como detectar beneficiários novos?",
    goldenTip: "💎 Novo destinatário + valor alto = alerta."
  },

  REGRESSION_RESIDUAL_OUTLIER: {
    name: "REGRESSION_RESIDUAL_OUTLIER",
    summary: "Detecta outlier em resíduos de regressão",
    syntax: "REGRESSION_RESIDUAL_OUTLIER(model, event) IS_TRUE",
    syntaxExplanation: "Diferença entre previsto e observado é grande.",
    story: "Modelo previa R$ 200, ocorreu R$ 2.000.",
    problem: "Como detectar desvios de modelo?",
    goldenTip: "💎 Use para monitorar drift e outliers."
  },

  REMITTANCE_INFO_ANALYSIS: {
    name: "REMITTANCE_INFO_ANALYSIS",
    summary: "Analisa informações de remessa",
    syntax: "REMITTANCE_INFO_ANALYSIS(message) SUSPICIOUS",
    syntaxExplanation: "Analisa texto de remessa para padrões suspeitos.",
    story: "Mensagens vagas ou inconsistentes.",
    problem: "Como analisar remittance info?",
    goldenTip: "💎 Termos genéricos e repetitivos podem indicar fraude."
  },

  ROUND_AMOUNT: {
    name: "ROUND_AMOUNT",
    summary: "Detecta valores redondos",
    syntax: "ROUND_AMOUNT(amount) IS_TRUE",
    syntaxExplanation: "Sem centavos (ex: 1000.00).",
    story: "Transações sempre em múltiplos de 100.",
    problem: "Como detectar valores artificiais?",
    goldenTip: "💎 Valores redondos são comuns em fraude e test."
  },

  ROUND_AMOUNT_FREQUENCY: {
    name: "ROUND_AMOUNT_FREQUENCY",
    summary: "Frequência de valores redondos",
    syntax: "ROUND_AMOUNT_FREQUENCY(customerId, DAY_30) GT 0.7",
    syntaxExplanation: "Mais de 70% de valores redondos.",
    story: "Cliente só faz R$ 500, R$ 1000.",
    problem: "Como detectar padrão de arredondamento?",
    goldenTip: "💎 Combine com PATTERN_ROUND_NUMBERS."
  },

  ROUND_TRIP_DETECTION: {
    name: "ROUND_TRIP_DETECTION",
    summary: "Detecta round-trip (fundos voltam à origem)",
    syntax: "ROUND_TRIP_DETECTION(network, DAY_7) IS_TRUE",
    syntaxExplanation: "A→B→A em curto período.",
    story: "Dinheiro sai e retorna em 24h.",
    problem: "Como detectar round-tripping?",
    goldenTip: "💎 Round-trip é típico de layering AML."
  },

  SCA_CHALLENGE_MANDATORY: {
    name: "SCA_CHALLENGE_MANDATORY",
    summary: "Define se challenge SCA é obrigatório",
    syntax: "SCA_CHALLENGE_MANDATORY(transaction) IS_TRUE",
    syntaxExplanation: "Força challenge independentemente de isenções.",
    story: "Transação com risco alto exige challenge.",
    problem: "Como forçar SCA?",
    goldenTip: "💎 Use quando sinais de risco são fortes."
  },

  SCA_CONTACTLESS_EXEMPTION: {
    name: "SCA_CONTACTLESS_EXEMPTION",
    summary: "Isenção de SCA para contactless",
    syntax: "SCA_CONTACTLESS_EXEMPTION(amount) IS_TRUE",
    syntaxExplanation: "Pequenos valores contactless podem ser isentos.",
    story: "Pagamento de R$ 20 sem SCA.",
    problem: "Como aplicar isenção contactless?",
    goldenTip: "💎 Existem limites acumulados para isenção."
  },

  SCA_CORPORATE_PAYMENT: {
    name: "SCA_CORPORATE_PAYMENT",
    summary: "Isenção SCA para pagamentos corporativos",
    syntax: "SCA_CORPORATE_PAYMENT(transaction) IS_TRUE",
    syntaxExplanation: "Pagamentos B2B com processo seguro.",
    story: "Empresa com protocolo seguro dispensada de SCA.",
    problem: "Como aplicar isenção corporativa?",
    goldenTip: "💎 Requer controles certificados."
  },

  SCA_DYNAMIC_3DS_ROUTING: {
    name: "SCA_DYNAMIC_3DS_ROUTING",
    summary: "Roteamento dinâmico para 3DS",
    syntax: "SCA_DYNAMIC_3DS_ROUTING(transaction) ROUTE",
    syntaxExplanation: "Define fluxo 3DS frictionless vs challenge.",
    story: "Baixo risco vai frictionless; alto risco vai challenge.",
    problem: "Como escolher fluxo 3DS?",
    goldenTip: "💎 Use score de risco para roteamento."
  },

  SCA_EXEMPTION_TRUSTED_BENEFICIARY: {
    name: "SCA_EXEMPTION_TRUSTED_BENEFICIARY",
    summary: "Isenção de SCA para beneficiário confiável",
    syntax: "SCA_EXEMPTION_TRUSTED_BENEFICIARY(customerId, beneficiaryId) IS_TRUE",
    syntaxExplanation: "Beneficiário na whitelist do cliente.",
    story: "Transferência para beneficiário já aprovado.",
    problem: "Como reduzir fricção para beneficiários confiáveis?",
    goldenTip: "💎 Whitelist deve ser controlada pelo cliente."
  },

  SCA_FRAUD_RATE_MONITORING: {
    name: "SCA_FRAUD_RATE_MONITORING",
    summary: "Monitora taxa de fraude para isenções SCA",
    syntax: "SCA_FRAUD_RATE_MONITORING(issuer) LT 0.13",
    syntaxExplanation: "Fraud rate define limites de isenção.",
    story: "Fraud rate alto reduz limites de isenção.",
    problem: "Como manter elegibilidade a isenções?",
    goldenTip: "💎 Reduzir fraude mantém isenções mais amplas."
  },

  SCA_LIABILITY_SHIFT: {
    name: "SCA_LIABILITY_SHIFT",
    summary: "Verifica se há shift de responsabilidade",
    syntax: "SCA_LIABILITY_SHIFT(transaction) YES",
    syntaxExplanation: "3DS/SCA pode transferir responsabilidade.",
    story: "Com SCA, liability shift para o emissor.",
    problem: "Como reduzir risco de chargeback?",
    goldenTip: "💎 SCA bem aplicado reduz disputas."
  },

  SCA_LOW_VALUE_EXEMPTION: {
    name: "SCA_LOW_VALUE_EXEMPTION",
    summary: "Isenção de SCA para baixo valor",
    syntax: "SCA_LOW_VALUE_EXEMPTION(amount) IS_TRUE",
    syntaxExplanation: "Valores baixos podem ser isentos.",
    story: "Compra de R$ 15 sem SCA.",
    problem: "Como aplicar isenção low-value?",
    goldenTip: "💎 Limites acumulados devem ser controlados."
  },

  SCA_MERCHANT_INITIATED: {
    name: "SCA_MERCHANT_INITIATED",
    summary: "Transação iniciada pelo merchant (MIT)",
    syntax: "SCA_MERCHANT_INITIATED(transaction) IS_TRUE",
    syntaxExplanation: "Ex: assinatura recorrente pós-autorização.",
    story: "Cobranca mensal de assinatura.",
    problem: "Como tratar MIT?",
    goldenTip: "💎 MIT requer consentimento inicial com SCA."
  },

  SCA_RECURRING_TRANSACTION: {
    name: "SCA_RECURRING_TRANSACTION",
    summary: "Isenção SCA para transações recorrentes",
    syntax: "SCA_RECURRING_TRANSACTION(transaction) IS_TRUE",
    syntaxExplanation: "Recorrências fixas podem ser isentas.",
    story: "Netflix cobrança mensal.",
    problem: "Como aplicar isenção para recorrência?",
    goldenTip: "💎 Só após primeira transação autenticada."
  },

  SCA_SECURE_CORPORATE_PROTOCOL: {
    name: "SCA_SECURE_CORPORATE_PROTOCOL",
    summary: "Protocolo corporativo seguro (isencao SCA)",
    syntax: "SCA_SECURE_CORPORATE_PROTOCOL(transaction) IS_TRUE",
    syntaxExplanation: "Pagamentos B2B com controles seguros.",
    story: "Empresa com assinaturas digitais internas.",
    problem: "Como aplicar isenções corporativas?",
    goldenTip: "💎 Requer processos certificados e auditados."
  },

  SCA_TRA_EXEMPTION: {
    name: "SCA_TRA_EXEMPTION",
    summary: "Isenção por Transaction Risk Analysis (TRA)",
    syntax: "SCA_TRA_EXEMPTION(transaction) IS_TRUE",
    syntaxExplanation: "Baixo risco permite isenção.",
    story: "Cliente confiável e baixo valor.",
    problem: "Como aplicar TRA?",
    goldenTip: "💎 Requer fraud rate abaixo de thresholds."
  },

  SCA_TRUSTED_BENEFICIARY: {
    name: "SCA_TRUSTED_BENEFICIARY",
    summary: "Beneficiário confiável para isenção SCA",
    syntax: "SCA_TRUSTED_BENEFICIARY(customerId, beneficiaryId) IS_TRUE",
    syntaxExplanation: "Beneficiário autorizado pelo cliente.",
    story: "Transferência para conta de familiares.",
    problem: "Como reduzir fricção em transferências frequentes?",
    goldenTip: "💎 Logue aprovação do cliente."
  },

  SCREEN_RESOLUTION_CHANGE: {
    name: "SCREEN_RESOLUTION_CHANGE",
    summary: "Detecta mudança de resolução de tela",
    syntax: "SCREEN_RESOLUTION_CHANGE(session) IS_TRUE",
    syntaxExplanation: "Resolução muda abruptamente.",
    story: "Sessão muda de 1920x1080 para 800x600.",
    problem: "Como detectar spoofing de device?",
    goldenTip: "💎 Mudança pode indicar VM ou remote control."
  },

  SECURITY: {
    name: "SECURITY",
    summary: "Marcador geral de segurança",
    syntax: "SECURITY(event) FLAG",
    syntaxExplanation: "Sinaliza evento para controles de segurança.",
    story: "Evento com múltiplos sinais de risco.",
    problem: "Como centralizar sinalização?",
    goldenTip: "💎 Use para encaminhar a pipelines de segurança."
  },

  SEQUENTIAL_AMOUNT_PATTERN: {
    name: "SEQUENTIAL_AMOUNT_PATTERN",
    summary: "Detecta valores sequenciais",
    syntax: "SEQUENTIAL_AMOUNT_PATTERN(customerId) IS_TRUE",
    syntaxExplanation: "Valores com incrementos regulares.",
    story: "R$ 100, 200, 300 em sequência.",
    problem: "Como detectar padrões artificiais?",
    goldenTip: "💎 Sequências regulares são típicas de testes."
  },

  SESSION_BEHAVIOR_ANOMALY: {
    name: "SESSION_BEHAVIOR_ANOMALY",
    summary: "Detecta anomalia comportamental na sessão",
    syntax: "SESSION_BEHAVIOR_ANOMALY(session) IS_TRUE",
    syntaxExplanation: "Velocidade, navegação e interações anormais.",
    story: "Sessão sem mouse e sem scroll.",
    problem: "Como detectar bots?",
    goldenTip: "💎 Combine com BOT_DETECTION."
  },

  SESSION_DURATION_LT: {
    name: "SESSION_DURATION_LT",
    summary: "Verifica se duração da sessão é menor que N",
    syntax: "SESSION_DURATION_LT(session, 10) IS_TRUE",
    syntaxExplanation: "Sessão muito curta pode indicar automação.",
    story: "Login e transação em 3 segundos.",
    problem: "Como detectar ações rápidas demais?",
    goldenTip: "💎 Humanos levam tempo para preencher formulários."
  },

  SHARED_DEVICE_COUNT: {
    name: "SHARED_DEVICE_COUNT",
    summary: "Conta quantas contas compartilham o mesmo device",
    syntax: "SHARED_DEVICE_COUNT(deviceId) GT 5",
    syntaxExplanation: "Device associado a muitas contas.",
    story: "Device usado por 12 contas.",
    problem: "Como detectar farms de contas?",
    goldenTip: "💎 Shared device é forte sinal de fraude."
  },

  SHARED_IP_COUNT: {
    name: "SHARED_IP_COUNT",
    summary: "Conta quantas contas compartilham o mesmo IP",
    syntax: "SHARED_IP_COUNT(ip) GT 20",
    syntaxExplanation: "Muitos usuários no mesmo IP.",
    story: "IP de data center usado por 100 contas.",
    problem: "Como detectar proxies e farms?",
    goldenTip: "💎 IP de data center + muitos usuários = risco alto."
  },

  SHELL_BANK_INDICATOR: {
    name: "SHELL_BANK_INDICATOR",
    summary: "Indica banco de fachada",
    syntax: "SHELL_BANK_INDICATOR(entity) IS_TRUE",
    syntaxExplanation: "Banco sem presença física real.",
    story: "Instituição sem licença clara.",
    problem: "Como detectar shell banks?",
    goldenTip: "💎 Shell banks são proibidos em muitas jurisdições."
  },

  SHELL_COMPANY_INDICATOR: {
    name: "SHELL_COMPANY_INDICATOR",
    summary: "Indica empresa de fachada",
    syntax: "SHELL_COMPANY_INDICATOR(company) IS_TRUE",
    syntaxExplanation: "Empresa sem operações reais.",
    story: "Empresa sem funcionários e com alto volume.",
    problem: "Como detectar shell companies?",
    goldenTip: "💎 Verifique UBO, endereço e atividade."
  },

  SKEWNESS_KURTOSIS_ANOMALY: {
    name: "SKEWNESS_KURTOSIS_ANOMALY",
    summary: "Detecta anomalia combinada de skewness e kurtosis",
    syntax: "SKEWNESS_KURTOSIS_ANOMALY(values) IS_TRUE",
    syntaxExplanation: "Distribuição com assimetria e caudas pesadas.",
    story: "Muitos extremos e distribuição assimétrica.",
    problem: "Como detectar distribuição anormal?",
    goldenTip: "💎 Combine com Z_SCORE e IQR."
  },

  SMALL_AMOUNT_VELOCITY: {
    name: "SMALL_AMOUNT_VELOCITY",
    summary: "Detecta velocidade de pequenas transações",
    syntax: "SMALL_AMOUNT_VELOCITY(cardId, HOUR_1) GT 10",
    syntaxExplanation: "Muitas transações pequenas em curto período.",
    story: "10 compras de R$ 1,00 em 5 minutos.",
    problem: "Como detectar card testing?",
    goldenTip: "💎 Pequenos valores + velocidade = teste."
  },

  SPENDING_CATEGORY_SHIFT: {
    name: "SPENDING_CATEGORY_SHIFT",
    summary: "Detecta mudança de categoria de gastos",
    syntax: "SPENDING_CATEGORY_SHIFT(customerId) IS_TRUE",
    syntaxExplanation: "Categorias de consumo mudam abruptamente.",
    story: "Cliente só comprava mercado e agora compra joias.",
    problem: "Como detectar mudança de padrão?",
    goldenTip: "💎 Mudança + valor alto = alerta."
  },

  SPLIT_TRANSACTION_DETECTION: {
    name: "SPLIT_TRANSACTION_DETECTION",
    summary: "Detecta transações divididas",
    syntax: "SPLIT_TRANSACTION_DETECTION(customerId) IS_TRUE",
    syntaxExplanation: "Divisão para burlar limites.",
    story: "2x R$ 4.900 com limite R$ 5.000.",
    problem: "Como detectar divisão de compras?",
    goldenTip: "💎 Combine com PATTERN_SPLIT_TRANSACTION."
  },

  STRUCTURED_ADDRESS_CHECK: {
    name: "STRUCTURED_ADDRESS_CHECK",
    summary: "Valida endereço estruturado",
    syntax: "STRUCTURED_ADDRESS_CHECK(address) IS_VALID",
    syntaxExplanation: "Verifica campos obrigatórios e coerência.",
    story: "CEP e cidade incompatíveis.",
    problem: "Como validar endereços estruturados?",
    goldenTip: "💎 Normalize com ADDRESS_STANDARDIZATION."
  },

  SUM_BY_CHANNEL_LAST_N_DAYS: {
    name: "SUM_BY_CHANNEL_LAST_N_DAYS",
    summary: "Soma valores por canal nos últimos N dias",
    syntax: "SUM_BY_CHANNEL_LAST_N_DAYS(customerId, 'WEB', 7) GT 3000",
    syntaxExplanation: "Agregação por canal.",
    story: "Web soma R$ 5k em 7 dias.",
    problem: "Como limitar volume por canal?",
    goldenTip: "💎 Use limites diferentes por canal."
  },

  SUSPICIOUS: {
    name: "SUSPICIOUS",
    summary: "Marca evento como suspeito",
    syntax: "SUSPICIOUS() IS_TRUE",
    syntaxExplanation: "Flag genérica para investigação.",
    story: "Evento com múltiplos sinais divergentes.",
    problem: "Como sinalizar suspeita geral?",
    goldenTip: "💎 Use como gatilho para revisão manual."
  },

  SUSPICIOUS_TERMINAL: {
    name: "SUSPICIOUS_TERMINAL",
    summary: "Detecta terminal suspeito",
    syntax: "SUSPICIOUS_TERMINAL(terminalId) IS_TRUE",
    syntaxExplanation: "Terminal com histórico de fraude.",
    story: "Terminal apareceu em 5 casos de fraude.",
    problem: "Como marcar terminais de risco?",
    goldenTip: "💎 Bloqueie terminais confirmados."
  },

  SUSPICIOUS_TRANSACTION_TYPE: {
    name: "SUSPICIOUS_TRANSACTION_TYPE",
    summary: "Detecta tipo de transação suspeito",
    syntax: "SUSPICIOUS_TRANSACTION_TYPE(txType) IS_TRUE",
    syntaxExplanation: "Tipos incomuns para o perfil.",
    story: "Cliente PF realizando transação corporativa.",
    problem: "Como validar tipo de transação?",
    goldenTip: "💎 Tipos incomuns devem exigir validação."
  },

  SYNTHETIC_FRAUD_SCORE: {
    name: "SYNTHETIC_FRAUD_SCORE",
    summary: "Score de risco para identidade sintética",
    syntax: "SYNTHETIC_FRAUD_SCORE(profile) GT 0.8",
    syntaxExplanation: "Score baseado em múltiplos sinais.",
    story: "Score 0.9 indica synthetic ID provável.",
    problem: "Como avaliar risco de identidades sintéticas?",
    goldenTip: "💎 Use sinais de PII, device e bureau."
  },

  SYNTHETIC_ID_LABEL_CORRECTION: {
    name: "SYNTHETIC_ID_LABEL_CORRECTION",
    summary: "Correção de labels para IDs sintéticos",
    syntax: "SYNTHETIC_ID_LABEL_CORRECTION(case) APPLIED",
    syntaxExplanation: "Ajusta rótulos para melhorar treinamento.",
    story: "Caso reclassificado após investigação.",
    problem: "Como manter labels precisos?",
    goldenTip: "💎 Labels corretos melhoram modelos e regras."
  },

  SYNTHETIC_IDENTITY_RING: {
    name: "SYNTHETIC_IDENTITY_RING",
    summary: "Detecta anel de identidades sintéticas",
    syntax: "SYNTHETIC_IDENTITY_RING(graph) FOUND",
    syntaxExplanation: "Cluster de contas com sinais de synthetic ID.",
    story: "Várias contas compartilham PII parcial.",
    problem: "Como identificar rings de synthetic IDs?",
    goldenTip: "💎 Use graph + entity resolution."
  }
};
