/**
 * OPERATOR_SPECS_COMPLETE - PARTE 7 (MISSING M-N)
 */

import type { OperatorSpec } from './operatorSpecs';

export const MISSING_SPECS_M_N: Record<string, OperatorSpec> = {
  MAX_AMOUNT_LAST_N_DAYS: {
    name: "MAX_AMOUNT_LAST_N_DAYS",
    summary: "Retorna o maior valor nos últimos N dias",
    syntax: "MAX_AMOUNT_LAST_N_DAYS(customerId, 30)",
    syntaxExplanation: "Calcula o máximo histórico da janela.",
    story: "Máximo 30 dias = R$ 1.200; TX atual = R$ 5.000.",
    problem: "Como comparar com pico histórico?",
    goldenTip: "💎 Use como baseline para spikes."
  },

  MCC_ANOMALY: {
    name: "MCC_ANOMALY",
    summary: "Detecta anomalia de MCC para o cliente",
    syntax: "MCC_ANOMALY(customerId, mcc) IS_TRUE",
    syntaxExplanation: "MCC fora do padrão histórico do cliente.",
    story: "Cliente só usa supermercado e gasolina, agora MCC de gambling.",
    problem: "Como detectar mudança de categoria?",
    goldenTip: "💎 Combine com MCC_GAMBLING/MCC_CRYPTO para risco."
  },

  MCC_CATEGORY_VELOCITY: {
    name: "MCC_CATEGORY_VELOCITY",
    summary: "Conta transações por categoria MCC",
    syntax: "MCC_CATEGORY_VELOCITY(customerId, '7995', HOUR_24) GT 3",
    syntaxExplanation: "Velocidade em MCC específico.",
    story: "3 apostas em 1 hora.",
    problem: "Como limitar uso por categoria?",
    goldenTip: "💎 Use thresholds mais baixos para MCCs de risco."
  },

  MCC_CROSS_CATEGORY_PATTERN: {
    name: "MCC_CROSS_CATEGORY_PATTERN",
    summary: "Detecta padrão de múltiplas categorias em curto período",
    syntax: "MCC_CROSS_CATEGORY_PATTERN(customerId, HOUR_2) IS_TRUE",
    syntaxExplanation: "Compras em categorias não relacionadas rapidamente.",
    story: "Apostas + eletrônicos + joias em 30 minutos.",
    problem: "Como detectar spree multi-categoria?",
    goldenTip: "💎 Variação alta em curto prazo sugere card testing."
  },

  MCC_CRYPTO: {
    name: "MCC_CRYPTO",
    summary: "Verifica se MCC é de cripto",
    syntax: "MCC_CRYPTO(mcc) IS_TRUE",
    syntaxExplanation: "MCCs associados a exchanges ou serviços cripto.",
    story: "MCC 6051 = cash-like/crypto.",
    problem: "Como identificar transações cripto?",
    goldenTip: "💎 MCC cripto requer monitoramento AML reforçado."
  },

  MCC_GAMBLING: {
    name: "MCC_GAMBLING",
    summary: "Verifica se MCC é de gambling",
    syntax: "MCC_GAMBLING(mcc) IS_TRUE",
    syntaxExplanation: "MCCs associados a apostas e cassinos.",
    story: "MCC 7995 = gambling online.",
    problem: "Como identificar apostas?",
    goldenTip: "💎 Gambling tem regras específicas e maior taxa de chargeback."
  },

  MCC_SPENDING_LIMIT_CHECK: {
    name: "MCC_SPENDING_LIMIT_CHECK",
    summary: "Verifica limite de gastos por MCC",
    syntax: "MCC_SPENDING_LIMIT_CHECK(mcc, amount) EXCEEDED",
    syntaxExplanation: "Limites customizados por categoria.",
    story: "R$ 10k em fast-food excede limite.",
    problem: "Como aplicar limites por categoria?",
    goldenTip: "💎 Use dados reais para definir limites típicos."
  },

  MERCHANT_AGE_CHECK: {
    name: "MERCHANT_AGE_CHECK",
    summary: "Verifica idade do merchant",
    syntax: "MERCHANT_AGE_CHECK(merchantId) LT 30",
    syntaxExplanation: "Merchant recém-onboarded tem maior risco.",
    story: "Merchant de 7 dias processa volume alto.",
    problem: "Como avaliar risco de merchant novo?",
    goldenTip: "💎 Merchants novos devem ter limites reduzidos."
  },

  MERCHANT_AMOUNT_DISTRIBUTION: {
    name: "MERCHANT_AMOUNT_DISTRIBUTION",
    summary: "Analisa distribuição de valores do merchant",
    syntax: "MERCHANT_AMOUNT_DISTRIBUTION(merchantId) ANOMALY",
    syntaxExplanation: "Distribuição mudou ou está fora do esperado.",
    story: "Merchant normalmente recebe R$ 50-200, agora recebe R$ 5.000.",
    problem: "Como detectar mudança no ticket médio?",
    goldenTip: "💎 Mudança de distribuição indica risco de bust-out."
  },

  MERCHANT_ANOMALY: {
    name: "MERCHANT_ANOMALY",
    summary: "Detecta anomalias gerais do merchant",
    syntax: "MERCHANT_ANOMALY(merchantId) IS_TRUE",
    syntaxExplanation: "Combina sinais de volume, chargeback e comportamento.",
    story: "Merchant com múltiplos desvios simultâneos.",
    problem: "Como ter indicador geral de risco do merchant?",
    goldenTip: "💎 Use score combinado para reduzir falsos positivos."
  },

  MERCHANT_CHARGEBACK_HISTORY: {
    name: "MERCHANT_CHARGEBACK_HISTORY",
    summary: "Verifica histórico de chargebacks do merchant",
    syntax: "MERCHANT_CHARGEBACK_HISTORY(merchantId) GT 2.0",
    syntaxExplanation: "Taxa histórica de chargebacks.",
    story: "Merchant com 4% de chargeback em 90 dias.",
    problem: "Como usar histórico de chargebacks?",
    goldenTip: "💎 Chargeback alto reduz confiança do merchant."
  },

  MERCHANT_CROSS_BORDER_RATIO: {
    name: "MERCHANT_CROSS_BORDER_RATIO",
    summary: "Mede proporção de transações cross-border",
    syntax: "MERCHANT_CROSS_BORDER_RATIO(merchantId) GT 0.6",
    syntaxExplanation: "Mais de 60% internacional.",
    story: "Merchant local com 80% internacional.",
    problem: "Como detectar padrões atípicos de cross-border?",
    goldenTip: "💎 Cross-border alto pode indicar fraud farms."
  },

  MERCHANT_CUSTOMER_CONCENTRATION: {
    name: "MERCHANT_CUSTOMER_CONCENTRATION",
    summary: "Detecta concentração de receita em poucos clientes",
    syntax: "MERCHANT_CUSTOMER_CONCENTRATION(merchantId) GT 0.7",
    syntaxExplanation: "Poucos clientes geram grande parte do volume.",
    story: "Top 3 clientes geram 80% do volume.",
    problem: "Como detectar dependência excessiva?",
    goldenTip: "💎 Concentração pode indicar fraude organizada."
  },

  MERCHANT_DEVIATION: {
    name: "MERCHANT_DEVIATION",
    summary: "Detecta desvio no comportamento do merchant",
    syntax: "MERCHANT_DEVIATION(merchantId) IS_TRUE",
    syntaxExplanation: "Desvio em volume, ticket médio ou horários.",
    story: "Merchant mudou padrão noturno repentinamente.",
    problem: "Como detectar mudanças abruptas?",
    goldenTip: "💎 Desvio múltiplo é mais forte que sinal isolado."
  },

  MERCHANT_DEVICE_DIVERSITY: {
    name: "MERCHANT_DEVICE_DIVERSITY",
    summary: "Mede diversidade de devices no merchant",
    syntax: "MERCHANT_DEVICE_DIVERSITY(merchantId) LT 0.2",
    syntaxExplanation: "Baixa diversidade sugere bots.",
    story: "100 compras do mesmo device em 1 hora.",
    problem: "Como detectar ataques concentrados?",
    goldenTip: "💎 Diversidade baixa + velocidade alta = risco."
  },

  MERCHANT_DORMANT_REACTIVATION: {
    name: "MERCHANT_DORMANT_REACTIVATION",
    summary: "Detecta reativação de merchant dormente",
    syntax: "MERCHANT_DORMANT_REACTIVATION(merchantId) IS_TRUE",
    syntaxExplanation: "Merchant inativo volta com volume alto.",
    story: "Merchant parado 6 meses volta com R$ 200k.",
    problem: "Como detectar bust-out por reativação?",
    goldenTip: "💎 Reativação + volume alto = risco elevado."
  },

  MERCHANT_FIRST_SEEN: {
    name: "MERCHANT_FIRST_SEEN",
    summary: "Marca primeira aparição do merchant",
    syntax: "MERCHANT_FIRST_SEEN(merchantId) IS_TRUE",
    syntaxExplanation: "Merchant nunca visto antes.",
    story: "Primeiro dia do merchant no sistema.",
    problem: "Como detectar merchants novos?",
    goldenTip: "💎 Combine com MERCHANT_AGE_CHECK."
  },

  MERCHANT_FRAUD_RATE_CHECK: {
    name: "MERCHANT_FRAUD_RATE_CHECK",
    summary: "Verifica taxa de fraude do merchant",
    syntax: "MERCHANT_FRAUD_RATE_CHECK(merchantId) GT 0.5",
    syntaxExplanation: "Fraud rate > 0.5% é alto.",
    story: "Merchant com fraude 1.2% nos últimos 30 dias.",
    problem: "Como monitorar fraude por merchant?",
    goldenTip: "💎 Defina thresholds por MCC e volume."
  },

  MERCHANT_GEOGRAPHIC_SPREAD: {
    name: "MERCHANT_GEOGRAPHIC_SPREAD",
    summary: "Mede dispersão geográfica de compras do merchant",
    syntax: "MERCHANT_GEOGRAPHIC_SPREAD(merchantId, DAY_7) GT 2000",
    syntaxExplanation: "Compras em locais muito distantes.",
    story: "Merchant local com compras de 10 países.",
    problem: "Como detectar anomalia geográfica?",
    goldenTip: "💎 Dispersão alta sem e-commerce = suspeito."
  },

  MERCHANT_HIGH_VALUE_FREQUENCY: {
    name: "MERCHANT_HIGH_VALUE_FREQUENCY",
    summary: "Frequência de transações de alto valor no merchant",
    syntax: "MERCHANT_HIGH_VALUE_FREQUENCY(merchantId, DAY_30) GT 10",
    syntaxExplanation: "Muitas transações acima de threshold.",
    story: "20 compras > R$ 10k em um merchant pequeno.",
    problem: "Como detectar ticket alto recorrente?",
    goldenTip: "💎 Use threshold por categoria de merchant."
  },

  MERCHANT_NEW_CUSTOMER_RATIO: {
    name: "MERCHANT_NEW_CUSTOMER_RATIO",
    summary: "Proporção de clientes novos para o merchant",
    syntax: "MERCHANT_NEW_CUSTOMER_RATIO(merchantId, DAY_30) GT 0.8",
    syntaxExplanation: "Muitos clientes novos em pouco tempo.",
    story: "80% dos compradores são novos no merchant.",
    problem: "Como detectar campanhas fraudulentas?",
    goldenTip: "💎 Alto ratio + alto volume = possível fraude."
  },

  MERCHANT_REFUND_RATIO: {
    name: "MERCHANT_REFUND_RATIO",
    summary: "Proporção de reembolsos do merchant",
    syntax: "MERCHANT_REFUND_RATIO(merchantId, MONTH_1) GT 0.3",
    syntaxExplanation: "30% de reembolso = anomalia.",
    story: "Merchant reembolsa 1/3 das transações.",
    problem: "Como detectar refund abuse?",
    goldenTip: "💎 Refund ratio alto pode indicar fraude interna."
  },

  MERCHANT_REPUTATION_SCORE: {
    name: "MERCHANT_REPUTATION_SCORE",
    summary: "Score de reputação do merchant",
    syntax: "MERCHANT_REPUTATION_SCORE(merchantId) LT 40",
    syntaxExplanation: "Score baixo = histórico ruim.",
    story: "Merchant com score 30 por chargebacks e reclamações.",
    problem: "Como resumir risco do merchant?",
    goldenTip: "💎 Use score para decisões automáticas."
  },

  MERCHANT_TIME_PATTERN: {
    name: "MERCHANT_TIME_PATTERN",
    summary: "Detecta padrão temporal do merchant",
    syntax: "MERCHANT_TIME_PATTERN(merchantId) ANOMALY",
    syntaxExplanation: "Mudança em horários de operação.",
    story: "Merchant que só operava de dia passa a operar madrugada.",
    problem: "Como detectar mudança de horário de operação?",
    goldenTip: "💎 Horário noturno em merchant físico é suspeito."
  },

  MERCHANT_TRANSACTION_VOLUME: {
    name: "MERCHANT_TRANSACTION_VOLUME",
    summary: "Monitora volume de transações do merchant",
    syntax: "MERCHANT_TRANSACTION_VOLUME(merchantId, DAY_7) GT 1000",
    syntaxExplanation: "Volume acima do esperado.",
    story: "Merchant pequeno com 5.000 transações/semana.",
    problem: "Como detectar spike de volume?",
    goldenTip: "💎 Volume + ticket alto = risco elevado."
  },

  MERCHANT_VELOCITY_SPIKE: {
    name: "MERCHANT_VELOCITY_SPIKE",
    summary: "Detecta spike de velocidade no merchant",
    syntax: "MERCHANT_VELOCITY_SPIKE(merchantId, HOUR_1) IS_TRUE",
    syntaxExplanation: "Aumento súbito de transações por minuto.",
    story: "Merchant passa de 1 para 50 tx/min.",
    problem: "Como detectar bursts?",
    goldenTip: "💎 Bursts podem indicar bot ou fraude coordenada."
  },

  MICRO_DEPOSIT_VELOCITY: {
    name: "MICRO_DEPOSIT_VELOCITY",
    summary: "Detecta velocidade de microdepósitos",
    syntax: "MICRO_DEPOSIT_VELOCITY(accountId, DAY_7) GT 3",
    syntaxExplanation: "Muitos microdepósitos em curto período.",
    story: "Conta recebe vários microdepósitos de verificação.",
    problem: "Como detectar abuso de validação?",
    goldenTip: "💎 Microdepósitos repetidos podem indicar fraude de verificação."
  },

  MICRO_TRANSACTION_TEST: {
    name: "MICRO_TRANSACTION_TEST",
    summary: "Detecta transações micro para testar cartões",
    syntax: "MICRO_TRANSACTION_TEST(cardId, HOUR_24) IS_TRUE",
    syntaxExplanation: "Muitas transações de valor muito baixo.",
    story: "10 transações de R$ 1,00 em 1 hora.",
    problem: "Como detectar card testing?",
    goldenTip: "💎 Micro + alta velocidade = teste de cartão."
  },

  MIN_AMOUNT_LAST_N_DAYS: {
    name: "MIN_AMOUNT_LAST_N_DAYS",
    summary: "Retorna menor valor nos últimos N dias",
    syntax: "MIN_AMOUNT_LAST_N_DAYS(customerId, 30)",
    syntaxExplanation: "Calcula mínimo histórico da janela.",
    story: "Menor valor recente = R$ 5.",
    problem: "Como usar mínimo histórico?",
    goldenTip: "💎 Útil para detectar padrões de micro-transações."
  },

  MOD_EQ: {
    name: "MOD_EQ",
    summary: "Verifica se valor A mod B é igual a X",
    syntax: "MOD_EQ(value, divisor, expectedRemainder)",
    syntaxExplanation: "Usado para detectar padrões numéricos.",
    story: "Valores terminando sempre em 00.",
    problem: "Como detectar padrões artificiais?",
    goldenTip: "💎 Fraudes geram valores com padrões repetidos."
  },

  MOD_NEQ: {
    name: "MOD_NEQ",
    summary: "Verifica se valor A mod B é diferente de X",
    syntax: "MOD_NEQ(value, divisor, remainder)",
    syntaxExplanation: "Útil para validar formatos esperados.",
    story: "Campos com checksum inválido.",
    problem: "Como validar padrões de resto?",
    goldenTip: "💎 Use para validar códigos com dígito verificador."
  },

  MOUSE_MOVEMENT_ANOMALY: {
    name: "MOUSE_MOVEMENT_ANOMALY",
    summary: "Detecta anomalia no movimento do mouse",
    syntax: "MOUSE_MOVEMENT_ANOMALY(session) IS_TRUE",
    syntaxExplanation: "Movimentos lineares/perfeitos indicam bot.",
    story: "Mouse move em linha reta com velocidade constante.",
    problem: "Como detectar automação?",
    goldenTip: "💎 Combine com BIOMETRIC_SCROLL_VELOCITY."
  },

  MULTI_LAYERED_SYNTHETIC_ID_CONTROLS: {
    name: "MULTI_LAYERED_SYNTHETIC_ID_CONTROLS",
    summary: "Detecta controles em múltiplas camadas para IDs sintéticos",
    syntax: "MULTI_LAYERED_SYNTHETIC_ID_CONTROLS(profile) RISK",
    syntaxExplanation: "Combina sinais de identidade, device e comportamento.",
    story: "Identidade nova + telefone VoIP + endereço suspeito.",
    problem: "Como detectar synthetic IDs?",
    goldenTip: "💎 Use múltiplas camadas para evitar falsos positivos."
  },

  NAME_SIMILARITY_GT: {
    name: "NAME_SIMILARITY_GT",
    summary: "Verifica se similaridade entre nomes é maior que limiar",
    syntax: "NAME_SIMILARITY_GT(name1, name2, 85)",
    syntaxExplanation: "Score 0-100 baseado em fuzzy matching.",
    story: "'Joao Silva' vs 'João Silva' = 95.",
    problem: "Como comparar nomes com variações?",
    goldenTip: "💎 Use para CoP e screening de sanções."
  },

  NAME_SIMILARITY_LT: {
    name: "NAME_SIMILARITY_LT",
    summary: "Verifica se similaridade entre nomes é menor que limiar",
    syntax: "NAME_SIMILARITY_LT(name1, name2, 60)",
    syntaxExplanation: "Score baixo indica nomes diferentes.",
    story: "'Maria' vs 'Marcos' = 40.",
    problem: "Como rejeitar matches fracos?",
    goldenTip: "💎 Use para evitar falsos positivos em listas."
  },

  NAME_TRANSLITERATION_MATCH: {
    name: "NAME_TRANSLITERATION_MATCH",
    summary: "Verifica correspondência via transliteração",
    syntax: "NAME_TRANSLITERATION_MATCH(nameLatin, nameOriginal) IS_TRUE",
    syntaxExplanation: "Compatibiliza nomes em alfabetos diferentes.",
    story: "محمد ↔ Muhammad.",
    problem: "Como comparar nomes em alfabetos diferentes?",
    goldenTip: "💎 Essencial para sanções e PEP globais."
  },

  NAVIGATION_PATTERN_ANOMALY: {
    name: "NAVIGATION_PATTERN_ANOMALY",
    summary: "Detecta anomalia em padrão de navegação",
    syntax: "NAVIGATION_PATTERN_ANOMALY(session) IS_TRUE",
    syntaxExplanation: "Sequência de páginas não típica para humano.",
    story: "Navegação 1→100 sem tempo de leitura.",
    problem: "Como detectar scraping?",
    goldenTip: "💎 Combine com SCRAPING_DETECTION."
  },

  NEO4J_BETWEENNESS_CENTRALITY_MULE: {
    name: "NEO4J_BETWEENNESS_CENTRALITY_MULE",
    summary: "Centralidade de intermediação para detectar mulas",
    syntax: "NEO4J_BETWEENNESS_CENTRALITY_MULE(node) GT 0.2",
    syntaxExplanation: "Nós ponte em caminhos de fluxo.",
    story: "Conta no meio de várias transferências.",
    problem: "Como identificar mulas em grafos?",
    goldenTip: "💎 Betweenness alto = intermediário típico."
  },

  NEO4J_CIRCULAR_TRANSACTION_DETECTION: {
    name: "NEO4J_CIRCULAR_TRANSACTION_DETECTION",
    summary: "Detecta ciclos de transação no grafo",
    syntax: "NEO4J_CIRCULAR_TRANSACTION_DETECTION(graph) FOUND",
    syntaxExplanation: "Identifica ciclos A→B→C→A.",
    story: "Circularidade para layering AML.",
    problem: "Como detectar lavagem via ciclos?",
    goldenTip: "💎 Ciclos curtos e frequentes são mais suspeitos."
  },

  NEO4J_DEGREE_CENTRALITY: {
    name: "NEO4J_DEGREE_CENTRALITY",
    summary: "Centralidade de grau para nó no grafo",
    syntax: "NEO4J_DEGREE_CENTRALITY(node) GT 50",
    syntaxExplanation: "Muitos relacionamentos diretos.",
    story: "Conta conectada a 200 outras contas.",
    problem: "Como identificar hubs?",
    goldenTip: "💎 Hubs podem ser gateways de fraude."
  },

  NEO4J_ENTITY_RESOLUTION_SHARED_PII: {
    name: "NEO4J_ENTITY_RESOLUTION_SHARED_PII",
    summary: "Resolve entidades por PII compartilhado",
    syntax: "NEO4J_ENTITY_RESOLUTION_SHARED_PII(graph) FOUND",
    syntaxExplanation: "Detecta contas com mesmo email/telefone/endereço.",
    story: "10 contas compartilham o mesmo telefone.",
    problem: "Como detectar clusters por PII?",
    goldenTip: "💎 PII compartilhado é sinal forte de fraude organizada."
  },

  NEO4J_FIRST_PARTY_FRAUD_CLUSTERING: {
    name: "NEO4J_FIRST_PARTY_FRAUD_CLUSTERING",
    summary: "Detecta clusters de fraude first-party",
    syntax: "NEO4J_FIRST_PARTY_FRAUD_CLUSTERING(graph) FOUND",
    syntaxExplanation: "Agrupa contas com comportamento fraudulento próprio.",
    story: "Grupo de contas que sempre chargebackam.",
    problem: "Como identificar fraudadores first-party?",
    goldenTip: "💎 Use labels confirmadas para treinar clusters."
  },

  NEO4J_GRAPH_EMBEDDING_FRAUD_PREDICTION: {
    name: "NEO4J_GRAPH_EMBEDDING_FRAUD_PREDICTION",
    summary: "Predição de fraude via embeddings de grafo",
    syntax: "NEO4J_GRAPH_EMBEDDING_FRAUD_PREDICTION(node) SCORE GT 0.8",
    syntaxExplanation: "Embeddings capturam proximidade em grafo.",
    story: "Conta próxima a cluster fraudulento tem score alto.",
    problem: "Como prever risco usando grafos?",
    goldenTip: "💎 Embeddings combinam bem com modelos supervisionados."
  },

  NEO4J_LABEL_PROPAGATION_FRAUD_SPREAD: {
    name: "NEO4J_LABEL_PROPAGATION_FRAUD_SPREAD",
    summary: "Propaga labels de fraude no grafo",
    syntax: "NEO4J_LABEL_PROPAGATION_FRAUD_SPREAD(graph) FOUND",
    syntaxExplanation: "Propaga rótulos em comunidades conectadas.",
    story: "Fraude detectada se espalha para vizinhos próximos.",
    problem: "Como ampliar detecção usando relacionamentos?",
    goldenTip: "💎 Propagação exige thresholds para evitar false positives."
  },

  NEO4J_LOUVAIN_COMMUNITY_DETECTION: {
    name: "NEO4J_LOUVAIN_COMMUNITY_DETECTION",
    summary: "Detecta comunidades pelo algoritmo Louvain",
    syntax: "NEO4J_LOUVAIN_COMMUNITY_DETECTION(graph) COMMUNITIES",
    syntaxExplanation: "Agrupa nós com forte conexão interna.",
    story: "Comunidade com alta densidade de fraude.",
    problem: "Como descobrir grupos organizados?",
    goldenTip: "💎 Use para identificar anéis e gangues."
  },

  NEO4J_MONEY_MULE_NETWORK_ANALYSIS: {
    name: "NEO4J_MONEY_MULE_NETWORK_ANALYSIS",
    summary: "Analisa rede de money mules",
    syntax: "NEO4J_MONEY_MULE_NETWORK_ANALYSIS(graph) FOUND",
    syntaxExplanation: "Identifica mulas recebendo/redistribuindo fundos.",
    story: "Conta recebendo de vários e repassando rapidamente.",
    problem: "Como detectar mulas financeiras?",
    goldenTip: "💎 Use fan-in + fan-out altos + tempo curto."
  },

  NEO4J_NODE_SIMILARITY_SYNTHETIC_ID: {
    name: "NEO4J_NODE_SIMILARITY_SYNTHETIC_ID",
    summary: "Similaridade de nós para detectar IDs sintéticos",
    syntax: "NEO4J_NODE_SIMILARITY_SYNTHETIC_ID(nodeA, nodeB) GT 0.8",
    syntaxExplanation: "Similaridade por atributos compartilhados.",
    story: "Duas identidades compartilham muitos atributos.",
    problem: "Como detectar identidades sintéticas?",
    goldenTip: "💎 Similaridade alta sugere identidade fabricada."
  },

  NEO4J_PAGERANK_FRAUD_SCORE: {
    name: "NEO4J_PAGERANK_FRAUD_SCORE",
    summary: "Score de PageRank para influência fraudulenta",
    syntax: "NEO4J_PAGERANK_FRAUD_SCORE(node) GT 0.05",
    syntaxExplanation: "Nós com alta influência na rede.",
    story: "Conta central com muitas conexões fraudulentas.",
    problem: "Como detectar nós influentes de fraude?",
    goldenTip: "💎 PageRank alto em clusters suspeitos = alerta."
  },

  NEO4J_PAIRWISE_SIMILARITY_PII: {
    name: "NEO4J_PAIRWISE_SIMILARITY_PII",
    summary: "Similaridade par-a-par por PII",
    syntax: "NEO4J_PAIRWISE_SIMILARITY_PII(nodeA, nodeB) GT 0.9",
    syntaxExplanation: "Compara PII compartilhado.",
    story: "Mesma conta bancária e telefone.",
    problem: "Como medir similaridade entre entidades?",
    goldenTip: "💎 Use para entity resolution."
  },

  NEO4J_SECOND_LEVEL_FRAUDSTER_ID: {
    name: "NEO4J_SECOND_LEVEL_FRAUDSTER_ID",
    summary: "Detecta fraudadores de segundo nível",
    syntax: "NEO4J_SECOND_LEVEL_FRAUDSTER_ID(graph) FOUND",
    syntaxExplanation: "Conexões indiretas com fraudes conhecidas.",
    story: "Conta conectada a um fraudador conhecido por 2 hops.",
    problem: "Como identificar fraude por proximidade?",
    goldenTip: "💎 Segundo nível = risco médio/alto."
  },

  NEO4J_SHORTEST_PATH_AML_TRACKING: {
    name: "NEO4J_SHORTEST_PATH_AML_TRACKING",
    summary: "Encontra caminho mais curto para rastrear AML",
    syntax: "NEO4J_SHORTEST_PATH_AML_TRACKING(source, target) PATH",
    syntaxExplanation: "Menor caminho entre origem e destino.",
    story: "Rastreamento de fundos ilícitos.",
    problem: "Como traçar caminhos financeiros?",
    goldenTip: "💎 Caminhos curtos e repetidos indicam rota de lavagem."
  },

  NEO4J_TEMPORAL_MOTIF_PATTERN: {
    name: "NEO4J_TEMPORAL_MOTIF_PATTERN",
    summary: "Detecta padrões temporais no grafo",
    syntax: "NEO4J_TEMPORAL_MOTIF_PATTERN(graph, DAY_7) FOUND",
    syntaxExplanation: "Motivos temporais repetidos.",
    story: "Sequência A→B→C repetida semanalmente.",
    problem: "Como detectar padrões temporais?",
    goldenTip: "💎 Motifs ajudam a capturar comportamentos estruturados."
  },

  NEO4J_TRIANGLE_COUNT_COLLUSION: {
    name: "NEO4J_TRIANGLE_COUNT_COLLUSION",
    summary: "Conta triângulos para detectar conluio",
    syntax: "NEO4J_TRIANGLE_COUNT_COLLUSION(node) GT 5",
    syntaxExplanation: "Triângulos indicam relação mútua entre três entidades.",
    story: "Três contas transferindo entre si.",
    problem: "Como detectar conluio?",
    goldenTip: "💎 Muitos triângulos = rede altamente conectada."
  },

  NEO4J_WEAKLY_CONNECTED_COMPONENTS: {
    name: "NEO4J_WEAKLY_CONNECTED_COMPONENTS",
    summary: "Detecta componentes fracamente conectados",
    syntax: "NEO4J_WEAKLY_CONNECTED_COMPONENTS(graph) COMPONENTS",
    syntaxExplanation: "Agrupa nós conectados ignorando direção.",
    story: "Cluster isolado de contas suspeitas.",
    problem: "Como segmentar o grafo?",
    goldenTip: "💎 Útil para separar redes independentes."
  },

  NESTED_CORRESPONDENT_CHECK: {
    name: "NESTED_CORRESPONDENT_CHECK",
    summary: "Detecta correspondentes aninhados (nested)",
    syntax: "NESTED_CORRESPONDENT_CHECK(transaction) IS_TRUE",
    syntaxExplanation: "Conta de correspondência usada por outra instituição.",
    story: "Banco pequeno usa correspondência de terceiro banco.",
    problem: "Como identificar risco de nested correspondents?",
    goldenTip: "💎 Nested correspondent aumenta opacidade e risco AML."
  },

  NIGHTTIME_TRANSACTION_RATIO: {
    name: "NIGHTTIME_TRANSACTION_RATIO",
    summary: "Proporção de transações noturnas",
    syntax: "NIGHTTIME_TRANSACTION_RATIO(customerId, DAY_30) GT 0.6",
    syntaxExplanation: "Percentual de transações entre 22h-5h.",
    story: "60% das transações à noite = anomalia.",
    problem: "Como detectar comportamento noturno incomum?",
    goldenTip: "💎 Use baseline por cliente e segmento."
  }
};
