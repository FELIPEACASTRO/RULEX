/**
 * OPERATOR_SPECS_COMPLETE - PARTE 2
 * Continuação da documentação ULTRA DIDÁTICA
 * 
 * NEO4J/GRAPH, FATF/AML, FRAUD PATTERNS, BIOMETRICS
 */

import type { OperatorSpec } from './operatorSpecs';

// ═══════════════════════════════════════════════════════════════════════════════════════
// PARTE 8: OPERADORES NEO4J/GRAPH (18 operadores)
// ═══════════════════════════════════════════════════════════════════════════════════════

export const NEO4J_SPECS: Record<string, OperatorSpec> = {
  NEO4J_FRAUD_RING_DETECTION: {
    name: "NEO4J_FRAUD_RING_DETECTION",
    summary: "Detecta se conta pertence a um ANEL DE FRAUDE (rede de contas conectadas)",
    syntax: "NEO4J_FRAUD_RING_DETECTION(accountId) IS_TRUE",
    syntaxExplanation: "Usa Louvain Community Detection para encontrar comunidades densamente conectadas. Se > 50% da comunidade teve chargeback, é fraud ring.",
    story: "20 contas usando os mesmos 3 endereços e 2 beneficiários. Manualmente: dias. Com Neo4j: 2 segundos.",
    problem: "Como detectar redes de contas controladas pela mesma pessoa/grupo?",
    goldenTip: "💎 Neo4j precisa ser ALIMENTADO! Crie relações: SAME_DEVICE, SAME_ADDRESS, SAME_IP, SAME_BENEFICIARY.",
    engineBehavior: {
      description: "GraphOperatorEvaluator.evaluateFraudRing():",
      steps: [
        "1. Chama Neo4jGraphService.detectFraudRing(accountId)",
        "2. Neo4j executa Louvain: CALL gds.louvain.stream()",
        "3. Identifica comunidade do accountId",
        "4. Calcula fraudRate = chargebacks / transactions",
        "5. Se fraudRate > 0.5 → fraud ring"
      ],
      performance: "Louvain em 1M nós: ~30s (async, cache 1h). Query: <10ms."
    },
    realScenarios: [
      {
        title: "Money Mule Network",
        context: "Banco detectou R$ 2M em transferências suspeitas em 1 semana",
        problem: "15 contas fake transferindo para 3 contas 'mula' que sacavam em dinheiro",
        solution: "Neo4j revelou: mesmo IP de cadastro + mesmos destinos = comunidade Louvain",
        impact: "Bloqueou 18 contas, recuperou R$ 1.2M, denunciou à PF"
      }
    ]
  },

  NEO4J_COMMUNITY_DETECTION: {
    name: "NEO4J_COMMUNITY_DETECTION",
    summary: "Identifica a COMUNIDADE a que uma conta pertence no grafo",
    syntax: "NEO4J_COMMUNITY_DETECTION(accountId)",
    syntaxExplanation: "Retorna ID da comunidade Louvain. Contas na mesma comunidade estão fortemente conectadas.",
    story: "Agrupar contas suspeitas para investigar juntas.",
    problem: "Como agrupar contas por relacionamento para investigação em bloco?",
    goldenTip: "💎 Investigue comunidades inteiras, não contas individuais. Uma conta ruim = comunidade inteira suspeita."
  },

  NEO4J_PAGERANK: {
    name: "NEO4J_PAGERANK",
    summary: "Calcula a IMPORTÂNCIA/CENTRALIDADE de uma conta no grafo",
    syntax: "NEO4J_PAGERANK(accountId) GT 0.5",
    syntaxExplanation: "PageRank alto = conta muito conectada/importante na rede. Possível 'hub' de fraude.",
    story: "Conta com PageRank 0.9 (altíssimo) era o 'controlador' de 50 contas mula.",
    problem: "Como identificar a conta CENTRAL de um fraud ring?",
    goldenTip: "💎 PageRank identifica 'mastermind'. Bloqueie o hub = paralisa a rede toda.",
    engineBehavior: {
      description: "Neo4j GDS PageRank:",
      steps: [
        "1. CALL gds.pageRank.stream()",
        "2. Calcula score iterativamente",
        "3. Contas que recebem muitas conexões têm score alto",
        "4. Retorna normalizado 0-1"
      ]
    }
  },

  NEO4J_BETWEENNESS_CENTRALITY: {
    name: "NEO4J_BETWEENNESS_CENTRALITY",
    summary: "Mede quantos caminhos PASSAM por uma conta (intermediária)",
    syntax: "NEO4J_BETWEENNESS_CENTRALITY(accountId) GT 0.3",
    syntaxExplanation: "Conta com alto betweenness = ponte entre grupos = se cair, desconecta a rede.",
    story: "Conta 'ponte' entre dois fraud rings. Remover = isola ambos os grupos.",
    problem: "Como identificar contas que conectam diferentes grupos de fraude?",
    goldenTip: "💎 Betweenness alto = broker/intermediário. Pode ser mula conectando grupos."
  },

  NEO4J_CLOSENESS_CENTRALITY: {
    name: "NEO4J_CLOSENESS_CENTRALITY",
    summary: "Mede quão PRÓXIMA uma conta está de todas as outras",
    syntax: "NEO4J_CLOSENESS_CENTRALITY(accountId) GT 0.7",
    syntaxExplanation: "Closeness alto = conta alcança todas as outras rapidamente = bem conectada.",
    story: "Conta com closeness 0.9 conseguia transferir para qualquer outra em no máximo 2 hops.",
    problem: "Como identificar contas com acesso rápido a toda a rede?",
    goldenTip: "💎 Closeness + PageRank altos = controlador central da rede."
  },

  NEO4J_SHORTEST_PATH_LENGTH: {
    name: "NEO4J_SHORTEST_PATH_LENGTH",
    summary: "Calcula o caminho mais curto entre duas contas",
    syntax: "NEO4J_SHORTEST_PATH_LENGTH(accountA, accountB) LT 3",
    syntaxExplanation: "Se caminho < 3 hops, contas estão próximas no grafo (possivelmente relacionadas).",
    story: "Conta A e B parecem independentes mas estão a 2 hops de distância = relacionadas.",
    problem: "Como descobrir se duas contas aparentemente independentes estão conectadas?",
    goldenTip: "💎 Menos de 3 hops geralmente indica relacionamento real. > 6 hops = provavelmente não relacionadas."
  },

  FAN_IN_COUNT: {
    name: "FAN_IN_COUNT",
    summary: "Conta quantas contas ENVIAM para esta conta (recebimento)",
    syntax: "FAN_IN_COUNT(accountId) GT 50",
    syntaxExplanation: "Conta que recebe de 100+ origens diferentes = possível mula coletora.",
    story: "Conta recebia PIX de 200 contas diferentes em 1 semana = coletor de mulas.",
    problem: "Como detectar contas que concentram recebimentos?",
    goldenTip: "💎 Fan-in alto + saque rápido = mula coletora. Fraudadores 'drenam' a mula antes de bloquear.",
    engineBehavior: {
      description: "Query Neo4j:",
      steps: [
        "1. MATCH (sender)-[:TRANSFERRED_TO]->(account)",
        "2. COUNT(DISTINCT sender)",
        "3. Retorna quantidade de origens únicas"
      ]
    }
  },

  FAN_OUT_COUNT: {
    name: "FAN_OUT_COUNT",
    summary: "Conta quantas contas RECEBEM desta conta (envio)",
    syntax: "FAN_OUT_COUNT(accountId) GT 30",
    syntaxExplanation: "Conta que envia para 50+ destinos diferentes = possível splitter/distribuidor.",
    story: "Conta recebia valor alto e distribuía em pequenas quantias para 50 contas.",
    problem: "Como detectar contas que distribuem valores (splitting)?",
    goldenTip: "💎 Fan-out alto + valores iguais = splitting (divisão para fugir de alertas)."
  },

  NEO4J_DEGREE: {
    name: "NEO4J_DEGREE",
    summary: "Conta o total de conexões (entrada + saída) de uma conta",
    syntax: "NEO4J_DEGREE(accountId) GT 100",
    syntaxExplanation: "Grau = FAN_IN + FAN_OUT. Conta com degree 200 = muito ativa na rede.",
    story: "Conta com degree 500 era hub central de operação de lavagem.",
    problem: "Como identificar contas super-conectadas?",
    goldenTip: "💎 Degree alto não é sempre fraude. Empresa legítima também tem muito degree. Contextualize."
  },

  NEO4J_TRIANGLE_COUNT: {
    name: "NEO4J_TRIANGLE_COUNT",
    summary: "Conta quantos TRIÂNGULOS a conta participa (A→B→C→A)",
    syntax: "NEO4J_TRIANGLE_COUNT(accountId) GT 10",
    syntaxExplanation: "Triângulo = 3 contas que transacionam entre si. Muitos triângulos = rede fechada.",
    story: "Grupo de 10 contas com 45 triângulos = todos transacionam entre si = rede fechada suspeita.",
    problem: "Como detectar redes circulares (contas que só transacionam entre si)?",
    goldenTip: "💎 Clustering alto = rede fechada. Legítimo: empresa com filiais. Suspeito: contas PF fazendo circular."
  },

  NEO4J_CLUSTERING_COEFFICIENT: {
    name: "NEO4J_CLUSTERING_COEFFICIENT",
    summary: "Mede quão interconectados são os vizinhos de uma conta (0-1)",
    syntax: "NEO4J_CLUSTERING_COEFFICIENT(accountId) GT 0.8",
    syntaxExplanation: "Coeficiente 1.0 = todos os vizinhos se conhecem. 0.0 = vizinhos não se conectam.",
    story: "Coeficiente 0.95 = clique quase perfeito = grupo muito unido = fraud ring.",
    problem: "Como medir o quão 'fechado' é o grupo de uma conta?",
    goldenTip: "💎 Clustering > 0.8 em grupo de PF = altamente suspeito. Pessoas normais não têm amigos tão interconectados."
  },

  NEO4J_JACCARD_SIMILARITY: {
    name: "NEO4J_JACCARD_SIMILARITY",
    summary: "Mede similaridade entre duas contas baseado em vizinhos comuns",
    syntax: "NEO4J_JACCARD_SIMILARITY(accountA, accountB) GT 0.7",
    syntaxExplanation: "Jaccard = vizinhos_comuns / vizinhos_totais. 0.7 = 70% de vizinhos em comum.",
    story: "Duas contas com Jaccard 0.9 = quase os mesmos vizinhos = provavelmente mesmo dono.",
    problem: "Como detectar contas que pertencem à mesma pessoa/grupo?",
    goldenTip: "💎 Jaccard > 0.7 entre PFs = muito provável que sejam do mesmo 'dono'."
  },

  NEO4J_COSINE_SIMILARITY: {
    name: "NEO4J_COSINE_SIMILARITY",
    summary: "Mede similaridade baseado em padrões de transação",
    syntax: "NEO4J_COSINE_SIMILARITY(accountA, accountB) GT 0.8",
    syntaxExplanation: "Compara 'vetores de comportamento'. 0.8 = padrões 80% similares.",
    story: "Duas contas com padrões idênticos de horário, valor e destino = mesmo operador.",
    problem: "Como detectar contas com comportamento idêntico (mesmo operador)?",
    goldenTip: "💎 Cosine considera volume. Jaccard considera apenas existência de conexão."
  },

  NEO4J_CONNECTED_COMPONENT: {
    name: "NEO4J_CONNECTED_COMPONENT",
    summary: "Identifica o componente conectado (grupo isolado) da conta",
    syntax: "NEO4J_CONNECTED_COMPONENT(accountId)",
    syntaxExplanation: "Retorna ID do componente. Contas no mesmo componente estão conectadas de alguma forma.",
    story: "Componente de 500 contas isolado do resto = possível operação organizada.",
    problem: "Como identificar grupos de contas que só transacionam entre si?",
    goldenTip: "💎 Componente isolado do grafo principal = investigar todo o grupo junto."
  },

  NEO4J_LABEL_PROPAGATION: {
    name: "NEO4J_LABEL_PROPAGATION",
    summary: "Detecta comunidades via propagação de rótulos",
    syntax: "NEO4J_LABEL_PROPAGATION(accountId)",
    syntaxExplanation: "Algoritmo rápido para clustering. Retorna label da comunidade.",
    story: "Label Propagation encontrou 15 comunidades em grafo de 1M de contas.",
    problem: "Como clusterizar rapidamente um grafo muito grande?",
    goldenTip: "💎 Label Propagation é mais rápido que Louvain para grafos gigantes (>10M nós)."
  },

  NEO4J_WEAKLY_CONNECTED: {
    name: "NEO4J_WEAKLY_CONNECTED",
    summary: "Verifica se duas contas estão no mesmo componente fracamente conectado",
    syntax: "NEO4J_WEAKLY_CONNECTED(accountA, accountB) IS_TRUE",
    syntaxExplanation: "TRUE se existe caminho (ignorando direção das setas) entre A e B.",
    story: "A não enviou para B diretamente, mas há caminho via C.",
    problem: "Como verificar se duas contas estão conectadas de alguma forma?",
    goldenTip: "💎 Weakly connected ignora direção. A→B e B→A são tratados igual."
  },

  NEO4J_STRONGLY_CONNECTED: {
    name: "NEO4J_STRONGLY_CONNECTED",
    summary: "Verifica se há caminho BIDIRECIONAL entre duas contas",
    syntax: "NEO4J_STRONGLY_CONNECTED(accountA, accountB) IS_TRUE",
    syntaxExplanation: "TRUE se existe caminho A→B E caminho B→A.",
    story: "A enviou para B e B enviou para A (direta ou indiretamente) = circulação.",
    problem: "Como detectar circulação de valores (A→B→A)?",
    goldenTip: "💎 Strongly connected = circulação confirmada. Sinal clássico de lavagem."
  },

  NEO4J_NODE_SIMILARITY: {
    name: "NEO4J_NODE_SIMILARITY",
    summary: "Encontra os nós mais SIMILARES a uma conta específica",
    syntax: "NEO4J_NODE_SIMILARITY(accountId, topK=5)",
    syntaxExplanation: "Retorna as 5 contas mais parecidas em padrão de conexões.",
    story: "Encontrou 5 contas quase idênticas em padrão = provavelmente mesmo fraudador.",
    problem: "Como encontrar 'clones' de uma conta suspeita?",
    goldenTip: "💎 Use para expandir investigação: se conta X é fraude, quais são similares? Provavelmente também."
  }
};

// ═══════════════════════════════════════════════════════════════════════════════════════
// PARTE 9: OPERADORES FATF/AML (28 operadores - Tipologias de Lavagem)
// ═══════════════════════════════════════════════════════════════════════════════════════

export const FATF_SPECS: Record<string, OperatorSpec> = {
  FATF_HIGH_RISK_JURISDICTION: {
    name: "FATF_HIGH_RISK_JURISDICTION",
    summary: "Verifica se país está na lista de ALTO RISCO do FATF",
    syntax: "FATF_HIGH_RISK_JURISDICTION(country) IS_TRUE",
    syntaxExplanation: "Países na 'lista negra' FATF: Coreia do Norte, Irã, Myanmar, etc.",
    story: "Transferência para Irã = due diligence reforçada obrigatória por lei.",
    problem: "Como garantir compliance com lista FATF?",
    goldenTip: "💎 Lista FATF é atualizada periodicamente. Implementar atualização automática!",
    engineBehavior: {
      description: "Consulta lista FATF:",
      steps: [
        "1. Lê país da transação",
        "2. Consulta lista FATF em memória (atualizada mensalmente)",
        "3. Retorna TRUE se país está na lista"
      ]
    }
  },

  FATF_GREY_LIST_JURISDICTION: {
    name: "FATF_GREY_LIST_JURISDICTION",
    summary: "Verifica se país está na LISTA CINZA do FATF (monitoramento)",
    syntax: "FATF_GREY_LIST_JURISDICTION(country) IS_TRUE",
    syntaxExplanation: "Países sob 'monitoramento intensificado': Emirados, Cayman, etc.",
    story: "TX para país em lista cinza = enhanced due diligence (não bloqueio automático).",
    problem: "Como implementar monitoramento diferenciado para lista cinza?",
    goldenTip: "💎 Lista cinza ≠ bloqueio. É alerta para análise mais rigorosa."
  },

  FATF_STRUCTURING: {
    name: "FATF_STRUCTURING",
    summary: "Detecta STRUCTURING (quebra de valores para fugir de reporte - Smurfing)",
    syntax: "FATF_STRUCTURING() IS_TRUE",
    syntaxExplanation: "Múltiplas TXs logo abaixo do limite de reporte (ex: R$ 9.900 repetido quando limite é R$ 10k).",
    story: "Cliente fez 10 depósitos de R$ 9.800 em vez de 1 de R$ 98.000 para fugir do CTR.",
    problem: "Como detectar quebra intencional de valores para fugir de obrigações de reporte?",
    goldenTip: "💎 Padrão clássico:\n• Limite = R$ 10.000\n• Fraudador faz 5x R$ 9.900 = R$ 49.500\n• Cada TX não reporta, mas soma é reportável",
    engineBehavior: {
      description: "Análise de padrão de valores:",
      steps: [
        "1. Identifica limite de reporte (ex: R$ 10k, USD 10k)",
        "2. Busca TXs do cliente próximas ao limite (90-99%)",
        "3. Conta quantas TXs estão nessa faixa",
        "4. Se > threshold em período curto = structuring"
      ]
    },
    realScenarios: [
      {
        title: "Smurfing Clássico",
        context: "Limite CTR = R$ 10.000",
        problem: "Cliente fez 15 depósitos de R$ 9.500 em 3 dias = R$ 142.500 sem CTR",
        solution: "FATF_STRUCTURING detectou padrão: 15 TXs a 95% do limite em 72h",
        impact: "Conta bloqueada, SAR submetido, investigação iniciada"
      }
    ]
  },

  FATF_LAYERING: {
    name: "FATF_LAYERING",
    summary: "Detecta LAYERING (múltiplas camadas de transações para ocultar origem)",
    syntax: "FATF_LAYERING() IS_TRUE",
    syntaxExplanation: "Dinheiro passa por várias contas intermediárias rapidamente para obscurecer trilha.",
    story: "R$ 1M passou por 12 contas em 4 horas antes de sair do país.",
    problem: "Como detectar 'lavagem em cascata' (multiple hops)?",
    goldenTip: "💎 Layering típico:\nOrigem → Shell 1 → Shell 2 → Shell 3 → Offshore\nCada hop dificulta rastreamento.",
    engineBehavior: {
      description: "Análise de fluxo no grafo:",
      steps: [
        "1. Rastreia dinheiro da origem atual",
        "2. Conta quantos 'hops' desde entrada",
        "3. Se > 5 hops em < 24h = layering provável"
      ]
    }
  },

  FATF_INTEGRATION: {
    name: "FATF_INTEGRATION",
    summary: "Detecta INTEGRATION (reintrodução de dinheiro lavado na economia)",
    syntax: "FATF_INTEGRATION() IS_TRUE",
    syntaxExplanation: "Fase final: dinheiro 'limpo' sendo usado para compras legítimas.",
    story: "Conta que só recebia transferências suspeitas agora compra imóveis.",
    problem: "Como detectar dinheiro lavado sendo reintegrado como 'limpo'?",
    goldenTip: "💎 Sinais de integration:\n• Compra de ativos de luxo\n• Investimentos em empresas legítimas\n• Empréstimos back-to-back"
  },

  FATF_ROUND_TRIPPING: {
    name: "FATF_ROUND_TRIPPING",
    summary: "Detecta ROUND TRIPPING (dinheiro que sai e retorna ao mesmo local)",
    syntax: "FATF_ROUND_TRIPPING() IS_TRUE",
    syntaxExplanation: "BR → Offshore → BR. Dinheiro 'dá a volta' para parecer investimento estrangeiro.",
    story: "Empresa brasileira 'recebe investimento' das Cayman que na verdade é dinheiro dela mesma.",
    problem: "Como detectar dinheiro que 'dá a volta' para parecer origem diferente?",
    goldenTip: "💎 Padrão: País A → Paraíso fiscal → País A. Mesma quantia, mesmos beneficiários finais."
  },

  FATF_SHELL_COMPANY_USAGE: {
    name: "FATF_SHELL_COMPANY_USAGE",
    summary: "Detecta uso de SHELL COMPANIES (empresas de fachada)",
    syntax: "FATF_SHELL_COMPANY_USAGE() IS_TRUE",
    syntaxExplanation: "TX para empresa sem atividade operacional real, só movimentação financeira.",
    story: "Empresa em Delaware com 0 funcionários movimentando R$ 50M/mês.",
    problem: "Como identificar uso de empresas de fachada?",
    goldenTip: "💎 Red flags de shell:\n• Sem funcionários\n• Endereço = escritório virtual\n• Atividade só financeira\n• Múltiplos donos em cascata"
  },

  FATF_TRADE_BASED_ML: {
    name: "FATF_TRADE_BASED_ML",
    summary: "Detecta TRADE-BASED MONEY LAUNDERING (lavagem via comércio)",
    syntax: "FATF_TRADE_BASED_ML() IS_TRUE",
    syntaxExplanation: "Uso de notas fiscais falsas ou superfaturadas para mover dinheiro.",
    story: "Importação de 'consultoria' por R$ 5M quando serviço vale R$ 50k.",
    problem: "Como detectar superfaturamento para lavagem via trade?",
    goldenTip: "💎 Patterns:\n• Invoice muito acima do mercado\n• Commodities com preço fora\n• Serviços intangíveis de alto valor"
  },

  FATF_REAL_ESTATE_ML: {
    name: "FATF_REAL_ESTATE_ML",
    summary: "Detecta lavagem via IMÓVEIS",
    syntax: "FATF_REAL_ESTATE_ML() IS_TRUE",
    syntaxExplanation: "Compra de imóveis com dinheiro suspeito ou valores fora do mercado.",
    story: "Imóvel comprado por R$ 5M cash, vendido R$ 2M meses depois (não faz sentido comercial).",
    problem: "Como detectar lavagem via mercado imobiliário?",
    goldenTip: "💎 Red flags:\n• Pagamento em espécie\n• Compra abaixo do mercado\n• Venda rápida com prejuízo\n• Múltiplas propriedades para PF"
  },

  FATF_CASINO_ML: {
    name: "FATF_CASINO_ML",
    summary: "Detecta lavagem via CASSINOS/JOGOS",
    syntax: "FATF_CASINO_ML() IS_TRUE",
    syntaxExplanation: "Uso de cassino para 'lavar' dinheiro em fichas e sacar como ganhos.",
    story: "Cliente compra R$ 1M em fichas cash, joga pouco, saca como 'prêmio'.",
    problem: "Como detectar lavagem via cassinos/jogos de azar?",
    goldenTip: "💎 Padrão: muita compra de fichas + pouco jogo + saque rápido = lavagem"
  },

  FATF_CRYPTO_ML: {
    name: "FATF_CRYPTO_ML",
    summary: "Detecta lavagem via CRIPTOMOEDAS",
    syntax: "FATF_CRYPTO_ML() IS_TRUE",
    syntaxExplanation: "Conversão para crypto e de volta para fiat para obscurecer origem.",
    story: "R$ 5M vira BTC, passa por mixer, vira USDT, saca em exchange diferente.",
    problem: "Como detectar lavagem via criptomoedas?",
    goldenTip: "💎 Red flags:\n• Uso de mixers/tumblers\n• Múltiplas exchanges\n• Privacy coins (Monero, ZCash)\n• Peer-to-peer"
  },

  FATF_GATEKEEPERS: {
    name: "FATF_GATEKEEPERS",
    summary: "Detecta envolvimento de GATEKEEPERS (advogados, contadores facilitadores)",
    syntax: "FATF_GATEKEEPERS() IS_TRUE",
    syntaxExplanation: "Profissionais usando conhecimento técnico para facilitar lavagem.",
    story: "Mesmo escritório de advocacia criou 20 shell companies para clientes suspeitos.",
    problem: "Como identificar profissionais que facilitam lavagem?",
    goldenTip: "💎 Gatekeepers típicos: advogados, contadores, formadores de empresa, trustee"
  },

  FATF_PEP_INVOLVEMENT: {
    name: "FATF_PEP_INVOLVEMENT",
    summary: "Detecta envolvimento de PEP (Pessoa Exposta Politicamente)",
    syntax: "FATF_PEP_INVOLVEMENT() IS_TRUE",
    syntaxExplanation: "TX envolve PEP ou família/associados próximos.",
    story: "Filho de político movimentando valores incompatíveis com renda declarada.",
    problem: "Como aplicar enhanced due diligence para PEPs?",
    goldenTip: "💎 PEP = político, juiz, militar alto patente, executivo estatal. Familia e associados também!"
  },

  FATF_CORRESPONDENT_BANKING: {
    name: "FATF_CORRESPONDENT_BANKING",
    summary: "Detecta uso suspeito de CORRESPONDENT BANKING",
    syntax: "FATF_CORRESPONDENT_BANKING() IS_TRUE",
    syntaxExplanation: "Uso de banco correspondente para acessar sistema financeiro indiretamente.",
    story: "Banco de país sancionado usando correspondente para chegar a USD.",
    problem: "Como detectar abuso de correspondent banking?",
    goldenTip: "💎 Nested accounts e payable-through accounts são red flags"
  },

  FATF_WIRE_STRIPPING: {
    name: "FATF_WIRE_STRIPPING",
    summary: "Detecta WIRE STRIPPING (remoção de informações de transferências)",
    syntax: "FATF_WIRE_STRIPPING() IS_TRUE",
    syntaxExplanation: "TX sem informações completas de originador/beneficiário (violação travel rule).",
    story: "SWIFT sem nome do beneficiário final = wire stripping.",
    problem: "Como detectar omissão intencional de dados em transferências?",
    goldenTip: "💎 Travel Rule: TX > $1000 DEVE ter dados completos. Sem dados = suspeito."
  },

  FATF_SANCTIONS_EVASION: {
    name: "FATF_SANCTIONS_EVASION",
    summary: "Detecta tentativa de EVASÃO DE SANÇÕES",
    syntax: "FATF_SANCTIONS_EVASION() IS_TRUE",
    syntaxExplanation: "Técnicas para evitar detecção de sanções: nomes alterados, intermediários, etc.",
    story: "Empresa iraniana usando intermediário nos Emirados para receber USD.",
    problem: "Como detectar técnicas de evasão de sanções?",
    goldenTip: "💎 Técnicas:\n• Nomes levemente alterados\n• Intermediários em países não sancionados\n• Front companies"
  },

  FATF_INFORMAL_VALUE_TRANSFER: {
    name: "FATF_INFORMAL_VALUE_TRANSFER",
    summary: "Detecta uso de SISTEMAS INFORMAIS (Hawala, Fei-ch'ien)",
    syntax: "FATF_INFORMAL_VALUE_TRANSFER() IS_TRUE",
    syntaxExplanation: "Transferência de valor sem movimento real de dinheiro entre países.",
    story: "Cliente 'manda' R$ 100k para Índia mas dinheiro nunca sai do Brasil (Hawala).",
    problem: "Como detectar uso de sistemas alternativos de remessa?",
    goldenTip: "💎 Hawala: acerta débito/crédito sem transferência real. Muito usado em corredores específicos."
  },

  FATF_NOMINEE_ARRANGEMENT: {
    name: "FATF_NOMINEE_ARRANGEMENT",
    summary: "Detecta uso de NOMINEES (laranjas/testas-de-ferro)",
    syntax: "FATF_NOMINEE_ARRANGEMENT() IS_TRUE",
    syntaxExplanation: "Pessoa que empresta nome para ocultar beneficiário real.",
    story: "Idosa de 80 anos é 'dona' de 15 empresas - claramente nominee.",
    problem: "Como identificar uso de laranjas/testas-de-ferro?",
    goldenTip: "💎 Red flags:\n• Muitas empresas para 1 PF\n• Perfil incompatível (idade, renda)\n• Mesmo endereço em várias empresas"
  },

  FATF_RAPID_MOVEMENT: {
    name: "FATF_RAPID_MOVEMENT",
    summary: "Detecta MOVIMENTAÇÃO RÁPIDA de valores (in-out no mesmo dia)",
    syntax: "FATF_RAPID_MOVEMENT() IS_TRUE",
    syntaxExplanation: "Dinheiro entra e sai da conta em horas/minutos, sem ficar parado.",
    story: "R$ 500k entrou às 10h e saiu às 10:15 para 10 destinos diferentes.",
    problem: "Como detectar conta sendo usada apenas como 'passagem'?",
    goldenTip: "💎 Pass-through account: dinheiro não para. Saldo quase sempre zerado logo após crédito."
  },

  FATF_FUNNEL_ACCOUNT: {
    name: "FATF_FUNNEL_ACCOUNT",
    summary: "Detecta FUNNEL ACCOUNT (conta funil que consolida valores)",
    syntax: "FATF_FUNNEL_ACCOUNT() IS_TRUE",
    syntaxExplanation: "Conta que recebe de muitas origens e consolida para poucos destinos.",
    story: "Conta recebe de 200 fontes e transfere tudo para 3 contas offshore.",
    problem: "Como identificar contas que concentram valores de múltiplas origens?",
    goldenTip: "💎 Padrão funil: muitos IN → 1 conta → poucos OUT (geralmente offshore)"
  },

  FATF_CURRENCY_EXCHANGE_ABUSE: {
    name: "FATF_CURRENCY_EXCHANGE_ABUSE",
    summary: "Detecta abuso de CÂMBIO para lavagem",
    syntax: "FATF_CURRENCY_EXCHANGE_ABUSE() IS_TRUE",
    syntaxExplanation: "Múltiplas conversões desnecessárias para obscurecer valor.",
    story: "BRL → USD → EUR → GBP → BRL em 24h sem razão comercial.",
    problem: "Como detectar conversões de câmbio suspeitas?",
    goldenTip: "💎 Conversão múltipla sem razão comercial = red flag. Especialmente se voltar à moeda original."
  },

  FATF_INSURANCE_ML: {
    name: "FATF_INSURANCE_ML",
    summary: "Detecta lavagem via SEGUROS",
    syntax: "FATF_INSURANCE_ML() IS_TRUE",
    syntaxExplanation: "Uso de produtos de seguro para lavar dinheiro.",
    story: "Compra seguro de vida por R$ 5M, cancela em 6 meses, pede resgate.",
    problem: "Como detectar abuso de produtos de seguro para lavagem?",
    goldenTip: "💎 Patterns:\n• Pagamento em espécie\n• Cancelamento precoce\n• Troca frequente de beneficiário"
  },

  FATF_LOAN_BACK: {
    name: "FATF_LOAN_BACK",
    summary: "Detecta LOAN-BACK scheme (empréstimo de próprio dinheiro)",
    syntax: "FATF_LOAN_BACK() IS_TRUE",
    syntaxExplanation: "Deposita dinheiro sujo em banco offshore, 'toma empréstimo' de volta.",
    story: "Envia R$ 10M para offshore, depois 'recebe empréstimo' de R$ 10M.",
    problem: "Como detectar esquema de loan-back?",
    goldenTip: "💎 Empréstimo de instituição offshore sem due diligence normal = red flag"
  },

  FATF_MINGLING: {
    name: "FATF_MINGLING",
    summary: "Detecta MINGLING (mistura de dinheiro sujo com legítimo)",
    syntax: "FATF_MINGLING() IS_TRUE",
    syntaxExplanation: "Negócio legítimo mistura receita real com dinheiro sujo.",
    story: "Restaurante fatura R$ 100k/mês mas deposita R$ 500k = mingling.",
    problem: "Como detectar mistura de fundos ilícitos com negócio legítimo?",
    goldenTip: "💎 Cash-intensive businesses são usados: restaurantes, lavanderias, postos, estacionamentos"
  },

  FATF_OVER_INVOICING: {
    name: "FATF_OVER_INVOICING",
    summary: "Detecta OVER-INVOICING (superfaturamento)",
    syntax: "FATF_OVER_INVOICING() IS_TRUE",
    syntaxExplanation: "Nota fiscal com valor muito acima do mercado.",
    story: "Importação de 'consultoria' por USD 5M quando serviço vale USD 50k.",
    problem: "Como detectar superfaturamento em trade?",
    goldenTip: "💎 Comparar com preços de mercado. Desvio > 200% = investigar."
  },

  FATF_UNDER_INVOICING: {
    name: "FATF_UNDER_INVOICING",
    summary: "Detecta UNDER-INVOICING (subfaturamento)",
    syntax: "FATF_UNDER_INVOICING() IS_TRUE",
    syntaxExplanation: "Nota fiscal com valor muito abaixo do mercado.",
    story: "Exportação de ouro por USD 10/grama quando mercado é USD 60/grama.",
    problem: "Como detectar subfaturamento em trade?",
    goldenTip: "💎 Under-invoicing permite 'ganho' na diferença de preço. Ouro, diamante, arte são comuns."
  },

  FATF_PHANTOM_SHIPMENT: {
    name: "FATF_PHANTOM_SHIPMENT",
    summary: "Detecta PHANTOM SHIPMENT (embarque fantasma)",
    syntax: "FATF_PHANTOM_SHIPMENT() IS_TRUE",
    syntaxExplanation: "Pagamento por mercadoria que nunca existiu/foi enviada.",
    story: "Invoice de USD 2M por container que nunca chegou no destino.",
    problem: "Como detectar pagamentos por embarques que não existem?",
    goldenTip: "💎 Cross-check com dados de alfândega/porto. Sem registro de saída = phantom."
  },

  FATF_BLACK_MARKET_EXCHANGE: {
    name: "FATF_BLACK_MARKET_EXCHANGE",
    summary: "Detecta uso de MERCADO NEGRO de câmbio",
    syntax: "FATF_BLACK_MARKET_EXCHANGE() IS_TRUE",
    syntaxExplanation: "Conversão de moeda por taxas muito diferentes do mercado oficial.",
    story: "Câmbio a R$ 3,50 quando dólar oficial estava R$ 5,00.",
    problem: "Como detectar uso de dólar paralelo/mercado negro?",
    goldenTip: "💎 Taxa muito diferente da oficial (> 10% spread) = suspeito."
  }
};

// ═══════════════════════════════════════════════════════════════════════════════════════
// PARTE 10: OPERADORES DE FRAUD PATTERNS (30 operadores)
// ═══════════════════════════════════════════════════════════════════════════════════════

export const FRAUD_PATTERN_SPECS: Record<string, OperatorSpec> = {
  STRUCTURING_DETECTION: {
    name: "STRUCTURING_DETECTION",
    summary: "Detecta SMURFING/STRUCTURING (quebra de valores para evitar reporte)",
    syntax: "STRUCTURING_DETECTION() IS_TRUE",
    syntaxExplanation: "Múltiplas TXs logo abaixo do limite de reporte.",
    story: "10 depósitos de R$ 9.900 em vez de 1 de R$ 99.000.",
    problem: "Como detectar quebra intencional de valores?",
    goldenTip: "💎 Exemplo:\n• Limite CTR = R$ 10.000\n• Fraudador: 5x R$ 9.800 = R$ 49.000\n• Nenhuma TX reporta, mas total SIM"
  },

  LAYERING_PATTERN: {
    name: "LAYERING_PATTERN",
    summary: "Detecta padrão de LAYERING (múltiplas camadas de transferências)",
    syntax: "LAYERING_PATTERN() IS_TRUE",
    syntaxExplanation: "Dinheiro passa por várias contas rapidamente.",
    story: "R$ 1M passou por 10 contas em 3 horas antes de sair do país.",
    problem: "Como detectar cascata de transferências?",
    goldenTip: "💎 > 5 hops em < 24h = layering provável."
  },

  BUST_OUT_PATTERN_DETECTION: {
    name: "BUST_OUT_PATTERN_DETECTION",
    summary: "Detecta BUST-OUT (conta 'estourar' depois de construir crédito)",
    syntax: "BUST_OUT_PATTERN_DETECTION() IS_TRUE",
    syntaxExplanation: "Conta constrói histórico bom, depois usa todo o limite e some.",
    story: "Conta de 6 meses com histórico perfeito de repente usa R$ 100k e para de pagar.",
    problem: "Como detectar bust-out (build-up + max-out + abandono)?",
    goldenTip: "💎 Padrão clássico:\n• Meses 1-5: uso baixo, pagamento em dia\n• Mês 6: estoura limite\n• Mês 7: some sem pagar",
    engineBehavior: {
      description: "Análise de padrão temporal:",
      steps: [
        "1. Analisa histórico de uso vs limite",
        "2. Detecta 'ramp-up' súbito",
        "3. Verifica se utilização passou de 50% → 95%+ rapidamente",
        "4. Se combinar com outros sinais = bust-out"
      ]
    }
  },

  PIG_BUTCHERING_INDICATOR: {
    name: "PIG_BUTCHERING_INDICATOR",
    summary: "Detecta golpe 'PIG BUTCHERING' (engorda antes do abate)",
    syntax: "PIG_BUTCHERING_INDICATOR() IS_TRUE",
    syntaxExplanation: "Vítima 'engordada' com pequenos ganhos antes de perder tudo.",
    story: "Cliente 'investe' R$ 1k, 'ganha' R$ 200, confia e investe R$ 100k, perde tudo.",
    problem: "Como detectar golpes de investimento do tipo pig butchering?",
    goldenTip: "💎 Padrão:\n1. Primeiro 'investimento' pequeno\n2. 'Retorno' rápido\n3. Confiança aumenta\n4. Investimento grande\n5. Plataforma some"
  },

  ROMANCE_SCAM_INDICATOR: {
    name: "ROMANCE_SCAM_INDICATOR",
    summary: "Detecta ROMANCE SCAM (golpe do amor)",
    syntax: "ROMANCE_SCAM_INDICATOR() IS_TRUE",
    syntaxExplanation: "Padrão de transferências para pessoa que vítima nunca conheceu pessoalmente.",
    story: "Viúva de 60 anos enviando R$ 200k para 'namorado' que conheceu online.",
    problem: "Como detectar golpes de romance/relacionamento?",
    goldenTip: "💎 Red flags:\n• Beneficiário no exterior\n• Pedidos de urgência\n• Valores crescentes\n• Vítima idosa ou viúva"
  },

  INVESTMENT_SCAM_PATTERN: {
    name: "INVESTMENT_SCAM_PATTERN",
    summary: "Detecta padrão de GOLPE DE INVESTIMENTO",
    syntax: "INVESTMENT_SCAM_PATTERN() IS_TRUE",
    syntaxExplanation: "Transferências para 'investimentos' com retornos irreais prometidos.",
    story: "Cliente 'investe' em cripto que promete 50%/mês.",
    problem: "Como detectar golpes de investimento (Ponzi, pirâmide)?",
    goldenTip: "💎 Retorno muito acima do mercado = golpe. CDI ~10%/ano. Quem promete 10%/mês = fraude."
  },

  APP_FRAUD_DETECTION: {
    name: "APP_FRAUD_DETECTION",
    summary: "Detecta APP FRAUD (Authorized Push Payment - vítima autoriza pagamento)",
    syntax: "APP_FRAUD_DETECTION() IS_TRUE",
    syntaxExplanation: "Vítima manipulada a fazer transferência voluntariamente.",
    story: "Golpista liga fingindo ser banco, vítima faz PIX 'para proteção'.",
    problem: "Como detectar quando cliente foi manipulado a fazer transferência?",
    goldenTip: "💎 APP fraud é difícil: cliente QUER fazer a TX. Detectar contexto suspeito."
  },

  CARD_TESTING_RING_DETECTION: {
    name: "CARD_TESTING_RING_DETECTION",
    summary: "Detecta CARD TESTING (teste de cartões roubados)",
    syntax: "CARD_TESTING_RING_DETECTION() IS_TRUE",
    syntaxExplanation: "Múltiplas TXs pequenas para validar se cartões roubados funcionam.",
    story: "Fraudador testa 500 cartões com compras de R$ 1 cada.",
    problem: "Como detectar validação de cartões clonados?",
    goldenTip: "💎 Padrão:\n• Muitas TXs pequenas (R$ 1-5)\n• Mesmo IP/device\n• Merchants diferentes\n• PANs diferentes",
    realScenarios: [
      {
        title: "Card Testing Attack",
        context: "E-commerce de eletrônicos",
        problem: "500 TXs de R$ 1 em 1 hora vindas do mesmo IP",
        solution: "VELOCITY_COUNT_GT + amount < 5 + same IP = BLOCK",
        impact: "Bloqueou 487 testes. Economia: R$ 1.5M em chargebacks evitados"
      }
    ]
  },

  SYNTHETIC_IDENTITY_FRAUD: {
    name: "SYNTHETIC_IDENTITY_FRAUD",
    summary: "Detecta IDENTIDADE SINTÉTICA (CPF/SSN fabricado)",
    syntax: "SYNTHETIC_IDENTITY_FRAUD() IS_TRUE",
    syntaxExplanation: "Identidade criada combinando dados reais de várias pessoas.",
    story: "CPF de criança + nome adulto + endereço diferente = synthetic.",
    problem: "Como detectar identidades fabricadas?",
    goldenTip: "💎 Sinais:\n• CPF emitido recentemente\n• Sem histórico de crédito\n• Múltiplas solicitações simultâneas\n• Dados inconsistentes"
  },

  FIRST_PARTY_FRAUD_PATTERN: {
    name: "FIRST_PARTY_FRAUD_PATTERN",
    summary: "Detecta FRAUDE DE PRIMEIRA PARTE (cliente fraudando a si mesmo)",
    syntax: "FIRST_PARTY_FRAUD_PATTERN() IS_TRUE",
    syntaxExplanation: "Cliente alega fraude mas foi ele mesmo que fez a TX.",
    story: "Cliente fez PIX, depois ligou dizendo que foi hackeado.",
    problem: "Como identificar alegação falsa de fraude?",
    goldenTip: "💎 Cross-check:\n• Device do chargeback = device da TX?\n• IP igual?\n• Padrão de comportamento igual?"
  },

  ACCOUNT_FARMING: {
    name: "ACCOUNT_FARMING",
    summary: "Detecta ACCOUNT FARMING (criação em massa de contas fake)",
    syntax: "ACCOUNT_FARMING() IS_TRUE",
    syntaxExplanation: "Múltiplas contas criadas com padrão similar (mesmo IP, device, etc).",
    story: "200 contas criadas em 1 hora, todas do mesmo IP de servidor.",
    problem: "Como detectar criação em massa de contas?",
    goldenTip: "💎 Farm patterns:\n• Mesmo device\n• Nomes sequenciais\n• Emails similares\n• Cadastro rápido demais"
  },

  PROMO_ABUSE_DETECTION: {
    name: "PROMO_ABUSE_DETECTION",
    summary: "Detecta ABUSO DE PROMOÇÃO (uso de múltiplas contas para promos)",
    syntax: "PROMO_ABUSE_DETECTION() IS_TRUE",
    syntaxExplanation: "Múltiplas contas usando mesma promoção/cupom de 'primeiro pedido'.",
    story: "100 'novos clientes' do mesmo device pegando cupom de R$ 30.",
    problem: "Como detectar abuso de promoções first-time?",
    goldenTip: "💎 Mesmo device/IP com múltiplas 'primeiras' compras = abuso."
  },

  REFUND_FRAUD_PATTERN: {
    name: "REFUND_FRAUD_PATTERN",
    summary: "Detecta FRAUDE DE REEMBOLSO (pedir reembolso indevido)",
    syntax: "REFUND_FRAUD_PATTERN() IS_TRUE",
    syntaxExplanation: "Padrão de compra + alegação de não recebimento + reembolso.",
    story: "Cliente compra, diz que não recebeu (mentira), pega reembolso E produto.",
    problem: "Como detectar alegações falsas de não recebimento?",
    goldenTip: "💎 Padrão:\n• Muitos pedidos de reembolso\n• Sempre 'não recebi'\n• Produtos de alto valor\n• Vendedor marketplace"
  },

  FRIENDLY_FRAUD_PATTERN: {
    name: "FRIENDLY_FRAUD_PATTERN",
    summary: "Detecta FRIENDLY FRAUD (chargeback indevido)",
    syntax: "FRIENDLY_FRAUD_PATTERN() IS_TRUE",
    syntaxExplanation: "Cliente faz compra legítima, depois disputa dizendo que não fez.",
    story: "Comprou iPhone, recebeu, depois abriu chargeback 'não reconheço'.",
    problem: "Como detectar chargebacks fraudulentos?",
    goldenTip: "💎 Sinais:\n• Device/IP da compra = do cliente\n• Entrega confirmada\n• Sem histórico de fraude real"
  },

  TRIANGULATION_FRAUD: {
    name: "TRIANGULATION_FRAUD",
    summary: "Detecta TRIANGULATION FRAUD (golpe de triangulação)",
    syntax: "TRIANGULATION_FRAUD() IS_TRUE",
    syntaxExplanation: "Fraudador vende produto, compra de loja real com cartão roubado, envia para comprador.",
    story: "Vende no ML por R$ 500, compra na Magalu por R$ 600 com cartão roubado, embolsa R$ 500.",
    problem: "Como detectar triangulação (resale fraud)?",
    goldenTip: "💎 Sinais:\n• Comprador ≠ destinatário\n• Vendedor em marketplace\n• Volume de compras/vendas desproporcional"
  },

  DROP_SHIPPING_FRAUD: {
    name: "DROP_SHIPPING_FRAUD",
    summary: "Detecta fraude via DROP SHIPPING",
    syntax: "DROP_SHIPPING_FRAUD() IS_TRUE",
    syntaxExplanation: "Uso de cartão roubado para 'dropship' para clientes de marketplace.",
    story: "Loja fake vende produto, envia direto da Amazon para cliente (paga com cartão roubado).",
    problem: "Como detectar drop shipping fraudulento?",
    goldenTip: "💎 Muitas compras com 'presente' ou destino diferente do cadastro = suspeito"
  },

  CREDENTIAL_STUFFING_PATTERN: {
    name: "CREDENTIAL_STUFFING_PATTERN",
    summary: "Detecta CREDENTIAL STUFFING (teste de senhas vazadas)",
    syntax: "CREDENTIAL_STUFFING_PATTERN() IS_TRUE",
    syntaxExplanation: "Muitas tentativas de login com senhas de outros vazamentos.",
    story: "10.000 tentativas de login em 1 hora, senhas diferentes, emails variados.",
    problem: "Como detectar ataque de credential stuffing?",
    goldenTip: "💎 Padrão:\n• Alto volume de logins falhados\n• Emails variados (lista vazada)\n• Mesmo IP/range"
  },

  BRUTE_FORCE_PATTERN: {
    name: "BRUTE_FORCE_PATTERN",
    summary: "Detecta ataque de FORÇA BRUTA",
    syntax: "BRUTE_FORCE_PATTERN() IS_TRUE",
    syntaxExplanation: "Muitas tentativas de senha para o mesmo usuário.",
    story: "500 tentativas de senha para email@empresa.com em 10 minutos.",
    problem: "Como detectar ataque de força bruta?",
    goldenTip: "💎 > 5 tentativas em 1 minuto = brute force. Bloquear por 30 min."
  },

  SIM_SWAP_INDICATOR: {
    name: "SIM_SWAP_INDICATOR",
    summary: "Detecta possível SIM SWAP (troca de chip)",
    syntax: "SIM_SWAP_INDICATOR() IS_TRUE",
    syntaxExplanation: "Sinais de que número de telefone foi portado recentemente.",
    story: "Telefone portado há 2h e já está fazendo reset de senha.",
    problem: "Como detectar SIM swap para account takeover?",
    goldenTip: "💎 SIM swap + reset senha + device novo + transferência grande = ATO garantido"
  },

  DEVICE_CLONING_DETECTION: {
    name: "DEVICE_CLONING_DETECTION",
    summary: "Detecta CLONAGEM DE DEVICE (mesmo device em 2 lugares)",
    syntax: "DEVICE_CLONING_DETECTION() IS_TRUE",
    syntaxExplanation: "Mesmo deviceId ativo em localizações diferentes simultaneamente.",
    story: "Mesmo celular logado em SP e Miami ao mesmo tempo = clonado.",
    problem: "Como detectar device clonado/spoofado?",
    goldenTip: "💎 Device em 2 lugares ao mesmo tempo = impossível = clonagem/spoofing"
  },

  MULE_ACCOUNT_INDICATOR: {
    name: "MULE_ACCOUNT_INDICATOR",
    summary: "Detecta CONTA MULA (usada para receber e repassar dinheiro)",
    syntax: "MULE_ACCOUNT_INDICATOR() IS_TRUE",
    syntaxExplanation: "Conta que recebe de origens suspeitas e rapidamente transfere.",
    story: "Conta de estudante recebe R$ 50k de origem suspeita, transfere 90% em 1h.",
    problem: "Como identificar mule accounts?",
    goldenTip: "💎 Padrão mula:\n• Recebe de origem desconhecida\n• Transfere quase tudo rapidamente\n• Fica com pequena 'comissão'\n• Perfil inconsistente com valores"
  },

  CIRCULAR_PAYMENT_DETECTION: {
    name: "CIRCULAR_PAYMENT_DETECTION",
    summary: "Detecta pagamento CIRCULAR (A→B→C→A)",
    syntax: "CIRCULAR_PAYMENT_DETECTION() IS_TRUE",
    syntaxExplanation: "Dinheiro que volta para a origem através de intermediários.",
    story: "A envia para B, B para C, C para A = círculo completo.",
    problem: "Como detectar circulação de dinheiro (layering circular)?",
    goldenTip: "💎 Circulação = lavagem clássica. Valores similares em loop = altíssimo risco."
  },

  SPLIT_PAYMENT_PATTERN: {
    name: "SPLIT_PAYMENT_PATTERN",
    summary: "Detecta SPLIT PAYMENT (divisão de valor grande em pequenos)",
    syntax: "SPLIT_PAYMENT_PATTERN() IS_TRUE",
    syntaxExplanation: "Valor grande dividido em múltiplos pequenos para evitar detecção.",
    story: "R$ 100k dividido em 50 transferências de R$ 2k cada.",
    problem: "Como detectar splitting/smurfing?",
    goldenTip: "💎 Valores similares + mesmo destino final + período curto = split"
  },

  MERCHANT_COLLUSION: {
    name: "MERCHANT_COLLUSION",
    summary: "Detecta CONLUIO COM MERCHANT (lojista participando da fraude)",
    syntax: "MERCHANT_COLLUSION() IS_TRUE",
    syntaxExplanation: "Lojista que processa TXs fraudulentas conscientemente.",
    story: "Lojista passa R$ 10k em cartões roubados e divide com fraudador.",
    problem: "Como identificar merchants desonestos?",
    goldenTip: "💎 Sinais:\n• Taxa de chargeback muito alta\n• Muitas TXs de cartões de outros países\n• Keyedtransactions com CVV"
  },

  AFFILIATE_FRAUD_PATTERN: {
    name: "AFFILIATE_FRAUD_PATTERN",
    summary: "Detecta FRAUDE DE AFILIADO",
    syntax: "AFFILIATE_FRAUD_PATTERN() IS_TRUE",
    syntaxExplanation: "Afiliado gerando cliques/vendas falsas para ganhar comissão.",
    story: "Afiliado criou bots para clicar em seus próprios links.",
    problem: "Como detectar fraude em programas de afiliados?",
    goldenTip: "💎 Padrões:\n• CTR impossível (>50%)\n• Conversões sem engagement\n• Tráfego de IPs de datacenter"
  }
};

// ═══════════════════════════════════════════════════════════════════════════════════════
// PARTE 11: OPERADORES BIOMÉTRICOS (15 operadores)
// ═══════════════════════════════════════════════════════════════════════════════════════

export const BIOMETRIC_SPECS: Record<string, OperatorSpec> = {
  BIOMETRIC_KEYSTROKE_DYNAMICS: {
    name: "BIOMETRIC_KEYSTROKE_DYNAMICS",
    summary: "Analisa padrão de DIGITAÇÃO do usuário",
    syntax: "BIOMETRIC_KEYSTROKE_DYNAMICS(userId) ANOMALY_DETECTED",
    syntaxExplanation: "Cada pessoa digita de forma única (velocidade, pressão, padrão). Mudança = possível impostor.",
    story: "João digita 60 palavras/min com pausas entre frases. De repente, 120 palavras/min sem pausas = bot ou outra pessoa.",
    problem: "Como detectar que NÃO é o usuário real digitando?",
    goldenTip: "💎 Keystroke dynamics é biometria COMPORTAMENTAL. Não precisa de hardware especial, só JavaScript.",
    engineBehavior: {
      description: "Análise de timing de teclado:",
      steps: [
        "1. Captura tempo entre teclas (dwell time, flight time)",
        "2. Compara com perfil histórico do usuário",
        "3. Calcula desvio estatístico",
        "4. Se desvio > threshold → anomalia"
      ],
      performance: "Precisa de pelo menos 50 chars para análise confiável"
    }
  },

  BIOMETRIC_MOUSE_MOVEMENT: {
    name: "BIOMETRIC_MOUSE_MOVEMENT",
    summary: "Analisa padrão de MOVIMENTO DO MOUSE",
    syntax: "BIOMETRIC_MOUSE_MOVEMENT(sessionId) ANOMALY_DETECTED",
    syntaxExplanation: "Humanos movem mouse de forma irregular/curva. Bots movem em linha reta ou com delay fixo.",
    story: "Mouse se movendo em linhas retas perfeitas entre cliques = bot.",
    problem: "Como distinguir humano de bot pelo comportamento do mouse?",
    goldenTip: "💎 Bots têm movimento muito 'perfeito'. Humanos têm tremor, curvas, hesitação."
  },

  TYPING_SPEED_ANOMALY: {
    name: "TYPING_SPEED_ANOMALY",
    summary: "Detecta velocidade de digitação ANORMAL",
    syntax: "TYPING_SPEED_ANOMALY() IS_TRUE",
    syntaxExplanation: "Digitação muito rápida (>150 wpm) ou muito regular = bot.",
    story: "Formulário preenchido em 2 segundos com 500 caracteres = paste/bot.",
    problem: "Como detectar preenchimento automático/bot?",
    goldenTip: "💎 > 120 wpm sustentado = impossível para humano. Digitador profissional: ~80 wpm."
  },

  LIVENESS_DETECTION_FAILED: {
    name: "LIVENESS_DETECTION_FAILED",
    summary: "Detecta falha na verificação de VIVACIDADE (liveness)",
    syntax: "LIVENESS_DETECTION_FAILED() IS_TRUE",
    syntaxExplanation: "Sistema não conseguiu confirmar que é pessoa real (não foto/vídeo).",
    story: "Fraudador tentou fazer selfie com foto impressa do dono da conta.",
    problem: "Como garantir que é pessoa REAL na frente da câmera?",
    goldenTip: "💎 Liveness checks:\n• Piscar\n• Virar cabeça\n• Sorrir\n• Análise de textura (skin vs papel)"
  },

  LIVENESS_DETECTION_PASSED: {
    name: "LIVENESS_DETECTION_PASSED",
    summary: "Confirma que verificação de VIVACIDADE passou",
    syntax: "LIVENESS_DETECTION_PASSED() IS_TRUE",
    syntaxExplanation: "Sistema confirmou que é pessoa real.",
    story: "Cliente passou no liveness check e pode prosseguir com onboarding.",
    problem: "Como confirmar presença física do usuário?",
    goldenTip: "💎 Liveness PASSADO não garante que é a pessoa CERTA. Combine com face match."
  },

  FACE_TO_ID_PHOTO_MATCHING: {
    name: "FACE_TO_ID_PHOTO_MATCHING",
    summary: "Compara selfie com foto do documento",
    syntax: "FACE_TO_ID_PHOTO_MATCHING(selfie, documentPhoto) MATCH_SCORE GT 0.9",
    syntaxExplanation: "Score de similaridade facial. > 0.9 = muito provável mesma pessoa.",
    story: "Selfie e RG com faces diferentes = documento de outra pessoa.",
    problem: "Como garantir que documento pertence ao usuário?",
    goldenTip: "💎 Threshold recomendado:\n• > 0.95 = match automático\n• 0.8-0.95 = revisão manual\n• < 0.8 = reject"
  },

  FACE_MATCH_SCORE_LT: {
    name: "FACE_MATCH_SCORE_LT",
    summary: "Verifica se score de face match está ABAIXO do threshold",
    syntax: "FACE_MATCH_SCORE_LT(0.8)",
    syntaxExplanation: "Score < 0.8 = baixa confiança de que é mesma pessoa.",
    story: "Score 0.5 = provavelmente pessoas diferentes.",
    problem: "Como rejeitar face matches de baixa confiança?",
    goldenTip: "💎 Óculos, barba, iluminação podem baixar score. < 0.6 = muito diferente."
  },

  VOICE_BIOMETRIC_MATCH: {
    name: "VOICE_BIOMETRIC_MATCH",
    summary: "Verifica se voz combina com perfil gravado",
    syntax: "VOICE_BIOMETRIC_MATCH(audioSample, userId) SCORE GT 0.85",
    syntaxExplanation: "Compara 'voiceprint' atual com gravação de cadastro.",
    story: "Fraudador ligou fingindo ser cliente, mas voz não bateu.",
    problem: "Como verificar identidade por voz (call centers)?",
    goldenTip: "💎 Voice biometrics funciona bem com 5+ segundos de áudio limpo."
  },

  VOICE_DEEPFAKE_DETECTION: {
    name: "VOICE_DEEPFAKE_DETECTION",
    summary: "Detecta voz sintetizada/deepfake",
    syntax: "VOICE_DEEPFAKE_DETECTION(audioSample) DEEPFAKE_PROBABILITY GT 0.7",
    syntaxExplanation: "Analisa se áudio foi gerado por IA.",
    story: "Fraudador usou clonagem de voz por IA para autorizar transação.",
    problem: "Como detectar voz clonada/sintetizada?",
    goldenTip: "💎 Deepfake de voz está cada vez melhor. Combinar com outros fatores de autenticação."
  },

  FACE_DEEPFAKE_DETECTION: {
    name: "FACE_DEEPFAKE_DETECTION",
    summary: "Detecta vídeo/imagem facial deepfake",
    syntax: "FACE_DEEPFAKE_DETECTION(image) DEEPFAKE_PROBABILITY GT 0.7",
    syntaxExplanation: "Analisa se imagem facial foi manipulada por IA.",
    story: "Fraudador criou deepfake do CEO para autorizar transferência.",
    problem: "Como detectar deepfakes visuais?",
    goldenTip: "💎 Sinais de deepfake:\n• Bordas do rosto instáveis\n• Piscadas irregulares\n• Texturas de pele estranhas"
  },

  INJECTION_ATTACK_DETECTION: {
    name: "INJECTION_ATTACK_DETECTION",
    summary: "Detecta ataque de INJEÇÃO em biometria",
    syntax: "INJECTION_ATTACK_DETECTION() IS_TRUE",
    syntaxExplanation: "Fraudador injetou imagem/vídeo fake direto na API, sem câmera real.",
    story: "Fraudador mandou foto direto para endpoint de selfie, sem passar pela câmera.",
    problem: "Como garantir que biometria veio de captura real, não de replay?",
    goldenTip: "💎 Defesas:\n• Challenge-response (mostrar número aleatório)\n• Verificar metadados de câmera\n• Análise de ruído de sensor"
  },

  DOCUMENT_FORGERY_DETECTION: {
    name: "DOCUMENT_FORGERY_DETECTION",
    summary: "Detecta FALSIFICAÇÃO de documento",
    syntax: "DOCUMENT_FORGERY_DETECTION(documentImage) FORGERY_PROBABILITY GT 0.8",
    syntaxExplanation: "Analisa se documento foi adulterado (foto trocada, dados alterados).",
    story: "RG com foto diferente colada no lugar da original.",
    problem: "Como detectar documentos falsificados?",
    goldenTip: "💎 Análises:\n• Fontes inconsistentes\n• Bordas de foto\n• Elementos de segurança (hologramas)\n• Metadados de edição"
  },

  BEHAVIORAL_BASELINE_DEVIATION: {
    name: "BEHAVIORAL_BASELINE_DEVIATION",
    summary: "Detecta DESVIO do padrão comportamental do usuário",
    syntax: "BEHAVIORAL_BASELINE_DEVIATION(userId) GT 2",
    syntaxExplanation: "Comportamento atual > 2 desvios padrão do normal = anomalia.",
    story: "Usuário que sempre navega devagar de repente navega em alta velocidade.",
    problem: "Como detectar mudança de comportamento (possível conta comprometida)?",
    goldenTip: "💎 Baseline inclui:\n• Velocidade de navegação\n• Horários típicos\n• Devices usados\n• Valores de TX"
  },

  ADAPTIVE_BEHAVIORAL_ANALYTICS: {
    name: "ADAPTIVE_BEHAVIORAL_ANALYTICS",
    summary: "Sistema de análise comportamental ADAPTATIVA",
    syntax: "ADAPTIVE_BEHAVIORAL_ANALYTICS(userId) RISK_SCORE GT 70",
    syntaxExplanation: "ML que aprende o padrão de cada usuário e detecta anomalias.",
    story: "Sistema aprendeu que João acessa às 9h de SP. Acesso às 3h de Miami = score alto.",
    problem: "Como ter análise comportamental personalizada para cada usuário?",
    goldenTip: "💎 Adaptive aprende continuamente. Cada usuário tem 'perfil' próprio."
  },

  SEGMENT_OF_ONE_PROFILING: {
    name: "SEGMENT_OF_ONE_PROFILING",
    summary: "Perfil INDIVIDUAL do usuário (segment of one)",
    syntax: "SEGMENT_OF_ONE_PROFILING(userId) DEVIATION_DETECTED",
    syntaxExplanation: "Em vez de comparar com média geral, compara com o próprio usuário.",
    story: "Para Maria, R$ 5k é normal. Para João, R$ 5k é anomalia.",
    problem: "Como personalizar detecção de fraude por usuário?",
    goldenTip: "💎 Segment of one = cada cliente é seu próprio baseline. Mais preciso, menos falsos positivos."
  }
};
