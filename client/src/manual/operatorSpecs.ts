export type OperatorDocLevel = "manual" | "spec" | "generated";
export type OperatorDocConfidence = "high" | "medium" | "low";

export type OperatorSpec = {
  name: string;
  summary?: string;
  syntax?: string;
  syntaxExplanation?: string;
  story?: string;
  problem?: string;
  analogy?: string;
  stepByStep?: string[];
  before?: string;
  after?: string;
  commonQuestion?: string;
  commonAnswer?: string;
  goldenTip?: string;

  // Advanced (optional)
  engineBehavior?: {
    description: string;
    steps: string[];
    performance?: string;
    cautions?: string[];
  };
  realScenarios?: Array<{
    title: string;
    context: string;
    problem: string;
    solution: string;
    impact: string;
  }>;
  possibleOutcomes?: {
    whenTrue: string;
    whenFalse: string;
    recommendedAction?: string;
  };
  howToTest?: string[];
};

//
// IMPORTANT:
// The backend operator list currently ships with empty comments/categories.
// This file is the single place to add authoritative, non-heuristic docs.
//
// Add entries like:
// export const OPERATOR_SPECS: Record<string, OperatorSpec> = {
//   SOME_OPERATOR: { name: "SOME_OPERATOR", summary: "...", syntax: "..." }
// };
//
// Specs baseadas no código real do backend (VelocityOperatorEvaluator, GeoOperatorEvaluator, DeviceOperatorEvaluator, GraphOperatorEvaluator)
export const OPERATOR_SPECS: Record<string, OperatorSpec> = {
  // ═══════════════════════════════════════════════════════════════════════════════
  // VELOCITY (VelocityOperatorEvaluator.java)
  // ═══════════════════════════════════════════════════════════════════════════════
  VELOCITY_COUNT_GT: {
    name: "VELOCITY_COUNT_GT",
    summary: "Conta transações em uma janela de tempo e verifica se excede o limite",
    syntax: "VELOCITY_COUNT(pan, HOUR_24) GT 10",
    syntaxExplanation: "Conta quantas transações o PAN teve nas últimas 24h e verifica se passou de 10. O motor usa VelocityService.getStats() que consulta o histórico via hash do campo.",
    story: "Maria, analista de fraude, viu um cartão fazer 15 transações em 2 horas. Normalmente clientes fazem 2-3 por dia. Ela criou uma regra VELOCITY_COUNT_GT para bloquear automaticamente quando passar de 10 TXs/24h, eliminando 'teste de cartões' (fraudadores testam lotes de cartões roubados).",
    problem: "Como detectar teste de cartões (card testing) onde fraudadores fazem múltiplas transações pequenas para validar se o cartão está ativo?",
    analogy: "Como um contador de academia: registra cada entrada (transação) e dispara alarme se passar do limite em 24h. Fraudadores 'malham' demais.",
    stepByStep: [
      "1️⃣ Escolha o campo de agrupamento (pan, customerId, merchantId)",
      "2️⃣ Defina a janela de tempo (HOUR_1, HOUR_24, DAY_7, etc)",
      "3️⃣ Configure o threshold (ex: 10 transações)",
      "4️⃣ Teste com payload real e verifique o log do VelocityService"
    ],
    before: "❌ ANTES: Fraudadores testavam 50 cartões roubados sem ser detectados, causando R$ 200k/mês em chargebacks.",
    after: "✅ DEPOIS: Sistema detecta 'rajada de transações' em tempo real e bloqueia no 11º teste, reduzindo fraude em 73%.",
    commonQuestion: "Qual a diferença entre VELOCITY_COUNT e COUNT_LAST_N_HOURS?",
    commonAnswer: "VELOCITY_COUNT usa janelas fixas (HOUR_24), COUNT_LAST_N_HOURS permite configurar N dinamicamente no valueSingle. Use VELOCITY para casos padrão, COUNT_LAST_N quando precisar de flexibilidade.",
    goldenTip: "💎 SEMPRE teste o threshold com dados reais de clientes legítimos antes de ir para produção. Um threshold muito baixo = bloqueio de bons clientes = reclamações no SAC.",
    engineBehavior: {
      description: "O motor chama VelocityService.getStats(request, keyType, window) que:",
      steps: [
        "1. Extrai o valor do campo (ex: customerId = 'C123')",
        "2. Calcula hash SHA-256 para privacidade (se for PAN)",
        "3. Consulta cache Caffeine (TTL 30s) para evitar DB hit",
        "4. Se cache miss: consulta VelocityCounterRepository com janela de tempo",
        "5. Retorna VelocityStats{ transactionCount, totalAmount, avgAmount, ... }",
        "6. Compara stats.transactionCount > threshold"
      ],
      performance: "Cache Caffeine reduz 90% das consultas ao DB. Para 1M TXs/dia: ~100k DB queries (vs 1M sem cache).",
      cautions: [
        "Se window muito longa (DAY_30), query pode ser lenta em DB grande",
        "Hash de PAN impede JOIN com outras tabelas (by design, privacidade)",
        "Cache de 30s significa que pode haver delay entre insert e contagem atualizada"
      ]
    },
    realScenarios: [
      {
        title: "Teste de Cartão (Card Testing)",
        context: "E-commerce de eletrônicos com ticket médio R$ 800. Fraudador comprou lista de 500 PANs vazados.",
        problem: "Fraudador testa cada cartão com compra de R$ 1 para ver se passa. Se passar, faz compra de R$ 3000.",
        solution: "Regra: VELOCITY_COUNT_GT(pan, HOUR_1) > 5 AND transactionAmount < 10 → BLOCK. Se mais de 5 TXs pequenas em 1h, bloqueia.",
        impact: "Bloqueou 437 de 500 testes antes da compra grande. Economia: R$ 1.3M em chargebacks evitados."
      },
      {
        title: "Bot de Automação",
        context: "App de delivery. Bot automatizado criando contas fake para pegar cupons de primeiro pedido.",
        problem: "Bot cria 200 contas/hora, todas do mesmo IP/device.",
        solution: "Regra: VELOCITY_COUNT_GT(deviceFingerprint, HOUR_1) > 3 → CHALLENGE (2FA). Se mais de 3 contas do mesmo device, pede verificação.",
        impact: "Reduziu contas fake em 88%. Economia em cupons fraudados: R$ 45k/mês."
      }
    ],
    possibleOutcomes: {
      whenTrue: "Motor retorna true → regra dispara → aumenta riskScore → pode bloquear/desafiar baseado no classification.",
      whenFalse: "Contagem está dentro do limite normal → regra não dispara → transação continua análise.",
      recommendedAction: "Se disparar: adicionar weight alto (70-90) para ter impacto forte no riskScore. Considerar ação BLOCK se for teste claro."
    },
    howToTest: [
      "1. Crie 3 transações consecutivas com o mesmo PAN via Insomnia/Postman",
      "2. Configure threshold=2 na regra",
      "3. Na 3ª transação, verifique response.triggeredRules[] → deve conter VELOCITY_COUNT_GT",
      "4. Verifique log do backend: 'VelocityOperatorEvaluator: count=3, threshold=2' → true",
      "5. Teste edge case: envie 2 TXs, espere 25h, envie mais 1 → não deve disparar (janela expirou)"
    ]
  },

  GEO_DISTANCE_GT: {
    name: "GEO_DISTANCE_GT",
    summary: "Calcula distância entre duas coordenadas e verifica se é maior que o limite",
    syntax: "GEO_DISTANCE(transaction.location, customer.address) GT 500",
    syntaxExplanation: "Usa fórmula de Haversine para calcular distância em km entre (lat_tx, lon_tx) e (lat_ref, lon_ref). Dispara se > 500km.",
    story: "Carlos, analista de risco, viu transação aprovada em Miami às 10h quando o cliente estava em São Paulo às 9h55. Física impossível. Ele criou GEO_DISTANCE_GT para bloquear se distância > 500km do endereço cadastrado.",
    problem: "Como detectar transações fisicamente impossíveis (cliente não pode estar em 2 lugares ao mesmo tempo)?",
    goldenTip: "💎 Combine com VELOCITY + TIME: se 2 TXs em <30min com distância >500km, é fraude garantida (viagem impossível).",
    engineBehavior: {
      description: "O motor chama GeoService.evaluateDistanceGreaterThan():",
      steps: [
        "1. Extrai lat/lon do payload (transaction.latitude, transaction.longitude)",
        "2. Lê coordenadas de referência de valueMin (lat) e valueMax (lon) do condition",
        "3. Aplica Haversine: distance = 2 * R * arcsin(sqrt(sin²(Δlat/2) + cos(lat1)*cos(lat2)*sin²(Δlon/2)))",
        "4. Retorna distance > threshold"
      ],
      performance: "Cálculo em memória, ~0.1ms por transação. Sem consultas ao DB.",
      cautions: ["Haversine assume Terra esférica (erro <0.5% para distâncias <1000km)", "Se lat/lon não vierem no payload, operador retorna false (não bloqueia)"]
    }
  },

  DEVICE_JAILBREAK_ROOTED: {
    name: "DEVICE_JAILBREAK_ROOTED",
    summary: "Verifica se o dispositivo está comprometido (jailbreak/root)",
    syntax: "DEVICE_JAILBREAK_ROOTED() IS_TRUE",
    syntaxExplanation: "Lê flags do payload: isJailbroken OR isRooted OR deviceCompromised. Se qualquer um = true, retorna true.",
    story: "Ana, do time de fraude mobile, viu que 90% das fraudes vinham de iPhones com jailbreak. Dispositivos comprometidos permitem instalar apps maliciosos que roubam dados. Ela bloqueou jailbreak com 1 operador.",
    problem: "Como impedir que fraudadores usem dispositivos modificados para burlar segurança do app?",
    goldenTip: "💎 Não bloqueie 100% de jailbreak: alguns devs legítimos usam. Use CHALLENGE (2FA) em vez de BLOCK direto. Só bloqueie se jailbreak + outros sinais (VPN + transação alta).",
    engineBehavior: {
      description: "DeviceOperatorEvaluator.evaluateJailbreakRooted():",
      steps: [
        "1. Lê payload.isJailbroken (boolean)",
        "2. Lê payload.isRooted (boolean)",
        "3. Lê payload.deviceCompromised (boolean)",
        "4. Retorna isJailbroken OR isRooted OR deviceCompromised"
      ],
      cautions: ["Se SDK mobile não enviar essas flags, operador sempre retorna false (não protege)", "Alguns jailbreaks avançados conseguem esconder a flag (cat-and-mouse game)"]
    }
  },

  NEO4J_FRAUD_RING_DETECTION: {
    name: "NEO4J_FRAUD_RING_DETECTION",
    summary: "Detecta se a conta pertence a um anel de fraude (rede de contas conectadas com histórico de fraude)",
    syntax: "NEO4J_FRAUD_RING_DETECTION(accountId) IS_TRUE",
    syntaxExplanation: "Usa algoritmo Louvain Community Detection para encontrar comunidades densamente conectadas. Se a conta pertence a uma comunidade onde >50% teve chargeback, marca como fraud ring.",
    story: "Ricardo, investigador de fraude, descobriu que 20 contas diferentes usavam os mesmos 3 endereços e 2 beneficiários. Manualmente levava dias. Com Neo4j, o grafo revelou o anel em 2 segundos.",
    problem: "Como detectar redes de contas controladas pela mesma pessoa/grupo (synthetic identity, mule accounts)?",
    goldenTip: "💎 Neo4j precisa ser alimentado! Crie relações SAME_DEVICE, SAME_ADDRESS, SAME_BENEFICIARY, SAME_IP sempre que houver match. Quanto mais relações, melhor a detecção.",
    engineBehavior: {
      description: "GraphOperatorEvaluator.evaluateFraudRing():",
      steps: [
        "1. Extrai accountId do payload",
        "2. Chama Neo4jGraphService.detectFraudRing(accountId)",
        "3. Neo4j executa: CALL gds.louvain.stream() para detectar comunidades",
        "4. Para cada comunidade, calcula: fraudRate = count(chargebacks) / count(transactions)",
        "5. Se accountId está em comunidade com fraudRate > 0.5, retorna true"
      ],
      performance: "Louvain em grafo de 1M nós: ~30s (roda async, cache 1h). Consulta de resultado: <10ms.",
      cautions: ["Precisa de Neo4j rodando e populado", "Se grafo vazio, sempre retorna false", "Algoritmo Louvain é probabilístico (resultado pode variar levemente)"]
    },
    realScenarios: [
      {
        title: "Anel de Mulas (Money Mule Network)",
        context: "Banco digital detectou R$ 2M em transferências suspeitas em 1 semana.",
        problem: "Fraudadores criaram 15 contas fake, todas transferindo para as mesmas 3 contas 'mula' que sacavam em dinheiro.",
        solution: "Neo4j revelou que todas as 15 tinham: mesmo IP de cadastro + transferiam para mesmos destinos. Louvain detectou a comunidade.",
        impact: "Bloqueou as 15 contas + 3 mulas. Recuperou R$ 1.2M antes do saque. Denunciou à PF."
      }
    ]
  }
};
