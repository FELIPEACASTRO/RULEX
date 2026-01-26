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
  },

  // ═══════════════════════════════════════════════════════════════════════════════
  // VELOCITY - OPERADORES ADICIONAIS (VelocityOperatorEvaluator.java)
  // ═══════════════════════════════════════════════════════════════════════════════
  VELOCITY_SUM_GT: {
    name: "VELOCITY_SUM_GT",
    summary: "Soma os valores das transações em uma janela de tempo e verifica se excede o limite",
    syntax: "VELOCITY_SUM(pan, HOUR_24, amount) GT 15000",
    syntaxExplanation: "Soma TODOS os valores (campo 'amount') das transações do PAN nas últimas 24h. Se a soma passar de R$ 15.000, dispara. Exemplo ULTRA didático: Cliente fez 10 compras de R$ 1.600 cada = R$ 16.000 total → DISPARA!",
    story: "Paula, analista de fraude, viu que fraudadores faziam múltiplas compras 'abaixo do radar' (R$ 900 cada) para não disparar alerta de valor alto. Somando tudo, passavam de R$ 20k/dia. Ela criou VELOCITY_SUM_GT para pegar esse padrão.",
    problem: "Como detectar fraudadores que fazem MUITAS compras pequenas para fugir do limite de valor único?",
    analogy: "🛒 Pense em um supermercado: cada item custa R$ 20 (barato), mas você encheu 3 carrinhos = R$ 1.200 no total. O caixa olha a SOMA, não cada item isolado.",
    stepByStep: [
      "1️⃣ Escolha o campo de agrupamento (pan, customerId, etc)",
      "2️⃣ Defina a janela de tempo (HOUR_1, HOUR_24, DAY_7)",
      "3️⃣ Especifique o campo a somar (amount, tipAmount, etc)",
      "4️⃣ Configure o threshold (ex: R$ 15.000)",
      "5️⃣ Exemplo: 10 TXs de R$ 1.600 = R$ 16.000 total → DISPARA ✅"
    ],
    before: "❌ ANTES: Fraudador fazia 20 compras de R$ 900 (total R$ 18k) sem ser detectado porque cada uma estava 'abaixo do radar' de R$ 5k.",
    after: "✅ DEPOIS: Sistema soma tudo e vê R$ 18k em 24h → bloqueia no meio do ataque. Economia: R$ 380k/mês.",
    commonQuestion: "Qual a diferença entre VELOCITY_SUM_GT e só verificar o valor da transação atual?",
    commonAnswer: "VELOCITY_SUM_GT olha o HISTÓRICO! Ele soma TODAS as transações passadas na janela. Uma TX de R$ 100 pode disparar se já houver R$ 14.950 antes dela (total = R$ 15.050).",
    goldenTip: "💎 EXEMPLO ULTRA DIDÁTICO:\n📅 Cliente João (PAN 4111****1111) em 24h:\n09:00 → R$ 1.200 (soma = R$ 1.200)\n10:30 → R$ 2.800 (soma = R$ 4.000)\n14:00 → R$ 3.500 (soma = R$ 7.500)\n18:20 → R$ 4.100 (soma = R$ 11.600)\n21:45 → R$ 5.000 (soma = R$ 16.600) ← DISPARA! (passou de R$ 15k)",
    engineBehavior: {
      description: "VelocityOperatorEvaluator chama VelocityService.getStats() e lê o campo 'totalAmount':",
      steps: [
        "1. Extrai PAN do payload (ex: '4111111111111111')",
        "2. Calcula hash SHA-256 do PAN",
        "3. Consulta VelocityCounterRepository: SELECT SUM(amount) FROM velocity_counters WHERE key_hash = ? AND timestamp > (NOW() - 24h)",
        "4. Retorna VelocityStats{ totalAmount: 16600.0 }",
        "5. Compara: 16600.0 > 15000.0 → TRUE → dispara regra"
      ],
      performance: "Usa índice em (key_hash, timestamp). Query retorna em ~5ms para janela de 24h. Cache Caffeine evita 90% das queries repetidas.",
      cautions: [
        "Se houver transações duplicadas (replay), soma será inflada",
        "Campos com valor NULL são ignorados (não somam zero, simplesmente não entram na conta)",
        "Atenção com moeda: se payload vier em centavos (150000 = R$ 1.500), ajuste o threshold!"
      ]
    },
    realScenarios: [
      {
        title: "Ataque 'Abaixo do Radar'",
        context: "Marketplace de luxo com limite de aprovação automática de R$ 5k. Transações acima precisam de análise manual.",
        problem: "Fraudador descobriu o limite e fazia compras de R$ 4.950 várias vezes ao dia, somando R$ 30k sem análise.",
        solution: "VELOCITY_SUM_GT(pan, DAY_1, amount) > 15000 → Se soma do dia passar de R$ 15k, bloqueia mesmo que cada TX seja pequena.",
        impact: "Bloqueou 342 ataques em 1 mês. Economia: R$ 2.1M. Fraudadores desistiram dessa tática."
      },
      {
        title: "Cliente Legítimo vs Fraudador - Distinguindo Padrões",
        context: "E-commerce. Threshold de R$ 10k/24h estava gerando MUITOS falsos positivos (clientes comprando presente de Natal).",
        problem: "Como diferenciar cliente fazendo compras legítimas de fraudador atacando?",
        solution: "VELOCITY_SUM_GT + VELOCITY_DISTINCT_GT: se soma > R$ 10k E merchants distintos > 5, é suspeito. Cliente normal compra no mesmo lugar.",
        impact: "Reduziu falsos positivos de 40% para 8%. Clientes VIPs não são mais incomodados."
      }
    ],
    possibleOutcomes: {
      whenTrue: "Soma das transações na janela excedeu o limite → riskScore aumenta significativamente → pode BLOQUEAR ou exigir 2FA dependendo do weight da regra.",
      whenFalse: "Soma ainda está dentro do aceitável → cliente pode continuar comprando normalmente.",
      recommendedAction: "Combine com COUNT: se SUM alto MAS count baixo (ex: 2 TXs de R$ 8k) → pode ser legítimo. Se SUM alto E count alto (20 TXs) → provável fraude."
    },
    howToTest: [
      "1. 🧪 Teste Básico: Envie 3 TXs de R$ 6.000 cada (total R$ 18k) com mesmo PAN",
      "2. Configure threshold = R$ 15.000 na regra",
      "3. Na 3ª TX, response.triggeredRules deve conter 'VELOCITY_SUM_GT'",
      "4. 📊 Verifique log: 'VelocityService: totalAmount=18000.0, threshold=15000.0 → true'",
      "5. 🎯 Edge Case 1: Envie TX de R$ 15.000,01 → deve disparar (passou por 1 centavo)",
      "6. 🎯 Edge Case 2: Envie 2 TXs de R$ 7.500 = R$ 15k exato → NÃO dispara (GT exclui igualdade)",
      "7. ⏰ Edge Case 3: Envie R$ 14k, espere 25h, envie R$ 2k → NÃO dispara (janela expirou)"
    ]
  },

  VELOCITY_AVG_GT: {
    name: "VELOCITY_AVG_GT",
    summary: "Calcula a MÉDIA dos valores das transações e verifica se é maior que o limite",
    syntax: "VELOCITY_AVG(customerId, DAY_7, amount) GT 500",
    syntaxExplanation: "Calcula: MÉDIA = SOMA(valores) ÷ QUANTIDADE de transações nos últimos 7 dias. Exemplo ULTRA didático: Cliente fez 10 TXs em 7 dias totalizando R$ 6.000 → média = R$ 600/TX → DISPARA (passou de R$ 500)!",
    story: "Marcos, analista de comportamento, notou que clientes normais têm ticket médio de R$ 80. Quando a média sobe para R$ 400, é sinal de conta comprometida (fraudador comprando coisas caras).",
    problem: "Como detectar MUDANÇA DE PADRÃO no valor das compras? Cliente que comprava R$ 50 agora compra R$ 500 = conta roubada!",
    analogy: "📊 Pense na sua conta de luz: você paga ~R$ 200/mês. De repente vem R$ 1.200. A MÉDIA subiu! Algo mudou (ar-condicionado novo ou medidor com problema).",
    goldenTip: "💎 EXEMPLO ULTRA DIDÁTICO:\n👤 Cliente Maria (ID C789) - Últimos 7 dias:\n5 TXs no total: R$ 120, R$ 150, R$ 95, R$ 180, R$ 105\nSOMA = R$ 650\nMÉDIA = R$ 650 ÷ 5 = R$ 130/TX ✅ OK (abaixo de R$ 500)\n\n🚨 DIA 8 (conta hackeada):\n3 TXs: R$ 2.500, R$ 3.200, R$ 2.800\nNova MÉDIA = (R$ 650 + R$ 8.500) ÷ 8 = R$ 1.143/TX ⚠️ DISPARA!",
    engineBehavior: {
      description: "VelocityService retorna VelocityStats.avgAmount:",
      steps: [
        "1. Query: SELECT SUM(amount) as total, COUNT(*) as qty FROM velocity_counters WHERE customer_id = ? AND timestamp > (NOW() - 7d)",
        "2. Calcula: avgAmount = total / qty",
        "3. Exemplo: total = R$ 9.150, qty = 8 → avg = R$ 1.143,75",
        "4. Compara: 1143.75 > 500.0 → TRUE"
      ],
      performance: "Mesma query de VELOCITY_SUM, só adiciona COUNT(*). Impacto zero de performance.",
      cautions: [
        "Se houver apenas 1 transação, AVG = valor dessa TX (óbvio, mas pode confundir iniciantes)",
        "Média é sensível a OUTLIERS: 1 TX gigante puxa a média pra cima"
      ]
    },
    howToTest: [
      "1. Crie 5 TXs de R$ 100 cada (total R$ 500, média R$ 100)",
      "2. Configure threshold = R$ 80",
      "3. Todas as 5 devem disparar a regra (média R$ 100 > R$ 80)",
      "4. Edge Case: Crie 1 TX de R$ 80 → média = R$ 80 → NÃO dispara (GT não inclui igualdade)"
    ]
  },

  VELOCITY_DISTINCT_GT: {
    name: "VELOCITY_DISTINCT_GT",
    summary: "Conta quantos valores DIFERENTES (únicos) aparecem em um campo e verifica se excede o limite",
    syntax: "VELOCITY_DISTINCT(pan, DAY_1, merchantId) GT 10",
    syntaxExplanation: "Conta merchants DISTINTOS que o PAN usou em 24h. Exemplo ULTRA didático: Cartão comprou em 15 lojas diferentes em 1 dia → DISPARA! Cliente normal compra em 2-3 lojas/dia.",
    story: "Luana, analista de cartões, viu um PAN fazer 30 compras em 24h em 25 lojas diferentes (farmácia, posto, eletrônica, joalheria...). Cliente real não varia tanto. Era teste de cartão clonado.",
    problem: "Como detectar fraudador TESTANDO cartão em vários lugares para ver se passa?",
    analogy: "🏪 Imagine você entrando em 20 lojas diferentes em 1 dia sem comprar quase nada em cada uma. Suspeito! Pessoa normal foca em 2-3 lojas.",
    goldenTip: "💎 EXEMPLO ULTRA DIDÁTICO:\n💳 Cartão 4532****7890 em 24h:\n09:15 → Merchant M001 (Farmácia) ✅\n09:45 → Merchant M001 (Farmácia de novo) ✅ (não conta, já viu)\n10:30 → Merchant M105 (Posto) ✅\n11:00 → Merchant M233 (Supermercado) ✅\n... (continua até 12 merchants distintos)\n⚠️ DISPARA quando chega no 11º merchant diferente!",
    engineBehavior: {
      description: "VelocityService.getStats() retorna distinctMerchants (ou distinctCountries, distinctMccs, etc):",
      steps: [
        "1. Query: SELECT COUNT(DISTINCT merchant_id) FROM velocity_counters WHERE pan_hash = ? AND timestamp > (NOW() - 24h)",
        "2. Retorna: distinctMerchants = 12",
        "3. Compara: 12 > 10 → TRUE"
      ],
      performance: "COUNT(DISTINCT ...) pode ser lento em tabelas gigantes. Considere materializar em cache se virar gargalo.",
      cautions: [
        "DISTINCT ignora NULLs: se merchantId vier NULL em algumas TXs, não conta",
        "Case-sensitive dependendo do DB: 'ABC' ≠ 'abc' podem ser contados como 2"
      ]
    },
    realScenarios: [
      {
        title: "Card Testing Multi-Merchant",
        context: "Fraudador tem lista de 500 cartões clonados. Quer testar quais estão ativos.",
        problem: "Ele faz compras de R$ 1 em 30 lojas diferentes em 2h para testar cada cartão rapidamente.",
        solution: "VELOCITY_DISTINCT(pan, HOUR_2, merchantId) > 5 AND amount < 10 → Se mais de 5 merchants diferentes com valores baixos, BLOQUEIA.",
        impact: "Bloqueou 98% dos testes. Fraudadores desistiram porque não conseguem mais validar os cartões."
      }
    ],
    howToTest: [
      "1. Crie 15 TXs com o mesmo PAN, cada uma em um merchantId diferente (M001, M002... M015)",
      "2. Configure threshold = 10",
      "3. Na 11ª TX (11º merchant distinto), deve disparar",
      "4. Edge Case: Crie 3 TXs no M001, 2 no M002 → apenas 2 distintos → não dispara"
    ]
  },

  // ═══════════════════════════════════════════════════════════════════════════════
  // GEO - OPERADORES ADICIONAIS (GeoOperatorEvaluator.java)
  // ═══════════════════════════════════════════════════════════════════════════════
  GEO_IN_POLYGON: {
    name: "GEO_IN_POLYGON",
    summary: "Verifica se uma coordenada (latitude, longitude) está DENTRO de um polígono geográfico",
    syntax: "GEO_IN_POLYGON(transaction.location, \"brazil_northeast\")",
    syntaxExplanation: "Usa algoritmo Ray Casting para verificar se o ponto (lat, lon) está dentro do polígono 'brazil_northeast'. Polígonos são pré-cadastrados (ex: zonas de risco, fronteiras, bairros).",
    story: "Sofia, analista geoespacial, criou polígonos de 'zonas de alto risco' (favelas dominadas por facções). Qualquer TX nessas áreas dispara alerta, mesmo sendo valor baixo.",
    problem: "Como detectar transações em ÁREAS GEOGRÁFICAS ESPECÍFICAS (não apenas distância, mas uma região complexa)?",
    analogy: "🗺️ Pense em um mapa do Brasil dividido em estados. Você quer saber se uma cidade está em SP. Não adianta medir distância - precisa verificar se está DENTRO do polígono de SP.",
    goldenTip: "💎 Ray Casting: desenha uma linha do ponto até o infinito. Se cruzar bordas do polígono um número ÍMPAR de vezes, está dentro. Se PAR, está fora.",
    engineBehavior: {
      description: "GeoService.evaluateInPolygon() usa Ray Casting:",
      steps: [
        "1. Extrai lat/lon do payload (ex: -23.5505, -46.6333 = São Paulo)",
        "2. Busca polígono 'brazil_northeast' no banco (lista de vértices)",
        "3. Ray Casting: desenha raio horizontal do ponto para a direita (→)",
        "4. Conta quantas vezes o raio cruza as bordas do polígono",
        "5. Se cruzamentos = ÍMPAR → está DENTRO → retorna true"
      ],
      performance: "Ray Casting é O(n) onde n = número de vértices. Polígono com 1000 vértices ~0.5ms. Cache polígonos em memória!",
      cautions: [
        "Polígonos precisam estar fechados (primeiro vértice = último vértice)",
        "Algoritmo falha se ponto estiver EXATAMENTE em uma borda (raro, mas possível)"
      ]
    },
    howToTest: [
      "1. Crie polígono de teste: quadrado [(0,0), (0,10), (10,10), (10,0), (0,0)]",
      "2. Teste ponto DENTRO: (5,5) → deve retornar TRUE",
      "3. Teste ponto FORA: (15,15) → deve retornar FALSE",
      "4. Teste ponto NA BORDA: (0,5) → comportamento indefinido (geralmente FALSE)"
    ]
  },

  // ═══════════════════════════════════════════════════════════════════════════════
  // DEVICE - OPERADORES CRÍTICOS (DeviceOperatorEvaluator.java)
  // ═══════════════════════════════════════════════════════════════════════════════
  EMULATOR_DETECTION: {
    name: "EMULATOR_DETECTION",
    summary: "Detecta se a transação vem de um emulador (dispositivo virtual, não real)",
    syntax: "EMULATOR_DETECTION() IS_TRUE",
    syntaxExplanation: "Lê flags do payload: isEmulator OR isVirtualMachine. Se qualquer um = true, retorna true. Exemplo ULTRA didático: Fraudador rodando 50 'celulares virtuais' no PC para criar contas fake → DETECTADO!",
    story: "Bruno, do time de segurança mobile, viu 200 contas criadas em 1h, todas do mesmo IP. Analisando, eram emuladores Android rodando em farm de servidores. Bloqueou emuladores e derrubou 99% das contas fake.",
    problem: "Como impedir bots que simulam milhares de dispositivos para atacar promoções/criar contas fake?",
    analogy: "🤖 Pense em um call center de telemarketing: 1 pessoa controla 10 telefones virtuais ao mesmo tempo. Não é humano real. Emulador = celular virtual.",
    goldenTip: "💎 EXEMPLO ULTRA DIDÁTICO:\n📱 Payload REAL (celular físico):\n{\n  deviceId: 'iPhone-12-ABC123',\n  isEmulator: false,\n  isVirtualMachine: false\n} → ✅ PASSA\n\n🤖 Payload FALSO (emulador):\n{\n  deviceId: 'generic_x86_arm',\n  isEmulator: true,\n  isVirtualMachine: false\n} → ⚠️ BLOQUEIA!",
    engineBehavior: {
      description: "DeviceOperatorEvaluator.evaluateEmulator():",
      steps: [
        "1. Lê payload.isEmulator (boolean)",
        "2. Lê payload.isVirtualMachine (boolean)",
        "3. Retorna: isEmulator OR isVirtualMachine",
        "4. Exemplos: true OR false = TRUE, false OR false = FALSE"
      ],
      performance: "Leitura de 2 flags booleanas do JSON = <0.1ms. Zero impacto.",
      cautions: [
        "SDK mobile PRECISA enviar essas flags. Se não enviar, sempre retorna false (não protege!)",
        "Emuladores avançados conseguem esconder a flag (cat-and-mouse game constante)",
        "Desenvolvedores legítimos usam emulador para testar - não bloqueie 100%, use CHALLENGE"
      ]
    },
    realScenarios: [
      {
        title: "Farm de Emuladores para Cupons",
        context: "App de delivery dava cupom de R$ 30 no primeiro pedido. Fraudadores criavam milhares de contas fake para pegar cupons.",
        problem: "1 PC rodando 100 emuladores Android criava 100 contas/hora = R$ 3.000 em cupons fraudados/hora.",
        solution: "EMULATOR_DETECTION() IS_TRUE → BLOCK ao cadastrar nova conta. Bloqueou 99.8% dos emuladores.",
        impact: "Economia de R$ 450k/mês em cupons fraudados. Apenas 0.2% de falsos positivos (devs testando)."
      }
    ],
    howToTest: [
      "1. Use SDK mobile real em celular físico → payload.isEmulator = false → não dispara",
      "2. Use Android Studio Emulator → payload.isEmulator = true → DISPARA ✅",
      "3. Teste edge case: isEmulator = false MAS isVirtualMachine = true → DISPARA (OR lógico)"
    ]
  },

  VPN_PROXY_DETECTION: {
    name: "VPN_PROXY_DETECTION",
    summary: "Detecta se a conexão vem de VPN, Proxy ou Datacenter (não é IP residencial real)",
    syntax: "VPN_PROXY_DETECTION() IS_TRUE",
    syntaxExplanation: "Lê flags: isVpn OR isProxy OR isDatacenter. Se qualquer um = true, retorna true. Exemplo ULTRA didático: Cliente em São Paulo usando VPN dos EUA para esconder localização → DETECTADO!",
    story: "Camila, analista de geolocalização, viu transações 'do Brasil' mas o IP era de servidor AWS em Virginia (EUA). Eram fraudadores usando VPN para fingir estar no Brasil e passar pelas regras de geofencing.",
    problem: "Como detectar quando alguém está ESCONDENDO sua localização real usando VPN/proxy?",
    analogy: "🎭 Pense em alguém usando máscara numa festa. Você sabe que a pessoa está lá, mas não sabe quem é. VPN = máscara digital.",
    goldenTip: "💎 EXEMPLO ULTRA DIDÁTICO:\n🌍 IP Real (residencial):\n{\n  ip: '187.95.123.45', // Vivo Fibra SP\n  isVpn: false,\n  isProxy: false,\n  isDatacenter: false\n} → ✅ PASSA\n\n🚨 IP Suspeito (VPN):\n{\n  ip: '45.142.212.61', // NordVPN server\n  isVpn: true,\n  isProxy: false,\n  isDatacenter: false\n} → ⚠️ BLOQUEIA!",
    engineBehavior: {
      description: "DeviceOperatorEvaluator.evaluateVpnProxy():",
      steps: [
        "1. Lê payload.isVpn (boolean)",
        "2. Lê payload.isProxy (boolean)",
        "3. Lê payload.isDatacenter (boolean)",
        "4. Retorna: isVpn OR isProxy OR isDatacenter"
      ],
      cautions: [
        "Detecção de VPN depende de bancos de IP atualizados (serviços como IPQualityScore, MaxMind)",
        "VPNs residenciais (IPs reais) são difíceis de detectar",
        "Usuários legítimos usam VPN para privacidade - não bloqueie 100%, só aumente riskScore"
      ]
    },
    realScenarios: [
      {
        title: "Fraude Cross-Border com VPN",
        context: "Regra bloqueava transações de fora do Brasil. Fraudadores usavam VPN brasileira para fingir estar no país.",
        problem: "VPN dava IP brasileiro (201.x.x.x) mas era servidor em Miami. Sistema achava que era TX doméstica.",
        solution: "VPN_PROXY_DETECTION() IS_TRUE AND transaction.country = 'BR' → Se VPN E diz ser Brasil, é suspeito.",
        impact: "Bloqueou 87% das fraudes cross-border disfarçadas. R$ 1.8M economizados em 3 meses."
      }
    ]
  },

  // ═══════════════════════════════════════════════════════════════════════════════
  // STRING - OPERADORES CRÍTICOS PARA EMAILS/PHONES
  // ═══════════════════════════════════════════════════════════════════════════════
  EMAIL_CONTAINS: {
    name: "CONTAINS",
    summary: "Verifica se um texto CONTÉM uma substring (está presente em qualquer posição)",
    syntax: "customer.email CONTAINS \"@tempmail\"",
    syntaxExplanation: "Procura a palavra '@tempmail' DENTRO do email. Exemplo ULTRA didático: 'joao@tempmail.com' → CONTÉM '@tempmail' → DISPARA! 'joao@gmail.com' → NÃO contém → passa.",
    story: "Daniela, do time de KYC, viu 500 contas criadas com emails temporários (tempmail, guerrillamail, 10minutemail). Esses emails expiram em 1h - sinal claro de fraude. Ela bloqueou QUALQUER email com 'tempmail' no nome.",
    problem: "Como bloquear provedores de email temporário/descartável sem listar TODOS os domínios (existem milhares)?",
    analogy: "🔍 Pense em procurar a palavra 'fraude' num livro de 500 páginas. Você não precisa ler tudo - só procurar onde 'fraude' aparece. CONTAINS faz isso com texto.",
    goldenTip: "💎 EXEMPLO ULTRA DIDÁTICO:\n📧 Emails testados:\n✅ 'maria.silva@gmail.com' CONTAINS '@gmail' → TRUE\n✅ 'joao123@tempmail.net' CONTAINS 'tempmail' → TRUE\n❌ 'ana@empresa.com.br' CONTAINS '@gmail' → FALSE\n❌ 'pedro@hotmail.com' CONTAINS 'temp' → FALSE (não tem 'temp')",
    engineBehavior: {
      description: "StringOperatorEvaluator.evaluateContains():",
      steps: [
        "1. Lê campo do payload (ex: customer.email = 'joao@tempmail.com')",
        "2. Lê substring a procurar (ex: 'tempmail')",
        "3. Faz busca case-insensitive: 'joao@TEMPMAIL.com'.toLowerCase().includes('tempmail')",
        "4. Retorna: true (encontrou) ou false (não encontrou)"
      ],
      performance: "String.includes() é O(n) onde n = tamanho do texto. Para emails (< 100 chars) = <0.01ms.",
      cautions: [
        "CONTAINS é case-INsensitive: 'TEMP' = 'temp' = 'Temp'",
        "Cuidado com falsos positivos: 'temp' pegaria 'template@empresa.com' (legítimo!)",
        "Use REGEX se precisar de match mais preciso"
      ]
    },
    realScenarios: [
      {
        title: "Bloqueio de Emails Temporários",
        context: "Fintech criando conta digital. Fraudadores usavam emails descartáveis para criar múltiplas contas e pegar bônus.",
        problem: "Existem 5.000+ domínios de email temporário. Impossível listar todos.",
        solution: "email CONTAINS 'tempmail' OR email CONTAINS 'guerrilla' OR email CONTAINS '10minute' → Bloqueia os mais comuns.",
        impact: "Bloqueou 78% das contas fake. Combinou com validação de email real (envio de código) para pegar os outros 22%."
      },
      {
        title: "Detecção de Email Corporativo",
        context: "B2B SaaS quer dar desconto apenas para emails corporativos, não @gmail/@hotmail.",
        problem: "Como identificar se é email corporativo?",
        solution: "NOT (email CONTAINS '@gmail' OR email CONTAINS '@hotmail' OR email CONTAINS '@yahoo') → Se NÃO tem provedor público, é corporativo.",
        impact: "Precisão de 95% na identificação. Evitou fraude de R$ 120k em descontos indevidos."
      }
    ],
    howToTest: [
      "1. Payload: { email: 'teste@tempmail.com' } → CONTAINS 'tempmail' → TRUE ✅",
      "2. Payload: { email: 'joao@gmail.com' } → CONTAINS 'tempmail' → FALSE",
      "3. Edge case: { email: 'TESTE@TEMPMAIL.COM' } → CONTAINS 'tempmail' → TRUE (case-insensitive)",
      "4. Edge case: { email: 'contemplate@art.com' } → CONTAINS 'temp' → TRUE ⚠️ (falso positivo!)"
    ]
  },

  PHONE_STARTS_WITH: {
    name: "STARTS_WITH",
    summary: "Verifica se um texto COMEÇA com um prefixo específico",
    syntax: "customer.phone STARTS_WITH \"+55\"",
    syntaxExplanation: "Verifica se o telefone começa com '+55' (código do Brasil). Exemplo ULTRA didático: '+5511987654321' → COMEÇA com '+55' → DISPARA! '+1234567890' → NÃO começa → passa.",
    story: "Eduardo, analista de onboarding, queria aceitar apenas clientes brasileiros. Usou STARTS_WITH '+55' para filtrar telefones. Qualquer DDD internacional era bloqueado na criação da conta.",
    problem: "Como validar o PAÍS de origem de um telefone sem fazer lookup em banco de dados?",
    analogy: "📞 Pense em um telefonema: quando toca, você vê +55 (Brasil) ou +1 (EUA) no início. Antes mesmo de atender, você sabe de onde é. STARTS_WITH faz isso.",
    goldenTip: "💎 EXEMPLO ULTRA DIDÁTICO:\n📱 Telefones testados:\n✅ '+5511987654321' STARTS_WITH '+55' → TRUE (Brasil)\n✅ '+5521999887766' STARTS_WITH '+55' → TRUE (RJ)\n❌ '+12025551234' STARTS_WITH '+55' → FALSE (EUA)\n❌ '11987654321' STARTS_WITH '+55' → FALSE (falta o +)",
    engineBehavior: {
      description: "StringOperatorEvaluator.evaluateStartsWith():",
      steps: [
        "1. Lê campo do payload (ex: customer.phone = '+5511987654321')",
        "2. Lê prefixo a verificar (ex: '+55')",
        "3. Compara: phone.startsWith('+55')",
        "4. Retorna: true ou false"
      ],
      cautions: [
        "STARTS_WITH é case-sensitive para strings, mas telefones são números então não importa",
        "Se telefone vier SEM código de país ('11987654321'), NÃO vai dar match com '+55'",
        "Normalize telefones antes: sempre adicionar +55 se for brasileiro"
      ]
    },
    realScenarios: [
      {
        title: "Bloqueio de DDIs de Alto Risco",
        context: "Banco digital viu 90% das fraudes vindas de telefones da Nigéria (+234), Gana (+233) e Costa do Marfim (+225).",
        problem: "Como bloquear países específicos sem precisar de banco de dados geo?",
        solution: "phone STARTS_WITH '+234' OR phone STARTS_WITH '+233' OR phone STARTS_WITH '+225' → BLOCK ao criar conta.",
        impact: "Reduziu fraude internacional de R$ 2.3M/mês para R$ 180k/mês (-92%)."
      }
    ],
    howToTest: [
      "1. Payload: { phone: '+5511999887766' } → STARTS_WITH '+55' → TRUE ✅",
      "2. Payload: { phone: '+12025551234' } → STARTS_WITH '+55' → FALSE",
      "3. Edge case: { phone: '5511999887766' } (sem +) → STARTS_WITH '+55' → FALSE ⚠️",
      "4. Edge case: { phone: '+551' } (incompleto) → STARTS_WITH '+55' → TRUE (cuidado!)"
    ]
  },

  REGEX_MATCH: {
    name: "REGEX",
    summary: "Verifica se um texto combina com um padrão de expressão regular (regex)",
    syntax: "customer.cpf MATCHES_REGEX /^\\d{3}\\.\\d{3}\\.\\d{3}-\\d{2}$/",
    syntaxExplanation: "Valida se CPF está no formato 123.456.789-00. Exemplo ULTRA didático: '123.456.789-00' → MATCH ✅ | '12345678900' → NÃO match (sem pontos/traço) ❌",
    story: "Fernanda, dev backend, precisava validar CPFs. Alguns vinham '12345678900', outros '123.456.789-00'. Usou REGEX para aceitar APENAS formato padrão brasileiro.",
    problem: "Como validar formatos complexos (CPF, placa de carro, CEP, etc) que têm estrutura específica?",
    analogy: "🔐 Pense em uma fechadura com formato específico: a chave precisa ter 3 dentes, espaço, 2 dentes, espaço, 1 dente. Se não tiver exatamente isso, não abre. REGEX = formato da chave.",
    goldenTip: "💎 EXEMPLO ULTRA DIDÁTICO:\n🆔 CPFs testados contra REGEX /^\\d{3}\\.\\d{3}\\.\\d{3}-\\d{2}$/:\n✅ '123.456.789-00' → MATCH (formato perfeito)\n✅ '987.654.321-99' → MATCH\n❌ '12345678900' → NO MATCH (falta pontos/traço)\n❌ '123.456.789-0' → NO MATCH (falta 1 dígito)\n❌ 'abc.def.ghi-jk' → NO MATCH (tem letras)",
    engineBehavior: {
      description: "StringOperatorEvaluator.evaluateRegex():",
      steps: [
        "1. Compila regex pattern (cache para performance)",
        "2. Lê valor do payload (ex: customer.cpf = '123.456.789-00')",
        "3. Testa: Pattern.compile(regex).matcher(value).matches()",
        "4. Retorna: true (match) ou false (no match)"
      ],
      performance: "Regex simples ~0.1ms. Regex complexos (lookahead, backtracking) podem chegar a 10ms. SEMPRE teste performance!",
      cautions: [
        "Regex mal escrito pode causar ReDoS (Regex Denial of Service) - trava o sistema!",
        "Escape caracteres especiais: . = qualquer char, \\. = ponto literal",
        "Use regex101.com para testar antes de colocar em produção"
      ]
    },
    realScenarios: [
      {
        title: "Validação de Placa Mercosul",
        context: "Sistema de pedágio precisa validar placas no novo formato ABC1D23 (Mercosul).",
        problem: "Placas antigas: ABC1234. Novas: ABC1D23. Como aceitar ambas?",
        solution: "plate MATCHES_REGEX /^[A-Z]{3}[0-9][A-Z0-9][0-9]{2}$/ → Aceita ambos formatos.",
        impact: "100% de precisão. Zero falsos positivos/negativos."
      }
    ],
    howToTest: [
      "1. Pattern: /^\\+55\\d{2}9\\d{8}$/ (celular BR)",
      "2. Test: '+5511987654321' → MATCH ✅",
      "3. Test: '+55119876543' (falta 1 dígito) → NO MATCH",
      "4. Test: '+551187654321' (fixo, não celular) → NO MATCH",
      "5. SEMPRE teste edge cases: string vazia, NULL, caracteres especiais"
    ]
  }
};
