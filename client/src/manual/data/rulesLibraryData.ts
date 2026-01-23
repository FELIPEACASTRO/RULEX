// ============================================================================
// TIPOS
// ============================================================================
export type RuleComplexity = "simples" | "media" | "complexa" | "extrema";
export type RuleCategory =
  | "valor"
  | "geolocalização"
  | "horário"
  | "velocidade"
  | "merchant"
  | "autenticação"
  | "dispositivo"
  | "comportamento"
  | "cartão"
  | "combinada";

export interface ExamplePayload {
  description: string;
  shouldTrigger: boolean;
  data: Record<string, unknown>;
}

export interface RuleExample {
  id: string;
  name: string;
  complexity: RuleComplexity;
  category: RuleCategory;
  narrativa: {
    situacao: string;
    problema: string;
    solucao: string;
  };
  passoAPasso: string[];
  json: {
    ruleName: string;
    ruleType: string;
    classification: string;
    conditions?: Array<{
      field: string;
      operator: string;
      value: unknown;
    }>;
    conditionGroups?: Array<{
      logic: string;
      conditions: Array<{
        field: string;
        operator: string;
        value: unknown;
      }>;
    }>;
    actions: Array<{
      type: string;
      value?: unknown;
    }>;
    priority: number;
    enabled: boolean;
  };
  payloads: ExamplePayload[];
  resultadoEsperado: string;
  operadoresUsados: string[];
  tags: string[];
}

// ============================================================================
// BIBLIOTECA DE REGRAS
// ============================================================================

// ---------------------------------------------------------------------------
// REGRAS SIMPLES (15 regras)
// ---------------------------------------------------------------------------
const REGRAS_SIMPLES: RuleExample[] = [
  {
    id: "S01",
    name: "Transação de Alto Valor",
    complexity: "simples",
    category: "valor",
    narrativa: {
      situacao:
        "Maria, uma aposentada de 65 anos, tem um cartão que normalmente usa para compras de supermercado (média R$300/mês). Às 3h da manhã, aparece uma compra de R$15.000 em uma loja de eletrônicos.",
      problema:
        "Compras de valor muito acima do padrão do cliente são indicadores clássicos de fraude, especialmente quando o cartão foi clonado ou roubado.",
      solucao:
        "Criar uma regra que alerta automaticamente transações acima de R$10.000 para análise manual.",
    },
    passoAPasso: [
      "1. Acesse a página de Regras (/rules)",
      "2. Clique em '+ Nova Regra'",
      "3. Preencha o nome: 'Alto Valor - Acima de 10K'",
      "4. Tipo: SECURITY",
      "5. Classificação: HARD",
      "6. Em Condições, adicione:",
      "   - Campo: transactionAmount",
      "   - Operador: GT (maior que)",
      "   - Valor: 10000",
      "7. Em Ações, adicione:",
      "   - Tipo: SET_DECISION",
      "   - Valor: REVIEW",
      "8. Prioridade: 100",
      "9. Clique em Salvar",
    ],
    json: {
      ruleName: "ALTO_VALOR_10K",
      ruleType: "SECURITY",
      classification: "HARD",
      conditions: [
        { field: "transactionAmount", operator: "GT", value: 10000 },
      ],
      actions: [{ type: "SET_DECISION", value: "REVIEW" }],
      priority: 100,
      enabled: true,
    },
    payloads: [
      {
        description: "Transação de R$15.000 - DEVE DISPARAR",
        shouldTrigger: true,
        data: {
          externalTransactionId: "TXN-001",
          transactionAmount: 15000,
          transactionCurrency: "BRL",
          merchantName: "Loja Eletrônicos",
          cardCountry: "BR",
        },
      },
      {
        description: "Transação de R$500 - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: {
          externalTransactionId: "TXN-002",
          transactionAmount: 500,
          transactionCurrency: "BRL",
          merchantName: "Supermercado",
          cardCountry: "BR",
        },
      },
    ],
    resultadoEsperado: "decision: REVIEW, triggeredRules: ['ALTO_VALOR_10K']",
    operadoresUsados: ["GT"],
    tags: ["valor", "alto-risco", "básica"],
  },
  {
    id: "S02",
    name: "País Estrangeiro",
    complexity: "simples",
    category: "geolocalização",
    narrativa: {
      situacao:
        "João tem um cartão emitido no Brasil e nunca viajou para o exterior. De repente, aparecem compras na Rússia.",
      problema:
        "Uso internacional inesperado é um dos principais indicadores de cartão clonado.",
      solucao:
        "Alertar transações onde o país do merchant é diferente do país de emissão do cartão.",
    },
    passoAPasso: [
      "1. Acesse /rules e clique em '+ Nova Regra'",
      "2. Nome: 'País Diferente do Cartão'",
      "3. Tipo: SECURITY",
      "4. Condição: merchantCountry NEQ cardCountry",
      "5. Ação: SET_DECISION = REVIEW",
      "6. Salvar",
    ],
    json: {
      ruleName: "PAIS_ESTRANGEIRO",
      ruleType: "SECURITY",
      classification: "HARD",
      conditions: [
        { field: "merchantCountry", operator: "NEQ", value: "$cardCountry" },
      ],
      actions: [{ type: "SET_DECISION", value: "REVIEW" }],
      priority: 90,
      enabled: true,
    },
    payloads: [
      {
        description: "Cartão BR usado na Rússia - DEVE DISPARAR",
        shouldTrigger: true,
        data: {
          transactionAmount: 1500,
          merchantCountry: "RU",
          cardCountry: "BR",
        },
      },
      {
        description: "Cartão BR usado no Brasil - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: {
          transactionAmount: 1500,
          merchantCountry: "BR",
          cardCountry: "BR",
        },
      },
    ],
    resultadoEsperado: "decision: REVIEW",
    operadoresUsados: ["NEQ"],
    tags: ["geolocalização", "internacional"],
  },
  {
    id: "S03",
    name: "Transação de Madrugada",
    complexity: "simples",
    category: "horário",
    narrativa: {
      situacao:
        "Cliente que só faz compras durante o dia comercial tem compra às 4h da manhã.",
      problema:
        "Fraudadores preferem agir de madrugada quando o titular está dormindo.",
      solucao:
        "Alertar transações entre 00:00 e 06:00.",
    },
    passoAPasso: [
      "1. Nova Regra: 'Transação Noturna'",
      "2. Condição: transactionTime GTE '00:00:00'",
      "3. Condição adicional (AND): transactionTime LTE '06:00:00'",
      "4. Ação: ADD_SCORE = 30",
    ],
    json: {
      ruleName: "MADRUGADA",
      ruleType: "CONTEXT",
      classification: "SOFT",
      conditions: [
        { field: "transactionTime", operator: "GTE", value: "00:00:00" },
        { field: "transactionTime", operator: "LTE", value: "06:00:00" },
      ],
      actions: [{ type: "ADD_SCORE", value: 30 }],
      priority: 50,
      enabled: true,
    },
    payloads: [
      {
        description: "Compra às 03:30 - DEVE DISPARAR",
        shouldTrigger: true,
        data: { transactionTime: "03:30:00", transactionAmount: 500 },
      },
      {
        description: "Compra às 14:00 - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { transactionTime: "14:00:00", transactionAmount: 500 },
      },
    ],
    resultadoEsperado: "score: +30",
    operadoresUsados: ["GTE", "LTE"],
    tags: ["horário", "madrugada"],
  },
  {
    id: "S04",
    name: "MCC de Alto Risco (Gambling)",
    complexity: "simples",
    category: "merchant",
    narrativa: {
      situacao:
        "Primeira transação de um cartão novo é em site de apostas online (MCC 7995).",
      problema:
        "Jogos de azar têm alto índice de fraude e chargeback.",
      solucao:
        "Bloquear ou revisar transações com MCCs de gambling.",
    },
    passoAPasso: [
      "1. Nova Regra: 'MCC Gambling'",
      "2. Condição: mcc IN [7995, 7994, 7993]",
      "3. Ação: SET_DECISION = DECLINE",
    ],
    json: {
      ruleName: "MCC_GAMBLING",
      ruleType: "SECURITY",
      classification: "HARD",
      conditions: [
        { field: "mcc", operator: "IN", value: [7995, 7994, 7993] },
      ],
      actions: [{ type: "SET_DECISION", value: "DECLINE" }],
      priority: 100,
      enabled: true,
    },
    payloads: [
      {
        description: "MCC 7995 (Gambling) - DEVE DISPARAR",
        shouldTrigger: true,
        data: { mcc: 7995, merchantName: "BetOnline", transactionAmount: 500 },
      },
      {
        description: "MCC 5411 (Supermercado) - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { mcc: 5411, merchantName: "Carrefour", transactionAmount: 500 },
      },
    ],
    resultadoEsperado: "decision: DECLINE",
    operadoresUsados: ["IN"],
    tags: ["mcc", "gambling", "bloqueio"],
  },
  {
    id: "S05",
    name: "Score de Autenticação Baixo",
    complexity: "simples",
    category: "autenticação",
    narrativa: {
      situacao:
        "Transação de alto valor com score 3DS de apenas 15 pontos.",
      problema:
        "Score baixo indica autenticação fraca ou fraudulenta.",
      solucao:
        "Revisar transações com score abaixo de 50.",
    },
    passoAPasso: [
      "1. Nova Regra: 'Score 3DS Baixo'",
      "2. Condição: consumerAuthenticationScore LT 50",
      "3. Ação: SET_DECISION = REVIEW",
    ],
    json: {
      ruleName: "SCORE_3DS_BAIXO",
      ruleType: "SECURITY",
      classification: "HARD",
      conditions: [
        { field: "consumerAuthenticationScore", operator: "LT", value: 50 },
      ],
      actions: [{ type: "SET_DECISION", value: "REVIEW" }],
      priority: 80,
      enabled: true,
    },
    payloads: [
      {
        description: "Score 15 - DEVE DISPARAR",
        shouldTrigger: true,
        data: { consumerAuthenticationScore: 15, transactionAmount: 5000 },
      },
      {
        description: "Score 85 - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { consumerAuthenticationScore: 85, transactionAmount: 5000 },
      },
    ],
    resultadoEsperado: "decision: REVIEW",
    operadoresUsados: ["LT"],
    tags: ["3ds", "autenticação", "score"],
  },
  {
    id: "S06",
    name: "CVV Ausente",
    complexity: "simples",
    category: "autenticação",
    narrativa: {
      situacao: "Compra online sem informar o código CVV do cartão.",
      problema: "Sem CVV, não há verificação de posse física do cartão.",
      solucao: "Alertar transações onde CVV não está presente.",
    },
    passoAPasso: [
      "1. Nova Regra: 'CVV Ausente'",
      "2. Condição: cvvPresent IS_FALSE",
      "3. Ação: ADD_SCORE = 25",
    ],
    json: {
      ruleName: "CVV_AUSENTE",
      ruleType: "SECURITY",
      classification: "SOFT",
      conditions: [{ field: "cvvPresent", operator: "IS_FALSE", value: null }],
      actions: [{ type: "ADD_SCORE", value: 25 }],
      priority: 60,
      enabled: true,
    },
    payloads: [
      {
        description: "CVV não informado - DEVE DISPARAR",
        shouldTrigger: true,
        data: { cvvPresent: false, transactionAmount: 1000 },
      },
      {
        description: "CVV informado - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { cvvPresent: true, transactionAmount: 1000 },
      },
    ],
    resultadoEsperado: "score: +25",
    operadoresUsados: ["IS_FALSE"],
    tags: ["cvv", "segurança"],
  },
  {
    id: "S07",
    name: "BIN de País de Alto Risco",
    complexity: "simples",
    category: "cartão",
    narrativa: {
      situacao: "Cartão com BIN emitido em país conhecido por fraudes.",
      problema: "Alguns países têm índices muito altos de fraude com cartões.",
      solucao: "Alertar cartões de países de alto risco.",
    },
    passoAPasso: [
      "1. Nova Regra: 'BIN País Alto Risco'",
      "2. Condição: cardCountry IN ['NG', 'RU', 'UA', 'VN']",
      "3. Ação: SET_DECISION = REVIEW",
    ],
    json: {
      ruleName: "BIN_PAIS_RISCO",
      ruleType: "SECURITY",
      classification: "HARD",
      conditions: [
        { field: "cardCountry", operator: "IN", value: ["NG", "RU", "UA", "VN"] },
      ],
      actions: [{ type: "SET_DECISION", value: "REVIEW" }],
      priority: 85,
      enabled: true,
    },
    payloads: [
      {
        description: "Cartão da Nigéria - DEVE DISPARAR",
        shouldTrigger: true,
        data: { cardCountry: "NG", transactionAmount: 2000 },
      },
      {
        description: "Cartão do Brasil - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { cardCountry: "BR", transactionAmount: 2000 },
      },
    ],
    resultadoEsperado: "decision: REVIEW",
    operadoresUsados: ["IN"],
    tags: ["bin", "país", "risco"],
  },
  {
    id: "S08",
    name: "Merchant Suspeito (Nome)",
    complexity: "simples",
    category: "merchant",
    narrativa: {
      situacao: "Merchant com nome contendo palavras suspeitas como 'CRYPTO' ou 'CASINO'.",
      problema: "Merchants de criptomoedas e cassinos têm alto risco.",
      solucao: "Detectar palavras-chave no nome do merchant.",
    },
    passoAPasso: [
      "1. Nova Regra: 'Merchant Crypto/Casino'",
      "2. Condição: merchantName CONTAINS_ANY ['CRYPTO', 'CASINO', 'BITCOIN']",
      "3. Ação: SET_DECISION = REVIEW",
    ],
    json: {
      ruleName: "MERCHANT_SUSPEITO",
      ruleType: "SECURITY",
      classification: "HARD",
      conditions: [
        { field: "merchantName", operator: "REGEX", value: "(CRYPTO|CASINO|BITCOIN|GAMBLING)" },
      ],
      actions: [{ type: "SET_DECISION", value: "REVIEW" }],
      priority: 75,
      enabled: true,
    },
    payloads: [
      {
        description: "CRYPTO EXCHANGE LTD - DEVE DISPARAR",
        shouldTrigger: true,
        data: { merchantName: "CRYPTO EXCHANGE LTD", transactionAmount: 5000 },
      },
      {
        description: "SUPERMERCADO EXTRA - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { merchantName: "SUPERMERCADO EXTRA", transactionAmount: 300 },
      },
    ],
    resultadoEsperado: "decision: REVIEW",
    operadoresUsados: ["REGEX"],
    tags: ["merchant", "nome", "padrão"],
  },
  {
    id: "S09",
    name: "Cartão Expirado",
    complexity: "simples",
    category: "cartão",
    narrativa: {
      situacao: "Tentativa de uso de cartão com data de expiração vencida.",
      problema: "Cartão expirado não deveria ser aceito.",
      solucao: "Verificar se a data de expiração já passou.",
    },
    passoAPasso: [
      "1. Nova Regra: 'Cartão Expirado'",
      "2. Condição: cardExpirationDate LT NOW()",
      "3. Ação: SET_DECISION = DECLINE",
    ],
    json: {
      ruleName: "CARTAO_EXPIRADO",
      ruleType: "SECURITY",
      classification: "HARD",
      conditions: [
        { field: "cardExpirationDate", operator: "LT", value: "NOW()" },
      ],
      actions: [{ type: "SET_DECISION", value: "DECLINE" }],
      priority: 100,
      enabled: true,
    },
    payloads: [
      {
        description: "Cartão vencido em 2024 - DEVE DISPARAR",
        shouldTrigger: true,
        data: { cardExpirationDate: "2024-12-31", transactionDate: "2026-01-16" },
      },
      {
        description: "Cartão válido até 2028 - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { cardExpirationDate: "2028-12-31", transactionDate: "2026-01-16" },
      },
    ],
    resultadoEsperado: "decision: DECLINE",
    operadoresUsados: ["LT"],
    tags: ["cartão", "expiração", "bloqueio"],
  },
  {
    id: "S10",
    name: "Email com Padrão Suspeito",
    complexity: "simples",
    category: "comportamento",
    narrativa: {
      situacao: "Email do cliente com padrão típico de email descartável (números aleatórios).",
      problema: "Fraudadores usam emails temporários ou gerados automaticamente.",
      solucao: "Detectar padrões de email suspeitos via regex.",
    },
    passoAPasso: [
      "1. Nova Regra: 'Email Suspeito'",
      "2. Condição: customerEmail REGEX '^[a-z0-9]{10,}@(temp|fake|test)'",
      "3. Ação: ADD_SCORE = 40",
    ],
    json: {
      ruleName: "EMAIL_SUSPEITO",
      ruleType: "CONTEXT",
      classification: "SOFT",
      conditions: [
        { field: "customerEmail", operator: "REGEX", value: "^[a-z0-9]{10,}@(tempmail|fakeinbox|guerrilla)" },
      ],
      actions: [{ type: "ADD_SCORE", value: 40 }],
      priority: 55,
      enabled: true,
    },
    payloads: [
      {
        description: "Email tempmail - DEVE DISPARAR",
        shouldTrigger: true,
        data: { customerEmail: "abc123xyz789@tempmail.com", transactionAmount: 1000 },
      },
      {
        description: "Email normal - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { customerEmail: "joao.silva@gmail.com", transactionAmount: 1000 },
      },
    ],
    resultadoEsperado: "score: +40",
    operadoresUsados: ["REGEX"],
    tags: ["email", "padrão", "fraude"],
  },
  {
    id: "S11",
    name: "Transação Sem Valor",
    complexity: "simples",
    category: "valor",
    narrativa: {
      situacao: "Transação com valor zero ou negativo.",
      problema: "Pode indicar teste de cartão ou erro.",
      solucao: "Bloquear transações com valor <= 0.",
    },
    passoAPasso: [
      "1. Nova Regra: 'Valor Inválido'",
      "2. Condição: transactionAmount LTE 0",
      "3. Ação: SET_DECISION = DECLINE",
    ],
    json: {
      ruleName: "VALOR_INVALIDO",
      ruleType: "SECURITY",
      classification: "HARD",
      conditions: [{ field: "transactionAmount", operator: "LTE", value: 0 }],
      actions: [{ type: "SET_DECISION", value: "DECLINE" }],
      priority: 100,
      enabled: true,
    },
    payloads: [
      {
        description: "Valor zero - DEVE DISPARAR",
        shouldTrigger: true,
        data: { transactionAmount: 0 },
      },
      {
        description: "Valor R$100 - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { transactionAmount: 100 },
      },
    ],
    resultadoEsperado: "decision: DECLINE",
    operadoresUsados: ["LTE"],
    tags: ["valor", "validação"],
  },
  {
    id: "S12",
    name: "Canal Desconhecido",
    complexity: "simples",
    category: "comportamento",
    narrativa: {
      situacao: "Transação vinda de canal não reconhecido pelo sistema.",
      problema: "Canais desconhecidos podem indicar tentativa de bypass.",
      solucao: "Alertar transações de canais não catalogados.",
    },
    passoAPasso: [
      "1. Nova Regra: 'Canal Desconhecido'",
      "2. Condição: channel NOT_IN ['WEB', 'MOBILE', 'POS', 'ATM']",
      "3. Ação: ADD_SCORE = 20",
    ],
    json: {
      ruleName: "CANAL_DESCONHECIDO",
      ruleType: "CONTEXT",
      classification: "SOFT",
      conditions: [
        { field: "channel", operator: "NOT_IN", value: ["WEB", "MOBILE", "POS", "ATM"] },
      ],
      actions: [{ type: "ADD_SCORE", value: 20 }],
      priority: 40,
      enabled: true,
    },
    payloads: [
      {
        description: "Canal API - DEVE DISPARAR",
        shouldTrigger: true,
        data: { channel: "API", transactionAmount: 500 },
      },
      {
        description: "Canal WEB - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { channel: "WEB", transactionAmount: 500 },
      },
    ],
    resultadoEsperado: "score: +20",
    operadoresUsados: ["NOT_IN"],
    tags: ["canal", "validação"],
  },
  {
    id: "S13",
    name: "Primeira Transação Internacional",
    complexity: "simples",
    category: "geolocalização",
    narrativa: {
      situacao: "Cliente sem histórico internacional fazendo primeira compra no exterior.",
      problema: "Primeira transação internacional pode indicar cartão roubado/clonado.",
      solucao: "Usar indicador de histórico internacional.",
    },
    passoAPasso: [
      "1. Nova Regra: 'Primeira Internacional'",
      "2. Condição: userIndicator1 EQ 'FIRST_INTERNATIONAL'",
      "3. Ação: SET_DECISION = REVIEW",
    ],
    json: {
      ruleName: "PRIMEIRA_INTERNACIONAL",
      ruleType: "CONTEXT",
      classification: "HARD",
      conditions: [
        { field: "userIndicator1", operator: "EQ", value: "FIRST_INTERNATIONAL" },
      ],
      actions: [{ type: "SET_DECISION", value: "REVIEW" }],
      priority: 70,
      enabled: true,
    },
    payloads: [
      {
        description: "Primeira internacional - DEVE DISPARAR",
        shouldTrigger: true,
        data: { userIndicator1: "FIRST_INTERNATIONAL", merchantCountry: "US" },
      },
      {
        description: "Cliente com histórico - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { userIndicator1: "REGULAR", merchantCountry: "US" },
      },
    ],
    resultadoEsperado: "decision: REVIEW",
    operadoresUsados: ["EQ"],
    tags: ["internacional", "primeira"],
  },
  {
    id: "S14",
    name: "Parcelas Excessivas",
    complexity: "simples",
    category: "valor",
    narrativa: {
      situacao: "Compra parcelada em 24x - muito acima do normal.",
      problema: "Parcelamento muito alto pode indicar fraude de compra para revenda.",
      solucao: "Alertar transações com mais de 12 parcelas.",
    },
    passoAPasso: [
      "1. Nova Regra: 'Parcelas Excessivas'",
      "2. Condição: installments GT 12",
      "3. Ação: ADD_SCORE = 35",
    ],
    json: {
      ruleName: "PARCELAS_EXCESSIVAS",
      ruleType: "CONTEXT",
      classification: "SOFT",
      conditions: [{ field: "installments", operator: "GT", value: 12 }],
      actions: [{ type: "ADD_SCORE", value: 35 }],
      priority: 45,
      enabled: true,
    },
    payloads: [
      {
        description: "24 parcelas - DEVE DISPARAR",
        shouldTrigger: true,
        data: { installments: 24, transactionAmount: 5000 },
      },
      {
        description: "6 parcelas - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { installments: 6, transactionAmount: 5000 },
      },
    ],
    resultadoEsperado: "score: +35",
    operadoresUsados: ["GT"],
    tags: ["parcelas", "valor"],
  },
  {
    id: "S15",
    name: "IP em Blacklist",
    complexity: "simples",
    category: "dispositivo",
    narrativa: {
      situacao: "Transação originada de IP conhecido por atividades fraudulentas.",
      problema: "IPs de VPNs suspeitas ou data centers são usados por fraudadores.",
      solucao: "Manter lista de IPs bloqueados.",
    },
    passoAPasso: [
      "1. Nova Regra: 'IP Blacklist'",
      "2. Condição: ipAddress IN [lista de IPs]",
      "3. Ação: SET_DECISION = DECLINE",
    ],
    json: {
      ruleName: "IP_BLACKLIST",
      ruleType: "SECURITY",
      classification: "HARD",
      conditions: [
        { field: "ipAddress", operator: "STARTS_WITH", value: "185.220." },
      ],
      actions: [{ type: "SET_DECISION", value: "DECLINE" }],
      priority: 95,
      enabled: true,
    },
    payloads: [
      {
        description: "IP de Tor exit node - DEVE DISPARAR",
        shouldTrigger: true,
        data: { ipAddress: "185.220.101.50", transactionAmount: 1000 },
      },
      {
        description: "IP normal - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { ipAddress: "189.50.120.33", transactionAmount: 1000 },
      },
    ],
    resultadoEsperado: "decision: DECLINE",
    operadoresUsados: ["STARTS_WITH"],
    tags: ["ip", "blacklist", "bloqueio"],
  },
];

// ---------------------------------------------------------------------------
// REGRAS MÉDIAS (15 regras)
// ---------------------------------------------------------------------------
const REGRAS_MEDIAS: RuleExample[] = [
  {
    id: "M01",
    name: "Alto Valor + País Estrangeiro",
    complexity: "media",
    category: "combinada",
    narrativa: {
      situacao:
        "Compra de R$8.000 em joalheria em Dubai de cartão brasileiro sem histórico de viagens internacionais.",
      problema:
        "Combinação de alto valor E uso internacional aumenta muito o risco.",
      solucao:
        "Regra com duas condições AND: valor alto E país diferente.",
    },
    passoAPasso: [
      "1. Nova Regra: 'Alto Valor Internacional'",
      "2. Lógica: AND",
      "3. Condição 1: transactionAmount GT 5000",
      "4. Condição 2: merchantCountry NEQ cardCountry",
      "5. Ação: SET_DECISION = REVIEW",
    ],
    json: {
      ruleName: "ALTO_VALOR_INTERNACIONAL",
      ruleType: "SECURITY",
      classification: "HARD",
      conditions: [
        { field: "transactionAmount", operator: "GT", value: 5000 },
        { field: "merchantCountry", operator: "NEQ", value: "$cardCountry" },
      ],
      actions: [{ type: "SET_DECISION", value: "REVIEW" }],
      priority: 95,
      enabled: true,
    },
    payloads: [
      {
        description: "R$8000 em Dubai - DEVE DISPARAR",
        shouldTrigger: true,
        data: {
          transactionAmount: 8000,
          merchantCountry: "AE",
          cardCountry: "BR",
          merchantName: "Dubai Gold Souk",
        },
      },
      {
        description: "R$8000 no Brasil - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: {
          transactionAmount: 8000,
          merchantCountry: "BR",
          cardCountry: "BR",
        },
      },
    ],
    resultadoEsperado: "decision: REVIEW",
    operadoresUsados: ["GT", "NEQ"],
    tags: ["combinada", "valor", "internacional"],
  },
  {
    id: "M02",
    name: "Madrugada + Valor Médio",
    complexity: "media",
    category: "combinada",
    narrativa: {
      situacao: "Compra de R$2.000 às 4h da manhã em e-commerce.",
      problema: "Valor significativo em horário suspeito.",
      solucao: "Combinar verificação de horário com valor mínimo.",
    },
    passoAPasso: [
      "1. Nova Regra: 'Madrugada Alto Valor'",
      "2. Condição 1: transactionTime BETWEEN '00:00' AND '06:00'",
      "3. Condição 2: transactionAmount GT 1000",
      "4. Ação: ADD_SCORE = 50",
    ],
    json: {
      ruleName: "MADRUGADA_VALOR_MEDIO",
      ruleType: "CONTEXT",
      classification: "HARD",
      conditions: [
        { field: "transactionTime", operator: "GTE", value: "00:00:00" },
        { field: "transactionTime", operator: "LTE", value: "06:00:00" },
        { field: "transactionAmount", operator: "GT", value: 1000 },
      ],
      actions: [{ type: "ADD_SCORE", value: 50 }],
      priority: 70,
      enabled: true,
    },
    payloads: [
      {
        description: "R$2000 às 4h - DEVE DISPARAR",
        shouldTrigger: true,
        data: { transactionTime: "04:00:00", transactionAmount: 2000 },
      },
      {
        description: "R$2000 às 14h - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { transactionTime: "14:00:00", transactionAmount: 2000 },
      },
    ],
    resultadoEsperado: "score: +50",
    operadoresUsados: ["GTE", "LTE", "GT"],
    tags: ["horário", "valor", "combinada"],
  },
  {
    id: "M03",
    name: "MCC Risco + Score Baixo",
    complexity: "media",
    category: "combinada",
    narrativa: {
      situacao: "Compra em site de apostas com score 3DS de 25.",
      problema: "MCC de risco combinado com autenticação fraca.",
      solucao: "Verificar MCC e score simultaneamente.",
    },
    passoAPasso: [
      "1. Nova Regra: 'MCC Risco Score Baixo'",
      "2. Condição 1: mcc IN [7995, 6211]",
      "3. Condição 2: consumerAuthenticationScore LT 50",
      "4. Ação: SET_DECISION = DECLINE",
    ],
    json: {
      ruleName: "MCC_RISCO_SCORE_BAIXO",
      ruleType: "SECURITY",
      classification: "HARD",
      conditions: [
        { field: "mcc", operator: "IN", value: [7995, 6211, 5967] },
        { field: "consumerAuthenticationScore", operator: "LT", value: 50 },
      ],
      actions: [{ type: "SET_DECISION", value: "DECLINE" }],
      priority: 100,
      enabled: true,
    },
    payloads: [
      {
        description: "MCC 7995 + score 25 - DEVE DISPARAR",
        shouldTrigger: true,
        data: { mcc: 7995, consumerAuthenticationScore: 25 },
      },
      {
        description: "MCC 7995 + score 80 - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { mcc: 7995, consumerAuthenticationScore: 80 },
      },
    ],
    resultadoEsperado: "decision: DECLINE",
    operadoresUsados: ["IN", "LT"],
    tags: ["mcc", "score", "bloqueio"],
  },
  {
    id: "M04",
    name: "Dispositivo Novo + Valor Alto",
    complexity: "media",
    category: "combinada",
    narrativa: {
      situacao: "Primeira compra de um dispositivo desconhecido por R$5.000.",
      problema: "Device fingerprint novo com valor alto é arriscado.",
      solucao: "Combinar verificação de device com valor.",
    },
    passoAPasso: [
      "1. Nova Regra: 'Device Novo Alto Valor'",
      "2. Condição 1: userIndicator2 EQ 'NEW_DEVICE'",
      "3. Condição 2: transactionAmount GT 3000",
      "4. Ação: SET_DECISION = REVIEW",
    ],
    json: {
      ruleName: "DEVICE_NOVO_ALTO_VALOR",
      ruleType: "CONTEXT",
      classification: "HARD",
      conditions: [
        { field: "userIndicator2", operator: "EQ", value: "NEW_DEVICE" },
        { field: "transactionAmount", operator: "GT", value: 3000 },
      ],
      actions: [{ type: "SET_DECISION", value: "REVIEW" }],
      priority: 80,
      enabled: true,
    },
    payloads: [
      {
        description: "Device novo + R$5000 - DEVE DISPARAR",
        shouldTrigger: true,
        data: { userIndicator2: "NEW_DEVICE", transactionAmount: 5000 },
      },
      {
        description: "Device conhecido + R$5000 - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { userIndicator2: "KNOWN_DEVICE", transactionAmount: 5000 },
      },
    ],
    resultadoEsperado: "decision: REVIEW",
    operadoresUsados: ["EQ", "GT"],
    tags: ["device", "valor", "combinada"],
  },
  {
    id: "M05",
    name: "Endereço Divergente + Internacional",
    complexity: "media",
    category: "combinada",
    narrativa: {
      situacao: "Endereço de entrega em país diferente do billing e do cartão.",
      problema: "Divergência geográfica indica possível revenda fraudulenta.",
      solucao: "Verificar se shipping, billing e card são de países diferentes.",
    },
    passoAPasso: [
      "1. Nova Regra: 'Endereço Divergente'",
      "2. Condição 1: shippingAddress NOT_CONTAINS cardCountry",
      "3. Condição 2: merchantCountry NEQ cardCountry",
      "4. Ação: ADD_SCORE = 45",
    ],
    json: {
      ruleName: "ENDERECO_DIVERGENTE",
      ruleType: "CONTEXT",
      classification: "HARD",
      conditions: [
        { field: "shippingAddress", operator: "NOT_CONTAINS", value: "$cardCountry" },
        { field: "merchantCountry", operator: "NEQ", value: "$cardCountry" },
      ],
      actions: [{ type: "ADD_SCORE", value: 45 }],
      priority: 65,
      enabled: true,
    },
    payloads: [
      {
        description: "Cartão BR, merchant US, shipping UK - DEVE DISPARAR",
        shouldTrigger: true,
        data: {
          cardCountry: "BR",
          merchantCountry: "US",
          shippingAddress: "123 London Street, UK",
        },
      },
      {
        description: "Tudo no Brasil - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: {
          cardCountry: "BR",
          merchantCountry: "BR",
          shippingAddress: "Rua São Paulo 100, BR",
        },
      },
    ],
    resultadoEsperado: "score: +45",
    operadoresUsados: ["NOT_CONTAINS", "NEQ"],
    tags: ["endereço", "internacional", "divergência"],
  },
  {
    id: "M06",
    name: "Cartão Recém-Ativado + Alto Valor",
    complexity: "media",
    category: "combinada",
    narrativa: {
      situacao: "Cartão ativado há menos de 7 dias com compra de R$10.000.",
      problema: "Cartões novos são mais vulneráveis a engenharia social.",
      solucao: "Monitorar transações altas em cartões recentes.",
    },
    passoAPasso: [
      "1. Nova Regra: 'Cartão Novo Alto Valor'",
      "2. Condição 1: userIndicator3 EQ 'CARD_AGE_LESS_7D'",
      "3. Condição 2: transactionAmount GT 5000",
      "4. Ação: SET_DECISION = REVIEW",
    ],
    json: {
      ruleName: "CARTAO_NOVO_ALTO_VALOR",
      ruleType: "CONTEXT",
      classification: "HARD",
      conditions: [
        { field: "userIndicator3", operator: "EQ", value: "CARD_AGE_LESS_7D" },
        { field: "transactionAmount", operator: "GT", value: 5000 },
      ],
      actions: [{ type: "SET_DECISION", value: "REVIEW" }],
      priority: 85,
      enabled: true,
    },
    payloads: [
      {
        description: "Cartão novo + R$10000 - DEVE DISPARAR",
        shouldTrigger: true,
        data: { userIndicator3: "CARD_AGE_LESS_7D", transactionAmount: 10000 },
      },
      {
        description: "Cartão antigo + R$10000 - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { userIndicator3: "CARD_AGE_MORE_90D", transactionAmount: 10000 },
      },
    ],
    resultadoEsperado: "decision: REVIEW",
    operadoresUsados: ["EQ", "GT"],
    tags: ["cartão-novo", "valor", "combinada"],
  },
  {
    id: "M07",
    name: "Recorrente + Valor Acima do Normal",
    complexity: "media",
    category: "combinada",
    narrativa: {
      situacao: "Transação marcada como recorrente com valor 3x acima do histórico.",
      problema: "Fraudadores alteram valores de assinaturas.",
      solucao: "Monitorar variações em transações recorrentes.",
    },
    passoAPasso: [
      "1. Nova Regra: 'Recorrente Valor Anormal'",
      "2. Condição 1: recurringIndicator IS_TRUE",
      "3. Condição 2: userIndicator4 EQ 'AMOUNT_3X_AVERAGE'",
      "4. Ação: SET_DECISION = REVIEW",
    ],
    json: {
      ruleName: "RECORRENTE_VALOR_ANORMAL",
      ruleType: "ANOMALY",
      classification: "HARD",
      conditions: [
        { field: "recurringIndicator", operator: "IS_TRUE", value: null },
        { field: "userIndicator4", operator: "EQ", value: "AMOUNT_3X_AVERAGE" },
      ],
      actions: [{ type: "SET_DECISION", value: "REVIEW" }],
      priority: 75,
      enabled: true,
    },
    payloads: [
      {
        description: "Recorrente com 3x do valor - DEVE DISPARAR",
        shouldTrigger: true,
        data: { recurringIndicator: true, userIndicator4: "AMOUNT_3X_AVERAGE" },
      },
      {
        description: "Recorrente normal - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { recurringIndicator: true, userIndicator4: "AMOUNT_NORMAL" },
      },
    ],
    resultadoEsperado: "decision: REVIEW",
    operadoresUsados: ["IS_TRUE", "EQ"],
    tags: ["recorrente", "anomalia", "valor"],
  },
  {
    id: "M08",
    name: "CVV Errado + Múltiplas Tentativas",
    complexity: "media",
    category: "autenticação",
    narrativa: {
      situacao: "CVV incorreto após 3 tentativas.",
      problema: "Tentativas repetidas indicam teste de cartão.",
      solucao: "Combinar resultado de CVV com contador de tentativas.",
    },
    passoAPasso: [
      "1. Nova Regra: 'CVV Errado Multiplas'",
      "2. Condição 1: cvvResult EQ 'N'",
      "3. Condição 2: pinEntryCount GTE 3",
      "4. Ação: SET_DECISION = DECLINE",
    ],
    json: {
      ruleName: "CVV_ERRADO_MULTIPLAS",
      ruleType: "SECURITY",
      classification: "HARD",
      conditions: [
        { field: "cvvResult", operator: "EQ", value: "N" },
        { field: "pinEntryCount", operator: "GTE", value: 3 },
      ],
      actions: [{ type: "SET_DECISION", value: "DECLINE" }],
      priority: 100,
      enabled: true,
    },
    payloads: [
      {
        description: "CVV errado + 4 tentativas - DEVE DISPARAR",
        shouldTrigger: true,
        data: { cvvResult: "N", pinEntryCount: 4 },
      },
      {
        description: "CVV errado + 1 tentativa - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { cvvResult: "N", pinEntryCount: 1 },
      },
    ],
    resultadoEsperado: "decision: DECLINE",
    operadoresUsados: ["EQ", "GTE"],
    tags: ["cvv", "tentativas", "bloqueio"],
  },
  {
    id: "M09",
    name: "Terminal ATM + Valor Alto",
    complexity: "media",
    category: "combinada",
    narrativa: {
      situacao: "Saque de R$5.000 em ATM em outro estado.",
      problema: "Saques altos em ATM distante do endereço cadastrado.",
      solucao: "Monitorar saques ATM acima de limite.",
    },
    passoAPasso: [
      "1. Nova Regra: 'ATM Alto Valor'",
      "2. Condição 1: terminalType EQ 'ATM'",
      "3. Condição 2: transactionAmount GT 2000",
      "4. Ação: SET_DECISION = REVIEW",
    ],
    json: {
      ruleName: "ATM_ALTO_VALOR",
      ruleType: "CONTEXT",
      classification: "HARD",
      conditions: [
        { field: "terminalType", operator: "EQ", value: "ATM" },
        { field: "transactionAmount", operator: "GT", value: 2000 },
      ],
      actions: [{ type: "SET_DECISION", value: "REVIEW" }],
      priority: 70,
      enabled: true,
    },
    payloads: [
      {
        description: "ATM + R$5000 - DEVE DISPARAR",
        shouldTrigger: true,
        data: { terminalType: "ATM", transactionAmount: 5000 },
      },
      {
        description: "ATM + R$500 - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { terminalType: "ATM", transactionAmount: 500 },
      },
    ],
    resultadoEsperado: "decision: REVIEW",
    operadoresUsados: ["EQ", "GT"],
    tags: ["atm", "saque", "valor"],
  },
  {
    id: "M10",
    name: "Fallback EMV + Alto Valor",
    complexity: "media",
    category: "autenticação",
    narrativa: {
      situacao: "Terminal com chip forçando fallback para tarja magnética em compra de R$3.000.",
      problema: "Fallback pode indicar skimming ou cartão clonado.",
      solucao: "Alertar fallbacks em transações significativas.",
    },
    passoAPasso: [
      "1. Nova Regra: 'Fallback Alto Valor'",
      "2. Condição 1: posEntryMode STARTS_WITH '80'",
      "3. Condição 2: transactionAmount GT 1500",
      "4. Ação: SET_DECISION = REVIEW",
    ],
    json: {
      ruleName: "FALLBACK_ALTO_VALOR",
      ruleType: "SECURITY",
      classification: "HARD",
      conditions: [
        { field: "posEntryMode", operator: "STARTS_WITH", value: "80" },
        { field: "transactionAmount", operator: "GT", value: 1500 },
      ],
      actions: [{ type: "SET_DECISION", value: "REVIEW" }],
      priority: 85,
      enabled: true,
    },
    payloads: [
      {
        description: "Fallback + R$3000 - DEVE DISPARAR",
        shouldTrigger: true,
        data: { posEntryMode: "801", transactionAmount: 3000 },
      },
      {
        description: "EMV normal + R$3000 - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { posEntryMode: "051", transactionAmount: 3000 },
      },
    ],
    resultadoEsperado: "decision: REVIEW",
    operadoresUsados: ["STARTS_WITH", "GT"],
    tags: ["emv", "fallback", "segurança"],
  },
  {
    id: "M11",
    name: "ECI Sem Autenticação + E-commerce",
    complexity: "media",
    category: "autenticação",
    narrativa: {
      situacao: "Transação e-commerce com ECI indicando sem autenticação.",
      problema: "Transações online sem 3DS são mais arriscadas.",
      solucao: "Verificar ECI e canal simultaneamente.",
    },
    passoAPasso: [
      "1. Nova Regra: 'ECI Sem Auth Ecommerce'",
      "2. Condição 1: eci IN ['07', '00']",
      "3. Condição 2: channel EQ 'WEB'",
      "4. Ação: ADD_SCORE = 30",
    ],
    json: {
      ruleName: "ECI_SEM_AUTH_ECOMMERCE",
      ruleType: "SECURITY",
      classification: "SOFT",
      conditions: [
        { field: "eci", operator: "IN", value: ["07", "00"] },
        { field: "channel", operator: "EQ", value: "WEB" },
      ],
      actions: [{ type: "ADD_SCORE", value: 30 }],
      priority: 55,
      enabled: true,
    },
    payloads: [
      {
        description: "ECI 07 + WEB - DEVE DISPARAR",
        shouldTrigger: true,
        data: { eci: "07", channel: "WEB" },
      },
      {
        description: "ECI 05 + WEB - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { eci: "05", channel: "WEB" },
      },
    ],
    resultadoEsperado: "score: +30",
    operadoresUsados: ["IN", "EQ"],
    tags: ["eci", "3ds", "ecommerce"],
  },
  {
    id: "M12",
    name: "Limite Próximo + Alto Valor",
    complexity: "media",
    category: "valor",
    narrativa: {
      situacao: "Transação que consome 90% do limite disponível.",
      problema: "Fraudadores tentam maximizar uso antes de bloqueio.",
      solucao: "Alertar quando transação está próxima do limite.",
    },
    passoAPasso: [
      "1. Nova Regra: 'Limite Próximo'",
      "2. Condição: Use expressão transactionAmount > creditLimit * 0.9",
      "3. Ação: SET_DECISION = REVIEW",
    ],
    json: {
      ruleName: "LIMITE_PROXIMO",
      ruleType: "CONTEXT",
      classification: "HARD",
      conditions: [
        { field: "userIndicator5", operator: "EQ", value: "NEAR_LIMIT_90" },
        { field: "transactionAmount", operator: "GT", value: 5000 },
      ],
      actions: [{ type: "SET_DECISION", value: "REVIEW" }],
      priority: 80,
      enabled: true,
    },
    payloads: [
      {
        description: "Perto do limite + R$8000 - DEVE DISPARAR",
        shouldTrigger: true,
        data: { userIndicator5: "NEAR_LIMIT_90", transactionAmount: 8000 },
      },
      {
        description: "Limite OK + R$8000 - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { userIndicator5: "LIMIT_OK", transactionAmount: 8000 },
      },
    ],
    resultadoEsperado: "decision: REVIEW",
    operadoresUsados: ["EQ", "GT"],
    tags: ["limite", "valor", "risco"],
  },
  {
    id: "M13",
    name: "País Diferente + Mesmo Dia",
    complexity: "media",
    category: "geolocalização",
    narrativa: {
      situacao: "Compra no Brasil às 10h e na Rússia às 14h do mesmo dia.",
      problema: "Impossível fisicamente estar em dois lugares tão distantes.",
      solucao: "Usar indicador de impossible travel.",
    },
    passoAPasso: [
      "1. Nova Regra: 'Impossible Travel'",
      "2. Condição 1: userIndicator1 EQ 'IMPOSSIBLE_TRAVEL'",
      "3. Condição 2: merchantCountry NEQ cardCountry",
      "4. Ação: SET_DECISION = DECLINE",
    ],
    json: {
      ruleName: "IMPOSSIBLE_TRAVEL",
      ruleType: "ANOMALY",
      classification: "HARD",
      conditions: [
        { field: "userIndicator1", operator: "EQ", value: "IMPOSSIBLE_TRAVEL" },
        { field: "merchantCountry", operator: "NEQ", value: "$cardCountry" },
      ],
      actions: [{ type: "SET_DECISION", value: "DECLINE" }],
      priority: 100,
      enabled: true,
    },
    payloads: [
      {
        description: "Impossible travel detectado - DEVE DISPARAR",
        shouldTrigger: true,
        data: { userIndicator1: "IMPOSSIBLE_TRAVEL", merchantCountry: "RU", cardCountry: "BR" },
      },
      {
        description: "Viagem normal - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { userIndicator1: "NORMAL_TRAVEL", merchantCountry: "AR", cardCountry: "BR" },
      },
    ],
    resultadoEsperado: "decision: DECLINE",
    operadoresUsados: ["EQ", "NEQ"],
    tags: ["geolocalização", "impossible-travel", "bloqueio"],
  },
  {
    id: "M14",
    name: "Transação Sem CVV + Sem 3DS",
    complexity: "media",
    category: "autenticação",
    narrativa: {
      situacao: "Compra online sem CVV e sem autenticação 3DS.",
      problema: "Nenhuma verificação de posse ou autenticação.",
      solucao: "Exigir pelo menos uma forma de verificação.",
    },
    passoAPasso: [
      "1. Nova Regra: 'Sem CVV Sem 3DS'",
      "2. Condição 1: cvvPresent IS_FALSE",
      "3. Condição 2: eci IN ['07', '00']",
      "4. Ação: SET_DECISION = REVIEW",
    ],
    json: {
      ruleName: "SEM_CVV_SEM_3DS",
      ruleType: "SECURITY",
      classification: "HARD",
      conditions: [
        { field: "cvvPresent", operator: "IS_FALSE", value: null },
        { field: "eci", operator: "IN", value: ["07", "00", null] },
      ],
      actions: [{ type: "SET_DECISION", value: "REVIEW" }],
      priority: 90,
      enabled: true,
    },
    payloads: [
      {
        description: "Sem CVV + ECI 07 - DEVE DISPARAR",
        shouldTrigger: true,
        data: { cvvPresent: false, eci: "07" },
      },
      {
        description: "Com CVV + ECI 05 - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { cvvPresent: true, eci: "05" },
      },
    ],
    resultadoEsperado: "decision: REVIEW",
    operadoresUsados: ["IS_FALSE", "IN"],
    tags: ["cvv", "3ds", "segurança"],
  },
  {
    id: "M15",
    name: "Merchant Novo + Alto Valor",
    complexity: "media",
    category: "merchant",
    narrativa: {
      situacao: "Primeira compra do cliente neste merchant com R$5.000.",
      problema: "Merchants desconhecidos para o cliente podem ser fraudulentos.",
      solucao: "Alertar primeira transação em merchant com valor alto.",
    },
    passoAPasso: [
      "1. Nova Regra: 'Merchant Novo Alto Valor'",
      "2. Condição 1: userIndicator2 EQ 'FIRST_MERCHANT'",
      "3. Condição 2: transactionAmount GT 2000",
      "4. Ação: ADD_SCORE = 35",
    ],
    json: {
      ruleName: "MERCHANT_NOVO_ALTO_VALOR",
      ruleType: "CONTEXT",
      classification: "SOFT",
      conditions: [
        { field: "userIndicator2", operator: "EQ", value: "FIRST_MERCHANT" },
        { field: "transactionAmount", operator: "GT", value: 2000 },
      ],
      actions: [{ type: "ADD_SCORE", value: 35 }],
      priority: 50,
      enabled: true,
    },
    payloads: [
      {
        description: "Primeiro merchant + R$5000 - DEVE DISPARAR",
        shouldTrigger: true,
        data: { userIndicator2: "FIRST_MERCHANT", transactionAmount: 5000 },
      },
      {
        description: "Merchant conhecido + R$5000 - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { userIndicator2: "KNOWN_MERCHANT", transactionAmount: 5000 },
      },
    ],
    resultadoEsperado: "score: +35",
    operadoresUsados: ["EQ", "GT"],
    tags: ["merchant", "primeiro", "valor"],
  },
];

// ---------------------------------------------------------------------------
// REGRAS COMPLEXAS (20 regras)
// ---------------------------------------------------------------------------
const REGRAS_COMPLEXAS: RuleExample[] = [
  {
    id: "C01",
    name: "Fraude Clássica de E-commerce",
    complexity: "complexa",
    category: "combinada",
    narrativa: {
      situacao:
        "Compra de R$8.000 em loja de eletrônicos, às 3h da manhã, de um país diferente, sem CVV, com score baixo.",
      problema:
        "Combinação de múltiplos indicadores de fraude em uma única transação.",
      solucao:
        "Regra com grupo de condições: (valor alto AND horário suspeito) OR (país diferente AND score baixo).",
    },
    passoAPasso: [
      "1. Criar regra complexa com grupos",
      "2. Grupo 1 (AND):",
      "   - transactionAmount GT 5000",
      "   - transactionTime BETWEEN '00:00' AND '06:00'",
      "3. Grupo 2 (AND):",
      "   - merchantCountry NEQ cardCountry",
      "   - consumerAuthenticationScore LT 50",
      "4. Lógica entre grupos: OR",
      "5. Ação: SET_DECISION = DECLINE",
    ],
    json: {
      ruleName: "FRAUDE_ECOMMERCE_CLASSICA",
      ruleType: "SECURITY",
      classification: "HARD",
      conditionGroups: [
        {
          logic: "AND",
          conditions: [
            { field: "transactionAmount", operator: "GT", value: 5000 },
            { field: "transactionTime", operator: "GTE", value: "00:00:00" },
            { field: "transactionTime", operator: "LTE", value: "06:00:00" },
          ],
        },
        {
          logic: "AND",
          conditions: [
            { field: "merchantCountry", operator: "NEQ", value: "$cardCountry" },
            { field: "consumerAuthenticationScore", operator: "LT", value: 50 },
          ],
        },
      ],
      actions: [{ type: "SET_DECISION", value: "DECLINE" }],
      priority: 100,
      enabled: true,
    },
    payloads: [
      {
        description: "R$8000 às 3h - DEVE DISPARAR (Grupo 1)",
        shouldTrigger: true,
        data: {
          transactionAmount: 8000,
          transactionTime: "03:00:00",
          merchantCountry: "BR",
          cardCountry: "BR",
          consumerAuthenticationScore: 80,
        },
      },
      {
        description: "País diferente + score baixo - DEVE DISPARAR (Grupo 2)",
        shouldTrigger: true,
        data: {
          transactionAmount: 1000,
          transactionTime: "14:00:00",
          merchantCountry: "RU",
          cardCountry: "BR",
          consumerAuthenticationScore: 30,
        },
      },
      {
        description: "Transação normal - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: {
          transactionAmount: 1000,
          transactionTime: "14:00:00",
          merchantCountry: "BR",
          cardCountry: "BR",
          consumerAuthenticationScore: 80,
        },
      },
    ],
    resultadoEsperado: "decision: DECLINE",
    operadoresUsados: ["GT", "GTE", "LTE", "NEQ", "LT"],
    tags: ["complexa", "grupos", "ecommerce"],
  },
  {
    id: "C02",
    name: "Teste de Cartão Sofisticado",
    complexity: "complexa",
    category: "velocidade",
    narrativa: {
      situacao:
        "Múltiplas tentativas de baixo valor em merchants diferentes em poucos minutos.",
      problema:
        "Fraudadores testam cartões com pequenas compras antes de fazer a grande.",
      solucao:
        "Detectar padrão de teste: (múltiplas tentativas OU CVV errado) E valor baixo.",
    },
    passoAPasso: [
      "1. Criar regra complexa",
      "2. Grupo 1 (OR):",
      "   - userIndicator1 EQ 'VELOCITY_HIGH'",
      "   - cvvResult EQ 'N'",
      "3. Grupo 2 (AND):",
      "   - transactionAmount LTE 50",
      "4. Lógica entre grupos: AND",
      "5. Ação: SET_DECISION = DECLINE",
    ],
    json: {
      ruleName: "TESTE_CARTAO_SOFISTICADO",
      ruleType: "VELOCITY",
      classification: "HARD",
      conditionGroups: [
        {
          logic: "OR",
          conditions: [
            { field: "userIndicator1", operator: "EQ", value: "VELOCITY_HIGH" },
            { field: "cvvResult", operator: "EQ", value: "N" },
          ],
        },
        {
          logic: "AND",
          conditions: [
            { field: "transactionAmount", operator: "LTE", value: 50 },
          ],
        },
      ],
      actions: [{ type: "SET_DECISION", value: "DECLINE" }],
      priority: 100,
      enabled: true,
    },
    payloads: [
      {
        description: "Alta velocidade + R$10 - DEVE DISPARAR",
        shouldTrigger: true,
        data: { userIndicator1: "VELOCITY_HIGH", transactionAmount: 10, cvvResult: "Y" },
      },
      {
        description: "CVV errado + R$5 - DEVE DISPARAR",
        shouldTrigger: true,
        data: { userIndicator1: "NORMAL", transactionAmount: 5, cvvResult: "N" },
      },
      {
        description: "Normal + R$500 - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { userIndicator1: "NORMAL", transactionAmount: 500, cvvResult: "Y" },
      },
    ],
    resultadoEsperado: "decision: DECLINE",
    operadoresUsados: ["EQ", "LTE"],
    tags: ["teste-cartão", "velocidade", "complexa"],
  },
  {
    id: "C03",
    name: "Account Takeover",
    complexity: "complexa",
    category: "comportamento",
    narrativa: {
      situacao:
        "Device novo, IP diferente do histórico, alteração recente de dados cadastrais, seguido de compra alta.",
      problema:
        "Invasor tomou controle da conta e está fazendo compras.",
      solucao:
        "Detectar combinação de sinais de account takeover.",
    },
    passoAPasso: [
      "1. Criar regra com múltiplos indicadores",
      "2. Grupo 1 (AND):",
      "   - userIndicator2 EQ 'NEW_DEVICE'",
      "   - userIndicator3 EQ 'NEW_IP'",
      "3. Grupo 2 (AND):",
      "   - userIndicator4 EQ 'RECENT_DATA_CHANGE'",
      "   - transactionAmount GT 2000",
      "4. Lógica: Grupo1 AND Grupo2",
      "5. Ação: SET_DECISION = DECLINE",
    ],
    json: {
      ruleName: "ACCOUNT_TAKEOVER",
      ruleType: "ANOMALY",
      classification: "HARD",
      conditionGroups: [
        {
          logic: "AND",
          conditions: [
            { field: "userIndicator2", operator: "EQ", value: "NEW_DEVICE" },
            { field: "userIndicator3", operator: "EQ", value: "NEW_IP" },
          ],
        },
        {
          logic: "AND",
          conditions: [
            { field: "userIndicator4", operator: "EQ", value: "RECENT_DATA_CHANGE" },
            { field: "transactionAmount", operator: "GT", value: 2000 },
          ],
        },
      ],
      actions: [{ type: "SET_DECISION", value: "DECLINE" }],
      priority: 100,
      enabled: true,
    },
    payloads: [
      {
        description: "Todos indicadores + R$5000 - DEVE DISPARAR",
        shouldTrigger: true,
        data: {
          userIndicator2: "NEW_DEVICE",
          userIndicator3: "NEW_IP",
          userIndicator4: "RECENT_DATA_CHANGE",
          transactionAmount: 5000,
        },
      },
      {
        description: "Device conhecido - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: {
          userIndicator2: "KNOWN_DEVICE",
          userIndicator3: "NEW_IP",
          userIndicator4: "RECENT_DATA_CHANGE",
          transactionAmount: 5000,
        },
      },
    ],
    resultadoEsperado: "decision: DECLINE",
    operadoresUsados: ["EQ", "GT"],
    tags: ["account-takeover", "comportamento", "complexa"],
  },
  {
    id: "C04",
    name: "Fraude de Primeira Compra",
    complexity: "complexa",
    category: "comportamento",
    narrativa: {
      situacao: "Primeira compra do cliente é de alto valor, em MCC de risco, sem autenticação.",
      problema: "Contas novas são alvos fáceis para fraude.",
      solucao: "Monitorar primeira transação com múltiplos fatores de risco.",
    },
    passoAPasso: [
      "1. Grupo 1: userIndicator1 EQ 'FIRST_PURCHASE'",
      "2. Grupo 2: transactionAmount GT 3000 AND mcc IN [7995, 6211]",
      "3. Grupo 3: eci IN ['07', '00']",
      "4. Lógica: Grupo1 AND (Grupo2 OR Grupo3)",
    ],
    json: {
      ruleName: "FRAUDE_PRIMEIRA_COMPRA",
      ruleType: "CONTEXT",
      classification: "HARD",
      conditionGroups: [
        {
          logic: "AND",
          conditions: [
            { field: "userIndicator1", operator: "EQ", value: "FIRST_PURCHASE" },
          ],
        },
        {
          logic: "AND",
          conditions: [
            { field: "transactionAmount", operator: "GT", value: 3000 },
            { field: "mcc", operator: "IN", value: [7995, 6211, 5967] },
          ],
        },
      ],
      actions: [{ type: "SET_DECISION", value: "REVIEW" }],
      priority: 90,
      enabled: true,
    },
    payloads: [
      {
        description: "Primeira compra + MCC risco + R$5000 - DEVE DISPARAR",
        shouldTrigger: true,
        data: { userIndicator1: "FIRST_PURCHASE", transactionAmount: 5000, mcc: 7995 },
      },
      {
        description: "Cliente recorrente - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { userIndicator1: "RETURNING", transactionAmount: 5000, mcc: 7995 },
      },
    ],
    resultadoEsperado: "decision: REVIEW",
    operadoresUsados: ["EQ", "GT", "IN"],
    tags: ["primeira-compra", "mcc", "complexa"],
  },
  {
    id: "C05",
    name: "Fraude de Terminal Comprometido",
    complexity: "complexa",
    category: "merchant",
    narrativa: {
      situacao: "Múltiplas transações no mesmo terminal com padrão suspeito.",
      problema: "Terminal pode ter sido comprometido por skimmer.",
      solucao: "Detectar padrões anômalos por terminal.",
    },
    passoAPasso: [
      "1. Condição: terminalId em lista de suspeitos",
      "2. Condição: posEntryMode indica fallback",
      "3. Condição: múltiplas transações recentes",
    ],
    json: {
      ruleName: "TERMINAL_COMPROMETIDO",
      ruleType: "SECURITY",
      classification: "HARD",
      conditionGroups: [
        {
          logic: "AND",
          conditions: [
            { field: "userIndicator1", operator: "EQ", value: "TERMINAL_FLAGGED" },
            { field: "posEntryMode", operator: "STARTS_WITH", value: "80" },
          ],
        },
        {
          logic: "AND",
          conditions: [
            { field: "userIndicator2", operator: "EQ", value: "HIGH_TERMINAL_VELOCITY" },
          ],
        },
      ],
      actions: [{ type: "SET_DECISION", value: "DECLINE" }, { type: "ADD_TAG", value: "TERMINAL_ALERT" }],
      priority: 100,
      enabled: true,
    },
    payloads: [
      {
        description: "Terminal flagged + fallback + alta velocidade - DEVE DISPARAR",
        shouldTrigger: true,
        data: {
          userIndicator1: "TERMINAL_FLAGGED",
          posEntryMode: "801",
          userIndicator2: "HIGH_TERMINAL_VELOCITY",
        },
      },
      {
        description: "Terminal normal - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: {
          userIndicator1: "TERMINAL_OK",
          posEntryMode: "051",
          userIndicator2: "NORMAL_VELOCITY",
        },
      },
    ],
    resultadoEsperado: "decision: DECLINE, tags: ['TERMINAL_ALERT']",
    operadoresUsados: ["EQ", "STARTS_WITH"],
    tags: ["terminal", "skimming", "complexa"],
  },
  {
    id: "C06",
    name: "Triangulação de Fraude",
    complexity: "complexa",
    category: "geolocalização",
    narrativa: {
      situacao: "Cartão usado em 3 países diferentes no mesmo dia.",
      problema: "Impossível estar em tantos lugares.",
      solucao: "Detectar múltiplos países em janela curta.",
    },
    passoAPasso: [
      "1. Condição: userIndicator1 EQ 'MULTI_COUNTRY_SAME_DAY'",
      "2. Condição: número de países > 2",
    ],
    json: {
      ruleName: "TRIANGULACAO_FRAUDE",
      ruleType: "ANOMALY",
      classification: "HARD",
      conditions: [
        { field: "userIndicator1", operator: "EQ", value: "MULTI_COUNTRY_3PLUS" },
      ],
      actions: [{ type: "SET_DECISION", value: "DECLINE" }],
      priority: 100,
      enabled: true,
    },
    payloads: [
      {
        description: "3+ países no dia - DEVE DISPARAR",
        shouldTrigger: true,
        data: { userIndicator1: "MULTI_COUNTRY_3PLUS" },
      },
      {
        description: "1 país - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { userIndicator1: "SINGLE_COUNTRY" },
      },
    ],
    resultadoEsperado: "decision: DECLINE",
    operadoresUsados: ["EQ"],
    tags: ["geolocalização", "triangulação", "complexa"],
  },
  {
    id: "C07",
    name: "Bust-out Fraud",
    complexity: "complexa",
    category: "comportamento",
    narrativa: {
      situacao: "Cliente aumenta limite, faz várias compras até o máximo, depois some.",
      problema: "Fraude de estouro de limite planejada.",
      solucao: "Detectar padrão de uso rápido após aumento de limite.",
    },
    passoAPasso: [
      "1. Condição: userIndicator1 EQ 'RECENT_LIMIT_INCREASE'",
      "2. Condição: userIndicator2 EQ 'USAGE_ABOVE_90_PERCENT'",
      "3. Condição: transactionAmount GT 5000",
    ],
    json: {
      ruleName: "BUST_OUT_FRAUD",
      ruleType: "ANOMALY",
      classification: "HARD",
      conditions: [
        { field: "userIndicator1", operator: "EQ", value: "RECENT_LIMIT_INCREASE" },
        { field: "userIndicator2", operator: "EQ", value: "USAGE_ABOVE_90_PERCENT" },
        { field: "transactionAmount", operator: "GT", value: 5000 },
      ],
      actions: [{ type: "SET_DECISION", value: "DECLINE" }],
      priority: 95,
      enabled: true,
    },
    payloads: [
      {
        description: "Aumento recente + uso 95% + R$8000 - DEVE DISPARAR",
        shouldTrigger: true,
        data: {
          userIndicator1: "RECENT_LIMIT_INCREASE",
          userIndicator2: "USAGE_ABOVE_90_PERCENT",
          transactionAmount: 8000,
        },
      },
      {
        description: "Sem aumento recente - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: {
          userIndicator1: "NO_RECENT_CHANGE",
          userIndicator2: "USAGE_ABOVE_90_PERCENT",
          transactionAmount: 8000,
        },
      },
    ],
    resultadoEsperado: "decision: DECLINE",
    operadoresUsados: ["EQ", "GT"],
    tags: ["bust-out", "limite", "complexa"],
  },
  {
    id: "C08",
    name: "Fraude de Afiliado",
    complexity: "complexa",
    category: "merchant",
    narrativa: {
      situacao: "Merchant com alta taxa de chargeback recebendo muitas transações.",
      problema: "Merchant pode estar conluiado com fraudadores.",
      solucao: "Monitorar merchants com histórico ruim.",
    },
    passoAPasso: [
      "1. Condição: userIndicator1 EQ 'HIGH_CHARGEBACK_MERCHANT'",
      "2. Condição: transactionAmount GT 1000",
    ],
    json: {
      ruleName: "MERCHANT_ALTO_CHARGEBACK",
      ruleType: "CONTEXT",
      classification: "HARD",
      conditions: [
        { field: "userIndicator1", operator: "EQ", value: "HIGH_CHARGEBACK_MERCHANT" },
        { field: "transactionAmount", operator: "GT", value: 1000 },
      ],
      actions: [{ type: "SET_DECISION", value: "REVIEW" }, { type: "ADD_TAG", value: "MERCHANT_RISK" }],
      priority: 80,
      enabled: true,
    },
    payloads: [
      {
        description: "Merchant alto chargeback + R$2000 - DEVE DISPARAR",
        shouldTrigger: true,
        data: { userIndicator1: "HIGH_CHARGEBACK_MERCHANT", transactionAmount: 2000 },
      },
      {
        description: "Merchant normal - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { userIndicator1: "NORMAL_MERCHANT", transactionAmount: 2000 },
      },
    ],
    resultadoEsperado: "decision: REVIEW, tags: ['MERCHANT_RISK']",
    operadoresUsados: ["EQ", "GT"],
    tags: ["merchant", "chargeback", "complexa"],
  },
  {
    id: "C09",
    name: "Synthetic Identity",
    complexity: "complexa",
    category: "comportamento",
    narrativa: {
      situacao: "Dados cadastrais combinam padrões de identidade sintética.",
      problema: "Fraudadores criam identidades falsas combinando dados reais.",
      solucao: "Detectar padrões de synthetic identity.",
    },
    passoAPasso: [
      "1. Condição: email com padrão suspeito",
      "2. Condição: telefone de área diferente do endereço",
      "3. Condição: cadastro recente",
    ],
    json: {
      ruleName: "SYNTHETIC_IDENTITY",
      ruleType: "ANOMALY",
      classification: "HARD",
      conditions: [
        { field: "userIndicator1", operator: "EQ", value: "SYNTHETIC_PATTERN" },
        { field: "userIndicator2", operator: "EQ", value: "PHONE_ADDRESS_MISMATCH" },
        { field: "userIndicator3", operator: "EQ", value: "NEW_ACCOUNT" },
      ],
      actions: [{ type: "SET_DECISION", value: "DECLINE" }],
      priority: 100,
      enabled: true,
    },
    payloads: [
      {
        description: "Padrão sintético + mismatch + conta nova - DEVE DISPARAR",
        shouldTrigger: true,
        data: {
          userIndicator1: "SYNTHETIC_PATTERN",
          userIndicator2: "PHONE_ADDRESS_MISMATCH",
          userIndicator3: "NEW_ACCOUNT",
        },
      },
      {
        description: "Conta antiga - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: {
          userIndicator1: "SYNTHETIC_PATTERN",
          userIndicator2: "PHONE_ADDRESS_MISMATCH",
          userIndicator3: "OLD_ACCOUNT",
        },
      },
    ],
    resultadoEsperado: "decision: DECLINE",
    operadoresUsados: ["EQ"],
    tags: ["synthetic-identity", "cadastro", "complexa"],
  },
  {
    id: "C10",
    name: "Refund Fraud",
    complexity: "complexa",
    category: "comportamento",
    narrativa: {
      situacao: "Cliente com histórico de muitos chargebacks pedindo compra alta.",
      problema: "Fraudadores de refund abusam do sistema de contestação.",
      solucao: "Monitorar clientes com histórico de chargebacks.",
    },
    passoAPasso: [
      "1. Condição: userIndicator1 EQ 'HIGH_CHARGEBACK_HISTORY'",
      "2. Condição: transactionAmount GT 500",
    ],
    json: {
      ruleName: "REFUND_FRAUD",
      ruleType: "ANOMALY",
      classification: "HARD",
      conditions: [
        { field: "userIndicator1", operator: "EQ", value: "HIGH_CHARGEBACK_HISTORY" },
        { field: "transactionAmount", operator: "GT", value: 500 },
      ],
      actions: [{ type: "SET_DECISION", value: "REVIEW" }],
      priority: 75,
      enabled: true,
    },
    payloads: [
      {
        description: "Alto histórico chargeback + R$1000 - DEVE DISPARAR",
        shouldTrigger: true,
        data: { userIndicator1: "HIGH_CHARGEBACK_HISTORY", transactionAmount: 1000 },
      },
      {
        description: "Histórico limpo - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { userIndicator1: "CLEAN_HISTORY", transactionAmount: 1000 },
      },
    ],
    resultadoEsperado: "decision: REVIEW",
    operadoresUsados: ["EQ", "GT"],
    tags: ["refund", "chargeback", "complexa"],
  },
  {
    id: "C11",
    name: "Fraude de Gift Card",
    complexity: "complexa",
    category: "merchant",
    narrativa: {
      situacao: "Múltiplas compras de gift cards de valores redondos em sequência.",
      problema: "Gift cards são usados para lavar dinheiro ou revender.",
      solucao: "Detectar padrão de compra de gift cards suspeito.",
    },
    passoAPasso: [
      "1. Condição: mcc EQ 5999 (Gift Cards)",
      "2. Condição: transactionAmount IN [100, 200, 500, 1000]",
      "3. Condição: userIndicator1 EQ 'MULTIPLE_GIFTCARDS_1H'",
    ],
    json: {
      ruleName: "FRAUDE_GIFT_CARD",
      ruleType: "ANOMALY",
      classification: "HARD",
      conditions: [
        { field: "mcc", operator: "EQ", value: 5999 },
        { field: "transactionAmount", operator: "IN", value: [100, 200, 500, 1000] },
        { field: "userIndicator1", operator: "EQ", value: "MULTIPLE_GIFTCARDS_1H" },
      ],
      actions: [{ type: "SET_DECISION", value: "DECLINE" }],
      priority: 90,
      enabled: true,
    },
    payloads: [
      {
        description: "Gift card + valor redondo + múltiplas - DEVE DISPARAR",
        shouldTrigger: true,
        data: { mcc: 5999, transactionAmount: 500, userIndicator1: "MULTIPLE_GIFTCARDS_1H" },
      },
      {
        description: "Compra única de gift card - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { mcc: 5999, transactionAmount: 50, userIndicator1: "SINGLE_PURCHASE" },
      },
    ],
    resultadoEsperado: "decision: DECLINE",
    operadoresUsados: ["EQ", "IN"],
    tags: ["gift-card", "lavagem", "complexa"],
  },
  {
    id: "C12",
    name: "Promo Abuse Detection",
    complexity: "complexa",
    category: "comportamento",
    narrativa: {
      situacao: "Múltiplas contas usando mesmo dispositivo para aproveitar promoções.",
      problema: "Abuso de cupons/promoções por criação de contas múltiplas.",
      solucao: "Detectar padrão de abuso promocional.",
    },
    passoAPasso: [
      "1. Condição: userIndicator1 EQ 'MULTI_ACCOUNT_SAME_DEVICE'",
      "2. Condição: userIndicator2 EQ 'PROMO_CODE_USED'",
      "3. Condição: userIndicator3 EQ 'NEW_ACCOUNT'",
    ],
    json: {
      ruleName: "PROMO_ABUSE",
      ruleType: "ANOMALY",
      classification: "HARD",
      conditions: [
        { field: "userIndicator1", operator: "EQ", value: "MULTI_ACCOUNT_SAME_DEVICE" },
        { field: "userIndicator2", operator: "EQ", value: "PROMO_CODE_USED" },
        { field: "userIndicator3", operator: "EQ", value: "NEW_ACCOUNT" },
      ],
      actions: [{ type: "SET_DECISION", value: "DECLINE" }, { type: "ADD_TAG", value: "PROMO_ABUSE" }],
      priority: 80,
      enabled: true,
    },
    payloads: [
      {
        description: "Multi-conta + promo + nova - DEVE DISPARAR",
        shouldTrigger: true,
        data: {
          userIndicator1: "MULTI_ACCOUNT_SAME_DEVICE",
          userIndicator2: "PROMO_CODE_USED",
          userIndicator3: "NEW_ACCOUNT",
        },
      },
      {
        description: "Conta única - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: {
          userIndicator1: "SINGLE_ACCOUNT",
          userIndicator2: "PROMO_CODE_USED",
          userIndicator3: "NEW_ACCOUNT",
        },
      },
    ],
    resultadoEsperado: "decision: DECLINE, tags: ['PROMO_ABUSE']",
    operadoresUsados: ["EQ"],
    tags: ["promo-abuse", "multi-conta", "complexa"],
  },
  {
    id: "C13",
    name: "Friendly Fraud Pattern",
    complexity: "complexa",
    category: "comportamento",
    narrativa: {
      situacao: "Cliente faz compra, recebe produto, depois contesta como fraude.",
      problema: "Fraude amigável onde o próprio titular alega não ter feito a compra.",
      solucao: "Identificar padrão de friendly fraud baseado em histórico.",
    },
    passoAPasso: [
      "1. Condição: userIndicator1 EQ 'PREVIOUS_FRIENDLY_FRAUD'",
      "2. Condição: transactionAmount GT 200",
      "3. Condição: deliveryConfirmed IS_TRUE",
    ],
    json: {
      ruleName: "FRIENDLY_FRAUD",
      ruleType: "ANOMALY",
      classification: "HARD",
      conditions: [
        { field: "userIndicator1", operator: "EQ", value: "PREVIOUS_FRIENDLY_FRAUD" },
        { field: "transactionAmount", operator: "GT", value: 200 },
      ],
      actions: [{ type: "SET_DECISION", value: "REVIEW" }, { type: "ADD_TAG", value: "FRIENDLY_FRAUD_RISK" }],
      priority: 70,
      enabled: true,
    },
    payloads: [
      {
        description: "Histórico de friendly fraud + R$500 - DEVE DISPARAR",
        shouldTrigger: true,
        data: { userIndicator1: "PREVIOUS_FRIENDLY_FRAUD", transactionAmount: 500 },
      },
      {
        description: "Sem histórico - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { userIndicator1: "CLEAN_HISTORY", transactionAmount: 500 },
      },
    ],
    resultadoEsperado: "decision: REVIEW, tags: ['FRIENDLY_FRAUD_RISK']",
    operadoresUsados: ["EQ", "GT"],
    tags: ["friendly-fraud", "chargeback", "complexa"],
  },
  {
    id: "C14",
    name: "Reshipping Fraud",
    complexity: "complexa",
    category: "geolocalização",
    narrativa: {
      situacao: "Compra com endereço de entrega em casa de 'laranja' para reenvio.",
      problema: "Fraudadores usam intermediários para receber produtos.",
      solucao: "Detectar endereços conhecidos de reshippers.",
    },
    passoAPasso: [
      "1. Condição: userIndicator1 EQ 'KNOWN_RESHIPPER_ADDRESS'",
      "2. Condição: merchantCountry NEQ shippingCountry",
      "3. Condição: transactionAmount GT 500",
    ],
    json: {
      ruleName: "RESHIPPING_FRAUD",
      ruleType: "ANOMALY",
      classification: "HARD",
      conditions: [
        { field: "userIndicator1", operator: "EQ", value: "KNOWN_RESHIPPER_ADDRESS" },
        { field: "transactionAmount", operator: "GT", value: 500 },
      ],
      actions: [{ type: "SET_DECISION", value: "DECLINE" }],
      priority: 95,
      enabled: true,
    },
    payloads: [
      {
        description: "Endereço de reshipper + R$1000 - DEVE DISPARAR",
        shouldTrigger: true,
        data: { userIndicator1: "KNOWN_RESHIPPER_ADDRESS", transactionAmount: 1000 },
      },
      {
        description: "Endereço normal - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { userIndicator1: "NORMAL_ADDRESS", transactionAmount: 1000 },
      },
    ],
    resultadoEsperado: "decision: DECLINE",
    operadoresUsados: ["EQ", "GT"],
    tags: ["reshipping", "laranja", "complexa"],
  },
  {
    id: "C15",
    name: "Return Fraud Pattern",
    complexity: "complexa",
    category: "comportamento",
    narrativa: {
      situacao: "Cliente com histórico de muitas devoluções fazendo nova compra alta.",
      problema: "Abuso do sistema de devoluções para obter produtos grátis.",
      solucao: "Monitorar taxa de devolução por cliente.",
    },
    passoAPasso: [
      "1. Condição: userIndicator1 EQ 'HIGH_RETURN_RATE'",
      "2. Condição: transactionAmount GT 300",
      "3. Condição: productCategory EQ 'ELECTRONICS'",
    ],
    json: {
      ruleName: "RETURN_FRAUD",
      ruleType: "ANOMALY",
      classification: "SOFT",
      conditions: [
        { field: "userIndicator1", operator: "EQ", value: "HIGH_RETURN_RATE" },
        { field: "transactionAmount", operator: "GT", value: 300 },
      ],
      actions: [{ type: "ADD_SCORE", value: 40 }],
      priority: 60,
      enabled: true,
    },
    payloads: [
      {
        description: "Alta taxa de devolução + R$500 - DEVE DISPARAR",
        shouldTrigger: true,
        data: { userIndicator1: "HIGH_RETURN_RATE", transactionAmount: 500 },
      },
      {
        description: "Taxa normal - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { userIndicator1: "NORMAL_RETURN_RATE", transactionAmount: 500 },
      },
    ],
    resultadoEsperado: "score: +40",
    operadoresUsados: ["EQ", "GT"],
    tags: ["return-fraud", "devolução", "complexa"],
  },
  {
    id: "C16",
    name: "SIM Swap Detection",
    complexity: "complexa",
    category: "autenticação",
    narrativa: {
      situacao: "Mudança recente de SIM card seguida de transação de alto valor.",
      problema: "Fraudadores fazem SIM swap para interceptar OTPs.",
      solucao: "Monitorar transações após mudanças de telefone.",
    },
    passoAPasso: [
      "1. Condição: userIndicator1 EQ 'RECENT_SIM_SWAP'",
      "2. Condição: transactionAmount GT 2000",
      "3. Condição: channel EQ 'MOBILE'",
    ],
    json: {
      ruleName: "SIM_SWAP_DETECTION",
      ruleType: "SECURITY",
      classification: "HARD",
      conditions: [
        { field: "userIndicator1", operator: "EQ", value: "RECENT_SIM_SWAP" },
        { field: "transactionAmount", operator: "GT", value: 2000 },
        { field: "channel", operator: "EQ", value: "MOBILE" },
      ],
      actions: [{ type: "SET_DECISION", value: "DECLINE" }],
      priority: 100,
      enabled: true,
    },
    payloads: [
      {
        description: "SIM swap recente + R$5000 + mobile - DEVE DISPARAR",
        shouldTrigger: true,
        data: { userIndicator1: "RECENT_SIM_SWAP", transactionAmount: 5000, channel: "MOBILE" },
      },
      {
        description: "Sem SIM swap - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { userIndicator1: "NO_SIM_CHANGE", transactionAmount: 5000, channel: "MOBILE" },
      },
    ],
    resultadoEsperado: "decision: DECLINE",
    operadoresUsados: ["EQ", "GT"],
    tags: ["sim-swap", "mobile", "complexa"],
  },
  {
    id: "C17",
    name: "Drop Shipping Fraud",
    complexity: "complexa",
    category: "merchant",
    narrativa: {
      situacao: "Merchant com padrão de drop shipping suspeito - muitos pedidos, endereços variados.",
      problema: "Fraudadores usam cartões roubados para fazer drop shipping.",
      solucao: "Detectar padrão de drop shipping fraudulento.",
    },
    passoAPasso: [
      "1. Condição: userIndicator1 EQ 'DROPSHIP_PATTERN'",
      "2. Condição: userIndicator2 EQ 'MANY_DIFFERENT_ADDRESSES'",
      "3. Condição: transactionAmount BETWEEN 50 AND 500",
    ],
    json: {
      ruleName: "DROPSHIP_FRAUD",
      ruleType: "ANOMALY",
      classification: "HARD",
      conditions: [
        { field: "userIndicator1", operator: "EQ", value: "DROPSHIP_PATTERN" },
        { field: "userIndicator2", operator: "EQ", value: "MANY_DIFFERENT_ADDRESSES" },
      ],
      actions: [{ type: "SET_DECISION", value: "REVIEW" }],
      priority: 75,
      enabled: true,
    },
    payloads: [
      {
        description: "Padrão dropship + muitos endereços - DEVE DISPARAR",
        shouldTrigger: true,
        data: { userIndicator1: "DROPSHIP_PATTERN", userIndicator2: "MANY_DIFFERENT_ADDRESSES" },
      },
      {
        description: "Endereço único - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: { userIndicator1: "NORMAL_PATTERN", userIndicator2: "SINGLE_ADDRESS" },
      },
    ],
    resultadoEsperado: "decision: REVIEW",
    operadoresUsados: ["EQ"],
    tags: ["dropship", "endereços", "complexa"],
  },
  {
    id: "C18",
    name: "Phishing Victim Pattern",
    complexity: "complexa",
    category: "comportamento",
    narrativa: {
      situacao: "Cliente idoso com comportamento atípico após possível golpe de phishing.",
      problema: "Vítimas de phishing fazem transações sob influência de fraudadores.",
      solucao: "Detectar padrão de vítima de phishing.",
    },
    passoAPasso: [
      "1. Condição: userIndicator1 EQ 'ELDERLY_CUSTOMER'",
      "2. Condição: userIndicator2 EQ 'UNUSUAL_MERCHANT_TYPE'",
      "3. Condição: transactionAmount GT averageTransaction * 5",
    ],
    json: {
      ruleName: "PHISHING_VICTIM",
      ruleType: "ANOMALY",
      classification: "HARD",
      conditions: [
        { field: "userIndicator1", operator: "EQ", value: "ELDERLY_CUSTOMER" },
        { field: "userIndicator2", operator: "EQ", value: "UNUSUAL_MERCHANT_TYPE" },
        { field: "userIndicator3", operator: "EQ", value: "AMOUNT_5X_AVERAGE" },
      ],
      actions: [{ type: "SET_DECISION", value: "REVIEW" }, { type: "ADD_TAG", value: "PHISHING_VICTIM_RISK" }],
      priority: 85,
      enabled: true,
    },
    payloads: [
      {
        description: "Idoso + merchant atípico + valor 5x - DEVE DISPARAR",
        shouldTrigger: true,
        data: {
          userIndicator1: "ELDERLY_CUSTOMER",
          userIndicator2: "UNUSUAL_MERCHANT_TYPE",
          userIndicator3: "AMOUNT_5X_AVERAGE",
        },
      },
      {
        description: "Cliente jovem - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: {
          userIndicator1: "YOUNG_CUSTOMER",
          userIndicator2: "UNUSUAL_MERCHANT_TYPE",
          userIndicator3: "AMOUNT_5X_AVERAGE",
        },
      },
    ],
    resultadoEsperado: "decision: REVIEW, tags: ['PHISHING_VICTIM_RISK']",
    operadoresUsados: ["EQ"],
    tags: ["phishing", "idoso", "complexa"],
  },
  {
    id: "C19",
    name: "Loyalty Points Fraud",
    complexity: "complexa",
    category: "comportamento",
    narrativa: {
      situacao: "Resgate de pontos de fidelidade em massa após acesso de novo dispositivo.",
      problema: "Fraudadores invadem contas para roubar pontos acumulados.",
      solucao: "Monitorar resgates de pontos após mudanças suspeitas.",
    },
    passoAPasso: [
      "1. Condição: userIndicator1 EQ 'LOYALTY_REDEMPTION'",
      "2. Condição: userIndicator2 EQ 'NEW_DEVICE'",
      "3. Condição: userIndicator3 EQ 'POINTS_ABOVE_10K'",
    ],
    json: {
      ruleName: "LOYALTY_FRAUD",
      ruleType: "ANOMALY",
      classification: "HARD",
      conditions: [
        { field: "userIndicator1", operator: "EQ", value: "LOYALTY_REDEMPTION" },
        { field: "userIndicator2", operator: "EQ", value: "NEW_DEVICE" },
        { field: "userIndicator3", operator: "EQ", value: "POINTS_ABOVE_10K" },
      ],
      actions: [{ type: "SET_DECISION", value: "REVIEW" }],
      priority: 80,
      enabled: true,
    },
    payloads: [
      {
        description: "Resgate + device novo + 10k+ pontos - DEVE DISPARAR",
        shouldTrigger: true,
        data: {
          userIndicator1: "LOYALTY_REDEMPTION",
          userIndicator2: "NEW_DEVICE",
          userIndicator3: "POINTS_ABOVE_10K",
        },
      },
      {
        description: "Device conhecido - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: {
          userIndicator1: "LOYALTY_REDEMPTION",
          userIndicator2: "KNOWN_DEVICE",
          userIndicator3: "POINTS_ABOVE_10K",
        },
      },
    ],
    resultadoEsperado: "decision: REVIEW",
    operadoresUsados: ["EQ"],
    tags: ["loyalty", "pontos", "complexa"],
  },
  {
    id: "C20",
    name: "Subscription Fraud",
    complexity: "complexa",
    category: "merchant",
    narrativa: {
      situacao: "Múltiplas assinaturas trial sendo criadas do mesmo cartão/device.",
      problema: "Abuso de trials gratuitos para obter serviços sem pagar.",
      solucao: "Detectar padrão de trial abuse.",
    },
    passoAPasso: [
      "1. Condição: recurringIndicator IS_TRUE",
      "2. Condição: userIndicator1 EQ 'MULTIPLE_TRIALS_SAME_CARD'",
      "3. Condição: transactionAmount LT 10",
    ],
    json: {
      ruleName: "SUBSCRIPTION_FRAUD",
      ruleType: "ANOMALY",
      classification: "HARD",
      conditions: [
        { field: "recurringIndicator", operator: "IS_TRUE", value: null },
        { field: "userIndicator1", operator: "EQ", value: "MULTIPLE_TRIALS_SAME_CARD" },
        { field: "transactionAmount", operator: "LT", value: 10 },
      ],
      actions: [{ type: "SET_DECISION", value: "DECLINE" }],
      priority: 70,
      enabled: true,
    },
    payloads: [
      {
        description: "Recorrente + múltiplos trials + valor baixo - DEVE DISPARAR",
        shouldTrigger: true,
        data: {
          recurringIndicator: true,
          userIndicator1: "MULTIPLE_TRIALS_SAME_CARD",
          transactionAmount: 1,
        },
      },
      {
        description: "Trial único - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: {
          recurringIndicator: true,
          userIndicator1: "SINGLE_TRIAL",
          transactionAmount: 1,
        },
      },
    ],
    resultadoEsperado: "decision: DECLINE",
    operadoresUsados: ["IS_TRUE", "EQ", "LT"],
    tags: ["subscription", "trial-abuse", "complexa"],
  },
];

// ---------------------------------------------------------------------------
// REGRAS EXTREMAMENTE COMPLEXAS (10 regras)
// ---------------------------------------------------------------------------
const REGRAS_EXTREMAS: RuleExample[] = [
  {
    id: "E01",
    name: "Ataque Coordenado Multi-Cartão",
    complexity: "extrema",
    category: "velocidade",
    narrativa: {
      situacao:
        "Múltiplos cartões diferentes sendo usados do mesmo dispositivo/IP em poucos minutos, todos com valores similares, em merchants de risco.",
      problema:
        "Grupo organizado de fraudadores usando batch de cartões roubados.",
      solucao:
        "Regra extremamente complexa que combina: velocidade de device, padrão de valor, MCC de risco, e múltiplos cartões.",
    },
    passoAPasso: [
      "1. Criar regra com 4 grupos",
      "2. Grupo 1 (device): userIndicator1 EQ 'MULTI_CARD_SAME_DEVICE'",
      "3. Grupo 2 (velocidade): userIndicator2 EQ 'HIGH_VELOCITY_10MIN'",
      "4. Grupo 3 (MCC): mcc IN [7995, 6211, 5967, 5816]",
      "5. Grupo 4 (valor): transactionAmount BETWEEN 500 AND 2000",
      "6. Lógica: (Grupo1 AND Grupo2) AND (Grupo3 OR Grupo4)",
      "7. Ação: SET_DECISION = DECLINE + ADD_TAG = 'COORDINATED_ATTACK'",
    ],
    json: {
      ruleName: "ATAQUE_COORDENADO_MULTI_CARTAO",
      ruleType: "VELOCITY",
      classification: "HARD",
      conditionGroups: [
        {
          logic: "AND",
          conditions: [
            { field: "userIndicator1", operator: "EQ", value: "MULTI_CARD_SAME_DEVICE" },
            { field: "userIndicator2", operator: "EQ", value: "HIGH_VELOCITY_10MIN" },
          ],
        },
        {
          logic: "OR",
          conditions: [
            { field: "mcc", operator: "IN", value: [7995, 6211, 5967, 5816] },
            { field: "transactionAmount", operator: "GTE", value: 500 },
            { field: "transactionAmount", operator: "LTE", value: 2000 },
          ],
        },
      ],
      actions: [
        { type: "SET_DECISION", value: "DECLINE" },
        { type: "ADD_TAG", value: "COORDINATED_ATTACK" },
      ],
      priority: 100,
      enabled: true,
    },
    payloads: [
      {
        description: "Ataque coordenado detectado - DEVE DISPARAR",
        shouldTrigger: true,
        data: {
          userIndicator1: "MULTI_CARD_SAME_DEVICE",
          userIndicator2: "HIGH_VELOCITY_10MIN",
          mcc: 7995,
          transactionAmount: 1500,
          deviceId: "DEVICE-123",
        },
      },
      {
        description: "Cartão único, velocidade normal - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: {
          userIndicator1: "SINGLE_CARD",
          userIndicator2: "NORMAL_VELOCITY",
          mcc: 5411,
          transactionAmount: 1500,
          deviceId: "DEVICE-456",
        },
      },
    ],
    resultadoEsperado: "decision: DECLINE, tags: ['COORDINATED_ATTACK']",
    operadoresUsados: ["EQ", "IN", "GTE", "LTE"],
    tags: ["extrema", "coordenado", "multi-cartão", "velocidade"],
  },
  {
    id: "E02",
    name: "Fraude de Afiliado Sofisticada",
    complexity: "extrema",
    category: "merchant",
    narrativa: {
      situacao:
        "Rede de merchants falsos processando transações de cartões roubados com padrão específico de valores e horários.",
      problema:
        "Esquema organizado de lavagem de dinheiro via merchants conluiados.",
      solucao:
        "Detectar padrão de merchant + horário + valor + origem geográfica suspeita.",
    },
    passoAPasso: [
      "1. Grupo 1: Merchant em lista de suspeitos OU MCC de risco",
      "2. Grupo 2: Horário fora do comercial OU final de semana",
      "3. Grupo 3: Valor em faixa suspeita (R$999 a R$1001 - evitando limites)",
      "4. Grupo 4: País de origem diferente do merchant",
      "5. Lógica: (G1 AND G2) OR (G3 AND G4)",
    ],
    json: {
      ruleName: "FRAUDE_AFILIADO_SOFISTICADA",
      ruleType: "ANOMALY",
      classification: "HARD",
      conditionGroups: [
        {
          logic: "AND",
          conditions: [
            { field: "userIndicator1", operator: "EQ", value: "SUSPECT_MERCHANT_NETWORK" },
            { field: "transactionTime", operator: "NOT_BETWEEN", value: ["08:00:00", "18:00:00"] },
          ],
        },
        {
          logic: "AND",
          conditions: [
            { field: "transactionAmount", operator: "GTE", value: 999 },
            { field: "transactionAmount", operator: "LTE", value: 1001 },
            { field: "merchantCountry", operator: "NEQ", value: "$cardCountry" },
          ],
        },
      ],
      actions: [
        { type: "SET_DECISION", value: "DECLINE" },
        { type: "ADD_TAG", value: "AFFILIATE_FRAUD" },
      ],
      priority: 100,
      enabled: true,
    },
    payloads: [
      {
        description: "Merchant suspeito + fora horário - DEVE DISPARAR",
        shouldTrigger: true,
        data: {
          userIndicator1: "SUSPECT_MERCHANT_NETWORK",
          transactionTime: "23:30:00",
          transactionAmount: 1500,
          merchantCountry: "BR",
          cardCountry: "BR",
        },
      },
      {
        description: "Valor R$1000 + país diferente - DEVE DISPARAR",
        shouldTrigger: true,
        data: {
          userIndicator1: "NORMAL_MERCHANT",
          transactionTime: "14:00:00",
          transactionAmount: 1000,
          merchantCountry: "US",
          cardCountry: "BR",
        },
      },
      {
        description: "Tudo normal - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: {
          userIndicator1: "NORMAL_MERCHANT",
          transactionTime: "14:00:00",
          transactionAmount: 500,
          merchantCountry: "BR",
          cardCountry: "BR",
        },
      },
    ],
    resultadoEsperado: "decision: DECLINE, tags: ['AFFILIATE_FRAUD']",
    operadoresUsados: ["EQ", "NOT_BETWEEN", "GTE", "LTE", "NEQ"],
    tags: ["extrema", "afiliado", "merchant", "lavagem"],
  },
  {
    id: "E03",
    name: "BIN Attack Distribuído",
    complexity: "extrema",
    category: "velocidade",
    narrativa: {
      situacao:
        "Ataque automatizado testando sequências de números de cartão com o mesmo BIN, distribuído por múltiplos IPs/devices.",
      problema:
        "Bot farm testando cartões gerados por algoritmo.",
      solucao:
        "Detectar padrão de BIN attack: mesmo BIN + alta velocidade + CVV incorreto + valores baixos.",
    },
    passoAPasso: [
      "1. Grupo 1: Alto volume no mesmo BIN em 5 minutos",
      "2. Grupo 2: Taxa de CVV incorreto > 50%",
      "3. Grupo 3: Valores de teste (< R$10)",
      "4. Grupo 4: Múltiplos IPs/devices",
      "5. Lógica: G1 AND (G2 OR G3) AND G4",
    ],
    json: {
      ruleName: "BIN_ATTACK_DISTRIBUIDO",
      ruleType: "VELOCITY",
      classification: "HARD",
      conditionGroups: [
        {
          logic: "AND",
          conditions: [
            { field: "userIndicator1", operator: "EQ", value: "BIN_HIGH_VELOCITY" },
            { field: "userIndicator2", operator: "EQ", value: "DISTRIBUTED_SOURCES" },
          ],
        },
        {
          logic: "OR",
          conditions: [
            { field: "userIndicator3", operator: "EQ", value: "HIGH_CVV_FAILURE_RATE" },
            { field: "transactionAmount", operator: "LTE", value: 10 },
          ],
        },
      ],
      actions: [
        { type: "SET_DECISION", value: "DECLINE" },
        { type: "ADD_TAG", value: "BIN_ATTACK" },
      ],
      priority: 100,
      enabled: true,
    },
    payloads: [
      {
        description: "BIN attack detectado - DEVE DISPARAR",
        shouldTrigger: true,
        data: {
          userIndicator1: "BIN_HIGH_VELOCITY",
          userIndicator2: "DISTRIBUTED_SOURCES",
          userIndicator3: "HIGH_CVV_FAILURE_RATE",
          transactionAmount: 1,
        },
      },
      {
        description: "Transação normal - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: {
          userIndicator1: "NORMAL_BIN_VELOCITY",
          userIndicator2: "SINGLE_SOURCE",
          userIndicator3: "NORMAL_CVV_RATE",
          transactionAmount: 500,
        },
      },
    ],
    resultadoEsperado: "decision: DECLINE, tags: ['BIN_ATTACK']",
    operadoresUsados: ["EQ", "LTE"],
    tags: ["extrema", "bin-attack", "bot", "automatizado"],
  },
  {
    id: "E04",
    name: "Money Mule Detection",
    complexity: "extrema",
    category: "comportamento",
    narrativa: {
      situacao:
        "Conta sendo usada para receber e rapidamente transferir fundos, padrão típico de 'laranja'.",
      problema:
        "Money mules são usados para lavar dinheiro de fraudes.",
      solucao:
        "Detectar padrão de receber → transferir rapidamente com novos beneficiários.",
    },
    passoAPasso: [
      "1. Grupo 1: Conta com depósitos recentes de múltiplas fontes",
      "2. Grupo 2: Transferências para novos beneficiários",
      "3. Grupo 3: Padrão de valor específico (99%, 95% do recebido)",
      "4. Grupo 4: Conta relativamente nova",
      "5. Lógica: G1 AND G2 AND (G3 OR G4)",
    ],
    json: {
      ruleName: "MONEY_MULE_DETECTION",
      ruleType: "ANOMALY",
      classification: "HARD",
      conditionGroups: [
        {
          logic: "AND",
          conditions: [
            { field: "userIndicator1", operator: "EQ", value: "MULTI_SOURCE_DEPOSITS" },
            { field: "userIndicator2", operator: "EQ", value: "NEW_BENEFICIARIES" },
          ],
        },
        {
          logic: "OR",
          conditions: [
            { field: "userIndicator3", operator: "EQ", value: "PASS_THROUGH_PATTERN" },
            { field: "userIndicator4", operator: "EQ", value: "ACCOUNT_AGE_LESS_30D" },
          ],
        },
      ],
      actions: [
        { type: "SET_DECISION", value: "DECLINE" },
        { type: "ADD_TAG", value: "MONEY_MULE_SUSPECT" },
      ],
      priority: 100,
      enabled: true,
    },
    payloads: [
      {
        description: "Padrão de money mule - DEVE DISPARAR",
        shouldTrigger: true,
        data: {
          userIndicator1: "MULTI_SOURCE_DEPOSITS",
          userIndicator2: "NEW_BENEFICIARIES",
          userIndicator3: "PASS_THROUGH_PATTERN",
          userIndicator4: "ACCOUNT_AGE_MORE_90D",
        },
      },
      {
        description: "Conta normal - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: {
          userIndicator1: "NORMAL_DEPOSITS",
          userIndicator2: "KNOWN_BENEFICIARIES",
          userIndicator3: "NORMAL_PATTERN",
          userIndicator4: "ACCOUNT_AGE_MORE_90D",
        },
      },
    ],
    resultadoEsperado: "decision: DECLINE, tags: ['MONEY_MULE_SUSPECT']",
    operadoresUsados: ["EQ"],
    tags: ["extrema", "money-mule", "lavagem", "laranja"],
  },
  {
    id: "E05",
    name: "Credential Stuffing Attack",
    complexity: "extrema",
    category: "velocidade",
    narrativa: {
      situacao:
        "Tentativas de login em massa usando credenciais vazadas, seguidas de compras.",
      problema:
        "Atacantes usam listas de credenciais de data breaches.",
      solucao:
        "Detectar padrão de credential stuffing: muitos logins falhos → sucesso → compra rápida.",
    },
    passoAPasso: [
      "1. Grupo 1: Múltiplas falhas de autenticação recentes",
      "2. Grupo 2: Login bem-sucedido após falhas",
      "3. Grupo 3: Compra imediatamente após login",
      "4. Grupo 4: Device/IP novo",
      "5. Lógica: G1 AND G2 AND G3 AND G4",
    ],
    json: {
      ruleName: "CREDENTIAL_STUFFING",
      ruleType: "VELOCITY",
      classification: "HARD",
      conditionGroups: [
        {
          logic: "AND",
          conditions: [
            { field: "userIndicator1", operator: "EQ", value: "RECENT_AUTH_FAILURES" },
            { field: "userIndicator2", operator: "EQ", value: "SUCCESS_AFTER_FAILURES" },
          ],
        },
        {
          logic: "AND",
          conditions: [
            { field: "userIndicator3", operator: "EQ", value: "PURCHASE_AFTER_LOGIN_5MIN" },
            { field: "userIndicator4", operator: "EQ", value: "NEW_DEVICE_IP" },
          ],
        },
      ],
      actions: [
        { type: "SET_DECISION", value: "DECLINE" },
        { type: "ADD_TAG", value: "CREDENTIAL_STUFFING" },
      ],
      priority: 100,
      enabled: true,
    },
    payloads: [
      {
        description: "Credential stuffing detectado - DEVE DISPARAR",
        shouldTrigger: true,
        data: {
          userIndicator1: "RECENT_AUTH_FAILURES",
          userIndicator2: "SUCCESS_AFTER_FAILURES",
          userIndicator3: "PURCHASE_AFTER_LOGIN_5MIN",
          userIndicator4: "NEW_DEVICE_IP",
        },
      },
      {
        description: "Login normal - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: {
          userIndicator1: "NO_AUTH_FAILURES",
          userIndicator2: "NORMAL_LOGIN",
          userIndicator3: "PURCHASE_AFTER_LOGIN_5MIN",
          userIndicator4: "KNOWN_DEVICE",
        },
      },
    ],
    resultadoEsperado: "decision: DECLINE, tags: ['CREDENTIAL_STUFFING']",
    operadoresUsados: ["EQ"],
    tags: ["extrema", "credential-stuffing", "account-takeover"],
  },
  {
    id: "E06",
    name: "Fraud Ring Detection",
    complexity: "extrema",
    category: "combinada",
    narrativa: {
      situacao:
        "Múltiplas contas com características compartilhadas (email similar, mesmo device, IPs próximos) fazendo compras coordenadas.",
      problema:
        "Grupos organizados de fraude criam múltiplas identidades para escalar operações.",
      solucao:
        "Detectar conexões entre contas usando grafos de relacionamento (email, device, IP, endereço).",
    },
    passoAPasso: [
      "1. Grupo 1: Contas conectadas por device fingerprint",
      "2. Grupo 2: Padrão de email similar (prefixo+número@domain)",
      "3. Grupo 3: Mesmo range de IP",
      "4. Grupo 4: Endereços muito próximos geograficamente",
      "5. Grupo 5: Compras em merchants similares",
      "6. Lógica: (G1 AND G2) OR (G3 AND G4 AND G5)",
    ],
    json: {
      ruleName: "FRAUD_RING_DETECTION",
      ruleType: "ANOMALY",
      classification: "HARD",
      conditionGroups: [
        {
          logic: "AND",
          conditions: [
            { field: "userIndicator1", operator: "EQ", value: "CONNECTED_ACCOUNTS_DEVICE" },
            { field: "userIndicator2", operator: "EQ", value: "SIMILAR_EMAIL_PATTERN" },
          ],
        },
        {
          logic: "AND",
          conditions: [
            { field: "userIndicator3", operator: "EQ", value: "SAME_IP_RANGE" },
            { field: "userIndicator4", operator: "EQ", value: "NEARBY_ADDRESSES" },
            { field: "userIndicator5", operator: "EQ", value: "SIMILAR_MERCHANTS" },
          ],
        },
      ],
      actions: [
        { type: "SET_DECISION", value: "DECLINE" },
        { type: "ADD_TAG", value: "FRAUD_RING_SUSPECT" },
      ],
      priority: 100,
      enabled: true,
    },
    payloads: [
      {
        description: "Fraud ring detectado - DEVE DISPARAR",
        shouldTrigger: true,
        data: {
          userIndicator1: "CONNECTED_ACCOUNTS_DEVICE",
          userIndicator2: "SIMILAR_EMAIL_PATTERN",
          userIndicator3: "SAME_IP_RANGE",
          userIndicator4: "NEARBY_ADDRESSES",
          userIndicator5: "SIMILAR_MERCHANTS",
        },
      },
      {
        description: "Conta isolada - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: {
          userIndicator1: "UNIQUE_DEVICE",
          userIndicator2: "UNIQUE_EMAIL",
          userIndicator3: "UNIQUE_IP",
          userIndicator4: "UNIQUE_ADDRESS",
          userIndicator5: "VARIED_MERCHANTS",
        },
      },
    ],
    resultadoEsperado: "decision: DECLINE, tags: ['FRAUD_RING_SUSPECT']",
    operadoresUsados: ["EQ"],
    tags: ["extrema", "fraud-ring", "organizado", "grafo"],
  },
  {
    id: "E07",
    name: "Social Engineering Defense",
    complexity: "extrema",
    category: "comportamento",
    narrativa: {
      situacao:
        "Cliente idoso fazendo transferência para conta desconhecida após ligação de número suspeito, valor muito acima do normal.",
      problema:
        "Golpes de engenharia social (golpe do falso sequestro, falso funcionário, etc).",
      solucao:
        "Detectar combinação de fatores que indicam vítima de engenharia social.",
    },
    passoAPasso: [
      "1. Grupo 1: Cliente vulnerável (idoso OU primeira vez nesse tipo de transação)",
      "2. Grupo 2: Valor muito acima do histórico (>5x média)",
      "3. Grupo 3: Beneficiário desconhecido OU conta recém-criada",
      "4. Grupo 4: Horário atípico OU transação urgente",
      "5. Lógica: G1 AND G2 AND (G3 OR G4)",
    ],
    json: {
      ruleName: "SOCIAL_ENGINEERING_DEFENSE",
      ruleType: "ANOMALY",
      classification: "HARD",
      conditionGroups: [
        {
          logic: "AND",
          conditions: [
            { field: "userIndicator1", operator: "IN", value: ["ELDERLY", "FIRST_TIME_TRANSFER"] },
            { field: "userIndicator2", operator: "EQ", value: "AMOUNT_5X_AVERAGE" },
          ],
        },
        {
          logic: "OR",
          conditions: [
            { field: "userIndicator3", operator: "EQ", value: "UNKNOWN_BENEFICIARY" },
            { field: "userIndicator4", operator: "EQ", value: "UNUSUAL_TIME" },
          ],
        },
      ],
      actions: [
        { type: "SET_DECISION", value: "REVIEW" },
        { type: "ADD_TAG", value: "SOCIAL_ENGINEERING_RISK" },
      ],
      priority: 95,
      enabled: true,
    },
    payloads: [
      {
        description: "Padrão de engenharia social - DEVE DISPARAR",
        shouldTrigger: true,
        data: {
          userIndicator1: "ELDERLY",
          userIndicator2: "AMOUNT_5X_AVERAGE",
          userIndicator3: "UNKNOWN_BENEFICIARY",
          userIndicator4: "NORMAL_TIME",
        },
      },
      {
        description: "Transferência normal - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: {
          userIndicator1: "YOUNG",
          userIndicator2: "NORMAL_AMOUNT",
          userIndicator3: "KNOWN_BENEFICIARY",
          userIndicator4: "NORMAL_TIME",
        },
      },
    ],
    resultadoEsperado: "decision: REVIEW, tags: ['SOCIAL_ENGINEERING_RISK']",
    operadoresUsados: ["IN", "EQ"],
    tags: ["extrema", "engenharia-social", "idoso", "golpe"],
  },
  {
    id: "E08",
    name: "Card Not Present Massive Attack",
    complexity: "extrema",
    category: "velocidade",
    narrativa: {
      situacao:
        "Ataque massivo de transações CNP (Card Not Present) com diferentes cartões mas padrões similares.",
      problema:
        "Ataques automatizados usando listas de cartões comprados na dark web.",
      solucao:
        "Detectar padrões de ataque CNP em larga escala.",
    },
    passoAPasso: [
      "1. Grupo 1: Alto volume de transações CNP em curto período",
      "2. Grupo 2: Múltiplos cartões do mesmo BIN",
      "3. Grupo 3: Valores similares OU padrão de incremento",
      "4. Grupo 4: Mesmo merchant OU IP",
      "5. Grupo 5: Alta taxa de falha de CVV/AVS",
      "6. Lógica: G1 AND (G2 OR G3) AND (G4 OR G5)",
    ],
    json: {
      ruleName: "CNP_MASSIVE_ATTACK",
      ruleType: "VELOCITY",
      classification: "HARD",
      conditionGroups: [
        {
          logic: "AND",
          conditions: [
            { field: "userIndicator1", operator: "EQ", value: "HIGH_CNP_VOLUME" },
            { field: "terminalType", operator: "EQ", value: "ECOMMERCE" },
          ],
        },
        {
          logic: "OR",
          conditions: [
            { field: "userIndicator2", operator: "EQ", value: "SAME_BIN_MULTIPLE_CARDS" },
            { field: "userIndicator3", operator: "EQ", value: "SIMILAR_AMOUNTS" },
          ],
        },
        {
          logic: "OR",
          conditions: [
            { field: "userIndicator4", operator: "EQ", value: "SAME_MERCHANT_IP" },
            { field: "userIndicator5", operator: "EQ", value: "HIGH_CVV_AVS_FAILURE" },
          ],
        },
      ],
      actions: [
        { type: "SET_DECISION", value: "DECLINE" },
        { type: "ADD_TAG", value: "CNP_ATTACK" },
      ],
      priority: 100,
      enabled: true,
    },
    payloads: [
      {
        description: "Ataque CNP massivo - DEVE DISPARAR",
        shouldTrigger: true,
        data: {
          userIndicator1: "HIGH_CNP_VOLUME",
          terminalType: "ECOMMERCE",
          userIndicator2: "SAME_BIN_MULTIPLE_CARDS",
          userIndicator4: "SAME_MERCHANT_IP",
        },
      },
      {
        description: "Transação CNP normal - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: {
          userIndicator1: "NORMAL_CNP_VOLUME",
          terminalType: "ECOMMERCE",
          userIndicator2: "UNIQUE_BIN",
          userIndicator4: "VARIED_MERCHANTS",
        },
      },
    ],
    resultadoEsperado: "decision: DECLINE, tags: ['CNP_ATTACK']",
    operadoresUsados: ["EQ"],
    tags: ["extrema", "cnp", "massivo", "automatizado"],
  },
  {
    id: "E09",
    name: "Cross-Border Fraud Network",
    complexity: "extrema",
    category: "geolocalização",
    narrativa: {
      situacao:
        "Padrão de transações cruzando múltiplas fronteiras em curto período com características de lavagem de dinheiro.",
      problema:
        "Redes de fraude internacionais usam múltiplos países para dificultar rastreamento.",
      solucao:
        "Detectar padrões de cross-border fraud usando análise geográfica avançada.",
    },
    passoAPasso: [
      "1. Grupo 1: Transações em 3+ países em 24h",
      "2. Grupo 2: Valores que evitam limites de reporting (< $10k)",
      "3. Grupo 3: Países de alto risco envolvidos",
      "4. Grupo 4: Conta relativamente nova OU comportamento atípico",
      "5. Grupo 5: Beneficiários em jurisdições offshore",
      "6. Lógica: G1 AND (G2 OR G3) AND (G4 OR G5)",
    ],
    json: {
      ruleName: "CROSS_BORDER_FRAUD",
      ruleType: "ANOMALY",
      classification: "HARD",
      conditionGroups: [
        {
          logic: "AND",
          conditions: [
            { field: "userIndicator1", operator: "EQ", value: "MULTI_COUNTRY_24H" },
            { field: "userIndicator2", operator: "EQ", value: "AMOUNT_BELOW_THRESHOLD" },
          ],
        },
        {
          logic: "OR",
          conditions: [
            { field: "merchantCountry", operator: "IN", value: ["NG", "RU", "UA", "VN", "PH"] },
            { field: "userIndicator3", operator: "EQ", value: "NEW_ACCOUNT" },
          ],
        },
        {
          logic: "AND",
          conditions: [
            { field: "userIndicator4", operator: "EQ", value: "OFFSHORE_BENEFICIARY" },
          ],
        },
      ],
      actions: [
        { type: "SET_DECISION", value: "DECLINE" },
        { type: "ADD_TAG", value: "CROSS_BORDER_FRAUD" },
      ],
      priority: 100,
      enabled: true,
    },
    payloads: [
      {
        description: "Cross-border fraud - DEVE DISPARAR",
        shouldTrigger: true,
        data: {
          userIndicator1: "MULTI_COUNTRY_24H",
          userIndicator2: "AMOUNT_BELOW_THRESHOLD",
          merchantCountry: "NG",
          userIndicator4: "OFFSHORE_BENEFICIARY",
        },
      },
      {
        description: "Viagem normal - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: {
          userIndicator1: "SINGLE_COUNTRY",
          userIndicator2: "NORMAL_AMOUNT",
          merchantCountry: "US",
          userIndicator4: "DOMESTIC_BENEFICIARY",
        },
      },
    ],
    resultadoEsperado: "decision: DECLINE, tags: ['CROSS_BORDER_FRAUD']",
    operadoresUsados: ["EQ", "IN"],
    tags: ["extrema", "cross-border", "internacional", "lavagem"],
  },
  {
    id: "E10",
    name: "Synthetic Identity Fraud Network",
    complexity: "extrema",
    category: "comportamento",
    narrativa: {
      situacao:
        "Rede de identidades sintéticas operando de forma coordenada para maximizar crédito antes de bust-out.",
      problema:
        "Grupos criam dezenas de identidades falsas, constroem histórico de crédito, depois fazem bust-out coordenado.",
      solucao:
        "Detectar rede de synthetic IDs através de conexões ocultas e padrão de bust-out.",
    },
    passoAPasso: [
      "1. Grupo 1: Indicadores de synthetic identity (SSN inconsistente, endereço compartilhado)",
      "2. Grupo 2: Contas com idade similar (criadas próximas)",
      "3. Grupo 3: Padrão de build-up de crédito (uso regular, pagamentos em dia)",
      "4. Grupo 4: Súbito aumento de utilização (>90% do limite)",
      "5. Grupo 5: Transações em merchants de alto valor de revenda",
      "6. Lógica: (G1 AND G2) AND (G3 OR G4) AND G5",
    ],
    json: {
      ruleName: "SYNTHETIC_ID_NETWORK",
      ruleType: "ANOMALY",
      classification: "HARD",
      conditionGroups: [
        {
          logic: "AND",
          conditions: [
            { field: "userIndicator1", operator: "EQ", value: "SYNTHETIC_INDICATORS" },
            { field: "userIndicator2", operator: "EQ", value: "SIMILAR_ACCOUNT_AGE_NETWORK" },
          ],
        },
        {
          logic: "OR",
          conditions: [
            { field: "userIndicator3", operator: "EQ", value: "CREDIT_BUILDUP_PATTERN" },
            { field: "userIndicator4", operator: "EQ", value: "UTILIZATION_ABOVE_90" },
          ],
        },
        {
          logic: "AND",
          conditions: [
            { field: "mcc", operator: "IN", value: [5732, 5045, 5946, 5942] },
          ],
        },
      ],
      actions: [
        { type: "SET_DECISION", value: "DECLINE" },
        { type: "ADD_TAG", value: "SYNTHETIC_ID_NETWORK" },
      ],
      priority: 100,
      enabled: true,
    },
    payloads: [
      {
        description: "Rede de synthetic IDs - DEVE DISPARAR",
        shouldTrigger: true,
        data: {
          userIndicator1: "SYNTHETIC_INDICATORS",
          userIndicator2: "SIMILAR_ACCOUNT_AGE_NETWORK",
          userIndicator3: "CREDIT_BUILDUP_PATTERN",
          userIndicator4: "UTILIZATION_BELOW_90",
          mcc: 5732,
        },
      },
      {
        description: "Conta legítima - NÃO DEVE DISPARAR",
        shouldTrigger: false,
        data: {
          userIndicator1: "LEGITIMATE_IDENTITY",
          userIndicator2: "UNIQUE_ACCOUNT_AGE",
          userIndicator3: "NORMAL_USAGE",
          userIndicator4: "UTILIZATION_BELOW_90",
          mcc: 5411,
        },
      },
    ],
    resultadoEsperado: "decision: DECLINE, tags: ['SYNTHETIC_ID_NETWORK']",
    operadoresUsados: ["EQ", "IN"],
    tags: ["extrema", "synthetic-identity", "rede", "bust-out"],
  },
];

// ============================================================================
// EXPORTAÇÃO DA BIBLIOTECA COMPLETA
// ============================================================================
export const RULES_LIBRARY: RuleExample[] = [
  ...REGRAS_SIMPLES,
  ...REGRAS_MEDIAS,
  ...REGRAS_COMPLEXAS,
  ...REGRAS_EXTREMAS,
];

export const RULES_LIBRARY_STATS = {
  total: RULES_LIBRARY.length,
  simples: REGRAS_SIMPLES.length,
  medias: REGRAS_MEDIAS.length,
  complexas: REGRAS_COMPLEXAS.length,
  extremas: REGRAS_EXTREMAS.length,
};

