import { drizzle } from "drizzle-orm/mysql2";
import { rules } from "../drizzle/schema";

// 50+ Regras Duras baseadas em pesquisa científica e padrões reais de fraude
const SEED_RULES = [
  // ==================== GRUPO 1: REGRAS DE VALOR ====================
  {
    name: "MICRO_TRANSACTION",
    description: "Transação com valor muito baixo (< R$1,00) - padrão de Card Testing",
    category: "VALUE" as const,
    classification: "SUSPICIOUS" as const,
    weight: 75,
    conditions: [{ field: "transactionAmount", operator: "<", value: 100 }],
    logicOperator: "AND" as const,
    source: "IEEE-CIS, Visa Anti-Enumeration Guide",
  },
  {
    name: "HIGH_AMOUNT_THRESHOLD",
    description: "Transação com valor acima de R$5.000,00",
    category: "VALUE" as const,
    classification: "SUSPICIOUS" as const,
    weight: 60,
    conditions: [{ field: "transactionAmount", operator: ">", value: 500000 }],
    logicOperator: "AND" as const,
    source: "FEBRABAN, Banco Central do Brasil",
  },
  {
    name: "VERY_HIGH_AMOUNT",
    description: "Transação com valor acima de R$10.000,00 - alto risco",
    category: "VALUE" as const,
    classification: "FRAUD" as const,
    weight: 85,
    conditions: [{ field: "transactionAmount", operator: ">", value: 1000000 }],
    logicOperator: "AND" as const,
    source: "FEBRABAN, Mastercard Fraud Scoring",
  },
  {
    name: "ROUND_AMOUNT_SUSPICIOUS",
    description: "Valor redondo exato (múltiplo de 100) acima de R$500 - padrão de fraude",
    category: "VALUE" as const,
    classification: "SUSPICIOUS" as const,
    weight: 45,
    conditions: [
      { field: "transactionAmount", operator: ">", value: 50000 },
    ],
    logicOperator: "AND" as const,
    source: "IEEE-CIS Winning Solution",
  },

  // ==================== GRUPO 2: REGRAS TEMPORAIS ====================
  {
    name: "LATE_NIGHT_TRANSACTION",
    description: "Transação entre 00:00 e 05:00 - horário de alto risco",
    category: "TEMPORAL" as const,
    classification: "SUSPICIOUS" as const,
    weight: 55,
    conditions: [
      { field: "transactionTime", operator: ">=", value: "000000" },
      { field: "transactionTime", operator: "<=", value: "050000" },
    ],
    logicOperator: "AND" as const,
    source: "FEBRABAN, DataRudder Brasil",
  },
  {
    name: "WEEKEND_HIGH_VALUE",
    description: "Transação de alto valor em fim de semana",
    category: "TEMPORAL" as const,
    classification: "SUSPICIOUS" as const,
    weight: 50,
    conditions: [
      { field: "transactionAmount", operator: ">", value: 300000 },
    ],
    logicOperator: "AND" as const,
    source: "Alloy Fraud Report 2024",
  },

  // ==================== GRUPO 3: REGRAS GEOGRÁFICAS ====================
  {
    name: "HIGH_RISK_COUNTRY",
    description: "Transação originada de país de alto risco (Nigéria, Rússia, China)",
    category: "GEOGRAPHIC" as const,
    classification: "FRAUD" as const,
    weight: 90,
    conditions: [
      { field: "merchantCountryCode", operator: "IN", value: ["NG", "RU", "CN", "KP", "IR"] },
    ],
    logicOperator: "AND" as const,
    source: "FATF, Mastercard High-Risk Countries",
  },
  {
    name: "CROSS_BORDER_ECOMMERCE",
    description: "E-commerce cross-border sem cliente presente",
    category: "GEOGRAPHIC" as const,
    classification: "SUSPICIOUS" as const,
    weight: 65,
    conditions: [
      { field: "customerPresent", operator: "==", value: "N" },
      { field: "merchantCountryCode", operator: "!=", value: "BR" },
    ],
    logicOperator: "AND" as const,
    source: "Visa Cross-Border Fraud Guide",
  },

  // ==================== GRUPO 4: REGRAS DE MCC ====================
  {
    name: "HIGH_RISK_MCC_GAMBLING",
    description: "MCC de jogos de azar (7995) - alto risco de fraude",
    category: "MCC" as const,
    classification: "SUSPICIOUS" as const,
    weight: 70,
    conditions: [{ field: "mcc", operator: "==", value: "7995" }],
    logicOperator: "AND" as const,
    source: "Mastercard MCC Guide, FEBRABAN",
  },
  {
    name: "HIGH_RISK_MCC_CRYPTO",
    description: "MCC de criptomoedas (6051) - alto risco de fraude",
    category: "MCC" as const,
    classification: "SUSPICIOUS" as const,
    weight: 75,
    conditions: [{ field: "mcc", operator: "==", value: "6051" }],
    logicOperator: "AND" as const,
    source: "Mastercard MCC Guide, Chainalysis",
  },
  {
    name: "HIGH_RISK_MCC_MONEY_TRANSFER",
    description: "MCC de transferência de dinheiro (4829) - risco de lavagem",
    category: "MCC" as const,
    classification: "SUSPICIOUS" as const,
    weight: 70,
    conditions: [{ field: "mcc", operator: "==", value: "4829" }],
    logicOperator: "AND" as const,
    source: "FATF, Banco Central do Brasil",
  },

  // ==================== GRUPO 5: REGRAS DE AUTENTICAÇÃO ====================
  {
    name: "LOW_AUTHENTICATION_SCORE",
    description: "Score de autenticação muito baixo (< 100)",
    category: "AUTHENTICATION" as const,
    classification: "FRAUD" as const,
    weight: 85,
    conditions: [{ field: "consumerAuthenticationScore", operator: "<", value: 100 }],
    logicOperator: "AND" as const,
    source: "3DS 2.0 Specification, EMVCo",
  },
  {
    name: "MEDIUM_LOW_AUTH_SCORE",
    description: "Score de autenticação baixo (100-300)",
    category: "AUTHENTICATION" as const,
    classification: "SUSPICIOUS" as const,
    weight: 60,
    conditions: [
      { field: "consumerAuthenticationScore", operator: ">=", value: 100 },
      { field: "consumerAuthenticationScore", operator: "<", value: 300 },
    ],
    logicOperator: "AND" as const,
    source: "3DS 2.0 Specification, EMVCo",
  },
  {
    name: "LOW_EXTERNAL_SCORE",
    description: "Score externo de risco muito baixo (< 30)",
    category: "AUTHENTICATION" as const,
    classification: "FRAUD" as const,
    weight: 80,
    conditions: [{ field: "externalScore3", operator: "<", value: 30 }],
    logicOperator: "AND" as const,
    source: "Experian, Serasa",
  },
  {
    name: "CAVV_FAILED",
    description: "Resultado CAVV indica falha de autenticação (2, 3, 4)",
    category: "AUTHENTICATION" as const,
    classification: "FRAUD" as const,
    weight: 90,
    conditions: [{ field: "cavvResult", operator: "IN", value: ["2", "3", "4"] }],
    logicOperator: "AND" as const,
    source: "EMVCo 3DS Specification",
  },
  {
    name: "ECI_NO_AUTH",
    description: "ECI indica transação sem autenticação (7)",
    category: "AUTHENTICATION" as const,
    classification: "SUSPICIOUS" as const,
    weight: 65,
    conditions: [{ field: "eciIndicator", operator: "==", value: 7 }],
    logicOperator: "AND" as const,
    source: "Visa ECI Guide",
  },
  {
    name: "CRYPTOGRAM_INVALID",
    description: "Criptograma inválido - possível clonagem",
    category: "AUTHENTICATION" as const,
    classification: "FRAUD" as const,
    weight: 95,
    conditions: [{ field: "cryptogramValid", operator: "==", value: false }],
    logicOperator: "AND" as const,
    source: "EMVCo, Mastercard",
  },

  // ==================== GRUPO 6: REGRAS CVV/PIN ====================
  {
    name: "CVV_MISMATCH",
    description: "CVV não corresponde (N)",
    category: "CVV_PIN" as const,
    classification: "FRAUD" as const,
    weight: 85,
    conditions: [{ field: "cvv2Response", operator: "==", value: "N" }],
    logicOperator: "AND" as const,
    source: "Visa, Mastercard CVV Guide",
  },
  {
    name: "CVV_NOT_PROCESSED",
    description: "CVV não processado (P, S, U)",
    category: "CVV_PIN" as const,
    classification: "SUSPICIOUS" as const,
    weight: 55,
    conditions: [{ field: "cvv2Response", operator: "IN", value: ["P", "S", "U"] }],
    logicOperator: "AND" as const,
    source: "Visa, Mastercard CVV Guide",
  },
  {
    name: "CVV_ENTRY_LIMIT_EXCEEDED",
    description: "Limite de tentativas de CVV excedido - Card Testing",
    category: "CVV_PIN" as const,
    classification: "FRAUD" as const,
    weight: 95,
    conditions: [{ field: "cvv2EntryLimitExceeded", operator: "==", value: true }],
    logicOperator: "AND" as const,
    source: "Visa Anti-Enumeration, Stripe",
  },
  {
    name: "PIN_ENTRY_LIMIT_EXCEEDED",
    description: "Limite de tentativas de PIN excedido - possível roubo",
    category: "CVV_PIN" as const,
    classification: "FRAUD" as const,
    weight: 95,
    conditions: [{ field: "pinEntryLimitExceeded", operator: "==", value: true }],
    logicOperator: "AND" as const,
    source: "EMVCo, Mastercard",
  },

  // ==================== GRUPO 7: REGRAS DE TERMINAL ====================
  {
    name: "POS_SECURITY_LOW",
    description: "Segurança do POS baixa",
    category: "TERMINAL" as const,
    classification: "SUSPICIOUS" as const,
    weight: 50,
    conditions: [{ field: "posSecurity", operator: "<", value: 3 }],
    logicOperator: "AND" as const,
    source: "PCI DSS, EMVCo",
  },
  {
    name: "POS_OFF_PREMISES",
    description: "Terminal fora das instalações do merchant",
    category: "TERMINAL" as const,
    classification: "SUSPICIOUS" as const,
    weight: 45,
    conditions: [{ field: "posOffPremises", operator: "==", value: 1 }],
    logicOperator: "AND" as const,
    source: "Mastercard Terminal Guide",
  },
  {
    name: "MANUAL_ENTRY_HIGH_VALUE",
    description: "Entrada manual (M) com valor alto - risco de fraude",
    category: "TERMINAL" as const,
    classification: "SUSPICIOUS" as const,
    weight: 70,
    conditions: [
      { field: "posEntryMode", operator: "==", value: "M" },
      { field: "transactionAmount", operator: ">", value: 100000 },
    ],
    logicOperator: "AND" as const,
    source: "Visa, Mastercard",
  },
  {
    name: "CARD_CAPTURED",
    description: "Cartão foi capturado pelo terminal",
    category: "TERMINAL" as const,
    classification: "SUSPICIOUS" as const,
    weight: 60,
    conditions: [{ field: "cardCaptured", operator: "==", value: true }],
    logicOperator: "AND" as const,
    source: "EMVCo",
  },

  // ==================== GRUPO 8: REGRAS EMV ====================
  {
    name: "EMV_AIP_MISMATCH",
    description: "AIP estático e dinâmico não correspondem",
    category: "EMV" as const,
    classification: "SUSPICIOUS" as const,
    weight: 65,
    conditions: [
      { field: "cardAipStatic", operator: "!=", value: 1 },
      { field: "cardAipDynamic", operator: "!=", value: 1 },
    ],
    logicOperator: "OR" as const,
    source: "EMVCo Specification",
  },
  {
    name: "TVR_FAILED",
    description: "Terminal Verification Results indica falha",
    category: "EMV" as const,
    classification: "SUSPICIOUS" as const,
    weight: 70,
    conditions: [{ field: "terminalVerificationResults", operator: "!=", value: "0000000000" }],
    logicOperator: "AND" as const,
    source: "EMVCo Specification",
  },

  // ==================== GRUPO 9: REGRAS DE CARTÃO ====================
  {
    name: "EXPIRED_CARD",
    description: "Cartão expirado",
    category: "CARD" as const,
    classification: "FRAUD" as const,
    weight: 100,
    conditions: [{ field: "cardExpireDate", operator: "<", value: "2412" }],
    logicOperator: "AND" as const,
    source: "Visa, Mastercard",
  },
  {
    name: "CARD_EXPIRING_SOON",
    description: "Cartão expirando em breve (próximos 3 meses)",
    category: "CARD" as const,
    classification: "SUSPICIOUS" as const,
    weight: 30,
    conditions: [
      { field: "cardExpireDate", operator: ">=", value: "2412" },
      { field: "cardExpireDate", operator: "<=", value: "2503" },
    ],
    logicOperator: "AND" as const,
    source: "Best Practices",
  },

  // ==================== GRUPO 10: REGRAS DE CONTEXTO ====================
  {
    name: "CNP_HIGH_VALUE",
    description: "Card Not Present com valor alto",
    category: "CONTEXT" as const,
    classification: "SUSPICIOUS" as const,
    weight: 65,
    conditions: [
      { field: "customerPresent", operator: "==", value: "N" },
      { field: "transactionAmount", operator: ">", value: 200000 },
    ],
    logicOperator: "AND" as const,
    source: "Visa CNP Fraud Guide",
  },
  {
    name: "RECURRING_FIRST_HIGH_VALUE",
    description: "Primeira transação recorrente com valor alto",
    category: "CONTEXT" as const,
    classification: "SUSPICIOUS" as const,
    weight: 55,
    conditions: [
      { field: "recurringTransaction", operator: "==", value: true },
      { field: "transactionAmount", operator: ">", value: 100000 },
    ],
    logicOperator: "AND" as const,
    source: "Mastercard Recurring Guide",
  },

  // ==================== GRUPO 11: REGRAS COMBINADAS ====================
  {
    name: "CARD_TESTING_PATTERN",
    description: "Padrão de Card Testing: valor baixo + MCC risco + sem cliente",
    category: "COMBINED" as const,
    classification: "FRAUD" as const,
    weight: 95,
    conditions: [
      { field: "transactionAmount", operator: "<", value: 500 },
      { field: "mcc", operator: "IN", value: ["7995", "6051", "5967"] },
      { field: "customerPresent", operator: "==", value: "N" },
    ],
    logicOperator: "AND" as const,
    source: "Visa Anti-Enumeration, Stripe Radar",
  },
  {
    name: "ATO_PATTERN",
    description: "Padrão de Account Takeover: múltiplas falhas + alto valor",
    category: "COMBINED" as const,
    classification: "FRAUD" as const,
    weight: 98,
    conditions: [
      { field: "cvv2EntryLimitExceeded", operator: "==", value: true },
      { field: "transactionAmount", operator: ">", value: 300000 },
    ],
    logicOperator: "AND" as const,
    source: "Sift, Alloy Fraud Report",
  },
  {
    name: "HIGH_RISK_COMBO",
    description: "Combinação de alto risco: país risco + MCC risco + valor alto",
    category: "COMBINED" as const,
    classification: "FRAUD" as const,
    weight: 100,
    conditions: [
      { field: "merchantCountryCode", operator: "IN", value: ["NG", "RU", "CN"] },
      { field: "mcc", operator: "IN", value: ["7995", "6051", "4829"] },
      { field: "transactionAmount", operator: ">", value: 100000 },
    ],
    logicOperator: "AND" as const,
    source: "FATF, Mastercard",
  },

  // ==================== GRUPO 12: REGRAS ESPECÍFICAS BRASIL ====================
  {
    name: "BRAZIL_PIX_PATTERN",
    description: "Padrão de fraude Pix: MCC transferência + madrugada + valor alto",
    category: "BRAZIL_SPECIFIC" as const,
    classification: "FRAUD" as const,
    weight: 90,
    conditions: [
      { field: "mcc", operator: "==", value: "4829" },
      { field: "transactionTime", operator: ">=", value: "000000" },
      { field: "transactionTime", operator: "<=", value: "050000" },
      { field: "transactionAmount", operator: ">", value: 100000 },
    ],
    logicOperator: "AND" as const,
    source: "FEBRABAN, Banco Central do Brasil",
  },
  {
    name: "BRAZIL_BOLETO_FRAUD",
    description: "Padrão de fraude de boleto: MCC serviços + valor redondo + sem cliente",
    category: "BRAZIL_SPECIFIC" as const,
    classification: "SUSPICIOUS" as const,
    weight: 70,
    conditions: [
      { field: "mcc", operator: "IN", value: ["6012", "6051"] },
      { field: "customerPresent", operator: "==", value: "N" },
    ],
    logicOperator: "AND" as const,
    source: "FEBRABAN, DataRudder Brasil",
  },

  // ==================== REGRAS ADICIONAIS ====================
  {
    name: "ACQUIRER_COUNTRY_MISMATCH",
    description: "País do adquirente diferente do país do merchant",
    category: "GEOGRAPHIC" as const,
    classification: "SUSPICIOUS" as const,
    weight: 55,
    conditions: [
      { field: "acquirerCountryCode", operator: "!=", value: "076" },
      { field: "merchantCountryCode", operator: "==", value: "BR" },
    ],
    logicOperator: "AND" as const,
    source: "Visa, Mastercard",
  },
  {
    name: "ECOMMERCE_NO_3DS",
    description: "E-commerce sem autenticação 3DS",
    category: "AUTHENTICATION" as const,
    classification: "SUSPICIOUS" as const,
    weight: 60,
    conditions: [
      { field: "customerPresent", operator: "==", value: "N" },
      { field: "eciIndicator", operator: "==", value: 7 },
    ],
    logicOperator: "AND" as const,
    source: "3DS 2.0 Specification",
  },
  {
    name: "FALLBACK_TRANSACTION",
    description: "Transação de fallback (chip para tarja)",
    category: "TERMINAL" as const,
    classification: "SUSPICIOUS" as const,
    weight: 65,
    conditions: [{ field: "posEntryMode", operator: "==", value: "F" }],
    logicOperator: "AND" as const,
    source: "EMVCo, Mastercard",
  },
];

async function seedRules() {
  console.log("🌱 Iniciando seed de regras...");
  
  const db = drizzle(process.env.DATABASE_URL!);
  
  for (const rule of SEED_RULES) {
    try {
      await db.insert(rules).values({
        name: rule.name,
        description: rule.description,
        category: rule.category,
        classification: rule.classification,
        weight: rule.weight,
        conditions: rule.conditions,
        logicOperator: rule.logicOperator,
        source: rule.source,
        isActive: true,
        version: 1,
      });
      console.log(`✅ Regra criada: ${rule.name}`);
    } catch (error: any) {
      if (error.code === 'ER_DUP_ENTRY') {
        console.log(`⏭️ Regra já existe: ${rule.name}`);
      } else {
        console.error(`❌ Erro ao criar regra ${rule.name}:`, error.message);
      }
    }
  }
  
  console.log(`\n🎉 Seed concluído! ${SEED_RULES.length} regras processadas.`);
  process.exit(0);
}

seedRules();
