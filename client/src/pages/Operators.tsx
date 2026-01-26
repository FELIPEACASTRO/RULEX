import { useState } from "react";
import { BACKEND_OPERATORS } from "@/manual/generated/backendOperators.generated";
import { OPERATOR_SPECS_BACKEND_ONLY as OPERATOR_SPECS } from "@/manual/operatorSpecsBackendOnly";
import { type OperatorDocConfidence, type OperatorDocLevel } from "@/manual/operatorSpecs";

type Operator = (typeof BACKEND_OPERATORS)[number];

type FieldTypeHint = "string" | "number" | "boolean" | "date" | "time" | "array" | "object";

type FieldHint = {
  path: string;
  type: FieldTypeHint;
  example: string;
  note?: string;
};

type TestCaseHint = {
  scenario: string;
  expected: string;
};

type OperatorNameExplain = {
  tokens: string[];
  leituraHumana: string;
  glossario: string[];
};

type DidacticKit = {
  resumo: string;
  modeloMental: string;
  quandoUsar: string[];
  quandoEvitar: string[];
  armadilhas: string[];
  camposSugeridos: FieldHint[];
  exemploPayload: string;
  exemploDsl: string;
  casosDeTeste: TestCaseHint[];
  relacionados: string[];
};

// ═══════════════════════════════════════════════════════════════════════════════
// 🧠 METODOLOGIA "USE A CABEÇA" (HEAD FIRST)
// ═══════════════════════════════════════════════════════════════════════════════
// Esta página usa técnicas comprovadas de aprendizado:
// ✅ Histórias do mundo real com personagens
// ✅ Analogias do dia a dia
// ✅ Perguntas provocativas ("E se...?")
// ✅ Exemplos visuais passo a passo
// ✅ Seção "Não existem perguntas idiotas"
// ✅ Antes vs Depois (o que acontece sem/com a regra)
// ═══════════════════════════════════════════════════════════════════════════════

const normalizeCategory = (category?: string) => {
  const normalized = category?.trim();
  if (!normalized || normalized === "=" || normalized.toLowerCase() === "natural") {
    return "Geral";
  }
  return normalized;
};

const uniq = <T,>(items: T[]) => Array.from(new Set(items));

const safeJsonStringify = (value: unknown) => {
  try {
    return JSON.stringify(value, null, 2);
  } catch {
    return String(value);
  }
};

const tokenizeOperatorName = (name: string) => name.split(/[_\s]+/g).filter(Boolean);

const tokensToSet = (name: string) => new Set(tokenizeOperatorName(name.toUpperCase()));

const pickFirst = <T,>(values: T[]) => (values.length ? values[0] : undefined);

type TokenContext = {
  entityPt: string;
  eventPlural: string;
  groupBy: string;
  thresholdExample: string;
};

const inferTokenContext = (name: string): TokenContext => {
  const t = tokensToSet(name);

  // Entities (prefer more specific)
  if (t.has("ADDRESS")) {
    return { entityPt: "endereço", eventPlural: "address_changes", groupBy: "customer_id", thresholdExample: "2" };
  }
  if (t.has("BENEFICIARY")) {
    return { entityPt: "beneficiário", eventPlural: "beneficiary_events", groupBy: "customer_id", thresholdExample: "3" };
  }
  if (t.has("CHARGEBACK")) {
    return { entityPt: "chargeback", eventPlural: "chargebacks", groupBy: "merchant_id", thresholdExample: "0.02" };
  }
  if (t.has("CARD")) {
    return { entityPt: "cartão", eventPlural: "card_events", groupBy: "card_id", thresholdExample: "5" };
  }
  if (t.has("ACCOUNT")) {
    return { entityPt: "conta", eventPlural: "account_events", groupBy: "account_id", thresholdExample: "2" };
  }
  if (t.has("DEVICE")) {
    return { entityPt: "dispositivo", eventPlural: "device_events", groupBy: "device_fingerprint", thresholdExample: "2" };
  }
  if (t.has("IP")) {
    return { entityPt: "IP", eventPlural: "ip_events", groupBy: "ip", thresholdExample: "10" };
  }
  if (t.has("CHANNEL")) {
    return { entityPt: "canal", eventPlural: "channel_events", groupBy: "customer_id", thresholdExample: "2" };
  }
  if (t.has("MERCHANT") || t.has("MCC") || t.has("STORE")) {
    return { entityPt: "merchant", eventPlural: "merchant_events", groupBy: "merchant_id", thresholdExample: "2" };
  }

  return { entityPt: "transação", eventPlural: "transactions", groupBy: "customer_id", thresholdExample: "5" };
};

const TOKEN_PT: Record<string, string> = {
  ACCOUNT: "conta",
  AGE: "idade",
  AMOUNT: "valor",
  AVG: "média",
  BETWEEN: "entre",
  BIN: "BIN",
  BOOLEAN: "booleano",
  BROWSER: "navegador",
  CARD: "cartão",
  CHANNEL: "canal",
  CITY: "cidade",
  CONTAINS: "contém",
  COUNT: "contagem",
  COUNTRY: "país",
  CPF: "CPF",
  DATE: "data",
  DAY: "dia",
  DAYS: "dias",
  DEVICE: "dispositivo",
  DISTANCE: "distância",
  EMAIL: "e-mail",
  ENDS: "termina",
  ENDS_WITH: "termina com",
  EQ: "igual",
  EQUAL: "igual",
  FAILED: "falhou",
  FINGERPRINT: "impressão digital",
  FRAUD: "fraude",
  GEO: "geo",
  GT: "maior que",
  GTE: "maior ou igual",
  HOUR: "hora",
  HOURS: "horas",
  IN: "está em",
  IP: "IP",
  IS: "é",
  IS_FALSE: "é falso",
  IS_NULL: "está vazio",
  IS_TRUE: "é verdadeiro",
  KRI: "KRI",
  LAST: "últimos",
  LIST: "lista",
  LT: "menor que",
  LTE: "menor ou igual",
  MAX: "máximo",
  MCC: "MCC",
  MIN: "mínimo",
  MINUTES: "minutos",
  MONTH: "mês",
  MONTHS: "meses",
  NEW: "novo",
  NOT: "não",
  NOT_IN: "não está em",
  NULL: "vazio",
  OR: "ou",
  PERCENT: "percentual",
  PER: "por",
  PHONE: "telefone",
  POS: "POS",
  RATE: "taxa",
  REGEX: "regex",
  RISK: "risco",
  SCORE: "score",
  STARTS: "começa",
  STARTS_WITH: "começa com",
  SUM: "soma",
  TIME: "horário",
  TXN: "transação",
  TRANSACTION: "transação",
  TRUE: "verdadeiro",
  USER: "usuário",
  VELOCITY: "velocidade",
  WEEK: "semana",
  WEEKS: "semanas",
  WITH: "com",
  YEAR: "ano",
  YEARS: "anos",
};

const explainOperatorName = (name: string): OperatorNameExplain => {
  const tokens = tokenizeOperatorName(name);
  const translated = tokens.map((t) => TOKEN_PT[t] ?? t.toLowerCase());
  const leituraHumana = translated.join(" ");
  const glossario = uniq(
    tokens
      .filter((t) => TOKEN_PT[t])
      .map((t) => `${t} = ${TOKEN_PT[t]}`)
  );
  return { tokens, leituraHumana, glossario };
};

type OperatorKind =
  | "logical"
  | "compare"
  | "range"
  | "list"
  | "string"
  | "null"
  | "boolean"
  | "array"
  | "datetime"
  | "aggregation"
  | "risk_pattern"
  | "graph"
  | "device"
  | "identity"
  | "merchant"
  | "platform"
  | "validation"
  | "statistical"
  | "unknown";

const classifyOperator = (nameRaw: string): OperatorKind => {
  const name = nameRaw.toUpperCase();

  // Operadores lógicos básicos
  if (["AND", "OR", "NOT", "NAND", "NOR", "XOR", "IMPLY"].includes(name)) return "logical";

  // Range / faixa
  if (name.includes("BETWEEN")) return "range";

  // Listas
  if (name === "IN" || name.endsWith("_IN") || name.includes("NOT_IN") || name.includes("IN_LIST")) return "list";

  // Strings / texto
  if (
    name.includes("CONTAINS") ||
    name.includes("REGEX") ||
    name.includes("STARTS_WITH") ||
    name.includes("ENDS_WITH") ||
    name.includes("MATCH")
  )
    return "string";

  // Nulos / vazios
  if (name.includes("NULL") || name.startsWith("IS_NULL") || name.startsWith("NOT_NULL") || name.includes("EMPTY"))
    return "null";

  // Booleanos
  if (name.startsWith("IS_TRUE") || name.startsWith("IS_FALSE") || name === "IS_VALID" || name === "IS_INVALID")
    return "boolean";

  // Arrays / listas
  if (name.startsWith("ARRAY_") || name.includes("ARRAY") || name.startsWith("LIST_")) return "array";

  // Data/tempo
  if (
    name.includes("DATE") ||
    name.includes("TIME") ||
    name.includes("DAY") ||
    name.includes("WEEK") ||
    name.includes("MONTH") ||
    name.includes("YEAR") ||
    name.includes("AGE_") ||
    name.includes("HOUR") ||
    name.includes("DORMANCY") ||
    name.includes("EXPIRED")
  )
    return "datetime";

  // Agregações
  if (
    name.includes("COUNT") ||
    name.includes("SUM") ||
    name.includes("AVG") ||
    name.includes("MAX") ||
    name.includes("MIN") ||
    name.includes("PERCENT") ||
    name.includes("MEDIAN") ||
    name.includes("VARIANCE") ||
    name.includes("STD_DEV")
  )
    return "aggregation";

  // Grafos
  if (
    name.startsWith("NEO4J_") ||
    name.includes("GRAPH") ||
    name.includes("LINK_DEPTH") ||
    name.includes("CLUSTER") ||
    name.includes("NETWORK") ||
    name.includes("RING")
  )
    return "graph";

  // Dispositivo / device
  if (
    name.startsWith("DEVICE_") ||
    name.includes("BROWSER") ||
    name.includes("FINGERPRINT") ||
    name.includes("JAILBREAK") ||
    name.includes("ROOTED") ||
    name.includes("AUDIO_FINGERPRINT") ||
    name.includes("TRUST_SCORE") ||
    name.includes("USER_AGENT")
  )
    return "device";

  // Identidade / cadastro
  if (
    name.includes("EMAIL") ||
    name.includes("PHONE") ||
    name.includes("CPF") ||
    name.includes("SSN") ||
    name.includes("ADDRESS") ||
    name.includes("NAME_") ||
    name.includes("BIOMETRIC") ||
    name.includes("IDENTITY") ||
    name.includes("CREDITOR")
  )
    return "identity";

  // Merchant / comerciante
  if (name.startsWith("MERCHANT_") || name.includes("MCC") || name.includes("STORE") || name.includes("POS_"))
    return "merchant";

  // Plataforma (PLT_)
  if (name.startsWith("PLT_") || name.startsWith("DORA_") || name.startsWith("EIDAS_") || name.startsWith("GDPR_"))
    return "platform";

  // Validação / verificação
  if (
    name.includes("VALIDATION") ||
    name.includes("CHECK") ||
    name.includes("VERIFY") ||
    name.includes("VALID") ||
    name.includes("SANCTION") ||
    name.includes("PEP") ||
    name.includes("ADVERSE") ||
    name.includes("CONSORTIUM")
  )
    return "validation";

  // Estatísticos / ML
  if (
    name.includes("ANOMALY") ||
    name.includes("DEVIATION") ||
    name.includes("TEST") ||
    name.includes("BENFORD") ||
    name.includes("ANDERSON") ||
    name.includes("CHI_SQUARE") ||
    name.includes("KOLMOGOROV") ||
    name.includes("ADAPTIVE") ||
    name.includes("FUZZY") ||
    name.includes("THRESHOLD") ||
    name.includes("SCORE") ||
    name.includes("INDICATOR")
  )
    return "statistical";

  // Padrões de risco / fraude (catch-all para especialistas)
  if (
    name.includes("VELOCITY") ||
    name.includes("DETECTION") ||
    name.includes("PATTERN") ||
    name.includes("RISK") ||
    name.includes("FRAUD") ||
    name.includes("SPIKE") ||
    name.includes("SUSPICIOUS") ||
    name.startsWith("FATF_") ||
    name.startsWith("SCA_") ||
    name.startsWith("BSL_") ||
    name.includes("TAKEOVER") ||
    name.includes("SMURFING") ||
    name.includes("LAYERING") ||
    name.includes("STRUCTURING")
  )
    return "risk_pattern";

  // Comparações (fallback para _GT, _LT, etc.)
  if (["GT", "GTE", "LT", "LTE", "EQ", "NEQ"].some((k) => name === k || name.endsWith(`_${k}`) || name.includes(`_${k}_`)))
    return "compare";

  return "unknown";
};

const defaultFieldHintsForKind = (kind: OperatorKind): FieldHint[] => {
  switch (kind) {
    case "compare":
    case "range":
      return [
        { path: "transaction.amount", type: "number", example: "1500", note: "Valor da transação" },
        { path: "customer.age", type: "number", example: "22", note: "Idade do cliente" },
        { path: "transaction.score", type: "number", example: "0.82", note: "Score do modelo" },
      ];
    case "list":
      return [
        { path: "transaction.channel", type: "string", example: "APP", note: "Canal de origem" },
        { path: "transaction.country", type: "string", example: "BR", note: "País" },
        { path: "merchant.mcc", type: "string", example: "5411", note: "Categoria do merchant" },
      ];
    case "string":
      return [
        { path: "customer.email", type: "string", example: "user@empresa.com", note: "E-mail" },
        { path: "transaction.description", type: "string", example: "PIX JOAO 123", note: "Descrição" },
        { path: "device.user_agent", type: "string", example: "Mozilla/5.0 ...", note: "User-Agent" },
      ];
    case "null":
      return [
        { path: "transaction.device_id", type: "string", example: "", note: "Campo pode vir ausente" },
        { path: "customer.phone", type: "string", example: "", note: "Pode estar vazio" },
      ];
    case "boolean":
      return [
        { path: "customer.is_vip", type: "boolean", example: "true", note: "Flag" },
        { path: "customer.email_verified", type: "boolean", example: "false", note: "Verificação" },
      ];
    case "array":
      return [
        { path: "order.items", type: "array", example: "[{...},{...}]", note: "Lista de itens" },
        { path: "order.tags", type: "array", example: "[\"promocao\",\"vip\"]", note: "Tags" },
      ];
    case "datetime":
      return [
        { path: "transaction.date", type: "date", example: "2026-01-25", note: "Data (ISO)" },
        { path: "transaction.time", type: "time", example: "22:30", note: "Horário" },
        { path: "customer.created_at", type: "date", example: "2026-01-20", note: "Data de criação" },
      ];
    case "aggregation":
      return [
        { path: "transactions.amount", type: "number", example: "100", note: "Campo agregado" },
        { path: "transactions", type: "array", example: "[...]", note: "Janela de eventos" },
      ];
    case "graph":
      return [
        { path: "customer_id", type: "string", example: "c_123", note: "Nó principal" },
        { path: "device_id", type: "string", example: "d_999", note: "Nó relacionado" },
      ];
    case "risk_pattern":
      return [
        { path: "transaction.amount", type: "number", example: "2500", note: "Sinal de risco" },
        { path: "transaction.ip", type: "string", example: "203.0.113.10", note: "IP" },
        { path: "device.fingerprint", type: "string", example: "fp_xxx", note: "Fingerprint" },
      ];
    case "device":
      return [
        { path: "device.fingerprint", type: "string", example: "fp_abc123", note: "Fingerprint do device" },
        { path: "device.trust_score", type: "number", example: "0.75", note: "Score de confiança" },
        { path: "device.is_rooted", type: "boolean", example: "false", note: "Dispositivo rooteado?" },
        { path: "device.browser", type: "string", example: "Chrome 120", note: "Navegador" },
      ];
    case "identity":
      return [
        { path: "customer.email", type: "string", example: "user@empresa.com", note: "E-mail do cliente" },
        { path: "customer.phone", type: "string", example: "+5511999998888", note: "Telefone" },
        { path: "customer.cpf", type: "string", example: "123.456.789-00", note: "CPF formatado" },
        { path: "customer.address", type: "object", example: "{...}", note: "Endereço completo" },
      ];
    case "merchant":
      return [
        { path: "merchant.mcc", type: "string", example: "5411", note: "Código MCC" },
        { path: "merchant.name", type: "string", example: "LOJA XYZ", note: "Nome do merchant" },
        { path: "merchant.country", type: "string", example: "BR", note: "País do merchant" },
        { path: "merchant.risk_level", type: "string", example: "HIGH", note: "Nível de risco" },
      ];
    case "platform":
      return [
        { path: "platform.compliance_status", type: "string", example: "COMPLIANT", note: "Status de compliance" },
        { path: "platform.region", type: "string", example: "EU", note: "Região regulatória" },
        { path: "platform.data_retention_days", type: "number", example: "365", note: "Dias de retenção" },
      ];
    case "validation":
      return [
        { path: "validation.result", type: "string", example: "PASS", note: "Resultado da validação" },
        { path: "validation.pep_status", type: "boolean", example: "false", note: "É PEP?" },
        { path: "validation.sanction_hit", type: "boolean", example: "false", note: "Match em sanções?" },
      ];
    case "statistical":
      return [
        { path: "transaction.amount", type: "number", example: "1500", note: "Valor para análise" },
        { path: "statistics.deviation", type: "number", example: "2.5", note: "Desvios da média" },
        { path: "statistics.percentile", type: "number", example: "95", note: "Percentil" },
        { path: "model.score", type: "number", example: "0.87", note: "Score do modelo" },
      ];
    default:
      return [
        { path: "campo", type: "string", example: "valor", note: "Substitua pelo seu payload" },
      ];
  }
};

// ═══════════════════════════════════════════════════════════════════════════
// GERADOR DE EXPLICAÇÕES ÚNICAS PARA CADA OPERADOR
// ═══════════════════════════════════════════════════════════════════════════
const gerarExplicacaoSintaxeUnica = (name: string, kind: OperatorKind, sintaxe: string): string => {
  const upper = name.toUpperCase();
  
  // Operadores lógicos
  if (upper === "NAND") return "📖 Leia: 'Só dispara se NÃO for verdade que AMBAS condições são verdadeiras'. É o oposto do AND.";
  if (upper === "NOR") return "📖 Leia: 'Só dispara se NENHUMA das condições for verdadeira'. É o oposto do OR.";
  if (upper === "XOR") return "📖 Leia: 'Dispara se APENAS UMA das condições for verdadeira, mas não ambas'. OU exclusivo.";
  
  // Comparadores de AMOUNT
  if (upper === "AMOUNT_GT") return "📖 Leia: 'Se o valor da transação for MAIOR QUE 10.000, dispare'. Detecta transações de alto valor.";
  if (upper === "AMOUNT_GTE") return "📖 Leia: 'Se o valor for MAIOR OU IGUAL a 5.000, dispare'. Inclui o limite.";
  if (upper === "AMOUNT_LT") return "📖 Leia: 'Se o valor for MENOR QUE 50, dispare'. Detecta micro-transações suspeitas.";
  if (upper === "AMOUNT_LTE") return "📖 Leia: 'Se o valor for MENOR OU IGUAL a 1.000, dispare'. Inclui o limite.";
  
  // Comparadores de SCORE
  if (upper === "SCORE_GT") return "📖 Leia: 'Se o score de risco for MAIOR QUE 70, dispare'. Threshold de alta confiança.";
  if (upper === "SCORE_GTE") return "📖 Leia: 'Se o score de risco for MAIOR OU IGUAL a 50, dispare'. Captura mais casos.";
  if (upper === "SCORE_LT") return "📖 Leia: 'Se o score de risco for MENOR QUE 30, permita'. Transações seguras.";
  if (upper === "SCORE_LTE") return "📖 Leia: 'Se o score for MENOR OU IGUAL a 40, considere baixo risco'.";
  
  // Comparadores de AGE (idade)
  if (upper === "AGE_GT") return "📖 Leia: 'Se a idade do cliente for MAIOR QUE 65, aplique regras para idosos'.";
  if (upper === "AGE_GTE") return "📖 Leia: 'Se a idade for MAIOR OU IGUAL a 18, permita'. Verificação de maioridade.";
  if (upper === "AGE_LT") return "📖 Leia: 'Se a idade for MENOR QUE 18, bloqueie'. Proteção de menores.";
  if (upper === "AGE_LTE") return "📖 Leia: 'Se a idade for MENOR OU IGUAL a 25, aplique regras para jovens'.";
  
  // Agregações COUNT
  if (upper === "COUNT_GT") return "📖 Leia: 'Se o número de transações nas últimas 24h com este cartão for MAIOR QUE 10, dispare'. Velocity.";
  if (upper === "COUNT_GTE") return "📖 Leia: 'Se houver 5 OU MAIS transações recentes, dispare'. Detecta burst.";
  if (upper === "COUNT_LT") return "📖 Leia: 'Se tiver MENOS QUE 3 logins na última hora, considere normal'.";
  if (upper === "COUNT_LTE") return "📖 Leia: 'Se as tentativas falhadas forem 5 OU MENOS, permita continuar'.";
  
  // Agregações SUM
  if (upper === "SUM_GT") return "📖 Leia: 'Se a SOMA dos valores nos últimos 7 dias for MAIOR QUE 50.000, alerte'. Anti-smurfing.";
  if (upper === "SUM_GTE") return "📖 Leia: 'Se a soma for MAIOR OU IGUAL a 50.000, sinalize para compliance'.";
  if (upper === "SUM_LT") return "📖 Leia: 'Se a soma de reembolsos for MENOR QUE 10.000, considere normal'.";
  if (upper === "SUM_LTE") return "📖 Leia: 'Se os saques forem MENORES OU IGUAIS a 3.000, permita'.";
  
  // Agregações AVG
  if (upper === "AVG_GT") return "📖 Leia: 'Se a MÉDIA dos valores for MAIOR QUE 500, o cliente tem ticket alto'.";
  if (upper === "AVG_GTE") return "📖 Leia: 'Se a média for MAIOR OU IGUAL a 200, classifique como premium'.";
  if (upper === "AVG_LT") return "📖 Leia: 'Se a média for MENOR QUE 50, pode ser teste de cartão (baixos valores)'.";
  if (upper === "AVG_LTE") return "📖 Leia: 'Se a gorjeta média for MENOR OU IGUAL a 10, comportamento normal'.";
  
  // Agregações MAX/MIN
  if (upper === "MAX_GT") return "📖 Leia: 'Se o MAIOR valor nas últimas 24h for MAIOR QUE 5.000, houve pico'.";
  if (upper === "MAX_GTE") return "📖 Leia: 'Se o máximo for MAIOR OU IGUAL a 10.000, revise manualmente'.";
  if (upper === "MIN_LT") return "📖 Leia: 'Se o MENOR valor for MENOR QUE 10, pode ser teste de cartão'.";
  if (upper === "MIN_LTE") return "📖 Leia: 'Se o mínimo de depósito for MENOR OU IGUAL a 100, considere normal'.";
  
  // Percentual e Velocity
  if (upper.includes("PERCENT")) return "📖 Leia: 'Se a VARIAÇÃO PERCENTUAL for MAIOR QUE 200%, houve mudança drástica no padrão'.";
  if (upper.includes("VELOCITY")) return "📖 Leia: 'Se a VELOCIDADE de transações por hora for MAIOR QUE 5, detectou burst de atividade'.";
  if (upper.includes("DISTINCT")) return "📖 Leia: 'Se a QUANTIDADE DE PAÍSES DISTINTOS nas últimas 24h for MAIOR QUE 3, possível fraude geo-distribuída'.";
  
  // Operadores de lista (IN)
  if (upper === "MCC_IN") return "📖 Leia: 'Se o MCC do merchant estiver NA LISTA [5411=supermercado, 5912=farmácia, 5999=varejo], dispare'.";
  if (upper === "MCC_NOT_IN") return "📖 Leia: 'Se o MCC NÃO estiver na lista de alto risco [7995=apostas, 5967=MLM], permita'.";
  if (upper === "COUNTRY_IN") return "📖 Leia: 'Se o país estiver NA LISTA [BR, AR, CL, MX] (América Latina), aplique regras regionais'.";
  if (upper === "COUNTRY_NOT_IN") return "📖 Leia: 'Se o país NÃO estiver na lista de sanções [KP, IR, CU, SY], permita'.";
  if (upper === "CHANNEL_IN") return "📖 Leia: 'Se o canal estiver NA LISTA [APP, WEB, API], são canais digitais válidos'.";
  if (upper === "STATUS_IN") return "📖 Leia: 'Se o status estiver em [PENDING, REVIEW, HOLD], a transação precisa de atenção'.";
  
  // Operadores de string
  if (upper === "EMAIL_CONTAINS") return "📖 Leia: 'Se o e-mail CONTIVER @tempmail, é provável e-mail descartável - alto risco'.";
  if (upper === "NAME_CONTAINS") return "📖 Leia: 'Se o nome CONTIVER TEST, pode ser conta de teste sendo usada em produção'.";
  if (upper === "DESCRIPTION_CONTAINS") return "📖 Leia: 'Se a descrição CONTIVER REFUND, é uma transação de estorno'.";
  if (upper === "EMAIL_STARTS_WITH") return "📖 Leia: 'Se o e-mail COMEÇAR COM test_, é provável conta de teste'.";
  if (upper === "PHONE_STARTS_WITH") return "📖 Leia: 'Se o telefone COMEÇAR COM +55, é número brasileiro'.";
  if (upper === "BIN_STARTS_WITH") return "📖 Leia: 'Se o BIN COMEÇAR COM 411111, é cartão Visa de teste'.";
  if (upper === "EMAIL_ENDS_WITH") return "📖 Leia: 'Se o e-mail TERMINAR COM @gmail.com, é conta pessoal (não corporativa)'.";
  if (upper === "DOMAIN_ENDS_WITH") return "📖 Leia: 'Se o domínio TERMINAR COM .ru, é domínio russo - pode requerer revisão'.";
  if (upper.includes("REGEX") || upper.includes("MATCH")) return "📖 O REGEX valida o formato exato do campo. Exemplo: /^\\+55\\d{11}$/ = telefone BR com +55 e 11 dígitos.";
  
  // Operadores de data/tempo
  if (upper === "DATE_BEFORE") return "📖 Leia: 'Se a data for ANTERIOR a 01/01/2025, aplique regras do ano anterior'.";
  if (upper === "DATE_AFTER_OR_EQ") return "📖 Leia: 'Se a data de criação for A PARTIR DE 01/06/2024, é conta nova'.";
  if (upper === "AGE_DAYS_GT") return "📖 Leia: 'Se a conta existir há MAIS DE 30 dias, é conta estabelecida - menor risco'.";
  if (upper === "AGE_DAYS_GTE") return "📖 Leia: 'Se o cartão foi emitido há 7 DIAS OU MAIS, já passou do período de alto risco'.";
  if (upper === "AGE_HOURS_LT") return "📖 Leia: 'Se a sessão começou há MENOS DE 24 horas, ainda é sessão ativa válida'.";
  if (upper === "HOUR_BETWEEN") return "📖 Leia: 'Se a hora estiver ENTRE 9h e 18h, é horário comercial - menor risco'.";
  if (upper === "DAY_OF_WEEK_IN") return "📖 Leia: 'Se o dia for SÁBADO ou DOMINGO, aplique regras de fim de semana'.";
  if (upper === "WEEKEND") return "📖 Leia: 'Se a data for FIM DE SEMANA, alguns padrões de fraude são mais comuns'.";
  if (upper === "BUSINESS_HOURS") return "📖 Leia: 'Se for HORÁRIO COMERCIAL, espera-se mais transações B2B'.";
  
  // Operadores de device
  if (upper.includes("DEVICE_NEW")) return "📖 Leia: 'Se o dispositivo foi visto pela PRIMEIRA VEZ há menos de 1 dia, é device novo - alto risco'.";
  if (upper.includes("DEVICE_TRUST")) return "📖 Leia: 'Se o TRUST SCORE do device for MAIOR QUE 0.8 (80%), é dispositivo confiável'.";
  if (upper.includes("DEVICE_FINGERPRINT")) return "📖 Leia: 'Se o FINGERPRINT DO DEVICE BATER com o histórico, é o mesmo aparelho'.";
  if (upper.includes("JAILBREAK") || upper.includes("ROOT")) return "📖 Leia: 'Se o device ESTÁ ROOTED/JAILBROKEN, pode ter sido adulterado - alto risco'.";
  if (upper.includes("EMULATOR")) return "📖 Leia: 'Se o device É UM EMULADOR, provável automação ou fraude - bloqueie'.";
  if (upper.includes("VPN") || upper.includes("PROXY")) return "📖 Leia: 'Se a conexão USA VPN/PROXY, o usuário está escondendo localização real'.";
  
  // Operadores de grafo
  if (upper.includes("NEO4J") || upper.includes("GRAPH")) {
    if (upper.includes("LINK")) return "📖 Leia: 'Se a PROFUNDIDADE DO LINK entre cliente e cartão for MAIOR QUE 2, há intermediários suspeitos'.";
    if (upper.includes("PATH")) return "📖 Leia: 'Se o CAMINHO MAIS CURTO entre contas A e B for MENOR OU IGUAL a 3, estão próximos na rede'.";
    if (upper.includes("CLUSTER")) return "📖 Leia: 'Se o TAMANHO DO CLUSTER do device for MAIOR QUE 10, muitas contas usam o mesmo device'.";
    return "📖 Leia: 'Se as CONEXÕES DO GRAFO forem MAIORES QUE 5, há muitos relacionamentos - investigar'.";
  }
  
  // Operadores FATF/AML
  if (upper.startsWith("FATF_")) {
    if (upper.includes("COUNTRY")) return "📖 Leia: 'Se o país estiver na LISTA FATF DE ALTO RISCO, requer EDD (Enhanced Due Diligence)'.";
    if (upper.includes("PEP")) return "📖 Leia: 'Se o cliente for PEP (Pessoa Exposta Politicamente), aplique controles reforçados'.";
    if (upper.includes("SANCTION")) return "📖 Leia: 'Se o nome tiver HIT EM LISTA DE SANÇÕES, bloqueie imediatamente e alerte compliance'.";
    return "📖 Leia: 'Se o SCORE FATF de risco for MAIOR QUE 70, a transação requer revisão AML'.";
  }
  
  // Operadores de compliance
  if (upper.startsWith("SCA_")) return "📖 Leia: 'Se a transação for ELEGÍVEL PARA ISENÇÃO SCA, pode pular autenticação forte'.";
  if (upper.startsWith("PSD")) return "📖 Leia: 'Se PSD2 REQUER SCA para esta transação, exija autenticação de 2 fatores'.";
  if (upper.startsWith("DORA_")) return "📖 Leia: 'Se a SEVERIDADE DO INCIDENTE DORA for MAIOR QUE 2, reporte ao regulador'.";
  if (upper.startsWith("BSL_")) return "📖 Leia: 'Se houver VIOLAÇÃO DE POLÍTICA BSL, aplique controles de segurança bancária'.";
  if (upper.startsWith("PLT_")) return "📖 Leia: 'Se o RATE LIMIT DA API foi EXCEDIDO, bloqueie para prevenir abuso'.";
  
  // Fallback por categoria com mais contexto
  if (kind === "logical") return `📖 O operador ${name} combina condições logicamente. Leia: 'Se (condição A) ${name} (condição B), então dispare'.`;
  if (kind === "range") return `📖 O operador ${name} verifica se um valor está dentro de uma faixa. Leia a sintaxe como: 'campo ${name} limite_inferior AND limite_superior'.`;
  if (kind === "list") return `📖 O operador ${name} verifica pertencimento a uma lista. Leia: 'Se campo ${name} [valor1, valor2, ...], dispare'.`;
  if (kind === "string") return `📖 O operador ${name} busca padrões em texto. Leia: 'Se texto_campo ${name} "padrão_buscado", dispare'.`;
  if (kind === "null") return `📖 O operador ${name} detecta campos vazios ou preenchidos. Use para tratar dados ausentes no payload.`;
  if (kind === "boolean") return `📖 O operador ${name} trabalha com true/false. Leia: 'Se campo_booleano ${name}, dispare'.`;
  if (kind === "array") return `📖 O operador ${name} trabalha com listas/arrays. Verifica conteúdo ou tamanho de coleções.`;
  if (kind === "datetime") return `📖 O operador ${name} avalia datas e horários. Use para criar regras temporais.`;
  if (kind === "aggregation") return `📖 O operador ${name} agrega dados históricos. Sintaxe: ${name}(eventos, janela_tempo, agrupamento) COMPARADOR valor.`;
  if (kind === "graph") return `📖 O operador ${name} analisa conexões em grafos. Revela relacionamentos ocultos entre entidades.`;
  if (kind === "device") return `📖 O operador ${name} avalia características do dispositivo. Use para detectar devices suspeitos.`;
  if (kind === "identity") return `📖 O operador ${name} valida dados cadastrais. Use para verificar consistência de identidade.`;
  if (kind === "merchant") return `📖 O operador ${name} avalia o comerciante. Use para regras baseadas em tipo de estabelecimento.`;
  if (kind === "platform") return `📖 O operador ${name} verifica compliance de plataforma. Use para requisitos regulatórios.`;
  if (kind === "validation") return `📖 O operador ${name} executa validações externas. Use para checagens de sanções, PEP, etc.`;
  if (kind === "statistical") return `📖 O operador ${name} aplica análise estatística. Use para detectar anomalias e desvios.`;
  if (kind === "risk_pattern") return `📖 O operador ${name} detecta padrões de risco. É um detector composto de fraude/AML.`;
  if (kind === "compare") return `📖 O operador ${name} compara valores. Leia: 'Se campo ${name} valor_limite, dispare'.`;

  return `📖 O operador ${name} aplica a lógica específica da sintaxe: ${sintaxe}. Consulte a documentação para casos de uso avançados.`;
};

const guessDslForKind = (name: string, kind: OperatorKind): string => {
  const upper = name.toUpperCase();
  if (HEAD_FIRST_EXAMPLES[upper]) return HEAD_FIRST_EXAMPLES[upper].sintaxe;

  const ctx = inferTokenContext(upper);

  // Templates por tokens (reduz repetição e deixa o exemplo "sobre" o assunto do operador)
  if (upper.includes("VELOCITY")) {
    return `VELOCITY(${ctx.eventPlural}, last_24h, ${ctx.groupBy}) GT ${ctx.thresholdExample}`;
  }

  // RATE / porcentagens (ex: CHARGEBACK_RATE_GT)
  if (upper.includes("RATE") && (upper.endsWith("_GT") || upper.endsWith("_GTE") || upper.endsWith("_LT") || upper.endsWith("_LTE"))) {
    if (upper.includes("CHARGEBACK")) return `merchant.chargeback_rate ${upper.endsWith("_LT") || upper.endsWith("_LTE") ? "LT" : "GT"} 0.02`;
    if (upper.includes("FRAUD")) return `customer.fraud_rate ${upper.endsWith("_LT") || upper.endsWith("_LTE") ? "LT" : "GT"} 0.03`;
    return `metrics.rate ${upper.endsWith("_LT") || upper.endsWith("_LTE") ? "LT" : "GT"} 0.02`;
  }

  // AGE em dias/minutos (ex: ACCOUNT_AGE_LT_DAYS)
  if (upper.includes("AGE") && (upper.includes("_DAYS") || upper.includes("_MINUTES") || upper.includes("_HOURS"))) {
    if (upper.includes("ACCOUNT")) return `account.age_${upper.includes("_DAYS") ? "days" : upper.includes("_HOURS") ? "hours" : "minutes"} LT 7`;
    if (upper.includes("DEVICE")) return `device.first_seen_age_${upper.includes("_DAYS") ? "days" : upper.includes("_HOURS") ? "hours" : "minutes"} LT 1`;
    return `entity.age_${upper.includes("_DAYS") ? "days" : upper.includes("_HOURS") ? "hours" : "minutes"} LT 7`;
  }

  // DETECTION/PATTERN/ANOMALY/TEST: deixa explícito que é ilustrativo (forma "função")
  if (isHeuristicHeavyOperator(upper)) {
    return `${upper}(transaction) IS_TRUE`;
  }

  // Gerar sintaxe ÚNICA baseada no nome do operador
  // ═══════════════════════════════════════════════════════════════════════════
  
  // Operadores lógicos
  if (upper === "NAND") return "NOT ((amount GT 1000) AND (country EQ \"BR\"))";
  if (upper === "NOR") return "NOT ((channel EQ \"APP\") OR (channel EQ \"WEB\"))";
  if (upper === "XOR") return "(is_vip EQ true) XOR (is_employee EQ true)";
  
  // Comparadores específicos
  if (upper === "EQ" || upper === "EQUALS") return "transaction.status EQ \"APPROVED\"";
  if (upper === "NE" || upper === "NEQ" || upper === "NOT_EQUALS") return "transaction.currency NEQ \"BRL\"";
  
  // Variações de GT/GTE/LT/LTE
  if (upper === "AMOUNT_GT") return "transaction.amount GT 10000";
  if (upper === "AMOUNT_GTE") return "transaction.amount GTE 5000";
  if (upper === "AMOUNT_LT") return "transaction.amount LT 50";
  if (upper === "AMOUNT_LTE") return "transaction.amount LTE 1000";
  if (upper === "SCORE_GT") return "risk.score GT 70";
  if (upper === "SCORE_GTE") return "risk.score GTE 50";
  if (upper === "SCORE_LT") return "risk.score LT 30";
  if (upper === "SCORE_LTE") return "risk.score LTE 40";
  if (upper === "AGE_GT") return "customer.age GT 65";
  if (upper === "AGE_GTE") return "customer.age GTE 18";
  if (upper === "AGE_LT") return "customer.age LT 18";
  if (upper === "AGE_LTE") return "customer.age LTE 25";
  
  // Agregações específicas
  if (upper === "COUNT_GTE") return "COUNT(transactions, last_24h, card_id) GTE 5";
  if (upper === "COUNT_LT") return "COUNT(logins, last_1h, user_id) LT 3";
  if (upper === "COUNT_LTE") return "COUNT(failed_attempts, last_15min, ip) LTE 5";
  if (upper === "SUM_GTE") return "SUM(transactions.amount, last_7d, wallet_id) GTE 50000";
  if (upper === "SUM_LT") return "SUM(refunds.amount, last_30d, merchant_id) LT 10000";
  if (upper === "SUM_LTE") return "SUM(withdrawals.amount, last_24h, account_id) LTE 3000";
  if (upper === "AVG_GT") return "AVG(transactions.amount, last_30d, customer_id) GT 500";
  if (upper === "AVG_GTE") return "AVG(order.value, last_7d, user_id) GTE 200";
  if (upper === "AVG_LT") return "AVG(purchase.amount, last_24h, session_id) LT 50";
  if (upper === "AVG_LTE") return "AVG(tip.amount, last_7d, driver_id) LTE 10";
  if (upper === "MAX_GT") return "MAX(transactions.amount, last_24h, customer_id) GT 5000";
  if (upper === "MAX_GTE") return "MAX(order.value, last_7d, merchant_id) GTE 10000";
  if (upper === "MIN_LT") return "MIN(transactions.amount, last_24h, card_id) LT 10";
  if (upper === "MIN_LTE") return "MIN(deposit.amount, last_30d, account_id) LTE 100";
  if (upper.includes("PERCENT")) return "PERCENT_CHANGE(transactions.amount, last_7d, customer_id) GT 200";
  if (upper.includes("VELOCITY")) return "VELOCITY(transactions, last_1h, device_id) GT 5";
  if (upper.includes("DISTINCT")) return "COUNT_DISTINCT(countries, last_24h, card_id) GT 3";
  
  // Operadores de lista específicos
  if (upper === "MCC_IN") return "merchant.mcc IN [\"5411\", \"5912\", \"5999\"]";
  if (upper === "MCC_NOT_IN") return "merchant.mcc NOT_IN [\"7995\", \"5967\", \"6051\"]";
  if (upper === "COUNTRY_IN") return "transaction.country IN [\"BR\", \"AR\", \"CL\", \"MX\"]";
  if (upper === "COUNTRY_NOT_IN") return "transaction.country NOT_IN [\"KP\", \"IR\", \"CU\", \"SY\"]";
  if (upper === "CHANNEL_IN") return "transaction.channel IN [\"APP\", \"WEB\", \"API\"]";
  if (upper === "STATUS_IN") return "transaction.status IN [\"PENDING\", \"REVIEW\", \"HOLD\"]";
  
  // Operadores de string específicos
  if (upper === "EMAIL_CONTAINS") return "customer.email CONTAINS \"@tempmail\"";
  if (upper === "NAME_CONTAINS") return "customer.name CONTAINS \"TEST\"";
  if (upper === "DESCRIPTION_CONTAINS") return "transaction.description CONTAINS \"REFUND\"";
  if (upper === "EMAIL_STARTS_WITH") return "customer.email STARTS_WITH \"test_\"";
  if (upper === "PHONE_STARTS_WITH") return "customer.phone STARTS_WITH \"+55\"";
  if (upper === "BIN_STARTS_WITH") return "card.bin STARTS_WITH \"411111\"";
  if (upper === "EMAIL_ENDS_WITH") return "customer.email ENDS_WITH \"@gmail.com\"";
  if (upper === "DOMAIN_ENDS_WITH") return "email.domain ENDS_WITH \".ru\"";
  if (upper.includes("REGEX") || upper.includes("MATCH")) return "customer.phone MATCHES_REGEX /^\\+55\\d{11}$/";
  
  // Operadores de data/tempo específicos
  if (upper === "DATE_BEFORE") return "transaction.date DATE_BEFORE \"2025-01-01\"";
  if (upper === "DATE_AFTER_OR_EQ") return "customer.created_at DATE_AFTER_OR_EQ \"2024-06-01\"";
  if (upper === "AGE_DAYS_GT") return "account.created_at AGE_DAYS_GT 30";
  if (upper === "AGE_DAYS_GTE") return "card.issued_at AGE_DAYS_GTE 7";
  if (upper === "AGE_HOURS_LT") return "session.started_at AGE_HOURS_LT 24";
  if (upper === "HOUR_BETWEEN") return "transaction.hour BETWEEN 9 AND 18";
  if (upper === "DAY_OF_WEEK_IN") return "transaction.day_of_week IN [\"SATURDAY\", \"SUNDAY\"]";
  if (upper === "WEEKEND") return "transaction.date IS_WEEKEND";
  if (upper === "BUSINESS_HOURS") return "transaction.time IS_BUSINESS_HOURS";
  
  // Operadores de device específicos
  if (upper.includes("DEVICE_NEW")) return "device.first_seen AGE_DAYS_LT 1";
  if (upper.includes("DEVICE_TRUST")) return "device.trust_score GT 0.8";
  if (upper.includes("DEVICE_FINGERPRINT")) return "device.fingerprint_match IS_TRUE";
  if (upper.includes("JAILBREAK") || upper.includes("ROOT")) return "device.is_rooted IS_TRUE";
  if (upper.includes("EMULATOR")) return "device.is_emulator IS_TRUE";
  if (upper.includes("VPN") || upper.includes("PROXY")) return "connection.is_vpn IS_TRUE";
  
  // Operadores de grafo específicos
  if (upper.includes("NEO4J") || upper.includes("GRAPH")) {
    if (upper.includes("LINK")) return "NEO4J_LINK_DEPTH(customer_id, card_id) GT 2";
    if (upper.includes("PATH")) return "GRAPH_SHORTEST_PATH(account_a, account_b) LTE 3";
    if (upper.includes("CLUSTER")) return "GRAPH_CLUSTER_SIZE(device_id) GT 10";
    return "GRAPH_CONNECTIONS(customer_id) GT 5";
  }
  
  // Operadores FATF/AML específicos
  if (upper.startsWith("FATF_")) {
    if (upper.includes("COUNTRY")) return "FATF_HIGH_RISK_COUNTRY(transaction.country) IS_TRUE";
    if (upper.includes("PEP")) return "FATF_IS_PEP(customer.name, customer.country) IS_TRUE";
    if (upper.includes("SANCTION")) return "FATF_SANCTION_HIT(customer.name) IS_TRUE";
    return "FATF_RISK_SCORE(transaction) GT 70";
  }
  
  // Operadores de compliance/regulatório
  if (upper.startsWith("SCA_")) return "SCA_EXEMPTION_ELIGIBLE(transaction) IS_TRUE";
  if (upper.startsWith("PSD")) return "PSD2_SCA_REQUIRED(transaction) IS_TRUE";
  if (upper.startsWith("DORA_")) return "DORA_INCIDENT_SEVERITY(event) GT 2";
  if (upper.startsWith("BSL_")) return "BSL_POLICY_VIOLATION(transaction) IS_TRUE";
  if (upper.startsWith("PLT_")) return "PLT_RATE_LIMIT_EXCEEDED(api_key) IS_TRUE";

  // Fallback por categoria (se nenhum específico acima)
  if (kind === "logical") return `(condition_a) ${upper} (condition_b)`;
  if (kind === "range") return `field ${upper} 100 AND 5000`;
  if (kind === "list") return `field ${upper} [\"value1\", \"value2\"]`;
  if (kind === "string") return `text_field ${upper} \"pattern\"`;
  if (kind === "null") return `optional_field ${upper}`;
  if (kind === "boolean") return `boolean_field ${upper}`;
  if (kind === "array") return `array_field ${upper} \"element\"`;
  if (kind === "datetime") return `date_field ${upper} \"2025-01-01\"`;
  if (kind === "aggregation") return `${upper}(events, last_24h, group_by) GT 10`;
  if (kind === "graph") return `${upper}(entity_a, entity_b) GT 2`;
  if (kind === "device") return `device.${name.toLowerCase()} GT 0.5`;
  if (kind === "identity") return `identity.${name.toLowerCase()} IS_TRUE`;
  if (kind === "merchant") return `merchant.${name.toLowerCase()} IN [\"value\"]`;
  if (kind === "platform") return `platform.${name.toLowerCase()} EQ \"value\"`;
  if (kind === "validation") return `validation.${name.toLowerCase()} IS_TRUE`;
  if (kind === "statistical") return `stats.${name.toLowerCase()} GT 2.0`;
  if (kind === "risk_pattern") return `${upper}(payload) GT threshold`;
  if (kind === "compare") {
    if (upper.endsWith("_GT") || upper === "GT") return `field ${upper.replace("_GT", "")} GT 1000`;
    if (upper.endsWith("_GTE") || upper === "GTE") return `field ${upper.replace("_GTE", "")} GTE 500`;
    if (upper.endsWith("_LT") || upper === "LT") return `field ${upper.replace("_LT", "")} LT 100`;
    if (upper.endsWith("_LTE") || upper === "LTE") return `field ${upper.replace("_LTE", "")} LTE 50`;
    if (upper.endsWith("_NEQ") || upper === "NEQ") return `field ${upper.replace("_NEQ", "")} NEQ \"value\"`;
    return `field ${upper} \"value\"`;
  }

  return `campo ${upper} valor`;
};

const deriveDidacticKit = (operator: Operator): DidacticKit => {
  const name = operator.name;
  const kind = classifyOperator(name);
  const explain = explainOperatorName(name);
  const baseResumo = operator.comment?.trim() ? operator.comment.trim() : derivePurpose(operator);

  const baseQuandoUsar: Record<OperatorKind, string[]> = {
    logical: [
      "Para combinar múltiplas condições na mesma regra.",
      "Para reduzir duplicação (evitar várias regras quase iguais).",
    ],
    compare: [
      "Quando você tem um número/valor e precisa comparar com um limite.",
      "Quando quer definir um mínimo/máximo objetivo (ex: valor > 5000).",
    ],
    range: ["Quando precisa validar se um valor está dentro (ou fora) de uma faixa.", "Quando quer legibilidade: faixa em uma única expressão."],
    list: [
      "Quando existe um conjunto conhecido de valores permitidos/proibidos.",
      "Quando você quer substituir vários OR/AND por uma lista.",
    ],
    string: [
      "Quando precisa identificar padrão em texto (e-mail, descrição, user-agent).",
      "Quando regras dependem de prefixo/sufixo/trecho.",
    ],
    null: ["Quando um campo pode vir ausente no payload.", "Quando ausência de dado é um sinal (ex: sem device_id)."],
    boolean: ["Quando o campo já é booleano (true/false).", "Quando quer legibilidade com IS_TRUE/IS_FALSE."],
    array: ["Quando o campo é uma lista (tags, itens, ids).", "Quando precisa validar conteúdo ou tamanho da lista."],
    datetime: ["Quando o tempo é parte do risco (madrugada, dias desde criação).", "Quando regras dependem de janelas e calendário."],
    aggregation: [
      "Quando a decisão depende do histórico (velocity, soma em 24h).",
      "Quando fraude tenta se esconder fragmentando valores (smurfing).",
    ],
    risk_pattern: [
      "Quando o operador representa um detector/padrão composto (anomaly, detection, pattern).",
      "Para capturar sinais avançados sem escrever tudo na mão.",
    ],
    graph: ["Quando o risco depende de relação entre entidades (conta↔dispositivo↔cartão).", "Para detectar redes e conexões indiretas."],
    device: [
      "Quando precisa avaliar a confiabilidade do dispositivo que está fazendo a transação.",
      "Quando quer detectar dispositivos adulterados (jailbreak, emuladores, bots).",
    ],
    identity: [
      "Quando precisa validar dados cadastrais do cliente.",
      "Quando quer verificar consistência de dados (CPF, e-mail, telefone).",
    ],
    merchant: [
      "Quando o risco depende do tipo de estabelecimento (MCC de alto risco).",
      "Quando precisa de regras específicas por categoria de merchant.",
    ],
    platform: [
      "Quando precisa garantir compliance regulatório (GDPR, DORA, eIDAS).",
      "Quando há requisitos específicos de plataforma a validar.",
    ],
    validation: [
      "Quando precisa checar listas de sanções, PEP ou adverse media.",
      "Quando a regra depende de verificações externas já realizadas.",
    ],
    statistical: [
      "Quando precisa detectar anomalias estatísticas (desvios, outliers).",
      "Quando quer usar machine learning ou scores calculados.",
    ],
    unknown: ["Quando você já conhece o operador e quer aplicá-lo diretamente.", "Para cenários específicos descritos pela área de negócio."]
  };

  const baseQuandoEvitar: Record<OperatorKind, string[]> = {
    logical: ["Quando uma condição simples resolve (evite overengineering)."],
    compare: ["Quando o dado não é numérico (use string/list/regex).", "Quando o limite deveria incluir igualdade e você escolheu GT/LT."] ,
    range: ["Quando a faixa é dinâmica e muda por segmento (talvez usar threshold adaptativo)."],
    list: ["Quando a lista cresce demais (prefira referência a cadastro/lookup se existir)."],
    string: ["Quando você pode usar igualdade exata (EQ) — mais preciso e mais barato."],
    null: ["Quando o campo existe mas vem vazio \"\" (use IS_EMPTY se disponível)."],
    boolean: ["Quando o campo não é booleano (não force)."],
    array: ["Quando o campo não é array (valide o payload)."],
    datetime: ["Quando horário/data está em timezone diferente (normalize antes)."],
    aggregation: ["Quando não há histórico suficiente (novos clientes podem gerar falsos positivos)."],
    risk_pattern: ["Quando você precisa de explicabilidade linha a linha (operadores compostos podem ser 'caixa preta')."],
    graph: ["Quando os dados de relacionamento não existem/estão incompletos (grafo vazio)."],
    device: ["Quando o device_id não está presente ou é inconsistente.", "Quando o dispositivo é um canal legítimo sem fingerprint (ex: API B2B)."],
    identity: ["Quando os dados já foram validados em etapa anterior.", "Quando quer velocidade e a validação é cara."],
    merchant: ["Quando o merchant não faz parte do risco (ex: transação interna).", "Quando MCC não está disponível no payload."],
    platform: ["Quando o requisito regulatório não se aplica à região/produto.", "Quando compliance é feito em camada separada."],
    validation: ["Quando a validação gera latência e não é crítica para a decisão.", "Quando o resultado da validação já está cacheado."],
    statistical: ["Quando o modelo não está calibrado para o segmento.", "Quando outliers legítimos são comuns (ex: VIPs com valores altos)."],
    unknown: ["Quando você não sabe o significado operacional: valide com a documentação do backend."]
  };

  const baseArmadilhas: Record<OperatorKind, string[]> = {
    logical: [
      "AND fica mais restritivo a cada condição; OR fica mais abrangente.",
      "Cuidado com precedência: use parênteses para deixar intenção explícita.",
    ],
    compare: [
      "GT/LT não incluem o limite; GTE/LTE incluem.",
      "Compare tipos compatíveis (número com número, texto com texto).",
    ],
    range: ["Entenda se a faixa inclui os limites.", "Faixas de horário podem atravessar meia-noite (22:00–06:00)."],
    list: ["Formato da lista importa (aspas para strings).", "Listas muito grandes dificultam manutenção."],
    string: ["Case sensitivity pode variar; confirme no motor.", "CONTAINS pode gerar falsos positivos se o trecho for muito genérico."],
    null: ["NULL é diferente de vazio \"\".", "Se o campo é opcional, cuidado para não bloquear usuários legítimos."],
    boolean: ["Não compare booleano como string (\"true\").", "Consistência de payload (true vs 1) depende do sistema."],
    array: ["Checar tamanho vs conteúdo são coisas diferentes.", "Arrays podem vir vazios; defina comportamento esperado."],
    datetime: ["Timezone e formato ISO.", "Regras de madrugada precisam ser testadas com casos que cruzam 00:00."],
    aggregation: [
      "Defina corretamente: janela (last_1h) e agrupamento (customer_id).",
      "Cuidado com duplicidade de eventos (replay).",
    ],
    risk_pattern: ["Operador pode depender de features/telemetria disponíveis.", "Tuning (limiares) é essencial para não explodir falsos positivos."],
    graph: ["Grafo precisa de identidade estável (IDs consistentes).", "Profundidade alta pode ser cara; comece baixo."],
    device: [
      "Fingerprint pode mudar após atualização do app/browser.",
      "Dispositivos legítimos podem aparecer como 'novos' após limpar cache.",
    ],
    identity: [
      "Dados podem ter formatos diferentes (CPF com/sem pontuação).",
      "E-mails temporários são comuns; não confie só em formato válido.",
    ],
    merchant: [
      "MCC pode ser genérico (5999 = 'outros').",
      "Mesmo MCC pode ter merchants de risco muito diferente.",
    ],
    platform: [
      "Requisitos regulatórios mudam; mantenha regras atualizadas.",
      "Região do cliente vs região do servidor podem divergir.",
    ],
    validation: [
      "Validações externas podem falhar/timeout; defina fallback.",
      "Resultados de validação podem ficar desatualizados rapidamente.",
    ],
    statistical: [
      "Modelos precisam de retreino periódico.",
      "Threshold fixo pode não funcionar para todos os segmentos.",
    ],
    unknown: ["Leia o nome do operador como uma frase e teste com 3 casos: passa, falha, borda (limite)."],
  };

  const camposSugeridos = defaultFieldHintsForKind(kind);
  const exemploDsl = guessDslForKind(name, kind);

  const payload = {
    transaction: {
      amount: 1500,
      country: "BR",
      channel: "APP",
      status: "PENDING",
      ip: "203.0.113.10",
      date: "2026-01-25",
      time: "22:30",
      device_id: "d_999",
    },
    customer: {
      id: "c_123",
      age: 22,
      created_at: "2026-01-20",
      email: "user@empresa.com",
      phone: null,
      is_vip: false,
      email_verified: false,
    },
    order: {
      tags: ["promocao", "vip"],
      items: [{ sku: "SKU-1" }, { sku: "SKU-2" }],
    },
    merchant: {
      mcc: "5411",
    },
    device: {
      fingerprint: "fp_xxx",
      user_agent: "Mozilla/5.0 ...",
    },
  };

  const casosDeTeste: TestCaseHint[] = [
    { scenario: "Caso normal (dado típico)", expected: "Regra deve se comportar conforme o comparador/lista/faixa" },
    { scenario: "Caso de borda (no limite)", expected: "Verifique GT vs GTE / LT vs LTE / inclusão de limites" },
    { scenario: "Caso inválido (tipo errado ou campo ausente)", expected: "Defina se a regra deve falhar, ignorar ou bloquear" },
  ];

  const relacionados = uniq(
    [
      kind === "compare" ? "BETWEEN" : null,
      kind === "range" ? "GT/GTE/LT/LTE" : null,
      kind === "list" ? "IN/NOT_IN" : null,
      kind === "string" ? "STARTS_WITH/ENDS_WITH/REGEX" : null,
      kind === "aggregation" ? "COUNT_GT/SUM_GT/AVG" : null,
      kind === "logical" ? "AND/OR/NOT" : null,
      name.includes("_GT") ? "_GTE" : null,
      name.includes("_LT") ? "_LTE" : null,
    ].filter(Boolean) as string[]
  );

  const modeloMentalByKind: Record<OperatorKind, string> = {
    logical: "Conectores de lógica (como Lego): você junta condições para formar uma regra.",
    compare: "Uma régua/balança: compara um valor com um limite.",
    range: "Uma faixa de preço/idade: verifica se está dentro ou fora do intervalo.",
    list: "Lista de convidados: o valor precisa estar (ou não estar) na lista.",
    string: "Ctrl+F do texto: procura trechos/padrões no conteúdo.",
    null: "Checklist de formulário: campo veio preenchido ou ficou em branco.",
    boolean: "Interruptor: ligado (true) ou desligado (false).",
    array: "Carrinho de compras: tem item X? quantos itens tem?",
    datetime: "Relógio/calendário: decide com base em quando aconteceu.",
    aggregation: "Extrato/resumo: olha o histórico e calcula contagem/soma.",
    risk_pattern: "Detector composto: avalia múltiplos sinais e retorna um resultado.",
    graph: "Mapa de conexões: segue relacionamentos e mede proximidade/rede.",
    device: "Identidade do aparelho: avalia se o dispositivo é confiável.",
    identity: "Checagem de documentos: valida dados pessoais do cliente.",
    merchant: "Perfil do estabelecimento: avalia risco do comerciante.",
    platform: "Checklist de compliance: garante conformidade regulatória.",
    validation: "Carimbo de aprovação: verifica se passou em checagens externas.",
    statistical: "Análise de dados: detecta anomalias e padrões estatísticos.",
    unknown: "Ferramenta especializada: use quando o nome/categoria descrevem o que você precisa.",
  };

  return {
    resumo: `${baseResumo} (leitura do nome: “${explain.leituraHumana}”)`,
    modeloMental: modeloMentalByKind[kind],
    quandoUsar: baseQuandoUsar[kind],
    quandoEvitar: baseQuandoEvitar[kind],
    armadilhas: baseArmadilhas[kind],
    camposSugeridos,
    exemploPayload: safeJsonStringify(payload),
    exemploDsl,
    casosDeTeste,
    relacionados,
  };
};

// ─────────────────────────────────────────────────────────────────────────────
// 📚 EXEMPLOS ULTRA-DIDÁTICOS NO ESTILO "HEAD FIRST"
// ─────────────────────────────────────────────────────────────────────────────

interface HeadFirstExample {
  // 📌 Metadados de documentação (para transparência/rigor)
  docLevel?: OperatorDocLevel;
  docConfidence?: OperatorDocConfidence;
  docWarnings?: string[];

  // 🎭 História do mundo real
  historia: string;
  personagem: string;
  
  // 🤔 Problema que resolve
  problema: string;
  
  // 💡 Analogia do dia a dia
  analogia: string;
  
  // 📋 Passo a passo detalhado
  passoAPasso: string[];
  
  // ⚠️ Antes (sem a regra) vs ✅ Depois (com a regra)
  antes: string;
  depois: string;
  
  // 💻 Sintaxe DSL com explicação linha a linha
  sintaxe: string;
  explicacaoSintaxe: string;
  
  // ❓ Pergunta comum (Não existem perguntas idiotas)
  perguntaComum: string;
  respostaPergunta: string;
  
  // 🎯 Dica de ouro
  dicaDeOuro: string;
  
  // ══════════════════════════════════════════════════════════════════════════
  // 📖 NOVOS CAMPOS PARA "AULA COMPLETA"
  // ══════════════════════════════════════════════════════════════════════════
  
  // 📝 Definição simples em 1 frase (para leigos)
  definicaoSimples?: string;
  
  // 🔧 Como funciona por dentro (mecânica)
  comoFunciona?: string;
  
  // 📊 Tabela de verdade ou comportamento (quando aplicável)
  tabelaVerdade?: string[][];
  
  // 🎯 Múltiplos exemplos práticos com diferentes cenários
  exemplosExtras?: Array<{
    titulo: string;
    cenario: string;
    codigo: string;
    resultado: string;
  }>;
  
  // ⚠️ Erros comuns que iniciantes cometem
  errosComuns?: string[];
  
  // 🔗 Operadores relacionados que você deveria conhecer
  operadoresRelacionados?: string[];
  
  // 📋 Checklist: "Antes de usar, verifique..."
  checklistUso?: string[];
  
  // 🧪 Mini exercício para praticar
  exercicio?: {
    pergunta: string;
    resposta: string;
  };
  
  // ══════════════════════════════════════════════════════════════════════════
  // 🏭 COMPORTAMENTO NO MOTOR DE REGRAS (O QUE ACONTECE QUANDO EXECUTA)
  // ══════════════════════════════════════════════════════════════════════════
  
  // 🔄 O que acontece quando a regra é avaliada (passo a passo do motor)
  comportamentoMotor?: {
    descricao: string;
    passos: string[];
    performance?: string;
    cuidados?: string[];
  };
  
  // 🎬 Situações REAIS do dia a dia onde usar este operador
  situacoesReais?: Array<{
    titulo: string;
    contexto: string;
    problema: string;
    solucao: string;
    impacto: string;
  }>;
  
  // 📊 Resultados possíveis quando a regra dispara/não dispara
  resultadosPossiveis?: {
    quandoDispara: string;
    quandoNaoDispara: string;
    acaoRecomendada?: string;
  };
  
  // 🔧 Como TESTAR esta regra antes de colocar em produção
  comoTestar?: string[];
}

type OperatorDocMeta = {
  level: OperatorDocLevel;
  confidence: OperatorDocConfidence;
  warnings: string[];
};

const isHeuristicHeavyOperator = (nameRaw: string) => {
  const name = nameRaw.toUpperCase();
  return (
    name.endsWith("_DETECTION") ||
    name.endsWith("_PATTERN") ||
    name.endsWith("_ANALYTICS") ||
    name.endsWith("_ANOMALY") ||
    name.endsWith("_TEST") ||
    name.includes("_TEST_") ||
    name.startsWith("ADAPTIVE_") ||
    name.includes("BEHAVIORAL") ||
    name.includes("BASELINE") ||
    name.includes("ML") ||
    name.includes("MODEL")
  );
};

const docMetaForOperator = (name: string): OperatorDocMeta => {
  const upper = name.toUpperCase();
  const hasManual = Boolean(HEAD_FIRST_EXAMPLES[upper]);
  const hasSpec = Boolean(OPERATOR_SPECS[upper]);

  if (hasManual) {
    return { level: "manual", confidence: "high", warnings: [] };
  }

  if (hasSpec) {
    return {
      level: "spec",
      confidence: "high",
      warnings: [],
    };
  }

  const warnings: string[] = [
    "Conteúdo gerado automaticamente (heurística).",
    "Exemplos podem ser ilustrativos; confirme a semântica no backend/motor.",
  ];

  if (isHeuristicHeavyOperator(upper)) {
    warnings.unshift("Este operador parece ser um detector/estatístico composto; a DSL exata pode não ser 1:1 com o nome.");
    return { level: "generated", confidence: "low", warnings };
  }

  return { level: "generated", confidence: "medium", warnings };
};

// Mapeamento completo de exemplos Head First
const HEAD_FIRST_EXAMPLES: Record<string, HeadFirstExample> = {
  // ══════════════════════════════════════════════════════════════════════════
  // OPERADORES LÓGICOS - A COLA QUE UNE TUDO
  // ══════════════════════════════════════════════════════════════════════════
  AND: {
    // BÁSICO
    historia: "Ana, analista de fraude do Banco Digital, precisa criar uma regra que só dispare quando DUAS coisas acontecem juntas: valor alto E país diferente. Uma só não basta.",
    personagem: "👩‍💼 Ana, Analista de Fraude",
    problema: "Como garantir que TODAS as condições sejam verdadeiras ao mesmo tempo?",
    analogia: "🚪 Pense em uma porta com DUAS fechaduras. Você só entra se tiver AMBAS as chaves. Se faltar uma, a porta não abre. O AND funciona assim: todas as condições precisam ser verdadeiras.",
    passoAPasso: [
      "1️⃣ Clique em 'Nova Condição' e selecione o operador AND",
      "2️⃣ Adicione a primeira condição filha (ex: valor > 1000)",
      "3️⃣ Adicione a segunda condição filha (ex: país != BR)",
      "4️⃣ O AND só retorna VERDADEIRO se AMBAS forem verdadeiras",
    ],
    antes: "❌ ANTES: Sem AND, você teria que criar regras separadas, e uma transação de R$5000 do Brasil dispararia a regra de valor alto mesmo sendo doméstica.",
    depois: "✅ DEPOIS: Com AND, a regra só dispara se o valor for alto E o país for diferente. Transações domésticas de alto valor passam tranquilas.",
    sintaxe: "(transaction.amount > 1000) AND (transaction.country != \"BR\")",
    explicacaoSintaxe: "📖 Leia assim: 'Se o valor for maior que 1000 E o país for diferente de BR, então dispare a regra'",
    perguntaComum: "Posso usar mais de duas condições no AND?",
    respostaPergunta: "Sim! Você pode encadear quantas quiser: (A) AND (B) AND (C). TODAS precisam ser verdadeiras.",
    dicaDeOuro: "💎 Use AND quando você quer ser RIGOROSO. Quanto mais condições no AND, mais específica (e restritiva) fica a regra.",
    
    // AULA COMPLETA
    definicaoSimples: "AND significa 'E'. Une duas ou mais condições e só retorna VERDADEIRO quando TODAS são verdadeiras.",
    comoFunciona: "O sistema avalia cada condição da esquerda para a direita. Se QUALQUER uma for falsa, para imediatamente e retorna FALSO (isso é chamado 'curto-circuito'). Só retorna VERDADEIRO se chegar ao final com todas verdadeiras.",
    tabelaVerdade: [
      ["Condição A", "Condição B", "A AND B"],
      ["✅ Verdadeiro", "✅ Verdadeiro", "✅ VERDADEIRO"],
      ["✅ Verdadeiro", "❌ Falso", "❌ FALSO"],
      ["❌ Falso", "✅ Verdadeiro", "❌ FALSO"],
      ["❌ Falso", "❌ Falso", "❌ FALSO"],
    ],
    exemplosExtras: [
      {
        titulo: "Transação internacional de alto valor",
        cenario: "Queremos detectar compras acima de R$5.000 vindas do exterior",
        codigo: "(amount GT 5000) AND (country NEQ \"BR\")",
        resultado: "Dispara apenas se valor > 5000 E país não for Brasil",
      },
      {
        titulo: "Cliente novo com valor alto",
        cenario: "Conta com menos de 7 dias fazendo compra > R$1.000",
        codigo: "(account_age_days LT 7) AND (amount GT 1000)",
        resultado: "Detecta possível fraude em conta recém-criada",
      },
      {
        titulo: "Tripla condição",
        cenario: "Madrugada + valor alto + primeiro uso do cartão",
        codigo: "(hour BETWEEN 0 AND 6) AND (amount GT 2000) AND (is_first_use EQ true)",
        resultado: "Cenário de altíssimo risco - todas as 3 devem ser verdadeiras",
      },
    ],
    errosComuns: [
      "❌ Confundir AND com OR: AND é restritivo (todas verdadeiras), OR é permissivo (basta uma)",
      "❌ Usar AND quando deveria usar OR: 'país = BR AND país = US' NUNCA será verdadeiro (um país não pode ser dois ao mesmo tempo!)",
      "❌ Esquecer parênteses: (A AND B) OR C é diferente de A AND (B OR C)",
    ],
    operadoresRelacionados: ["OR", "NOT", "NAND"],
    checklistUso: [
      "☐ Todas as condições precisam ser verdadeiras juntas?",
      "☐ Uma condição falsa deve bloquear o resultado?",
      "☐ Os parênteses estão corretos para a precedência desejada?",
    ],
    exercicio: {
      pergunta: "Crie uma regra AND que detecte: valor > R$3.000 E cartão não verificado E horário entre 22h e 6h",
      resposta: "(amount GT 3000) AND (card_verified EQ false) AND (hour BETWEEN 22 AND 6)",
    },
    
    // 🏭 COMPORTAMENTO NO MOTOR DE REGRAS
    comportamentoMotor: {
      descricao: "Quando o motor encontra um AND, ele avalia as condições da esquerda para a direita com 'curto-circuito': se uma falha, para imediatamente.",
      passos: [
        "1️⃣ Motor recebe a transação no payload JSON",
        "2️⃣ Identifica que a regra usa AND com N condições",
        "3️⃣ Avalia a primeira condição (ex: amount GT 5000)",
        "4️⃣ Se FALSA → para imediatamente, retorna FALSO (não avalia as outras)",
        "5️⃣ Se VERDADEIRA → avalia a próxima condição",
        "6️⃣ Repete até encontrar FALSA ou chegar ao fim",
        "7️⃣ Se todas foram verdadeiras → retorna VERDADEIRO e dispara a regra",
      ],
      performance: "⚡ O curto-circuito torna AND eficiente: coloque a condição mais provável de ser FALSA primeiro para economizar processamento",
      cuidados: [
        "A ordem das condições pode afetar performance",
        "Condições com chamadas externas (APIs) devem vir por último",
        "Se uma condição depende de outra, garanta a ordem correta",
      ],
    },
    
    // 🎬 SITUAÇÕES REAIS
    situacoesReais: [
      {
        titulo: "Black Friday - Compra fora do padrão",
        contexto: "Durante a Black Friday, um cliente VIP (3 anos de conta) faz uma compra de R$15.000 em eletrônicos",
        problema: "Sem AND, você bloquearia TODAS as compras acima de R$10.000, irritando clientes VIPs legítimos",
        solucao: "(amount GT 10000) AND (account_age_days LT 30) - só bloqueia se valor alto E conta nova",
        impacto: "🎯 Reduz falsos positivos em 60%: VIPs compram à vontade, contas novas são monitoradas",
      },
      {
        titulo: "Cartão clonado - Múltiplos sinais",
        contexto: "Fraudador rouba dados do cartão e tenta compra de madrugada, valor alto, primeiro uso online",
        problema: "Cada sinal isolado pode ser legítimo. Juntos, são alarmantes.",
        solucao: "(hour BETWEEN 1 AND 5) AND (amount GT 3000) AND (is_first_online_purchase EQ true)",
        impacto: "🔒 Detecta 85% dos cartões clonados com combinação de fatores de risco",
      },
      {
        titulo: "Compliance BACEN - PIX noturno",
        contexto: "Regulação exige limite de R$1.000 para PIX noturno (20h-6h)",
        problema: "Você precisa aplicar limite APENAS no horário noturno E apenas para PIX",
        solucao: "(channel EQ \"PIX\") AND (hour BETWEEN 20 AND 6) AND (amount GT 1000)",
        impacto: "📋 Conformidade regulatória 100%: bloqueia PIX noturno acima do limite",
      },
    ],
    
    // 📊 RESULTADOS POSSÍVEIS
    resultadosPossiveis: {
      quandoDispara: "🚨 A transação é marcada para AÇÃO (bloqueio, análise manual, score aumentado, etc.) - TODAS as condições foram atendidas",
      quandoNaoDispara: "✅ A transação PASSA normalmente - pelo menos UMA condição não foi atendida",
      acaoRecomendada: "Configure a ação da regra: BLOCK (bloquear), REVIEW (análise manual), FLAG (marcar), ou SCORE (adicionar pontos de risco)",
    },
    
    // 🔧 COMO TESTAR
    comoTestar: [
      "📝 Teste 1 (Deve disparar): Envie transação com TODAS as condições verdadeiras",
      "  💡 Exemplo payload:",
      "  { amount: 6000, country: 'US', hour: 3 }",
      "  📊 Resultado esperado: triggeredRules contém 'AND' ✅",
      "",
      "📝 Teste 2 (Não deve disparar): Envie com apenas UMA condição falsa",
      "  💡 Exemplo payload:",
      "  { amount: 6000, country: 'BR', hour: 3 } ← país = BR (falso)",
      "  📊 Resultado esperado: triggeredRules NÃO contém 'AND' ❌",
      "",
      "📝 Teste 3 (Borda): Teste valores exatamente no limite",
      "  💡 Exemplo: amount = 5000 quando regra é (amount GT 5000)",
      "  📊 GT exclui igualdade → não dispara ❌",
      "",
      "📝 Teste 4 (Dados ausentes): O que acontece se campo vier null?",
      "  💡 Payload: { amount: null, country: 'US' }",
      "  📊 amount null geralmente = condição FALSA → AND retorna false",
      "",
      "📝 Teste 5 (Performance): Com 5+ condições, meça o tempo",
      "  ⏱️ Tempo esperado: < 1ms (curto-circuito otimiza)",
      "  📋 Log do motor: 'RuleEngine: AND evaluated in 0.3ms'",
    ],
  },

  OR: {
    historia: "Carlos, do time de risco, quer bloquear transações que venham do APP ou do WEB em horário suspeito. Basta vir de UM dos canais para disparar.",
    personagem: "👨‍💻 Carlos, Analista de Risco",
    problema: "Como disparar uma regra quando PELO MENOS UMA condição é verdadeira?",
    analogia: "🚪 Pense em uma sala com DUAS portas. Você entra se QUALQUER uma estiver aberta. O OR funciona assim: basta UMA condição ser verdadeira.",
    passoAPasso: [
      "1️⃣ Clique em 'Nova Condição' e selecione o operador OR",
      "2️⃣ Adicione a primeira opção (ex: canal = APP)",
      "3️⃣ Adicione a segunda opção (ex: canal = WEB)",
      "4️⃣ O OR retorna VERDADEIRO se QUALQUER uma for verdadeira",
    ],
    antes: "❌ ANTES: Sem OR, você teria que criar duas regras separadas, uma para APP e outra para WEB, duplicando trabalho.",
    depois: "✅ DEPOIS: Com OR, uma única regra captura AMBOS os cenários. Veio do APP? Dispara. Veio do WEB? Também dispara.",
    sintaxe: "(transaction.channel = \"APP\") OR (transaction.channel = \"WEB\")",
    explicacaoSintaxe: "📖 Leia assim: 'Se o canal for APP OU o canal for WEB, então dispare a regra'",
    perguntaComum: "Qual a diferença entre OR e AND?",
    respostaPergunta: "AND = TODAS verdadeiras (mais restritivo). OR = PELO MENOS UMA verdadeira (mais abrangente).",
    dicaDeOuro: "💎 Use OR quando você quer capturar MÚLTIPLOS cenários com uma única regra. É como uma rede de pesca maior.",
    
    // AULA COMPLETA
    definicaoSimples: "OR retorna VERDADEIRO se PELO MENOS UMA das condições for verdadeira. É o 'OU' inclusivo - basta uma!",
    comoFunciona: "O sistema avalia cada condição da esquerda para direita. No momento em que encontra UMA verdadeira, já retorna VERDADEIRO sem precisar verificar as outras (short-circuit evaluation). Só retorna FALSO se TODAS forem falsas.",
    tabelaVerdade: [
      ["Condição A", "Condição B", "A OR B", "Explicação"],
      ["❌ FALSO", "❌ FALSO", "❌ FALSO", "Nenhuma verdadeira = FALSO"],
      ["❌ FALSO", "✅ VERDADEIRO", "✅ VERDADEIRO", "Uma verdadeira basta!"],
      ["✅ VERDADEIRO", "❌ FALSO", "✅ VERDADEIRO", "Uma verdadeira basta!"],
      ["✅ VERDADEIRO", "✅ VERDADEIRO", "✅ VERDADEIRO", "Ambas verdadeiras = ainda VERDADEIRO"],
    ],
    exemplosExtras: [
      {
        titulo: "Canais de alto risco",
        cenario: "Transações do APP ou WEB merecem atenção extra",
        codigo: "(channel EQ \"APP\") OR (channel EQ \"WEB\")",
        resultado: "APP = dispara, WEB = dispara, POS = não dispara",
      },
      {
        titulo: "Valores extremos",
        cenario: "Alertar valores muito baixos OU muito altos",
        codigo: "(amount LT 10) OR (amount GT 10000)",
        resultado: "R$5 = dispara, R$500 = não, R$50.000 = dispara",
      },
      {
        titulo: "Países de risco",
        cenario: "Monitorar transações de países específicos",
        codigo: "(country EQ \"NG\") OR (country EQ \"RU\") OR (country EQ \"UA\")",
        resultado: "Nigéria, Rússia ou Ucrânia = dispara",
      },
    ],
    errosComuns: [
      "❌ Confundir OR com AND: OR é MENOS restritivo (captura MAIS), AND é MAIS restritivo",
      "❌ Usar OR quando deveria usar IN: channel IN [\"APP\", \"WEB\"] é mais limpo que múltiplos OR",
      "❌ Esquecer parênteses: (A OR B) AND C é diferente de A OR (B AND C)",
    ],
    operadoresRelacionados: ["AND", "NOT", "XOR", "NOR", "IN"],
    checklistUso: [
      "☐ Você quer capturar MÚLTIPLOS cenários alternativos?",
      "☐ Basta UMA condição ser verdadeira para disparar?",
      "☐ Os parênteses estão corretos para precedência?",
    ],
    exercicio: {
      pergunta: "Crie uma regra que dispare para transações do Brasil (BR) OU Argentina (AR)",
      resposta: "(country EQ \"BR\") OR (country EQ \"AR\")",
    },
    
    // 🏭 COMPORTAMENTO NO MOTOR DE REGRAS
    comportamentoMotor: {
      descricao: "O motor avalia da esquerda para a direita com 'curto-circuito': assim que encontra UMA verdadeira, para e retorna VERDADEIRO.",
      passos: [
        "1️⃣ Motor recebe a transação e identifica regra OR",
        "2️⃣ Avalia a primeira condição",
        "3️⃣ Se VERDADEIRA → para imediatamente, retorna VERDADEIRO",
        "4️⃣ Se FALSA → avalia a próxima condição",
        "5️⃣ Repete até encontrar VERDADEIRA ou acabarem as condições",
        "6️⃣ Se TODAS foram falsas → retorna FALSO",
      ],
      performance: "⚡ Coloque a condição mais provável de ser VERDADEIRA primeiro para economizar processamento",
      cuidados: [
        "OR captura MAIS transações que AND - cuidado com falsos positivos",
        "Múltiplos OR pode ser substituído por IN para melhor legibilidade",
      ],
    },
    
    // 🎬 SITUAÇÕES REAIS
    situacoesReais: [
      {
        titulo: "Lista de países de alto risco FATF",
        contexto: "Compliance precisa monitorar transações de países na lista FATF (Coreia do Norte, Irã, Myanmar...)",
        problema: "São 20+ países. Criar 20 regras separadas é impraticável.",
        solucao: "(country EQ \"KP\") OR (country EQ \"IR\") OR (country EQ \"MM\") OR ... [ou melhor: country IN lista_fatf]",
        impacto: "📋 Uma única regra cobre todos os países de risco, fácil de manter",
      },
      {
        titulo: "Detecção de anomalia em valores",
        contexto: "Transações muito pequenas (teste de cartão) OU muito grandes (fraude) são suspeitas",
        problema: "Você quer capturar os DOIS extremos com uma regra",
        solucao: "(amount LT 10) OR (amount GT 50000)",
        impacto: "🎯 Captura testes de cartão (R$1-R$9) E fraudes grandes (>R$50k) na mesma regra",
      },
      {
        titulo: "Múltiplos canais de risco",
        contexto: "Transações de APP mobile ou API externa têm risco diferente de POS físico",
        problema: "Quer aplicar regras específicas para canais digitais",
        solucao: "(channel EQ \"APP\") OR (channel EQ \"API\") OR (channel EQ \"WEB\")",
        impacto: "🔒 Regras de segurança digital aplicadas apenas onde necessário",
      },
    ],
    
    // 📊 RESULTADOS POSSÍVEIS
    resultadosPossiveis: {
      quandoDispara: "🚨 PELO MENOS UMA condição foi atendida - a transação é capturada pela regra",
      quandoNaoDispara: "✅ NENHUMA das condições foi atendida - transação passa",
      acaoRecomendada: "Use OR para criar 'redes amplas' de captura. Combine com AND para refinar: (condição_ampla_OR) AND (condição_específica)",
    },
    
    // 🔧 COMO TESTAR
    comoTestar: [
      "📝 Teste 1: Envie transação que atenda a PRIMEIRA condição apenas",
      "📝 Teste 2: Envie transação que atenda a ÚLTIMA condição apenas",
      "📝 Teste 3: Envie transação que não atenda NENHUMA (deve passar)",
      "📝 Teste 4: Envie transação que atenda TODAS (deve disparar, mas não duplicar)",
      "📝 Teste 5: Verifique se a ordem das condições afeta o resultado (não deveria)",
    ],
  },

  NOT: {
    historia: "Beatriz precisa criar uma regra que dispare para TODOS os clientes, EXCETO os VIPs. Ela quer inverter a lógica.",
    personagem: "👩‍🔬 Beatriz, Cientista de Dados",
    problema: "Como inverter uma condição? Como dizer 'dispare se NÃO for VIP'?",
    analogia: "🔄 Pense no NOT como um interruptor que inverte tudo. Se a luz está acesa, o NOT apaga. Se está apagada, o NOT acende. Ele transforma VERDADEIRO em FALSO e vice-versa.",
    passoAPasso: [
      "1️⃣ Clique em 'Nova Condição' e selecione o operador NOT",
      "2️⃣ Adicione a condição que você quer NEGAR (ex: cliente é VIP)",
      "3️⃣ O NOT inverte: se era verdadeiro, vira falso",
      "4️⃣ Resultado: a regra dispara para quem NÃO é VIP",
    ],
    antes: "❌ ANTES: Você teria que listar todos os tipos de cliente que NÃO são VIP, um por um.",
    depois: "✅ DEPOIS: Com NOT, você simplesmente diz 'não é VIP' e pronto. Simples e elegante.",
    sintaxe: "NOT (customer.is_vip = true)",
    explicacaoSintaxe: "📖 Leia assim: 'Se o cliente NÃO for VIP, então dispare a regra'",
    perguntaComum: "Posso usar NOT com AND e OR?",
    respostaPergunta: "Sim! NOT (A AND B) significa 'não é verdade que A e B são ambos verdadeiros'. É muito poderoso!",
    dicaDeOuro: "💎 Use NOT quando é mais fácil descrever o que você NÃO quer do que o que você quer.",
  },

  // ══════════════════════════════════════════════════════════════════════════
  // COMPARAÇÕES - O BÁSICO QUE VOCÊ USA TODOS OS DIAS
  // ══════════════════════════════════════════════════════════════════════════
  EQ: {
    historia: "Daniel precisa criar uma regra que só dispare quando o status da transação for exatamente 'PENDING'. Nem 'APPROVED', nem 'DECLINED' - apenas 'PENDING'.",
    personagem: "👨‍💼 Daniel, Gerente de Operações",
    problema: "Como verificar se um campo tem EXATAMENTE um valor específico?",
    analogia: "🎯 Pense em um cadeado de combinação. Só abre com a combinação EXATA. 1234 abre, mas 1235 não. O EQ funciona assim: precisa ser IGUAL, caractere por caractere.",
    passoAPasso: [
      "1️⃣ Selecione o campo que você quer verificar (ex: transaction.status)",
      "2️⃣ Escolha o operador EQ (igual)",
      "3️⃣ Digite o valor esperado entre aspas (ex: \"PENDING\")",
      "4️⃣ A regra só dispara se o valor for EXATAMENTE igual",
    ],
    antes: "❌ ANTES: Sem EQ, você não consegue filtrar por um valor específico. A regra dispara para qualquer status.",
    depois: "✅ DEPOIS: Com EQ, você captura APENAS as transações pendentes. Precisão cirúrgica.",
    sintaxe: "transaction.status EQ \"PENDING\"",
    explicacaoSintaxe: "📖 Leia assim: 'Se o status da transação for IGUAL a PENDING, então dispare'",
    perguntaComum: "EQ diferencia maiúsculas de minúsculas?",
    respostaPergunta: "Sim! 'PENDING' é diferente de 'pending'. Sempre verifique como os dados chegam no payload.",
    dicaDeOuro: "💎 Sempre use aspas para textos: \"PENDING\". Para números, não precisa: amount EQ 100.",
    
    // AULA COMPLETA
    definicaoSimples: "EQ (equals) verifica se um valor é EXATAMENTE igual a outro. É o operador mais básico e mais usado!",
    comoFunciona: "O sistema compara o valor do campo com o valor especificado, caractere por caractere para textos ou bit por bit para números. Só retorna VERDADEIRO se forem IDÊNTICOS. Um espaço a mais, uma letra diferente = FALSO.",
    tabelaVerdade: [
      ["Valor do Campo", "Comparar com", "EQ", "Explicação"],
      ["\"PENDING\"", "\"PENDING\"", "✅ VERDADEIRO", "Textos idênticos"],
      ["\"PENDING\"", "\"pending\"", "❌ FALSO", "Maiúscula ≠ minúscula"],
      ["\"PENDING \"", "\"PENDING\"", "❌ FALSO", "Espaço extra no fim!"],
      ["100", "100", "✅ VERDADEIRO", "Números iguais"],
      ["100.0", "100", "✅ VERDADEIRO*", "Geralmente considera iguais"],
    ],
    exemplosExtras: [
      {
        titulo: "Filtrar por status",
        cenario: "Processar apenas transações pendentes",
        codigo: "status EQ \"PENDING\"",
        resultado: "PENDING = captura, APPROVED = ignora, DECLINED = ignora",
      },
      {
        titulo: "Bandeira específica",
        cenario: "Regras especiais para cartões Visa",
        codigo: "card_brand EQ \"VISA\"",
        resultado: "VISA = captura, MASTERCARD = ignora",
      },
      {
        titulo: "Valor exato",
        cenario: "Detectar transações de teste (valor R$1)",
        codigo: "amount EQ 1",
        resultado: "R$1 exato = captura (provável teste)",
      },
    ],
    errosComuns: [
      "❌ Esquecer case-sensitivity: 'VISA' ≠ 'visa' ≠ 'Visa'",
      "❌ Não usar aspas para texto: status EQ PENDING está errado, use \"PENDING\"",
      "❌ Usar EQ para faixas: se quer 'acima de 100', use GT, não EQ",
      "❌ Comparar tipos diferentes: \"100\" (texto) ≠ 100 (número)",
    ],
    operadoresRelacionados: ["NEQ", "IN", "CONTAINS", "IS_TRUE", "IS_FALSE"],
    checklistUso: [
      "☐ Você quer um valor EXATO (não faixa, não contém)?",
      "☐ O case (maiúscula/minúscula) está correto?",
      "☐ Está usando aspas para texto?",
      "☐ O tipo de dado está correto (texto vs número)?",
    ],
    exercicio: {
      pergunta: "Crie uma regra que capture transações com status exatamente igual a APPROVED",
      resposta: "status EQ \"APPROVED\"",
    },
  },

  NEQ: {
    historia: "Elena quer uma regra que dispare para TODOS os países, EXCETO Brasil. Ela não quer listar 194 países - só excluir um.",
    personagem: "👩‍💼 Elena, Compliance Officer",
    problema: "Como dizer 'qualquer valor MENOS este'?",
    analogia: "🚫 Pense em uma festa onde TODOS podem entrar, EXCETO uma pessoa específica. O NEQ é o segurança que barra apenas aquele um convidado.",
    passoAPasso: [
      "1️⃣ Selecione o campo (ex: transaction.country)",
      "2️⃣ Escolha o operador NEQ (diferente, não igual)",
      "3️⃣ Digite o valor a EXCLUIR (ex: \"BR\")",
      "4️⃣ A regra dispara para QUALQUER valor diferente de BR",
    ],
    antes: "❌ ANTES: Você teria que listar todos os 194 países do mundo usando OR.",
    depois: "✅ DEPOIS: Com NEQ, uma linha resolve: país diferente de BR. Elegante!",
    sintaxe: "transaction.country NEQ \"BR\"",
    explicacaoSintaxe: "📖 Leia assim: 'Se o país da transação for DIFERENTE de BR, então dispare'",
    perguntaComum: "Qual a diferença entre NEQ e NOT EQ?",
    respostaPergunta: "São equivalentes! NEQ é um atalho para NOT (campo EQ valor). Use o que preferir.",
    dicaDeOuro: "💎 NEQ é perfeito quando você quer excluir UMA exceção de uma regra ampla.",
  },

  GT: {
    historia: "Fernando precisa alertar sobre transações de alto valor. Qualquer compra ACIMA de R$5.000 deve ser analisada.",
    personagem: "👨‍🔍 Fernando, Investigador de Fraudes",
    problema: "Como verificar se um número é MAIOR que um limite?",
    analogia: "📏 Pense na placa 'Altura mínima para brinquedo: 1,20m'. Se você tem 1,21m, pode entrar. Se tem 1,20m exato, NÃO pode (precisa ser MAIOR, não igual). O GT funciona assim.",
    passoAPasso: [
      "1️⃣ Selecione o campo numérico (ex: transaction.amount)",
      "2️⃣ Escolha o operador GT (greater than = maior que)",
      "3️⃣ Digite o limite (ex: 5000)",
      "4️⃣ A regra dispara para valores 5001, 5002... mas NÃO para 5000",
    ],
    antes: "❌ ANTES: Sem GT, você não consegue definir um limite mínimo. Todas as transações disparariam.",
    depois: "✅ DEPOIS: Com GT, você captura apenas transações de alto valor. R$4.999 passa, R$5.001 dispara.",
    sintaxe: "transaction.amount GT 5000",
    explicacaoSintaxe: "📖 Leia assim: 'Se o valor for MAIOR QUE 5000, então dispare'",
    perguntaComum: "E se eu quiser incluir o valor 5000 também?",
    respostaPergunta: "Use GTE (maior ou igual). GT = maior que (exclui o limite). GTE = maior ou igual (inclui o limite).",
    dicaDeOuro: "💎 Lembre: GT NÃO inclui o valor do limite. Se quer incluir, use GTE.",
    
    // AULA COMPLETA
    definicaoSimples: "GT significa 'Greater Than' (Maior Que). Compara um número e retorna VERDADEIRO se for MAIOR que o limite especificado.",
    comoFunciona: "O sistema pega o valor do campo (ex: amount = 5001), compara com o limite (5000), e verifica se é ESTRITAMENTE maior. 5001 > 5000? Sim! Retorna verdadeiro. 5000 > 5000? Não! São iguais, não é maior.",
    tabelaVerdade: [
      ["Valor do Campo", "Limite", "Resultado GT", "Explicação Visual"],
      ["5001", "5000", "✅ VERDADEIRO", "5001 > 5000 (passou por R$ 1)"],
      ["5000", "5000", "❌ FALSO", "5000 = 5000 (igual, NÃO é maior!)"],
      ["4999", "5000", "❌ FALSO", "4999 < 5000 (faltou R$ 1)"],
      ["10000", "5000", "✅ VERDADEIRO", "10000 > 5000 (passou MUITO)"],
      ["5000.01", "5000", "✅ VERDADEIRO", "passou por 1 centavo!"],
    ],
    exemplosExtras: [
      {
        titulo: "Alerta de alto valor - EXEMPLO ULTRA DIDÁTICO",
        cenario: "Transações acima de R$10.000 precisam de aprovação extra",
        codigo: "transaction.amount GT 10000",
        resultado: `🧪 TESTES PRÁTICOS:
┌─────────────┬──────────┬────────────┐
│ Valor (R$)  │ GT 10000 │ Resultado  │
├─────────────┼──────────┼────────────┤
│ 10,001.00   │    ✅    │ DISPARA    │
│ 10,000.00   │    ❌    │ não dispara│
│  9,999.99   │    ❌    │ não dispara│
│ 50,000.00   │    ✅    │ DISPARA    │
└─────────────┴──────────┴────────────┘`,
      },
      {
        titulo: "Score de risco elevado",
        cenario: "Alertar quando score de risco passar de 80 pontos",
        codigo: "risk_score GT 80",
        resultado: "Score 81 dispara, score 80 não dispara",
      },
      {
        titulo: "Combinado com AND",
        cenario: "Valor alto + país estrangeiro",
        codigo: "(amount GT 5000) AND (country NEQ \"BR\")",
        resultado: "Só dispara se AMBAS condições forem verdadeiras",
      },
    ],
    errosComuns: [
      "❌ Confundir GT com GTE: GT exclui o limite! Se você quer 'a partir de 5000', use GTE",
      "❌ Usar GT com texto: GT é para números! Para texto, use outros operadores",
      "❌ Esquecer que 5000 GT 5000 é FALSO: são iguais, não é maior",
    ],
    operadoresRelacionados: ["GTE", "LT", "LTE", "BETWEEN", "EQ"],
    checklistUso: [
      "☐ O campo é numérico? (GT só funciona com números)",
      "☐ Você quer EXCLUIR o limite? (se não, use GTE)",
      "☐ O limite está na unidade correta? (centavos vs reais)",
    ],
    exercicio: {
      pergunta: "Crie uma regra que alerte transações ACIMA de R$15.000 (R$15.000 não deve alertar)",
      resposta: "transaction.amount GT 15000",
    },
  },

  GTE: {
    historia: "Gabriela define que clientes precisam ter no mínimo 18 anos. Quem tem 18 pode, quem tem 17 não pode.",
    personagem: "👩‍⚖️ Gabriela, Jurídico",
    problema: "Como verificar se um número é MAIOR OU IGUAL a um limite?",
    analogia: "🎂 Pense na maioridade: 18 anos ou mais. Se você tem exatamente 18, já pode. O GTE inclui o limite.",
    passoAPasso: [
      "1️⃣ Selecione o campo numérico (ex: customer.age)",
      "2️⃣ Escolha o operador GTE (greater than or equal = maior ou igual)",
      "3️⃣ Digite o limite mínimo (ex: 18)",
      "4️⃣ A regra dispara para 18, 19, 20... inclui o 18!",
    ],
    antes: "❌ ANTES: Se usasse GT 18, um cliente de exatamente 18 anos seria barrado incorretamente.",
    depois: "✅ DEPOIS: Com GTE, quem tem 18 anos passa. É o 'maior ou igual' que você precisa.",
    sintaxe: "customer.age GTE 18",
    explicacaoSintaxe: "📖 Leia assim: 'Se a idade for MAIOR OU IGUAL a 18, então permita'",
    perguntaComum: "Quando usar GT vs GTE?",
    respostaPergunta: "GT = 'acima de' (exclui o limite). GTE = 'a partir de' (inclui o limite). Pense no contexto!",
    dicaDeOuro: "💎 Na dúvida, pergunte: o limite deve ser incluído? Se sim, use GTE. Se não, use GT.",
  },

  LT: {
    historia: "Hugo quer identificar micro-transações suspeitas. Valores ABAIXO de R$10 podem ser testes de cartão roubado.",
    personagem: "👨‍🔬 Hugo, Cientista de Fraude",
    problema: "Como verificar se um número é MENOR que um limite?",
    analogia: "🌡️ Pense em um termômetro: 'alerta de hipotermia abaixo de 35°C'. Se a temperatura é 34.9°C, dispara. Se é 35°C exato, não dispara.",
    passoAPasso: [
      "1️⃣ Selecione o campo numérico (ex: transaction.amount)",
      "2️⃣ Escolha o operador LT (less than = menor que)",
      "3️⃣ Digite o limite máximo (ex: 10)",
      "4️⃣ A regra dispara para valores 9, 5, 1... mas NÃO para 10",
    ],
    antes: "❌ ANTES: Sem LT, você não consegue definir um limite máximo. Micro-transações passariam despercebidas.",
    depois: "✅ DEPOIS: Com LT, você captura transações suspeitas de baixo valor. R$9.99 dispara, R$10.00 passa.",
    sintaxe: "transaction.amount LT 10",
    explicacaoSintaxe: "📖 Leia assim: 'Se o valor for MENOR QUE 10, então dispare'",
    perguntaComum: "Posso combinar LT com GT para criar uma faixa?",
    respostaPergunta: "Sim! (amount GT 10) AND (amount LT 100) captura valores entre 10 e 100. Ou use BETWEEN, que é mais elegante.",
    dicaDeOuro: "💎 LT é ótimo para detectar valores anormalmente baixos, como testes de cartão ou erros de digitação.",
  },

  LTE: {
    historia: "Isabela precisa limitar cupons de desconto: até R$50 de desconto é permitido. R$50 exatos também vale.",
    personagem: "👩‍💼 Isabela, Gerente de Promoções",
    problema: "Como verificar se um número é MENOR OU IGUAL a um limite?",
    analogia: "🎟️ Pense em 'desconto máximo de R$50'. Se o desconto é exatamente R$50, é válido. O LTE inclui o limite.",
    passoAPasso: [
      "1️⃣ Selecione o campo numérico (ex: discount.amount)",
      "2️⃣ Escolha o operador LTE (less than or equal = menor ou igual)",
      "3️⃣ Digite o limite máximo (ex: 50)",
      "4️⃣ A regra aceita 50, 49, 48... inclui o 50!",
    ],
    antes: "❌ ANTES: Se usasse LT 50, um desconto de exatamente R$50 seria rejeitado incorretamente.",
    depois: "✅ DEPOIS: Com LTE, desconto de R$50 é aceito. É o 'até' que você precisa.",
    sintaxe: "discount.amount LTE 50",
    explicacaoSintaxe: "📖 Leia assim: 'Se o desconto for MENOR OU IGUAL a 50, então permita'",
    perguntaComum: "Quando usar LT vs LTE?",
    respostaPergunta: "LT = 'abaixo de' (exclui o limite). LTE = 'até' (inclui o limite).",
    dicaDeOuro: "💎 Use LTE quando o limite é válido. Exemplo: 'idade até 17' = LTE 17 (17 anos é válido).",
  },

  // ══════════════════════════════════════════════════════════════════════════
  // FAIXAS (RANGE) - QUANDO VOCÊ QUER UM INTERVALO
  // ══════════════════════════════════════════════════════════════════════════
  BETWEEN: {
    historia: "João quer criar uma regra para transações de 'valor médio': entre R$100 e R$5.000. Nem muito baixo, nem muito alto.",
    personagem: "👨‍💼 João, Gerente de Risco",
    problema: "Como verificar se um valor está DENTRO de uma faixa?",
    analogia: "🎯 Pense em uma faixa etária: '18 a 65 anos'. Se você tem 18 ou 65, está dentro. Se tem 17 ou 66, está fora. O BETWEEN inclui os limites.",
    passoAPasso: [
      "1️⃣ Selecione o campo numérico (ex: transaction.amount)",
      "2️⃣ Escolha o operador BETWEEN",
      "3️⃣ Digite o limite inferior (ex: 100)",
      "4️⃣ Digite o limite superior (ex: 5000)",
      "5️⃣ A regra dispara para 100, 101... 4999, 5000 (inclui ambos limites)",
    ],
    antes: "❌ ANTES: Você teria que escrever (amount GTE 100) AND (amount LTE 5000). Mais verboso.",
    depois: "✅ DEPOIS: Com BETWEEN, uma linha resolve: amount BETWEEN 100 AND 5000. Limpo!",
    sintaxe: "transaction.amount BETWEEN 100 AND 5000",
    explicacaoSintaxe: "📖 Leia assim: 'Se o valor estiver ENTRE 100 E 5000 (inclusive), então dispare'",
    perguntaComum: "BETWEEN inclui os limites?",
    respostaPergunta: "Sim! BETWEEN 100 AND 5000 inclui 100 e 5000. É equivalente a GTE 100 AND LTE 5000.",
    dicaDeOuro: "💎 BETWEEN é perfeito para faixas de valor, idade, score, etc. Mais legível que AND + AND.",
    
    // AULA COMPLETA
    definicaoSimples: "BETWEEN verifica se um valor está DENTRO de uma faixa (intervalo). Inclui os limites inferior e superior.",
    comoFunciona: "O sistema verifica: valor >= limite_inferior E valor <= limite_superior. Se ambas forem verdadeiras, retorna VERDADEIRO. É um atalho elegante para (campo GTE X) AND (campo LTE Y).",
    tabelaVerdade: [
      ["Valor", "BETWEEN 100 AND 5000", "Resultado"],
      ["50", "50 está abaixo de 100", "❌ FALSO"],
      ["100", "100 é o limite inferior", "✅ VERDADEIRO (inclui)"],
      ["2500", "2500 está no meio", "✅ VERDADEIRO"],
      ["5000", "5000 é o limite superior", "✅ VERDADEIRO (inclui)"],
      ["5001", "5001 está acima de 5000", "❌ FALSO"],
    ],
    exemplosExtras: [
      {
        titulo: "Faixa de horário comercial",
        cenario: "Transações das 9h às 18h são consideradas normais",
        codigo: "transaction.hour BETWEEN 9 AND 18",
        resultado: "9h, 12h, 18h = OK. 8h, 19h = fora do horário",
      },
      {
        titulo: "Score de risco médio",
        cenario: "Queremos regras para score entre 40 e 70",
        codigo: "risk_score BETWEEN 40 AND 70",
        resultado: "Captura faixa de risco médio para análise manual",
      },
      {
        titulo: "Idade de conta madura",
        cenario: "Contas entre 30 e 365 dias",
        codigo: "account_age_days BETWEEN 30 AND 365",
        resultado: "Não é nova nem muito antiga",
      },
    ],
    errosComuns: [
      "❌ Esquecer que BETWEEN INCLUI os limites: 100 e 5000 estão DENTRO da faixa",
      "❌ Inverter os limites: BETWEEN 5000 AND 100 pode não funcionar como esperado",
      "❌ Usar para exclusão: se quer valores FORA da faixa, use NOT_BETWEEN",
    ],
    operadoresRelacionados: ["NOT_BETWEEN", "GT", "GTE", "LT", "LTE"],
    checklistUso: [
      "☐ Você quer valores DENTRO de uma faixa?",
      "☐ Os limites devem ser INCLUÍDOS?",
      "☐ O limite inferior é menor que o superior?",
    ],
    exercicio: {
      pergunta: "Crie uma regra para transações entre R$500 e R$3.000 (incluindo ambos)",
      resposta: "transaction.amount BETWEEN 500 AND 3000",
    },
    
    // 🏭 COMPORTAMENTO NO MOTOR DE REGRAS
    comportamentoMotor: {
      descricao: "O motor extrai o valor do campo, compara com limite inferior (>=) e limite superior (<=). Se ambas comparações forem verdadeiras, retorna VERDADEIRO.",
      passos: [
        "1️⃣ Motor recebe a transação com campo numérico (ex: amount = 2500)",
        "2️⃣ Extrai os limites da regra: inferior = 100, superior = 5000",
        "3️⃣ Verifica: 2500 >= 100? SIM ✓",
        "4️⃣ Verifica: 2500 <= 5000? SIM ✓",
        "5️⃣ Ambas verdadeiras → retorna VERDADEIRO",
        "6️⃣ Se qualquer uma falhar, retorna FALSO",
      ],
      performance: "⚡ BETWEEN é muito eficiente - apenas 2 comparações numéricas. Ideal para índices de range em banco de dados.",
      cuidados: [
        "Certifique-se que limite_inferior < limite_superior",
        "BETWEEN INCLUI os limites (é inclusive nas duas pontas)",
        "Para faixas de horário que cruzam meia-noite, use lógica especial",
      ],
    },
    
    // 🎬 SITUAÇÕES REAIS
    situacoesReais: [
      {
        titulo: "Faixa de valor para análise manual",
        contexto: "Valores muito baixos (<R$100) ou muito altos (>R$5.000) vão para análise automática. O meio precisa de humano.",
        problema: "Como separar a 'faixa cinzenta' que precisa de olho humano?",
        solucao: "amount BETWEEN 100 AND 5000 → envia para fila de análise manual",
        impacto: "📊 Otimiza time de analistas: só revisam casos ambíguos, não os óbvios",
      },
      {
        titulo: "Horário comercial para suporte",
        contexto: "Chamados abertos das 9h às 18h têm SLA de 2h. Fora desse horário, SLA é 24h.",
        problema: "Como aplicar SLA diferente baseado no horário?",
        solucao: "created_hour BETWEEN 9 AND 18 → SLA = 2h",
        impacto: "⏰ Expectativas corretas para cliente: promete o que pode cumprir",
      },
      {
        titulo: "Score de risco para revisão",
        contexto: "Score 0-30 = aprova auto, 31-70 = revisão, 71-100 = rejeita auto",
        problema: "Como criar a faixa de revisão?",
        solucao: "risk_score BETWEEN 31 AND 70 → envia para fila de compliance",
        impacto: "🎯 Compliance foca nos casos borderline, não nos óbvios",
      },
    ],
    
    // 📊 RESULTADOS POSSÍVEIS
    resultadosPossiveis: {
      quandoDispara: "🎯 O valor está DENTRO da faixa (inclusive os limites) - transação se encaixa no perfil definido",
      quandoNaoDispara: "↔️ O valor está FORA da faixa (abaixo do mínimo ou acima do máximo)",
      acaoRecomendada: "Use BETWEEN para segmentação: diferentes faixas → diferentes ações. Combine múltiplos BETWEEN com OR para faixas complexas.",
    },
    
    // 🔧 COMO TESTAR
    comoTestar: [
      "📝 Teste 1: Valor exatamente no limite inferior (100) → deve disparar",
      "📝 Teste 2: Valor exatamente no limite superior (5000) → deve disparar",
      "📝 Teste 3: Valor 1 abaixo do limite inferior (99) → não deve disparar",
      "📝 Teste 4: Valor 1 acima do limite superior (5001) → não deve disparar",
      "📝 Teste 5: Valor no meio (2500) → deve disparar",
    ],
  },

  NOT_BETWEEN: {
    historia: "Karen quer alertar sobre valores FORA do padrão: abaixo de R$10 OU acima de R$10.000. Valores no meio são normais.",
    personagem: "👩‍🔍 Karen, Investigadora",
    problema: "Como verificar se um valor está FORA de uma faixa?",
    analogia: "🚨 Pense em um detector de anomalia: 'pressão normal é entre 10 e 14'. Se está fora dessa faixa (9 ou 15), alerta!",
    passoAPasso: [
      "1️⃣ Selecione o campo numérico (ex: transaction.amount)",
      "2️⃣ Escolha o operador NOT_BETWEEN",
      "3️⃣ Digite a faixa 'normal' (ex: 10 a 10000)",
      "4️⃣ A regra dispara para valores FORA: 9, 10001...",
    ],
    antes: "❌ ANTES: Você teria que escrever (amount LT 10) OR (amount GT 10000). Mais complexo.",
    depois: "✅ DEPOIS: Com NOT_BETWEEN, uma linha resolve: amount NOT_BETWEEN 10 AND 10000.",
    sintaxe: "transaction.amount NOT_BETWEEN 10 AND 10000",
    explicacaoSintaxe: "📖 Leia assim: 'Se o valor estiver FORA da faixa 10-10000, então dispare'",
    perguntaComum: "NOT_BETWEEN exclui os limites?",
    respostaPergunta: "Sim! NOT_BETWEEN 10 AND 10000 exclui 10 e 10000 (eles são considerados 'dentro' da faixa).",
    dicaDeOuro: "💎 Use NOT_BETWEEN para detectar outliers e anomalias. Valores muito baixos OU muito altos.",
  },

  // ══════════════════════════════════════════════════════════════════════════
  // LISTAS - QUANDO VOCÊ TEM MÚLTIPLAS OPÇÕES
  // ══════════════════════════════════════════════════════════════════════════
  IN: {
    historia: "Lucas precisa criar uma regra que dispare para 3 canais específicos: APP, WEB e POS. Se vier de qualquer um desses, dispara.",
    personagem: "👨‍💻 Lucas, Desenvolvedor",
    problema: "Como verificar se um valor está em uma LISTA de opções válidas?",
    analogia: "📋 Pense em uma lista de convidados VIP. Se seu nome está na lista, você entra. Se não está, fica de fora. O IN verifica se o valor está na lista.",
    passoAPasso: [
      "1️⃣ Selecione o campo (ex: transaction.channel)",
      "2️⃣ Escolha o operador IN",
      "3️⃣ Digite a lista de valores válidos: [\"APP\", \"WEB\", \"POS\"]",
      "4️⃣ A regra dispara se o canal for QUALQUER um da lista",
    ],
    antes: "❌ ANTES: Você teria que escrever (channel = APP) OR (channel = WEB) OR (channel = POS). Muito verboso!",
    depois: "✅ DEPOIS: Com IN, uma linha resolve: channel IN [\"APP\", \"WEB\", \"POS\"]. Elegante!",
    sintaxe: "transaction.channel IN [\"APP\", \"WEB\", \"POS\"]",
    explicacaoSintaxe: "📖 Leia assim: 'Se o canal estiver NA LISTA [APP, WEB, POS], então dispare'",
    perguntaComum: "Qual o formato correto da lista?",
    respostaPergunta: "Use colchetes e aspas: [\"valor1\", \"valor2\"]. Para números: [100, 200, 300].",
    dicaDeOuro: "💎 IN é perfeito para validar canais, status, países, MCCs, etc. Muito mais limpo que múltiplos OR.",
    
    // AULA COMPLETA
    definicaoSimples: "IN verifica se um valor está presente em uma LISTA de opções. Basta estar em UMA posição da lista para retornar VERDADEIRO.",
    comoFunciona: "O sistema percorre a lista item por item e compara com o valor do campo. Se encontrar uma correspondência EXATA em qualquer posição, retorna VERDADEIRO. Se chegar ao fim sem encontrar, retorna FALSO.",
    tabelaVerdade: [
      ["Valor do Campo", "Lista", "Resultado IN"],
      ["\"APP\"", "[\"APP\", \"WEB\", \"POS\"]", "✅ VERDADEIRO (APP está na lista)"],
      ["\"WEB\"", "[\"APP\", \"WEB\", \"POS\"]", "✅ VERDADEIRO (WEB está na lista)"],
      ["\"API\"", "[\"APP\", \"WEB\", \"POS\"]", "❌ FALSO (API não está na lista)"],
      ["\"app\"", "[\"APP\", \"WEB\", \"POS\"]", "❌ FALSO (case-sensitive!)"],
    ],
    exemplosExtras: [
      {
        titulo: "Países de alto risco FATF",
        cenario: "Bloquear transações de países na lista negra",
        codigo: "country IN [\"KP\", \"IR\", \"SY\", \"CU\"]",
        resultado: "Coreia do Norte, Irã, Síria, Cuba = BLOQUEIA",
      },
      {
        titulo: "MCCs de gambling",
        cenario: "Alertar transações em estabelecimentos de jogos",
        codigo: "mcc IN [\"7995\", \"7994\", \"7993\"]",
        resultado: "Captura casinos, loterias, apostas",
      },
      {
        titulo: "Status que precisam de ação",
        cenario: "Processar apenas pedidos pendentes ou em análise",
        codigo: "status IN [\"PENDING\", \"REVIEW\", \"WAITING\"]",
        resultado: "Ignora APPROVED, DECLINED, CANCELLED",
      },
    ],
    errosComuns: [
      "❌ Esquecer as aspas em textos: [APP, WEB] está errado, use [\"APP\", \"WEB\"]",
      "❌ Esquecer que é case-sensitive: \"app\" não é igual a \"APP\"",
      "❌ Usar IN quando deveria usar CONTAINS: IN é para valor exato, CONTAINS é para trecho",
    ],
    operadoresRelacionados: ["NOT_IN", "EQ", "OR", "CONTAINS"],
    checklistUso: [
      "☐ Você tem uma lista finita de valores válidos?",
      "☐ Os valores estão formatados corretamente? (aspas para texto)",
      "☐ O case (maiúscula/minúscula) está correto?",
    ],
    exercicio: {
      pergunta: "Crie uma regra que detecte transações dos canais MOBILE, TABLET ou SMARTWATCH",
      resposta: "channel IN [\"MOBILE\", \"TABLET\", \"SMARTWATCH\"]",
    },
    
    // 🏭 COMPORTAMENTO NO MOTOR DE REGRAS
    comportamentoMotor: {
      descricao: "O motor extrai o valor do campo e verifica se existe na lista especificada. É uma busca sequencial ou hashmap dependendo da implementação.",
      passos: [
        "1️⃣ Motor recebe a transação com campo (ex: channel = \"APP\")",
        "2️⃣ Carrega a lista da regra: [\"APP\", \"WEB\", \"POS\"]",
        "3️⃣ Compara valor com primeiro elemento: \"APP\" == \"APP\"? SIM ✓",
        "4️⃣ Encontrou match → retorna VERDADEIRO imediatamente",
        "5️⃣ (Se não encontrar, continua comparando até o fim da lista)",
        "6️⃣ Se chegar ao fim sem match → retorna FALSO",
      ],
      performance: "⚡ Para listas pequenas (<20 itens), busca sequencial é rápida. Para listas grandes, considere usar lookup tables ou hashsets.",
      cuidados: [
        "Case-sensitive por padrão: \"APP\" != \"app\"",
        "Formato correto: strings entre aspas, números sem aspas",
        "Listas muito grandes podem impactar performance",
        "Considere usar referência a lista cadastrada no sistema",
      ],
    },
    
    // 🎬 SITUAÇÕES REAIS
    situacoesReais: [
      {
        titulo: "Lista de países FATF/GAFI de alto risco",
        contexto: "Compliance precisa bloquear transações de países na lista negra FATF",
        problema: "São 20+ países e a lista muda periodicamente",
        solucao: "country IN [\"KP\", \"IR\", \"MM\", \"SY\", ...] → BLOCK",
        impacto: "📋 100% conformidade com FATF, atualização fácil da lista",
      },
      {
        titulo: "MCCs de alto risco (gambling, adult, crypto)",
        contexto: "Certas categorias de merchant têm risco elevado de fraude/chargeback",
        problema: "Precisa monitorar MCCs específicos sem criar N regras",
        solucao: "mcc IN [\"7995\", \"5967\", \"6051\", \"4829\"] → FLAG para análise",
        impacto: "🎰 Detecta 90% das transações em estabelecimentos de risco",
      },
      {
        titulo: "Canais digitais vs físicos",
        contexto: "Transações online têm regras diferentes de POS físico",
        problema: "Quer aplicar regras apenas para canais digitais",
        solucao: "channel IN [\"APP\", \"WEB\", \"API\"] → aplica regras de e-commerce",
        impacto: "🌐 Regras específicas para cada tipo de canal, zero desperdício",
      },
    ],
    
    // 📊 RESULTADOS POSSÍVEIS
    resultadosPossiveis: {
      quandoDispara: "📋 O valor do campo ESTÁ na lista - transação pertence ao grupo definido",
      quandoNaoDispara: "🚫 O valor do campo NÃO está na lista - transação não pertence ao grupo",
      acaoRecomendada: "Use IN para whitelists (valores permitidos) ou para segmentar grupos. Combine com AND para refinar: (country IN lista_latam) AND (amount GT 1000)",
    },
    
    // 🔧 COMO TESTAR
    comoTestar: [
      "📝 Teste 1: Valor que está na lista (primeiro item) → deve disparar",
      "📝 Teste 2: Valor que está na lista (último item) → deve disparar",
      "📝 Teste 3: Valor que NÃO está na lista → não deve disparar",
      "📝 Teste 4: Mesmo valor com case diferente (\"app\" vs \"APP\") → verificar comportamento",
      "📝 Teste 5: Valor null ou vazio → verificar se dá erro ou retorna FALSO",
    ],
  },

  NOT_IN: {
    historia: "Mariana quer bloquear transações de países de alto risco: Coreia do Norte, Irã, Síria. Se vier de QUALQUER um desses, bloqueia.",
    personagem: "👩‍⚖️ Mariana, Compliance",
    problema: "Como verificar se um valor NÃO está em uma lista proibida?",
    analogia: "🚫 Pense em uma lista negra de restaurantes. Se o restaurante está na lista, você não vai. O NOT_IN verifica se o valor NÃO está na lista.",
    passoAPasso: [
      "1️⃣ Selecione o campo (ex: transaction.country)",
      "2️⃣ Escolha o operador NOT_IN",
      "3️⃣ Digite a lista de valores PROIBIDOS: [\"KP\", \"IR\", \"SY\"]",
      "4️⃣ A regra dispara se o país NÃO estiver na lista (ou seja, é permitido)",
    ],
    antes: "❌ ANTES: Você teria que escrever (country != KP) AND (country != IR) AND (country != SY). Verboso!",
    depois: "✅ DEPOIS: Com NOT_IN, uma linha: country NOT_IN [\"KP\", \"IR\", \"SY\"].",
    sintaxe: "transaction.country NOT_IN [\"KP\", \"IR\", \"SY\"]",
    explicacaoSintaxe: "📖 Leia assim: 'Se o país NÃO estiver na lista [KP, IR, SY], então permita'",
    perguntaComum: "Posso usar NOT_IN para blacklists?",
    respostaPergunta: "Sim! NOT_IN é perfeito para blacklists. Se está na lista, bloqueia. Se não está, libera.",
    dicaDeOuro: "💎 Combine NOT_IN com listas cadastradas no sistema para manter blacklists atualizadas sem mudar a regra.",
  },

  // ══════════════════════════════════════════════════════════════════════════
  // STRINGS - TRABALHANDO COM TEXTO
  // ══════════════════════════════════════════════════════════════════════════
  CONTAINS: {
    historia: "Nelson quer identificar e-mails suspeitos que contenham 'tempmail' ou 'disposable' no domínio.",
    personagem: "👨‍🔍 Nelson, Segurança da Informação",
    problema: "Como verificar se um texto CONTÉM uma palavra ou trecho específico?",
    analogia: "🔍 Pense em usar Ctrl+F em um documento. Você busca 'tempmail' e ele destaca todas as ocorrências. O CONTAINS faz isso: verifica se o texto contém o trecho.",
    passoAPasso: [
      "1️⃣ Selecione o campo de texto (ex: customer.email)",
      "2️⃣ Escolha o operador CONTAINS",
      "3️⃣ Digite o trecho a buscar (ex: \"tempmail\")",
      "4️⃣ A regra dispara se o e-mail contiver 'tempmail' em qualquer posição",
    ],
    antes: "❌ ANTES: Você não conseguia detectar padrões no meio do texto. Um e-mail user@tempmail.com passaria despercebido.",
    depois: "✅ DEPOIS: Com CONTAINS, qualquer e-mail com 'tempmail' é detectado: user@tempmail.com, test@mytempmail.net, etc.",
    sintaxe: "customer.email CONTAINS \"tempmail\"",
    explicacaoSintaxe: "📖 Leia assim: 'Se o e-mail CONTIVER o texto tempmail, então dispare'",
    perguntaComum: "CONTAINS diferencia maiúsculas/minúsculas?",
    respostaPergunta: "Depende da configuração. Por padrão, geralmente é case-insensitive. Verifique a documentação do motor.",
    dicaDeOuro: "💎 Use CONTAINS para detectar padrões suspeitos em descrições, e-mails, nomes, etc.",
    
    // AULA COMPLETA
    definicaoSimples: "CONTAINS verifica se um texto CONTÉM um trecho específico em QUALQUER posição (início, meio ou fim).",
    comoFunciona: "O sistema percorre o texto caractere por caractere procurando a sequência especificada. Se encontrar em qualquer posição, retorna VERDADEIRO. A busca é como um Ctrl+F - não importa onde está, só importa que existe.",
    tabelaVerdade: [
      ["Valor do Campo", "Busca (CONTAINS)", "Resultado"],
      ["\"user@tempmail.com\"", "\"tempmail\"", "✅ VERDADEIRO (está no meio)"],
      ["\"tempmail_user@gmail.com\"", "\"tempmail\"", "✅ VERDADEIRO (está no início)"],
      ["\"user@gmail.tempmail\"", "\"tempmail\"", "✅ VERDADEIRO (está no fim)"],
      ["\"user@gmail.com\"", "\"tempmail\"", "❌ FALSO (não contém)"],
      ["\"user@TempMail.com\"", "\"tempmail\"", "⚠️ Depende se é case-sensitive"],
    ],
    exemplosExtras: [
      {
        titulo: "Detectar e-mails descartáveis",
        cenario: "Bloquear domínios de e-mail temporário conhecidos",
        codigo: "email CONTAINS \"tempmail\" OR email CONTAINS \"disposable\" OR email CONTAINS \"guerrilla\"",
        resultado: "Captura qualquer e-mail com esses provedores descartáveis",
      },
      {
        titulo: "Palavras suspeitas em descrição",
        cenario: "Identificar descrições que mencionam 'test' ou 'fake'",
        codigo: "description CONTAINS \"test\" OR description CONTAINS \"fake\"",
        resultado: "Alerta para transações de teste ou potencialmente fraudulentas",
      },
      {
        titulo: "Nome de estabelecimento suspeito",
        cenario: "Detectar merchants com 'crypto' ou 'forex' no nome",
        codigo: "merchant_name CONTAINS \"crypto\" OR merchant_name CONTAINS \"forex\"",
        resultado: "Sinaliza estabelecimentos de alto risco",
      },
    ],
    errosComuns: [
      "❌ Usar CONTAINS quando precisa de EXATO: CONTAINS \"BR\" vai pegar BRASIL, BRAGA, COBRA...",
      "❌ Esquecer de considerar case-sensitivity: 'TempMail' vs 'tempmail'",
      "❌ Usar CONTAINS em campos numéricos - é para TEXTO apenas",
      "❌ Confundir com IN: CONTAINS busca TRECHO, IN busca VALOR EXATO na lista",
    ],
    operadoresRelacionados: ["NOT_CONTAINS", "STARTS_WITH", "ENDS_WITH", "MATCHES_REGEX", "IN"],
    checklistUso: [
      "☐ O campo é do tipo TEXTO (string)?",
      "☐ Você quer encontrar um TRECHO (não valor exato)?",
      "☐ O trecho que você busca é único o suficiente? (cuidado com 'BR', 'A', etc.)",
      "☐ Você considerou variações de maiúscula/minúscula?",
    ],
    exercicio: {
      pergunta: "Crie uma regra que detecte e-mails com 'hotmail' em qualquer posição",
      resposta: "email CONTAINS \"hotmail\"",
    },
  },

  NOT_CONTAINS: {
    historia: "Ana quer garantir que e-mails NÃO contenham termos de domínios internos antes de enviar comunicações externas.",
    personagem: "👩‍💻 Ana, DPO",
    problema: "Como verificar se um texto NÃO contém uma palavra específica?",
    analogia: "🚫 Pense em um filtro de spam ao contrário. Em vez de detectar spam, você quer garantir que NÃO há conteúdo proibido.",
    passoAPasso: [
      "1️⃣ Selecione o campo de texto",
      "2️⃣ Escolha o operador NOT_CONTAINS",
      "3️⃣ Digite o termo que NÃO deve aparecer",
      "4️⃣ A regra dispara apenas se o texto NÃO contiver o termo",
    ],
    antes: "❌ ANTES: Você teria que usar negação complexa ou validação manual.",
    depois: "✅ DEPOIS: Com NOT_CONTAINS, é direto: mensagem NOT_CONTAINS \"confidencial\" garante envio externo seguro.",
    sintaxe: "message NOT_CONTAINS \"confidencial\"",
    explicacaoSintaxe: "📖 Leia assim: 'Se a mensagem NÃO CONTIVER confidencial, então pode prosseguir'",
    perguntaComum: "NOT_CONTAINS é o oposto de CONTAINS?",
    respostaPergunta: "Exatamente! Se CONTAINS retorna VERDADEIRO, NOT_CONTAINS retorna FALSO, e vice-versa.",
    dicaDeOuro: "💎 Use NOT_CONTAINS para validação de dados - garantir que campos não tenham conteúdo proibido.",
  },

  STARTS_WITH: {
    historia: "Olivia quer identificar cartões Visa. Todos os cartões Visa começam com o dígito 4.",
    personagem: "👩‍💼 Olivia, Analista de Pagamentos",
    problema: "Como verificar se um texto COMEÇA com um prefixo específico?",
    analogia: "📞 Pense em DDDs de telefone. Se começa com 11, é São Paulo. Se começa com 21, é Rio. O STARTS_WITH verifica o início.",
    passoAPasso: [
      "1️⃣ Selecione o campo de texto (ex: card.number)",
      "2️⃣ Escolha o operador STARTS_WITH",
      "3️⃣ Digite o prefixo esperado (ex: \"4\")",
      "4️⃣ A regra dispara se o número do cartão começar com 4 (Visa)",
    ],
    antes: "❌ ANTES: Você teria que usar REGEX complexo ou verificar manualmente o primeiro caractere.",
    depois: "✅ DEPOIS: Com STARTS_WITH, é simples: card.number STARTS_WITH \"4\" captura todos os Visa.",
    sintaxe: "card.number STARTS_WITH \"4\"",
    explicacaoSintaxe: "📖 Leia assim: 'Se o número do cartão COMEÇAR COM 4, então é Visa'",
    perguntaComum: "Posso usar STARTS_WITH com mais de um caractere?",
    respostaPergunta: "Sim! STARTS_WITH \"411111\" verificaria um BIN completo de 6 dígitos.",
    dicaDeOuro: "💎 Use STARTS_WITH para identificar bandeiras de cartão, DDIs de telefone, prefixos de códigos, etc.",
  },

  ENDS_WITH: {
    historia: "Paulo quer identificar e-mails corporativos da empresa. Todos terminam com @empresa.com.br.",
    personagem: "👨‍💼 Paulo, RH",
    problema: "Como verificar se um texto TERMINA com um sufixo específico?",
    analogia: "📧 Pense em extensões de arquivo: .pdf, .docx, .xlsx. O ENDS_WITH verifica o final do texto.",
    passoAPasso: [
      "1️⃣ Selecione o campo de texto (ex: customer.email)",
      "2️⃣ Escolha o operador ENDS_WITH",
      "3️⃣ Digite o sufixo esperado (ex: \"@empresa.com.br\")",
      "4️⃣ A regra dispara se o e-mail terminar com esse domínio",
    ],
    antes: "❌ ANTES: Você teria que usar REGEX ou CONTAINS, que poderia pegar falsos positivos.",
    depois: "✅ DEPOIS: Com ENDS_WITH, você garante que é exatamente o final: joao@empresa.com.br ✓, empresa.com.br@fake.com ✗",
    sintaxe: "customer.email ENDS_WITH \"@empresa.com.br\"",
    explicacaoSintaxe: "📖 Leia assim: 'Se o e-mail TERMINAR COM @empresa.com.br, então é corporativo'",
    perguntaComum: "Qual a diferença entre CONTAINS e ENDS_WITH?",
    respostaPergunta: "CONTAINS busca em qualquer posição. ENDS_WITH só no final. ENDS_WITH é mais preciso para domínios.",
    dicaDeOuro: "💎 Use ENDS_WITH para validar domínios de e-mail, extensões de arquivo, sufixos de códigos.",
  },

  REGEX: {
    historia: "Quitéria, expert em dados, precisa identificar CPFs em formato específico: 000.000.000-00. O padrão é complexo.",
    personagem: "👩‍🔬 Quitéria, Engenheira de Dados",
    problema: "Como validar padrões complexos de texto que não podem ser expressos com CONTAINS/STARTS/ENDS?",
    analogia: "🧩 Pense em um molde de biscoito. Só passa a massa que tem exatamente aquele formato. O REGEX é um 'molde' para texto.",
    passoAPasso: [
      "1️⃣ Selecione o campo de texto (ex: customer.cpf)",
      "2️⃣ Escolha o operador REGEX",
      "3️⃣ Digite a expressão regular entre barras (ex: /^\\d{3}\\.\\d{3}\\.\\d{3}-\\d{2}$/)",
      "4️⃣ A regra dispara se o CPF casar com o padrão",
    ],
    antes: "❌ ANTES: Você não conseguia validar padrões complexos. Qualquer formato de CPF passaria.",
    depois: "✅ DEPOIS: Com REGEX, você valida o formato exato: 123.456.789-00 ✓, 12345678900 ✗",
    sintaxe: "customer.cpf REGEX /^\\d{3}\\.\\d{3}\\.\\d{3}-\\d{2}$/",
    explicacaoSintaxe: "📖 O REGEX diz: começa (^), 3 dígitos, ponto, 3 dígitos, ponto, 3 dígitos, hífen, 2 dígitos, termina ($)",
    perguntaComum: "REGEX é difícil. Tem como aprender?",
    respostaPergunta: "Sim! Use sites como regex101.com para testar. Comece com padrões simples e vá evoluindo.",
    dicaDeOuro: "💎 Use REGEX quando precisa de validação complexa: formatos de documento, placas, códigos específicos.",
  },

  // ══════════════════════════════════════════════════════════════════════════
  // NULOS - VERIFICANDO CAMPOS VAZIOS
  // ══════════════════════════════════════════════════════════════════════════
  IS_NULL: {
    historia: "Rafaela precisa identificar clientes que não informaram telefone. Campo vazio = risco.",
    personagem: "👩‍💼 Rafaela, Onboarding",
    problema: "Como verificar se um campo está VAZIO ou não foi informado?",
    analogia: "📝 Pense em um formulário de papel. Alguns campos estão em branco - não foram preenchidos. O IS_NULL detecta esses campos vazios.",
    passoAPasso: [
      "1️⃣ Selecione o campo que pode estar vazio (ex: customer.phone)",
      "2️⃣ Escolha o operador IS_NULL",
      "3️⃣ Não precisa de valor à direita - IS_NULL é unário",
      "4️⃣ A regra dispara se o campo for nulo/vazio",
    ],
    antes: "❌ ANTES: Campos vazios passavam despercebidos. Clientes sem telefone eram aprovados.",
    depois: "✅ DEPOIS: Com IS_NULL, você detecta cadastros incompletos e pode solicitar a informação.",
    sintaxe: "customer.phone IS_NULL",
    explicacaoSintaxe: "📖 Leia assim: 'Se o telefone ESTIVER VAZIO, então alerte'",
    perguntaComum: "Qual a diferença entre NULL e string vazia \"\"?",
    respostaPergunta: "NULL = campo não existe ou não foi enviado. \"\" = campo existe mas está vazio. IS_EMPTY pega strings vazias.",
    dicaDeOuro: "💎 Combine IS_NULL com regras de bloqueio para garantir dados cadastrais completos.",
    
    // AULA COMPLETA
    definicaoSimples: "IS_NULL verifica se um campo está AUSENTE, VAZIO ou não foi informado. É um operador UNÁRIO (não precisa de valor à direita).",
    comoFunciona: "O sistema verifica se o campo é literalmente 'null' (não existe), 'undefined' (não definido) ou, em alguns casos, vazio. É diferente de string vazia \"\" ou zero 0 - esses são VALORES, não ausência de valor.",
    tabelaVerdade: [
      ["Valor do Campo", "IS_NULL", "Explicação"],
      ["null", "✅ VERDADEIRO", "Campo não existe"],
      ["undefined", "✅ VERDADEIRO", "Campo não foi definido"],
      ["\"\"", "❌ FALSO*", "String vazia É um valor (depende do sistema)"],
      ["0", "❌ FALSO", "Zero É um valor numérico"],
      ["\"João\"", "❌ FALSO", "Campo tem valor"],
      ["false", "❌ FALSO", "False É um valor booleano"],
    ],
    exemplosExtras: [
      {
        titulo: "Cadastro incompleto - telefone",
        cenario: "Detectar clientes que não informaram telefone",
        codigo: "customer.phone IS_NULL",
        resultado: "Solicita telefone antes de aprovar",
      },
      {
        titulo: "Endereço de entrega ausente",
        cenario: "Validar pedidos que precisam de endereço",
        codigo: "order.shipping_address IS_NULL AND order.type EQ \"PHYSICAL\"",
        resultado: "Bloqueia pedidos físicos sem endereço",
      },
      {
        titulo: "Device fingerprint ausente",
        cenario: "Identificar transações sem fingerprint (suspeitas)",
        codigo: "transaction.device_fingerprint IS_NULL",
        resultado: "Score de risco aumentado para análise manual",
      },
    ],
    errosComuns: [
      "❌ Confundir NULL com string vazia: \"\" NÃO é NULL (use IS_EMPTY para strings vazias)",
      "❌ Confundir NULL com zero: 0 NÃO é NULL (use EQ 0 para verificar zero)",
      "❌ Confundir NULL com false: false NÃO é NULL (use IS_FALSE para booleanos)",
      "❌ Esquecer que IS_NULL é unário: NÃO escreva 'campo IS_NULL true'",
    ],
    operadoresRelacionados: ["NOT_NULL", "IS_EMPTY", "NOT_EMPTY", "EQ"],
    checklistUso: [
      "☐ Você quer verificar se o campo NÃO FOI INFORMADO?",
      "☐ O campo pode ser enviado como NULL no payload?",
      "☐ Você entende a diferença entre NULL e string vazia?",
    ],
    exercicio: {
      pergunta: "Crie uma regra que detecte transações sem o campo customer.email",
      resposta: "customer.email IS_NULL",
    },
  },

  NOT_NULL: {
    historia: "Sérgio quer garantir que toda transação tenha o campo device_id preenchido para rastreabilidade.",
    personagem: "👨‍🔒 Sérgio, Segurança",
    problema: "Como garantir que um campo OBRIGATÓRIO está preenchido?",
    analogia: "✅ Pense em uma checklist de voo. 'Combustível: verificado ✓'. O NOT_NULL é a marca de 'verificado' - o campo existe e tem valor.",
    passoAPasso: [
      "1️⃣ Selecione o campo obrigatório (ex: transaction.device_id)",
      "2️⃣ Escolha o operador NOT_NULL",
      "3️⃣ A regra dispara se o campo ESTIVER preenchido",
      "4️⃣ Use para validar que dados essenciais foram enviados",
    ],
    antes: "❌ ANTES: Transações sem device_id eram processadas, dificultando investigações.",
    depois: "✅ DEPOIS: Com NOT_NULL, você valida a presença de campos críticos antes de processar.",
    sintaxe: "transaction.device_id NOT_NULL",
    explicacaoSintaxe: "📖 Leia assim: 'Se o device_id ESTIVER PREENCHIDO, então é válido'",
    perguntaComum: "NOT_NULL é o oposto de IS_NULL?",
    respostaPergunta: "Exatamente! IS_NULL = vazio. NOT_NULL = preenchido. São complementares.",
    dicaDeOuro: "💎 Use NOT_NULL em combinação com outras regras: (device_id NOT_NULL) AND (amount GT 1000).",
  },

  // ══════════════════════════════════════════════════════════════════════════
  // BOOLEANOS - VERDADEIRO OU FALSO
  // ══════════════════════════════════════════════════════════════════════════
  IS_TRUE: {
    historia: "Tatiana quer dar tratamento especial para clientes VIP. O campo is_vip é true ou false.",
    personagem: "👩‍💼 Tatiana, CX Manager",
    problema: "Como verificar se uma flag booleana é VERDADEIRA?",
    analogia: "💡 Pense em um interruptor de luz: ligado (true) ou desligado (false). O IS_TRUE verifica se o interruptor está ligado.",
    passoAPasso: [
      "1️⃣ Selecione o campo booleano (ex: customer.is_vip)",
      "2️⃣ Escolha o operador IS_TRUE",
      "3️⃣ A regra dispara se o campo for true",
      "4️⃣ Cliente VIP recebe tratamento diferenciado",
    ],
    antes: "❌ ANTES: Você escrevia is_vip = true, que funciona, mas IS_TRUE é mais semântico.",
    depois: "✅ DEPOIS: Com IS_TRUE, o código fica mais legível: customer.is_vip IS_TRUE.",
    sintaxe: "customer.is_vip IS_TRUE",
    explicacaoSintaxe: "📖 Leia assim: 'Se o cliente FOR VIP (is_vip = true), então aplique benefícios'",
    perguntaComum: "Posso usar IS_TRUE com campos que não são booleanos?",
    respostaPergunta: "Não recomendado. Use IS_TRUE apenas com campos true/false. Para outros, use EQ ou NEQ.",
    dicaDeOuro: "💎 IS_TRUE deixa a regra mais legível. Em vez de 'campo = true', use 'campo IS_TRUE'.",
  },

  IS_FALSE: {
    historia: "Ulisses quer identificar clientes que ainda não verificaram o e-mail. O campo email_verified é false.",
    personagem: "👨‍💼 Ulisses, Growth",
    problema: "Como verificar se uma flag booleana é FALSA?",
    analogia: "💡 Pense no mesmo interruptor: IS_FALSE verifica se está desligado (false).",
    passoAPasso: [
      "1️⃣ Selecione o campo booleano (ex: customer.email_verified)",
      "2️⃣ Escolha o operador IS_FALSE",
      "3️⃣ A regra dispara se o campo for false",
      "4️⃣ Cliente sem e-mail verificado recebe lembrete",
    ],
    antes: "❌ ANTES: Você escrevia email_verified = false.",
    depois: "✅ DEPOIS: Com IS_FALSE, fica mais claro: customer.email_verified IS_FALSE.",
    sintaxe: "customer.email_verified IS_FALSE",
    explicacaoSintaxe: "📖 Leia assim: 'Se o e-mail NÃO ESTIVER verificado, então envie lembrete'",
    perguntaComum: "IS_FALSE é diferente de NOT IS_TRUE?",
    respostaPergunta: "Na prática, são equivalentes. Mas IS_FALSE é mais direto e legível.",
    dicaDeOuro: "💎 Use IS_FALSE para detectar configurações desativadas, verificações pendentes, etc.",
  },

  // ══════════════════════════════════════════════════════════════════════════
  // ARRAYS - TRABALHANDO COM LISTAS
  // ══════════════════════════════════════════════════════════════════════════
  ARRAY_CONTAINS: {
    historia: "Vanessa quer dar desconto para pedidos com a tag 'promocao'. O campo tags é uma lista.",
    personagem: "👩‍💼 Vanessa, Marketing",
    problema: "Como verificar se uma LISTA contém um elemento específico?",
    analogia: "🛒 Pense em um carrinho de compras. Você quer saber se tem 'leite' no carrinho. O ARRAY_CONTAINS verifica se o item está na lista.",
    passoAPasso: [
      "1️⃣ Selecione o campo de array (ex: order.tags)",
      "2️⃣ Escolha o operador ARRAY_CONTAINS",
      "3️⃣ Digite o elemento a buscar (ex: \"promocao\")",
      "4️⃣ A regra dispara se a lista contiver esse elemento",
    ],
    antes: "❌ ANTES: Não era possível verificar conteúdo de listas. Você precisaria de código customizado.",
    depois: "✅ DEPOIS: Com ARRAY_CONTAINS, você busca dentro de listas facilmente.",
    sintaxe: "order.tags ARRAY_CONTAINS \"promocao\"",
    explicacaoSintaxe: "📖 Leia assim: 'Se as tags do pedido CONTIVEREM promocao, então dê desconto'",
    perguntaComum: "ARRAY_CONTAINS funciona com números?",
    respostaPergunta: "Sim! order.item_ids ARRAY_CONTAINS 12345 funciona perfeitamente.",
    dicaDeOuro: "💎 Use ARRAY_CONTAINS para verificar tags, categorias, IDs em listas, etc.",
  },

  ARRAY_SIZE_GT: {
    historia: "Wesley quer detectar pedidos com muitos itens. Mais de 10 itens pode ser estoque irregular.",
    personagem: "👨‍🔍 Wesley, Prevenção de Perdas",
    problema: "Como verificar se uma LISTA tem MAIS de X elementos?",
    analogia: "🛒 Pense no limite de itens do caixa rápido: 'até 10 itens'. O ARRAY_SIZE_GT verifica se passou do limite.",
    passoAPasso: [
      "1️⃣ Selecione o campo de array (ex: order.items)",
      "2️⃣ Escolha o operador ARRAY_SIZE_GT",
      "3️⃣ Digite o limite (ex: 10)",
      "4️⃣ A regra dispara se a lista tiver MAIS de 10 elementos (11+)",
    ],
    antes: "❌ ANTES: Pedidos com 50 itens passavam sem análise. Possível fraude ou erro.",
    depois: "✅ DEPOIS: Com ARRAY_SIZE_GT, você detecta listas anormalmente grandes.",
    sintaxe: "order.items ARRAY_SIZE_GT 10",
    explicacaoSintaxe: "📖 Leia assim: 'Se o pedido tiver MAIS DE 10 itens, então investigue'",
    perguntaComum: "ARRAY_SIZE_GT inclui o limite?",
    respostaPergunta: "Não! GT = maior que. Se quer incluir 10, use ARRAY_SIZE_GTE 10.",
    dicaDeOuro: "💎 Combine com valor: (items GT 10) AND (amount GT 5000) = pedido grande e caro.",
  },

  // ══════════════════════════════════════════════════════════════════════════
  // DATA/TEMPO - REGRAS TEMPORAIS
  // ══════════════════════════════════════════════════════════════════════════
  DATE_AFTER: {
    historia: "Xavier só quer processar transações após a data de lançamento da campanha: 01/01/2024.",
    personagem: "👨‍💼 Xavier, Campanhas",
    problema: "Como verificar se uma data é POSTERIOR a uma data de referência?",
    analogia: "📅 Pense em 'promoção válida a partir de 01/01'. Se a data for 02/01, é válida. O DATE_AFTER verifica se a data é posterior.",
    passoAPasso: [
      "1️⃣ Selecione o campo de data (ex: transaction.date)",
      "2️⃣ Escolha o operador DATE_AFTER",
      "3️⃣ Digite a data de referência (ex: \"2024-01-01\")",
      "4️⃣ A regra dispara se a data for DEPOIS de 01/01/2024",
    ],
    antes: "❌ ANTES: Transações antigas eram processadas na campanha nova, gerando confusão.",
    depois: "✅ DEPOIS: Com DATE_AFTER, você garante que só transações novas entram na campanha.",
    sintaxe: "transaction.date DATE_AFTER \"2024-01-01\"",
    explicacaoSintaxe: "📖 Leia assim: 'Se a data for DEPOIS DE 01/01/2024, então aplique a campanha'",
    perguntaComum: "DATE_AFTER inclui a data de referência?",
    respostaPergunta: "Não! 01/01/2024 não é 'depois' de 01/01/2024. Se quiser incluir, use DATE_AFTER_OR_EQ ou ajuste a data.",
    dicaDeOuro: "💎 Use formato ISO: \"2024-01-01\". Evita confusão entre DD/MM e MM/DD.",
  },

  TIME_BETWEEN: {
    historia: "Yasmin quer alertar transações em horário suspeito: entre 22h e 6h (madrugada).",
    personagem: "👩‍🔍 Yasmin, SOC",
    problema: "Como verificar se um HORÁRIO está dentro de uma faixa?",
    analogia: "🌙 Pense em 'horário de silêncio: 22h às 6h'. O TIME_BETWEEN verifica se o horário está na faixa.",
    passoAPasso: [
      "1️⃣ Selecione o campo de horário (ex: transaction.time)",
      "2️⃣ Escolha o operador TIME_BETWEEN",
      "3️⃣ Digite o horário inicial (ex: \"22:00\")",
      "4️⃣ Digite o horário final (ex: \"06:00\")",
      "5️⃣ A regra dispara se o horário estiver na faixa (atravessa meia-noite!)",
    ],
    antes: "❌ ANTES: Transações de madrugada passavam sem alerta. Fraudadores adoram a madrugada.",
    depois: "✅ DEPOIS: Com TIME_BETWEEN, você monitora horários de alto risco.",
    sintaxe: "transaction.time TIME_BETWEEN \"22:00\" AND \"06:00\"",
    explicacaoSintaxe: "📖 Leia assim: 'Se o horário estiver ENTRE 22h E 6h, então alerte'",
    perguntaComum: "TIME_BETWEEN funciona quando atravessa meia-noite?",
    respostaPergunta: "Sim! 22:00 a 06:00 captura 23:00, 00:00, 01:00... até 05:59.",
    dicaDeOuro: "💎 Combine TIME_BETWEEN com dia da semana para regras tipo 'madrugada de domingo'.",
  },

  AGE_DAYS_LT: {
    historia: "Zélia quer identificar contas recém-criadas. Contas com menos de 7 dias são de alto risco.",
    personagem: "👩‍🔒 Zélia, Antifraude",
    problema: "Como verificar se uma data tem MENOS de X dias de idade?",
    analogia: "👶 Pense em 'recém-nascido: menos de 7 dias'. O AGE_DAYS_LT verifica a 'idade' de uma data.",
    passoAPasso: [
      "1️⃣ Selecione o campo de data (ex: customer.created_at)",
      "2️⃣ Escolha o operador AGE_DAYS_LT",
      "3️⃣ Digite o número de dias (ex: 7)",
      "4️⃣ A regra dispara se a conta tiver MENOS de 7 dias",
    ],
    antes: "❌ ANTES: Contas de 1 dia tinham o mesmo tratamento que contas de 5 anos.",
    depois: "✅ DEPOIS: Com AGE_DAYS_LT, você detecta contas novas e aplica regras mais rigorosas.",
    sintaxe: "customer.created_at AGE_DAYS_LT 7",
    explicacaoSintaxe: "📖 Leia assim: 'Se a conta tiver MENOS DE 7 dias, então é nova (alto risco)'",
    perguntaComum: "AGE_DAYS_LT calcula a partir de hoje?",
    respostaPergunta: "Sim! Compara a data do campo com a data atual. 'Idade' = hoje - data_do_campo.",
    dicaDeOuro: "💎 Combine com valor: (age_days LT 7) AND (amount GT 1000) = conta nova + valor alto = alerta máximo!",
  },

  // ══════════════════════════════════════════════════════════════════════════
  // AGREGAÇÕES - OLHANDO O HISTÓRICO
  // ══════════════════════════════════════════════════════════════════════════
  COUNT_GT: {
    historia: "Amanda quer alertar quando um cliente fizer mais de 10 transações em 1 hora. Pode ser teste de cartão.",
    personagem: "👩‍🔍 Amanda, Monitoramento",
    problema: "Como CONTAR quantos eventos aconteceram em um período?",
    analogia: "📊 Pense em um contador de pessoas na loja: 'se entrar mais de 100 em 1 hora, chame reforço'. O COUNT_GT conta eventos.",
    passoAPasso: [
      "1️⃣ Escolha o operador COUNT_GT",
      "2️⃣ Defina o que contar (ex: transactions)",
      "3️⃣ Defina o período (ex: last_1h)",
      "4️⃣ Defina o agrupamento (ex: customer_id)",
      "5️⃣ Defina o limite (ex: 10)",
    ],
    antes: "❌ ANTES: Um fraudador podia fazer 50 transações seguidas sem alerta.",
    depois: "✅ DEPOIS: Com COUNT_GT, você detecta comportamento anômalo em tempo real.",
    sintaxe: "COUNT(transactions, last_1h, customer_id) GT 10",
    explicacaoSintaxe: "📖 Leia assim: 'Se o cliente fez MAIS DE 10 transações na última hora, então alerte'",
    perguntaComum: "Quais períodos posso usar?",
    respostaPergunta: "Exemplos: last_1h, last_24h, last_7d, last_30d. Depende da configuração do sistema.",
    dicaDeOuro: "💎 COUNT é essencial para regras de velocity. Combine com diferentes períodos e limites.",
    
    // AULA COMPLETA
    definicaoSimples: "COUNT_GT conta quantos eventos aconteceram em um período e verifica se passou de um limite. É a base das regras de VELOCITY (frequência).",
    comoFunciona: "O sistema olha para trás no tempo (janela temporal), conta quantos eventos do tipo especificado aconteceram para aquele agrupamento (ex: customer_id), e compara com o limite. Se a contagem for MAIOR que o limite, dispara.",
    tabelaVerdade: [
      ["Contagem na Janela", "Limite (GT 10)", "Resultado"],
      ["5 transações", "GT 10", "❌ FALSO (5 não é maior que 10)"],
      ["10 transações", "GT 10", "❌ FALSO (10 não é MAIOR que 10, é igual)"],
      ["11 transações", "GT 10", "✅ VERDADEIRO (11 > 10)"],
      ["50 transações", "GT 10", "✅ VERDADEIRO (50 > 10, alerta crítico!)"],
    ],
    exemplosExtras: [
      {
        titulo: "Teste de cartão (card testing)",
        cenario: "Fraudador testa se cartão roubado funciona com várias transações pequenas",
        codigo: "COUNT(transactions, last_1h, card_id) GT 5",
        resultado: "Mais de 5 transações com mesmo cartão em 1h = provável teste",
      },
      {
        titulo: "Múltiplos logins falhos",
        cenario: "Tentativa de brute force na conta",
        codigo: "COUNT(failed_logins, last_15min, user_id) GT 3",
        resultado: "Mais de 3 tentativas falhas em 15min = bloqueia conta",
      },
      {
        titulo: "Criação em massa",
        cenario: "Mesmo dispositivo criando muitas contas",
        codigo: "COUNT(account_creations, last_24h, device_fingerprint) GT 2",
        resultado: "Mais de 2 contas do mesmo device em 24h = suspeito",
      },
    ],
    errosComuns: [
      "❌ Esquecer que GT não inclui o limite: COUNT > 10 NÃO dispara quando tem exatamente 10",
      "❌ Usar janela muito grande: last_30d pode ser lento e capturar muito ruído",
      "❌ Escolher agrupamento errado: agrupar por email quando deveria agrupar por device",
      "❌ Limite muito baixo: COUNT > 1 gera muitos falsos positivos",
    ],
    operadoresRelacionados: ["COUNT_GTE", "COUNT_LT", "COUNT_LTE", "SUM_GT", "VELOCITY"],
    checklistUso: [
      "☐ O que você quer CONTAR? (transações, logins, etc.)",
      "☐ Em qual JANELA de tempo? (1h, 24h, 7d)",
      "☐ Agrupar por quê? (customer_id, device, card)",
      "☐ Qual o limite razoável? (não muito baixo, não muito alto)",
    ],
    exercicio: {
      pergunta: "Crie uma regra que detecte mais de 20 transações por cartão nas últimas 24 horas",
      resposta: "COUNT(transactions, last_24h, card_id) GT 20",
    },
    
    // 🏭 COMPORTAMENTO NO MOTOR DE REGRAS
    comportamentoMotor: {
      descricao: "O motor acessa o banco de dados de histórico, agrupa eventos pela chave especificada (ex: customer_id), conta quantos existem na janela temporal, e compara com o limite.",
      passos: [
        "1️⃣ Motor recebe a transação atual com identificadores (customer_id, card_id, etc.)",
        "2️⃣ Identifica a janela temporal (ex: last_1h = últimos 60 minutos)",
        "3️⃣ Consulta o histórico: 'quantas transações deste customer_id existem nos últimos 60 min?'",
        "4️⃣ Recebe o COUNT (ex: 12 transações)",
        "5️⃣ Aplica o comparador: 12 GT 10? SIM → dispara a regra",
        "6️⃣ Se a transação atual conta na janela depende da configuração (inclusive/exclusive)",
      ],
      performance: "⚡ Agregações são mais pesadas que comparações simples. Use índices no banco de histórico. Considere cache para janelas comuns.",
      cuidados: [
        "Janelas muito longas (last_30d) podem ser lentas sem otimização",
        "Verifique se a transação atual entra ou não na contagem",
        "Eventos duplicados/replay podem inflar a contagem",
        "Considere usar COUNT_GTE se quiser incluir o limite",
      ],
    },
    
    // 🎬 SITUAÇÕES REAIS
    situacoesReais: [
      {
        titulo: "Card Testing (teste de cartão roubado)",
        contexto: "Fraudador obtém dados de cartão e faz várias transações pequenas para testar se funciona",
        problema: "Cada transação individual parece normal (R$10, R$15, R$8...), mas o volume é anormal",
        solucao: "COUNT(transactions, last_1h, card_id) GT 5 - mais de 5 transações em 1 hora com mesmo cartão",
        impacto: "🛡️ Detecta 90% dos testes de cartão antes do fraudador fazer a compra grande",
      },
      {
        titulo: "Account Takeover (ATO) - Múltiplos logins falhos",
        contexto: "Atacante tenta adivinhar senha com múltiplas tentativas",
        problema: "1 ou 2 tentativas falhas são normais (erro de digitação). 10+ é ataque.",
        solucao: "COUNT(failed_logins, last_15min, user_id) GT 3",
        impacto: "🔒 Bloqueia conta após 4ª tentativa falha, protege contra brute force",
      },
      {
        titulo: "Criação de contas em massa (account farming)",
        contexto: "Fraudador cria múltiplas contas para abusar promoções ou laundering",
        problema: "Mesmo device/IP criando várias contas é suspeito",
        solucao: "COUNT(account_creations, last_24h, device_fingerprint) GT 2",
        impacto: "🚫 Bloqueia criação de mais de 2 contas por dispositivo por dia",
      },
    ],
    
    // 📊 RESULTADOS POSSÍVEIS
    resultadosPossiveis: {
      quandoDispara: "🚨 A contagem na janela temporal EXCEDE o limite - comportamento anômalo detectado (velocity alta)",
      quandoNaoDispara: "✅ A contagem está DENTRO do esperado - frequência normal de uso",
      acaoRecomendada: "Para velocity rules, considere escalar ações: >5 = FLAG, >10 = REVIEW, >20 = BLOCK",
    },
    
    // 🔧 COMO TESTAR
    comoTestar: [
      "📝 Teste 1: Envie 1 transação → COUNT deve ser 1 (não dispara se limite > 1)",
      "📝 Teste 2: Envie N+1 transações rapidamente onde N = limite → deve disparar na N+1",
      "📝 Teste 3: Envie N transações, espere janela expirar, envie mais → não deve disparar (janela resetou)",
      "📝 Teste 4: Verifique se a transação atual entra na contagem",
      "📝 Teste 5: Teste com agrupamentos diferentes (mesmo customer, cards diferentes)",
    ],
  },

  SUM_GT: {
    historia: "Bruno quer alertar quando a soma de transações de um cliente passar de R$10.000 em 24h.",
    personagem: "👨‍💼 Bruno, Compliance",
    problema: "Como SOMAR valores de múltiplos eventos em um período?",
    analogia: "🧮 Pense em um caixa somando as compras do dia: 'se passar de R$10.000, precisa de aprovação'. O SUM_GT soma valores.",
    passoAPasso: [
      "1️⃣ Escolha o operador SUM_GT",
      "2️⃣ Defina o campo a somar (ex: transactions.amount)",
      "3️⃣ Defina o período (ex: last_24h)",
      "4️⃣ Defina o agrupamento (ex: customer_id)",
      "5️⃣ Defina o limite (ex: 10000)",
    ],
    antes: "❌ ANTES: Um cliente podia fazer 100 transações de R$100 = R$10.000 sem alerta.",
    depois: "✅ DEPOIS: Com SUM_GT, você detecta fragmentação de valores (smurfing).",
    sintaxe: "SUM(transactions.amount, last_24h, customer_id) GT 10000",
    explicacaoSintaxe: "📖 Leia assim: 'Se a soma das transações nas últimas 24h for MAIOR QUE R$10.000, alerte'",
    perguntaComum: "Posso usar SUM com diferentes moedas?",
    respostaPergunta: "Depende. Geralmente você precisa converter para uma moeda base antes de somar.",
    dicaDeOuro: "💎 SUM é essencial para detectar estruturação (smurfing). Fraudadores dividem valores para não chamar atenção.",
    
    // AULA COMPLETA
    definicaoSimples: "SUM_GT soma os valores de um campo em um período e verifica se o total passou de um limite. Essencial para detectar ESTRUTURAÇÃO (smurfing).",
    comoFunciona: "O sistema olha para trás na janela temporal, soma todos os valores do campo especificado para aquele agrupamento (ex: customer_id), e compara com o limite. É como um totalizador que acumula valores ao longo do tempo.",
    tabelaVerdade: [
      ["Soma na Janela", "Limite (GT 10000)", "Resultado"],
      ["R$ 5.000", "GT 10000", "❌ FALSO (soma ainda baixa)"],
      ["R$ 10.000", "GT 10000", "❌ FALSO (soma igual, não MAIOR)"],
      ["R$ 10.001", "GT 10000", "✅ VERDADEIRO (passou do limite)"],
      ["R$ 50.000", "GT 10000", "✅ VERDADEIRO (muito acima, alerta crítico!)"],
    ],
    exemplosExtras: [
      {
        titulo: "Estruturação (Smurfing)",
        cenario: "Fraudador divide R$15.000 em várias transações pequenas",
        codigo: "SUM(transactions.amount, last_24h, customer_id) GT 10000",
        resultado: "10 x R$1.500 = R$15.000 dispara alerta!",
      },
      {
        titulo: "Limite de saque diário",
        cenario: "Controlar saques em caixas eletrônicos",
        codigo: "SUM(withdrawals.amount, last_24h, card_id) GT 5000",
        resultado: "Soma de saques > R$5.000/dia bloqueia novos saques",
      },
      {
        titulo: "Limite mensal por carteira",
        cenario: "Controle de limites em carteiras digitais",
        codigo: "SUM(transactions.amount, last_30d, wallet_id) GT 50000",
        resultado: "Carteira com movimentação > R$50k/mês vai para análise",
      },
    ],
    errosComuns: [
      "❌ Esquecer de considerar moeda: SUM de BRL + USD dá resultado errado sem conversão",
      "❌ Usar campo errado: somar 'quantity' em vez de 'amount'",
      "❌ Janela muito curta: last_1h pode não capturar smurfing ao longo do dia",
      "❌ Não considerar estornos: transações estornadas ainda contam na soma?",
    ],
    operadoresRelacionados: ["SUM_GTE", "SUM_LT", "SUM_LTE", "COUNT_GT", "AVG_GT"],
    checklistUso: [
      "☐ Qual campo você quer SOMAR? (amount, quantity)",
      "☐ Em qual JANELA de tempo? (24h para diário, 30d para mensal)",
      "☐ Agrupar por quê? (customer, card, wallet)",
      "☐ Qual o limite regulatório ou de negócio?",
    ],
    exercicio: {
      pergunta: "Crie uma regra para detectar saques acima de R$3.000 por cartão em 24 horas",
      resposta: "SUM(withdrawals.amount, last_24h, card_id) GT 3000",
    },
    
    // 🏭 COMPORTAMENTO NO MOTOR DE REGRAS
    comportamentoMotor: {
      descricao: "O motor consulta o histórico, agrupa eventos pela chave, SOMA os valores do campo especificado na janela temporal, e compara com o limite.",
      passos: [
        "1️⃣ Motor recebe a transação atual (ex: R$500 do customer_123)",
        "2️⃣ Consulta histórico: 'qual a soma de amount para customer_123 nas últimas 24h?'",
        "3️⃣ Banco retorna: R$9.600 (soma das transações anteriores)",
        "4️⃣ Motor soma com transação atual: R$9.600 + R$500 = R$10.100",
        "5️⃣ Compara: R$10.100 GT R$10.000? SIM → dispara",
        "6️⃣ Ação é executada: BLOCK, REVIEW, FLAG, etc.",
      ],
      performance: "⚡ SUM requer agregação no banco. Use índices compostos (customer_id + created_at). Pre-agregar em janelas fixas pode ajudar.",
      cuidados: [
        "Conversão de moeda: some na moeda base para evitar erros",
        "Estornos: decida se devem subtrair da soma ou não",
        "Valores negativos: créditos/estornos podem diminuir a soma",
        "Considere usar SUM_GTE se o limite deve ser inclusive",
      ],
    },
    
    // 🎬 SITUAÇÕES REAIS
    situacoesReais: [
      {
        titulo: "Smurfing (estruturação) para evadir detecção",
        contexto: "Fraudador/lavador divide R$50.000 em 50 transações de R$1.000 para evitar alerta de valor alto",
        problema: "Cada transação individual (R$1.000) não dispara regra de valor alto (GT 5.000)",
        solucao: "SUM(transactions.amount, last_24h, customer_id) GT 10000",
        impacto: "💰 Detecta 85% dos casos de estruturação que passariam despercebidos",
      },
      {
        titulo: "Limite de transferência PIX diário",
        contexto: "BACEN exige limite de R$1.000 para PIX noturno por segurança",
        problema: "Cliente pode fazer 10 PIX de R$200 = R$2.000 (burla o limite unitário)",
        solucao: "SUM(pix.amount, last_24h, customer_id) GT 1000 AND hour BETWEEN 20 AND 6",
        impacto: "📋 Conformidade 100% com regulação BACEN de PIX noturno",
      },
      {
        titulo: "Controle de saque em ATM",
        contexto: "Cartão clonado sendo usado para sacar em múltiplos ATMs",
        problema: "Fraudador faz vários saques pequenos para não disparar alerta unitário",
        solucao: "SUM(withdrawals.amount, last_24h, card_id) GT 3000",
        impacto: "🏧 Bloqueia cartão após R$3k em saques/dia, mesmo que distribuídos",
      },
    ],
    
    // 📊 RESULTADOS POSSÍVEIS
    resultadosPossiveis: {
      quandoDispara: "🚨 A SOMA de valores na janela EXCEDE o limite - possível estruturação/smurfing ou limite de operação ultrapassado",
      quandoNaoDispara: "✅ A soma está DENTRO do limite - operações dentro do esperado para o período",
      acaoRecomendada: "Configure thresholds escalonados: >10k = FLAG, >30k = REVIEW, >100k = BLOCK + SAR",
    },
    
    // 🔧 COMO TESTAR
    comoTestar: [
      "📝 Teste 1: Envie transações que somem MENOS que o limite → não dispara",
      "📝 Teste 2: Envie transações que EXATAMENTE igualam o limite → não dispara (GT é maior, não igual)",
      "📝 Teste 3: Envie uma transação que faça a soma PASSAR do limite → dispara",
      "📝 Teste 4: Teste com estornos (se aplicável) para ver se subtraem da soma",
      "📝 Teste 5: Espere a janela expirar e verifique se a soma reseta",
    ],
  },
};

// ─────────────────────────────────────────────────────────────────────────────
// FALLBACK para operadores sem exemplo Head First específico
// ─────────────────────────────────────────────────────────────────────────────

const derivePurpose = (operator: Operator): string => {
  const comment = operator.comment?.trim();
  if (comment) return comment;

  const name = operator.name.toUpperCase();

  if (["AND", "OR", "NOT", "NAND", "NOR", "XOR"].includes(name)) return "Combinar condições lógicas.";
  if (name.includes("BETWEEN")) return "Validar se um valor está dentro de uma faixa.";
  if (name.includes("CONTAINS") || name.includes("REGEX") || name.includes("STARTS_WITH") || name.includes("ENDS_WITH"))
    return "Verificar padrões ou trechos em texto.";
  if (name.includes("IN_LIST") || name.includes("NOT_IN") || name.endsWith("_IN") || name === "IN")
    return "Checar pertencimento a uma lista.";
  if (name.includes("COUNT") || name.includes("SUM") || name.includes("AVG") || name.includes("MAX") || name.includes("MIN") || name.includes("PERCENT"))
    return "Calcular agregações e indicadores.";
  if (name.includes("GT") || name.includes("GTE") || name.includes("LT") || name.includes("LTE") || name.includes("EQ") || name.includes("NEQ"))
    return "Comparar valores e limites.";
  if (name.startsWith("IS_") || name.includes("NULL")) return "Validar estado ou presença de dados.";
  if (name.includes("DATE") || name.includes("DAY") || name.includes("HOUR") || name.includes("WEEK") || name.includes("TIME"))
    return "Aplicar regras de tempo e calendário.";
  if (name.includes("GEO") || name.includes("DISTANCE")) return "Validar localização e distância.";
  if (name.includes("DEVICE") || name.includes("FINGERPRINT")) return "Verificar sinais do dispositivo.";
  if (name.startsWith("FATF_")) return "Aplicar tipologias e controles de AML.";
  if (name.startsWith("SCA_") || name.startsWith("PSD") || name.startsWith("DORA"))
    return "Aplicar requisitos regulatórios de autenticação e resiliência.";
  if (name.startsWith("BSL_")) return "Aplicar políticas de risco operacional.";
  if (name.startsWith("NEO4J_")) return "Analisar relações em grafo.";
  if (name.startsWith("PLT_")) return "Aplicar boas práticas de plataforma.";
  if (name.includes("MCC")) return "Avaliar categoria do merchant (MCC).";
  if (name.includes("EMAIL") || name.includes("PHONE") || name.includes("CPF")) return "Validar dados cadastrais.";
  if (name.includes("AMOUNT")) return "Avaliar comportamento de valor da transação.";
  if (name.includes("CARD")) return "Avaliar dados e uso do cartão.";
  if (name.includes("VELOCITY")) return "Medir velocidade de eventos ou valores.";

  return `Operador da categoria ${normalizeCategory(operator.category)}.`;
};

// ─────────────────────────────────────────────────────────────────────────────
// 🎯 ANALOGIAS ESPECÍFICAS POR TIPO DE OPERADOR
// ─────────────────────────────────────────────────────────────────────────────
const ANALOGIAS_POR_TIPO: Record<OperatorKind, { analogia: string; personagem: string; dicaDeOuro: string }> = {
  logical: {
    analogia: "🧠 Como um juiz que avalia múltiplas evidências: com AND todas precisam ser verdadeiras, com OR basta uma, com NOT inverte o veredicto.",
    personagem: "👨‍⚖️ Juiz de Regras",
    dicaDeOuro: "💎 Combine operadores lógicos para criar regras sofisticadas sem duplicação.",
  },
  compare: {
    analogia: "⚖️ Como uma balança de precisão: coloque o valor do campo de um lado e o limite do outro. A balança mostra se é maior, menor ou igual.",
    personagem: "👩‍🔬 Cientista de Dados",
    dicaDeOuro: "💎 Lembre-se: GT exclui o limite, GTE inclui. Na dúvida, pergunte: o limite é válido?",
  },
  range: {
    analogia: "📏 Como uma régua com duas marcações: o valor precisa estar entre elas. BETWEEN inclui as marcas, NOT_BETWEEN captura o que está fora.",
    personagem: "👨‍💼 Gerente de Limites",
    dicaDeOuro: "💎 Use BETWEEN para faixas de valor, idade, score. Muito mais legível que AND + AND.",
  },
  list: {
    analogia: "📋 Como uma lista de convidados VIP: IN verifica se o nome está na lista, NOT_IN verifica se está na lista negra.",
    personagem: "👮 Segurança da Festa",
    dicaDeOuro: "💎 Mantenha suas listas em cadastros do sistema para atualizá-las sem mudar as regras.",
  },
  string: {
    analogia: "🔍 Como um detetive com lupa examinando texto: CONTAINS busca pistas no meio, STARTS/ENDS verifica início/fim, REGEX usa padrões complexos.",
    personagem: "🕵️ Detetive de Padrões",
    dicaDeOuro: "💎 Use REGEX com cuidado - é poderoso mas pode ser lento. Prefira CONTAINS quando possível.",
  },
  null: {
    analogia: "📭 Como verificar uma caixa de correio: IS_NULL = está vazia, NOT_NULL = tem algo dentro. Essencial para dados opcionais.",
    personagem: "📮 Carteiro de Dados",
    dicaDeOuro: "💎 Sempre considere: e se esse campo vier vazio? Use IS_NULL para tratar o caso.",
  },
  boolean: {
    analogia: "💡 Como um interruptor de luz: só tem dois estados - ligado (true) ou desligado (false). Simples e binário.",
    personagem: "🔌 Eletricista Lógico",
    dicaDeOuro: "💎 IS_TRUE/IS_FALSE são mais legíveis que campo = true. Use para flags e configurações.",
  },
  array: {
    analogia: "🛒 Como um carrinho de compras: ARRAY_CONTAINS verifica se tem leite, ARRAY_SIZE conta quantos itens tem.",
    personagem: "🛍️ Conferente de Listas",
    dicaDeOuro: "💎 Use operadores de array quando o campo é uma lista (tags, itens, IDs).",
  },
  datetime: {
    analogia: "⏰ Como um calendário inteligente: verifica datas, horários, idade de registros, janelas de tempo. O tempo é crucial em fraude!",
    personagem: "📅 Guardião do Tempo",
    dicaDeOuro: "💎 Transações de madrugada e contas recém-criadas são sinais clássicos de risco.",
  },
  aggregation: {
    analogia: "📊 Como um contador automático: COUNT soma quantas vezes, SUM soma valores, AVG calcula média. Essencial para velocity!",
    personagem: "🧮 Matemático de Fraude",
    dicaDeOuro: "💎 Agregações são a base de regras de velocity. Fraudadores fragmentam - você soma!",
  },
  risk_pattern: {
    analogia: "🎯 Como um radar de fraude: detecta padrões suspeitos automaticamente - velocity spikes, comportamento anômalo, sinais de AML.",
    personagem: "🛡️ Sentinela Antifraude",
    dicaDeOuro: "💎 Estes operadores encapsulam conhecimento especialista. Use-os para regras avançadas.",
  },
  graph: {
    analogia: "🕸️ Como um mapa de conexões: mostra quem está ligado a quem. Essencial para detectar redes de fraude e conluios.",
    personagem: "🔗 Analista de Redes",
    dicaDeOuro: "💎 Grafos revelam conexões invisíveis: mesmo dispositivo, mesmo endereço, mesma rede.",
  },
  device: {
    analogia: "📱 Como um perito forense de dispositivos: analisa fingerprint, detecta jailbreak, avalia trust score. O dispositivo conta a verdade!",
    personagem: "🔬 Perito Digital",
    dicaDeOuro: "💎 Dispositivos adulterados (root/jailbreak) e emuladores são red flags importantes.",
  },
  identity: {
    analogia: "🪪 Como um verificador de documentos: valida CPF, e-mail, telefone, endereço. Dados cadastrais falsos são sinal de fraude.",
    personagem: "👤 Verificador de Identidade",
    dicaDeOuro: "💎 E-mails temporários, telefones VoIP e CPFs inválidos são sinais clássicos.",
  },
  merchant: {
    analogia: "🏪 Como um inspetor de estabelecimentos: avalia MCC, categoria, histórico do merchant. Alguns MCCs são de alto risco!",
    personagem: "🔎 Inspetor de Merchants",
    dicaDeOuro: "💎 MCCs de gambling, crypto e gift cards merecem atenção especial.",
  },
  platform: {
    analogia: "🏛️ Como um auditor de compliance: verifica DORA, GDPR, eIDAS. Regulamentação é obrigatória, não opcional!",
    personagem: "📋 Auditor Regulatório",
    dicaDeOuro: "💎 Mantenha-se atualizado com regulamentações - multas podem ser severas.",
  },
  validation: {
    analogia: "✅ Como um checklist de aprovação: verifica sanções, PEP, adverse media. Cada verificação é um carimbo necessário.",
    personagem: "✔️ Validador Oficial",
    dicaDeOuro: "💎 Listas de sanções (OFAC, EU) são obrigatórias. Automatize essas verificações.",
  },
  statistical: {
    analogia: "📈 Como um cientista de dados: detecta anomalias, calcula desvios, aplica testes estatísticos. Números não mentem!",
    personagem: "📊 Estatístico de Fraude",
    dicaDeOuro: "💎 Machine learning e estatística encontram padrões que regras simples não pegam.",
  },
  unknown: {
    analogia: "🔧 Operador especializado para cenários específicos. Consulte a documentação técnica para entender seu uso exato.",
    personagem: "👤 Especialista Técnico",
    dicaDeOuro: "💎 Teste sempre em ambiente de homologação antes de usar em produção.",
  },
};

// Gera história contextualizada baseada no nome do operador
const gerarHistoriaContextualizada = (name: string, kind: OperatorKind): string => {
  const upper = name.toUpperCase();
  const tokens = tokensToSet(upper);
  const ctx = inferTokenContext(upper);

  const actionPt =
    tokens.has("CHANGE") ? "mudança" :
    tokens.has("SWITCH") ? "troca" :
    tokens.has("REUSE") ? "reuso" :
    tokens.has("ADD") ? "adição" :
    tokens.has("CAPTURE") ? "captura" :
    tokens.has("TAKEOVER") ? "sequestro" :
    tokens.has("SPIKE") ? "pico" :
    tokens.has("DEVIATION") ? "desvio" :
    tokens.has("ANOMALY") ? "anomalia" :
    tokens.has("RING") ? "anel (rede)" :
    tokens.has("LINK") ? "ligação" :
    undefined;

  const topic = actionPt ? `${actionPt} de ${ctx.entityPt}` : ctx.entityPt;
  
  // Detectar contexto pelo nome do operador (token-based; menos repetição)
  if (tokens.has("VELOCITY") || tokens.has("COUNT") || tokens.has("SUM") || tokens.has("AVG")) {
    return `Maria, analista de fraude, precisa medir ${topic} ao longo do tempo. O operador ${name} ajuda a transformar histórico em um número (contagem/soma/velocidade) para detectar comportamento fora do normal.`;
  }
  if (tokens.has("DEVICE") || tokens.has("FINGERPRINT") || tokens.has("BROWSER") || tokens.has("USER_AGENT")) {
    return `Carlos, especialista em segurança, precisa avaliar a confiabilidade do ${topic}. O operador ${name} destaca sinais técnicos (fingerprint, inconsistência, automação) que um olho humano não percebe facilmente.`;
  }
  if (tokens.has("EMAIL") || tokens.has("PHONE") || tokens.has("CPF") || tokens.has("ADDRESS") || tokens.has("IDENTITY")) {
    return `Ana, do onboarding, precisa validar ${topic} antes de aprovar o cliente. O operador ${name} ajuda a detectar dados falsos, temporários ou inconsistentes.`;
  }
  if (tokens.has("MERCHANT") || tokens.has("MCC") || tokens.has("STORE")) {
    return `Pedro, analista de pagamentos, precisa entender o risco do ${topic}. O operador ${name} ajuda a aplicar regras diferentes por tipo de estabelecimento e comportamento.`;
  }
  if (tokens.has("DATE") || tokens.has("TIME") || tokens.has("DAY") || tokens.has("HOUR") || tokens.has("WEEK") || tokens.has("MONTH") || tokens.has("YEAR")) {
    return `Fernanda, do monitoramento, precisa criar regras baseadas em tempo: ${topic}. O operador ${name} permite capturar padrões como "madrugada", "fim de semana" e "conta recente".`;
  }
  if (tokens.has("GRAPH") || tokens.has("NEO4J") || tokens.has("NETWORK") || tokens.has("LINK") || tokens.has("RING") || tokens.has("CLUSTER")) {
    return `Ricardo, investigador de fraude, precisa mapear conexões relacionadas a ${topic}. O operador ${name} ajuda a revelar relações indiretas (mesmo device, mesmo endereço, mesma rede) que indicam conluio.`;
  }
  if (tokens.has("SANCTION") || tokens.has("PEP") || tokens.has("ADVERSE") || tokens.has("FATF") || tokens.has("GDPR") || tokens.has("DORA") || tokens.has("EIDAS")) {
    return `Juliana, do compliance, precisa validar requisitos regulatórios ligados a ${topic}. O operador ${name} automatiza checagens que seriam manuais e sujeitas a erro.`;
  }
  if (tokens.has("ANOMALY") || tokens.has("DEVIATION") || tokens.has("TEST") || tokens.has("SCORE") || tokens.has("ADAPTIVE")) {
    return `Marcos, cientista de dados, precisa detectar ${topic} com base em estatística/modelos. O operador ${name} ajuda a sinalizar outliers e mudanças abruptas de padrão.`;
  }
  
  // Fallback baseado no kind
  const kindHistorias: Record<OperatorKind, string> = {
    logical: `Um analista precisa combinar múltiplas condições em uma regra complexa. O operador ${name} permite conectar condições de forma lógica.`,
    compare: `Um gerente de risco precisa definir limites para transações. O operador ${name} compara valores com precisão.`,
    range: `Uma analista precisa verificar se valores estão dentro de faixas aceitáveis. O operador ${name} valida intervalos.`,
    list: `Um especialista precisa verificar valores contra listas conhecidas. O operador ${name} facilita essa validação.`,
    string: `Um investigador precisa analisar padrões em textos. O operador ${name} busca e valida strings.`,
    null: `Um analista precisa tratar campos opcionais. O operador ${name} detecta dados ausentes.`,
    boolean: `Um desenvolvedor precisa avaliar flags de configuração. O operador ${name} trabalha com valores true/false.`,
    array: `Uma analista precisa verificar conteúdo de listas. O operador ${name} opera sobre arrays.`,
    datetime: `Um monitor precisa criar regras baseadas em tempo. O operador ${name} avalia datas e horários.`,
    aggregation: `Um especialista precisa calcular métricas agregadas. O operador ${name} realiza cálculos sobre conjuntos.`,
    risk_pattern: `Um analista de fraude precisa detectar padrões de risco. O operador ${name} identifica sinais suspeitos.`,
    graph: `Um investigador precisa mapear redes de relacionamento. O operador ${name} analisa conexões em grafos.`,
    device: `Um especialista de segurança precisa avaliar dispositivos. O operador ${name} analisa características do device.`,
    identity: `Um verificador precisa validar dados de identidade. O operador ${name} checa informações cadastrais.`,
    merchant: `Um analista de pagamentos precisa avaliar merchants. O operador ${name} verifica estabelecimentos.`,
    platform: `Um auditor precisa garantir compliance. O operador ${name} verifica requisitos regulatórios.`,
    validation: `Um verificador precisa checar listas e validações. O operador ${name} automatiza verificações.`,
    statistical: `Um cientista de dados precisa aplicar análises. O operador ${name} usa métodos estatísticos.`,
    unknown: `Um especialista precisa aplicar uma verificação específica. O operador ${name} atende esse cenário.`,
  };
  
  return kindHistorias[kind];
};

// Gera problema contextualizado
const gerarProblemaContextualizado = (name: string, kind: OperatorKind): string => {
  const upper = name.toUpperCase();
  const tokens = tokensToSet(upper);
  const ctx = inferTokenContext(upper);

  const actionPt =
    tokens.has("CHANGE") ? "mudança" :
    tokens.has("SWITCH") ? "troca" :
    tokens.has("REUSE") ? "reuso" :
    tokens.has("TAKEOVER") ? "sequestro de conta" :
    tokens.has("CAPTURE") ? "captura" :
    tokens.has("SPIKE") ? "pico" :
    tokens.has("ANOMALY") ? "anomalia" :
    tokens.has("DEVIATION") ? "desvio" :
    undefined;

  const topic = actionPt ? `${actionPt} de ${ctx.entityPt}` : ctx.entityPt;
  
  if (tokens.has("VELOCITY")) return `Como detectar ${topic} em alta frequência (sinal de automação/fraude)?`;
  if (tokens.has("COUNT")) return `Como contar eventos relacionados a ${topic} dentro de uma janela de tempo?`;
  if (tokens.has("SUM")) return `Como somar valores relacionados a ${topic} para detectar fragmentação/estruturação (smurfing)?`;
  if (tokens.has("DEVICE") || tokens.has("FINGERPRINT") || tokens.has("BROWSER")) return `Como avaliar se o ${topic} é confiável ou suspeito?`;
  if (tokens.has("EMAIL")) return `Como validar se o e-mail é legítimo ou temporário/descartável?`;
  if (tokens.has("PHONE")) return `Como verificar se o telefone é real ou descartável/VoIP?`;
  if (tokens.has("MERCHANT") || tokens.has("MCC")) return `Como avaliar o risco do merchant (categoria/MCC/comportamento)?`;
  if (tokens.has("GRAPH") || tokens.has("NEO4J") || tokens.has("NETWORK") || tokens.has("LINK") || tokens.has("RING")) return `Como descobrir conexões ocultas relacionadas a ${topic}?`;
  if (tokens.has("SANCTION") || tokens.has("PEP") || tokens.has("ADVERSE") || tokens.has("FATF")) return `Como automatizar checagens obrigatórias de compliance relacionadas a ${topic}?`;
  if (tokens.has("ANOMALY") || tokens.has("DEVIATION") || tokens.has("TEST") || tokens.has("ADAPTIVE") || tokens.has("SCORE")) return `Como detectar ${topic} que foge do padrão estatístico/modelo?`;
  
  const kindProblemas: Record<OperatorKind, string> = {
    logical: "Como combinar múltiplas condições de forma eficiente?",
    compare: "Como definir limites precisos para valores?",
    range: "Como verificar se um valor está em uma faixa aceitável?",
    list: "Como verificar valores contra listas conhecidas?",
    string: "Como encontrar padrões em dados textuais?",
    null: "Como tratar campos que podem estar vazios?",
    boolean: "Como avaliar flags de forma clara e legível?",
    array: "Como trabalhar com campos que contêm listas?",
    datetime: "Como criar regras baseadas em tempo e calendário?",
    aggregation: "Como calcular métricas sobre múltiplos eventos?",
    risk_pattern: "Como detectar padrões de risco automaticamente?",
    graph: "Como identificar redes e conexões suspeitas?",
    device: "Como avaliar a confiabilidade do dispositivo?",
    identity: "Como validar dados cadastrais do cliente?",
    merchant: "Como avaliar o risco do estabelecimento?",
    platform: "Como garantir conformidade regulatória?",
    validation: "Como automatizar verificações de compliance?",
    statistical: "Como aplicar análises estatísticas na detecção?",
    unknown: `Como aplicar o operador ${name} corretamente?`,
  };
  
  return kindProblemas[kind];
};

const deriveHeadFirstExample = (name: string): HeadFirstExample => {
  const upper = name.toUpperCase();
  const found = HEAD_FIRST_EXAMPLES[name] || HEAD_FIRST_EXAMPLES[upper];
  if (found) {
    return {
      ...found,
      docLevel: "manual",
      docConfidence: "high",
      docWarnings: [],
    };
  }

  const spec = OPERATOR_SPECS[upper];
  if (spec) {
    const kind = classifyOperator(name);
    const info = ANALOGIAS_POR_TIPO[kind];
    const sintaxeGerada = spec.syntax ?? guessDslForKind(name, kind);
    return {
      docLevel: "spec",
      docConfidence: "high",
      docWarnings: [],
      historia: spec.story ?? gerarHistoriaContextualizada(name, kind),
      personagem: info.personagem,
      problema: spec.problem ?? gerarProblemaContextualizado(name, kind),
      analogia: spec.analogy ?? info.analogia,
      passoAPasso: spec.stepByStep ?? [
        `1️⃣ Identifique o campo relevante para o operador ${name}`,
        `2️⃣ Aplique ${name} com os parâmetros apropriados`,
        "3️⃣ Configure valores/limites baseados no seu cenário",
        "4️⃣ Teste com dados reais antes de publicar",
      ],
      antes: spec.before ?? `❌ ANTES: Sem ${name}, você precisaria de lógica mais complexa ou manual para este cenário.`,
      depois: spec.after ?? `✅ DEPOIS: Com ${name}, a regra fica direta, eficiente e fácil de manter.`,
      sintaxe: sintaxeGerada,
      explicacaoSintaxe: spec.syntaxExplanation ?? gerarExplicacaoSintaxeUnica(name, kind, sintaxeGerada),
      perguntaComum: spec.commonQuestion ?? gerarProblemaContextualizado(name, kind),
      respostaPergunta: spec.commonAnswer ?? `Use ${name} quando precisar de ${kind === "unknown" ? "verificação especializada" : kind.replace("_", " ")}. Veja os campos sugeridos e exemplos nesta página.`,
      dicaDeOuro: spec.goldenTip ?? info.dicaDeOuro,
      comportamentoMotor: spec.engineBehavior
        ? {
            descricao: spec.engineBehavior.description,
            passos: spec.engineBehavior.steps,
            performance: spec.engineBehavior.performance,
            cuidados: spec.engineBehavior.cautions,
          }
        : undefined,
      situacoesReais: spec.realScenarios
        ? spec.realScenarios.map((s) => ({
            titulo: s.title,
            contexto: s.context,
            problema: s.problem,
            solucao: s.solution,
            impacto: s.impact,
          }))
        : undefined,
      resultadosPossiveis: spec.possibleOutcomes
        ? {
            quandoDispara: spec.possibleOutcomes.whenTrue,
            quandoNaoDispara: spec.possibleOutcomes.whenFalse,
            acaoRecomendada: spec.possibleOutcomes.recommendedAction,
          }
        : undefined,
      comoTestar: spec.howToTest,
    };
  }

  // Gerar exemplo contextualizado baseado na classificação
  const kind = classifyOperator(name);
  const info = ANALOGIAS_POR_TIPO[kind];
  const sintaxeGerada = guessDslForKind(name, kind);
  const meta = docMetaForOperator(name);
  
  return {
    docLevel: meta.level,
    docConfidence: meta.confidence,
    docWarnings: meta.warnings,
    historia: gerarHistoriaContextualizada(name, kind),
    personagem: info.personagem,
    problema: gerarProblemaContextualizado(name, kind),
    analogia: info.analogia,
    passoAPasso: [
      `1️⃣ Identifique o campo relevante para o operador ${name}`,
      `2️⃣ Aplique ${name} com os parâmetros apropriados`,
      "3️⃣ Configure valores/limites baseados no seu cenário",
      "4️⃣ Teste com dados reais antes de publicar",
    ],
    antes: `❌ ANTES: Sem ${name}, você precisaria de lógica mais complexa ou manual para este cenário.`,
    depois: `✅ DEPOIS: Com ${name}, a regra fica direta, eficiente e fácil de manter.`,
    sintaxe: sintaxeGerada,
    explicacaoSintaxe: gerarExplicacaoSintaxeUnica(name, kind, sintaxeGerada),
    perguntaComum: gerarProblemaContextualizado(name, kind),
    respostaPergunta: `Use ${name} quando precisar de ${kind === "unknown" ? "verificação especializada" : kind.replace("_", " ")}. Veja os campos sugeridos e exemplos nesta página.`,
    dicaDeOuro: info.dicaDeOuro,
  };
};

const CATEGORY_GUIDE: Record<string, { title: string; emoji: string; intro: string; analogia: string }> = {
  "Comparação básica": {
    title: "Comparações Simples",
    emoji: "⚖️",
    intro: "Os operadores mais usados! Compare valores, verifique igualdade, defina limites.",
    analogia: "Como uma balança: um lado tem o campo, outro lado tem o valor. A balança mostra se são iguais, qual é maior, etc.",
  },
  Listas: {
    title: "Listas (IN / NOT IN)",
    emoji: "📋",
    intro: "Verifique se um valor está (ou não) em uma lista de opções.",
    analogia: "Como uma lista de convidados: se o nome está na lista, entra. Se não está, fica de fora.",
  },
  Strings: {
    title: "Texto e Padrões",
    emoji: "🔤",
    intro: "Trabalhe com texto: busque trechos, verifique início/fim, use regex.",
    analogia: "Como o Ctrl+F do computador: você busca um texto dentro de outro texto.",
  },
  Nulos: {
    title: "Campos Vazios",
    emoji: "❓",
    intro: "Detecte campos não preenchidos ou garanta que estão preenchidos.",
    analogia: "Como verificar se uma caixa está vazia ou tem algo dentro.",
  },
  Booleanos: {
    title: "Verdadeiro / Falso",
    emoji: "🔘",
    intro: "Trabalhe com flags que só podem ser true ou false.",
    analogia: "Como um interruptor: ligado (true) ou desligado (false).",
  },
  Range: {
    title: "Faixas (Between)",
    emoji: "📊",
    intro: "Verifique se um valor está dentro ou fora de um intervalo.",
    analogia: "Como uma faixa de preço: 'entre R$100 e R$500'.",
  },
  "Comparação entre campos": {
    title: "Campo vs Campo",
    emoji: "🔄",
    intro: "Compare dois campos do mesmo registro entre si.",
    analogia: "Como comparar duas colunas de uma planilha na mesma linha.",
  },
  "Funções de data/tempo": {
    title: "Datas e Horários",
    emoji: "📅",
    intro: "Regras baseadas em tempo: datas, horários, idade de registros.",
    analogia: "Como um calendário ou relógio que você consulta para tomar decisões.",
  },
  "Funções de lista/array": {
    title: "Arrays",
    emoji: "🗃️",
    intro: "Trabalhe com listas: verifique conteúdo, meça tamanho.",
    analogia: "Como um carrinho de compras: quantos itens tem? Tem leite?",
  },
  "Funções matemáticas": {
    title: "Matemática",
    emoji: "🧮",
    intro: "Cálculos: diferenças, percentuais, valores absolutos.",
    analogia: "Como uma calculadora para suas regras.",
  },
  Geolocalização: {
    title: "Localização",
    emoji: "🌍",
    intro: "Regras baseadas em geografia: país, cidade, distância.",
    analogia: "Como um mapa que mostra onde as coisas acontecem.",
  },
  "Operadores lógicos": {
    title: "Lógica (AND/OR/NOT)",
    emoji: "🧠",
    intro: "A cola que une tudo! Combine múltiplas condições.",
    analogia: "Como conectar peças de Lego: você junta várias condições em uma regra.",
  },
  // ═══════════════════════════════════════════════════════════════════════════
  // NOVAS CATEGORIAS EXPANDIDAS (após double-check rigoroso)
  // ═══════════════════════════════════════════════════════════════════════════
  device: {
    title: "Dispositivo & Fingerprint",
    emoji: "📱",
    intro: "Operadores que avaliam características do dispositivo: fingerprint, browser, jailbreak, trust score.",
    analogia: "Como um detetive examinando a 'identidade' do aparelho que está fazendo a transação.",
  },
  identity: {
    title: "Identidade & Cadastro",
    emoji: "👤",
    intro: "Validações de dados pessoais: e-mail, telefone, CPF, endereço, biometria.",
    analogia: "Como verificar documentos antes de aprovar alguém.",
  },
  merchant: {
    title: "Merchant & MCC",
    emoji: "🏪",
    intro: "Operadores relacionados ao comerciante: MCC, categoria, risco do estabelecimento.",
    analogia: "Como avaliar se a loja onde a compra foi feita é confiável.",
  },
  platform: {
    title: "Plataforma & Compliance",
    emoji: "🏛️",
    intro: "Operadores regulatórios: DORA, eIDAS, GDPR, controles de plataforma.",
    analogia: "Como um checklist de auditoria para garantir que tudo está em conformidade.",
  },
  validation: {
    title: "Validações & Verificações",
    emoji: "✅",
    intro: "Checagens específicas: sanções, PEP, adverse media, verificações cadastrais.",
    analogia: "Como passar um documento por vários carimbos de aprovação.",
  },
  statistical: {
    title: "Estatísticas & ML",
    emoji: "📈",
    intro: "Operadores estatísticos e de machine learning: scores, desvios, testes, thresholds adaptativos.",
    analogia: "Como um cientista de dados analisando padrões nos números.",
  },
  graph: {
    title: "Grafos & Redes",
    emoji: "🕸️",
    intro: "Análise de conexões: Neo4j, detecção de anéis de fraude, centralidade.",
    analogia: "Como um mapa de relacionamentos mostrando quem está conectado a quem.",
  },
  risk_pattern: {
    title: "Padrões de Risco",
    emoji: "🎯",
    intro: "Detecção de fraude e AML: velocity, anomalias, FATF, SCA, BSL.",
    analogia: "Como um radar que detecta comportamentos suspeitos automaticamente.",
  },
  Geral: {
    title: "Outros Operadores",
    emoji: "🔧",
    intro: "Operadores variados para cenários específicos.",
    analogia: "Ferramentas especializadas para casos especiais.",
  },
};

const getCategoryGuide = (category: string) =>
  CATEGORY_GUIDE[category] ?? {
    title: category,
    emoji: "📦",
    intro: `Operadores da categoria: ${category}.`,
    analogia: "Consulte cada operador para entender o uso específico.",
  };

// ═══════════════════════════════════════════════════════════════════════════════
// 🎨 COMPONENTE PRINCIPAL
// ═══════════════════════════════════════════════════════════════════════════════

export default function Operators() {
  const [expandedOperator, setExpandedOperator] = useState<string | null>(null);
  const [searchTerm, setSearchTerm] = useState("");
  const [strictDocs, setStrictDocs] = useState(false);

  const operatorNames = BACKEND_OPERATORS.map((o) => o.name);
  const uniqueNameCount = new Set(operatorNames).size;
  const duplicates = (() => {
    const counts = operatorNames.reduce<Record<string, number>>((acc, n) => {
      acc[n] = (acc[n] ?? 0) + 1;
      return acc;
    }, {});
    return Object.entries(counts)
      .filter(([, c]) => c > 1)
      .map(([n, c]) => `${n} (${c}x)`)
      .slice(0, 10);
  })();

  const categoryCounts = BACKEND_OPERATORS.reduce<Record<string, number>>((acc, op) => {
    const cat = normalizeCategory(op.category);
    acc[cat] = (acc[cat] ?? 0) + 1;
    return acc;
  }, {});
  const categoriesTotal = Object.keys(categoryCounts).length;

  const operators = BACKEND_OPERATORS.map((operator) => ({
    ...operator,
    type: normalizeCategory(operator.category),
    purpose: derivePurpose(operator),
    headFirst: deriveHeadFirstExample(operator.name),
    didactic: deriveDidacticKit(operator),
    explainName: explainOperatorName(operator.name),
  }));

  const headFirstCoverage = operators.filter((o) => Boolean(HEAD_FIRST_EXAMPLES[o.name] || HEAD_FIRST_EXAMPLES[o.name.toUpperCase()])).length;
  const specCoverage = operators.filter((o) => Boolean(OPERATOR_SPECS[o.name.toUpperCase()])).length;
  const generatedCoverage = Math.max(0, operators.length - headFirstCoverage - specCoverage);

  const filteredOperators = searchTerm
    ? operators.filter(
        (op) =>
          op.name.toLowerCase().includes(searchTerm.toLowerCase()) ||
          op.purpose.toLowerCase().includes(searchTerm.toLowerCase()) ||
          op.type.toLowerCase().includes(searchTerm.toLowerCase())
      )
    : operators;

  const grouped = filteredOperators.reduce<Record<string, typeof operators>>((acc, op) => {
    acc[op.type] ??= [];
    acc[op.type].push(op);
    return acc;
  }, {});

  const categories = Object.keys(grouped).sort((a, b) => a.localeCompare(b, "pt-BR"));

  const toggleExpand = (name: string) => {
    setExpandedOperator(expandedOperator === name ? null : name);
  };

  return (
    <div className="space-y-6">
      {/* ═══════════════════════════════════════════════════════════════════════ */}
      {/* HEADER - BEM-VINDO AO GUIA */}
      {/* ═══════════════════════════════════════════════════════════════════════ */}
      <div className="rounded-xl border-2 border-blue-200 bg-gradient-to-r from-blue-50 to-indigo-50 p-6 dark:border-blue-800 dark:from-blue-950 dark:to-indigo-950">
        <div className="flex items-center justify-between gap-3">
          <div className="flex items-center gap-3">
            <span className="text-4xl">🧠</span>
            <div>
              <h1 className="text-2xl font-bold text-foreground">Guia de Operadores - Estilo "Use a Cabeça"</h1>
              <p className="text-sm text-muted-foreground">
                Aprenda cada operador com histórias, analogias e exemplos do mundo real
              </p>
            </div>
          </div>
          <a
            href="/src/manual/COMO_USAR_OPERADORES.md"
            target="_blank"
            className="flex items-center gap-2 rounded-lg border-2 border-green-500 bg-green-50 px-4 py-2 text-sm font-bold text-green-800 transition-all hover:bg-green-100 dark:bg-green-950 dark:text-green-200"
          >
            <span className="text-2xl">📖</span>
            <div>
              <div>Como Usar</div>
              <div className="text-xs font-normal">Exemplos práticos do backend</div>
            </div>
          </a>
        </div>

        <div className="mt-4 grid gap-4 md:grid-cols-2">
          {/* O que você vai aprender */}
          <div className="rounded-lg border bg-white/50 p-4 dark:bg-black/20">
            <div className="font-semibold text-foreground">📚 O que você vai aprender</div>
            <ul className="mt-2 space-y-1 text-sm text-muted-foreground">
              <li>✅ Quando usar cada operador (cenário real)</li>
              <li>✅ Como preencher os campos (passo a passo)</li>
              <li>✅ Sintaxe DSL (copie e cole)</li>
              <li>✅ Dicas de especialistas</li>
            </ul>
          </div>

          {/* Metodologia */}
          <div className="rounded-lg border bg-white/50 p-4 dark:bg-black/20">
            <div className="font-semibold text-foreground">🎯 Metodologia "Head First"</div>
            <ul className="mt-2 space-y-1 text-sm text-muted-foreground">
              <li>🎭 Histórias com personagens reais</li>
              <li>💡 Analogias do dia a dia</li>
              <li>❓ Perguntas que você teria vergonha de fazer</li>
              <li>⚠️ Antes vs Depois (ver a diferença)</li>
            </ul>
          </div>
        </div>

        {/* Barra de busca */}
        <div className="mt-4">
          <input
            type="text"
            placeholder="🔍 Buscar operador por nome, categoria ou descrição..."
            className="w-full rounded-lg border bg-white px-4 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-blue-500 dark:bg-slate-800"
            value={searchTerm}
            onChange={(e) => setSearchTerm(e.target.value)}
          />
        </div>

        <div className="mt-4 text-center text-sm text-muted-foreground">
          📊 {filteredOperators.length} operadores disponíveis
          {searchTerm && ` (filtrado de ${operators.length})`}
        </div>

        {/* Triple-check quick audit */}
        <div className="mt-4 rounded-lg border bg-white/60 p-4 text-xs text-muted-foreground dark:bg-black/20">
          <div className="flex flex-wrap gap-2">
            <span className="rounded-full bg-slate-100 px-2 py-1 dark:bg-slate-800">
              ✅ Total: <span className="font-semibold text-foreground">{BACKEND_OPERATORS.length}</span>
            </span>
            <span className="rounded-full bg-slate-100 px-2 py-1 dark:bg-slate-800">
              🧬 Únicos: <span className="font-semibold text-foreground">{uniqueNameCount}</span>
            </span>
            <span className="rounded-full bg-slate-100 px-2 py-1 dark:bg-slate-800">
              🗂️ Categorias: <span className="font-semibold text-foreground">{categoriesTotal}</span>
            </span>
            <span className="rounded-full bg-slate-100 px-2 py-1 dark:bg-slate-800">
              🎭 Head First: <span className="font-semibold text-foreground">{headFirstCoverage}</span> com histórias completas
            </span>
            <span className="rounded-full bg-slate-100 px-2 py-1 dark:bg-slate-800">
              📘 Spec: <span className="font-semibold text-foreground">{specCoverage}</span>
            </span>
            <span className="rounded-full bg-slate-100 px-2 py-1 dark:bg-slate-800">
              🤖 Gerado: <span className="font-semibold text-foreground">{generatedCoverage}</span>
            </span>
          </div>

          <div className="mt-3 flex flex-wrap items-center justify-between gap-3 rounded-md bg-slate-50 p-3 dark:bg-slate-900/30">
            <div className="text-xs">
              <div className="font-semibold text-foreground">🧾 Transparência</div>
              <div className="mt-0.5">
                Quando estiver marcado como <span className="font-semibold text-foreground">Gerado</span>, o texto é heurístico (porque o backend não traz descrição por operador).
              </div>
            </div>
            <label className="flex cursor-pointer items-center gap-2 rounded-full bg-slate-100 px-3 py-1 dark:bg-slate-800">
              <input type="checkbox" checked={strictDocs} onChange={(e) => setStrictDocs(e.target.checked)} />
              <span className="font-medium text-foreground">Modo rigoroso</span>
            </label>
          </div>

          {uniqueNameCount !== BACKEND_OPERATORS.length && (
            <div className="mt-3 rounded-md border-l-4 border-red-500 bg-red-50 p-3 text-red-700 dark:bg-red-950 dark:text-red-200">
              <div className="font-semibold">⚠️ Atenção: detectei nomes duplicados</div>
              <div className="mt-1">{duplicates.length ? duplicates.join(", ") : "Verifique a fonte gerada"}</div>
            </div>
          )}

          <details className="mt-3">
            <summary className="cursor-pointer select-none font-medium text-foreground">
              Ver distribuição por categoria
            </summary>
            <div className="mt-2 grid gap-2 sm:grid-cols-2 lg:grid-cols-3">
              {Object.entries(categoryCounts)
                .sort((a, b) => b[1] - a[1])
                .slice(0, 12)
                .map(([cat, count]) => (
                  <div key={cat} className="rounded-md bg-slate-100 p-2 dark:bg-slate-800">
                    <div className="flex items-center justify-between gap-2">
                      <span className="truncate text-foreground">{cat}</span>
                      <span className="font-semibold text-foreground">{count}</span>
                    </div>
                  </div>
                ))}
            </div>
            {categoriesTotal > 12 && (
              <div className="mt-2 text-muted-foreground">Mostrando top 12 de {categoriesTotal} categorias.</div>
            )}
          </details>
        </div>
      </div>

      {/* ═══════════════════════════════════════════════════════════════════════ */}
      {/* DICA INICIAL */}
      {/* ═══════════════════════════════════════════════════════════════════════ */}
      <div className="rounded-lg border-l-4 border-amber-500 bg-amber-50 p-4 dark:bg-amber-950">
        <div className="flex items-start gap-3">
          <span className="text-2xl">💡</span>
          <div>
            <div className="font-semibold text-amber-800 dark:text-amber-200">
              Dica: Clique em qualquer operador para expandir
            </div>
            <p className="mt-1 text-sm text-amber-700 dark:text-amber-300">
              Cada card tem uma versão resumida. Clique para ver a história completa, analogias,
              passo a passo e dicas de especialistas!
            </p>
          </div>
        </div>
      </div>

      {/* ═══════════════════════════════════════════════════════════════════════ */}
      {/* CATEGORIAS E OPERADORES */}
      {/* ═══════════════════════════════════════════════════════════════════════ */}
      {categories.map((category) => {
        const guide = getCategoryGuide(category);
        const list = grouped[category];

        return (
          <section key={category} className="space-y-4">
            {/* Category header */}
            <div className="rounded-xl border-2 bg-card p-5">
              <div className="flex items-center gap-3">
                <span className="text-3xl">{guide.emoji}</span>
                <div className="flex-1">
                  <div className="flex items-center gap-2">
                    <span className="text-lg font-bold text-foreground">{guide.title}</span>
                    <span className="rounded-full bg-blue-100 px-2 py-0.5 text-xs font-medium text-blue-800 dark:bg-blue-900 dark:text-blue-200">
                      {list.length} operadores
                    </span>
                  </div>
                  <p className="mt-1 text-sm text-muted-foreground">{guide.intro}</p>
                </div>
              </div>
              <div className="mt-3 rounded-lg bg-slate-100 p-3 dark:bg-slate-800">
                <div className="flex items-center gap-2 text-sm">
                  <span>🎯</span>
                  <span className="font-medium text-foreground">Analogia:</span>
                  <span className="text-muted-foreground">{guide.analogia}</span>
                </div>
              </div>
            </div>

            {/* Operator cards */}
            <div className="grid gap-4 md:grid-cols-1 lg:grid-cols-2">
              {list.map((operator) => {
                const isExpanded = expandedOperator === operator.name;
                const hf = operator.headFirst;
                const kit = operator.didactic;
                const explain = operator.explainName;

                return (
                  <div
                    key={operator.name}
                    className={`rounded-xl border-2 bg-card p-4 transition-all hover:border-blue-300 hover:shadow-lg ${
                      isExpanded ? "border-blue-500 shadow-xl" : ""
                    }`}
                  >
                    {/* Header sempre visível */}
                    <div 
                      className="flex cursor-pointer items-start justify-between gap-2"
                      onClick={() => toggleExpand(operator.name)}
                    >
                      <div className="flex-1">
                        <div className="flex items-center gap-2">
                          <code className="rounded bg-slate-100 px-2 py-1 text-sm font-bold text-blue-600 dark:bg-slate-800 dark:text-blue-400">
                            {operator.name}
                          </code>
                          <span className="rounded-full bg-muted px-2 py-0.5 text-xs text-muted-foreground">
                            {operator.type}
                          </span>
                          <span
                            className={`rounded-full px-2 py-0.5 text-xs font-medium ${
                              hf.docLevel === "manual"
                                ? "bg-emerald-100 text-emerald-800 dark:bg-emerald-900 dark:text-emerald-200"
                                : hf.docLevel === "spec"
                                  ? "bg-blue-100 text-blue-800 dark:bg-blue-900 dark:text-blue-200"
                                  : hf.docConfidence === "low"
                                    ? "bg-red-100 text-red-800 dark:bg-red-900 dark:text-red-200"
                                    : "bg-amber-100 text-amber-800 dark:bg-amber-900 dark:text-amber-200"
                            }`}
                          >
                            {hf.docLevel === "manual" && "✅ Manual"}
                            {hf.docLevel === "spec" && "📘 Spec"}
                            {hf.docLevel === "generated" && (hf.docConfidence === "low" ? "⚠️ Gerado (baixo)" : "🤖 Gerado")}
                          </span>
                        </div>
                        <p className="mt-1 text-sm text-muted-foreground">{operator.purpose}</p>
                      </div>
                      <span className="text-lg">{isExpanded ? "🔽" : "▶️"}</span>
                    </div>

                    {/* ═══════════════════════════════════════════════════════════════════ */}
                    {/* 🎯 GUIA RÁPIDO - SEMPRE VISÍVEL */}
                    {/* ═══════════════════════════════════════════════════════════════════ */}
                    <div className="mt-3 space-y-2">
                      {/* Sintaxe copiável */}
                      <div className="rounded-lg bg-slate-900 p-3">
                        <div className="flex items-center justify-between">
                          <span className="text-xs text-slate-400">📋 Sintaxe (clique para copiar)</span>
                          <button
                            className="rounded bg-slate-700 px-2 py-0.5 text-xs text-slate-300 hover:bg-slate-600"
                            onClick={(e) => {
                              e.stopPropagation();
                              navigator.clipboard.writeText(hf.sintaxe);
                            }}
                          >
                            Copiar
                          </button>
                        </div>
                        {strictDocs && hf.docLevel === "generated" ? (
                          <div className="mt-2 rounded bg-red-900/40 p-2 text-xs text-red-200">
                            Modo rigoroso: este operador não tem documentação fonte.
                            Adicione uma entrada em <span className="font-semibold">client/src/manual/operatorSpecs.ts</span> para liberar exemplos.
                          </div>
                        ) : (
                          <pre className="mt-1 overflow-x-auto text-sm text-green-400">{hf.sintaxe}</pre>
                        )}
                      </div>

                      {!strictDocs && hf.docWarnings && hf.docWarnings.length > 0 && (
                        <div className="rounded-lg border-l-4 border-amber-500 bg-amber-50 p-3 text-xs text-amber-800 dark:bg-amber-950 dark:text-amber-200">
                          <div className="font-semibold">⚠️ Atenção (conteúdo gerado)</div>
                          <div className="mt-1">{hf.docWarnings[0]}</div>
                        </div>
                      )}

                      {/* Quando usar - resumo em 1 linha */}
                      <div className="flex items-start gap-2 rounded-lg bg-green-50 p-2 text-xs dark:bg-green-950">
                        <span className="mt-0.5">✅</span>
                        <div>
                          <span className="font-semibold text-green-800 dark:text-green-200">Quando usar: </span>
                          <span className="text-green-700 dark:text-green-300">{kit.quandoUsar[0]}</span>
                        </div>
                      </div>

                      {/* Dica rápida */}
                      <div className="flex items-start gap-2 rounded-lg bg-amber-50 p-2 text-xs dark:bg-amber-950">
                        <span className="mt-0.5">💎</span>
                        <div>
                          <span className="font-semibold text-amber-800 dark:text-amber-200">Dica: </span>
                          <span className="text-amber-700 dark:text-amber-300">{hf.dicaDeOuro.replace("💎 ", "")}</span>
                        </div>
                      </div>
                    </div>

                    {/* Clique para expandir */}
                    <div 
                      className="mt-3 cursor-pointer text-center text-xs text-muted-foreground hover:text-foreground"
                      onClick={() => toggleExpand(operator.name)}
                    >
                      {isExpanded ? "▲ Ver menos" : "▼ Ver exemplo completo, passo a passo e mais detalhes"}
                    </div>

                    {/* Conteúdo expandido */}
                    {isExpanded && (
                      <div className="mt-4 space-y-4 border-t pt-4" onClick={(e) => e.stopPropagation()}>
                        {!strictDocs && hf.docWarnings && hf.docWarnings.length > 0 && (
                          <div className="rounded-lg border border-amber-300 bg-amber-50 p-4 text-sm text-amber-800 dark:border-amber-800 dark:bg-amber-950 dark:text-amber-200">
                            <div className="font-semibold">⚠️ Transparência</div>
                            <ul className="mt-2 space-y-1 text-xs">
                              {hf.docWarnings.slice(0, 5).map((w) => (
                                <li key={w}>• {w}</li>
                              ))}
                            </ul>
                          </div>
                        )}
                        {/* 🧩 Como ler o nome */}
                        <div className="rounded-lg bg-slate-50 p-4 dark:bg-slate-900/30">
                          <div className="mb-2 flex items-center gap-2 text-sm font-semibold text-slate-800 dark:text-slate-200">
                            <span>🧩</span> Como ler o nome do operador
                          </div>
                          <div className="text-xs text-muted-foreground">
                            <div>
                              <span className="font-medium text-foreground">Tokens:</span> {explain.tokens.join(" · ")}
                            </div>
                            <div className="mt-1">
                              <span className="font-medium text-foreground">Leitura humana:</span> {explain.leituraHumana}
                            </div>
                            {explain.glossario.length > 0 && (
                              <details className="mt-2">
                                <summary className="cursor-pointer select-none font-medium text-foreground">
                                  Mini glossário
                                </summary>
                                <ul className="mt-2 space-y-1">
                                  {explain.glossario.slice(0, 12).map((g) => (
                                    <li key={g}>{g}</li>
                                  ))}
                                </ul>
                              </details>
                            )}
                          </div>
                        </div>

                        {/* 🎭 História */}
                        <div className="rounded-lg bg-purple-50 p-4 dark:bg-purple-950">
                          <div className="mb-2 flex items-center gap-2 text-sm font-semibold text-purple-800 dark:text-purple-200">
                            <span>🎭</span> História do Mundo Real
                          </div>
                          <p className="text-sm text-purple-700 dark:text-purple-300">{hf.historia}</p>
                          <div className="mt-2 text-xs text-purple-600 dark:text-purple-400">
                            — {hf.personagem}
                          </div>
                        </div>

                        {/* 🤔 Problema */}
                        <div className="rounded-lg bg-orange-50 p-4 dark:bg-orange-950">
                          <div className="mb-2 flex items-center gap-2 text-sm font-semibold text-orange-800 dark:text-orange-200">
                            <span>🤔</span> O Problema
                          </div>
                          <p className="text-sm text-orange-700 dark:text-orange-300">{hf.problema}</p>
                        </div>

                        {/* 💡 Analogia */}
                        <div className="rounded-lg bg-yellow-50 p-4 dark:bg-yellow-950">
                          <div className="mb-2 flex items-center gap-2 text-sm font-semibold text-yellow-800 dark:text-yellow-200">
                            <span>💡</span> Analogia do Dia a Dia
                          </div>
                          <p className="text-sm text-yellow-700 dark:text-yellow-300">{hf.analogia}</p>
                        </div>

                        {/* 📋 Passo a Passo */}
                        <div className="rounded-lg bg-green-50 p-4 dark:bg-green-950">
                          <div className="mb-2 flex items-center gap-2 text-sm font-semibold text-green-800 dark:text-green-200">
                            <span>📋</span> Passo a Passo
                          </div>
                          <ul className="space-y-1 text-sm text-green-700 dark:text-green-300">
                            {hf.passoAPasso.map((passo, i) => (
                              <li key={i}>{passo}</li>
                            ))}
                          </ul>
                        </div>

                        {/* ⚠️ Antes vs ✅ Depois */}
                        <div className="grid gap-2 md:grid-cols-2">
                          <div className="rounded-lg bg-red-50 p-3 dark:bg-red-950">
                            <div className="text-xs font-semibold text-red-800 dark:text-red-200">
                              ⚠️ ANTES (sem a regra)
                            </div>
                            <p className="mt-1 text-xs text-red-700 dark:text-red-300">{hf.antes}</p>
                          </div>
                          <div className="rounded-lg bg-green-50 p-3 dark:bg-green-950">
                            <div className="text-xs font-semibold text-green-800 dark:text-green-200">
                              ✅ DEPOIS (com a regra)
                            </div>
                            <p className="mt-1 text-xs text-green-700 dark:text-green-300">{hf.depois}</p>
                          </div>
                        </div>

                        {/* ═══════════════════════════════════════════════════════════════════ */}
                        {/* 🏭 INFORMAÇÕES ENRIQUECIDAS DO BACKEND (OPERATOR_SPECS) */}
                        {/* ═══════════════════════════════════════════════════════════════════ */}
                        {OPERATOR_SPECS[operator.name] && (
                          <div className="space-y-4 rounded-xl border-2 border-blue-300 bg-blue-50/50 p-4 dark:border-blue-700 dark:bg-blue-950/30">
                            <div className="flex items-center gap-2 text-lg font-bold text-blue-800 dark:text-blue-200">
                              <span>📘</span> Documentação Técnica (Backend Real)
                            </div>

                            {/* 🔄 Comportamento do Motor */}
                            {OPERATOR_SPECS[operator.name].engineBehavior && (
                              <div className="rounded-lg bg-white/80 p-4 dark:bg-black/40">
                                <div className="mb-3 flex items-center gap-2 text-sm font-bold text-indigo-800 dark:text-indigo-200">
                                  <span>🔄</span> Como o Motor Executa Este Operador
                                </div>
                                <p className="mb-3 text-sm text-indigo-700 dark:text-indigo-300">
                                  {OPERATOR_SPECS[operator.name].engineBehavior?.description}
                                </p>
                                <div className="space-y-1.5">
                                  {OPERATOR_SPECS[operator.name].engineBehavior?.steps.map((step, i) => (
                                    <div key={i} className="flex items-start gap-2 rounded bg-indigo-50 p-2 text-xs text-indigo-700 dark:bg-indigo-950 dark:text-indigo-300">
                                      <span className="font-mono font-bold">{step}</span>
                                    </div>
                                  ))}
                                </div>
                                {OPERATOR_SPECS[operator.name].engineBehavior?.performance && (
                                  <div className="mt-3 rounded-lg bg-green-50 p-3 dark:bg-green-950">
                                    <div className="text-xs font-semibold text-green-800 dark:text-green-200">
                                      ⚡ Performance
                                    </div>
                                    <p className="mt-1 text-xs text-green-700 dark:text-green-300">
                                      {OPERATOR_SPECS[operator.name].engineBehavior?.performance}
                                    </p>
                                  </div>
                                )}
                                {OPERATOR_SPECS[operator.name].engineBehavior?.cautions && OPERATOR_SPECS[operator.name].engineBehavior.cautions.length > 0 && (
                                  <div className="mt-3 rounded-lg bg-amber-50 p-3 dark:bg-amber-950">
                                    <div className="text-xs font-semibold text-amber-800 dark:text-amber-200">
                                      ⚠️ Cuidados Importantes
                                    </div>
                                    <ul className="mt-2 space-y-1 text-xs text-amber-700 dark:text-amber-300">
                                      {OPERATOR_SPECS[operator.name].engineBehavior?.cautions.map((caution, i) => (
                                        <li key={i} className="flex items-start gap-1">
                                          <span className="mt-0.5">•</span>
                                          <span>{caution}</span>
                                        </li>
                                      ))}
                                    </ul>
                                  </div>
                                )}
                              </div>
                            )}

                            {/* 🎬 Cenários Reais */}
                            {OPERATOR_SPECS[operator.name].realScenarios && OPERATOR_SPECS[operator.name].realScenarios.length > 0 && (
                              <div className="rounded-lg bg-white/80 p-4 dark:bg-black/40">
                                <div className="mb-3 flex items-center gap-2 text-sm font-bold text-purple-800 dark:text-purple-200">
                                  <span>🎬</span> Cenários Reais do Dia a Dia ({OPERATOR_SPECS[operator.name].realScenarios.length})
                                </div>
                                <div className="space-y-3">
                                  {OPERATOR_SPECS[operator.name].realScenarios?.map((scenario, i) => (
                                    <div key={i} className="rounded-lg border-l-4 border-purple-400 bg-purple-50 p-3 dark:border-purple-600 dark:bg-purple-950">
                                      <div className="text-sm font-bold text-purple-900 dark:text-purple-100">
                                        {i + 1}. {scenario.title}
                                      </div>
                                      <div className="mt-2 space-y-1.5 text-xs">
                                        <div>
                                          <span className="font-semibold text-purple-800 dark:text-purple-200">📍 Contexto:</span>
                                          <span className="ml-1 text-purple-700 dark:text-purple-300">{scenario.context}</span>
                                        </div>
                                        <div>
                                          <span className="font-semibold text-purple-800 dark:text-purple-200">🔴 Problema:</span>
                                          <span className="ml-1 text-purple-700 dark:text-purple-300">{scenario.problem}</span>
                                        </div>
                                        <div>
                                          <span className="font-semibold text-purple-800 dark:text-purple-200">✅ Solução:</span>
                                          <span className="ml-1 text-purple-700 dark:text-purple-300">{scenario.solution}</span>
                                        </div>
                                        <div className="mt-2 rounded bg-emerald-100 p-2 dark:bg-emerald-900">
                                          <span className="font-semibold text-emerald-800 dark:text-emerald-200">💰 Impacto Real:</span>
                                          <span className="ml-1 text-emerald-700 dark:text-emerald-300">{scenario.impact}</span>
                                        </div>
                                      </div>
                                    </div>
                                  ))}
                                </div>
                              </div>
                            )}

                            {/* 📊 Resultados Possíveis */}
                            {OPERATOR_SPECS[operator.name].possibleOutcomes && (
                              <div className="rounded-lg bg-white/80 p-4 dark:bg-black/40">
                                <div className="mb-3 flex items-center gap-2 text-sm font-bold text-cyan-800 dark:text-cyan-200">
                                  <span>📊</span> O Que Acontece Quando...
                                </div>
                                <div className="space-y-2">
                                  <div className="rounded-lg bg-green-50 p-3 dark:bg-green-950">
                                    <div className="text-xs font-semibold text-green-800 dark:text-green-200">
                                      ✅ Quando a regra DISPARA (retorna true)
                                    </div>
                                    <p className="mt-1 text-xs text-green-700 dark:text-green-300">
                                      {OPERATOR_SPECS[operator.name].possibleOutcomes?.whenTrue}
                                    </p>
                                  </div>
                                  <div className="rounded-lg bg-slate-50 p-3 dark:bg-slate-950">
                                    <div className="text-xs font-semibold text-slate-800 dark:text-slate-200">
                                      ⏸️ Quando a regra NÃO dispara (retorna false)
                                    </div>
                                    <p className="mt-1 text-xs text-slate-700 dark:text-slate-300">
                                      {OPERATOR_SPECS[operator.name].possibleOutcomes?.whenFalse}
                                    </p>
                                  </div>
                                  {OPERATOR_SPECS[operator.name].possibleOutcomes?.recommendedAction && (
                                    <div className="rounded-lg bg-blue-50 p-3 dark:bg-blue-950">
                                      <div className="text-xs font-semibold text-blue-800 dark:text-blue-200">
                                        💡 Ação Recomendada
                                      </div>
                                      <p className="mt-1 text-xs text-blue-700 dark:text-blue-300">
                                        {OPERATOR_SPECS[operator.name].possibleOutcomes?.recommendedAction}
                                      </p>
                                    </div>
                                  )}
                                </div>
                              </div>
                            )}

                            {/* 🧪 Como Testar */}
                            {OPERATOR_SPECS[operator.name].howToTest && OPERATOR_SPECS[operator.name].howToTest.length > 0 && (
                              <div className="rounded-lg bg-white/80 p-4 dark:bg-black/40">
                                <div className="mb-3 flex items-center gap-2 text-sm font-bold text-teal-800 dark:text-teal-200">
                                  <span>🧪</span> Como Testar Esta Regra (Passo a Passo)
                                </div>
                                <div className="space-y-2">
                                  {OPERATOR_SPECS[operator.name].howToTest?.map((step, i) => (
                                    <div key={i} className="flex items-start gap-2 rounded-lg bg-teal-50 p-2 text-xs text-teal-700 dark:bg-teal-950 dark:text-teal-300">
                                      <span className="font-mono font-semibold text-teal-800 dark:text-teal-200">{step}</span>
                                    </div>
                                  ))}
                                </div>
                                <div className="mt-3 rounded-lg border border-dashed border-teal-400 bg-teal-50/50 p-3 dark:border-teal-600 dark:bg-teal-950/30">
                                  <div className="text-xs font-semibold text-teal-800 dark:text-teal-200">
                                    💡 Dica de Teste
                                  </div>
                                  <p className="mt-1 text-xs text-teal-700 dark:text-teal-300">
                                    Sempre teste com 3 casos: (1) caso normal que deve disparar, (2) caso normal que NÃO deve disparar, (3) edge case (no limite exato do threshold).
                                  </p>
                                </div>
                              </div>
                            )}
                          </div>
                        )}

                        {/* 💻 Sintaxe DSL */}
                        <div className="rounded-lg bg-slate-100 p-4 dark:bg-slate-800">
                          <div className="mb-2 flex items-center gap-2 text-sm font-semibold text-slate-800 dark:text-slate-200">
                            <span>💻</span> Sintaxe DSL
                          </div>
                          {strictDocs && hf.docLevel === "generated" ? (
                            <div className="rounded-lg border-l-4 border-red-500 bg-red-50 p-3 text-sm text-red-800 dark:bg-red-950 dark:text-red-200">
                              <div className="font-semibold">Modo rigoroso</div>
                              <div className="mt-1 text-xs">
                                Este operador está sem documentação fonte. Para liberar exemplos, crie uma entrada em <span className="font-semibold">client/src/manual/operatorSpecs.ts</span>.
                              </div>
                              <button
                                className="mt-2 rounded bg-red-600 px-3 py-1 text-xs font-semibold text-white hover:bg-red-700"
                                onClick={() => navigator.clipboard.writeText(operator.name)}
                              >
                                Copiar nome do operador
                              </button>
                            </div>
                          ) : (
                            <>
                              <pre className="overflow-x-auto rounded-lg bg-slate-900 p-3 text-sm text-green-400">
                                {hf.sintaxe}
                              </pre>
                              <p className="mt-2 text-xs text-slate-600 dark:text-slate-400">
                                {hf.explicacaoSintaxe}
                              </p>
                            </>
                          )}
                        </div>

                        {/* ❓ Não existem perguntas idiotas */}
                        <div className="rounded-lg bg-blue-50 p-4 dark:bg-blue-950">
                          <div className="mb-2 flex items-center gap-2 text-sm font-semibold text-blue-800 dark:text-blue-200">
                            <span>❓</span> Não existem perguntas idiotas
                          </div>
                          <p className="text-sm font-medium text-blue-700 dark:text-blue-300">
                            P: {hf.perguntaComum}
                          </p>
                          <p className="mt-1 text-sm text-blue-600 dark:text-blue-400">
                            R: {hf.respostaPergunta}
                          </p>
                        </div>

                        {/* 💎 Dica de Ouro */}
                        <div className="rounded-lg border-2 border-amber-400 bg-amber-50 p-4 dark:bg-amber-950">
                          <div className="flex items-center gap-2">
                            <span className="text-2xl">💎</span>
                            <div>
                              <div className="text-sm font-bold text-amber-800 dark:text-amber-200">
                                Dica de Ouro
                              </div>
                              <p className="text-sm text-amber-700 dark:text-amber-300">{hf.dicaDeOuro}</p>
                            </div>
                          </div>
                        </div>

                        {/* ═══════════════════════════════════════════════════════════════ */}
                        {/* 📖 SEÇÃO "AULA COMPLETA" - CONTEÚDO AVANÇADO */}
                        {/* ═══════════════════════════════════════════════════════════════ */}
                        
                        {/* 📝 Definição Simples (se existir) */}
                        {hf.definicaoSimples && (
                          <div className="rounded-lg border-2 border-blue-300 bg-blue-50 p-4 dark:border-blue-700 dark:bg-blue-950">
                            <div className="mb-2 flex items-center gap-2 text-sm font-bold text-blue-800 dark:text-blue-200">
                              <span>📝</span> Definição em 1 Frase (Para Leigos)
                            </div>
                            <p className="text-lg font-medium text-blue-900 dark:text-blue-100">{hf.definicaoSimples}</p>
                          </div>
                        )}

                        {/* 🔧 Como Funciona Por Dentro (se existir) */}
                        {hf.comoFunciona && (
                          <div className="rounded-lg bg-slate-100 p-4 dark:bg-slate-800">
                            <div className="mb-2 flex items-center gap-2 text-sm font-bold text-slate-800 dark:text-slate-200">
                              <span>🔧</span> Como Funciona Por Dentro
                            </div>
                            <p className="text-sm text-slate-700 dark:text-slate-300">{hf.comoFunciona}</p>
                          </div>
                        )}

                        {/* 📊 Tabela Verdade (se existir) */}
                        {hf.tabelaVerdade && hf.tabelaVerdade.length > 0 && (
                          <div className="rounded-lg bg-indigo-50 p-4 dark:bg-indigo-950">
                            <div className="mb-3 flex items-center gap-2 text-sm font-bold text-indigo-800 dark:text-indigo-200">
                              <span>📊</span> Tabela de Comportamento
                            </div>
                            <div className="overflow-x-auto">
                              <table className="w-full text-sm">
                                <thead>
                                  <tr className="border-b border-indigo-200 dark:border-indigo-700">
                                    {hf.tabelaVerdade[0].map((header, i) => (
                                      <th key={i} className="px-3 py-2 text-left font-semibold text-indigo-900 dark:text-indigo-100">
                                        {header}
                                      </th>
                                    ))}
                                  </tr>
                                </thead>
                                <tbody>
                                  {hf.tabelaVerdade.slice(1).map((row, i) => (
                                    <tr key={i} className="border-b border-indigo-100 dark:border-indigo-800">
                                      {row.map((cell, j) => (
                                        <td key={j} className="px-3 py-2 text-indigo-700 dark:text-indigo-300">
                                          {cell}
                                        </td>
                                      ))}
                                    </tr>
                                  ))}
                                </tbody>
                              </table>
                            </div>
                          </div>
                        )}

                        {/* 🎯 Exemplos Extras (se existir) */}
                        {hf.exemplosExtras && hf.exemplosExtras.length > 0 && (
                          <div className="rounded-lg bg-emerald-50 p-4 dark:bg-emerald-950">
                            <div className="mb-3 flex items-center gap-2 text-sm font-bold text-emerald-800 dark:text-emerald-200">
                              <span>🎯</span> Exemplos Práticos ({hf.exemplosExtras.length} cenários)
                            </div>
                            <div className="space-y-3">
                              {hf.exemplosExtras.map((ex, i) => (
                                <div key={i} className="rounded-lg bg-white/60 p-3 dark:bg-black/20">
                                  <div className="font-semibold text-emerald-900 dark:text-emerald-100">
                                    {i + 1}. {ex.titulo}
                                  </div>
                                  <div className="mt-1 text-xs text-emerald-700 dark:text-emerald-300">
                                    📍 Cenário: {ex.cenario}
                                  </div>
                                  <pre className="mt-2 overflow-x-auto rounded bg-slate-900 p-2 text-xs text-green-400">
                                    {ex.codigo}
                                  </pre>
                                  <div className="mt-1 text-xs text-emerald-600 dark:text-emerald-400">
                                    ✅ Resultado: {ex.resultado}
                                  </div>
                                </div>
                              ))}
                            </div>
                          </div>
                        )}

                        {/* ❌ Erros Comuns (se existir) */}
                        {hf.errosComuns && hf.errosComuns.length > 0 && (
                          <div className="rounded-lg bg-red-50 p-4 dark:bg-red-950">
                            <div className="mb-2 flex items-center gap-2 text-sm font-bold text-red-800 dark:text-red-200">
                              <span>❌</span> Erros Comuns de Iniciantes (EVITE!)
                            </div>
                            <ul className="space-y-2 text-sm text-red-700 dark:text-red-300">
                              {hf.errosComuns.map((erro, i) => (
                                <li key={i} className="flex items-start gap-2">
                                  <span className="mt-1">•</span>
                                  <span>{erro}</span>
                                </li>
                              ))}
                            </ul>
                          </div>
                        )}

                        {/* ☑️ Checklist de Uso (se existir) */}
                        {hf.checklistUso && hf.checklistUso.length > 0 && (
                          <div className="rounded-lg bg-cyan-50 p-4 dark:bg-cyan-950">
                            <div className="mb-2 flex items-center gap-2 text-sm font-bold text-cyan-800 dark:text-cyan-200">
                              <span>☑️</span> Checklist: Antes de Usar, Verifique...
                            </div>
                            <ul className="space-y-1 text-sm text-cyan-700 dark:text-cyan-300">
                              {hf.checklistUso.map((item, i) => (
                                <li key={i}>{item}</li>
                              ))}
                            </ul>
                          </div>
                        )}

                        {/* 🔗 Operadores Relacionados (se existir) */}
                        {hf.operadoresRelacionados && hf.operadoresRelacionados.length > 0 && (
                          <div className="rounded-lg bg-violet-50 p-4 dark:bg-violet-950">
                            <div className="mb-2 flex items-center gap-2 text-sm font-bold text-violet-800 dark:text-violet-200">
                              <span>🔗</span> Operadores Relacionados (estude também)
                            </div>
                            <div className="flex flex-wrap gap-2">
                              {hf.operadoresRelacionados.map((op) => (
                                <span key={op} className="rounded-full bg-violet-200 px-3 py-1 text-xs font-medium text-violet-800 dark:bg-violet-800 dark:text-violet-200">
                                  {op}
                                </span>
                              ))}
                            </div>
                          </div>
                        )}

                        {/* 🧪 Mini Exercício (se existir) */}
                        {hf.exercicio && (
                          <div className="rounded-lg border-2 border-teal-400 bg-teal-50 p-4 dark:border-teal-600 dark:bg-teal-950">
                            <div className="mb-2 flex items-center gap-2 text-sm font-bold text-teal-800 dark:text-teal-200">
                              <span>🧪</span> Mini Exercício (Pratique!)
                            </div>
                            <div className="rounded-lg bg-white/60 p-3 dark:bg-black/20">
                              <div className="font-medium text-teal-900 dark:text-teal-100">
                                📝 Desafio: {hf.exercicio.pergunta}
                              </div>
                              <details className="mt-3">
                                <summary className="cursor-pointer text-sm font-medium text-teal-600 hover:text-teal-800 dark:text-teal-400 dark:hover:text-teal-200">
                                  👀 Ver resposta
                                </summary>
                                <pre className="mt-2 overflow-x-auto rounded bg-slate-900 p-2 text-xs text-green-400">
                                  {hf.exercicio.resposta}
                                </pre>
                              </details>
                            </div>
                          </div>
                        )}

                        {/* ══════════════════════════════════════════════════════════════════════════ */}
                        {/* 🏭 SEÇÃO: COMPORTAMENTO NO MOTOR DE REGRAS */}
                        {/* ══════════════════════════════════════════════════════════════════════════ */}
                        {hf.comportamentoMotor && (
                          <div className="rounded-lg border-2 border-purple-400 bg-purple-50 p-4 dark:border-purple-600 dark:bg-purple-950">
                            <div className="mb-3 flex items-center gap-2 text-lg font-bold text-purple-800 dark:text-purple-200">
                              <span>🏭</span> O Que Acontece no Motor de Regras?
                            </div>
                            <p className="mb-4 text-sm text-purple-700 dark:text-purple-300">
                              {hf.comportamentoMotor.descricao}
                            </p>
                            
                            <div className="mb-4 rounded-lg bg-white/60 p-3 dark:bg-black/20">
                              <div className="mb-2 text-sm font-semibold text-purple-800 dark:text-purple-200">
                                📋 Passo a passo da execução:
                              </div>
                              <ol className="space-y-1 text-sm text-purple-700 dark:text-purple-300">
                                {hf.comportamentoMotor.passos.map((passo, i) => (
                                  <li key={i}>{passo}</li>
                                ))}
                              </ol>
                            </div>
                            
                            {hf.comportamentoMotor.performance && (
                              <div className="mb-3 rounded-lg bg-green-100 p-2 text-sm text-green-800 dark:bg-green-900/50 dark:text-green-200">
                                {hf.comportamentoMotor.performance}
                              </div>
                            )}
                            
                            {hf.comportamentoMotor.cuidados && hf.comportamentoMotor.cuidados.length > 0 && (
                              <div className="rounded-lg bg-amber-100 p-2 dark:bg-amber-900/50">
                                <div className="text-sm font-semibold text-amber-800 dark:text-amber-200">⚠️ Cuidados:</div>
                                <ul className="mt-1 space-y-1 text-sm text-amber-700 dark:text-amber-300">
                                  {hf.comportamentoMotor.cuidados.map((c, i) => (
                                    <li key={i}>• {c}</li>
                                  ))}
                                </ul>
                              </div>
                            )}
                          </div>
                        )}

                        {/* ══════════════════════════════════════════════════════════════════════════ */}
                        {/* 🎬 SEÇÃO: SITUAÇÕES REAIS DO DIA A DIA */}
                        {/* ══════════════════════════════════════════════════════════════════════════ */}
                        {hf.situacoesReais && hf.situacoesReais.length > 0 && (
                          <div className="rounded-lg border-2 border-orange-400 bg-orange-50 p-4 dark:border-orange-600 dark:bg-orange-950">
                            <div className="mb-3 flex items-center gap-2 text-lg font-bold text-orange-800 dark:text-orange-200">
                              <span>🎬</span> Situações REAIS: Quando Usar Este Operador
                            </div>
                            <div className="space-y-4">
                              {hf.situacoesReais.map((sit, i) => (
                                <div key={i} className="rounded-lg bg-white/60 p-4 dark:bg-black/20">
                                  <div className="mb-2 text-base font-bold text-orange-900 dark:text-orange-100">
                                    📌 {sit.titulo}
                                  </div>
                                  <div className="mb-2 text-sm">
                                    <span className="font-semibold text-orange-700 dark:text-orange-300">Contexto: </span>
                                    <span className="text-orange-600 dark:text-orange-400">{sit.contexto}</span>
                                  </div>
                                  <div className="mb-2 text-sm">
                                    <span className="font-semibold text-red-700 dark:text-red-300">❌ Problema: </span>
                                    <span className="text-red-600 dark:text-red-400">{sit.problema}</span>
                                  </div>
                                  <div className="mb-2">
                                    <span className="text-sm font-semibold text-green-700 dark:text-green-300">✅ Solução: </span>
                                    <pre className="mt-1 overflow-x-auto rounded bg-slate-900 p-2 text-xs text-green-400">
                                      {sit.solucao}
                                    </pre>
                                  </div>
                                  <div className="rounded bg-blue-100 p-2 text-sm text-blue-800 dark:bg-blue-900/50 dark:text-blue-200">
                                    <span className="font-semibold">💡 Impacto: </span>{sit.impacto}
                                  </div>
                                </div>
                              ))}
                            </div>
                          </div>
                        )}

                        {/* ══════════════════════════════════════════════════════════════════════════ */}
                        {/* 📊 SEÇÃO: RESULTADOS POSSÍVEIS */}
                        {/* ══════════════════════════════════════════════════════════════════════════ */}
                        {hf.resultadosPossiveis && (
                          <div className="rounded-lg border-2 border-pink-400 bg-pink-50 p-4 dark:border-pink-600 dark:bg-pink-950">
                            <div className="mb-3 flex items-center gap-2 text-lg font-bold text-pink-800 dark:text-pink-200">
                              <span>📊</span> Resultados Quando a Regra é Avaliada
                            </div>
                            <div className="grid gap-3 md:grid-cols-2">
                              <div className="rounded-lg bg-red-100 p-3 dark:bg-red-900/50">
                                <div className="mb-1 text-sm font-bold text-red-800 dark:text-red-200">
                                  🚨 Quando DISPARA (Verdadeiro):
                                </div>
                                <p className="text-sm text-red-700 dark:text-red-300">{hf.resultadosPossiveis.quandoDispara}</p>
                              </div>
                              <div className="rounded-lg bg-green-100 p-3 dark:bg-green-900/50">
                                <div className="mb-1 text-sm font-bold text-green-800 dark:text-green-200">
                                  ✅ Quando NÃO DISPARA (Falso):
                                </div>
                                <p className="text-sm text-green-700 dark:text-green-300">{hf.resultadosPossiveis.quandoNaoDispara}</p>
                              </div>
                            </div>
                            {hf.resultadosPossiveis.acaoRecomendada && (
                              <div className="mt-3 rounded-lg bg-blue-100 p-2 text-sm text-blue-800 dark:bg-blue-900/50 dark:text-blue-200">
                                <span className="font-semibold">💡 Ação recomendada: </span>
                                {hf.resultadosPossiveis.acaoRecomendada}
                              </div>
                            )}
                          </div>
                        )}

                        {/* ══════════════════════════════════════════════════════════════════════════ */}
                        {/* 🔧 SEÇÃO: COMO TESTAR ESTA REGRA */}
                        {/* ══════════════════════════════════════════════════════════════════════════ */}
                        {hf.comoTestar && hf.comoTestar.length > 0 && (
                          <div className="rounded-lg border-2 border-sky-400 bg-sky-50 p-4 dark:border-sky-600 dark:bg-sky-950">
                            <div className="mb-3 flex items-center gap-2 text-lg font-bold text-sky-800 dark:text-sky-200">
                              <span>🔧</span> Como TESTAR Esta Regra Antes de Produção
                            </div>
                            <ul className="space-y-2 text-sm text-sky-700 dark:text-sky-300">
                              {hf.comoTestar.map((teste, i) => (
                                <li key={i} className="flex items-start gap-2 rounded-lg bg-white/60 p-2 dark:bg-black/20">
                                  <span className="mt-0.5 flex h-5 w-5 shrink-0 items-center justify-center rounded-full bg-sky-500 text-xs font-bold text-white">
                                    {i + 1}
                                  </span>
                                  <span>{teste}</span>
                                </li>
                              ))}
                            </ul>
                          </div>
                        )}

                        {/* 🎒 Kit ultra-didático (gerado) */}
                        <details className="rounded-lg border bg-white/40 p-4 dark:bg-black/10">
                          <summary className="cursor-pointer select-none text-sm font-semibold text-foreground">
                            🎒 Kit ultra-didático (gerado para este operador)
                          </summary>

                          <div className="mt-3 grid gap-3 lg:grid-cols-2">
                            <div className="rounded-lg bg-slate-50 p-3 text-sm dark:bg-slate-900/30">
                              <div className="font-semibold text-foreground">🧠 Modelo mental</div>
                              <p className="mt-1 text-muted-foreground">{kit.modeloMental}</p>
                            </div>
                            <div className="rounded-lg bg-slate-50 p-3 text-sm dark:bg-slate-900/30">
                              <div className="font-semibold text-foreground">📝 Resumo</div>
                              <p className="mt-1 text-muted-foreground">{kit.resumo}</p>
                            </div>
                          </div>

                          <div className="mt-3 grid gap-3 lg:grid-cols-2">
                            <div className="rounded-lg bg-green-50 p-3 dark:bg-green-950">
                              <div className="text-sm font-semibold text-green-800 dark:text-green-200">✅ Quando usar</div>
                              <ul className="mt-2 space-y-1 text-sm text-green-700 dark:text-green-300">
                                {kit.quandoUsar.map((x) => (
                                  <li key={x}>• {x}</li>
                                ))}
                              </ul>
                            </div>
                            <div className="rounded-lg bg-red-50 p-3 dark:bg-red-950">
                              <div className="text-sm font-semibold text-red-800 dark:text-red-200">⛔ Quando evitar</div>
                              <ul className="mt-2 space-y-1 text-sm text-red-700 dark:text-red-300">
                                {kit.quandoEvitar.map((x) => (
                                  <li key={x}>• {x}</li>
                                ))}
                              </ul>
                            </div>
                          </div>

                          <div className="mt-3 rounded-lg bg-amber-50 p-3 dark:bg-amber-950">
                            <div className="text-sm font-semibold text-amber-800 dark:text-amber-200">⚠️ Armadilhas comuns</div>
                            <ul className="mt-2 space-y-1 text-sm text-amber-700 dark:text-amber-300">
                              {kit.armadilhas.map((x) => (
                                <li key={x}>• {x}</li>
                              ))}
                            </ul>
                          </div>

                          <details className="mt-3 rounded-lg bg-slate-100 p-3 dark:bg-slate-800">
                            <summary className="cursor-pointer select-none text-sm font-semibold text-foreground">
                              🧾 Campos sugeridos (com tipos)
                            </summary>
                            <div className="mt-2 grid gap-2 sm:grid-cols-2">
                              {kit.camposSugeridos.map((f) => (
                                <div key={f.path} className="rounded-md bg-white/60 p-2 text-xs dark:bg-black/20">
                                  <div className="flex items-center justify-between gap-2">
                                    <code className="text-blue-600 dark:text-blue-400">{f.path}</code>
                                    <span className="rounded bg-slate-200 px-2 py-0.5 text-[10px] dark:bg-slate-700">
                                      {f.type}
                                    </span>
                                  </div>
                                  <div className="mt-1 text-muted-foreground">
                                    Ex: <span className="font-medium text-foreground">{f.example}</span>
                                    {f.note ? ` — ${f.note}` : ""}
                                  </div>
                                </div>
                              ))}
                            </div>
                          </details>

                          <div className="mt-3 rounded-lg bg-slate-100 p-3 dark:bg-slate-800">
                            <div className="text-sm font-semibold text-foreground">🧪 Exemplo de payload (para você mentalizar)</div>
                            <pre className="mt-2 max-h-64 overflow-auto rounded-lg bg-slate-900 p-3 text-xs text-slate-100">
                              {kit.exemploPayload}
                            </pre>
                          </div>

                          <div className="mt-3 rounded-lg bg-slate-100 p-3 dark:bg-slate-800">
                            <div className="text-sm font-semibold text-foreground">🧾 Exemplo de regra (DSL)</div>
                            <pre className="mt-2 overflow-x-auto rounded-lg bg-slate-900 p-3 text-sm text-green-400">
                              {kit.exemploDsl}
                            </pre>
                            {kit.relacionados.length > 0 && (
                              <div className="mt-2 text-xs text-muted-foreground">
                                Relacionados: <span className="font-medium text-foreground">{kit.relacionados.join(", ")}</span>
                              </div>
                            )}
                          </div>

                          <details className="mt-3 rounded-lg bg-blue-50 p-3 dark:bg-blue-950">
                            <summary className="cursor-pointer select-none text-sm font-semibold text-blue-800 dark:text-blue-200">
                              🧠 Mini-exercícios (teste sua compreensão)
                            </summary>
                            <ul className="mt-2 space-y-2 text-sm text-blue-700 dark:text-blue-300">
                              {kit.casosDeTeste.map((tc) => (
                                <li key={tc.scenario}>
                                  <div className="font-medium">• {tc.scenario}</div>
                                  <div className="text-xs opacity-90">Esperado: {tc.expected}</div>
                                </li>
                              ))}
                            </ul>
                          </details>
                        </details>
                      </div>
                    )}
                  </div>
                );
              })}
            </div>
          </section>
        );
      })}

      {/* ═══════════════════════════════════════════════════════════════════════ */}
      {/* FOOTER - PRÓXIMOS PASSOS */}
      {/* ═══════════════════════════════════════════════════════════════════════ */}
      <div className="rounded-xl border-2 border-green-200 bg-gradient-to-r from-green-50 to-emerald-50 p-6 dark:border-green-800 dark:from-green-950 dark:to-emerald-950">
        <div className="flex items-center gap-3">
          <span className="text-3xl">🎉</span>
          <div>
            <div className="text-lg font-bold text-foreground">Parabéns! Você agora conhece os operadores!</div>
            <p className="text-sm text-muted-foreground">
              Agora é hora de criar suas próprias regras. Lembre-se: comece simples e vá evoluindo!
            </p>
          </div>
        </div>

        <div className="mt-4 grid gap-4 md:grid-cols-3">
          <div className="rounded-lg border bg-white/50 p-4 text-center dark:bg-black/20">
            <div className="text-2xl">🧪</div>
            <div className="mt-2 font-semibold">Teste em Homologação</div>
            <p className="mt-1 text-xs text-muted-foreground">Sempre teste antes de ir para produção</p>
          </div>
          <div className="rounded-lg border bg-white/50 p-4 text-center dark:bg-black/20">
            <div className="text-2xl">📊</div>
            <div className="mt-2 font-semibold">Monitore os Resultados</div>
            <p className="mt-1 text-xs text-muted-foreground">Acompanhe falsos positivos e negativos</p>
          </div>
          <div className="rounded-lg border bg-white/50 p-4 text-center dark:bg-black/20">
            <div className="text-2xl">🔄</div>
            <div className="mt-2 font-semibold">Itere e Melhore</div>
            <p className="mt-1 text-xs text-muted-foreground">Regras boas evoluem com o tempo</p>
          </div>
        </div>
      </div>
    </div>
  );
}
