import { useState } from "react";
import { BACKEND_OPERATORS } from "@/manual/generated/backendOperators.generated";

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

const guessDslForKind = (name: string, kind: OperatorKind): string => {
  const upper = name.toUpperCase();
  if (HEAD_FIRST_EXAMPLES[upper]) return HEAD_FIRST_EXAMPLES[upper].sintaxe;

  if (kind === "logical") return "(A) AND (B)";
  if (kind === "range") return "transaction.amount BETWEEN 100 AND 5000";
  if (kind === "list") return "transaction.channel IN [\"APP\", \"WEB\", \"POS\"]";
  if (kind === "string") return "customer.email CONTAINS \"tempmail\"";
  if (kind === "null") return "transaction.device_id IS_NULL";
  if (kind === "boolean") return "customer.is_vip IS_TRUE";
  if (kind === "array") return "order.items ARRAY_SIZE_GT 10";
  if (kind === "datetime") return "transaction.time TIME_BETWEEN \"22:00\" AND \"06:00\"";
  if (kind === "aggregation") return "COUNT(transactions, last_1h, customer_id) GT 10";
  if (kind === "graph") return "NEO4J_LINK_DEPTH(customer_id, device_id) GT 2";
  if (kind === "device") return "device.trust_score GT 0.7";
  if (kind === "identity") return "customer.email CONTAINS \"tempmail\"";
  if (kind === "merchant") return "merchant.mcc IN [\"5999\", \"7995\"]";
  if (kind === "platform") return "platform.compliance_status EQ \"COMPLIANT\"";
  if (kind === "validation") return "validation.sanction_hit IS_FALSE";
  if (kind === "statistical") return "statistics.deviation GT 3.0";
  if (kind === "risk_pattern") return `${upper}(payload) GT threshold`;
  if (kind === "compare") {
    if (upper.endsWith("_GT") || upper === "GT") return "transaction.amount GT 5000";
    if (upper.endsWith("_GTE") || upper === "GTE") return "transaction.amount GTE 5000";
    if (upper.endsWith("_LT") || upper === "LT") return "transaction.amount LT 10";
    if (upper.endsWith("_LTE") || upper === "LTE") return "transaction.amount LTE 10";
    if (upper.endsWith("_NEQ") || upper === "NEQ") return "transaction.country NEQ \"BR\"";
    return "transaction.status EQ \"PENDING\"";
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
}

// Mapeamento completo de exemplos Head First
const HEAD_FIRST_EXAMPLES: Record<string, HeadFirstExample> = {
  // ══════════════════════════════════════════════════════════════════════════
  // OPERADORES LÓGICOS - A COLA QUE UNE TUDO
  // ══════════════════════════════════════════════════════════════════════════
  AND: {
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
  
  // Detectar contexto pelo nome do operador
  if (upper.includes("VELOCITY") || upper.includes("COUNT")) {
    return `Maria, analista de fraude, precisa detectar comportamento de alta frequência. O operador ${name} permite monitorar a velocidade de eventos e identificar padrões anômalos.`;
  }
  if (upper.includes("AMOUNT") || upper.includes("SUM") || upper.includes("VALUE")) {
    return `João, do time de risco, precisa avaliar valores de transação. O operador ${name} ajuda a identificar movimentações suspeitas por valor.`;
  }
  if (upper.includes("DEVICE") || upper.includes("FINGERPRINT") || upper.includes("BROWSER")) {
    return `Carlos, especialista em segurança, precisa avaliar a confiabilidade do dispositivo. O operador ${name} analisa características técnicas do device.`;
  }
  if (upper.includes("EMAIL") || upper.includes("PHONE") || upper.includes("CPF") || upper.includes("ADDRESS")) {
    return `Ana, do onboarding, precisa validar dados cadastrais. O operador ${name} verifica a consistência das informações do cliente.`;
  }
  if (upper.includes("MERCHANT") || upper.includes("MCC") || upper.includes("STORE")) {
    return `Pedro, analista de pagamentos, precisa avaliar o estabelecimento. O operador ${name} verifica características do merchant.`;
  }
  if (upper.includes("DATE") || upper.includes("TIME") || upper.includes("DAY") || upper.includes("HOUR")) {
    return `Fernanda, do monitoramento, precisa criar regras temporais. O operador ${name} permite avaliar datas e horários suspeitos.`;
  }
  if (upper.includes("GRAPH") || upper.includes("NEO4J") || upper.includes("NETWORK") || upper.includes("LINK")) {
    return `Ricardo, investigador de fraude, precisa mapear conexões. O operador ${name} revela relações ocultas entre entidades.`;
  }
  if (upper.includes("SANCTION") || upper.includes("PEP") || upper.includes("ADVERSE") || upper.includes("FATF")) {
    return `Juliana, do compliance, precisa verificar listas regulatórias. O operador ${name} automatiza verificações obrigatórias.`;
  }
  if (upper.includes("ANOMALY") || upper.includes("DEVIATION") || upper.includes("SCORE")) {
    return `Marcos, cientista de dados, precisa detectar outliers. O operador ${name} usa estatística para identificar anomalias.`;
  }
  if (upper.includes("DORA") || upper.includes("GDPR") || upper.includes("PLT_") || upper.includes("EIDAS")) {
    return `Beatriz, do jurídico, precisa garantir compliance regulatório. O operador ${name} verifica conformidade com normas específicas.`;
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
  
  if (upper.includes("VELOCITY")) return "Como detectar padrões de alta frequência que indicam automação ou fraude?";
  if (upper.includes("COUNT")) return "Como contar eventos em um período para identificar comportamento anômalo?";
  if (upper.includes("SUM")) return "Como somar valores para detectar fragmentação (smurfing)?";
  if (upper.includes("DEVICE")) return "Como avaliar se o dispositivo é confiável ou suspeito?";
  if (upper.includes("FINGERPRINT")) return "Como identificar dispositivos únicos mesmo com dados alterados?";
  if (upper.includes("EMAIL")) return "Como validar se o e-mail é legítimo ou temporário/descartável?";
  if (upper.includes("PHONE")) return "Como verificar se o telefone é real ou VoIP descartável?";
  if (upper.includes("MERCHANT") || upper.includes("MCC")) return "Como avaliar o risco do estabelecimento comercial?";
  if (upper.includes("GRAPH") || upper.includes("NEO4J")) return "Como descobrir conexões ocultas entre entidades suspeitas?";
  if (upper.includes("SANCTION") || upper.includes("PEP")) return "Como automatizar verificações de compliance obrigatórias?";
  if (upper.includes("ANOMALY") || upper.includes("DEVIATION")) return "Como detectar comportamentos que fogem do padrão estatístico?";
  
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
  const found = HEAD_FIRST_EXAMPLES[name] || HEAD_FIRST_EXAMPLES[name.toUpperCase()];
  if (found) return found;

  // Gerar exemplo contextualizado baseado na classificação
  const kind = classifyOperator(name);
  const info = ANALOGIAS_POR_TIPO[kind];
  const explain = explainOperatorName(name);
  
  return {
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
    sintaxe: guessDslForKind(name, kind),
    explicacaoSintaxe: `📖 O operador ${name} (${explain.leituraHumana}) aplica a lógica de ${kind} ao seu campo.`,
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
        <div className="flex items-center gap-3">
          <span className="text-4xl">🧠</span>
          <div>
            <h1 className="text-2xl font-bold text-foreground">Guia de Operadores - Estilo "Use a Cabeça"</h1>
            <p className="text-sm text-muted-foreground">
              Aprenda cada operador com histórias, analogias e exemplos do mundo real
            </p>
          </div>
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
                        <pre className="mt-1 overflow-x-auto text-sm text-green-400">{hf.sintaxe}</pre>
                      </div>

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

                        {/* 💻 Sintaxe DSL */}
                        <div className="rounded-lg bg-slate-100 p-4 dark:bg-slate-800">
                          <div className="mb-2 flex items-center gap-2 text-sm font-semibold text-slate-800 dark:text-slate-200">
                            <span>💻</span> Sintaxe DSL
                          </div>
                          <pre className="overflow-x-auto rounded-lg bg-slate-900 p-3 text-sm text-green-400">
                            {hf.sintaxe}
                          </pre>
                          <p className="mt-2 text-xs text-slate-600 dark:text-slate-400">
                            {hf.explicacaoSintaxe}
                          </p>
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
