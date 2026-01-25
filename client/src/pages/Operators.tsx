import { BACKEND_OPERATORS } from "@/manual/generated/backendOperators.generated";

type Operator = (typeof BACKEND_OPERATORS)[number];

// ─────────────────────────────────────────────────────────────────────────────
// HELPERS
// ─────────────────────────────────────────────────────────────────────────────

const normalizeCategory = (category?: string) => {
  const normalized = category?.trim();
  if (!normalized || normalized === "=" || normalized.toLowerCase() === "natural") {
    return "Geral";
  }
  return normalized;
};

// ─────────────────────────────────────────────────────────────────────────────
// GUIA POR CATEGORIA — quando usar cada grupo de operadores
// ─────────────────────────────────────────────────────────────────────────────

const CATEGORY_GUIDE: Record<string, { title: string; purpose: string; tip: string }> = {
  "Comparação básica": {
    title: "Comparações Simples",
    purpose: "Compare valores numéricos ou textuais diretamente.",
    tip: "Use para limites de valor, status ou códigos fixos.",
  },
  Listas: {
    title: "Listas (IN / NOT IN)",
    purpose: "Verifique se um valor pertence a um conjunto.",
    tip: "Ideal para canais permitidos, países bloqueados ou MCCs suspeitos.",
  },
  Strings: {
    title: "Texto e Padrões",
    purpose: "Valide trechos, prefixos, sufixos ou regex em campos de texto.",
    tip: "Use para e-mails temporários, domínios suspeitos ou descrições.",
  },
  Nulos: {
    title: "Campos Vazios / Preenchidos",
    purpose: "Detecte campos não informados ou obrigatórios.",
    tip: "Útil para dados cadastrais incompletos.",
  },
  Booleanos: {
    title: "Verdadeiro / Falso",
    purpose: "Valide flags booleanas do payload.",
    tip: "Ex.: cliente_vip = true, primeiro_acesso = false.",
  },
  Range: {
    title: "Faixas (Between)",
    purpose: "Confirme se um valor está dentro de um intervalo.",
    tip: "Ex.: valor entre 100 e 500.",
  },
  "Comparação entre campos": {
    title: "Campo vs Campo",
    purpose: "Compare dois campos do mesmo registro.",
    tip: "Ex.: valor_informado diferente de valor_cobrado.",
  },
  "Funções de data/tempo": {
    title: "Datas e Horários",
    purpose: "Aplique regras de calendário, horário ou idade.",
    tip: "Ex.: transação fora do horário comercial, conta criada há menos de 7 dias.",
  },
  "Funções de lista/array": {
    title: "Arrays",
    purpose: "Meça tamanho ou conteúdo de listas.",
    tip: "Ex.: itens do carrinho > 10, tags contém \"promocao\".",
  },
  "Funções matemáticas": {
    title: "Matemática",
    purpose: "Calcule diferenças, percentuais ou valores absolutos.",
    tip: "Ex.: abs(saldo) > 1000, percentual de desconto > 50%.",
  },
  Geolocalização: {
    title: "Localização e Distância",
    purpose: "Verifique país, cidade ou distância geográfica.",
    tip: "Ex.: compra em país diferente do cadastro, distância > 500km.",
  },
  "Operadores lógicos": {
    title: "Lógica (AND / OR / NOT)",
    purpose: "Combine várias condições em uma única regra.",
    tip: "Ex.: (valor > 1000) AND (pais != \"BR\").",
  },
  Geral: {
    title: "Operadores Gerais",
    purpose: "Operadores variados para cenários comuns.",
    tip: "Consulte o nome e a descrição para entender o uso.",
  },
};

// ─────────────────────────────────────────────────────────────────────────────
// EXEMPLOS DIDÁTICOS COMPLETOS
// Estrutura: { cenario, comoUsar, sintaxe }
// ─────────────────────────────────────────────────────────────────────────────

interface DidaticExample {
  cenario: string;   // Quando usar
  comoUsar: string;  // Explicação de preenchimento
  sintaxe: string;   // Código DSL real
}

const DIDATIC_EXAMPLES: Record<string, DidaticExample> = {
  // ── Lógicos ──
  AND: {
    cenario: "Duas ou mais condições precisam ser verdadeiras ao mesmo tempo.",
    comoUsar: "Selecione AND e adicione as condições filhas. Todas devem ser verdadeiras.",
    sintaxe: "(transaction.amount > 1000) AND (transaction.country != \"BR\")",
  },
  OR: {
    cenario: "Pelo menos uma das condições deve ser verdadeira.",
    comoUsar: "Selecione OR e adicione as condições. Basta uma ser verdadeira.",
    sintaxe: "(channel = \"APP\") OR (channel = \"WEB\")",
  },
  NOT: {
    cenario: "Inverter o resultado de uma condição.",
    comoUsar: "Selecione NOT e adicione a condição que deseja negar.",
    sintaxe: "NOT (customer.is_vip = true)",
  },
  XOR: {
    cenario: "Exatamente uma das condições deve ser verdadeira (exclusivo).",
    comoUsar: "Selecione XOR e adicione duas condições mutuamente exclusivas.",
    sintaxe: "(payment_method = \"CREDIT\") XOR (payment_method = \"DEBIT\")",
  },
  NAND: {
    cenario: "Negação do AND — pelo menos uma condição é falsa.",
    comoUsar: "Selecione NAND para garantir que nem todas as condições sejam verdadeiras.",
    sintaxe: "NAND((a > 10), (b > 10))",
  },
  NOR: {
    cenario: "Negação do OR — todas as condições são falsas.",
    comoUsar: "Selecione NOR para garantir que nenhuma condição seja verdadeira.",
    sintaxe: "NOR((status = \"BLOCKED\"), (status = \"FRAUD\"))",
  },

  // ── Comparação básica ──
  EQ: {
    cenario: "Verificar se um campo é igual a um valor específico.",
    comoUsar: "Campo esquerdo: nome do campo. Campo direito: valor esperado.",
    sintaxe: "transaction.status EQ \"APPROVED\"",
  },
  NEQ: {
    cenario: "Verificar se um campo é diferente de um valor.",
    comoUsar: "Campo esquerdo: nome do campo. Campo direito: valor a excluir.",
    sintaxe: "transaction.country NEQ \"BR\"",
  },
  GT: {
    cenario: "Verificar se um valor numérico é maior que um limite.",
    comoUsar: "Campo: valor numérico. Limite: número de referência.",
    sintaxe: "transaction.amount GT 5000",
  },
  GTE: {
    cenario: "Verificar se um valor é maior ou igual a um limite.",
    comoUsar: "Campo: valor numérico. Limite: número mínimo aceitável.",
    sintaxe: "customer.age GTE 18",
  },
  LT: {
    cenario: "Verificar se um valor é menor que um limite.",
    comoUsar: "Campo: valor numérico. Limite: número máximo.",
    sintaxe: "transaction.amount LT 100",
  },
  LTE: {
    cenario: "Verificar se um valor é menor ou igual a um limite.",
    comoUsar: "Campo: valor numérico. Limite: número máximo aceitável.",
    sintaxe: "customer.score LTE 300",
  },

  // ── Range ──
  BETWEEN: {
    cenario: "Verificar se um valor está dentro de uma faixa (inclusive).",
    comoUsar: "Campo: valor. Min: limite inferior. Max: limite superior.",
    sintaxe: "transaction.amount BETWEEN 100 AND 5000",
  },
  NOT_BETWEEN: {
    cenario: "Verificar se um valor está fora de uma faixa.",
    comoUsar: "Campo: valor. Min e Max: faixa a excluir.",
    sintaxe: "transaction.amount NOT_BETWEEN 100 AND 5000",
  },

  // ── Listas ──
  IN: {
    cenario: "Verificar se um valor está em uma lista de valores permitidos.",
    comoUsar: "Campo: valor a verificar. Lista: valores separados por vírgula.",
    sintaxe: "transaction.channel IN [\"WEB\", \"APP\", \"POS\"]",
  },
  IN_LIST: {
    cenario: "Verificar se um valor pertence a uma lista pré-definida.",
    comoUsar: "Campo: valor. Lista: referência a lista cadastrada ou inline.",
    sintaxe: "merchant.mcc IN_LIST [\"5411\", \"5812\", \"5814\"]",
  },
  NOT_IN: {
    cenario: "Verificar se um valor NÃO está em uma lista.",
    comoUsar: "Campo: valor. Lista: valores a excluir.",
    sintaxe: "transaction.country NOT_IN [\"BR\", \"AR\", \"CL\"]",
  },
  NOT_IN_LIST: {
    cenario: "Verificar se um valor não pertence a uma lista cadastrada.",
    comoUsar: "Campo: valor. Lista: referência a lista de exclusão.",
    sintaxe: "customer.email_domain NOT_IN_LIST @dominios_suspeitos",
  },

  // ── Strings ──
  CONTAINS: {
    cenario: "Verificar se um texto contém uma substring.",
    comoUsar: "Campo: texto. Substring: trecho a buscar.",
    sintaxe: "transaction.description CONTAINS \"frete\"",
  },
  NOT_CONTAINS: {
    cenario: "Verificar se um texto NÃO contém uma substring.",
    comoUsar: "Campo: texto. Substring: trecho que não deve existir.",
    sintaxe: "customer.email NOT_CONTAINS \"temp\"",
  },
  STARTS_WITH: {
    cenario: "Verificar se um texto começa com um prefixo.",
    comoUsar: "Campo: texto. Prefixo: início esperado.",
    sintaxe: "card.bin STARTS_WITH \"4\"",
  },
  ENDS_WITH: {
    cenario: "Verificar se um texto termina com um sufixo.",
    comoUsar: "Campo: texto. Sufixo: final esperado.",
    sintaxe: "customer.email ENDS_WITH \"@empresa.com\"",
  },
  REGEX: {
    cenario: "Verificar se um texto casa com uma expressão regular.",
    comoUsar: "Campo: texto. Regex: padrão entre barras.",
    sintaxe: "customer.email REGEX /^[a-z]+@tempmail\\.(com|net)$/",
  },
  NOT_REGEX: {
    cenario: "Verificar se um texto NÃO casa com uma expressão regular.",
    comoUsar: "Campo: texto. Regex: padrão a rejeitar.",
    sintaxe: "customer.phone NOT_REGEX /^\\+55/",
  },

  // ── Nulos ──
  IS_NULL: {
    cenario: "Verificar se um campo está vazio ou não foi informado.",
    comoUsar: "Campo: nome do campo a verificar.",
    sintaxe: "customer.phone IS_NULL",
  },
  NOT_NULL: {
    cenario: "Verificar se um campo está preenchido.",
    comoUsar: "Campo: nome do campo obrigatório.",
    sintaxe: "customer.email NOT_NULL",
  },
  IS_EMPTY: {
    cenario: "Verificar se um campo de texto está vazio (string vazia).",
    comoUsar: "Campo: nome do campo de texto.",
    sintaxe: "customer.address IS_EMPTY",
  },
  NOT_EMPTY: {
    cenario: "Verificar se um campo de texto não está vazio.",
    comoUsar: "Campo: nome do campo de texto.",
    sintaxe: "customer.name NOT_EMPTY",
  },

  // ── Booleanos ──
  IS_TRUE: {
    cenario: "Verificar se uma flag booleana é verdadeira.",
    comoUsar: "Campo: nome do campo booleano.",
    sintaxe: "customer.is_vip IS_TRUE",
  },
  IS_FALSE: {
    cenario: "Verificar se uma flag booleana é falsa.",
    comoUsar: "Campo: nome do campo booleano.",
    sintaxe: "customer.email_verified IS_FALSE",
  },

  // ── Arrays ──
  ARRAY_CONTAINS: {
    cenario: "Verificar se um array contém um elemento específico.",
    comoUsar: "Campo: array. Elemento: valor a buscar.",
    sintaxe: "order.tags ARRAY_CONTAINS \"promocao\"",
  },
  ARRAY_NOT_CONTAINS: {
    cenario: "Verificar se um array NÃO contém um elemento.",
    comoUsar: "Campo: array. Elemento: valor proibido.",
    sintaxe: "customer.flags ARRAY_NOT_CONTAINS \"blocked\"",
  },
  ARRAY_SIZE_EQ: {
    cenario: "Verificar se o tamanho de um array é igual a um número.",
    comoUsar: "Campo: array. Tamanho: número esperado.",
    sintaxe: "order.items ARRAY_SIZE_EQ 1",
  },
  ARRAY_SIZE_GT: {
    cenario: "Verificar se o tamanho de um array é maior que um número.",
    comoUsar: "Campo: array. Tamanho mínimo: número.",
    sintaxe: "order.items ARRAY_SIZE_GT 10",
  },
  ARRAY_SIZE_LT: {
    cenario: "Verificar se o tamanho de um array é menor que um número.",
    comoUsar: "Campo: array. Tamanho máximo: número.",
    sintaxe: "order.items ARRAY_SIZE_LT 3",
  },
  ARRAY_SIZE_GTE: {
    cenario: "Verificar se o tamanho de um array é maior ou igual.",
    comoUsar: "Campo: array. Tamanho mínimo: número.",
    sintaxe: "customer.devices ARRAY_SIZE_GTE 2",
  },
  ARRAY_SIZE_LTE: {
    cenario: "Verificar se o tamanho de um array é menor ou igual.",
    comoUsar: "Campo: array. Tamanho máximo: número.",
    sintaxe: "order.coupons ARRAY_SIZE_LTE 5",
  },

  // ── Data/Tempo ──
  DATE_BEFORE: {
    cenario: "Verificar se uma data é anterior a outra.",
    comoUsar: "Campo: data. Referência: data limite.",
    sintaxe: "customer.created_at DATE_BEFORE \"2024-01-01\"",
  },
  DATE_AFTER: {
    cenario: "Verificar se uma data é posterior a outra.",
    comoUsar: "Campo: data. Referência: data limite.",
    sintaxe: "transaction.date DATE_AFTER \"2024-06-01\"",
  },
  DATE_BETWEEN: {
    cenario: "Verificar se uma data está dentro de um período.",
    comoUsar: "Campo: data. Início e Fim: datas limite.",
    sintaxe: "transaction.date DATE_BETWEEN \"2024-01-01\" AND \"2024-12-31\"",
  },
  TIME_BETWEEN: {
    cenario: "Verificar se um horário está dentro de uma faixa.",
    comoUsar: "Campo: horário. Início e Fim: horários (HH:MM).",
    sintaxe: "transaction.time TIME_BETWEEN \"22:00\" AND \"06:00\"",
  },
  DAY_OF_WEEK_IN: {
    cenario: "Verificar se o dia da semana está em uma lista.",
    comoUsar: "Campo: data. Lista: dias (1=Seg, 7=Dom).",
    sintaxe: "transaction.date DAY_OF_WEEK_IN [6, 7]",
  },
  HOUR_BETWEEN: {
    cenario: "Verificar se a hora está dentro de um intervalo.",
    comoUsar: "Campo: datetime. Início e Fim: horas (0-23).",
    sintaxe: "transaction.datetime HOUR_BETWEEN 0 AND 5",
  },
  AGE_DAYS_GT: {
    cenario: "Verificar se a idade em dias de uma data é maior que um valor.",
    comoUsar: "Campo: data. Dias: número mínimo de dias.",
    sintaxe: "customer.created_at AGE_DAYS_GT 7",
  },
  AGE_DAYS_LT: {
    cenario: "Verificar se a idade em dias de uma data é menor que um valor.",
    comoUsar: "Campo: data. Dias: número máximo de dias.",
    sintaxe: "customer.created_at AGE_DAYS_LT 30",
  },

  // ── Geolocalização ──
  GEO_COUNTRY_EQ: {
    cenario: "Verificar se o país da transação é igual ao esperado.",
    comoUsar: "Campo: país (ISO). Valor: código do país.",
    sintaxe: "transaction.country GEO_COUNTRY_EQ \"BR\"",
  },
  GEO_COUNTRY_NEQ: {
    cenario: "Verificar se o país da transação é diferente do esperado.",
    comoUsar: "Campo: país. Valor: código a excluir.",
    sintaxe: "transaction.country GEO_COUNTRY_NEQ customer.country",
  },
  GEO_DISTANCE_GT: {
    cenario: "Verificar se a distância entre dois pontos é maior que um valor.",
    comoUsar: "Campos: lat/lon origem e destino. Distância: km.",
    sintaxe: "DISTANCE(customer.lat, customer.lon, transaction.lat, transaction.lon) GT 500",
  },
  GEO_DISTANCE_LT: {
    cenario: "Verificar se a distância entre dois pontos é menor que um valor.",
    comoUsar: "Campos: lat/lon origem e destino. Distância: km.",
    sintaxe: "DISTANCE(customer.lat, customer.lon, transaction.lat, transaction.lon) LT 50",
  },

  // ── Agregações ──
  COUNT_GT: {
    cenario: "Verificar se a contagem de eventos é maior que um limite.",
    comoUsar: "Agregação: COUNT. Filtro: período. Limite: número.",
    sintaxe: "COUNT(transactions, last_24h, customer_id) GT 10",
  },
  COUNT_LT: {
    cenario: "Verificar se a contagem de eventos é menor que um limite.",
    comoUsar: "Agregação: COUNT. Filtro: período. Limite: número.",
    sintaxe: "COUNT(transactions, last_1h, card_id) LT 3",
  },
  SUM_GT: {
    cenario: "Verificar se a soma de valores é maior que um limite.",
    comoUsar: "Agregação: SUM. Campo: valor. Período: janela de tempo.",
    sintaxe: "SUM(transactions.amount, last_24h, customer_id) GT 10000",
  },
  SUM_LT: {
    cenario: "Verificar se a soma de valores é menor que um limite.",
    comoUsar: "Agregação: SUM. Campo: valor. Período: janela de tempo.",
    sintaxe: "SUM(transactions.amount, last_7d, customer_id) LT 50000",
  },
  AVG_GT: {
    cenario: "Verificar se a média de valores é maior que um limite.",
    comoUsar: "Agregação: AVG. Campo: valor. Período: janela de tempo.",
    sintaxe: "AVG(transactions.amount, last_30d, customer_id) GT 500",
  },
  AVG_LT: {
    cenario: "Verificar se a média de valores é menor que um limite.",
    comoUsar: "Agregação: AVG. Campo: valor. Período: janela de tempo.",
    sintaxe: "AVG(transactions.amount, last_30d, customer_id) LT 100",
  },
  MAX_GT: {
    cenario: "Verificar se o valor máximo é maior que um limite.",
    comoUsar: "Agregação: MAX. Campo: valor. Período: janela de tempo.",
    sintaxe: "MAX(transactions.amount, last_7d, customer_id) GT 5000",
  },
  MIN_LT: {
    cenario: "Verificar se o valor mínimo é menor que um limite.",
    comoUsar: "Agregação: MIN. Campo: valor. Período: janela de tempo.",
    sintaxe: "MIN(transactions.amount, last_7d, customer_id) LT 10",
  },
  PERCENT_GT: {
    cenario: "Verificar se um percentual é maior que um limite.",
    comoUsar: "Cálculo: percentual. Referência: base. Limite: %.",
    sintaxe: "PERCENT(declined, total_transactions, last_24h) GT 30",
  },

  // ── Comparação entre campos ──
  FIELD_EQ: {
    cenario: "Verificar se dois campos têm o mesmo valor.",
    comoUsar: "Campo 1: primeiro campo. Campo 2: segundo campo.",
    sintaxe: "transaction.billing_country FIELD_EQ transaction.shipping_country",
  },
  FIELD_NEQ: {
    cenario: "Verificar se dois campos têm valores diferentes.",
    comoUsar: "Campo 1: primeiro campo. Campo 2: segundo campo.",
    sintaxe: "transaction.amount FIELD_NEQ transaction.original_amount",
  },
  FIELD_GT: {
    cenario: "Verificar se um campo é maior que outro.",
    comoUsar: "Campo 1: campo a comparar. Campo 2: referência.",
    sintaxe: "transaction.amount FIELD_GT customer.avg_amount",
  },
  FIELD_LT: {
    cenario: "Verificar se um campo é menor que outro.",
    comoUsar: "Campo 1: campo a comparar. Campo 2: referência.",
    sintaxe: "transaction.amount FIELD_LT customer.limit",
  },

  // ── Dispositivo ──
  DEVICE_NEW: {
    cenario: "Verificar se o dispositivo é novo para o cliente.",
    comoUsar: "Campo: device_id. Contexto: customer_id.",
    sintaxe: "DEVICE_NEW(transaction.device_id, customer.id)",
  },
  DEVICE_COUNT_GT: {
    cenario: "Verificar se o número de dispositivos é maior que um limite.",
    comoUsar: "Agregação: contagem de devices. Período: janela de tempo.",
    sintaxe: "DEVICE_COUNT(customer.id, last_24h) GT 3",
  },
  FINGERPRINT_MISMATCH: {
    cenario: "Verificar se o fingerprint não corresponde ao histórico.",
    comoUsar: "Campo: fingerprint atual. Referência: fingerprint esperado.",
    sintaxe: "FINGERPRINT_MISMATCH(transaction.fingerprint, customer.known_fingerprint)",
  },

  // ── MCC ──
  MCC_IN: {
    cenario: "Verificar se o MCC está em uma lista de categorias.",
    comoUsar: "Campo: MCC. Lista: códigos de categoria.",
    sintaxe: "merchant.mcc MCC_IN [\"7995\", \"5933\", \"6051\"]",
  },
  MCC_NOT_IN: {
    cenario: "Verificar se o MCC não está em uma lista de categorias.",
    comoUsar: "Campo: MCC. Lista: códigos a excluir.",
    sintaxe: "merchant.mcc MCC_NOT_IN [\"5411\", \"5812\"]",
  },
  MCC_RISK_HIGH: {
    cenario: "Verificar se o MCC é de alto risco.",
    comoUsar: "Campo: MCC. O sistema avalia automaticamente.",
    sintaxe: "merchant.mcc MCC_RISK_HIGH",
  },

  // ── AML / Compliance ──
  FATF_HIGH_RISK_COUNTRY: {
    cenario: "Verificar se o país está na lista FATF de alto risco.",
    comoUsar: "Campo: código do país (ISO).",
    sintaxe: "transaction.country FATF_HIGH_RISK_COUNTRY",
  },
  FATF_GREY_LIST: {
    cenario: "Verificar se o país está na lista cinza do FATF.",
    comoUsar: "Campo: código do país (ISO).",
    sintaxe: "customer.country FATF_GREY_LIST",
  },
  PEP_CHECK: {
    cenario: "Verificar se o cliente é uma pessoa politicamente exposta.",
    comoUsar: "Campo: CPF ou nome do cliente.",
    sintaxe: "customer.cpf PEP_CHECK",
  },
  SANCTIONS_CHECK: {
    cenario: "Verificar se o cliente está em lista de sanções.",
    comoUsar: "Campo: CPF, nome ou identificador.",
    sintaxe: "customer.name SANCTIONS_CHECK",
  },

  // ── Cartão ──
  CARD_BIN_IN: {
    cenario: "Verificar se o BIN do cartão está em uma lista.",
    comoUsar: "Campo: BIN (6 primeiros dígitos). Lista: BINs.",
    sintaxe: "card.bin CARD_BIN_IN [\"411111\", \"422222\"]",
  },
  CARD_COUNTRY_NEQ: {
    cenario: "Verificar se o país do cartão é diferente do país da transação.",
    comoUsar: "Campo 1: país do cartão. Campo 2: país da transação.",
    sintaxe: "card.country CARD_COUNTRY_NEQ transaction.country",
  },
  CARD_TYPE_EQ: {
    cenario: "Verificar se o tipo de cartão é igual ao esperado.",
    comoUsar: "Campo: tipo do cartão. Valor: CREDIT, DEBIT, PREPAID.",
    sintaxe: "card.type CARD_TYPE_EQ \"PREPAID\"",
  },
  CARD_FIRST_USE: {
    cenario: "Verificar se é o primeiro uso do cartão pelo cliente.",
    comoUsar: "Campo: card_id. Contexto: customer_id.",
    sintaxe: "CARD_FIRST_USE(card.id, customer.id)",
  },

  // ── Email / Telefone / CPF ──
  EMAIL_DISPOSABLE: {
    cenario: "Verificar se o e-mail é de provedor temporário/descartável.",
    comoUsar: "Campo: e-mail do cliente.",
    sintaxe: "customer.email EMAIL_DISPOSABLE",
  },
  EMAIL_DOMAIN_IN: {
    cenario: "Verificar se o domínio do e-mail está em uma lista.",
    comoUsar: "Campo: e-mail. Lista: domínios.",
    sintaxe: "customer.email EMAIL_DOMAIN_IN [\"gmail.com\", \"hotmail.com\"]",
  },
  PHONE_COUNTRY_CODE_EQ: {
    cenario: "Verificar se o código de país do telefone é o esperado.",
    comoUsar: "Campo: telefone. Código: DDI esperado.",
    sintaxe: "customer.phone PHONE_COUNTRY_CODE_EQ \"+55\"",
  },
  CPF_VALID: {
    cenario: "Verificar se o CPF é válido (dígitos verificadores).",
    comoUsar: "Campo: CPF do cliente.",
    sintaxe: "customer.cpf CPF_VALID",
  },
  CPF_MULTIPLE_ACCOUNTS: {
    cenario: "Verificar se o CPF está vinculado a múltiplas contas.",
    comoUsar: "Campo: CPF. Limite: número de contas.",
    sintaxe: "CPF_ACCOUNT_COUNT(customer.cpf) GT 1",
  },

  // ── Velocity ──
  VELOCITY_COUNT: {
    cenario: "Verificar velocidade de eventos (contagem por tempo).",
    comoUsar: "Evento: tipo. Período: janela. Limite: número.",
    sintaxe: "VELOCITY_COUNT(transactions, customer_id, last_1h) GT 5",
  },
  VELOCITY_SUM: {
    cenario: "Verificar velocidade de valores (soma por tempo).",
    comoUsar: "Campo: valor. Período: janela. Limite: número.",
    sintaxe: "VELOCITY_SUM(transactions.amount, customer_id, last_1h) GT 10000",
  },

  // ── Grafo (Neo4j) ──
  NEO4J_CONNECTED_TO: {
    cenario: "Verificar se há conexão com entidade suspeita no grafo.",
    comoUsar: "Entidade: nó origem. Destino: tipo de entidade.",
    sintaxe: "NEO4J_CONNECTED_TO(customer.id, \"FRAUD_RING\")",
  },
  NEO4J_PATH_EXISTS: {
    cenario: "Verificar se existe caminho entre duas entidades.",
    comoUsar: "Origem: nó 1. Destino: nó 2. Profundidade: máximo.",
    sintaxe: "NEO4J_PATH_EXISTS(customer.id, merchant.id, max_depth=3)",
  },
  NEO4J_DEGREE_GT: {
    cenario: "Verificar se o grau de conexões é maior que um limite.",
    comoUsar: "Entidade: nó. Limite: número de conexões.",
    sintaxe: "NEO4J_DEGREE(customer.id) GT 50",
  },
};

// ─────────────────────────────────────────────────────────────────────────────
// FALLBACK para operadores sem exemplo didático específico
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

const deriveDidaticExample = (name: string): DidaticExample => {
  const found = DIDATIC_EXAMPLES[name] || DIDATIC_EXAMPLES[name.toUpperCase()];
  if (found) return found;

  // Fallback genérico baseado em padrões
  const upper = name.toUpperCase();

  if (upper.includes("GT")) return { cenario: "Comparar se é maior que um limite.", comoUsar: "Campo: valor. Limite: número.", sintaxe: `campo ${name} 100` };
  if (upper.includes("LT")) return { cenario: "Comparar se é menor que um limite.", comoUsar: "Campo: valor. Limite: número.", sintaxe: `campo ${name} 100` };
  if (upper.includes("EQ")) return { cenario: "Comparar igualdade.", comoUsar: "Campo: valor. Esperado: valor.", sintaxe: `campo ${name} \"valor\"` };
  if (upper.includes("IN")) return { cenario: "Verificar pertencimento a lista.", comoUsar: "Campo: valor. Lista: valores.", sintaxe: `campo ${name} [\"a\", \"b\"]` };
  if (upper.includes("BETWEEN")) return { cenario: "Verificar faixa de valores.", comoUsar: "Campo: valor. Min e Max: limites.", sintaxe: `campo ${name} 10 AND 100` };
  if (upper.includes("NULL")) return { cenario: "Verificar campo vazio.", comoUsar: "Campo: nome do campo.", sintaxe: `campo ${name}` };
  if (upper.includes("CONTAINS")) return { cenario: "Verificar substring.", comoUsar: "Campo: texto. Busca: substring.", sintaxe: `campo ${name} \"texto\"` };

  return {
    cenario: "Aplicar operador ao campo desejado.",
    comoUsar: "Selecione o campo e configure os parâmetros conforme a documentação.",
    sintaxe: `campo ${name} valor`,
  };
};

const getCategoryGuide = (category: string) =>
  CATEGORY_GUIDE[category] ?? {
    title: category,
    purpose: `Operadores da categoria: ${category}.`,
    tip: "Consulte cada operador para entender o uso específico.",
  };

export default function Operators() {
  const operators = BACKEND_OPERATORS.map((operator) => ({
    ...operator,
    type: normalizeCategory(operator.category),
    purpose: derivePurpose(operator),
    didatic: deriveDidaticExample(operator.name),
  }));

  const grouped = operators.reduce<Record<string, typeof operators>>((acc, op) => {
    acc[op.type] ??= [];
    acc[op.type].push(op);
    return acc;
  }, {});

  const categories = Object.keys(grouped).sort((a, b) => a.localeCompare(b, "pt-BR"));

  return (
    <div className="space-y-6">
      {/* ─── Header ─── */}
      <div className="rounded-lg border bg-card p-5">
        <h1 className="text-xl font-semibold text-foreground">📘 Guia Completo de Operadores</h1>
        <p className="text-sm text-muted-foreground">
          Referência didática com todos os {operators.length} operadores suportados pelo RULEX.
        </p>

        <div className="mt-4 rounded-md border bg-background px-4 py-3 text-sm">
          <div className="font-medium text-foreground">Como usar esta página</div>
          <ul className="mt-2 list-disc space-y-1 pl-5 text-muted-foreground">
            <li>
              <strong className="text-foreground">Quando usar:</strong> descreve o cenário de negócio onde o operador se aplica.
            </li>
            <li>
              <strong className="text-foreground">Como preencher:</strong> explica quais campos e valores configurar na regra.
            </li>
            <li>
              <strong className="text-foreground">Sintaxe DSL:</strong> mostra exatamente como escrever a condição no motor.
            </li>
          </ul>
        </div>

        <div className="mt-4 rounded-md border-l-4 border-blue-500 bg-blue-50 px-4 py-3 text-sm dark:bg-blue-950">
          <div className="font-medium text-blue-800 dark:text-blue-200">💡 Dica</div>
          <p className="text-blue-700 dark:text-blue-300">
            Copie a sintaxe DSL e adapte os campos (ex.: <code className="rounded bg-blue-100 px-1 dark:bg-blue-900">transaction.amount</code>) 
            para os nomes reais do seu payload.
          </p>
        </div>
      </div>

      {/* ─── Categories ─── */}
      {categories.map((category) => {
        const guide = getCategoryGuide(category);
        const list = grouped[category];

        return (
          <section key={category} className="space-y-4">
            {/* Category header */}
            <div className="rounded-lg border bg-card p-4">
              <div className="flex items-center gap-2">
                <span className="text-lg">📂</span>
                <span className="text-base font-semibold text-foreground">{guide.title}</span>
                <span className="rounded-full bg-muted px-2 py-0.5 text-xs text-muted-foreground">
                  {list.length} operadores
                </span>
              </div>
              <p className="mt-1 text-sm text-muted-foreground">{guide.purpose}</p>
              <p className="mt-1 text-xs text-blue-600 dark:text-blue-400">💡 {guide.tip}</p>
            </div>

            {/* Operator cards */}
            <div className="grid gap-4 sm:grid-cols-1 lg:grid-cols-2">
              {list.map((operator) => (
                <div
                  key={operator.name}
                  className="rounded-lg border bg-card p-4 shadow-sm transition-shadow hover:shadow-md"
                >
                  {/* Header */}
                  <div className="flex items-start justify-between gap-2 border-b pb-2">
                    <div>
                      <h2 className="font-mono text-sm font-bold text-foreground">{operator.name}</h2>
                      <p className="text-xs text-muted-foreground">{operator.purpose}</p>
                    </div>
                    <span className="shrink-0 rounded-full border bg-muted px-2 py-0.5 text-xs text-muted-foreground">
                      {operator.type}
                    </span>
                  </div>

                  {/* Didatic content */}
                  <div className="mt-3 space-y-3 text-sm">
                    {/* Cenário */}
                    <div className="rounded-md bg-green-50 p-3 dark:bg-green-950">
                      <div className="flex items-center gap-1 text-xs font-medium text-green-800 dark:text-green-200">
                        <span>🎯</span> Quando usar
                      </div>
                      <p className="mt-1 text-green-700 dark:text-green-300">{operator.didatic.cenario}</p>
                    </div>

                    {/* Como preencher */}
                    <div className="rounded-md bg-amber-50 p-3 dark:bg-amber-950">
                      <div className="flex items-center gap-1 text-xs font-medium text-amber-800 dark:text-amber-200">
                        <span>📝</span> Como preencher
                      </div>
                      <p className="mt-1 text-amber-700 dark:text-amber-300">{operator.didatic.comoUsar}</p>
                    </div>

                    {/* Sintaxe DSL */}
                    <div className="rounded-md bg-slate-100 p-3 dark:bg-slate-800">
                      <div className="flex items-center gap-1 text-xs font-medium text-slate-700 dark:text-slate-200">
                        <span>💻</span> Sintaxe DSL
                      </div>
                      <pre className="mt-1 overflow-x-auto rounded bg-slate-200 p-2 font-mono text-xs text-slate-800 dark:bg-slate-900 dark:text-slate-100">
                        {operator.didatic.sintaxe}
                      </pre>
                    </div>
                  </div>
                </div>
              ))}
            </div>
          </section>
        );
      })}
    </div>
  );
}
