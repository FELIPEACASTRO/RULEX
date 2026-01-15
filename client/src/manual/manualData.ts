/**
 * manualData.ts - Fonte única de dados para a página Manual do RULEX
 *
 * IMPORTANTE: Este arquivo NÃO inventa dados. Importa e re-exporta
 * apenas constantes reais definidas no código do frontend.
 *
 * Gerado a partir de:
 * - client/src/lib/operators.ts (448 operadores)
 * - client/src/lib/fieldLabels.ts (102 campos)
 * - client/src/lib/operatorNullBehavior.ts (semântica NULL)
 * - client/src/components/ComplexRuleBuilder/types.ts
 * - client/src/components/RuleFormDialog/types.ts
 */

// ============================================================================
// OPERADORES (448 operadores do operators.ts)
// ============================================================================
import { OPERATORS, type OperatorDefinition } from "@/lib/operators";

// ============================================================================
// CAMPOS DO PAYLOAD (102 campos do fieldLabels.ts)
// ============================================================================
import {
  FIELD_LABELS,
  getFieldLabel,
  getAllFieldNames,
  searchFields,
} from "@/lib/fieldLabels";

// ============================================================================
// SEMÂNTICA NULL (operatorNullBehavior.ts)
// ============================================================================
import {
  OPERATOR_NULL_BEHAVIORS,
  getNullBehavior,
  type NullBehavior,
} from "@/lib/operatorNullBehavior";

// ============================================================================
// TIPOS DO COMPLEXRULEBUILDER
// ============================================================================
import {
  LOGIC_OPERATORS as COMPLEX_LOGIC_OPERATORS,
  COMPARISON_OPERATORS,
  VALUE_TYPES,
  RULE_STATUSES,
  DECISION_TYPES,
  type LogicOperator,
  type ValueType,
  type RuleStatus,
  type DecisionType,
  type Condition,
  type ConditionGroup,
  type ComplexRule,
} from "@/components/ComplexRuleBuilder/types";

// ============================================================================
// TIPOS DO RULEFORMDIALOG
// ============================================================================
import {
  RULE_TYPES,
  CLASSIFICATIONS,
  LOGIC_OPERATORS as SIMPLE_LOGIC_OPERATORS,
  OPERATORS as SIMPLE_OPERATORS,
  UNARY_OPERATORS,
  FIELD_REF_OPERATORS,
  OPERATORS_BY_TYPE,
  FALLBACK_FIELDS,
} from "@/components/RuleFormDialog/types";

// ============================================================================
// CATEGORIAS DE OPERADORES (extraídas dos 448 operadores)
// ============================================================================
export function getOperatorCategories(): string[] {
  const categories = new Set<string>();
  OPERATORS.forEach((op) => {
    if (op.category) {
      categories.add(op.category);
    }
  });
  return Array.from(categories).sort();
}

export function getOperatorsByCategory(): Record<string, OperatorDefinition[]> {
  const byCategory: Record<string, OperatorDefinition[]> = {};

  OPERATORS.forEach((op) => {
    const cat = op.category || "Outros";
    if (!byCategory[cat]) {
      byCategory[cat] = [];
    }
    byCategory[cat].push(op);
  });

  // Ordenar categorias e operadores dentro de cada categoria
  const sorted: Record<string, OperatorDefinition[]> = {};
  Object.keys(byCategory)
    .sort()
    .forEach((cat) => {
      sorted[cat] = byCategory[cat].sort((a, b) =>
        a.value.localeCompare(b.value)
      );
    });

  return sorted;
}

// ============================================================================
// CATEGORIAS DE CAMPOS (agrupamento lógico)
// ============================================================================
export interface FieldCategory {
  id: string;
  label: string;
  fields: string[];
}

export const FIELD_CATEGORIES: FieldCategory[] = [
  {
    id: "transaction",
    label: "Identificadores da Transação",
    fields: [
      "externalTransactionId",
      "internalTransactionId",
      "transactionAmount",
      "transactionCurrency",
      "transactionDate",
      "transactionTime",
      "transactionType",
      "transactionStatus",
    ],
  },
  {
    id: "customer",
    label: "Cliente",
    fields: ["customerId", "customerName", "customerEmail", "customerPhone"],
  },
  {
    id: "card",
    label: "Cartão",
    fields: [
      "cardNumber",
      "cardBin",
      "cardLast4",
      "cardBrand",
      "cardType",
      "cardExpirationDate",
      "cardCountry",
    ],
  },
  {
    id: "token",
    label: "Token",
    fields: [
      "tokenId",
      "tokenType",
      "tokenStatus",
      "tokenExpirationDate",
      "tokenPan",
    ],
  },
  {
    id: "merchant",
    label: "Merchant",
    fields: [
      "merchantId",
      "merchantName",
      "merchantCategory",
      "mcc",
      "merchantCountry",
      "merchantCity",
      "merchantState",
      "merchantPostalCode",
    ],
  },
  {
    id: "terminal",
    label: "Terminal/POS",
    fields: [
      "terminalId",
      "terminalType",
      "terminalCapability",
      "posEntryMode",
      "posConditionCode",
      "pinEntryCapability",
      "cardholderVerificationMethod",
      "terminalCountry",
      "terminalCity",
    ],
  },
  {
    id: "authentication",
    label: "Autenticação e Verificação",
    fields: [
      "consumerAuthenticationScore",
      "authenticationMethod",
      "threeDSVersion",
      "eci",
      "cavv",
      "xid",
    ],
  },
  {
    id: "cvv",
    label: "CVV/CVV2",
    fields: ["cvvResult", "cvv2Result", "cvvPresent", "cvv2Present"],
  },
  {
    id: "avs",
    label: "AVS",
    fields: ["avsResult"],
  },
  {
    id: "pin",
    label: "PIN",
    fields: ["pinPresent", "pinEntryCount", "pinValidationResult"],
  },
  {
    id: "security",
    label: "Criptografia e Segurança",
    fields: [
      "emvCryptogram",
      "emvCryptogramType",
      "applicationCryptogram",
      "unpredictableNumber",
    ],
  },
  {
    id: "emv",
    label: "EMV (AIP/ATC/TVR)",
    fields: [
      "aip",
      "aipSda",
      "aipDda",
      "aipCardholderVerification",
      "aipTerminalRiskManagement",
      "aipIssuerAuthentication",
      "atc",
      "lastOnlineAtc",
      "tvr",
    ],
  },
  {
    id: "acquirer",
    label: "Acquirer",
    fields: ["acquirerId", "acquirerCountry", "acquirerResponseCode"],
  },
  {
    id: "network",
    label: "Network",
    fields: ["networkId"],
  },
  {
    id: "scores",
    label: "Scores e Autenticação",
    fields: ["fraudScore", "riskScore", "authenticationScore"],
  },
  {
    id: "workflow",
    label: "Workflow e Portfolio",
    fields: ["workflowId", "portfolioId"],
  },
  {
    id: "credit",
    label: "Crédito",
    fields: ["creditLimit"],
  },
  {
    id: "user",
    label: "Campos de Usuário",
    fields: [
      "userField1",
      "userField2",
      "userField3",
      "userField4",
      "userField5",
      "userField6",
      "userField7",
      "userField8",
    ],
  },
  {
    id: "indicators",
    label: "Indicadores de Usuário",
    fields: [
      "userIndicator1",
      "userIndicator2",
      "userIndicator3",
      "userIndicator4",
      "userIndicator5",
    ],
  },
  {
    id: "other",
    label: "Outros",
    fields: [
      "channel",
      "deviceId",
      "deviceType",
      "ipAddress",
      "userAgent",
      "sessionId",
      "geoLatitude",
      "geoLongitude",
      "billingAddress",
      "shippingAddress",
      "installments",
      "recurringIndicator",
    ],
  },
];

// ============================================================================
// EXPLICAÇÕES DIDÁTICAS (estilo "Use a Cabeça")
// ============================================================================
export interface DidacticExplanation {
  oQueFaz: string;
  porQueImportante: string;
  exemploReal: string;
  analogia: string;
  icone: string;
}

export const OPERATOR_CATEGORY_EXPLANATIONS: Record<string, DidacticExplanation> = {
  "Comparação Básica": {
    oQueFaz: "Compara dois valores usando operadores como igual, maior, menor, etc.",
    porQueImportante:
      "É a base de qualquer regra de fraude. Sem comparações, não conseguimos detectar nada.",
    exemploReal:
      "Verificar se o valor da transação é maior que R$10.000 para alertar sobre transações de alto valor.",
    analogia:
      "É como verificar a idade na entrada de uma balada: você compara a idade da pessoa com o limite mínimo.",
    icone: "⚖️",
  },
  Listas: {
    oQueFaz: "Verifica se um valor está presente ou ausente em uma lista predefinida.",
    porQueImportante:
      "Permite criar listas de países bloqueados, MCCs de alto risco, BINs suspeitos, etc.",
    exemploReal:
      "Verificar se o país da transação está na lista de países de alto risco (ex: Nigéria, Rússia).",
    analogia:
      "É como a lista VIP de uma festa: você verifica se o nome está na lista antes de liberar entrada.",
    icone: "📋",
  },
  Strings: {
    oQueFaz: "Opera sobre textos: verifica se contém, começa com, termina com, etc.",
    porQueImportante:
      "Útil para detectar padrões em nomes de merchants, emails suspeitos, etc.",
    exemploReal:
      'Verificar se o nome do merchant contém "CASINO" ou "GAMBLING" para detectar jogos de azar.',
    analogia:
      'É como procurar uma palavra em um livro: você quer saber se "fraude" aparece no texto.',
    icone: "🔤",
  },
  "Nulos/Booleanos": {
    oQueFaz: "Verifica se um campo está nulo, vazio ou é verdadeiro/falso.",
    porQueImportante:
      "Dados faltantes podem indicar fraude. Fraudadores às vezes omitem informações.",
    exemploReal:
      "Verificar se o CVV está ausente em uma transação de e-commerce (pode ser teste de cartão).",
    analogia:
      "É como verificar se alguém deixou campos em branco no formulário - pode ser preguiça ou má intenção.",
    icone: "❓",
  },
  Range: {
    oQueFaz: "Verifica se um valor está dentro de um intervalo específico.",
    porQueImportante:
      "Permite definir faixas de valores normais vs. suspeitos.",
    exemploReal:
      "Verificar se o valor da transação está entre R$1.000 e R$5.000 (faixa de fraude comum).",
    analogia:
      "É como verificar se a temperatura está na faixa normal do corpo humano (36-37°C).",
    icone: "📏",
  },
  "Comparação entre Campos": {
    oQueFaz: "Compara o valor de um campo com outro campo da mesma transação.",
    porQueImportante:
      "Detecta inconsistências onde dois campos deveriam concordar mas não concordam.",
    exemploReal:
      "Verificar se o país do cartão é diferente do país do merchant (possível uso no exterior).",
    analogia:
      "É como verificar se o endereço de entrega bate com o endereço de cobrança.",
    icone: "🔀",
  },
  "Data/Hora": {
    oQueFaz: "Opera sobre datas e horários: verifica intervalos, horário do dia, dia da semana, etc.",
    porQueImportante:
      "Padrões temporais são cruciais. Fraudes acontecem mais à noite e nos fins de semana.",
    exemploReal:
      "Verificar se a transação ocorreu entre 00:00 e 06:00 (horário suspeito).",
    analogia:
      "É como saber que ladrões preferem agir à noite quando há menos vigilância.",
    icone: "🕐",
  },
  Arrays: {
    oQueFaz: "Opera sobre listas de valores: verifica se contém, quantos elementos, intersecção, etc.",
    porQueImportante:
      "Útil para verificar histórico de MCCs, lista de dispositivos usados, etc.",
    exemploReal:
      "Verificar se o dispositivo atual está na lista de dispositivos conhecidos do cliente.",
    analogia:
      "É como verificar se o telefone que está ligando está na sua lista de contatos salvos.",
    icone: "📚",
  },
  Matemáticos: {
    oQueFaz: "Realiza operações matemáticas: módulo, valor absoluto, arredondamento.",
    porQueImportante:
      "Detecta padrões numéricos específicos como valores redondos (teste de cartão).",
    exemploReal:
      "Verificar se o valor da transação é um número redondo (R$100, R$500 - padrão de teste).",
    analogia:
      "É como perceber que alguém está testando senhas com números sequenciais (1234, 5678).",
    icone: "🔢",
  },
  Geolocalização: {
    oQueFaz: "Opera sobre coordenadas geográficas: distância entre pontos, área, raio.",
    porQueImportante:
      "Detecta transações impossíveis geograficamente (ex: São Paulo e Londres em 1 hora).",
    exemploReal:
      "Verificar se a distância entre a transação atual e a anterior é maior que 500km em menos de 1 hora.",
    analogia:
      "É como perceber que alguém carimbou o ponto em São Paulo e depois em Miami no mesmo dia.",
    icone: "🌍",
  },
  Velocity: {
    oQueFaz: "Conta transações em janelas de tempo: última hora, últimas 24h, etc.",
    porQueImportante:
      "Detecta explosões de atividade que indicam fraude em andamento.",
    exemploReal:
      "Verificar se o cartão teve mais de 5 transações na última hora (possível fraude em massa).",
    analogia:
      "É como perceber que alguém está sacando dinheiro em vários caixas ao mesmo tempo.",
    icone: "⚡",
  },
  "Agregações Temporais": {
    oQueFaz: "Calcula estatísticas em janelas de tempo: soma, média, máximo, mínimo.",
    porQueImportante:
      "Permite criar perfis de comportamento e detectar desvios.",
    exemploReal:
      "Verificar se a soma das transações das últimas 24h excede o limite diário do cliente.",
    analogia:
      "É como verificar se alguém gastou mais no cartão este mês do que nos últimos 6 meses.",
    icone: "📊",
  },
  "Fraude Avançada": {
    oQueFaz: "Detecta padrões complexos de fraude: card testing, account takeover, etc.",
    porQueImportante:
      "Fraudes modernas usam técnicas sofisticadas que requerem detecção especializada.",
    exemploReal:
      "Detectar padrão de teste de cartão: múltiplas transações pequenas seguidas de uma grande.",
    analogia:
      "É como perceber que alguém está testando várias chaves até encontrar a que abre a porta.",
    icone: "🔍",
  },
  "Velocity Avançado": {
    oQueFaz: "Velocity com contexto adicional: por merchant, por device, por região.",
    porQueImportante:
      "Detecta padrões que só são visíveis quando agrupados por contexto específico.",
    exemploReal:
      "Verificar quantas transações diferentes vieram do mesmo IP nas últimas 24h.",
    analogia:
      "É como perceber que 50 pedidos de pizza vieram do mesmo telefone - claramente suspeito.",
    icone: "🚀",
  },
  Behavioral: {
    oQueFaz: "Analisa o comportamento do cliente comparando com seu histórico.",
    porQueImportante:
      "Cada cliente tem um padrão normal. Desvios significativos podem indicar fraude.",
    exemploReal:
      "Cliente que sempre compra em SP de repente faz compra de alto valor em outro país.",
    analogia:
      "É como estranhar seu amigo que odeia peixe de repente pedindo sushi todo dia.",
    icone: "🧠",
  },
  "Graph/Network": {
    oQueFaz: "Analisa conexões entre entidades: cartões, devices, merchants, etc.",
    porQueImportante:
      "Fraudes em rede envolvem múltiplas entidades conectadas de forma suspeita.",
    exemploReal:
      "Detectar que 10 cartões diferentes estão sendo usados do mesmo dispositivo.",
    analogia:
      "É como descobrir que todas as contas fake de uma rede social vêm do mesmo computador.",
    icone: "🕸️",
  },
  "Neo4j Graph": {
    oQueFaz: "Consulta banco de dados de grafos Neo4j para análise de redes complexas.",
    porQueImportante:
      "Permite detectar fraudes em anel, lavagem de dinheiro e conexões ocultas.",
    exemploReal:
      "Detectar um anel de fraude onde 5 pessoas transferem dinheiro em círculo.",
    analogia:
      "É como mapear quem conhece quem em uma rede de crime organizado.",
    icone: "🔗",
  },
  Sanctions: {
    oQueFaz: "Verifica listas de sanções: OFAC, EU, ONU, PEPs.",
    porQueImportante:
      "Obrigatório por regulamentação. Transações com entidades sancionadas são ilegais.",
    exemploReal:
      "Verificar se o beneficiário está na lista OFAC antes de aprovar transferência.",
    analogia:
      'É como verificar se alguém está na "lista negra" antes de fazer negócio.',
    icone: "🚫",
  },
  "Synthetic ID": {
    oQueFaz: "Detecta identidades sintéticas criadas combinando dados reais e falsos.",
    porQueImportante:
      "Fraude de identidade sintética é uma das mais difíceis de detectar.",
    exemploReal:
      "Detectar CPF recém-criado com histórico de crédito impossível para a idade.",
    analogia:
      'É como perceber que alguém criou um "Frankenstein" de identidade juntando partes de várias pessoas.',
    icone: "🎭",
  },
  AML: {
    oQueFaz: "Anti-Money Laundering: detecta padrões de lavagem de dinheiro.",
    porQueImportante:
      "Obrigatório por regulamentação. Instituições devem reportar atividades suspeitas.",
    exemploReal:
      "Detectar estruturação: múltiplos depósitos de R$9.900 para evitar limite de R$10.000.",
    analogia:
      "É como perceber que alguém está depositando dinheiro aos poucos para não chamar atenção.",
    icone: "💰",
  },
  Regulatory: {
    oQueFaz: "Verifica conformidade com regulamentações: PCI, LGPD, Bacen, etc.",
    porQueImportante:
      "Não-conformidade resulta em multas pesadas e perda de licenças.",
    exemploReal:
      "Verificar se dados sensíveis estão sendo mascarados conforme LGPD.",
    analogia:
      "É como verificar se o restaurante está seguindo todas as normas da vigilância sanitária.",
    icone: "📜",
  },
  Device: {
    oQueFaz: "Analisa características do dispositivo: fingerprint, emulador, VPN, etc.",
    porQueImportante:
      "Dispositivos suspeitos (emuladores, VPNs) são frequentemente usados em fraudes.",
    exemploReal:
      "Detectar se a transação vem de um emulador Android (fraude automatizada).",
    analogia:
      "É como perceber que alguém está usando peruca, óculos escuros e bigode falso.",
    icone: "📱",
  },
  "Merchant/MCC": {
    oQueFaz: "Analisa características do merchant e seu código de categoria (MCC).",
    porQueImportante:
      "Alguns MCCs são de alto risco: jogos, criptomoedas, conteúdo adulto.",
    exemploReal:
      "Verificar se o MCC 7995 (gambling) está sendo usado em transação de alto valor.",
    analogia:
      "É como saber que algumas lojas são mais propensas a vender produtos falsificados.",
    icone: "🏪",
  },
  Estatísticos: {
    oQueFaz: "Calcula estatísticas: desvio padrão, percentil, z-score.",
    porQueImportante:
      "Detecta anomalias estatísticas que indicam comportamento fora do normal.",
    exemploReal:
      "Verificar se o valor da transação está 3 desvios padrão acima da média do cliente.",
    analogia:
      "É como perceber que alguém cresceu 30cm em um mês - estatisticamente impossível.",
    icone: "📈",
  },
  "Velocity Phase 1": {
    oQueFaz: "Velocity básico para detecção inicial: contagens simples em janelas fixas.",
    porQueImportante:
      "Primeira linha de defesa contra explosões de atividade fraudulenta.",
    exemploReal:
      "Verificar se houve mais de 3 transações do mesmo cartão nos últimos 10 minutos.",
    analogia:
      "É o alarme que dispara quando alguém tenta a senha errada 3 vezes seguidas.",
    icone: "🔔",
  },
  "Behavioral Phase 1B": {
    oQueFaz: "Análise comportamental de segunda fase com contexto histórico.",
    porQueImportante:
      "Permite comparar comportamento atual com baseline histórico do cliente.",
    exemploReal:
      "Verificar se o cliente está comprando em categoria que nunca comprou antes.",
    analogia:
      "É como seu banco ligar perguntando se foi você que comprou passagem para Dubai.",
    icone: "🎯",
  },
  PLT: {
    oQueFaz: "Payment Lifecycle Tracking: rastreia todo o ciclo de vida do pagamento.",
    porQueImportante:
      "Permite análise completa desde autorização até liquidação.",
    exemploReal:
      "Verificar se há muitos chargebacks para transações de determinado merchant.",
    analogia:
      "É como acompanhar uma encomenda desde o pedido até a entrega na sua casa.",
    icone: "🔄",
  },
};

// ============================================================================
// NULL BEHAVIOR EXPLICAÇÕES
// ============================================================================
export const NULL_BEHAVIOR_LABELS: Record<NullBehavior, string> = {
  returns_false: "Retorna FALSE se campo for NULL",
  returns_true: "Retorna TRUE se campo for NULL",
  checks_null: "Verifica especificamente se é NULL",
  context_dependent: "Depende do contexto (Redis/Graph)",
  not_applicable: "Não recebe campo diretamente",
};

export const NULL_BEHAVIOR_DESCRIPTIONS: Record<NullBehavior, string> = {
  returns_false:
    "Se o campo estiver NULL, a condição automaticamente retorna FALSE. Exemplo: amount > 100 com amount=NULL retorna FALSE.",
  returns_true:
    "Se o campo estiver NULL, a condição automaticamente retorna TRUE. Exemplo: amount NOT_IN [100, 200] com amount=NULL retorna TRUE (NULL não está na lista).",
  checks_null:
    "O operador existe especificamente para verificar NULL. Exemplo: IS_NULL verifica se o campo é nulo.",
  context_dependent:
    "O comportamento depende de onde os dados são buscados (Redis, Neo4j, etc.). Pode falhar silenciosamente ou usar valor default.",
  not_applicable:
    "Este operador não recebe um campo diretamente, então o conceito de NULL não se aplica.",
};

// ============================================================================
// TEMPLATES (baseado em TemplateSelector.tsx)
// ============================================================================
export interface ManualTemplate {
  id: string;
  name: string;
  description: string;
  category: string;
  icon: string;
  conditions: string[];
  explanation: DidacticExplanation;
}

export const MANUAL_TEMPLATES: ManualTemplate[] = [
  {
    id: "high-amount",
    name: "Transação de Alto Valor",
    description: "Detecta transações acima de R$10.000",
    category: "Valor",
    icon: "💰",
    conditions: ["transactionAmount > 10000"],
    explanation: {
      oQueFaz: "Alerta quando uma transação excede um valor considerado alto.",
      porQueImportante:
        "Transações de alto valor são mais atrativas para fraudadores e merecem atenção especial.",
      exemploReal:
        "Compra de R$15.000 em eletrônicos às 3h da manhã de um cartão que normalmente gasta R$500/mês.",
      analogia:
        "É como o segurança da loja ficar mais atento quando alguém entra e pede o produto mais caro.",
      icone: "💰",
    },
  },
  {
    id: "foreign-country",
    name: "País Estrangeiro",
    description: "Detecta transações em países diferentes do cartão",
    category: "Geolocalização",
    icon: "🌍",
    conditions: ["merchantCountry != cardCountry"],
    explanation: {
      oQueFaz:
        "Alerta quando a transação ocorre em um país diferente do país de emissão do cartão.",
      porQueImportante:
        "Uso internacional inesperado pode indicar cartão clonado ou roubado.",
      exemploReal:
        "Cartão emitido no Brasil sendo usado na Rússia sem histórico de viagens.",
      analogia:
        "É como receber uma ligação do seu número de telefone vindo de outro país.",
      icone: "🌍",
    },
  },
  {
    id: "night-transaction",
    name: "Transação Noturna",
    description: "Detecta transações entre 00:00 e 06:00",
    category: "Horário",
    icon: "🌙",
    conditions: ["transactionTime BETWEEN '00:00' AND '06:00'"],
    explanation: {
      oQueFaz:
        "Alerta para transações realizadas durante a madrugada.",
      porQueImportante:
        "Fraudes frequentemente ocorrem à noite quando o titular está dormindo.",
      exemploReal:
        "Sequência de compras online às 3h da manhã quando o cliente nunca comprou nesse horário.",
      analogia:
        "É como alguém batendo na sua porta às 3h da manhã - pode ser emergência ou problema.",
      icone: "🌙",
    },
  },
  {
    id: "high-risk-mcc",
    name: "MCC de Alto Risco",
    description: "Detecta transações em categorias de alto risco (gambling, crypto)",
    category: "Categoria",
    icon: "⚠️",
    conditions: ["mcc IN [7995, 6211, 5967, 5966]"],
    explanation: {
      oQueFaz:
        "Alerta para transações em merchants de categorias consideradas de alto risco.",
      porQueImportante:
        "Jogos de azar, criptomoedas e produtos digitais têm alto índice de fraude e chargeback.",
      exemploReal:
        "Primeira transação de um cartão novo é em site de apostas - alto risco de fraude.",
      analogia:
        "É como verificar com mais cuidado quem entra em áreas restritas de um prédio.",
      icone: "⚠️",
    },
  },
  {
    id: "low-auth-score",
    name: "Score de Autenticação Baixo",
    description: "Detecta transações com score de autenticação abaixo de 50",
    category: "Autenticação",
    icon: "🛡️",
    conditions: ["consumerAuthenticationScore < 50"],
    explanation: {
      oQueFaz:
        "Alerta quando o score de autenticação do 3DS está abaixo do limiar aceitável.",
      porQueImportante:
        "Score baixo indica que a autenticação pode ter falhado ou ser fraudulenta.",
      exemploReal:
        "Transação de alto valor com score 3DS de 20 - provavelmente não foi o titular.",
      analogia:
        "É como alguém tentando entrar com crachá mas a foto não bate muito bem.",
      icone: "🛡️",
    },
  },
  {
    id: "complex-high-amount-foreign",
    name: "Alto Valor + País Estrangeiro",
    description: "Combina alto valor com país estrangeiro",
    category: "Combinada",
    icon: "⚡",
    conditions: ["transactionAmount > 5000", "merchantCountry != cardCountry"],
    explanation: {
      oQueFaz:
        "Alerta quando há combinação de alto valor E uso no exterior.",
      porQueImportante:
        "A combinação de múltiplos fatores de risco aumenta significativamente a probabilidade de fraude.",
      exemploReal:
        "Compra de R$8.000 em joalheria em Dubai de cartão brasileiro sem histórico de viagens.",
      analogia:
        "É como quando múltiplos alarmes disparam ao mesmo tempo - algo sério está acontecendo.",
      icone: "⚡",
    },
  },
  {
    id: "complex-nested",
    name: "Regra com Grupos Aninhados",
    description: "Exemplo de regra complexa: (A AND B) OR (C AND D)",
    category: "Avançada",
    icon: "🔧",
    conditions: [
      "(transactionAmount > 10000 AND merchantCountry = 'US')",
      "OR",
      "(transactionAmount > 5000 AND transactionTime BETWEEN '00:00' AND '06:00')",
    ],
    explanation: {
      oQueFaz:
        "Permite criar regras complexas com múltiplas condições agrupadas com lógica AND/OR.",
      porQueImportante:
        "Fraudes reais raramente se encaixam em uma única condição simples.",
      exemploReal:
        "Alertar se: (valor alto E país estrangeiro) OU (valor médio E madrugada E MCC suspeito).",
      analogia:
        "É como um sistema de segurança que ativa quando (porta aberta E alarme desligado) OU (janela quebrada).",
      icone: "🔧",
    },
  },
];

// ============================================================================
// ESTATÍSTICAS GERAIS
// ============================================================================
export const MANUAL_STATS = {
  totalOperators: OPERATORS.length,
  totalFields: Object.keys(FIELD_LABELS).length,
  totalTemplates: MANUAL_TEMPLATES.length,
  totalCategories: getOperatorCategories().length,
  totalFieldCategories: FIELD_CATEGORIES.length,
};

// ============================================================================
// MANTER COMPATIBILIDADE COM VERSÃO ANTERIOR (MANUAL_DATA)
// ============================================================================
export const MANUAL_DATA = {
  generatedFrom: {
    ruleFormDialog: {
      ruleTypes: RULE_TYPES,
      classifications: CLASSIFICATIONS,
      logicOperators: SIMPLE_LOGIC_OPERATORS,
      operators: SIMPLE_OPERATORS,
      unaryOperators: UNARY_OPERATORS,
      fieldRefOperators: FIELD_REF_OPERATORS,
      operatorsByType: OPERATORS_BY_TYPE,
    },
    complexRuleBuilder: {
      logicOperators: COMPLEX_LOGIC_OPERATORS,
      comparisonOperators: COMPARISON_OPERATORS,
      valueTypes: VALUE_TYPES,
    },
  },
} as const;

// ============================================================================
// RE-EXPORTS PARA CONVENIÊNCIA
// ============================================================================
export {
  // Operators
  OPERATORS,
  type OperatorDefinition,
  // Fields
  FIELD_LABELS,
  getFieldLabel,
  getAllFieldNames,
  searchFields,
  // NULL Behavior
  OPERATOR_NULL_BEHAVIORS,
  getNullBehavior,
  type NullBehavior,
  // ComplexRuleBuilder
  COMPLEX_LOGIC_OPERATORS,
  COMPARISON_OPERATORS,
  VALUE_TYPES,
  RULE_STATUSES,
  DECISION_TYPES,
  type LogicOperator,
  type ValueType,
  type RuleStatus,
  type DecisionType,
  type Condition,
  type ConditionGroup,
  type ComplexRule,
  // RuleFormDialog
  RULE_TYPES,
  CLASSIFICATIONS,
  SIMPLE_LOGIC_OPERATORS,
  SIMPLE_OPERATORS,
  UNARY_OPERATORS,
  FIELD_REF_OPERATORS,
  OPERATORS_BY_TYPE,
  FALLBACK_FIELDS,
};
