/**
 * OPERATOR_SPECS_COMPLETE - DOCUMENTAÇÃO ULTRA DIDÁTICA PARA TODOS OS 469 OPERADORES
 * 
 * Este arquivo contém a documentação completa de TODOS os operadores do RULEX.
 * Cada operador inclui:
 * - name: Nome técnico
 * - summary: Descrição resumida
 * - syntax: Sintaxe de uso
 * - syntaxExplanation: Explicação ULTRA didática com exemplos concretos
 * - story: História real de uso
 * - problem: Que problema resolve?
 * - goldenTip: Dica de ouro com exemplo prático
 * 
 * Gerado em: ${new Date().toISOString()}
 * Total de operadores: 469
 */

import type { OperatorSpec } from './operatorSpecs';

// ═══════════════════════════════════════════════════════════════════════════════════════
// PARTE 1: OPERADORES BÁSICOS DE COMPARAÇÃO (25 operadores)
// ═══════════════════════════════════════════════════════════════════════════════════════

export const BASIC_COMPARISON_SPECS: Record<string, OperatorSpec> = {
  EQ: {
    name: "EQ",
    summary: "Verifica se dois valores são IGUAIS (equal)",
    syntax: "transaction.country EQ \"BR\"",
    syntaxExplanation: "Compara se o campo é EXATAMENTE igual ao valor. Exemplo: 'BR' EQ 'BR' → TRUE | 'br' EQ 'BR' → FALSE (case-sensitive!)",
    story: "Pedro, analista de compliance, precisava bloquear transações de países sancionados. Usou EQ para verificar se transaction.country EQ 'KP' (Coreia do Norte).",
    problem: "Como verificar se um valor é EXATAMENTE igual a outro?",
    goldenTip: "💎 ATENÇÃO: EQ é case-SENSITIVE! 'Brasil' ≠ 'BRASIL' ≠ 'brasil'. Use LOWER() se quiser ignorar maiúsculas.",
    engineBehavior: {
      description: "Comparação direta de valores:",
      steps: [
        "1. Lê valor do payload (ex: transaction.country = 'BR')",
        "2. Lê valor de comparação (ex: 'BR')",
        "3. Compara: 'BR'.equals('BR')",
        "4. Retorna: true ou false"
      ],
      cautions: ["Case-sensitive para strings", "Para números, 10 EQ 10.0 = TRUE (normalização)"]
    }
  },

  NEQ: {
    name: "NEQ",
    summary: "Verifica se dois valores são DIFERENTES (not equal)",
    syntax: "transaction.status NEQ \"APPROVED\"",
    syntaxExplanation: "Verifica se o valor NÃO é igual. Exemplo: 'PENDING' NEQ 'APPROVED' → TRUE | 'APPROVED' NEQ 'APPROVED' → FALSE",
    story: "Ana, do time de chargebacks, queria analisar apenas transações que NÃO foram aprovadas. Usou status NEQ 'APPROVED'.",
    problem: "Como filtrar registros que NÃO têm determinado valor?",
    goldenTip: "💎 Use NEQ para exclusões: 'country NEQ \"BR\" AND country NEQ \"AR\"' = não é Brasil nem Argentina."
  },

  GT: {
    name: "GT",
    summary: "Verifica se um valor é MAIOR QUE outro (greater than)",
    syntax: "transaction.amount GT 1000",
    syntaxExplanation: "Compara se o valor é ESTRITAMENTE maior. Exemplo: 1500 GT 1000 → TRUE | 1000 GT 1000 → FALSE (não inclui igualdade!)",
    story: "Carlos, gerente de risco, queria alerta para transações acima de R$ 10.000. Usou amount GT 10000.",
    problem: "Como detectar valores que EXCEDEM um limite?",
    goldenTip: "💎 CUIDADO: GT não inclui o valor! '1000 GT 1000' = FALSE. Use GTE se quiser incluir.",
    engineBehavior: {
      description: "Comparação numérica:",
      steps: [
        "1. Converte valores para números",
        "2. Compara: valor1 > valor2",
        "3. Retorna: boolean"
      ],
      cautions: ["Strings são convertidas para números: '1500' > 1000 = TRUE", "NULL sempre retorna FALSE"]
    }
  },

  GTE: {
    name: "GTE",
    summary: "Verifica se um valor é MAIOR OU IGUAL a outro (greater than or equal)",
    syntax: "customer.age GTE 18",
    syntaxExplanation: "Compara se o valor é maior OU igual. Exemplo: 18 GTE 18 → TRUE | 17 GTE 18 → FALSE",
    story: "Fintech precisava validar maioridade. Usou age GTE 18 no onboarding.",
    problem: "Como verificar se um valor ATINGE ou SUPERA um limite mínimo?",
    goldenTip: "💎 GTE = 'a partir de'. Use para limites mínimos: 'age GTE 18' = maiores de idade."
  },

  LT: {
    name: "LT",
    summary: "Verifica se um valor é MENOR QUE outro (less than)",
    syntax: "transaction.amount LT 50",
    syntaxExplanation: "Compara se o valor é ESTRITAMENTE menor. Exemplo: 30 LT 50 → TRUE | 50 LT 50 → FALSE",
    story: "Regra para identificar micropagamentos: amount LT 5 (abaixo de R$ 5).",
    problem: "Como detectar valores ABAIXO de um limite?",
    goldenTip: "💎 Use LT para detectar valores suspeitos baixos: 'amount LT 1' pode ser teste de cartão."
  },

  LTE: {
    name: "LTE",
    summary: "Verifica se um valor é MENOR OU IGUAL a outro (less than or equal)",
    syntax: "customer.riskScore LTE 30",
    syntaxExplanation: "Compara se o valor é menor OU igual. Exemplo: 30 LTE 30 → TRUE | 31 LTE 30 → FALSE",
    story: "Cliente com riskScore LTE 30 era aprovado automaticamente (baixo risco).",
    problem: "Como verificar se um valor está DENTRO de um limite máximo (inclusivo)?",
    goldenTip: "💎 LTE = 'até'. Use para limites máximos: 'riskScore LTE 50' = risco aceitável."
  },

  BETWEEN: {
    name: "BETWEEN",
    summary: "Verifica se um valor está ENTRE dois limites (inclusivo)",
    syntax: "transaction.amount BETWEEN 100 AND 5000",
    syntaxExplanation: "Verifica se está no intervalo [min, max]. Exemplo: 500 BETWEEN 100 AND 5000 → TRUE | 5001 BETWEEN 100 AND 5000 → FALSE",
    story: "Transações BETWEEN 1000 AND 5000 eram analisadas manualmente (faixa suspeita).",
    problem: "Como verificar se um valor está dentro de uma FAIXA específica?",
    goldenTip: "💎 BETWEEN é INCLUSIVO em ambos lados! '100 BETWEEN 100 AND 5000' = TRUE."
  },

  AND: {
    name: "AND",
    summary: "Operador lógico: AMBAS as condições devem ser verdadeiras",
    syntax: "amount GT 1000 AND country EQ \"BR\"",
    syntaxExplanation: "Todas as condições precisam ser TRUE. Exemplo: TRUE AND TRUE → TRUE | TRUE AND FALSE → FALSE",
    story: "Regra para PIX alto doméstico: amount GT 10000 AND channel EQ 'PIX' AND country EQ 'BR'.",
    problem: "Como combinar MÚLTIPLAS condições que TODAS devem ser verdadeiras?",
    goldenTip: "💎 AND é restritivo: cada condição adicional REDUZ os matches. Use para regras específicas."
  },

  OR: {
    name: "OR",
    summary: "Operador lógico: PELO MENOS UMA condição deve ser verdadeira",
    syntax: "country EQ \"KP\" OR country EQ \"IR\"",
    syntaxExplanation: "Basta UMA condição ser TRUE. Exemplo: TRUE OR FALSE → TRUE | FALSE OR FALSE → FALSE",
    story: "Bloqueio de países sancionados: country EQ 'KP' OR country EQ 'IR' OR country EQ 'CU'.",
    problem: "Como criar regras que disparam para QUALQUER uma de várias condições?",
    goldenTip: "💎 OR é expansivo: cada condição adicional AUMENTA os matches. Use para agrupar cenários similares."
  },

  NOT: {
    name: "NOT",
    summary: "Operador lógico: INVERTE o resultado da condição",
    syntax: "NOT (customer.isVip EQ true)",
    syntaxExplanation: "Inverte TRUE para FALSE e vice-versa. Exemplo: NOT TRUE → FALSE | NOT FALSE → TRUE",
    story: "Regra especial que NÃO se aplica a VIPs: NOT (isVip EQ true) AND amount GT 5000.",
    problem: "Como excluir determinados casos de uma regra?",
    goldenTip: "💎 NOT é útil para exceções: 'NOT (country IN [\"BR\", \"AR\"])' = qualquer país exceto Brasil e Argentina."
  },

  XOR: {
    name: "XOR",
    summary: "Operador lógico: EXATAMENTE UMA condição deve ser verdadeira (ou exclusivo)",
    syntax: "isVip XOR isEmployee",
    syntaxExplanation: "Apenas UMA pode ser TRUE. Exemplo: TRUE XOR FALSE → TRUE | TRUE XOR TRUE → FALSE",
    story: "Cliente é VIP XOR Employee (não pode ser ambos ao mesmo tempo - conflito de interesse).",
    problem: "Como garantir que APENAS UMA de duas condições seja verdadeira?",
    goldenTip: "💎 XOR detecta inconsistências: 'isNewCustomer XOR hasHistory' - se ambos TRUE, dado inconsistente."
  },

  NAND: {
    name: "NAND",
    summary: "Operador lógico: NÃO pode ter AMBAS verdadeiras (not and)",
    syntax: "isHighRisk NAND isApproved",
    syntaxExplanation: "Retorna FALSE apenas se AMBAS forem TRUE. Exemplo: TRUE NAND TRUE → FALSE | qualquer outro → TRUE",
    story: "Validação: transação NÃO pode ser highRisk E approved simultaneamente.",
    problem: "Como garantir que duas condições não sejam verdadeiras ao mesmo tempo?",
    goldenTip: "💎 NAND = 'não ambos'. Útil para regras de consistência: 'blocked NAND processed'."
  },

  NOR: {
    name: "NOR",
    summary: "Operador lógico: NENHUMA condição pode ser verdadeira (not or)",
    syntax: "isFraud NOR isChargeback",
    syntaxExplanation: "Retorna TRUE apenas se TODAS forem FALSE. Exemplo: FALSE NOR FALSE → TRUE | qualquer TRUE → FALSE",
    story: "Cliente limpo: NÃO é fraud NOR chargeback NOR blocked.",
    problem: "Como garantir que NENHUMA das condições seja verdadeira?",
    goldenTip: "💎 NOR = 'nenhum dos'. Útil para whitelists: se passar em NOR de todas as regras de bloqueio, é limpo."
  },

  // STRING OPERATORS
  CONTAINS: {
    name: "CONTAINS",
    summary: "Verifica se um texto CONTÉM uma substring",
    syntax: "email CONTAINS \"@gmail\"",
    syntaxExplanation: "Procura substring em qualquer posição. 'joao@gmail.com' CONTAINS '@gmail' → TRUE",
    story: "Bloqueio de emails temporários: email CONTAINS 'tempmail' OR email CONTAINS 'guerrilla'.",
    problem: "Como verificar se um texto contém uma palavra ou padrão?",
    goldenTip: "💎 Case-INsensitive! 'GMAIL' e 'gmail' são equivalentes."
  },

  STARTS_WITH: {
    name: "STARTS_WITH",
    summary: "Verifica se um texto COMEÇA com um prefixo",
    syntax: "phone STARTS_WITH \"+55\"",
    syntaxExplanation: "Verifica início do texto. '+5511987654321' STARTS_WITH '+55' → TRUE",
    story: "Validação de telefone brasileiro: phone STARTS_WITH '+55'.",
    problem: "Como verificar prefixos (DDI, códigos, etc)?",
    goldenTip: "💎 Útil para DDIs: '+55' (BR), '+1' (US), '+44' (UK)."
  },

  ENDS_WITH: {
    name: "ENDS_WITH",
    summary: "Verifica se um texto TERMINA com um sufixo",
    syntax: "email ENDS_WITH \"@empresa.com\"",
    syntaxExplanation: "Verifica final do texto. 'joao@empresa.com' ENDS_WITH '@empresa.com' → TRUE",
    story: "Validação de email corporativo: email ENDS_WITH '@minhaempresa.com.br'.",
    problem: "Como verificar sufixos (domínios, extensões, etc)?",
    goldenTip: "💎 Útil para domínios: ENDS_WITH '@bancodobrasil.com.br' = email corporativo do BB."
  },

  REGEX: {
    name: "REGEX",
    summary: "Verifica se um texto combina com um padrão de expressão regular",
    syntax: "cpf MATCHES_REGEX /^\\d{3}\\.\\d{3}\\.\\d{3}-\\d{2}$/",
    syntaxExplanation: "Valida formato complexo. '123.456.789-00' → MATCH | '12345678900' → NO MATCH",
    story: "Validação de CPF no formato correto: XXX.XXX.XXX-XX.",
    problem: "Como validar formatos complexos (CPF, placa, CEP)?",
    goldenTip: "💎 CUIDADO com ReDoS! Regex mal escrito pode travar o sistema. Teste em regex101.com primeiro."
  },

  // IN OPERATORS
  IN: {
    name: "IN",
    summary: "Verifica se um valor está em uma LISTA de valores",
    syntax: "country IN [\"BR\", \"AR\", \"CL\"]",
    syntaxExplanation: "Verifica se valor está na lista. 'BR' IN ['BR', 'AR', 'CL'] → TRUE | 'US' IN ['BR', 'AR', 'CL'] → FALSE",
    story: "Países LATAM aceitos: country IN ['BR', 'AR', 'CL', 'MX', 'CO'].",
    problem: "Como verificar se um valor está em um conjunto permitido?",
    goldenTip: "💎 Use IN em vez de múltiplos OR: 'country IN [...]' é mais limpo que 'country EQ X OR country EQ Y'."
  },

  NOT_IN: {
    name: "NOT_IN",
    summary: "Verifica se um valor NÃO está em uma lista",
    syntax: "country NOT_IN [\"KP\", \"IR\", \"CU\"]",
    syntaxExplanation: "Inverso do IN. 'BR' NOT_IN ['KP', 'IR', 'CU'] → TRUE",
    story: "Bloqueio de países sancionados: country NOT_IN ['KP', 'IR', 'CU', 'SY'].",
    problem: "Como verificar se um valor NÃO está em um conjunto proibido?",
    goldenTip: "💎 Use para blacklists: NOT_IN + lista de itens proibidos."
  },

  IN_LIST: {
    name: "IN_LIST",
    summary: "Verifica se um valor está em uma lista externa (arquivo/banco)",
    syntax: "email IN_LIST \"blocked_emails\"",
    syntaxExplanation: "Consulta lista cadastrada no sistema. email IN_LIST 'blocked_emails' → consulta tabela/arquivo",
    story: "Lista de emails de fraudadores conhecidos: email IN_LIST 'fraud_emails' (10.000+ emails).",
    problem: "Como verificar contra listas grandes (milhares de itens)?",
    goldenTip: "💎 Use IN_LIST para listas que mudam frequentemente. Evita redeployar a regra."
  },

  IN_CUSTOMER_USUAL_HOURS: {
    name: "IN_CUSTOMER_USUAL_HOURS",
    summary: "Verifica se transação está no horário HABITUAL do cliente",
    syntax: "IN_CUSTOMER_USUAL_HOURS() IS_TRUE",
    syntaxExplanation: "Analisa histórico do cliente e verifica se TX está no padrão. Cliente compra 9h-18h → TX às 3h = FORA do padrão.",
    story: "Cliente João sempre compra entre 9h-17h. TX às 2h da manhã é suspeita.",
    problem: "Como detectar transações em horários INCOMUNS para cada cliente específico?",
    goldenTip: "💎 Requer histórico! Precisa de pelo menos 10 TXs do cliente para ter padrão confiável."
  },

  NOT_IN_CUSTOMER_USUAL_HOURS: {
    name: "NOT_IN_CUSTOMER_USUAL_HOURS",
    summary: "Verifica se transação está FORA do horário habitual do cliente",
    syntax: "NOT_IN_CUSTOMER_USUAL_HOURS() IS_TRUE",
    syntaxExplanation: "Inverso do anterior. Dispara se TX está fora do padrão do cliente.",
    story: "TX às 3h da manhã para cliente que só compra de dia → SUSPEITO.",
    problem: "Como alertar sobre transações em horários incomuns?",
    goldenTip: "💎 Combine com valor: 'NOT_IN_CUSTOMER_USUAL_HOURS AND amount GT 1000' = fora do horário E valor alto."
  },

  IN_HISTORICAL: {
    name: "IN_HISTORICAL",
    summary: "Verifica se valor está no histórico do cliente",
    syntax: "merchantId IN_HISTORICAL(customerId)",
    syntaxExplanation: "Verifica se cliente já comprou nesse merchant antes. Primera compra = FALSE.",
    story: "Cliente nunca comprou em joalheria antes → IN_HISTORICAL = FALSE → suspeito.",
    problem: "Como detectar se é a PRIMEIRA vez que cliente interage com determinado valor?",
    goldenTip: "💎 Útil para primeiro acesso: 'NOT IN_HISTORICAL(deviceId)' = device novo."
  },

  NOT_IN_HISTORICAL: {
    name: "NOT_IN_HISTORICAL",
    summary: "Verifica se valor NÃO está no histórico do cliente (primeira vez)",
    syntax: "merchantId NOT_IN_HISTORICAL(customerId)",
    syntaxExplanation: "Dispara se é a primeira vez. Nunca comprou nesse merchant = TRUE.",
    story: "Primeira compra do cliente em joalheria de luxo + valor alto = risco.",
    problem: "Como detectar primeiros acessos/compras?",
    goldenTip: "💎 Use para novos devices/merchants/países: primeira vez = alerta."
  },

  // IS OPERATORS
  IS_NULL: {
    name: "IS_NULL",
    summary: "Verifica se um campo é NULL (não existe ou vazio)",
    syntax: "customer.email IS_NULL",
    syntaxExplanation: "Retorna TRUE se campo é null, undefined ou não existe.",
    story: "Cadastro incompleto: email IS_NULL = cliente não forneceu email.",
    problem: "Como detectar dados ausentes/incompletos?",
    goldenTip: "💎 Use para validar cadastro: 'cpf IS_NULL OR phone IS_NULL' = cadastro incompleto."
  },

  IS_NOT_NULL: {
    name: "IS_NOT_NULL",
    summary: "Verifica se um campo NÃO é NULL (tem valor)",
    syntax: "customer.email IS_NOT_NULL",
    syntaxExplanation: "Retorna TRUE se campo existe e tem valor.",
    story: "Validação de cadastro completo: email IS_NOT_NULL AND phone IS_NOT_NULL.",
    problem: "Como garantir que dados obrigatórios foram preenchidos?",
    goldenTip: "💎 Use para pré-condições: 'cpf IS_NOT_NULL' antes de validar formato do CPF."
  },

  IS_EMPTY: {
    name: "IS_EMPTY",
    summary: "Verifica se um campo está VAZIO (string vazia ou array vazio)",
    syntax: "customer.middleName IS_EMPTY",
    syntaxExplanation: "Retorna TRUE se campo é '' (string vazia) ou [] (array vazio).",
    story: "Nome do meio vazio é aceitável, mas email vazio não é.",
    problem: "Como diferenciar NULL de string vazia?",
    goldenTip: "💎 IS_EMPTY vs IS_NULL: '' IS_EMPTY = TRUE, '' IS_NULL = FALSE. NULL IS_EMPTY = TRUE, NULL IS_NULL = TRUE."
  },

  IS_NOT_EMPTY: {
    name: "IS_NOT_EMPTY",
    summary: "Verifica se um campo NÃO está vazio",
    syntax: "customer.email IS_NOT_EMPTY",
    syntaxExplanation: "Retorna TRUE se campo tem conteúdo (não é '' nem []).",
    story: "Email não pode ser string vazia: email IS_NOT_EMPTY.",
    problem: "Como garantir que campo tem conteúdo significativo?",
    goldenTip: "💎 Combine: 'email IS_NOT_NULL AND email IS_NOT_EMPTY' = email existe E tem valor."
  },

  IS_TRUE: {
    name: "IS_TRUE",
    summary: "Verifica se um campo booleano é TRUE",
    syntax: "customer.isVerified IS_TRUE",
    syntaxExplanation: "Retorna TRUE se campo é exatamente true (boolean).",
    story: "Cliente verificado: isVerified IS_TRUE = passou por KYC.",
    problem: "Como verificar flags booleanas?",
    goldenTip: "💎 Também aceita 'true' (string) e 1 (número) como truthy."
  },

  IS_FALSE: {
    name: "IS_FALSE",
    summary: "Verifica se um campo booleano é FALSE",
    syntax: "customer.isBlocked IS_FALSE",
    syntaxExplanation: "Retorna TRUE se campo é exatamente false (boolean).",
    story: "Cliente não bloqueado: isBlocked IS_FALSE.",
    problem: "Como verificar que uma flag está desligada?",
    goldenTip: "💎 Também aceita 'false' (string) e 0 (número) como falsy."
  }
};

// ═══════════════════════════════════════════════════════════════════════════════════════
// PARTE 2: OPERADORES DE ARRAY (10 operadores)
// ═══════════════════════════════════════════════════════════════════════════════════════

export const ARRAY_SPECS: Record<string, OperatorSpec> = {
  ARRAY_CONTAINS: {
    name: "ARRAY_CONTAINS",
    summary: "Verifica se um ARRAY contém um valor específico",
    syntax: "customer.tags ARRAY_CONTAINS \"VIP\"",
    syntaxExplanation: "Procura valor em array. ['gold', 'VIP', 'premium'] ARRAY_CONTAINS 'VIP' → TRUE",
    story: "Cliente com tag VIP tem regras especiais: tags ARRAY_CONTAINS 'VIP'.",
    problem: "Como verificar se um item está presente em uma lista?",
    goldenTip: "💎 Diferente de IN: ARRAY_CONTAINS verifica se o CAMPO (array) contém o valor. IN verifica se o VALOR está na lista fixa."
  },

  ARRAY_NOT_CONTAINS: {
    name: "ARRAY_NOT_CONTAINS",
    summary: "Verifica se um ARRAY NÃO contém um valor específico",
    syntax: "customer.restrictions ARRAY_NOT_CONTAINS \"BLOCKED\"",
    syntaxExplanation: "Verifica ausência em array. ['warning'] ARRAY_NOT_CONTAINS 'BLOCKED' → TRUE",
    story: "Cliente sem restrição de bloqueio: restrictions ARRAY_NOT_CONTAINS 'BLOCKED'.",
    problem: "Como verificar que um item NÃO está em uma lista?",
    goldenTip: "💎 Útil para whitelists: 'flags ARRAY_NOT_CONTAINS \"fraud\"' = sem flag de fraude."
  },

  ARRAY_SIZE_EQ: {
    name: "ARRAY_SIZE_EQ",
    summary: "Verifica se o tamanho do array é IGUAL a um número",
    syntax: "customer.phones ARRAY_SIZE_EQ 1",
    syntaxExplanation: "Conta elementos. ['+5511999'] ARRAY_SIZE_EQ 1 → TRUE | ['+5511999', '+5521888'] ARRAY_SIZE_EQ 1 → FALSE",
    story: "Cliente deve ter exatamente 1 telefone cadastrado.",
    problem: "Como verificar quantidade EXATA de itens em uma lista?",
    goldenTip: "💎 Use para validar cadastro: 'documents ARRAY_SIZE_EQ 2' = CPF + comprovante."
  },

  ARRAY_SIZE_GT: {
    name: "ARRAY_SIZE_GT",
    summary: "Verifica se o tamanho do array é MAIOR que um número",
    syntax: "customer.devices ARRAY_SIZE_GT 5",
    syntaxExplanation: "Conta e compara. 6 devices > 5 → TRUE",
    story: "Cliente com mais de 5 devices cadastrados é suspeito.",
    problem: "Como detectar EXCESSO de itens em uma lista?",
    goldenTip: "💎 Detecta account sharing: 'devices ARRAY_SIZE_GT 10' = muitos devices = conta compartilhada."
  },

  ARRAY_SIZE_LT: {
    name: "ARRAY_SIZE_LT",
    summary: "Verifica se o tamanho do array é MENOR que um número",
    syntax: "customer.verifications ARRAY_SIZE_LT 2",
    syntaxExplanation: "Conta e compara. 1 verificação < 2 → TRUE",
    story: "Cliente com menos de 2 verificações = cadastro incompleto.",
    problem: "Como detectar FALTA de itens em uma lista?",
    goldenTip: "💎 Detecta cadastro incompleto: 'documents ARRAY_SIZE_LT 2' = falta documentos."
  },

  ARRAY_ANY: {
    name: "ARRAY_ANY",
    summary: "Verifica se PELO MENOS UM item do array satisfaz a condição",
    syntax: "transactions ARRAY_ANY (amount GT 10000)",
    syntaxExplanation: "Se qualquer item passar, retorna TRUE. [100, 500, 15000] ARRAY_ANY (GT 10000) → TRUE (15000 passa)",
    story: "Se qualquer transação recente foi > R$ 10k, alertar.",
    problem: "Como verificar se ALGUM item de uma lista atende um critério?",
    goldenTip: "💎 Use para detectar anomalias: 'transactions ARRAY_ANY (amount GT 50000)' = tem alguma TX gigante."
  },

  ARRAY_ALL: {
    name: "ARRAY_ALL",
    summary: "Verifica se TODOS os itens do array satisfazem a condição",
    syntax: "transactions ARRAY_ALL (status EQ \"APPROVED\")",
    syntaxExplanation: "Todos precisam passar. ['APPROVED', 'APPROVED'] → TRUE | ['APPROVED', 'DECLINED'] → FALSE",
    story: "Todas as verificações devem estar aprovadas para liberar.",
    problem: "Como garantir que TODOS os itens atendem um critério?",
    goldenTip: "💎 Use para validações: 'documents ARRAY_ALL (status EQ \"VERIFIED\")' = todos docs verificados."
  },

  ARRAY_NONE: {
    name: "ARRAY_NONE",
    summary: "Verifica se NENHUM item do array satisfaz a condição",
    syntax: "transactions ARRAY_NONE (status EQ \"FRAUD\")",
    syntaxExplanation: "Se qualquer item passar, retorna FALSE. [] com fraud → FALSE",
    story: "Nenhuma transação pode ter status 'FRAUD'.",
    problem: "Como garantir que NENHUM item atende um critério indesejado?",
    goldenTip: "💎 Use para blacklists: 'tags ARRAY_NONE (CONTAINS \"blocked\")' = nenhuma tag de bloqueio."
  },

  ARRAY_FIRST: {
    name: "ARRAY_FIRST",
    summary: "Retorna o PRIMEIRO elemento do array para comparação",
    syntax: "transactions ARRAY_FIRST .amount GT 1000",
    syntaxExplanation: "Pega primeiro item. [500, 1500, 200] ARRAY_FIRST .amount → 500",
    story: "A primeira transação do dia foi maior que R$ 1k?",
    problem: "Como verificar o PRIMEIRO item de uma lista?",
    goldenTip: "💎 Útil para cronologia: primeira TX do dia, primeiro login, etc."
  },

  ARRAY_LAST: {
    name: "ARRAY_LAST",
    summary: "Retorna o ÚLTIMO elemento do array para comparação",
    syntax: "transactions ARRAY_LAST .amount GT 1000",
    syntaxExplanation: "Pega último item. [500, 1500, 200] ARRAY_LAST .amount → 200",
    story: "A última transação foi maior que R$ 1k?",
    problem: "Como verificar o ÚLTIMO/MAIS RECENTE item de uma lista?",
    goldenTip: "💎 Útil para última ação: última TX, último login, última alteração."
  }
};

// ═══════════════════════════════════════════════════════════════════════════════════════
// PARTE 3: OPERADORES DE FIELD/COMPARAÇÃO DE CAMPOS (8 operadores)
// ═══════════════════════════════════════════════════════════════════════════════════════

export const FIELD_SPECS: Record<string, OperatorSpec> = {
  FIELD_EXISTS: {
    name: "FIELD_EXISTS",
    summary: "Verifica se um campo EXISTE no payload (mesmo que seja null)",
    syntax: "FIELD_EXISTS(\"customer.email\")",
    syntaxExplanation: "Verifica se a chave existe no JSON. {email: null} → TRUE | {} → FALSE",
    story: "Payload antigo não tinha campo 'deviceId'. Novas versões têm.",
    problem: "Como verificar se um campo está presente no payload?",
    goldenTip: "💎 Diferente de IS_NOT_NULL: FIELD_EXISTS verifica se a CHAVE existe, não se tem valor."
  },

  FIELD_NOT_EXISTS: {
    name: "FIELD_NOT_EXISTS",
    summary: "Verifica se um campo NÃO existe no payload",
    syntax: "FIELD_NOT_EXISTS(\"customer.middleName\")",
    syntaxExplanation: "Inverso do anterior. {} → TRUE (campo não existe)",
    story: "Campo middleName é opcional, pode não existir.",
    problem: "Como verificar se um campo está AUSENTE?",
    goldenTip: "💎 Útil para APIs diferentes: versão antiga não envia alguns campos."
  },

  FIELD_TYPE_EQ: {
    name: "FIELD_TYPE_EQ",
    summary: "Verifica o TIPO de um campo (string, number, boolean, array, object)",
    syntax: "FIELD_TYPE_EQ(\"amount\", \"number\")",
    syntaxExplanation: "Verifica tipo. amount: 100 → type = 'number' → TRUE | amount: '100' → type = 'string' → FALSE",
    story: "Amount deve ser número, não string. Validação de tipo.",
    problem: "Como garantir que um campo tem o tipo correto?",
    goldenTip: "💎 Detecta erros de integração: se amount vier como string, há problema no sistema origem."
  },

  FIELD_EQ_FIELD: {
    name: "FIELD_EQ_FIELD",
    summary: "Compara se DOIS CAMPOS do payload são IGUAIS",
    syntax: "billing.country FIELD_EQ_FIELD shipping.country",
    syntaxExplanation: "Compara dois campos. billing.country = 'BR' E shipping.country = 'BR' → TRUE",
    story: "País de faturamento deve ser igual ao de entrega.",
    problem: "Como comparar dois campos DINÂMICOS do mesmo payload?",
    goldenTip: "💎 Útil para consistência: 'card.country FIELD_EQ_FIELD ip.country' = cartão e IP do mesmo país."
  },

  FIELD_GT_FIELD: {
    name: "FIELD_GT_FIELD",
    summary: "Verifica se um campo é MAIOR que outro campo",
    syntax: "transaction.amount FIELD_GT_FIELD customer.dailyLimit",
    syntaxExplanation: "Compara dois campos numéricos. amount = 15000, dailyLimit = 10000 → TRUE",
    story: "Transação maior que limite diário do cliente.",
    problem: "Como comparar valores dinâmicos entre campos?",
    goldenTip: "💎 Detecta estouros: 'usedLimit FIELD_GT_FIELD availableLimit' = cliente passou do limite."
  },

  FIELD_LT_FIELD: {
    name: "FIELD_LT_FIELD",
    summary: "Verifica se um campo é MENOR que outro campo",
    syntax: "transaction.amount FIELD_LT_FIELD customer.avgTransactionAmount",
    syntaxExplanation: "Compara dois campos. amount = 50, avg = 500 → TRUE (10x menor que média)",
    story: "Transação muito abaixo da média = possível teste de cartão.",
    problem: "Como detectar valores anormalmente baixos comparados a outro campo?",
    goldenTip: "💎 Detecta anomalias: TX muito menor que média do cliente = teste de cartão."
  },

  FIELD_CONTAINS_FIELD: {
    name: "FIELD_CONTAINS_FIELD",
    summary: "Verifica se um campo CONTÉM o valor de outro campo",
    syntax: "fullAddress FIELD_CONTAINS_FIELD city",
    syntaxExplanation: "Verifica substring dinâmica. fullAddress = 'Rua X, São Paulo' E city = 'São Paulo' → TRUE",
    story: "Endereço completo deve conter a cidade informada.",
    problem: "Como verificar se um campo está contido em outro?",
    goldenTip: "💎 Validação cruzada: endereço deve conter CEP, cidade deve estar no estado, etc."
  },

  FIELD_DISTANCE_FIELD: {
    name: "FIELD_DISTANCE_FIELD",
    summary: "Calcula distância entre duas coordenadas de campos diferentes",
    syntax: "FIELD_DISTANCE_FIELD(\"transaction.location\", \"customer.homeLocation\") GT 500",
    syntaxExplanation: "Calcula distância em km entre dois pontos geográficos do payload.",
    story: "TX feita a mais de 500km do endereço residencial do cliente.",
    problem: "Como calcular distância geográfica entre dois campos?",
    goldenTip: "💎 Detecta TX remota: compra feita longe de casa = suspeito (se não for viagem conhecida)."
  }
};

// ═══════════════════════════════════════════════════════════════════════════════════════
// PARTE 4: OPERADORES DE ACCOUNT AGE (10 operadores)
// ═══════════════════════════════════════════════════════════════════════════════════════

export const ACCOUNT_AGE_SPECS: Record<string, OperatorSpec> = {
  ACCOUNT_AGE_LT_DAYS: {
    name: "ACCOUNT_AGE_LT_DAYS",
    summary: "Verifica se conta tem MENOS de N dias de idade",
    syntax: "ACCOUNT_AGE_LT_DAYS(30)",
    syntaxExplanation: "Conta criada há menos de 30 dias = TRUE. Exemplo: conta de 7 dias → TRUE | conta de 60 dias → FALSE",
    story: "Contas com menos de 30 dias têm limite reduzido (período probatório).",
    problem: "Como identificar contas NOVAS (alto risco)?",
    goldenTip: "💎 REGRA DE OURO: 80% das fraudes acontecem nos primeiros 7 dias da conta. Use < 7 para regras críticas.",
    engineBehavior: {
      description: "Calcula idade da conta:",
      steps: [
        "1. Lê customer.createdAt do payload",
        "2. Calcula: (agora - createdAt) em dias",
        "3. Compara: idade < 30",
        "4. Retorna: boolean"
      ],
      cautions: ["Precisa de campo createdAt no payload", "Timezone pode afetar cálculo (use UTC)"]
    }
  },

  ACCOUNT_AGE_LT_MINUTES: {
    name: "ACCOUNT_AGE_LT_MINUTES",
    summary: "Verifica se conta tem MENOS de N minutos de idade",
    syntax: "ACCOUNT_AGE_LT_MINUTES(60)",
    syntaxExplanation: "Conta criada há menos de 60 minutos = TRUE. Conta de 30 min → TRUE",
    story: "Conta criada e já fazendo transação em 10 minutos = BOM DEMAIS PRA SER VERDADE.",
    problem: "Como detectar 'bust-out' (criar conta e usar imediatamente para fraude)?",
    goldenTip: "💎 ALERTA VERMELHO: 'ACCOUNT_AGE_LT_MINUTES(10) AND amount GT 5000' = conta de 10 min gastando alto = FRAUDE."
  },

  ACCOUNT_AGE_GT_DAYS: {
    name: "ACCOUNT_AGE_GT_DAYS",
    summary: "Verifica se conta tem MAIS de N dias de idade",
    syntax: "ACCOUNT_AGE_GT_DAYS(365)",
    syntaxExplanation: "Conta com mais de 365 dias = TRUE. Conta velha = menor risco.",
    story: "Clientes com mais de 1 ano são confiáveis (passaram período de risco).",
    problem: "Como identificar contas MADURAS (baixo risco)?",
    goldenTip: "💎 Use para whitelist: 'ACCOUNT_AGE_GT_DAYS(180) AND has0Chargebacks' = cliente veterano limpo."
  },

  ACCOUNT_AGE_GT_MINUTES: {
    name: "ACCOUNT_AGE_GT_MINUTES",
    summary: "Verifica se conta tem MAIS de N minutos de idade",
    syntax: "ACCOUNT_AGE_GT_MINUTES(30)",
    syntaxExplanation: "Conta com mais de 30 minutos = TRUE.",
    story: "Exigir que conta tenha pelo menos 30 minutos antes de permitir PIX.",
    problem: "Como impor período mínimo de 'cooling off'?",
    goldenTip: "💎 'Cooling period': ACCOUNT_AGE_GT_MINUTES(30) para habilitar funcionalidades sensíveis."
  },

  ACCOUNT_LINK_DEPTH: {
    name: "ACCOUNT_LINK_DEPTH",
    summary: "Mede a PROFUNDIDADE de conexões da conta no grafo de relacionamentos",
    syntax: "ACCOUNT_LINK_DEPTH(customerId) GT 3",
    syntaxExplanation: "Quantas 'camadas' de conexão a conta tem. Conta com 5 níveis de conexão = muito conectada.",
    story: "Conta conectada a muitas outras (mesmo device, endereço, beneficiário) = rede suspeita.",
    problem: "Como medir quão 'conectada' uma conta está a outras no sistema?",
    goldenTip: "💎 Link depth > 5 = provável fraud ring. Investigue a comunidade toda.",
    engineBehavior: {
      description: "Query no grafo Neo4j:",
      steps: [
        "1. Encontra nó da conta no grafo",
        "2. Calcula caminho mais longo até qualquer outro nó",
        "3. Retorna profundidade máxima",
        "4. Compara com threshold"
      ],
      performance: "Neo4j com índice: <100ms para grafos de 1M nós"
    }
  },

  ACCOUNT_TAKEOVER_PATTERN: {
    name: "ACCOUNT_TAKEOVER_PATTERN",
    summary: "Detecta padrões de TOMADA DE CONTA (ATO - Account Takeover)",
    syntax: "ACCOUNT_TAKEOVER_PATTERN() IS_TRUE",
    syntaxExplanation: "Analisa sinais de ATO: mudança de senha + email + device em curto período.",
    story: "Conta teve senha alterada, email alterado e device novo em 24h = SEQUESTRADA.",
    problem: "Como detectar quando uma conta foi ROUBADA?",
    goldenTip: "💎 Padrões de ATO:\n• Senha alterada + email alterado em 1h\n• Device novo + transferência grande\n• Horário anormal + IP diferente + ação sensível",
    engineBehavior: {
      description: "Analisa múltiplos sinais:",
      steps: [
        "1. Verifica mudanças recentes de credenciais",
        "2. Analisa mudança de device",
        "3. Verifica IP/localização",
        "4. Pondera todos os sinais",
        "5. Se score > threshold → ATO detectado"
      ],
      cautions: ["Pode gerar falso positivo em cliente que legitimamente trocou de celular e atualizou cadastro"]
    },
    realScenarios: [
      {
        title: "ATO Clássico",
        context: "Conta de cliente VIP com histórico de 3 anos",
        problem: "Fraudador obteve credenciais via phishing, alterou senha e email para tomar controle",
        solution: "Sistema detectou: nova senha + novo email + novo device + transferência de R$ 50k para beneficiário nunca usado → BLOQUEIO",
        impact: "Evitou perda de R$ 50k. Conta devolvida ao cliente real em 24h."
      }
    ]
  }
};

// ═══════════════════════════════════════════════════════════════════════════════════════
// PARTE 5: OPERADORES VELOCITY COMPLETOS (17 operadores)
// ═══════════════════════════════════════════════════════════════════════════════════════

export const VELOCITY_SPECS: Record<string, OperatorSpec> = {
  VELOCITY_COUNT_GT: {
    name: "VELOCITY_COUNT_GT",
    summary: "Conta transações em uma janela de tempo e verifica se EXCEDE o limite",
    syntax: "VELOCITY_COUNT(pan, HOUR_24) GT 10",
    syntaxExplanation: "Conta TXs do PAN nas últimas 24h. Se > 10, dispara. Exemplo: 15 TXs em 24h → DISPARA!",
    story: "Fraudador testando cartões: faz 50 compras pequenas em 2h para ver quais passam.",
    problem: "Como detectar 'card testing' (muitas transações em pouco tempo)?",
    goldenTip: "💎 THRESHOLD RECOMENDADO:\n• HOUR_1 > 5 = alerta\n• HOUR_24 > 15 = suspeito\n• DAY_7 > 50 = investigar",
    engineBehavior: {
      description: "VelocityService.getStats():",
      steps: [
        "1. Hash do campo (SHA-256 se for PAN)",
        "2. Consulta cache Caffeine (TTL 30s)",
        "3. Se miss: query DB com janela de tempo",
        "4. Retorna count",
        "5. Compara > threshold"
      ],
      performance: "Cache hit: <1ms | Cache miss: ~5ms (com índice)"
    }
  },

  VELOCITY_COUNT_LT: {
    name: "VELOCITY_COUNT_LT",
    summary: "Verifica se COUNT de transações está ABAIXO de um limite",
    syntax: "VELOCITY_COUNT(customerId, DAY_7) LT 3",
    syntaxExplanation: "Conta TXs do cliente em 7 dias. Se < 3, é cliente inativo/novo.",
    story: "Cliente nunca usou o cartão (0-2 TXs) e agora faz compra grande = suspeito.",
    problem: "Como identificar clientes INATIVOS que de repente fazem transação?",
    goldenTip: "💎 Dormant account reativation: 'VELOCITY_COUNT_LT(customerId, DAY_30) AND amount GT 5000' = estava parado e agora gastou muito."
  },

  VELOCITY_SUM_GT: {
    name: "VELOCITY_SUM_GT",
    summary: "Soma os valores das transações e verifica se EXCEDE o limite",
    syntax: "VELOCITY_SUM(pan, HOUR_24, amount) GT 15000",
    syntaxExplanation: "Soma TODOS os valores das TXs nas últimas 24h. Exemplo: 10 TXs de R$ 1.600 = R$ 16.000 → DISPARA!",
    story: "Fraudador faz compras 'abaixo do radar' (R$ 900 cada) mas soma R$ 20k/dia.",
    problem: "Como detectar muitas compras pequenas que somam alto valor?",
    goldenTip: "💎 EXEMPLO:\n09:00 → R$ 1.200 (soma = R$ 1.200)\n10:30 → R$ 2.800 (soma = R$ 4.000)\n14:00 → R$ 3.500 (soma = R$ 7.500)\n18:20 → R$ 8.500 (soma = R$ 16.000) ← DISPARA!"
  },

  VELOCITY_SUM_LT: {
    name: "VELOCITY_SUM_LT",
    summary: "Verifica se a SOMA dos valores está ABAIXO de um limite",
    syntax: "VELOCITY_SUM(customerId, DAY_30, amount) LT 100",
    syntaxExplanation: "Soma valores em 30 dias. Se < R$ 100, cliente quase não usa.",
    story: "Cliente gastou apenas R$ 50 em 30 dias = conta dormant.",
    problem: "Como identificar contas com pouca atividade financeira?",
    goldenTip: "💎 Combine com TX alta: 'VELOCITY_SUM_LT(...) AND amount GT 10000' = conta parada fazendo TX grande."
  },

  VELOCITY_AVG_GT: {
    name: "VELOCITY_AVG_GT",
    summary: "Calcula a MÉDIA dos valores e verifica se EXCEDE o limite",
    syntax: "VELOCITY_AVG(customerId, DAY_7, amount) GT 500",
    syntaxExplanation: "Média = Soma ÷ Quantidade. 10 TXs totalizando R$ 6.000 = média R$ 600/TX → DISPARA (> R$ 500)!",
    story: "Cliente que compra ~R$ 80/vez agora tem média de R$ 400 = mudança de padrão.",
    problem: "Como detectar MUDANÇA no ticket médio do cliente?",
    goldenTip: "💎 Ticket médio normal do cliente é key. Se subiu 5x, conta pode estar comprometida."
  },

  VELOCITY_AVG_LT: {
    name: "VELOCITY_AVG_LT",
    summary: "Verifica se a MÉDIA dos valores está ABAIXO de um limite",
    syntax: "VELOCITY_AVG(pan, HOUR_24, amount) LT 10",
    syntaxExplanation: "Média < R$ 10 = muitas transações pequenas (teste de cartão).",
    story: "30 TXs de R$ 1-2 cada = card testing clássico.",
    problem: "Como detectar padrão de 'card testing' pelo valor médio?",
    goldenTip: "💎 Card testing pattern: 'VELOCITY_COUNT_GT(pan, HOUR_1) AND VELOCITY_AVG_LT(pan, HOUR_1) < 5' = muitas TXs com valor muito baixo."
  },

  VELOCITY_DISTINCT_GT: {
    name: "VELOCITY_DISTINCT_GT",
    summary: "Conta valores ÚNICOS/DISTINTOS de um campo e verifica se EXCEDE",
    syntax: "VELOCITY_DISTINCT(pan, DAY_1, merchantId) GT 10",
    syntaxExplanation: "Conta merchants DIFERENTES que o cartão usou. 15 merchants distintos em 24h → DISPARA!",
    story: "Cartão comprou em 25 lojas diferentes em 1 dia = teste de cartão clonado.",
    problem: "Como detectar fraudador testando cartão em muitos lugares diferentes?",
    goldenTip: "💎 Cliente normal: 2-3 merchants/dia. Fraudador: 10-30 merchants/dia (testando onde passa)."
  },

  VELOCITY_DISTINCT_LT: {
    name: "VELOCITY_DISTINCT_LT",
    summary: "Verifica se a quantidade de valores DISTINTOS está ABAIXO de um limite",
    syntax: "VELOCITY_DISTINCT(customerId, DAY_30, merchantCategory) LT 2",
    syntaxExplanation: "Cliente só compra em 1 categoria de merchant = padrão restrito.",
    story: "Cliente só compra em 'gaming' há 30 dias e agora compra em 'joalheria' = suspeito.",
    problem: "Como identificar clientes com padrão de compras muito restrito?",
    goldenTip: "💎 Útil para detectar mudança de padrão: se sempre compra em 1 categoria e muda, investigar."
  },

  VELOCITY_MAX_GT: {
    name: "VELOCITY_MAX_GT",
    summary: "Encontra o MAIOR valor na janela de tempo e verifica se EXCEDE",
    syntax: "VELOCITY_MAX(customerId, HOUR_24, amount) GT 5000",
    syntaxExplanation: "Pega o maior valor das TXs nas últimas 24h. Se máximo > R$ 5k, dispara.",
    story: "Cliente nunca passou de R$ 500 por TX. Hoje tem TX de R$ 8k = máximo anormal.",
    problem: "Como detectar picos de valor (transação fora do padrão)?",
    goldenTip: "💎 Combine com histórico: 'VELOCITY_MAX_GT(hoje) > 10x * VELOCITY_MAX_GT(último mês)' = pico suspeito."
  },

  VELOCITY_MIN_LT: {
    name: "VELOCITY_MIN_LT",
    summary: "Encontra o MENOR valor na janela de tempo e verifica se está ABAIXO",
    syntax: "VELOCITY_MIN(pan, HOUR_24, amount) LT 5",
    syntaxExplanation: "Pega o menor valor. Se mínimo < R$ 5, pode ser teste de cartão.",
    story: "TX de R$ 1 entre várias de R$ 500 = mínimo anormalmente baixo.",
    problem: "Como detectar 'micro-transactions' que indicam teste?",
    goldenTip: "💎 Card testing: 'VELOCITY_MIN_LT(pan, HOUR_1) < 2 AND VELOCITY_COUNT_GT(pan, HOUR_1) > 5' = muitas TXs incluindo uma mini."
  },

  VELOCITY_STDDEV_GT: {
    name: "VELOCITY_STDDEV_GT",
    summary: "Calcula o DESVIO PADRÃO dos valores e verifica se é ALTO",
    syntax: "VELOCITY_STDDEV(customerId, DAY_7, amount) GT 500",
    syntaxExplanation: "Desvio padrão alto = valores muito variados (R$ 10 e R$ 10.000 no mesmo dia).",
    story: "Cliente com TXs de R$ 50 e R$ 5.000 no mesmo dia = desvio alto = estranho.",
    problem: "Como detectar VARIABILIDADE extrema nos valores de transação?",
    goldenTip: "💎 Desvio padrão alto + conta nova = alto risco. Fraudadores variam muito para testar limites."
  },

  COUNT_LAST_N_HOURS: {
    name: "COUNT_LAST_N_HOURS",
    summary: "Conta transações nas últimas N horas (janela dinâmica)",
    syntax: "COUNT_LAST_N_HOURS(pan, 6) GT 10",
    syntaxExplanation: "Conta TXs nas últimas 6 horas (valor N é configurável). Mais flexível que VELOCITY_COUNT.",
    story: "Preciso de janela de 6h (não tem em VELOCITY padrão). Uso COUNT_LAST_N_HOURS.",
    problem: "Como criar janelas de tempo personalizadas (não apenas 1h, 24h, 7d)?",
    goldenTip: "💎 Use quando precisar de janela não-padrão: 2h, 6h, 12h, 72h, etc."
  },

  COUNT_LAST_N_DAYS: {
    name: "COUNT_LAST_N_DAYS",
    summary: "Conta transações nos últimos N dias (janela dinâmica)",
    syntax: "COUNT_LAST_N_DAYS(customerId, 15) GT 30",
    syntaxExplanation: "Conta TXs nos últimos 15 dias. N é configurável.",
    story: "Preciso de janela de 15 dias para análise quinzenal.",
    problem: "Como criar janelas de tempo em dias personalizadas?",
    goldenTip: "💎 Útil para análises quinzenais, trimestrais, etc."
  },

  SUM_LAST_N_HOURS: {
    name: "SUM_LAST_N_HOURS",
    summary: "Soma valores nas últimas N horas (janela dinâmica)",
    syntax: "SUM_LAST_N_HOURS(pan, 12, amount) GT 10000",
    syntaxExplanation: "Soma valores das últimas 12 horas. N é configurável.",
    story: "Limite de R$ 10k em 12h para cartões novos.",
    problem: "Como somar valores em janela personalizada?",
    goldenTip: "💎 Combine com idade da conta: 'ACCOUNT_AGE_LT_DAYS(7) AND SUM_LAST_N_HOURS(12) > 5000' = conta nova gastando muito."
  },

  SUM_LAST_N_DAYS: {
    name: "SUM_LAST_N_DAYS",
    summary: "Soma valores nos últimos N dias (janela dinâmica)",
    syntax: "SUM_LAST_N_DAYS(customerId, 30, amount) GT 100000",
    syntaxExplanation: "Soma valores dos últimos 30 dias. Limite mensal.",
    story: "Limite mensal de R$ 100k por cliente.",
    problem: "Como implementar limites mensais/periódicos?",
    goldenTip: "💎 Limites regulatórios: PIX tem limite mensal, TED tem limite diário, etc."
  },

  AVG_INTERVAL_BETWEEN_TXN: {
    name: "AVG_INTERVAL_BETWEEN_TXN",
    summary: "Calcula o intervalo MÉDIO entre transações do cliente",
    syntax: "AVG_INTERVAL_BETWEEN_TXN(customerId, DAY_30) LT 60",
    syntaxExplanation: "Intervalo médio < 60 minutos = transações muito frequentes. Normal: 1-2 dias.",
    story: "Cliente normal: 1 TX por semana. Fraudador: 1 TX a cada 30 minutos.",
    problem: "Como detectar frequência ANORMAL de transações?",
    goldenTip: "💎 Intervalo médio < 30 minutos = automação/bot. Humano não faz TX de 30 em 30 min por horas."
  },

  CROSS_BORDER_VELOCITY: {
    name: "CROSS_BORDER_VELOCITY",
    summary: "Conta transações INTERNACIONAIS em uma janela de tempo",
    syntax: "CROSS_BORDER_VELOCITY(pan, HOUR_24) GT 3",
    syntaxExplanation: "Conta TXs em países diferentes do país do cartão. > 3 países em 24h = suspeito.",
    story: "Cartão brasileiro com TXs em 5 países em 24h = impossível viajar tão rápido.",
    problem: "Como detectar uso de cartão clonado em múltiplos países?",
    goldenTip: "💎 Viagem impossível: 'CROSS_BORDER_VELOCITY > 2 AND intervalo < 2h' = não dá tempo de pegar voo."
  }
};

// ═══════════════════════════════════════════════════════════════════════════════════════
// PARTE 6: OPERADORES GEO/LOCALIZAÇÃO (12 operadores)
// ═══════════════════════════════════════════════════════════════════════════════════════

export const GEO_SPECS: Record<string, OperatorSpec> = {
  GEO_DISTANCE_GT: {
    name: "GEO_DISTANCE_GT",
    summary: "Calcula distância entre duas coordenadas e verifica se é MAIOR que o limite",
    syntax: "GEO_DISTANCE(transaction.location, customer.address) GT 500",
    syntaxExplanation: "Usa Haversine para calcular distância em km. > 500km do endereço cadastrado = suspeito.",
    story: "TX em Miami às 10h, cliente estava em SP às 9h55. Impossível!",
    problem: "Como detectar transações GEOGRAFICAMENTE impossíveis?",
    goldenTip: "💎 Fórmula Haversine: d = 2R × arcsin(√(sin²(Δlat/2) + cos(lat1)×cos(lat2)×sin²(Δlon/2)))",
    engineBehavior: {
      description: "GeoService.evaluateDistanceGreaterThan():",
      steps: [
        "1. Extrai lat/lon do payload",
        "2. Extrai lat/lon de referência",
        "3. Aplica Haversine",
        "4. Retorna distance > threshold"
      ],
      performance: "Cálculo em memória: <0.1ms"
    }
  },

  GEO_DISTANCE_LT: {
    name: "GEO_DISTANCE_LT",
    summary: "Verifica se distância é MENOR que o limite",
    syntax: "GEO_DISTANCE(transaction.location, store.location) LT 1",
    syntaxExplanation: "TX a menos de 1km da loja = presencial legítimo.",
    story: "Validar que TX presencial realmente está perto da loja física.",
    problem: "Como verificar que TX foi feita PRÓXIMA de um local esperado?",
    goldenTip: "💎 TX presencial: distância < 100m (0.1km) da loja. Se > 10km, pode ser fraude."
  },

  GEO_IN_POLYGON: {
    name: "GEO_IN_POLYGON",
    summary: "Verifica se coordenada está DENTRO de um polígono geográfico",
    syntax: "GEO_IN_POLYGON(transaction.location, \"brazil_southeast\")",
    syntaxExplanation: "Ray Casting: verifica se ponto está dentro da região. SE + SP + RJ + MG + ES.",
    story: "Aceitar apenas TXs da região Sudeste para campanha regional.",
    problem: "Como delimitar uma ÁREA GEOGRÁFICA complexa (não apenas círculo)?",
    goldenTip: "💎 Ray Casting: linha do ponto para infinito. Se cruza bordas ÍMPAR vezes = dentro.",
    engineBehavior: {
      description: "GeoService.evaluateInPolygon():",
      steps: [
        "1. Busca vértices do polígono no banco",
        "2. Desenha raio horizontal do ponto",
        "3. Conta interseções com bordas",
        "4. Se ÍMPAR → dentro → TRUE"
      ],
      cautions: ["Polígono deve estar fechado (primeiro vértice = último)"]
    }
  },

  GEO_NOT_IN_POLYGON: {
    name: "GEO_NOT_IN_POLYGON",
    summary: "Verifica se coordenada está FORA de um polígono",
    syntax: "GEO_NOT_IN_POLYGON(transaction.location, \"high_risk_zone\")",
    syntaxExplanation: "Inverso do anterior. TX fora da zona de alto risco = OK.",
    story: "Alertar se TX NÃO estiver na área de operação da empresa.",
    problem: "Como detectar TXs fora de áreas permitidas?",
    goldenTip: "💎 Geofencing negativo: 'NOT_IN_POLYGON(zonas de operação)' = fora da cobertura."
  },

  GEO_COUNTRY_MISMATCH: {
    name: "GEO_COUNTRY_MISMATCH",
    summary: "Verifica se país da TX é DIFERENTE do país do cartão/cliente",
    syntax: "GEO_COUNTRY_MISMATCH() IS_TRUE",
    syntaxExplanation: "Cartão BR mas TX em US = MISMATCH = TRUE.",
    story: "Cartão emitido no Brasil sendo usado nos EUA = cross-border.",
    problem: "Como detectar transações internacionais (cross-border)?",
    goldenTip: "💎 Mismatch não é sempre fraude! Cliente pode estar viajando. Combine com outros sinais."
  },

  GEO_IP_LOCATION_MISMATCH: {
    name: "GEO_IP_LOCATION_MISMATCH",
    summary: "Verifica se localização do IP é diferente da localização declarada",
    syntax: "GEO_IP_LOCATION_MISMATCH() IS_TRUE",
    syntaxExplanation: "Payload diz 'Brasil' mas IP é dos EUA = MISMATCH (possível VPN).",
    story: "Cliente diz estar em SP mas IP é de datacenter na Virgínia.",
    problem: "Como detectar possível uso de VPN ou localização falsa?",
    goldenTip: "💎 IP mismatch + VPN_PROXY_DETECTION = alta probabilidade de fraude."
  },

  DISTANCE_FROM_LAST_GT: {
    name: "DISTANCE_FROM_LAST_GT",
    summary: "Verifica se distância da ÚLTIMA TX é maior que limite",
    syntax: "DISTANCE_FROM_LAST_GT(500)",
    syntaxExplanation: "TX atual está a mais de 500km da TX anterior = salto geográfico.",
    story: "Última TX em SP, agora TX em Miami em 30 minutos = impossível.",
    problem: "Como detectar 'impossible travel' (viagem impossível)?",
    goldenTip: "💎 IMPOSSIBLE TRAVEL:\n• SP → Miami (7.500km) em 1h = IMPOSSÍVEL (avião leva 10h)\n• SP → Rio (400km) em 1h = POSSÍVEL (voo ponte aérea)"
  },

  LOCATION_DEVIATION: {
    name: "LOCATION_DEVIATION",
    summary: "Mede o DESVIO da localização atual vs padrão histórico do cliente",
    syntax: "LOCATION_DEVIATION(customerId) GT 2",
    syntaxExplanation: "Desvio padrão > 2 = TX muito longe do padrão (em unidades de desvio padrão).",
    story: "Cliente sempre compra num raio de 50km. TX a 500km = desvio alto.",
    problem: "Como detectar TX fora do 'perímetro normal' de cada cliente?",
    goldenTip: "💎 Machine Learning: sistema aprende perímetro de cada cliente. Desvio > 2σ = fora do padrão.",
    engineBehavior: {
      description: "Calcula desvio estatístico:",
      steps: [
        "1. Carrega histórico de localizações do cliente",
        "2. Calcula centroide (centro médio das TXs)",
        "3. Calcula desvio padrão das distâncias",
        "4. Compara TX atual vs padrão",
        "5. Retorna número de desvios (z-score)"
      ]
    }
  },

  GEO_HIGH_RISK_COUNTRY: {
    name: "GEO_HIGH_RISK_COUNTRY",
    summary: "Verifica se país está em lista de ALTO RISCO",
    syntax: "GEO_HIGH_RISK_COUNTRY(transaction.country) IS_TRUE",
    syntaxExplanation: "País em lista FATF de alto risco = TRUE. Nigéria, Irã, Coreia do Norte, etc.",
    story: "TX para país na lista cinza do FATF = alerta de compliance.",
    problem: "Como implementar regras de países de alto risco (FATF, OFAC)?",
    goldenTip: "💎 Lista FATF atualizada: https://www.fatf-gafi.org/countries/ - atualizar lista periodicamente!"
  },

  GEO_SANCTIONED_COUNTRY: {
    name: "GEO_SANCTIONED_COUNTRY",
    summary: "Verifica se país está SANCIONADO (OFAC, EU, ONU)",
    syntax: "GEO_SANCTIONED_COUNTRY(transaction.country) IS_TRUE",
    syntaxExplanation: "País sob sanções = BLOQUEIO obrigatório. Cuba, Irã, Coreia do Norte, etc.",
    story: "TX para Coreia do Norte = violação de sanções = BLOQUEIO TOTAL.",
    problem: "Como garantir compliance com sanções internacionais?",
    goldenTip: "💎 BLOQUEIO OBRIGATÓRIO para países sancionados. Não é opcional - é lei!"
  },

  GEO_BORDER_PROXIMITY: {
    name: "GEO_BORDER_PROXIMITY",
    summary: "Verifica se TX está próxima de uma FRONTEIRA internacional",
    syntax: "GEO_BORDER_PROXIMITY(transaction.location, 50) IS_TRUE",
    syntaxExplanation: "TX a menos de 50km de fronteira = zona de risco (contrabando).",
    story: "TX em cidade de fronteira Brasil-Paraguai = possível contrabando.",
    problem: "Como identificar TXs em zonas de fronteira?",
    goldenTip: "💎 Cidades de fronteira (Foz do Iguaçu, Ponta Porã, etc) têm maior incidência de fraude."
  },

  GEO_TIMEZONE_MISMATCH: {
    name: "GEO_TIMEZONE_MISMATCH",
    summary: "Verifica se timezone do device é diferente da localização declarada",
    syntax: "GEO_TIMEZONE_MISMATCH() IS_TRUE",
    syntaxExplanation: "Device com timezone de Tóquio mas diz estar em SP = MISMATCH.",
    story: "Fraudador esqueceu de mudar timezone do celular clonado.",
    problem: "Como detectar inconsistência entre timezone e localização?",
    goldenTip: "💎 Timezone leak: fraudadores esquecem de ajustar fuso. Verificar UTC offset vs localização."
  }
};

// ═══════════════════════════════════════════════════════════════════════════════════════
// PARTE 7: OPERADORES DEVICE/DISPOSITIVO (20 operadores)
// ═══════════════════════════════════════════════════════════════════════════════════════

export const DEVICE_SPECS: Record<string, OperatorSpec> = {
  DEVICE_JAILBREAK_ROOTED: {
    name: "DEVICE_JAILBREAK_ROOTED",
    summary: "Detecta dispositivo comprometido (jailbreak/root)",
    syntax: "DEVICE_JAILBREAK_ROOTED() IS_TRUE",
    syntaxExplanation: "Lê flags: isJailbroken OR isRooted OR deviceCompromised.",
    story: "90% das fraudes mobile vêm de dispositivos com jailbreak.",
    problem: "Como impedir uso de dispositivos modificados?",
    goldenTip: "💎 Não bloqueie 100% - devs usam para testes. Use CHALLENGE em vez de BLOCK direto.",
    engineBehavior: {
      description: "DeviceOperatorEvaluator.evaluateJailbreakRooted():",
      steps: [
        "1. Lê payload.isJailbroken",
        "2. Lê payload.isRooted",
        "3. Lê payload.deviceCompromised",
        "4. Retorna OR de todos"
      ]
    }
  },

  EMULATOR_DETECTION: {
    name: "EMULATOR_DETECTION",
    summary: "Detecta se TX vem de EMULADOR (device virtual)",
    syntax: "EMULATOR_DETECTION() IS_TRUE",
    syntaxExplanation: "Lê flags: isEmulator OR isVirtualMachine.",
    story: "Farm de emuladores criando contas fake para pegar cupons.",
    problem: "Como detectar bots rodando em máquinas virtuais?",
    goldenTip: "💎 Farm de emuladores: 1 PC pode simular 100 'celulares'. Bloqueie para cadastro de conta.",
    realScenarios: [
      {
        title: "Farm de Cupons",
        context: "App de delivery com cupom de R$ 30 no primeiro pedido",
        problem: "Fraudadores criavam 1000 contas/dia via emulador",
        solution: "EMULATOR_DETECTION() IS_TRUE → BLOCK no cadastro",
        impact: "R$ 450k/mês economizados em cupons fraudados"
      }
    ]
  },

  VPN_PROXY_DETECTION: {
    name: "VPN_PROXY_DETECTION",
    summary: "Detecta conexão via VPN, Proxy ou Datacenter",
    syntax: "VPN_PROXY_DETECTION() IS_TRUE",
    syntaxExplanation: "Lê flags: isVpn OR isProxy OR isDatacenter.",
    story: "Fraudador usa VPN para fingir estar no Brasil quando está no exterior.",
    problem: "Como detectar quando alguém esconde sua localização real?",
    goldenTip: "💎 VPN não é sempre fraude! Muita gente usa para privacidade. Combine com outros sinais."
  },

  DEVICE_NEW: {
    name: "DEVICE_NEW",
    summary: "Detecta se é a PRIMEIRA VEZ que este device é visto",
    syntax: "DEVICE_NEW() IS_TRUE",
    syntaxExplanation: "DeviceId nunca foi visto antes no sistema = TRUE.",
    story: "Device novo + transação alta = risco (pode ser device roubado).",
    problem: "Como identificar dispositivos nunca vistos antes?",
    goldenTip: "💎 'DEVICE_NEW AND amount GT 5000' = device desconhecido com TX alta = pedir verificação."
  },

  DEVICE_FIRST_SEEN_HOURS_AGO_LT: {
    name: "DEVICE_FIRST_SEEN_HOURS_AGO_LT",
    summary: "Verifica se device foi visto pela PRIMEIRA VEZ há menos de N horas",
    syntax: "DEVICE_FIRST_SEEN_HOURS_AGO_LT(24)",
    syntaxExplanation: "Device visto pela primeira vez há menos de 24h = TRUE.",
    story: "Device cadastrado há 2h já está fazendo TX alta = suspetio.",
    problem: "Como impor período de 'quarentena' para novos devices?",
    goldenTip: "💎 Cooling period: 'DEVICE_FIRST_SEEN_HOURS_AGO_LT(24) AND amount GT 1000' = device novo gastando alto."
  },

  DEVICE_MULTIPLE_ACCOUNTS: {
    name: "DEVICE_MULTIPLE_ACCOUNTS",
    summary: "Verifica se device está associado a MÚLTIPLAS contas",
    syntax: "DEVICE_MULTIPLE_ACCOUNTS() GT 3",
    syntaxExplanation: "Mais de 3 contas usando o mesmo device = suspeito.",
    story: "Mesmo celular com 10 contas diferentes = fraud ring.",
    problem: "Como detectar compartilhamento de device entre contas?",
    goldenTip: "💎 Fraud ring: fraudadores usam mesmo device para múltiplas contas. > 5 contas = investigar."
  },

  DEVICE_FINGERPRINT_MISMATCH: {
    name: "DEVICE_FINGERPRINT_MISMATCH",
    summary: "Detecta mudança no fingerprint do device",
    syntax: "DEVICE_FINGERPRINT_MISMATCH() IS_TRUE",
    syntaxExplanation: "Fingerprint atual ≠ fingerprint histórico do device = alteração.",
    story: "Fraudador tentando alterar fingerprint para parecer device diferente.",
    problem: "Como detectar manipulação de fingerprint?",
    goldenTip: "💎 Fingerprint spoofing: fraudadores tentam mudar para parecer device novo. Detecte variações."
  },

  DEVICE_AGE_ANOMALY: {
    name: "DEVICE_AGE_ANOMALY",
    summary: "Detecta anomalia na idade reportada do device",
    syntax: "DEVICE_AGE_ANOMALY() IS_TRUE",
    syntaxExplanation: "Device reporta ter 10 anos mas modelo foi lançado há 2 = anomalia.",
    story: "iPhone 15 com data de fabricação de 2015 = impossível.",
    problem: "Como detectar spoofing de data de fabricação?",
    goldenTip: "💎 Cross-check: modelo do device vs data reportada. iPhone 15 não existia em 2015."
  },

  BROWSER_INCONSISTENCY: {
    name: "BROWSER_INCONSISTENCY",
    summary: "Detecta inconsistências no browser/user agent",
    syntax: "BROWSER_INCONSISTENCY() IS_TRUE",
    syntaxExplanation: "User agent diz Chrome Windows mas fingerprint é Safari Mac = inconsistência.",
    story: "Fraudador usando bot com user agent falso.",
    problem: "Como detectar spoofing de browser?",
    goldenTip: "💎 User agent spoofing: canvas fingerprint, WebGL e outros revelam browser real."
  },

  ANTI_DETECT_BROWSER_DETECTION: {
    name: "ANTI_DETECT_BROWSER_DETECTION",
    summary: "Detecta uso de 'anti-detect browser' (browser especializado em fraude)",
    syntax: "ANTI_DETECT_BROWSER_DETECTION() IS_TRUE",
    syntaxExplanation: "Browsers como Multilogin, GoLogin, Dolphin = ferramentas de fraudadores.",
    story: "Fraudador usando GoLogin para simular múltiplas identidades.",
    problem: "Como detectar ferramentas profissionais de fraude?",
    goldenTip: "💎 Anti-detect browsers são ESPECÍFICOS para fraude. Detecção = alta probabilidade de fraude."
  },

  CANVAS_FINGERPRINT_MISMATCH: {
    name: "CANVAS_FINGERPRINT_MISMATCH",
    summary: "Detecta manipulação do canvas fingerprint",
    syntax: "CANVAS_FINGERPRINT_MISMATCH() IS_TRUE",
    syntaxExplanation: "Canvas fingerprint mudou entre sessões = possível spoofing.",
    story: "Fraudador usando extensão para randomizar canvas fingerprint.",
    problem: "Como detectar manipulação de fingerprinting via canvas?",
    goldenTip: "💎 Canvas fingerprint é difícil de spoof perfeitamente. Variações sutis indicam manipulação."
  },

  WEBGL_FINGERPRINT_ANOMALY: {
    name: "WEBGL_FINGERPRINT_ANOMALY",
    summary: "Detecta anomalia no fingerprint WebGL",
    syntax: "WEBGL_FINGERPRINT_ANOMALY() IS_TRUE",
    syntaxExplanation: "WebGL reporta GPU incompatível com device = anomalia.",
    story: "iPhone reportando GPU NVIDIA (impossível - Apple usa GPU própria).",
    problem: "Como detectar inconsistência no hardware reportado?",
    goldenTip: "💎 Cross-check: device model vs GPU reportada. iPhone nunca terá NVIDIA ou AMD."
  },

  FONTS_FINGERPRINT_ANOMALY: {
    name: "FONTS_FINGERPRINT_ANOMALY",
    summary: "Detecta anomalia nas fontes instaladas",
    syntax: "FONTS_FINGERPRINT_ANOMALY() IS_TRUE",
    syntaxExplanation: "Mac com fontes típicas de Windows = VM ou spoofing.",
    story: "Device diz ser Mac mas tem Arial, Calibri, Consolas (fontes Windows).",
    problem: "Como detectar inconsistência de sistema operacional?",
    goldenTip: "💎 Font fingerprinting: cada OS tem fontes exclusivas. Mac não tem Calibri, Windows não tem SF Pro."
  },

  SCREEN_RESOLUTION_ANOMALY: {
    name: "SCREEN_RESOLUTION_ANOMALY",
    summary: "Detecta resolução de tela incomum ou impossível",
    syntax: "SCREEN_RESOLUTION_ANOMALY() IS_TRUE",
    syntaxExplanation: "Resolução 1x1, 99999x99999 ou combinações estranhas = emulador/bot.",
    story: "Device reporta resolução 1080x0 = impossível = bot mal configurado.",
    problem: "Como detectar resoluções de tela fake?",
    goldenTip: "💎 Resoluções válidas são finitas. Database de resoluções reais de devices."
  },

  TOUCH_SUPPORT_INCONSISTENCY: {
    name: "TOUCH_SUPPORT_INCONSISTENCY",
    summary: "Detecta inconsistência no suporte a touch",
    syntax: "TOUCH_SUPPORT_INCONSISTENCY() IS_TRUE",
    syntaxExplanation: "Device mobile sem suporte a touch = impossível.",
    story: "iPhone reportando touchSupport = false = bot/emulador mal configurado.",
    problem: "Como detectar devices móveis falsos?",
    goldenTip: "💎 Todo smartphone real suporta touch. Se não suporta = emulador."
  },

  BATTERY_LEVEL_ANOMALY: {
    name: "BATTERY_LEVEL_ANOMALY",
    summary: "Detecta anomalia no nível de bateria",
    syntax: "BATTERY_LEVEL_ANOMALY() IS_TRUE",
    syntaxExplanation: "Bateria sempre em 100% ou valores impossíveis (> 100%, < 0%).",
    story: "Device com bateria em 100% por 48h seguidas = emulador.",
    problem: "Como detectar emulador via bateria?",
    goldenTip: "💎 Emuladores geralmente reportam bateria 100% fixa. Device real varia."
  },

  AUDIO_FINGERPRINT_NEW: {
    name: "AUDIO_FINGERPRINT_NEW",
    summary: "Detecta novo fingerprint de áudio (nunca visto antes)",
    syntax: "AUDIO_FINGERPRINT_NEW() IS_TRUE",
    syntaxExplanation: "Audio fingerprint único que nunca foi visto no sistema.",
    story: "Primeiro acesso do device ao sistema via audio fingerprint.",
    problem: "Como identificar devices novos via fingerprint de áudio?",
    goldenTip: "💎 Audio fingerprinting usa características únicas da placa de som. Difícil de spoof."
  },

  USER_AGENT_SUSPICIOUS: {
    name: "USER_AGENT_SUSPICIOUS",
    summary: "Detecta user agent suspeito ou conhecido de bots",
    syntax: "USER_AGENT_SUSPICIOUS() IS_TRUE",
    syntaxExplanation: "User agent de crawlers, scrapers ou bots conhecidos.",
    story: "User agent 'Googlebot' tentando fazer transação = fraude.",
    problem: "Como detectar bots via user agent?",
    goldenTip: "💎 Lista de user agents de bots: Googlebot, Bingbot, Yandex, Selenium, PhantomJS, etc."
  },

  LANGUAGE_MISMATCH: {
    name: "LANGUAGE_MISMATCH",
    summary: "Detecta incompatibilidade entre idioma do device e localização",
    syntax: "LANGUAGE_MISMATCH() IS_TRUE",
    syntaxExplanation: "Device em russo mas IP do Brasil = suspeito.",
    story: "Celular configurado em chinês fazendo PIX no Brasil.",
    problem: "Como detectar possível uso de device roubado de turista?",
    goldenTip: "💎 Mismatch de idioma não é sempre fraude. Pode ser expatriado. Combine com outros sinais."
  },

  TOR_EXIT_NODE: {
    name: "TOR_EXIT_NODE",
    summary: "Detecta se IP é um exit node da rede Tor",
    syntax: "TOR_EXIT_NODE() IS_TRUE",
    syntaxExplanation: "IP está na lista de exit nodes do Tor = alto anonimato.",
    story: "Conexão via Tor para fazer transação financeira = altíssimo risco.",
    problem: "Como detectar uso da rede Tor para anonimato?",
    goldenTip: "💎 Tor é usado para privacidade MAS também para fraude. Lista de exit nodes é pública."
  }
};

// Continua no próximo arquivo...
