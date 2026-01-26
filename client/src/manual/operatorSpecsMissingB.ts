/**
 * OPERATOR_SPECS_COMPLETE - PARTE 6 (MISSING EMV-MANN)
 */

import type { OperatorSpec } from './operatorSpecs';

export const MISSING_SPECS_EMV_MANN: Record<string, OperatorSpec> = {
  EMV_SECURITY_CHECK: {
    name: "EMV_SECURITY_CHECK",
    summary: "Verifica conformidade de segurança EMV",
    syntax: "EMV_SECURITY_CHECK(transaction) PASSED",
    syntaxExplanation: "Confere chip/EMV tags e parâmetros esperados.",
    story: "Transação chip sem CVM esperado = suspeita.",
    problem: "Como validar integridade EMV?",
    goldenTip: "💎 Falhas EMV em presença física são red flags fortes."
  },

  ENTROPY_SCORE_ANOMALY: {
    name: "ENTROPY_SCORE_ANOMALY",
    summary: "Detecta entropia anormal em dados (padrões artificiais)",
    syntax: "ENTROPY_SCORE_ANOMALY(values) IS_TRUE",
    syntaxExplanation: "Entropia muito baixa = dados repetitivos; alta demais = ruído.",
    story: "Valores todos iguais = entropia baixa, possível fraude scriptada.",
    problem: "Como detectar padrões artificiais?",
    goldenTip: "💎 Entropia baixa é típica de bots; alta pode indicar randomização maliciosa."
  },

  EXPIRES_WITHIN_DAYS: {
    name: "EXPIRES_WITHIN_DAYS",
    summary: "Verifica se algo expira dentro de N dias",
    syntax: "EXPIRES_WITHIN_DAYS(cardExpiry, 30) IS_TRUE",
    syntaxExplanation: "Retorna true se expira em até 30 dias.",
    story: "Cartão vence em 2 semanas; alertar cliente.",
    problem: "Como alertar sobre expiração próxima?",
    goldenTip: "💎 Combine com EXPIRED_CARD para bloquear vencidos."
  },

  FATF_CORRESPONDENT_LAYERING: {
    name: "FATF_CORRESPONDENT_LAYERING",
    summary: "Tipologia FATF: layering via bancos correspondentes",
    syntax: "FATF_CORRESPONDENT_LAYERING(transaction) INDICATOR",
    syntaxExplanation: "Uso de correspondentes para obscurecer origem.",
    story: "Transferência por múltiplos correspondentes offshore.",
    problem: "Como detectar layering via correspondentes?",
    goldenTip: "💎 Combine com NESTED_CORRESPONDENT_CHECK."
  },

  FATF_CRYPTO_ATM_CASHOUT: {
    name: "FATF_CRYPTO_ATM_CASHOUT",
    summary: "Tipologia FATF: cashout via ATM cripto",
    syntax: "FATF_CRYPTO_ATM_CASHOUT(transaction) INDICATOR",
    syntaxExplanation: "Conversão cripto para cash em ATM.",
    story: "Saque em ATM cripto após entrada suspeita.",
    problem: "Como detectar cashout via ATM cripto?",
    goldenTip: "💎 ATMs cripto são usados para anonimização."
  },

  FATF_CRYPTO_MIXING: {
    name: "FATF_CRYPTO_MIXING",
    summary: "Tipologia FATF: uso de mixers",
    syntax: "FATF_CRYPTO_MIXING(transaction) INDICATOR",
    syntaxExplanation: "Funds passam por mixer/tumbler.",
    story: "BTC enviado para mixer e retornado fragmentado.",
    problem: "Como detectar mixing services?",
    goldenTip: "💎 Endereços de mixers são conhecidos e podem ser listados."
  },

  FATF_HAWALA_INFORMAL: {
    name: "FATF_HAWALA_INFORMAL",
    summary: "Tipologia FATF: sistemas informais (hawala)",
    syntax: "FATF_HAWALA_INFORMAL(transaction) INDICATOR",
    syntaxExplanation: "Transferência informal sem lastro bancário.",
    story: "Fluxos entre brokers informais em países distintos.",
    problem: "Como detectar hawala?",
    goldenTip: "💎 Procure padrões de compensação sem transferências bancárias claras."
  },

  FATF_INSURANCE_CASH_VALUE: {
    name: "FATF_INSURANCE_CASH_VALUE",
    summary: "Tipologia FATF: seguros com valor de resgate",
    syntax: "FATF_INSURANCE_CASH_VALUE(transaction) INDICATOR",
    syntaxExplanation: "Compra e resgate rápido de seguros para lavar dinheiro.",
    story: "Cliente compra seguro e resgata em poucos dias.",
    problem: "Como detectar uso de seguros para lavagem?",
    goldenTip: "💎 Resgates rápidos e valores altos são sinais."
  },

  FATF_INTEGRATION_BUSINESS_INVESTMENT: {
    name: "FATF_INTEGRATION_BUSINESS_INVESTMENT",
    summary: "Tipologia FATF: integração via investimento empresarial",
    syntax: "FATF_INTEGRATION_BUSINESS_INVESTMENT(transaction) INDICATOR",
    syntaxExplanation: "Lavagem via aquisição/investimento em empresas.",
    story: "Investimento elevado em empresa recém-criada.",
    problem: "Como detectar integração via investimentos?",
    goldenTip: "💎 Investimentos sem justificativa econômica são suspeitos."
  },

  FATF_INTEGRATION_LOAN_REPAYMENT: {
    name: "FATF_INTEGRATION_LOAN_REPAYMENT",
    summary: "Tipologia FATF: integração via quitação de empréstimos",
    syntax: "FATF_INTEGRATION_LOAN_REPAYMENT(transaction) INDICATOR",
    syntaxExplanation: "Criminosos quitam empréstimos com fundos ilícitos.",
    story: "Empréstimo quitado à vista com recursos suspeitos.",
    problem: "Como detectar integração por quitação?",
    goldenTip: "💎 Prepayment incomum + origem obscura = alerta."
  },

  FATF_INTEGRATION_LUXURY_GOODS: {
    name: "FATF_INTEGRATION_LUXURY_GOODS",
    summary: "Tipologia FATF: integração via bens de luxo",
    syntax: "FATF_INTEGRATION_LUXURY_GOODS(transaction) INDICATOR",
    syntaxExplanation: "Compra de itens caros para legitimar fundos.",
    story: "Compra de relógio de luxo com cash sem origem clara.",
    problem: "Como detectar integração via bens de luxo?",
    goldenTip: "💎 Bens de luxo são portáteis e fáceis de revender."
  },

  FATF_INTEGRATION_REAL_ESTATE: {
    name: "FATF_INTEGRATION_REAL_ESTATE",
    summary: "Tipologia FATF: integração via imóveis",
    syntax: "FATF_INTEGRATION_REAL_ESTATE(transaction) INDICATOR",
    syntaxExplanation: "Compra de imóveis com fundos ilícitos.",
    story: "Imóvel comprado à vista por empresa recém-criada.",
    problem: "Como detectar integração via real estate?",
    goldenTip: "💎 Use avaliação de preço para detectar over/under-valuation."
  },

  FATF_LAYERING_CONVERTIBLE_INSTRUMENTS: {
    name: "FATF_LAYERING_CONVERTIBLE_INSTRUMENTS",
    summary: "Tipologia FATF: layering via instrumentos conversíveis",
    syntax: "FATF_LAYERING_CONVERTIBLE_INSTRUMENTS(transaction) INDICATOR",
    syntaxExplanation: "Uso de bonds/notes para obscurecer fluxo.",
    story: "Compra e conversão rápida de instrumentos financeiros.",
    problem: "Como detectar layering financeiro?",
    goldenTip: "💎 Instrumentos conversíveis reduzem rastreabilidade."
  },

  FATF_LAYERING_OFFSHORE: {
    name: "FATF_LAYERING_OFFSHORE",
    summary: "Tipologia FATF: layering via offshore",
    syntax: "FATF_LAYERING_OFFSHORE(transaction) INDICATOR",
    syntaxExplanation: "Uso de empresas offshore para esconder origem.",
    story: "Transferências para jurisdições offshore sem razão econômica.",
    problem: "Como detectar layering offshore?",
    goldenTip: "💎 Offshore + empresas recém-criadas = alto risco."
  },

  FATF_LAYERING_RAPID_MOVEMENT: {
    name: "FATF_LAYERING_RAPID_MOVEMENT",
    summary: "Tipologia FATF: movimentação rápida de fundos",
    syntax: "FATF_LAYERING_RAPID_MOVEMENT(transaction) INDICATOR",
    syntaxExplanation: "Entradas e saídas rápidas para obscurecer.",
    story: "Recebe e transfere em minutos para várias contas.",
    problem: "Como detectar rapid movement?",
    goldenTip: "💎 Rapid movement é típico de layering."
  },

  FATF_LAYERING_SHELL_COMPANY: {
    name: "FATF_LAYERING_SHELL_COMPANY",
    summary: "Tipologia FATF: shell companies",
    syntax: "FATF_LAYERING_SHELL_COMPANY(entity) INDICATOR",
    syntaxExplanation: "Empresas sem atividade real usadas para transitar fundos.",
    story: "Empresa sem funcionários movimenta milhões.",
    problem: "Como identificar shell companies?",
    goldenTip: "💎 Verifique UBO, endereço, funcionários, receita."
  },

  FATF_LAYERING_WIRE_CHAINS: {
    name: "FATF_LAYERING_WIRE_CHAINS",
    summary: "Tipologia FATF: cadeias de wire transfers",
    syntax: "FATF_LAYERING_WIRE_CHAINS(transaction) INDICATOR",
    syntaxExplanation: "Séries de wires curtas e sequenciais.",
    story: "Wire A→B→C→D em poucas horas.",
    problem: "Como detectar chains de wire transfers?",
    goldenTip: "💎 Use análise de grafos para cadeia profunda."
  },

  FATF_NEW_PAYMENT_EXPLOITATION: {
    name: "FATF_NEW_PAYMENT_EXPLOITATION",
    summary: "Tipologia FATF: exploração de novos meios de pagamento",
    syntax: "FATF_NEW_PAYMENT_EXPLOITATION(transaction) INDICATOR",
    syntaxExplanation: "Uso de novos rails para mascarar origem.",
    story: "Fraudadores migram para método recém-lançado.",
    problem: "Como monitorar novos meios?",
    goldenTip: "💎 Novos produtos precisam de regras mais rígidas no início."
  },

  FATF_PEP_TRANSACTION: {
    name: "FATF_PEP_TRANSACTION",
    summary: "Tipologia FATF: transações envolvendo PEP",
    syntax: "FATF_PEP_TRANSACTION(customer) INDICATOR",
    syntaxExplanation: "PEP requer EDD e monitoramento intensivo.",
    story: "PEP faz transferência internacional grande.",
    problem: "Como detectar risco PEP?",
    goldenTip: "💎 PEP não é proibido, mas exige revisão reforçada."
  },

  FATF_PLACEMENT_CASH_INTENSIVE: {
    name: "FATF_PLACEMENT_CASH_INTENSIVE",
    summary: "Tipologia FATF: placement em negócios cash-intensive",
    syntax: "FATF_PLACEMENT_CASH_INTENSIVE(business) INDICATOR",
    syntaxExplanation: "Dinheiro ilícito entra via negócios com alto cash.",
    story: "Lanchonete com receita cash muito acima do normal.",
    problem: "Como detectar placement via cash-intensive?",
    goldenTip: "💎 Compare faturamento declarado vs movimentação."
  },

  FATF_PLACEMENT_CASINO_GAMBLING: {
    name: "FATF_PLACEMENT_CASINO_GAMBLING",
    summary: "Tipologia FATF: placement em cassinos",
    syntax: "FATF_PLACEMENT_CASINO_GAMBLING(transaction) INDICATOR",
    syntaxExplanation: "Compra de fichas e resgate para “limpar” dinheiro.",
    story: "Compra de fichas e resgate imediato.",
    problem: "Como detectar lavagem via cassino?",
    goldenTip: "💎 Grandes apostas com pouca volatilidade são suspeitas."
  },

  FATF_PLACEMENT_CURRENCY_EXCHANGE: {
    name: "FATF_PLACEMENT_CURRENCY_EXCHANGE",
    summary: "Tipologia FATF: placement via câmbio",
    syntax: "FATF_PLACEMENT_CURRENCY_EXCHANGE(transaction) INDICATOR",
    syntaxExplanation: "Troca de moedas para ofuscar origem.",
    story: "Múltiplas trocas de moeda sem viagem associada.",
    problem: "Como detectar uso de casas de câmbio?",
    goldenTip: "💎 Exchanges frequentes sem justificativa = red flag."
  },

  FATF_PLACEMENT_SMURFING: {
    name: "FATF_PLACEMENT_SMURFING",
    summary: "Tipologia FATF: smurfing",
    syntax: "FATF_PLACEMENT_SMURFING(transactions) INDICATOR",
    syntaxExplanation: "Fragmentação de valores para evitar reportes.",
    story: "Múltiplos depósitos de R$ 9.900.",
    problem: "Como detectar structuring?",
    goldenTip: "💎 Use PATTERN_SPLIT_TRANSACTION e ROUND_AMOUNT."
  },

  FATF_PLACEMENT_STRUCTURING: {
    name: "FATF_PLACEMENT_STRUCTURING",
    summary: "Tipologia FATF: structuring",
    syntax: "FATF_PLACEMENT_STRUCTURING(transactions) INDICATOR",
    syntaxExplanation: "Muitos depósitos abaixo de limite regulatório.",
    story: "Depósitos de R$ 9.999 repetidos.",
    problem: "Como detectar structuring?",
    goldenTip: "💎 Combine com DECIMAL_PLACES_GT e ROUND_AMOUNT."
  },

  FATF_TBML_FALSE_DESCRIPTION: {
    name: "FATF_TBML_FALSE_DESCRIPTION",
    summary: "Tipologia FATF: descrição falsa de mercadorias (TBML)",
    syntax: "FATF_TBML_FALSE_DESCRIPTION(invoice) INDICATOR",
    syntaxExplanation: "Descrição não condiz com mercadoria real.",
    story: "Declaração de 'peças' para eletrônicos de alto valor.",
    problem: "Como detectar TBML por descrição falsa?",
    goldenTip: "💎 Compare descrição com NCM/HS code."
  },

  FATF_TBML_MULTIPLE_INVOICING: {
    name: "FATF_TBML_MULTIPLE_INVOICING",
    summary: "Tipologia FATF: múltiplas faturas para mesma carga",
    syntax: "FATF_TBML_MULTIPLE_INVOICING(shipment) INDICATOR",
    syntaxExplanation: "Duplicação de faturas para inflar valores.",
    story: "Mesma carga faturada 3 vezes.",
    problem: "Como detectar multi-invoicing?",
    goldenTip: "💎 Verifique BL, AWB e data de embarque."
  },

  FATF_TBML_OVER_INVOICING: {
    name: "FATF_TBML_OVER_INVOICING",
    summary: "Tipologia FATF: sobre-invoicing",
    syntax: "FATF_TBML_OVER_INVOICING(invoice) INDICATOR",
    syntaxExplanation: "Fatura acima do preço de mercado.",
    story: "Mercadoria de R$ 10k faturada por R$ 50k.",
    problem: "Como detectar over-invoicing?",
    goldenTip: "💎 Compare com preços de referência e mercado."
  },

  FATF_TBML_PHANTOM_SHIPPING: {
    name: "FATF_TBML_PHANTOM_SHIPPING",
    summary: "Tipologia FATF: embarque fantasma",
    syntax: "FATF_TBML_PHANTOM_SHIPPING(shipment) INDICATOR",
    syntaxExplanation: "Mercadoria nunca embarcou, mas fatura existe.",
    story: "BL inválido e sem registro de transporte.",
    problem: "Como detectar phantom shipping?",
    goldenTip: "💎 Verificar com transportadoras e registros aduaneiros."
  },

  FATF_TBML_UNDER_INVOICING: {
    name: "FATF_TBML_UNDER_INVOICING",
    summary: "Tipologia FATF: under-invoicing",
    syntax: "FATF_TBML_UNDER_INVOICING(invoice) INDICATOR",
    syntaxExplanation: "Fatura abaixo do preço real.",
    story: "Mercadoria de R$ 50k faturada por R$ 10k.",
    problem: "Como detectar under-invoicing?",
    goldenTip: "💎 Subfaturamento reduz impostos e movimenta fundos ilícitos."
  },

  FIELD_EQ: {
    name: "FIELD_EQ",
    summary: "Compara se dois campos são iguais",
    syntax: "FIELD_EQ(fieldA, fieldB)",
    syntaxExplanation: "Retorna true quando fieldA == fieldB.",
    story: "billingAddress == shippingAddress.",
    problem: "Como comparar dois campos no mesmo payload?",
    goldenTip: "💎 Use FIELD_NEQ para detectar divergências."
  },

  FIELD_GT: {
    name: "FIELD_GT",
    summary: "Compara se campo A é maior que campo B",
    syntax: "FIELD_GT(fieldA, fieldB)",
    syntaxExplanation: "Retorna true se A > B.",
    story: "amount > dailyLimit.",
    problem: "Como comparar campos numéricos?",
    goldenTip: "💎 Combine com FIELD_GTE se limite é inclusivo."
  },

  FIELD_GTE: {
    name: "FIELD_GTE",
    summary: "Compara se campo A é maior ou igual a campo B",
    syntax: "FIELD_GTE(fieldA, fieldB)",
    syntaxExplanation: "Retorna true se A >= B.",
    story: "amount >= maxAllowed.",
    problem: "Como comparar com inclusão de igualdade?",
    goldenTip: "💎 Use FIELD_GT para exclusividade."
  },

  FIELD_LT: {
    name: "FIELD_LT",
    summary: "Compara se campo A é menor que campo B",
    syntax: "FIELD_LT(fieldA, fieldB)",
    syntaxExplanation: "Retorna true se A < B.",
    story: "availableBalance < amount.",
    problem: "Como comparar limites com campos?",
    goldenTip: "💎 Use FIELD_LTE quando igualdade for aceitável."
  },

  FIELD_LTE: {
    name: "FIELD_LTE",
    summary: "Compara se campo A é menor ou igual a campo B",
    syntax: "FIELD_LTE(fieldA, fieldB)",
    syntaxExplanation: "Retorna true se A <= B.",
    story: "amount <= dailyLimit.",
    problem: "Como comparar com limite inclusivo?",
    goldenTip: "💎 Limites inclusivos evitam rejeição indevida."
  },

  FIELD_NEQ: {
    name: "FIELD_NEQ",
    summary: "Compara se dois campos são diferentes",
    syntax: "FIELD_NEQ(fieldA, fieldB)",
    syntaxExplanation: "Retorna true se A != B.",
    story: "billingAddress != shippingAddress.",
    problem: "Como detectar divergências entre campos?",
    goldenTip: "💎 Divergência não é fraude, mas aumenta risco."
  },

  FPGROWTH_FREQUENT_PATTERNS: {
    name: "FPGROWTH_FREQUENT_PATTERNS",
    summary: "Detecta padrões frequentes de crescimento (FP-Growth)",
    syntax: "FPGROWTH_FREQUENT_PATTERNS(events) HAS_PATTERN",
    syntaxExplanation: "Minera padrões frequentes de eventos sem gerar candidatos.",
    story: "Padrão recorrente de device novo + VPN + horário noturno.",
    problem: "Como descobrir padrões frequentes eficientemente?",
    goldenTip: "💎 FP-Growth é mais eficiente que Apriori em datasets grandes."
  },

  FRAUD: {
    name: "FRAUD",
    summary: "Marca evento como fraude (labeling)",
    syntax: "FRAUD() IS_TRUE",
    syntaxExplanation: "Usado para tagging em backtests ou datasets.",
    story: "Transação confirmada como fraude recebe label.",
    problem: "Como marcar casos para treinamento/avaliação?",
    goldenTip: "💎 Labels corretos são essenciais para modelos e regras."
  },

  FREQUENCY_PATTERN_CHANGE: {
    name: "FREQUENCY_PATTERN_CHANGE",
    summary: "Detecta mudança no padrão de frequência",
    syntax: "FREQUENCY_PATTERN_CHANGE(customerId) IS_TRUE",
    syntaxExplanation: "Frequência histórica muda abruptamente.",
    story: "Cliente de 1 TX/semana faz 20/dia.",
    problem: "Como detectar alteração de ritmo?",
    goldenTip: "💎 Compare com baseline do próprio cliente."
  },

  FUZZY_ADAPTIVE_THRESHOLD: {
    name: "FUZZY_ADAPTIVE_THRESHOLD",
    summary: "Threshold fuzzy adaptativo",
    syntax: "FUZZY_ADAPTIVE_THRESHOLD(score) GT 0.7",
    syntaxExplanation: "Limiar se ajusta com incerteza e contexto.",
    story: "Score 0.65 pode ser alto em contexto de risco.",
    problem: "Como lidar com incerteza em decisões?",
    goldenTip: "💎 Use com FUZZY_MEMBERSHIP para suavizar decisões."
  },

  FUZZY_MEMBERSHIP: {
    name: "FUZZY_MEMBERSHIP",
    summary: "Calcula pertinência fuzzy a um conjunto",
    syntax: "FUZZY_MEMBERSHIP(value, 'high_risk') GT 0.8",
    syntaxExplanation: "Pertinência 0-1 em categorias fuzzy.",
    story: "Valor tem 0.9 de pertinência a 'alto risco'.",
    problem: "Como evitar decisões binárias rígidas?",
    goldenTip: "💎 Fuzzy é útil para sinais ambíguos."
  },

  GDPR_DATA_RETENTION_CHECK: {
    name: "GDPR_DATA_RETENTION_CHECK",
    summary: "Verifica política de retenção de dados (GDPR)",
    syntax: "GDPR_DATA_RETENTION_CHECK(record) COMPLIANT",
    syntaxExplanation: "Garante que dados não ultrapassaram prazo legal.",
    story: "Dado pessoal > 5 anos sem base legal = não compliance.",
    problem: "Como garantir compliance com retenção GDPR?",
    goldenTip: "💎 Sempre registre base legal e prazos por categoria de dado."
  },

  GEOGRAPHIC_BEHAVIOR_SHIFT: {
    name: "GEOGRAPHIC_BEHAVIOR_SHIFT",
    summary: "Detecta mudança geográfica no comportamento",
    syntax: "GEOGRAPHIC_BEHAVIOR_SHIFT(customerId) IS_TRUE",
    syntaxExplanation: "Padrão de localização mudou abruptamente.",
    story: "Cliente do Nordeste começa a operar só no Sul.",
    problem: "Como detectar mudança geográfica suspeita?",
    goldenTip: "💎 Combine com TIMEZONE_MISMATCH."
  },

  GT_CURRENT_DATE: {
    name: "GT_CURRENT_DATE",
    summary: "Verifica se data é maior que a data atual",
    syntax: "transaction.date GT_CURRENT_DATE()",
    syntaxExplanation: "Usado para validar datas futuras inválidas.",
    story: "Data de nascimento no futuro.",
    problem: "Como validar datas incoerentes?",
    goldenTip: "💎 Datas futuras em campos históricos são erros/fraude."
  },

  GT_FIELD_MULTIPLIER: {
    name: "GT_FIELD_MULTIPLIER",
    summary: "Verifica se campo A é maior que campo B multiplicado",
    syntax: "GT_FIELD_MULTIPLIER(amount, avgAmount, 3)",
    syntaxExplanation: "amount > avgAmount * 3.",
    story: "Valor 3x acima do normal.",
    problem: "Como expressar múltiplos de um campo?",
    goldenTip: "💎 Útil para limites dinâmicos (ex: 3x média)."
  },

  GTE_PERCENT_OF_LAST_INCOMING: {
    name: "GTE_PERCENT_OF_LAST_INCOMING",
    summary: "Verifica se valor é >= % da última entrada",
    syntax: "GTE_PERCENT_OF_LAST_INCOMING(amount, 80)",
    syntaxExplanation: "Ex: saque >= 80% do último depósito.",
    story: "Deposita R$ 10k e saca R$ 9k imediatamente.",
    problem: "Como detectar cashout rápido?",
    goldenTip: "💎 Percentual alto logo após entrada = risco de lavagem."
  },

  HARDWARE_CONCURRENCY_MISMATCH: {
    name: "HARDWARE_CONCURRENCY_MISMATCH",
    summary: "Detecta mismatch de núcleos reportados",
    syntax: "HARDWARE_CONCURRENCY_MISMATCH(device) IS_TRUE",
    syntaxExplanation: "navigator.hardwareConcurrency incoerente.",
    story: "Device reporta 64 cores em browser normal.",
    problem: "Como detectar spoofing de device?",
    goldenTip: "💎 Combine com DEVICE_MEMORY_ANOMALY."
  },

  HAS_FAILED_3DS_LAST_N_MINUTES: {
    name: "HAS_FAILED_3DS_LAST_N_MINUTES",
    summary: "Detecta falhas de 3DS nos últimos N minutos",
    syntax: "HAS_FAILED_3DS_LAST_N_MINUTES(cardId, 30) IS_TRUE",
    syntaxExplanation: "Falhas recentes em autenticação 3DS.",
    story: "3DS falhou 2x em 15 minutos.",
    problem: "Como detectar tentativa de bypass 3DS?",
    goldenTip: "💎 Falha repetida = exigir MFA adicional."
  },

  HAS_INCOMING_TRANSFER_LAST_N_HOURS: {
    name: "HAS_INCOMING_TRANSFER_LAST_N_HOURS",
    summary: "Verifica entrada recente de transferência",
    syntax: "HAS_INCOMING_TRANSFER_LAST_N_HOURS(accountId, 24) IS_TRUE",
    syntaxExplanation: "Identifica recebimento recente de fundos.",
    story: "Conta recebeu e tenta sacar imediatamente.",
    problem: "Como detectar cashout pós-entrada?",
    goldenTip: "💎 Entrada recente + saque alto = alerta AML."
  },

  HIGH_RISK_CORRIDOR_CHECK: {
    name: "HIGH_RISK_CORRIDOR_CHECK",
    summary: "Verifica corredor de risco (origem→destino)",
    syntax: "HIGH_RISK_CORRIDOR_CHECK(origin, destination) IS_TRUE",
    syntaxExplanation: "Alguns corredores têm alta incidência de fraude.",
    story: "BR → NG com alto risco.",
    problem: "Como avaliar risco por corredor?",
    goldenTip: "💎 Use dados históricos de fraude por par de países."
  },

  HIGH_RISK_JURISDICTION: {
    name: "HIGH_RISK_JURISDICTION",
    summary: "Verifica se jurisdição é de alto risco",
    syntax: "HIGH_RISK_JURISDICTION(country) IS_TRUE",
    syntaxExplanation: "Baseada em FATF, UE ou listas internas.",
    story: "Transação com destino em jurisdição de alto risco.",
    problem: "Como identificar países de risco?",
    goldenTip: "💎 Atualize a lista com FATF e reguladores locais."
  },

  HOLIDAY_TRANSACTION_SPIKE: {
    name: "HOLIDAY_TRANSACTION_SPIKE",
    summary: "Detecta pico de transações em feriados",
    syntax: "HOLIDAY_TRANSACTION_SPIKE(merchantId) IS_TRUE",
    syntaxExplanation: "Feriados tendem a ter padrões específicos.",
    story: "Merchant com volume 5x no Natal.",
    problem: "Como detectar spikes em feriados?",
    goldenTip: "💎 Use baseline de feriados anteriores."
  },

  HOUR_BETWEEN: {
    name: "HOUR_BETWEEN",
    summary: "Verifica se hora está entre dois valores",
    syntax: "HOUR_BETWEEN(transaction.time, '22:00', '05:00')",
    syntaxExplanation: "Intervalos que cruzam meia-noite são suportados.",
    story: "Transações na madrugada.",
    problem: "Como filtrar por horário?",
    goldenTip: "💎 Use TIME_BETWEEN para precisão com minutos/segundos."
  },

  IDENTITY_VELOCITY: {
    name: "IDENTITY_VELOCITY",
    summary: "Detecta criação/uso rápido de identidades",
    syntax: "IDENTITY_VELOCITY(identityId, DAY_7) GT 3",
    syntaxExplanation: "Múltiplas identidades em curto período.",
    story: "Mesmo device cria 5 identidades em 1 dia.",
    problem: "Como detectar identity farming?",
    goldenTip: "💎 Combine com DEVICE_ACCOUNT_RATIO."
  },

  IMPOSSIBLE_TRAVEL: {
    name: "IMPOSSIBLE_TRAVEL",
    summary: "Detecta deslocamento impossível",
    syntax: "IMPOSSIBLE_TRAVEL(loc1, loc2, timeDelta) IS_TRUE",
    syntaxExplanation: "Velocidade necessária > limite humano.",
    story: "Transação em SP e NY com 2 horas de diferença.",
    problem: "Como detectar uso simultâneo em locais distantes?",
    goldenTip: "💎 Use distância geográfica + janela temporal."
  },

  IN_CUSTOMER_CHARGEBACK_MERCHANTS: {
    name: "IN_CUSTOMER_CHARGEBACK_MERCHANTS",
    summary: "Verifica se merchant já teve chargeback do cliente",
    syntax: "IN_CUSTOMER_CHARGEBACK_MERCHANTS(customerId, merchantId) IS_TRUE",
    syntaxExplanation: "Cliente já contestou esse merchant antes.",
    story: "Cliente volta a comprar em loja que já gerou chargeback.",
    problem: "Como identificar merchants problemáticos para o cliente?",
    goldenTip: "💎 Reincidência aumenta risco de nova contestação."
  },

  IN_CUSTOMER_HISTORY: {
    name: "IN_CUSTOMER_HISTORY",
    summary: "Verifica se valor/entidade está no histórico do cliente",
    syntax: "IN_CUSTOMER_HISTORY(customerId, merchantId) IS_TRUE",
    syntaxExplanation: "Merchant já visto anteriormente pelo cliente.",
    story: "Cliente já comprou nesse merchant 3 vezes.",
    problem: "Como tratar repetição de comportamento legítimo?",
    goldenTip: "💎 Histórico positivo reduz risco e fricção."
  },

  INTEGRATION_PATTERN: {
    name: "INTEGRATION_PATTERN",
    summary: "Detecta padrão de integração (fase final de lavagem)",
    syntax: "INTEGRATION_PATTERN(transactions) IS_TRUE",
    syntaxExplanation: "Gastos em bens/serviços para legitimar fundos.",
    story: "Compra de imóveis após layering.",
    problem: "Como detectar fase de integração AML?",
    goldenTip: "💎 Combine com FATF_INTEGRATION_* regras."
  },

  IS_CRYPTO_RANSOM_AMOUNT: {
    name: "IS_CRYPTO_RANSOM_AMOUNT",
    summary: "Detecta valor típico de resgate em cripto",
    syntax: "IS_CRYPTO_RANSOM_AMOUNT(amount, currency) IS_TRUE",
    syntaxExplanation: "Valores redondos em BTC/ETH associados a ransom.",
    story: "Pagamento de 1.5 BTC para endereço suspeito.",
    problem: "Como identificar ransom payments?",
    goldenTip: "💎 Valores típicos e endereços conhecidos elevam o risco."
  },

  IS_FIRST: {
    name: "IS_FIRST",
    summary: "Verifica se é a primeira ocorrência",
    syntax: "IS_FIRST(customerId, eventType) IS_TRUE",
    syntaxExplanation: "Primeira vez do cliente neste evento.",
    story: "Primeira transferência internacional.",
    problem: "Como identificar primeiro evento?",
    goldenTip: "💎 Primeira vez + valor alto = validação extra."
  },

  IS_HOLIDAY: {
    name: "IS_HOLIDAY",
    summary: "Verifica se data é feriado",
    syntax: "IS_HOLIDAY(transaction.date, 'BR') IS_TRUE",
    syntaxExplanation: "Consulta calendário local de feriados.",
    story: "Transação no Natal.",
    problem: "Como identificar feriados?",
    goldenTip: "💎 Atualize calendários por país e região."
  },

  IS_IMPOSSIBLE_COMBINATION: {
    name: "IS_IMPOSSIBLE_COMBINATION",
    summary: "Detecta combinação impossível de atributos",
    syntax: "IS_IMPOSSIBLE_COMBINATION(device, geo) IS_TRUE",
    syntaxExplanation: "Ex: iOS + model Android, timezone incompatível.",
    story: "User-Agent Android em device fingerprint Apple.",
    problem: "Como detectar inconsistências?",
    goldenTip: "💎 Inconsistências são típicas de spoofing."
  },

  IS_NEW: {
    name: "IS_NEW",
    summary: "Verifica se entidade é nova",
    syntax: "IS_NEW(entityId, DAYS=30) IS_TRUE",
    syntaxExplanation: "Entidade criada há menos de N dias.",
    story: "Conta criada há 3 dias.",
    problem: "Como tratar entidades recentes?",
    goldenTip: "💎 Novo + alto valor = risco alto."
  },

  IS_NEW_DEVICE: {
    name: "IS_NEW_DEVICE",
    summary: "Verifica se device é novo para o usuário",
    syntax: "IS_NEW_DEVICE(customerId, deviceId) IS_TRUE",
    syntaxExplanation: "Device nunca usado antes por este cliente.",
    story: "Cliente loga em device novo e tenta transferir alto valor.",
    problem: "Como detectar device novo?",
    goldenTip: "💎 Novo device = step-up authentication."
  },

  IS_NEW_LOCATION: {
    name: "IS_NEW_LOCATION",
    summary: "Verifica se localização é nova para o usuário",
    syntax: "IS_NEW_LOCATION(customerId, geo) IS_TRUE",
    syntaxExplanation: "Local nunca visto no histórico.",
    story: "Cliente de SP operando em país diferente pela primeira vez.",
    problem: "Como detectar local novo?",
    goldenTip: "💎 Combine com IMPOSSIBLE_TRAVEL."
  },

  IS_VOIP: {
    name: "IS_VOIP",
    summary: "Detecta se telefone é VoIP",
    syntax: "IS_VOIP(phone) IS_TRUE",
    syntaxExplanation: "Telefones VoIP são mais fáceis de descartar.",
    story: "Número VoIP usado em cadastro fraudulento.",
    problem: "Como detectar telefones VoIP?",
    goldenTip: "💎 VoIP + email temporário = alto risco."
  },

  IS_WEEKEND: {
    name: "IS_WEEKEND",
    summary: "Verifica se data cai no fim de semana",
    syntax: "IS_WEEKEND(transaction.date) IS_TRUE",
    syntaxExplanation: "Sábado ou domingo.",
    story: "Regras mais rígidas em fim de semana.",
    problem: "Como aplicar regras por dia?",
    goldenTip: "💎 Combine com BUSINESS_HOURS_DEVIATION."
  },

  LARGE_AMOUNT_FREQUENCY: {
    name: "LARGE_AMOUNT_FREQUENCY",
    summary: "Detecta frequência de valores altos",
    syntax: "LARGE_AMOUNT_FREQUENCY(customerId, DAY_30) GT 3",
    syntaxExplanation: "Conta quantas transações acima de um valor.",
    story: "3 transações > R$ 20k em 1 semana.",
    problem: "Como detectar padrão de alto valor recorrente?",
    goldenTip: "💎 Ajuste valor de referência por perfil."
  },

  LAYERED_TRANSFER_PATTERN: {
    name: "LAYERED_TRANSFER_PATTERN",
    summary: "Detecta padrão de layering em transferências",
    syntax: "LAYERED_TRANSFER_PATTERN(network) IS_TRUE",
    syntaxExplanation: "Múltiplas camadas de transferência em sequência.",
    story: "A→B→C→D em poucas horas.",
    problem: "Como detectar layering AML?",
    goldenTip: "💎 Use tempo curto entre hops como sinal forte."
  },

  LIVENESS_DETECTION_FACIAL: {
    name: "LIVENESS_DETECTION_FACIAL",
    summary: "Verifica se prova de vida facial passou",
    syntax: "LIVENESS_DETECTION_FACIAL(session) PASSED",
    syntaxExplanation: "Detecta se é rosto vivo e não foto/vídeo.",
    story: "Foto impressa falha na liveness.",
    problem: "Como evitar fraude com fotos?",
    goldenTip: "💎 Combine com FACE_DEEPFAKE_DETECTION."
  },

  LIVENESS_DETECTION_VOICE: {
    name: "LIVENESS_DETECTION_VOICE",
    summary: "Verifica prova de vida por voz",
    syntax: "LIVENESS_DETECTION_VOICE(session) PASSED",
    syntaxExplanation: "Detecta se voz é real e presente.",
    story: "Reprodução de áudio falha no liveness.",
    problem: "Como evitar replay attacks de voz?",
    goldenTip: "💎 Use desafio dinâmico (frases aleatórias)."
  },

  LOGIN_PATTERN_DEVIATION: {
    name: "LOGIN_PATTERN_DEVIATION",
    summary: "Detecta desvio no padrão de login",
    syntax: "LOGIN_PATTERN_DEVIATION(userId) IS_TRUE",
    syntaxExplanation: "Horário/IP/device diferentes do habitual.",
    story: "Login às 3h de device novo em outro país.",
    problem: "Como detectar ATO via login?",
    goldenTip: "💎 Login anômalo + tentativa sensível = step-up."
  },

  LT_CURRENT_DATE: {
    name: "LT_CURRENT_DATE",
    summary: "Verifica se data é menor que a data atual",
    syntax: "document.issueDate LT_CURRENT_DATE()",
    syntaxExplanation: "Valida se data está no passado.",
    story: "Data de emissão no futuro é inválida.",
    problem: "Como validar datas futuras?",
    goldenTip: "💎 Use GT_CURRENT_DATE para o oposto."
  },

  MANN_WHITNEY_U_TEST: {
    name: "MANN_WHITNEY_U_TEST",
    summary: "Teste estatístico Mann-Whitney para diferenças de distribuição",
    syntax: "MANN_WHITNEY_U_TEST(groupA, groupB) PVALUE LT 0.05",
    syntaxExplanation: "Teste não-paramétrico para comparar medianas.",
    story: "Distribuição de valores antes vs depois é diferente.",
    problem: "Como detectar mudança estatística sem assumir normalidade?",
    goldenTip: "💎 Útil quando dados são assimétricos ou têm outliers."
  }
};
