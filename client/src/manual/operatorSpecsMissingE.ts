/**
 * OPERATOR_SPECS_COMPLETE - PARTE 9 (MISSING T-W)
 */

import type { OperatorSpec } from './operatorSpecs';

export const MISSING_SPECS_T_W: Record<string, OperatorSpec> = {
  T_TEST_AMOUNT_DEVIATION: {
    name: "T_TEST_AMOUNT_DEVIATION",
    summary: "Teste t para desvio de valores",
    syntax: "T_TEST_AMOUNT_DEVIATION(currentPeriod, baseline) PVALUE LT 0.05",
    syntaxExplanation: "Compara médias para detectar mudança significativa.",
    story: "Média atual muito maior que a histórica.",
    problem: "Como detectar mudança estatística de valores?",
    goldenTip: "💎 Use quando dados são aproximadamente normais."
  },

  TERMINAL_VERIFICATION_FAILED: {
    name: "TERMINAL_VERIFICATION_FAILED",
    summary: "Detecta falha de verificação do terminal",
    syntax: "TERMINAL_VERIFICATION_FAILED(terminalId) IS_TRUE",
    syntaxExplanation: "Terminal não passou validação de segurança.",
    story: "Terminal não autenticado tenta processar.",
    problem: "Como bloquear terminais não confiáveis?",
    goldenTip: "💎 Terminal não verificado deve ser bloqueado."
  },

  TIME_AFTER: {
    name: "TIME_AFTER",
    summary: "Verifica se horário é depois de outro",
    syntax: "TIME_AFTER(transaction.time, '22:00')",
    syntaxExplanation: "Comparação apenas de hora/minuto.",
    story: "Transações após 22h.",
    problem: "Como aplicar regras por horário?",
    goldenTip: "💎 Combine com TIME_BEFORE para intervalos."
  },

  TIME_ANOMALY: {
    name: "TIME_ANOMALY",
    summary: "Detecta anomalia de horário",
    syntax: "TIME_ANOMALY(customerId, transaction.time) IS_TRUE",
    syntaxExplanation: "Horário fora do perfil do cliente.",
    story: "Cliente só opera de dia; agora opera 3h da manhã.",
    problem: "Como detectar horário incomum?",
    goldenTip: "💎 Use com TIME_DEVIATION_FROM_USUAL."
  },

  TIME_BEFORE: {
    name: "TIME_BEFORE",
    summary: "Verifica se horário é antes de outro",
    syntax: "TIME_BEFORE(transaction.time, '06:00')",
    syntaxExplanation: "Comparação de hora/minuto.",
    story: "Transações antes das 6h.",
    problem: "Como filtrar por horário inicial?",
    goldenTip: "💎 Combine com TIME_AFTER."
  },

  TIME_BETWEEN: {
    name: "TIME_BETWEEN",
    summary: "Verifica se horário está entre dois horários",
    syntax: "TIME_BETWEEN(transaction.time, '22:00', '05:00')",
    syntaxExplanation: "Suporta intervalos que cruzam meia-noite.",
    story: "Transações na madrugada.",
    problem: "Como definir janela de horário?",
    goldenTip: "💎 Intervalos noturnos são mais sensíveis."
  },

  TIME_BETWEEN_CONSECUTIVE_TX: {
    name: "TIME_BETWEEN_CONSECUTIVE_TX",
    summary: "Tempo entre transações consecutivas",
    syntax: "TIME_BETWEEN_CONSECUTIVE_TX(accountId) LT 5",
    syntaxExplanation: "Intervalo mínimo em segundos.",
    story: "Transações a cada 2 segundos.",
    problem: "Como detectar velocidade impossível?",
    goldenTip: "💎 Intervalos < 5s indicam automação."
  },

  TIME_DEVIATION_FROM_USUAL: {
    name: "TIME_DEVIATION_FROM_USUAL",
    summary: "Desvio do horário usual do cliente",
    syntax: "TIME_DEVIATION_FROM_USUAL(customerId, transaction.time) GT 3",
    syntaxExplanation: "Diferença em horas do padrão.",
    story: "Cliente usualmente opera 9-18h; fez 2h.",
    problem: "Como medir desvio temporal?",
    goldenTip: "💎 Desvio grande requer step-up."
  },

  TIME_OF_DAY_ANOMALY: {
    name: "TIME_OF_DAY_ANOMALY",
    summary: "Anomalia no período do dia",
    syntax: "TIME_OF_DAY_ANOMALY(customerId) IS_TRUE",
    syntaxExplanation: "Mudança entre manhã/tarde/noite.",
    story: "Cliente só transaciona pela manhã, agora à noite.",
    problem: "Como detectar mudanças de período do dia?",
    goldenTip: "💎 Use com BUSINESS_HOURS_DEVIATION."
  },

  TIME_PREFERENCE_SHIFT: {
    name: "TIME_PREFERENCE_SHIFT",
    summary: "Mudança na preferência de horário",
    syntax: "TIME_PREFERENCE_SHIFT(customerId) IS_TRUE",
    syntaxExplanation: "Preferência de horário mudou.",
    story: "Cliente migra para operações noturnas.",
    problem: "Como detectar mudança de preferência?",
    goldenTip: "💎 Mudança súbita pode ser ATO."
  },

  TIME_SINCE_LAST_LT: {
    name: "TIME_SINCE_LAST_LT",
    summary: "Tempo desde última transação menor que N",
    syntax: "TIME_SINCE_LAST_LT(customerId, 60)",
    syntaxExplanation: "Segundos desde última transação.",
    story: "Transações com 30s de intervalo.",
    problem: "Como detectar bursts?",
    goldenTip: "💎 Combine com VELOCITY."
  },

  TIMEZONE_MISMATCH: {
    name: "TIMEZONE_MISMATCH",
    summary: "Detecta mismatch de fuso horário",
    syntax: "TIMEZONE_MISMATCH(deviceTZ, geoTZ) IS_TRUE",
    syntaxExplanation: "Fuso do device difere da geolocalização.",
    story: "IP Brasil, timezone Japão.",
    problem: "Como detectar spoofing?",
    goldenTip: "💎 Mismatch forte indica VPN ou falsificação."
  },

  TRADE_BASED_ML_INDICATOR: {
    name: "TRADE_BASED_ML_INDICATOR",
    summary: "Indicador de lavagem baseada em comércio (TBML)",
    syntax: "TRADE_BASED_ML_INDICATOR(transaction) INDICATOR",
    syntaxExplanation: "Sinais de TBML em operações comerciais.",
    story: "Fatura incompatível com preço de mercado.",
    problem: "Como detectar TBML?",
    goldenTip: "💎 Combine com FATF_TBML_* regras."
  },

  TRANSACTION_ATTEMPT_COUNT_PER_CARD: {
    name: "TRANSACTION_ATTEMPT_COUNT_PER_CARD",
    summary: "Conta tentativas de transação por cartão",
    syntax: "TRANSACTION_ATTEMPT_COUNT_PER_CARD(cardId, HOUR_1) GT 5",
    syntaxExplanation: "Inclui aprovadas e negadas.",
    story: "Cartão tenta 6 transações em 10 min.",
    problem: "Como detectar card testing?",
    goldenTip: "💎 Use com CVV_FAILURE_VELOCITY."
  },

  TRANSACTION_COUNT_PER_CARD_HOUR: {
    name: "TRANSACTION_COUNT_PER_CARD_HOUR",
    summary: "Conta transações por cartão por hora",
    syntax: "TRANSACTION_COUNT_PER_CARD_HOUR(cardId) GT 10",
    syntaxExplanation: "Agrega transações na última hora.",
    story: "Cartão fez 12 transações em 1h.",
    problem: "Como limitar frequência por cartão?",
    goldenTip: "💎 Defina limites por segmento."
  },

  TRANSACTION_COUNT_PER_CUSTOMER_HOUR: {
    name: "TRANSACTION_COUNT_PER_CUSTOMER_HOUR",
    summary: "Conta transações por cliente por hora",
    syntax: "TRANSACTION_COUNT_PER_CUSTOMER_HOUR(customerId) GT 15",
    syntaxExplanation: "Agrega transações na última hora.",
    story: "Cliente fez 20 transações em 1h.",
    problem: "Como detectar rajadas por cliente?",
    goldenTip: "💎 Combine com AMOUNT_SUM_PER_CUSTOMER_DAY."
  },

  TRANSACTION_COUNT_PER_DEVICE_DAY: {
    name: "TRANSACTION_COUNT_PER_DEVICE_DAY",
    summary: "Conta transações por device no dia",
    syntax: "TRANSACTION_COUNT_PER_DEVICE_DAY(deviceId) GT 50",
    syntaxExplanation: "Agrega transações em 24h.",
    story: "Device processa 80 transações/dia.",
    problem: "Como detectar devices suspeitos?",
    goldenTip: "💎 Device com muitas contas é risco."
  },

  TRANSACTION_COUNT_PER_IP_HOUR: {
    name: "TRANSACTION_COUNT_PER_IP_HOUR",
    summary: "Conta transações por IP por hora",
    syntax: "TRANSACTION_COUNT_PER_IP_HOUR(ip) GT 30",
    syntaxExplanation: "Agrega transações na última hora.",
    story: "IP faz 60 transações em 1h.",
    problem: "Como detectar abuso por IP?",
    goldenTip: "💎 IP com alta velocidade pode ser bot."
  },

  TRANSACTION_COUNT_PER_MERCHANT_HOUR: {
    name: "TRANSACTION_COUNT_PER_MERCHANT_HOUR",
    summary: "Conta transações por merchant por hora",
    syntax: "TRANSACTION_COUNT_PER_MERCHANT_HOUR(merchantId) GT 500",
    syntaxExplanation: "Agrega volume horário do merchant.",
    story: "Merchant pequeno com 1000 tx/h.",
    problem: "Como detectar spikes de merchant?",
    goldenTip: "💎 Spike + merchant novo = risco alto."
  },

  TRANSACTION_FREQUENCY_ANOMALY: {
    name: "TRANSACTION_FREQUENCY_ANOMALY",
    summary: "Detecta anomalia de frequência",
    syntax: "TRANSACTION_FREQUENCY_ANOMALY(customerId) IS_TRUE",
    syntaxExplanation: "Frequência fora do padrão histórico.",
    story: "Cliente sai de 1/dia para 20/dia.",
    problem: "Como detectar mudança de frequência?",
    goldenTip: "💎 Compare com baseline individual."
  },

  TRANSACTION_SIZE_ESCALATION: {
    name: "TRANSACTION_SIZE_ESCALATION",
    summary: "Detecta escalada do tamanho das transações",
    syntax: "TRANSACTION_SIZE_ESCALATION(customerId) IS_TRUE",
    syntaxExplanation: "Valores crescem progressivamente.",
    story: "R$ 200 → 500 → 1500 em sequência.",
    problem: "Como detectar escalada?",
    goldenTip: "💎 Escalada é típica de fraude em progressão."
  },

  TRANSACTION_TIMING_CLUSTER: {
    name: "TRANSACTION_TIMING_CLUSTER",
    summary: "Detecta cluster temporal de transações",
    syntax: "TRANSACTION_TIMING_CLUSTER(customerId) IS_TRUE",
    syntaxExplanation: "Transações se agrupam em janelas curtas.",
    story: "20 transações entre 14:00-14:05.",
    problem: "Como detectar burst?",
    goldenTip: "💎 Cluster temporal indica automação."
  },

  TRANSFER_AMOUNT_GT: {
    name: "TRANSFER_AMOUNT_GT",
    summary: "Verifica se valor de transferência excede limite",
    syntax: "TRANSFER_AMOUNT_GT(amount, 10000)",
    syntaxExplanation: "Retorna true se valor > limite.",
    story: "Transferência de R$ 50k.",
    problem: "Como impor limites?",
    goldenTip: "💎 Limites variam por perfil e canal."
  },

  UETR_DUPLICATE_CHECK: {
    name: "UETR_DUPLICATE_CHECK",
    summary: "Detecta UETR duplicado em pagamentos",
    syntax: "UETR_DUPLICATE_CHECK(uetr) IS_DUPLICATE",
    syntaxExplanation: "UETR deve ser único em pagamentos SWIFT.",
    story: "Mesmo UETR reutilizado para outra transação.",
    problem: "Como detectar duplicidades SWIFT?",
    goldenTip: "💎 UETR duplicado pode indicar replay."
  },

  UNIQUE_CARD_COUNT_PER_IP_HOUR: {
    name: "UNIQUE_CARD_COUNT_PER_IP_HOUR",
    summary: "Conta cartões únicos por IP por hora",
    syntax: "UNIQUE_CARD_COUNT_PER_IP_HOUR(ip) GT 5",
    syntaxExplanation: "Muitos cartões em um IP.",
    story: "IP tenta 10 cartões em 1h.",
    problem: "Como detectar card testing por IP?",
    goldenTip: "💎 Combine com CVV_FAILURE_VELOCITY."
  },

  UNIQUE_MERCHANT_COUNT_PER_CARD_DAY: {
    name: "UNIQUE_MERCHANT_COUNT_PER_CARD_DAY",
    summary: "Conta merchants únicos por cartão por dia",
    syntax: "UNIQUE_MERCHANT_COUNT_PER_CARD_DAY(cardId) GT 10",
    syntaxExplanation: "Muitas lojas diferentes no mesmo dia.",
    story: "Cartão usado em 15 merchants em 24h.",
    problem: "Como detectar spree?",
    goldenTip: "💎 Spree = alta diversidade em pouco tempo."
  },

  UNUSUAL_BUSINESS_PATTERN: {
    name: "UNUSUAL_BUSINESS_PATTERN",
    summary: "Detecta padrão incomum de negócio",
    syntax: "UNUSUAL_BUSINESS_PATTERN(company) IS_TRUE",
    syntaxExplanation: "Atividade não condiz com perfil declarado.",
    story: "Empresa de TI recebendo grandes depósitos em cash.",
    problem: "Como detectar inconsistências de negócio?",
    goldenTip: "💎 Mismatch com CNAE/segmento é sinal forte."
  },

  UNUSUAL_CARD_MEDIA: {
    name: "UNUSUAL_CARD_MEDIA",
    summary: "Detecta mídia de cartão incomum",
    syntax: "UNUSUAL_CARD_MEDIA(card) IS_TRUE",
    syntaxExplanation: "Ex: cartão virtual usado em POS físico.",
    story: "Cartão virtual em compra presencial.",
    problem: "Como detectar uso indevido?",
    goldenTip: "💎 Combine com POS_SECURITY_MISSING."
  },

  VELOCITY: {
    name: "VELOCITY",
    summary: "Operador genérico de velocidade",
    syntax: "VELOCITY(entity, WINDOW) GT threshold",
    syntaxExplanation: "Conta eventos em janela e compara.",
    story: "10 eventos em 1h.",
    problem: "Como aplicar regra de velocidade genérica?",
    goldenTip: "💎 Prefira operadores específicos quando existirem."
  },

  VELOCITY_ACCELERATION: {
    name: "VELOCITY_ACCELERATION",
    summary: "Detecta aceleração na velocidade",
    syntax: "VELOCITY_ACCELERATION(entity) IS_TRUE",
    syntaxExplanation: "Velocidade aumenta rapidamente.",
    story: "Passa de 1 tx/h para 20 tx/h.",
    problem: "Como detectar aceleração?",
    goldenTip: "💎 Aceleração indica comportamento anômalo."
  },

  VELOCITY_ANOMALY: {
    name: "VELOCITY_ANOMALY",
    summary: "Detecta anomalia na velocidade",
    syntax: "VELOCITY_ANOMALY(entity) IS_TRUE",
    syntaxExplanation: "Velocidade fora do padrão histórico.",
    story: "Cliente fazia 2/dia e passou a 50/dia.",
    problem: "Como detectar velocidade anormal?",
    goldenTip: "💎 Combine com AMOUNT_SUM_PER_CUSTOMER_DAY."
  },

  VELOCITY_CROSS_CHANNEL: {
    name: "VELOCITY_CROSS_CHANNEL",
    summary: "Velocidade cruzada entre canais",
    syntax: "VELOCITY_CROSS_CHANNEL(customerId, HOUR_1) GT 5",
    syntaxExplanation: "Múltiplas transações em canais diferentes.",
    story: "App + web + call center em minutos.",
    problem: "Como detectar uso cruzado suspeito?",
    goldenTip: "💎 Cross-channel rápido indica ATO."
  },

  VELOCITY_PERCENTILE: {
    name: "VELOCITY_PERCENTILE",
    summary: "Compara velocidade com percentil histórico",
    syntax: "VELOCITY_PERCENTILE(entity, 95) EXCEEDED",
    syntaxExplanation: "Velocidade acima do percentil 95.",
    story: "Velocidade atual está no top 1%.",
    problem: "Como usar percentis para velocidade?",
    goldenTip: "💎 Percentis reduzem impacto de outliers."
  },

  VELOCITY_RATIO_GT: {
    name: "VELOCITY_RATIO_GT",
    summary: "Verifica se razão de velocidade excede limite",
    syntax: "VELOCITY_RATIO_GT(currentRate, baselineRate, 3)",
    syntaxExplanation: "Velocidade atual > 3x baseline.",
    story: "Velocidade triplicou em 1h.",
    problem: "Como medir crescimento relativo?",
    goldenTip: "💎 Use ratio para detectar spikes rápidos."
  },

  VELOCITY_ROLLING_WINDOW: {
    name: "VELOCITY_ROLLING_WINDOW",
    summary: "Velocidade em janela móvel",
    syntax: "VELOCITY_ROLLING_WINDOW(entity, MINUTE_5) GT 10",
    syntaxExplanation: "Janela móvel para detectar bursts.",
    story: "10 eventos em 5 minutos.",
    problem: "Como detectar rajadas instantâneas?",
    goldenTip: "💎 Rolling window captura picos rápidos."
  },

  VELOCITY_SPIKE: {
    name: "VELOCITY_SPIKE",
    summary: "Detecta spike de velocidade",
    syntax: "VELOCITY_SPIKE(entity) IS_TRUE",
    syntaxExplanation: "Aumento súbito da velocidade.",
    story: "0→30 transações em minutos.",
    problem: "Como detectar spikes?",
    goldenTip: "💎 Combine com VELOCITY_TREND."
  },

  VELOCITY_TREND: {
    name: "VELOCITY_TREND",
    summary: "Detecta tendência de aumento de velocidade",
    syntax: "VELOCITY_TREND(entity, DAY_7) UP",
    syntaxExplanation: "Tendência crescente ao longo do tempo.",
    story: "Velocidade aumenta dia após dia.",
    problem: "Como detectar tendências?",
    goldenTip: "💎 Tendência positiva pode indicar escalada."
  },

  WEEKLY_LIMIT_PROXIMITY: {
    name: "WEEKLY_LIMIT_PROXIMITY",
    summary: "Verifica proximidade do limite semanal",
    syntax: "WEEKLY_LIMIT_PROXIMITY(accountId) GT 0.9",
    syntaxExplanation: "Consumo de 90% do limite semanal.",
    story: "Conta atinge limite semanal em 2 dias.",
    problem: "Como monitorar limites semanais?",
    goldenTip: "💎 Use alertas em 80% para prevenir bloqueios."
  }
};
