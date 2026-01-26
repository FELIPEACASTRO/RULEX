/**
 * OPERATOR_SPECS_COMPLETE - PARTE 3
 * Continuação da documentação ULTRA DIDÁTICA
 * 
 * STATISTICAL, DATE/TIME, MERCHANT/MCC, TRANSACTION, SCA/PSD3
 */

import type { OperatorSpec } from './operatorSpecs';

// ═══════════════════════════════════════════════════════════════════════════════════════
// PARTE 12: OPERADORES ESTATÍSTICOS (15 operadores)
// ═══════════════════════════════════════════════════════════════════════════════════════

export const STATISTICAL_SPECS: Record<string, OperatorSpec> = {
  Z_SCORE_GT: {
    name: "Z_SCORE_GT",
    summary: "Verifica se valor está N desvios padrão ACIMA da média (outlier)",
    syntax: "Z_SCORE(amount, customerHistory) GT 2",
    syntaxExplanation: "Z-score = (valor - média) / desvio_padrão. Z > 2 = muito acima da média (~2.3% probabilidade).",
    story: "Cliente com média de R$ 100/TX faz TX de R$ 800. Z-score = 3.5 = outlier extremo.",
    problem: "Como detectar valores que são OUTLIERS estatísticos?",
    goldenTip: "💎 Interpretação:\n• Z = 1: 32% das TXs são maiores (comum)\n• Z = 2: 2.3% das TXs são maiores (raro)\n• Z = 3: 0.1% das TXs são maiores (muito raro)\n• Z > 3: outlier extremo",
    engineBehavior: {
      description: "Cálculo de Z-score:",
      steps: [
        "1. Carrega histórico de valores do cliente",
        "2. Calcula média (μ) e desvio padrão (σ)",
        "3. Z = (valor_atual - μ) / σ",
        "4. Compara Z > threshold"
      ],
      cautions: ["Precisa de histórico mínimo (>20 TXs) para ser estatisticamente válido"]
    }
  },

  STANDARD_DEVIATION_GT: {
    name: "STANDARD_DEVIATION_GT",
    summary: "Verifica se DESVIO PADRÃO de valores está acima do limite",
    syntax: "STANDARD_DEVIATION(customerId, DAY_7, amount) GT 500",
    syntaxExplanation: "Desvio padrão alto = valores muito variados. Cliente com TXs de R$ 10 e R$ 10.000 = desvio alto.",
    story: "Cliente estável tinha desvio de R$ 50. De repente, desvio de R$ 2.000 = mudança de padrão.",
    problem: "Como detectar VARIABILIDADE excessiva nos valores?",
    goldenTip: "💎 Desvio padrão baixo = cliente previsível. Desvio alto + conta nova = alto risco."
  },

  BENFORD_LAW_DEVIATION: {
    name: "BENFORD_LAW_DEVIATION",
    summary: "Detecta DESVIO da Lei de Benford (distribuição de primeiros dígitos)",
    syntax: "BENFORD_LAW_DEVIATION(transactions, amount) GT 0.1",
    syntaxExplanation: "Lei de Benford: em dados naturais, ~30% começam com 1, ~18% com 2, etc. Fraudadores não sabem disso.",
    story: "Notas fiscais fraudadas tinham 15% começando com 1 (deveria ser 30%) = manipulação.",
    problem: "Como detectar dados fabricados/manipulados?",
    goldenTip: "💎 Lei de Benford:\n• 1: 30.1%\n• 2: 17.6%\n• 3: 12.5%\n• ...\n• 9: 4.6%\nDesvio grande = dados fabricados!",
    engineBehavior: {
      description: "Análise de Benford:",
      steps: [
        "1. Extrai primeiro dígito de cada valor",
        "2. Calcula distribuição observada",
        "3. Compara com distribuição teórica de Benford",
        "4. Calcula chi-square ou MAD (Mean Absolute Deviation)",
        "5. Se desvio > threshold → dados suspeitos"
      ],
      performance: "Precisa de pelo menos 100 valores para análise confiável"
    },
    realScenarios: [
      {
        title: "Detecção de Notas Fiscais Falsas",
        context: "Empresa com 10.000 notas fiscais de fornecedores",
        problem: "Suspeita de notas frias misturadas com legítimas",
        solution: "BENFORD_LAW_DEVIATION detectou desvio de 0.25 (esperado < 0.05)",
        impact: "Identificou 340 notas fraudulentas. Economia: R$ 2.3M em impostos"
      }
    ]
  },

  CHI_SQUARE_DISTRIBUTION_TEST: {
    name: "CHI_SQUARE_DISTRIBUTION_TEST",
    summary: "Teste estatístico CHI-QUADRADO para verificar distribuição",
    syntax: "CHI_SQUARE_DISTRIBUTION_TEST(transactions, expectedDistribution) PVALUE LT 0.05",
    syntaxExplanation: "Testa se distribuição observada difere significativamente da esperada. P < 0.05 = diferença significativa.",
    story: "Transações deveriam estar uniformes por hora. Chi-square mostrou pico às 3h = bot.",
    problem: "Como testar se distribuição é 'normal' ou 'esperada'?",
    goldenTip: "💎 P-value:\n• > 0.05 = distribuição OK\n• < 0.05 = distribuição suspeita\n• < 0.01 = muito suspeita"
  },

  KOLMOGOROV_SMIRNOV_TEST: {
    name: "KOLMOGOROV_SMIRNOV_TEST",
    summary: "Teste de KOLMOGOROV-SMIRNOV para comparar distribuições",
    syntax: "KOLMOGOROV_SMIRNOV_TEST(currentPeriod, historicalPeriod) PVALUE LT 0.05",
    syntaxExplanation: "Compara se duas distribuições são iguais. P < 0.05 = diferente (padrão mudou).",
    story: "Distribuição de valores esse mês vs mês passado = muito diferente = algo mudou.",
    problem: "Como detectar MUDANÇA DE PADRÃO estatisticamente?",
    goldenTip: "💎 KS-test é 'non-parametric' - funciona com qualquer distribuição, não só normal."
  },

  ANDERSON_DARLING_TEST: {
    name: "ANDERSON_DARLING_TEST",
    summary: "Teste de ANDERSON-DARLING para normalidade",
    syntax: "ANDERSON_DARLING_TEST(transactions, amount) NORMAL_DISTRIBUTION_REJECTED",
    syntaxExplanation: "Testa se dados seguem distribuição normal. Rejeição = dados não são normais.",
    story: "Valores de TX deveriam ser normais. Anderson-Darling rejeitou = há outliers/manipulação.",
    problem: "Como verificar se dados seguem distribuição normal esperada?",
    goldenTip: "💎 Anderson-Darling é mais sensível a caudas que KS-test. Melhor para detectar outliers."
  },

  T_TEST_SIGNIFICANT_CHANGE: {
    name: "T_TEST_SIGNIFICANT_CHANGE",
    summary: "Teste T de STUDENT para mudança significativa de média",
    syntax: "T_TEST_SIGNIFICANT_CHANGE(currentWeek, lastWeek, amount) IS_TRUE",
    syntaxExplanation: "Compara média de dois períodos. Mudança estatisticamente significativa?",
    story: "Média essa semana = R$ 500. Semana passada = R$ 100. T-test: p < 0.001 = mudança real.",
    problem: "Como saber se mudança de média é REAL ou variação aleatória?",
    goldenTip: "💎 T-test requer:\n• Pelo menos 30 amostras por período\n• Dados aproximadamente normais"
  },

  VARIANCE_RATIO_TEST: {
    name: "VARIANCE_RATIO_TEST",
    summary: "Teste de RAZÃO DE VARIÂNCIA (F-test)",
    syntax: "VARIANCE_RATIO_TEST(groupA, groupB) VARIANCE_DIFFERENT",
    syntaxExplanation: "Compara se variabilidade de dois grupos é diferente.",
    story: "Cliente tinha variação baixa (TXs consistentes). Agora variação alta = conta comprometida?",
    problem: "Como detectar mudança na VARIABILIDADE do comportamento?",
    goldenTip: "💎 Aumento de variância = comportamento errático = possível compromisso de conta."
  },

  SKEWNESS_ANOMALY: {
    name: "SKEWNESS_ANOMALY",
    summary: "Detecta ASSIMETRIA anormal na distribuição de valores",
    syntax: "SKEWNESS_ANOMALY(transactions, amount) GT 2",
    syntaxExplanation: "Skewness mede 'inclinação' da distribuição. > 2 = muito assimétrica.",
    story: "Distribuição de TXs deveria ser simétrica. Skewness 4 = muitos outliers altos.",
    problem: "Como detectar distribuição 'puxada' para um lado?",
    goldenTip: "💎 Skewness:\n• 0 = simétrica\n• > 0 = cauda à direita (valores altos)\n• < 0 = cauda à esquerda (valores baixos)"
  },

  KURTOSIS_ANOMALY: {
    name: "KURTOSIS_ANOMALY",
    summary: "Detecta CURTOSE anormal (concentração nos extremos)",
    syntax: "KURTOSIS_ANOMALY(transactions, amount) GT 3",
    syntaxExplanation: "Kurtosis mede 'peakedness'. > 3 = caudas pesadas (muitos extremos).",
    story: "Distribuição com kurtosis 7 = muitas TXs nos extremos (muito baixo ou muito alto).",
    problem: "Como detectar excesso de valores extremos?",
    goldenTip: "💎 Kurtosis:\n• = 3 = normal\n• > 3 = leptocúrtica (pico + caudas pesadas)\n• < 3 = platocúrtica (achatada)"
  },

  GRUBBS_OUTLIER_TEST: {
    name: "GRUBBS_OUTLIER_TEST",
    summary: "Teste de GRUBBS para detectar outlier único",
    syntax: "GRUBBS_OUTLIER_TEST(currentValue, historicalValues) IS_OUTLIER",
    syntaxExplanation: "Determina estatisticamente se valor específico é outlier.",
    story: "TX de R$ 50.000 em conta que só fez R$ 100-500. Grubbs: outlier confirmado.",
    problem: "Como ter CERTEZA estatística de que valor é outlier?",
    goldenTip: "💎 Grubbs é conservador - só detecta outliers EXTREMOS. Bom para não ter falsos positivos."
  },

  PERCENTILE_GT: {
    name: "PERCENTILE_GT",
    summary: "Verifica se valor está acima de determinado PERCENTIL",
    syntax: "PERCENTILE_GT(amount, customerHistory, 95)",
    syntaxExplanation: "Valor acima do percentil 95 = maior que 95% das TXs históricas.",
    story: "R$ 5.000 está no percentil 99 para esse cliente = top 1% das suas TXs.",
    problem: "Como comparar valor com histórico usando percentis?",
    goldenTip: "💎 Percentis são mais robustos que média/desvio para dados não-normais."
  },

  INTERQUARTILE_RANGE_OUTLIER: {
    name: "INTERQUARTILE_RANGE_OUTLIER",
    summary: "Detecta outlier pelo método IQR (Interquartile Range)",
    syntax: "INTERQUARTILE_RANGE_OUTLIER(amount, customerHistory) IS_TRUE",
    syntaxExplanation: "Outlier = fora de [Q1 - 1.5*IQR, Q3 + 1.5*IQR]. Método clássico de boxplot.",
    story: "IQR = Q3 - Q1. Valor > Q3 + 1.5*IQR = outlier por definição clássica.",
    problem: "Como detectar outliers de forma robusta a distribuições não-normais?",
    goldenTip: "💎 IQR é resistente a outliers existentes (diferente da média que é 'puxada')."
  },

  AMOUNT_DEVIATION_FROM_AVG: {
    name: "AMOUNT_DEVIATION_FROM_AVG",
    summary: "Calcula DESVIO percentual do valor em relação à média",
    syntax: "AMOUNT_DEVIATION_FROM_AVG(amount, customerAvg) GT 200",
    syntaxExplanation: "Desvio > 200% = valor é mais de 3x a média. (valor - média) / média * 100.",
    story: "Média R$ 100, valor R$ 350 = desvio 250% = muito acima do normal.",
    problem: "Como medir quão 'longe' da média um valor está?",
    goldenTip: "💎 Desvio %:\n• < 50%: normal\n• 50-100%: acima do comum\n• > 200%: muito fora do padrão"
  },

  REAL_TIME_RISK_SCORING: {
    name: "REAL_TIME_RISK_SCORING",
    summary: "Score de risco calculado em TEMPO REAL",
    syntax: "REAL_TIME_RISK_SCORING(transaction) GT 70",
    syntaxExplanation: "Combina múltiplas features estatísticas em score único 0-100.",
    story: "TX com score 85: Z-score alto + device novo + horário anormal = alto risco.",
    problem: "Como ter uma métrica ÚNICA de risco em tempo real?",
    goldenTip: "💎 Risk scoring combina:\n• Velocidade\n• Valor\n• Device\n• Geo\n• Comportamento\n• Histórico"
  }
};

// ═══════════════════════════════════════════════════════════════════════════════════════
// PARTE 13: OPERADORES DATE/TIME (15 operadores)
// ═══════════════════════════════════════════════════════════════════════════════════════

export const DATE_TIME_SPECS: Record<string, OperatorSpec> = {
  DATE_EQ: {
    name: "DATE_EQ",
    summary: "Verifica se data é IGUAL a uma data específica",
    syntax: "transaction.date DATE_EQ \"2024-01-01\"",
    syntaxExplanation: "Compara apenas a data (ignora hora). 2024-01-01 10:00 DATE_EQ 2024-01-01 = TRUE.",
    story: "Buscar todas as transações do Ano Novo.",
    problem: "Como filtrar por data específica?",
    goldenTip: "💎 DATE_EQ ignora hora. Para comparar data+hora, use TIMESTAMP_EQ."
  },

  DATE_GT: {
    name: "DATE_GT",
    summary: "Verifica se data é POSTERIOR a uma data específica",
    syntax: "customer.createdAt DATE_GT \"2024-01-01\"",
    syntaxExplanation: "Clientes criados DEPOIS de 01/01/2024.",
    story: "Analisar apenas clientes novos (criados no último mês).",
    problem: "Como filtrar por datas posteriores?",
    goldenTip: "💎 DATE_GT não inclui a data. Para incluir, use DATE_GTE."
  },

  DATE_LT: {
    name: "DATE_LT",
    summary: "Verifica se data é ANTERIOR a uma data específica",
    syntax: "card.expiryDate DATE_LT \"2024-06-01\"",
    syntaxExplanation: "Cartões que expiram ANTES de 01/06/2024.",
    story: "Identificar cartões prestes a vencer.",
    problem: "Como filtrar por datas anteriores?",
    goldenTip: "💎 Útil para verificar validade: 'expiryDate DATE_LT TODAY' = cartão vencido."
  },

  DATE_BETWEEN: {
    name: "DATE_BETWEEN",
    summary: "Verifica se data está em um INTERVALO",
    syntax: "transaction.date DATE_BETWEEN \"2024-01-01\" AND \"2024-01-31\"",
    syntaxExplanation: "Transações de janeiro de 2024 (inclusivo em ambos lados).",
    story: "Relatório mensal: todas as TXs do mês.",
    problem: "Como filtrar por período (data inicial e final)?",
    goldenTip: "💎 BETWEEN é inclusivo: 01/01 e 31/01 estão incluídos."
  },

  TIME_OF_DAY_BETWEEN: {
    name: "TIME_OF_DAY_BETWEEN",
    summary: "Verifica se HORA DO DIA está em um intervalo",
    syntax: "transaction.time TIME_OF_DAY_BETWEEN \"22:00\" AND \"05:00\"",
    syntaxExplanation: "Transações entre 22h e 5h (madrugada). Note que cruza meia-noite!",
    story: "Transações de madrugada são mais arriscadas.",
    problem: "Como identificar transações em horários específicos?",
    goldenTip: "💎 Cuidado com intervalos que cruzam meia-noite! 22:00-05:00 inclui madrugada."
  },

  DAY_OF_WEEK_EQ: {
    name: "DAY_OF_WEEK_EQ",
    summary: "Verifica se é um DIA DA SEMANA específico",
    syntax: "transaction.date DAY_OF_WEEK_EQ \"SUNDAY\"",
    syntaxExplanation: "Transações aos domingos. Valores: MONDAY, TUESDAY, ..., SUNDAY.",
    story: "TXs de domingo são incomuns para B2B.",
    problem: "Como filtrar por dia da semana?",
    goldenTip: "💎 B2B aos domingos = suspeito. Pessoa física é normal."
  },

  DAY_OF_MONTH_EQ: {
    name: "DAY_OF_MONTH_EQ",
    summary: "Verifica se é um DIA DO MÊS específico",
    syntax: "transaction.date DAY_OF_MONTH_EQ 25",
    syntaxExplanation: "Transações no dia 25 (provavelmente pagamento de salário).",
    story: "Pico de TXs no dia 5 e 25 (pagamentos).",
    problem: "Como filtrar por dia específico do mês?",
    goldenTip: "💎 Dias 1-10: pós-salário = TXs maiores são esperadas."
  },

  WEEKEND_VS_WEEKDAY_PATTERN: {
    name: "WEEKEND_VS_WEEKDAY_PATTERN",
    summary: "Compara padrão de FIM DE SEMANA vs DIA DE SEMANA",
    syntax: "WEEKEND_VS_WEEKDAY_PATTERN(customerId) ANOMALY_DETECTED",
    syntaxExplanation: "Cliente que só gasta no fim de semana agora gasta segunda = mudança.",
    story: "Empregado doméstico só recebe sábado. TX sexta = cartão roubado?",
    problem: "Como detectar mudança no padrão semanal?",
    goldenTip: "💎 Cada cliente tem ritmo. Mudança súbita = investigar."
  },

  TIME_ZONE_OFFSET_MISMATCH: {
    name: "TIME_ZONE_OFFSET_MISMATCH",
    summary: "Detecta incompatibilidade de FUSO HORÁRIO",
    syntax: "TIME_ZONE_OFFSET_MISMATCH() IS_TRUE",
    syntaxExplanation: "Device em UTC-3 (Brasil) mas TX marcada como UTC+9 (Japão).",
    story: "Fraudador esqueceu de ajustar timezone do sistema.",
    problem: "Como detectar spoofing de timezone?",
    goldenTip: "💎 Timezone leak: verificar device timezone vs IP geolocation vs horário declarado."
  },

  BUSINESS_HOURS_CHECK: {
    name: "BUSINESS_HOURS_CHECK",
    summary: "Verifica se TX está em HORÁRIO COMERCIAL",
    syntax: "BUSINESS_HOURS_CHECK(transaction.time) IS_TRUE",
    syntaxExplanation: "9h-18h seg-sex = TRUE. Fora disso = FALSE.",
    story: "TX B2B às 3h de sábado = muito suspeito.",
    problem: "Como identificar TXs fora do horário comercial?",
    goldenTip: "💎 Horário comercial varia por país! Configure por região."
  },

  HOLIDAY_CHECK: {
    name: "HOLIDAY_CHECK",
    summary: "Verifica se TX é em FERIADO",
    syntax: "HOLIDAY_CHECK(transaction.date, \"BR\") IS_TRUE",
    syntaxExplanation: "Verifica se data é feriado no país especificado.",
    story: "TX corporativa no Natal = suspeito (empresa fechada).",
    problem: "Como detectar TXs em feriados?",
    goldenTip: "💎 Mantenha calendário de feriados atualizado por país."
  },

  TIMESTAMP_SEQUENCE_ANOMALY: {
    name: "TIMESTAMP_SEQUENCE_ANOMALY",
    summary: "Detecta anomalia na SEQUÊNCIA de timestamps",
    syntax: "TIMESTAMP_SEQUENCE_ANOMALY(transactions) IS_TRUE",
    syntaxExplanation: "TXs com timestamps fora de ordem ou muito regulares = bot.",
    story: "10 TXs exatamente a cada 5.000ms = bot automatizado.",
    problem: "Como detectar automação por padrão de tempo?",
    goldenTip: "💎 Humanos têm variação natural. Bots são muito regulares (ou irreais)."
  },

  TRANSACTION_TIME_CONSISTENCY: {
    name: "TRANSACTION_TIME_CONSISTENCY",
    summary: "Verifica CONSISTÊNCIA de horário com padrão do cliente",
    syntax: "TRANSACTION_TIME_CONSISTENCY(customerId, transaction.time) SCORE LT 0.3",
    syntaxExplanation: "Score baixo = horário inconsistente com histórico do cliente.",
    story: "Cliente sempre opera 9-17h. TX às 3h tem score 0.1 = muito inconsistente.",
    problem: "Como medir quão 'normal' é o horário para cada cliente?",
    goldenTip: "💎 Cada cliente tem perfil temporal. Desvio = alerta."
  },

  TIMESTAMP_GAP_ANALYSIS: {
    name: "TIMESTAMP_GAP_ANALYSIS",
    summary: "Analisa GAPS (intervalos) entre transações",
    syntax: "TIMESTAMP_GAP_ANALYSIS(transactions) MIN_GAP_SECONDS LT 5",
    syntaxExplanation: "Se intervalo mínimo < 5 segundos = TXs muito rápidas (bot).",
    story: "5 TXs em 10 segundos = impossível para humano.",
    problem: "Como detectar velocidade impossível de transações?",
    goldenTip: "💎 Humano precisa de pelo menos 10-15s para fazer TX completa."
  },

  EXPIRED_CARD: {
    name: "EXPIRED_CARD",
    summary: "Verifica se cartão está VENCIDO",
    syntax: "EXPIRED_CARD() IS_TRUE",
    syntaxExplanation: "Data de expiração < data atual = cartão vencido.",
    story: "Tentativa de uso de cartão vencido há 6 meses.",
    problem: "Como validar se cartão ainda é válido?",
    goldenTip: "💎 Cartão vencido deve ser rejeitado pelo emissor, mas verificar também no sistema."
  }
};

// ═══════════════════════════════════════════════════════════════════════════════════════
// PARTE 14: OPERADORES MERCHANT/MCC (20 operadores)
// ═══════════════════════════════════════════════════════════════════════════════════════

export const MERCHANT_SPECS: Record<string, OperatorSpec> = {
  MERCHANT_RISK_SCORE_GT: {
    name: "MERCHANT_RISK_SCORE_GT",
    summary: "Verifica se SCORE DE RISCO do merchant está acima do limite",
    syntax: "MERCHANT_RISK_SCORE_GT(merchantId, 70)",
    syntaxExplanation: "Merchant com score 85 = alto risco (histórico de chargebacks, fraudes).",
    story: "Merchant novo com taxa de chargeback de 15% = score 95 = altíssimo risco.",
    problem: "Como avaliar risco de comprar em determinado merchant?",
    goldenTip: "💎 Score considera:\n• Taxa de chargeback\n• Histórico de fraude\n• Tempo de atividade\n• Volume"
  },

  MERCHANT_FIRST_TRANSACTION: {
    name: "MERCHANT_FIRST_TRANSACTION",
    summary: "Verifica se é a PRIMEIRA transação do cliente com este merchant",
    syntax: "MERCHANT_FIRST_TRANSACTION(customerId, merchantId) IS_TRUE",
    syntaxExplanation: "Primeira compra do cliente nessa loja = TRUE.",
    story: "Primeira compra em joalheria de luxo + valor alto = alto risco.",
    problem: "Como identificar primeiras compras em merchants novos?",
    goldenTip: "💎 Primeiro merchant + valor alto + device novo = tríplice suspeita."
  },

  MERCHANT_CATEGORY_CHANGE: {
    name: "MERCHANT_CATEGORY_CHANGE",
    summary: "Detecta MUDANÇA de categoria de merchant vs histórico",
    syntax: "MERCHANT_CATEGORY_CHANGE(customerId) IS_TRUE",
    syntaxExplanation: "Cliente só comprava em supermercado, agora compra em joalheria.",
    story: "Padrão estável por 1 ano. De repente, categorias completamente diferentes.",
    problem: "Como detectar mudança de padrão de compras?",
    goldenTip: "💎 Mudança de categoria não é sempre fraude. Pode ser presente, viagem. Contextualize."
  },

  MCC_HIGH_RISK: {
    name: "MCC_HIGH_RISK",
    summary: "Verifica se MCC (Merchant Category Code) é de ALTO RISCO",
    syntax: "MCC_HIGH_RISK(transaction.mcc) IS_TRUE",
    syntaxExplanation: "MCCs como 7995 (gambling), 5967 (adult), 5912 (drugs) = alto risco.",
    story: "TX em MCC 7995 (apostas online) = regras especiais aplicam.",
    problem: "Como identificar categorias de risco?",
    goldenTip: "💎 MCCs de risco:\n• 7995: Gambling\n• 5967: Adult content\n• 6211: Securities/Crypto\n• 4829: Wire transfer",
    engineBehavior: {
      description: "Consulta lista de MCCs de risco:",
      steps: [
        "1. Extrai MCC da transação",
        "2. Consulta lista de MCCs de alto risco",
        "3. Retorna TRUE se está na lista"
      ]
    }
  },

  MCC_MISMATCH: {
    name: "MCC_MISMATCH",
    summary: "Detecta INCOMPATIBILIDADE entre MCC e tipo de produto",
    syntax: "MCC_MISMATCH(transaction.mcc, transaction.productType) IS_TRUE",
    syntaxExplanation: "MCC de farmácia mas vendendo eletrônicos = mismatch.",
    story: "Merchant registrado como farmácia vendendo iPhone = fraude de MCC.",
    problem: "Como detectar merchant usando MCC errado para fugir de regras?",
    goldenTip: "💎 Merchants usam MCCs de baixo risco para processar produtos de alto risco."
  },

  MCC_VELOCITY: {
    name: "MCC_VELOCITY",
    summary: "Conta transações em determinado MCC em janela de tempo",
    syntax: "MCC_VELOCITY(customerId, \"7995\", HOUR_24) GT 5",
    syntaxExplanation: "Mais de 5 TXs em gambling em 24h = possível vício ou fraude.",
    story: "Cliente fez 20 depósitos em cassino online em 24h.",
    problem: "Como limitar TXs por categoria?",
    goldenTip: "💎 Limites por MCC:\n• Gambling: max 3/dia\n• Crypto: max 2/dia\n• Adult: max 1/dia"
  },

  MCC_AMOUNT_LIMIT: {
    name: "MCC_AMOUNT_LIMIT",
    summary: "Verifica se valor excede limite para determinado MCC",
    syntax: "MCC_AMOUNT_LIMIT(transaction.mcc, transaction.amount) EXCEEDED",
    syntaxExplanation: "TX de R$ 50k em MCC de fast food = impossível (limite R$ 500).",
    story: "McDonalds processando TX de R$ 10.000 = merchant fraud.",
    problem: "Como validar valores plausíveis por categoria?",
    goldenTip: "💎 Limites por MCC:\n• Fast food: R$ 500\n• Eletrônicos: R$ 50k\n• Supermercado: R$ 5k"
  },

  MERCHANT_COUNTRY_MISMATCH: {
    name: "MERCHANT_COUNTRY_MISMATCH",
    summary: "Detecta incompatibilidade entre país do merchant e da transação",
    syntax: "MERCHANT_COUNTRY_MISMATCH() IS_TRUE",
    syntaxExplanation: "Merchant registrado no Brasil mas TX marcada como EUA.",
    story: "Loja física em SP processando como se fosse Miami.",
    problem: "Como detectar merchant fraudando localização?",
    goldenTip: "💎 Merchant deve processar no país onde está. Cross-border deve ser explícito."
  },

  MERCHANT_TERMINAL_MISMATCH: {
    name: "MERCHANT_TERMINAL_MISMATCH",
    summary: "Detecta incompatibilidade entre terminal e tipo de transação",
    syntax: "MERCHANT_TERMINAL_MISMATCH() IS_TRUE",
    syntaxExplanation: "Terminal de POS físico mas TX marcada como e-commerce.",
    story: "Maquininha processando como se fosse site = possível fraude.",
    problem: "Como validar consistência terminal/canal?",
    goldenTip: "💎 POS físico = card present. E-commerce = card not present. Mistura = suspeito."
  },

  MERCHANT_VOLUME_SPIKE: {
    name: "MERCHANT_VOLUME_SPIKE",
    summary: "Detecta PICO de volume no merchant",
    syntax: "MERCHANT_VOLUME_SPIKE(merchantId) IS_TRUE",
    syntaxExplanation: "Merchant que processava R$ 10k/dia de repente processa R$ 500k = spike.",
    story: "Merchant novo com spike de 5000% em volume = possível bust-out.",
    problem: "Como detectar merchants processando volume anormal?",
    goldenTip: "💎 Spike + merchant novo + MCCde alto risco = alto risco de bust-out."
  },

  MERCHANT_CHARGEBACK_RATE_GT: {
    name: "MERCHANT_CHARGEBACK_RATE_GT",
    summary: "Verifica se taxa de chargeback do merchant excede limite",
    syntax: "MERCHANT_CHARGEBACK_RATE_GT(merchantId, 3)",
    syntaxExplanation: "Taxa > 3% = alto risco (Visa/MC exigem < 1%).",
    story: "Merchant com 8% de chargeback rate = fraudulento ou péssimo.",
    problem: "Como monitorar qualidade de merchants?",
    goldenTip: "💎 Thresholds de bandeiras:\n• Visa: 0.9%\n• Mastercard: 1.0%\n• Acima = penalidades"
  },

  MERCHANT_NEW: {
    name: "MERCHANT_NEW",
    summary: "Verifica se merchant é NOVO no sistema",
    syntax: "MERCHANT_NEW(merchantId) IS_TRUE",
    syntaxExplanation: "Merchant ativado há menos de 30 dias = novo.",
    story: "TX de alto valor para merchant de 3 dias = risco.",
    problem: "Como identificar merchants novos?",
    goldenTip: "💎 Merchants novos têm limites reduzidos até provarem histórico."
  },

  MERCHANT_AGGREGATOR: {
    name: "MERCHANT_AGGREGATOR",
    summary: "Verifica se TX é via AGREGADOR (PayPal, PagSeguro, etc)",
    syntax: "MERCHANT_AGGREGATOR() IS_TRUE",
    syntaxExplanation: "TX processada por agregador = menos visibilidade do merchant final.",
    story: "Compra via PayPal = não sabemos o merchant real = risco adicional.",
    problem: "Como identificar TXs via agregadores?",
    goldenTip: "💎 Agregadores agregam risco. Merchant real pode ser de alto risco escondido."
  },

  MERCHANT_BLOCKED_LIST: {
    name: "MERCHANT_BLOCKED_LIST",
    summary: "Verifica se merchant está em LISTA DE BLOQUEIO",
    syntax: "MERCHANT_BLOCKED_LIST(merchantId) IS_TRUE",
    syntaxExplanation: "Merchant na blacklist = BLOQUEIO total.",
    story: "Merchant já teve fraudes confirmadas = blocked list.",
    problem: "Como manter e consultar lista de merchants bloqueados?",
    goldenTip: "💎 Blocked list deve incluir: merchant ID, aliases, grupos relacionados."
  },

  MERCHANT_WHITELIST: {
    name: "MERCHANT_WHITELIST",
    summary: "Verifica se merchant está em LISTA BRANCA (confiável)",
    syntax: "MERCHANT_WHITELIST(merchantId) IS_TRUE",
    syntaxExplanation: "Merchant verificado e confiável = menos fricção.",
    story: "Amazon, Mercado Livre = whitelist = aprovação facilitada.",
    problem: "Como dar tratamento diferenciado para merchants confiáveis?",
    goldenTip: "💎 Whitelist deve ter critérios objetivos: tempo, volume, taxa de chargeback."
  },

  MCC_FIRST_TIME: {
    name: "MCC_FIRST_TIME",
    summary: "Verifica se é PRIMEIRA VEZ do cliente em determinado MCC",
    syntax: "MCC_FIRST_TIME(customerId, transaction.mcc) IS_TRUE",
    syntaxExplanation: "Primeira compra do cliente em cassino = TRUE.",
    story: "Cliente nunca comprou em gambling. Primeira vez = alerta.",
    problem: "Como detectar primeira interação com categoria?",
    goldenTip: "💎 Primeiro MCC de risco = alerta especial. Confirmar intenção do cliente."
  },

  MERCHANT_DORMANT_ACTIVATION: {
    name: "MERCHANT_DORMANT_ACTIVATION",
    summary: "Detecta merchant DORMANT que reativou",
    syntax: "MERCHANT_DORMANT_ACTIVATION(merchantId) IS_TRUE",
    syntaxExplanation: "Merchant inativo há 6 meses de repente processa R$ 100k.",
    story: "Conta dormant reativada para bust-out.",
    problem: "Como detectar reativação suspeita de merchants?",
    goldenTip: "💎 Dormant + reativação + alto volume = clássico bust-out."
  },

  MERCHANT_CROSS_BORDER: {
    name: "MERCHANT_CROSS_BORDER",
    summary: "Verifica se TX é CROSS-BORDER (merchant em país diferente)",
    syntax: "MERCHANT_CROSS_BORDER() IS_TRUE",
    syntaxExplanation: "Cliente BR comprando de merchant US = cross-border.",
    story: "Compra internacional tem regras e riscos diferentes.",
    problem: "Como identificar transações internacionais?",
    goldenTip: "💎 Cross-border: taxa de fraude 2x maior. Aplicar regras mais rigorosas."
  },

  MERCHANT_HOURS_ANOMALY: {
    name: "MERCHANT_HOURS_ANOMALY",
    summary: "Detecta TX fora do horário de funcionamento do merchant",
    syntax: "MERCHANT_HOURS_ANOMALY(merchantId, transaction.time) IS_TRUE",
    syntaxExplanation: "Loja física processando às 3h = impossível (fechada).",
    story: "Padaria processando TXs às 2h da manhã = fraude.",
    problem: "Como validar horário de funcionamento?",
    goldenTip: "💎 Lojas físicas têm horário. E-commerce 24h. Verificar tipo de merchant."
  },

  MERCHANT_UNUSUAL_AMOUNT: {
    name: "MERCHANT_UNUSUAL_AMOUNT",
    summary: "Detecta valor INCOMUM para o tipo de merchant",
    syntax: "MERCHANT_UNUSUAL_AMOUNT(merchantId, transaction.amount) IS_TRUE",
    syntaxExplanation: "TX de R$ 20k em cafeteria = impossível.",
    story: "Lanchonete processando R$ 15.000 = fraude.",
    problem: "Como validar plausibilidade de valor por merchant?",
    goldenTip: "💎 Cada merchant tem faixa de valores típica. Fora = suspeito."
  }
};

// ═══════════════════════════════════════════════════════════════════════════════════════
// PARTE 15: OPERADORES TRANSACTION (15 operadores)
// ═══════════════════════════════════════════════════════════════════════════════════════

export const TRANSACTION_SPECS: Record<string, OperatorSpec> = {
  TRANSACTION_REVERSAL_PATTERN: {
    name: "TRANSACTION_REVERSAL_PATTERN",
    summary: "Detecta padrão de REVERSÕES/ESTORNOS",
    syntax: "TRANSACTION_REVERSAL_PATTERN(customerId) IS_TRUE",
    syntaxExplanation: "Muitas reversões em sequência = possível fraude de estorno.",
    story: "10 compras e 8 estornos em 1 mês = padrão de friendly fraud.",
    problem: "Como detectar abuso de estornos?",
    goldenTip: "💎 Taxa de estorno > 20% = investigar cliente."
  },

  TRANSACTION_SPLIT_PATTERN: {
    name: "TRANSACTION_SPLIT_PATTERN",
    summary: "Detecta TX que parece ter sido DIVIDIDA",
    syntax: "TRANSACTION_SPLIT_PATTERN(customerId) IS_TRUE",
    syntaxExplanation: "5 TXs de R$ 1.900 quando limite de aprovação automática é R$ 2.000.",
    story: "Cliente descobriu limite e divide compras para evitar revisão.",
    problem: "Como detectar divisão intencional de compras?",
    goldenTip: "💎 TXs logo abaixo de limites + mesmo merchant + curto período = split."
  },

  TRANSACTION_VELOCITY_ANOMALY: {
    name: "TRANSACTION_VELOCITY_ANOMALY",
    summary: "Detecta VELOCIDADE anormal de transações",
    syntax: "TRANSACTION_VELOCITY_ANOMALY(customerId) IS_TRUE",
    syntaxExplanation: "De 2 TXs/dia para 50 TXs/dia = anomalia de velocidade.",
    story: "Cliente pacato virou 'ativo' de repente = conta comprometida.",
    problem: "Como detectar aumento súbito na frequência?",
    goldenTip: "💎 Comparar com baseline do próprio cliente, não média geral."
  },

  TRANSACTION_AMOUNT_ANOMALY: {
    name: "TRANSACTION_AMOUNT_ANOMALY",
    summary: "Detecta VALOR anormal de transação",
    syntax: "TRANSACTION_AMOUNT_ANOMALY(customerId, amount) IS_TRUE",
    syntaxExplanation: "Cliente de R$ 100 médio fazendo R$ 10.000 = anomalia de valor.",
    story: "Empregada doméstica comprando R$ 30k em eletrônicos.",
    problem: "Como detectar valores fora do padrão individual?",
    goldenTip: "💎 Use Z-score ou percentil do próprio cliente."
  },

  TRANSACTION_CHANNEL_SWITCH: {
    name: "TRANSACTION_CHANNEL_SWITCH",
    summary: "Detecta MUDANÇA de canal de transação",
    syntax: "TRANSACTION_CHANNEL_SWITCH(customerId) IS_TRUE",
    syntaxExplanation: "Cliente só usava app, agora usa web = mudança de canal.",
    story: "Mudança de canal + device novo + valor alto = ATO.",
    problem: "Como detectar troca inesperada de canal?",
    goldenTip: "💎 Channel switch isolado não é fraude. Combine com outros sinais."
  },

  TRANSACTION_DUPLICATE_CHECK: {
    name: "TRANSACTION_DUPLICATE_CHECK",
    summary: "Verifica se TX é DUPLICATA de outra recente",
    syntax: "TRANSACTION_DUPLICATE_CHECK(transaction) IS_DUPLICATE",
    syntaxExplanation: "Mesmos valores, merchant, tempo = possível duplicata (erro ou fraude).",
    story: "2 TXs idênticas em 30 segundos = replay attack ou erro de sistema.",
    problem: "Como detectar transações duplicadas?",
    goldenTip: "💎 Duplicata acidental (clique duplo) vs replay attack. Contexto importa."
  },

  TRANSACTION_GEOGRAPHIC_SPREAD: {
    name: "TRANSACTION_GEOGRAPHIC_SPREAD",
    summary: "Analisa DISPERSÃO geográfica das transações",
    syntax: "TRANSACTION_GEOGRAPHIC_SPREAD(customerId, HOUR_24) GT 1000",
    syntaxExplanation: "TXs em locais a mais de 1000km de distância em 24h.",
    story: "TXs em SP e Miami em 2 horas = impossible travel.",
    problem: "Como medir dispersão geográfica?",
    goldenTip: "💎 Spread grande em período curto = cartão clonado em uso múltiplo."
  },

  TRANSACTION_ROUNDING_PATTERN: {
    name: "TRANSACTION_ROUNDING_PATTERN",
    summary: "Detecta padrão de valores REDONDOS",
    syntax: "TRANSACTION_ROUNDING_PATTERN(customerId) IS_TRUE",
    syntaxExplanation: "Todas TXs em R$ 1.000, R$ 2.000, R$ 5.000 = muito redondo = suspeito.",
    story: "Valores naturais têm centavos. Só valores redondos = fabricado.",
    problem: "Como detectar valores artificialmente redondos?",
    goldenTip: "💎 Compras reais raramente são exatamente R$ 5.000,00. Sempre tem centavos."
  },

  TRANSACTION_BENEFICIARY_NEW: {
    name: "TRANSACTION_BENEFICIARY_NEW",
    summary: "Verifica se BENEFICIÁRIO é novo (primeira transferência)",
    syntax: "TRANSACTION_BENEFICIARY_NEW(fromAccount, toAccount) IS_TRUE",
    syntaxExplanation: "Primeira transferência para este destinatário = TRUE.",
    story: "Primeira transferência + valor alto = alto risco.",
    problem: "Como identificar primeiras transferências?",
    goldenTip: "💎 Beneficiário novo + valor > R$ 5k + device novo = tríplice suspeita."
  },

  TRANSACTION_RECIPIENT_HIGH_RISK: {
    name: "TRANSACTION_RECIPIENT_HIGH_RISK",
    summary: "Verifica se DESTINATÁRIO é de alto risco",
    syntax: "TRANSACTION_RECIPIENT_HIGH_RISK(recipientAccount) IS_TRUE",
    syntaxExplanation: "Conta destino tem histórico de receber de fraudes.",
    story: "Transferindo para conta que já recebeu de 50 contas fraudadas.",
    problem: "Como avaliar risco do destinatário?",
    goldenTip: "💎 Recipient scoring: contas que recebem de muitas fraudes = mula."
  },

  AMOUNT_SPIKE: {
    name: "AMOUNT_SPIKE",
    summary: "Detecta PICO de valor vs histórico",
    syntax: "AMOUNT_SPIKE(customerId, amount) IS_TRUE",
    syntaxExplanation: "Valor 10x maior que a média histórica = spike.",
    story: "Média R$ 500, de repente R$ 15.000 = spike de 30x.",
    problem: "Como detectar valores anormalmente altos?",
    goldenTip: "💎 Spike > 5x = alerta. > 10x = alto risco. > 20x = bloqueio."
  },

  AMOUNT_ROUNDING_BEHAVIOR: {
    name: "AMOUNT_ROUNDING_BEHAVIOR",
    summary: "Analisa COMPORTAMENTO de arredondamento",
    syntax: "AMOUNT_ROUNDING_BEHAVIOR(transactions) ROUND_PERCENTAGE GT 80",
    syntaxExplanation: "Mais de 80% das TXs são valores redondos = suspeito.",
    story: "Cliente só faz TXs em valores exatos = padrão não natural.",
    problem: "Como identificar padrão de valores não-naturais?",
    goldenTip: "💎 Mix natural: 20-30% redondos, 70-80% com centavos."
  },

  TRANSFER_TO_SELF: {
    name: "TRANSFER_TO_SELF",
    summary: "Detecta transferência para SI MESMO (contas diferentes)",
    syntax: "TRANSFER_TO_SELF(fromAccount, toAccount) IS_TRUE",
    syntaxExplanation: "Mesma pessoa em origem e destino = transferência própria.",
    story: "João transfere para João em outro banco = self-transfer.",
    problem: "Como identificar transferências próprias?",
    goldenTip: "💎 Self-transfer não é fraude, mas pode ser usado para contornar limites."
  },

  TRANSFER_VELOCITY_GT: {
    name: "TRANSFER_VELOCITY_GT",
    summary: "Conta TRANSFERÊNCIAS em janela de tempo",
    syntax: "TRANSFER_VELOCITY_GT(accountId, HOUR_24) GT 10",
    syntaxExplanation: "Mais de 10 transferências em 24h = alta velocidade.",
    story: "50 PIX em 1 hora = possível drenagem de conta comprometida.",
    problem: "Como limitar velocidade de transferências?",
    goldenTip: "💎 PIX permite muitas TXs. Limitar por número E por valor total."
  },

  UNIQUE_RECIPIENTS_GT: {
    name: "UNIQUE_RECIPIENTS_GT",
    summary: "Conta DESTINATÁRIOS únicos em janela de tempo",
    syntax: "UNIQUE_RECIPIENTS_GT(accountId, DAY_1) GT 20",
    syntaxExplanation: "Transferiu para mais de 20 contas diferentes em 1 dia.",
    story: "Conta enviando para 50 destinatários únicos em 1 hora = mula distribuindo.",
    problem: "Como detectar distribuição para muitos destinos?",
    goldenTip: "💎 Fan-out alto = splitting/distribuição. Normal: 2-5 destinos/dia."
  }
};

// ═══════════════════════════════════════════════════════════════════════════════════════
// PARTE 16: OPERADORES SCA/PSD3 (17 operadores - Strong Customer Authentication)
// ═══════════════════════════════════════════════════════════════════════════════════════

export const SCA_PSD3_SPECS: Record<string, OperatorSpec> = {
  SCA_REQUIRED: {
    name: "SCA_REQUIRED",
    summary: "Verifica se SCA (Strong Customer Authentication) é OBRIGATÓRIO",
    syntax: "SCA_REQUIRED(transaction) IS_TRUE",
    syntaxExplanation: "TX requer 2FA conforme PSD2/PSD3. Exceções: < €30, merchant confiável, etc.",
    story: "TX de €500 em e-commerce = SCA obrigatório (PSD2).",
    problem: "Como determinar se TX precisa de SCA?",
    goldenTip: "💎 SCA = 2 de 3 fatores:\n• Algo que sabe (senha)\n• Algo que tem (telefone)\n• Algo que é (biometria)",
    engineBehavior: {
      description: "Avaliação de requisitos SCA:",
      steps: [
        "1. Verifica valor (< €30 = isenção)",
        "2. Verifica se merchant está em whitelist do cliente",
        "3. Verifica TRA (Transaction Risk Analysis)",
        "4. Se nenhuma isenção aplicável → SCA_REQUIRED"
      ]
    }
  },

  SCA_EXEMPTION_LOW_VALUE: {
    name: "SCA_EXEMPTION_LOW_VALUE",
    summary: "Verifica elegibilidade para ISENÇÃO de baixo valor",
    syntax: "SCA_EXEMPTION_LOW_VALUE(amount) IS_TRUE",
    syntaxExplanation: "TX < €30 pode ser isenta de SCA (limite cumulativo de €100/5 TXs).",
    story: "Compra de €15 em cafeteria = isenção de baixo valor aplicável.",
    problem: "Como aplicar isenção de SCA para valores baixos?",
    goldenTip: "💎 Limites PSD2:\n• Por TX: < €30\n• Cumulativo: 5 TXs ou €100 total\nApós limite: SCA obrigatório"
  },

  SCA_EXEMPTION_TRA: {
    name: "SCA_EXEMPTION_TRA",
    summary: "Verifica elegibilidade para isenção por TRA (Transaction Risk Analysis)",
    syntax: "SCA_EXEMPTION_TRA(transaction) IS_TRUE",
    syntaxExplanation: "TX de baixo risco conforme análise pode ser isenta de SCA.",
    story: "Cliente frequente, device conhecido, valor normal = baixo risco = isenção TRA.",
    problem: "Como aplicar isenção TRA?",
    goldenTip: "💎 TRA thresholds por fraud rate:\n• < €500 se fraud rate < 0.13%\n• < €250 se fraud rate < 0.06%\n• < €100 se fraud rate < 0.01%"
  },

  SCA_EXEMPTION_WHITELIST: {
    name: "SCA_EXEMPTION_WHITELIST",
    summary: "Verifica se merchant está na WHITELIST do cliente",
    syntax: "SCA_EXEMPTION_WHITELIST(customerId, merchantId) IS_TRUE",
    syntaxExplanation: "Cliente adicionou merchant como 'confiável' = isenção de SCA.",
    story: "Netflix na whitelist do cliente = assinatura mensal sem SCA.",
    problem: "Como permitir isenção para merchants de confiança?",
    goldenTip: "💎 Cliente gerencia sua whitelist. Útil para assinaturas recorrentes."
  },

  SCA_EXEMPTION_RECURRING: {
    name: "SCA_EXEMPTION_RECURRING",
    summary: "Verifica elegibilidade para isenção de pagamento RECORRENTE",
    syntax: "SCA_EXEMPTION_RECURRING(transaction) IS_TRUE",
    syntaxExplanation: "Pagamento recorrente com mesmo valor = isenção de SCA após 1º.",
    story: "Assinatura Spotify R$ 21,90/mês = SCA só na 1ª, depois isento.",
    problem: "Como isentar pagamentos recorrentes?",
    goldenTip: "💎 Recorrente fixo: valor igual, mesmo merchant, periodicidade. Variável (ex: conta de luz) não é isento."
  },

  SCA_EXEMPTION_CORPORATE: {
    name: "SCA_EXEMPTION_CORPORATE",
    summary: "Verifica elegibilidade para isenção CORPORATIVA",
    syntax: "SCA_EXEMPTION_CORPORATE(transaction) IS_TRUE",
    syntaxExplanation: "Pagamentos corporativos entre empresas podem ser isentos.",
    story: "B2B com processo de pagamento seguro = isenção corporativa.",
    problem: "Como aplicar isenção para pagamentos empresariais?",
    goldenTip: "💎 Requer: emissor e adquirente na Europa, processos seguros certificados."
  },

  SCA_CHALLENGE_TYPE: {
    name: "SCA_CHALLENGE_TYPE",
    summary: "Determina TIPO de challenge SCA necessário",
    syntax: "SCA_CHALLENGE_TYPE(transaction)",
    syntaxExplanation: "Retorna tipo: SMS_OTP, APP_PUSH, BIOMETRIC, etc.",
    story: "TX de €10.000 = biometria. TX de €100 = SMS OTP.",
    problem: "Como determinar nível de autenticação apropriado?",
    goldenTip: "💎 Proporcionalidade:\n• < €100: SMS\n• €100-€500: App push\n• > €500: Biometria"
  },

  SCA_STEP_UP_REQUIRED: {
    name: "SCA_STEP_UP_REQUIRED",
    summary: "Verifica se é necessário STEP-UP de autenticação",
    syntax: "SCA_STEP_UP_REQUIRED(session, transaction) IS_TRUE",
    syntaxExplanation: "Sessão atual não tem nível de autenticação suficiente para TX.",
    story: "Logou com senha, agora quer fazer PIX de R$ 10k = step-up para biometria.",
    problem: "Como determinar necessidade de autenticação adicional?",
    goldenTip: "💎 Step-up progressivo: ação sensível requer re-autenticação."
  },

  PSD3_IBAN_CHECK: {
    name: "PSD3_IBAN_CHECK",
    summary: "Verificação de IBAN conforme PSD3",
    syntax: "PSD3_IBAN_CHECK(transaction) VALID",
    syntaxExplanation: "Valida IBAN (formato, dígitos verificadores, banco existente).",
    story: "IBAN inválido ou de banco inexistente = reject.",
    problem: "Como validar IBANs europeus?",
    goldenTip: "💎 IBAN check: formato + dígitos + existência do banco."
  },

  PSD3_PAYEE_VERIFICATION: {
    name: "PSD3_PAYEE_VERIFICATION",
    summary: "Verificação do BENEFICIÁRIO conforme PSD3",
    syntax: "PSD3_PAYEE_VERIFICATION(transaction) MATCH",
    syntaxExplanation: "Nome do beneficiário confere com titular da conta destino?",
    story: "Transfere para 'João Silva' mas conta é de 'Maria Santos' = mismatch.",
    problem: "Como verificar se nome corresponde à conta?",
    goldenTip: "💎 Confirmation of Payee (CoP): obrigatório em PSD3 para reduzir APP fraud."
  },

  PSD3_FRAUD_RATE_MONITORING: {
    name: "PSD3_FRAUD_RATE_MONITORING",
    summary: "Monitoramento de TAXA DE FRAUDE conforme PSD3",
    syntax: "PSD3_FRAUD_RATE_MONITORING(issuer) RATE GT 0.13",
    syntaxExplanation: "Monitora fraud rate do emissor. Acima de 0.13% perde direito a isenções TRA.",
    story: "Banco com fraud rate 0.2% perde isenção TRA para TXs > €100.",
    problem: "Como monitorar e reportar fraud rates?",
    goldenTip: "💎 Fraud rate thresholds:\n• 0.13% = TRA até €500\n• 0.06% = TRA até €250\n• 0.01% = TRA até €100"
  },

  PSD3_OPEN_BANKING_CONSENT: {
    name: "PSD3_OPEN_BANKING_CONSENT",
    summary: "Verifica CONSENTIMENTO para Open Banking",
    syntax: "PSD3_OPEN_BANKING_CONSENT(customerId, tpp) IS_VALID",
    syntaxExplanation: "Cliente deu consentimento para TPP acessar dados/iniciar pagamento?",
    story: "Fintech quer iniciar pagamento = precisa consentimento válido.",
    problem: "Como verificar e gerenciar consentimentos Open Banking?",
    goldenTip: "💎 Consentimento PSD3: específico, informado, renovável a cada 90 dias."
  },

  SCA_DYNAMIC_LINKING: {
    name: "SCA_DYNAMIC_LINKING",
    summary: "Verifica DYNAMIC LINKING da autenticação",
    syntax: "SCA_DYNAMIC_LINKING(authentication, transaction) LINKED",
    syntaxExplanation: "Código de autenticação está vinculado a esta TX específica?",
    story: "OTP gerado para TX de €100 não pode ser usado para TX de €10.000.",
    problem: "Como garantir que autenticação está vinculada à TX correta?",
    goldenTip: "💎 Dynamic linking: OTP/código deve conter valor e beneficiário."
  },

  SCA_REAUTHENTICATION_REQUIRED: {
    name: "SCA_REAUTHENTICATION_REQUIRED",
    summary: "Verifica se REAUTENTICAÇÃO é necessária",
    syntax: "SCA_REAUTHENTICATION_REQUIRED(session) IS_TRUE",
    syntaxExplanation: "Sessão autenticada há mais de 5 minutos para ação sensível.",
    story: "Logou há 1h, quer mudar senha = reautentica.",
    problem: "Como exigir reautenticação para ações sensíveis?",
    goldenTip: "💎 Ações sensíveis: mudança de senha, email, telefone, transferência grande."
  },

  SCA_FALLBACK_MECHANISM: {
    name: "SCA_FALLBACK_MECHANISM",
    summary: "Ativa mecanismo de FALLBACK de SCA",
    syntax: "SCA_FALLBACK_MECHANISM(primaryMethod) ALTERNATIVE",
    syntaxExplanation: "Se método primário falhar, qual alternativa usar?",
    story: "App push não entregou → fallback para SMS.",
    problem: "Como garantir que cliente consiga autenticar se método falhar?",
    goldenTip: "💎 Hierarquia: App push → SMS → Ligação → Presencial."
  },

  SCA_DELEGATION_CHECK: {
    name: "SCA_DELEGATION_CHECK",
    summary: "Verifica DELEGAÇÃO de autenticação",
    syntax: "SCA_DELEGATION_CHECK(merchant, transaction) DELEGATED",
    syntaxExplanation: "Merchant está autorizado a realizar SCA em nome do emissor?",
    story: "Amazon pode fazer 3DS em nome do banco emissor.",
    problem: "Como verificar delegação válida de SCA?",
    goldenTip: "💎 Delegated authentication: merchant certificado assume responsabilidade."
  },

  SCA_RISK_INDICATOR_HIGH: {
    name: "SCA_RISK_INDICATOR_HIGH",
    summary: "Indicador de ALTO RISCO para decisão de SCA",
    syntax: "SCA_RISK_INDICATOR_HIGH(transaction) IS_TRUE",
    syntaxExplanation: "TX tem indicadores de alto risco → SCA obrigatório independente de isenções.",
    story: "Device novo + beneficiário novo + valor alto = alto risco = SCA mandatório.",
    problem: "Como sobrepor isenções quando risco é alto?",
    goldenTip: "💎 Override de isenções: mesmo elegível para isenção, risco alto = SCA."
  }
};
