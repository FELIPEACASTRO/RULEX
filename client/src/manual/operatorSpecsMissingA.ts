/**
 * OPERATOR_SPECS_COMPLETE - PARTE 5 (MISSING A-E)
 */

import type { OperatorSpec } from './operatorSpecs';

export const MISSING_SPECS_A_E: Record<string, OperatorSpec> = {
  ADAPTIVE_PARAMETRIC_THRESHOLD: {
    name: "ADAPTIVE_PARAMETRIC_THRESHOLD",
    summary: "Ajusta limiar automaticamente com base no comportamento recente",
    syntax: "ADAPTIVE_PARAMETRIC_THRESHOLD(metric, WINDOW_30_DAYS) GT 1.2",
    syntaxExplanation: "Calcula média/percentil da métrica na janela e aplica fator de ajuste.",
    story: "Média de valor subiu para R$ 800. Limiar se ajusta para 1.2x = R$ 960.",
    problem: "Como evitar thresholds fixos que ficam obsoletos?",
    goldenTip: "💎 Use janela suficiente (>=30 dias) para evitar overfitting de curto prazo."
  },

  ADDRESS_CHANGE_VELOCITY: {
    name: "ADDRESS_CHANGE_VELOCITY",
    summary: "Detecta muitas mudanças de endereço em curto período",
    syntax: "ADDRESS_CHANGE_VELOCITY(customerId, DAY_30) GT 2",
    syntaxExplanation: "Conta quantas vezes o cliente alterou endereço em 30 dias.",
    story: "Cliente mudou 3 vezes em 2 semanas = possível fraude de identidade.",
    problem: "Como identificar churn anormal de endereço?",
    goldenTip: "💎 Mudança frequente + conta nova = risco elevado."
  },

  ALIAS_DETECTION: {
    name: "ALIAS_DETECTION",
    summary: "Detecta possíveis aliases (apelidos) usados para mascarar identidade",
    syntax: "ALIAS_DETECTION(name, knownAliases) IS_TRUE",
    syntaxExplanation: "Compara nome com lista de aliases e padrões comuns.",
    story: "" +
      "'José da Silva' também aparece como 'Zé Silva' em outras contas.",
    problem: "Como identificar o mesmo indivíduo usando nomes diferentes?",
    goldenTip: "💎 Combine alias com CPF/telefone/endereço para confirmação."
  },

  AMOUNT_ANOMALY: {
    name: "AMOUNT_ANOMALY",
    summary: "Detecta anomalia de valor baseada no histórico do cliente",
    syntax: "AMOUNT_ANOMALY(customerId, amount) IS_TRUE",
    syntaxExplanation: "Compara valor com baseline do cliente (média/percentil).",
    story: "Média R$ 120. TX de R$ 5.000 = anomalia.",
    problem: "Como detectar valores fora do padrão individual?",
    goldenTip: "💎 Use percentis (P95/P99) para reduzir impacto de outliers."
  },

  AMOUNT_SUM_PER_CARD_HOUR: {
    name: "AMOUNT_SUM_PER_CARD_HOUR",
    summary: "Soma valores por cartão em janela de 1 hora",
    syntax: "AMOUNT_SUM_PER_CARD_HOUR(cardId) GT 5000",
    syntaxExplanation: "Agrega todas as transações do cartão na última hora.",
    story: "Cartão fez R$ 7.000 em 1h (normal é < R$ 500).",
    problem: "Como detectar gasto intenso por cartão em curto período?",
    goldenTip: "💎 Combine com COUNT_PER_CARD_HOUR para evitar 1 única compra legítima."
  },

  AMOUNT_SUM_PER_CUSTOMER_DAY: {
    name: "AMOUNT_SUM_PER_CUSTOMER_DAY",
    summary: "Soma valores por cliente em janela de 1 dia",
    syntax: "AMOUNT_SUM_PER_CUSTOMER_DAY(customerId) GT 20000",
    syntaxExplanation: "Agrega todas as transações do cliente no dia.",
    story: "Cliente somou R$ 25k hoje, quando média diária é R$ 2k.",
    problem: "Como limitar volume diário por cliente?",
    goldenTip: "💎 Use limites diferentes por segmentação (PF, PJ, VIP)."
  },

  AMOUNT_VARIANCE_ANOMALY: {
    name: "AMOUNT_VARIANCE_ANOMALY",
    summary: "Detecta anomalia na variância dos valores",
    syntax: "AMOUNT_VARIANCE_ANOMALY(customerId, DAY_30) IS_TRUE",
    syntaxExplanation: "Variância explode quando padrões mudam abruptamente.",
    story: "Valores antes consistentes, agora variam 0-10x = anomalia.",
    problem: "Como capturar mudança no perfil de dispersão?",
    goldenTip: "💎 Use junto com SKEWNESS/KURTOSIS para capturar caudas."
  },

  APRIORI_ASSOCIATION: {
    name: "APRIORI_ASSOCIATION",
    summary: "Detecta associações frequentes entre eventos (Apriori)",
    syntax: "APRIORI_ASSOCIATION(events, MIN_SUPPORT=0.2) HAS_RULE",
    syntaxExplanation: "Busca regras tipo 'A e B → C' em padrões históricos.",
    story: "Quando há login novo + device novo, 60% vira chargeback.",
    problem: "Como descobrir combinações de sinais que aumentam risco?",
    goldenTip: "💎 Use para gerar regras candidatas e depois validar em backtest."
  },

  AVG_LAST_N_DAYS: {
    name: "AVG_LAST_N_DAYS",
    summary: "Calcula média de um valor nos últimos N dias",
    syntax: "AVG_LAST_N_DAYS(customerId, amount, 30)",
    syntaxExplanation: "Média histórica usada como baseline dinâmica.",
    story: "Média 30 dias = R$ 200. TX atual = R$ 2.000.",
    problem: "Como comparar com média móvel?",
    goldenTip: "💎 N pequeno = sensível. N grande = mais estável."
  },

  AVG_TRANSACTION_SPIKE: {
    name: "AVG_TRANSACTION_SPIKE",
    summary: "Detecta spike entre média móvel e valor atual",
    syntax: "AVG_TRANSACTION_SPIKE(amount, AVG_LAST_30_DAYS) GT 5",
    syntaxExplanation: "Valor atual / média móvel > 5 = spike.",
    story: "Cliente com média R$ 100 fez R$ 1.200 (12x).",
    problem: "Como detectar picos relativos ao histórico?",
    goldenTip: "💎 Use fator > 3 para alerta e > 10 para bloqueio automático."
  },

  BENEFICIARY_ADD_VELOCITY: {
    name: "BENEFICIARY_ADD_VELOCITY",
    summary: "Detecta velocidade de adição de beneficiários",
    syntax: "BENEFICIARY_ADD_VELOCITY(customerId, DAY_7) GT 3",
    syntaxExplanation: "Muitos beneficiários adicionados em pouco tempo.",
    story: "Cliente adicionou 5 contas em 2 dias = suspeito.",
    problem: "Como detectar preparação para transferências em massa?",
    goldenTip: "💎 Adição em massa + transferências imediatas = ATO provável."
  },

  BENEFICIARY_CONCENTRATION: {
    name: "BENEFICIARY_CONCENTRATION",
    summary: "Detecta concentração excessiva de transferências em 1 beneficiário",
    syntax: "BENEFICIARY_CONCENTRATION(customerId, DAY_30) GT 0.8",
    syntaxExplanation: "80% do volume indo para 1 destinatário.",
    story: "Cliente começa a enviar quase tudo para uma única conta.",
    problem: "Como identificar escoamento concentrado?",
    goldenTip: "💎 Concentração alta + beneficiário novo = risco alto."
  },

  BENEFICIARY_REUSE_PATTERN: {
    name: "BENEFICIARY_REUSE_PATTERN",
    summary: "Detecta reutilização de beneficiários em padrões suspeitos",
    syntax: "BENEFICIARY_REUSE_PATTERN(customerId, DAY_7) IS_TRUE",
    syntaxExplanation: "Reuso de beneficiários associados a fraude.",
    story: "Mesmos 3 beneficiários usados por várias contas recém-criadas.",
    problem: "Como detectar contas 'mula' compartilhando destinos?",
    goldenTip: "💎 Use com graph analytics para ver ligações entre contas."
  },

  BIOMETRIC_SCROLL_VELOCITY: {
    name: "BIOMETRIC_SCROLL_VELOCITY",
    summary: "Detecta velocidade de scroll anormal (biometria comportamental)",
    syntax: "BIOMETRIC_SCROLL_VELOCITY(session) ANOMALY",
    syntaxExplanation: "Scroll muito rápido/linear indica automação.",
    story: "Scroll de 5.000px em 0.2s = bot.",
    problem: "Como diferenciar humano de script?",
    goldenTip: "💎 Combine com mouse movement e entropy de navegação."
  },

  BSL_BUCKET_CLASSIFICATION: {
    name: "BSL_BUCKET_CLASSIFICATION",
    summary: "Classifica perdas em buckets Basel (BSL)",
    syntax: "BSL_BUCKET_CLASSIFICATION(lossEvent) EQ 'EF1'",
    syntaxExplanation: "Classificação de eventos de perda operacional.",
    story: "Fraude interna classificada como EF2 conforme BSL.",
    problem: "Como padronizar categorias de perda operacional?",
    goldenTip: "💎 Use taxonomy BSL para consistência regulatória."
  },

  BSL_BUSINESS_INDICATOR: {
    name: "BSL_BUSINESS_INDICATOR",
    summary: "Calcula Business Indicator (BI) Basel",
    syntax: "BSL_BUSINESS_INDICATOR(financials) GT 1_000_000",
    syntaxExplanation: "BI = métrica de volume e complexidade da instituição.",
    story: "BI alto exige capital operacional maior.",
    problem: "Como estimar BI para capital regulatório?",
    goldenTip: "💎 BI usa componentes: juros, serviços e financeiro."
  },

  BSL_BUSINESS_INDICATOR_COMPONENT: {
    name: "BSL_BUSINESS_INDICATOR_COMPONENT",
    summary: "Calcula componente específico do Business Indicator",
    syntax: "BSL_BUSINESS_INDICATOR_COMPONENT(type='services')",
    syntaxExplanation: "Retorna componente de serviços/juros/financeiro.",
    story: "Componente de serviços subiu 30% com novos produtos.",
    problem: "Como analisar BI por componente?",
    goldenTip: "💎 Use para explicar variações no BI total."
  },

  BSL_CONTROL_DEFICIENCY: {
    name: "BSL_CONTROL_DEFICIENCY",
    summary: "Registra deficiência de controle interno",
    syntax: "BSL_CONTROL_DEFICIENCY(event) IS_TRUE",
    syntaxExplanation: "Sinaliza falha de controle relevante em Basel Loss.",
    story: "Falha de dupla aprovação → control deficiency.",
    problem: "Como registrar falhas de controle para Basel?",
    goldenTip: "💎 Cada deficiência deve ter owner, plano de ação e prazo."
  },

  BSL_INTERNAL_LOSS_MULTIPLIER: {
    name: "BSL_INTERNAL_LOSS_MULTIPLIER",
    summary: "Calcula Internal Loss Multiplier (ILM) Basel",
    syntax: "BSL_INTERNAL_LOSS_MULTIPLIER(lossHistory) GT 1.1",
    syntaxExplanation: "ILM ajusta capital conforme perdas internas históricas.",
    story: "Perdas recentes elevam ILM e capital exigido.",
    problem: "Como refletir histórico de perdas no capital?",
    goldenTip: "💎 ILM alto = maior capital. Incentiva redução de perdas."
  },

  BSL_KRI_MONITORING: {
    name: "BSL_KRI_MONITORING",
    summary: "Monitora Key Risk Indicators (KRIs) Basel",
    syntax: "BSL_KRI_MONITORING(kri) GT threshold",
    syntaxExplanation: "KRIs como downtime, falhas operacionais, etc.",
    story: "KRI de indisponibilidade > 99.5%? alerta.",
    problem: "Como acompanhar indicadores de risco operacional?",
    goldenTip: "💎 KRIs devem ter owner e limite tolerável (RTO/RPO)."
  },

  BSL_LOSS_DATA_COLLECTION: {
    name: "BSL_LOSS_DATA_COLLECTION",
    summary: "Registra coleta de dados de perdas (Basel)",
    syntax: "BSL_LOSS_DATA_COLLECTION(event) RECORDED",
    syntaxExplanation: "Confirma captura de perda operacional.",
    story: "Evento de fraude interna registrado corretamente.",
    problem: "Como garantir qualidade dos dados de perda?",
    goldenTip: "💎 Sem dados de perda, ILM fica subestimado e risco real oculto."
  },

  BSL_LOSS_EVENT_REPORTING: {
    name: "BSL_LOSS_EVENT_REPORTING",
    summary: "Reporta eventos de perda conforme Basel",
    syntax: "BSL_LOSS_EVENT_REPORTING(event) SUBMITTED",
    syntaxExplanation: "Evento reportado a comitê/board conforme política.",
    story: "Fraude operacional > limite reportável enviada ao board.",
    problem: "Como garantir reporte de perdas relevantes?",
    goldenTip: "💎 Defina thresholds de reporte por severidade."
  },

  BSL_LOSS_EXCLUSION_APPROVAL: {
    name: "BSL_LOSS_EXCLUSION_APPROVAL",
    summary: "Controle de aprovação para exclusão de perdas",
    syntax: "BSL_LOSS_EXCLUSION_APPROVAL(lossEvent) APPROVED",
    syntaxExplanation: "Exclusão de perda do histórico requer aprovação formal.",
    story: "Perda recuperada 100% pode ser excluída com aprovação.",
    problem: "Como evitar manipulação de perdas históricas?",
    goldenTip: "💎 Excluir perda sem aprovação = risco regulatório sério."
  },

  BSL_LOSS_THRESHOLD_SETTING: {
    name: "BSL_LOSS_THRESHOLD_SETTING",
    summary: "Define threshold mínimo para registro de perdas",
    syntax: "BSL_LOSS_THRESHOLD_SETTING(1000) SET",
    syntaxExplanation: "Só perdas acima de R$ 1.000 entram no banco Basel.",
    story: "Pequenas perdas operacionais não entram no dataset Basel.",
    problem: "Como calibrar limites de registro?",
    goldenTip: "💎 Threshold muito alto subestima risco. Muito baixo gera ruído."
  },

  BSL_MARGINAL_COEFFICIENT: {
    name: "BSL_MARGINAL_COEFFICIENT",
    summary: "Aplica coeficiente marginal Basel",
    syntax: "BSL_MARGINAL_COEFFICIENT(BI) EQ 0.15",
    syntaxExplanation: "Coeficientes por faixa de BI.",
    story: "BI alto aplica coeficiente maior para capital.",
    problem: "Como calcular capital pelo método standard?",
    goldenTip: "💎 Coeficientes variam por faixa de BI."
  },

  BSL_RETENTION_PERIOD: {
    name: "BSL_RETENTION_PERIOD",
    summary: "Define período de retenção de dados Basel",
    syntax: "BSL_RETENTION_PERIOD(YEARS=10) ACTIVE",
    syntaxExplanation: "Basel exige retenção mínima de dados históricos.",
    story: "Loss data precisa de 10 anos para cálculo ILM.",
    problem: "Como cumprir retenção de dados regulatórios?",
    goldenTip: "💎 Retenção curta = não conformidade com Basel."
  },

  BSL_RISK_GOVERNANCE: {
    name: "BSL_RISK_GOVERNANCE",
    summary: "Verifica requisitos de governança de risco Basel",
    syntax: "BSL_RISK_GOVERNANCE(policy) COMPLIANT",
    syntaxExplanation: "Checa existência de políticas, comitês e ownership.",
    story: "Sem comitê de risco operacional = não compliance.",
    problem: "Como avaliar governança de risco?",
    goldenTip: "💎 Reguladores exigem evidência documental."
  },

  BSL_SCENARIO_ANALYSIS: {
    name: "BSL_SCENARIO_ANALYSIS",
    summary: "Executa análise de cenários Basel",
    syntax: "BSL_SCENARIO_ANALYSIS(scenario) RESULT",
    syntaxExplanation: "Simula perdas extremas e impactos.",
    story: "Simular falha cibernética com perda de R$ 50M.",
    problem: "Como avaliar riscos raros e extremos?",
    goldenTip: "💎 Combine cenário com dados históricos para stress tests."
  },

  BUSINESS_HOURS_DEVIATION: {
    name: "BUSINESS_HOURS_DEVIATION",
    summary: "Detecta desvio de horário comercial habitual",
    syntax: "BUSINESS_HOURS_DEVIATION(customerId) IS_TRUE",
    syntaxExplanation: "Cliente opera fora do horário usual (perfil temporal).",
    story: "Empresa só opera 9-18h. TX às 2h = desvio.",
    problem: "Como identificar atividades fora do padrão temporal?",
    goldenTip: "💎 Cada cliente tem janela normal. Desvio = alerta."
  },

  CAPTCHA_FAILED: {
    name: "CAPTCHA_FAILED",
    summary: "Indica falha em desafio de CAPTCHA",
    syntax: "CAPTCHA_FAILED(session) IS_TRUE",
    syntaxExplanation: "Usuário falhou no desafio de verificação humana.",
    story: "Bot não consegue resolver CAPTCHA em 3 tentativas.",
    problem: "Como bloquear automações?",
    goldenTip: "💎 Após falha, aplique rate limit e step-up."
  },

  CARD_ADD_VELOCITY: {
    name: "CARD_ADD_VELOCITY",
    summary: "Detecta velocidade de adição de cartões",
    syntax: "CARD_ADD_VELOCITY(customerId, DAY_7) GT 2",
    syntaxExplanation: "Muitos cartões adicionados em pouco tempo.",
    story: "Conta adicionou 4 cartões em 2 dias = suspeito.",
    problem: "Como detectar carding associado a contas?",
    goldenTip: "💎 Card add velocity + CVV failures = card testing."
  },

  CARD_CAPTURE_FRAUD: {
    name: "CARD_CAPTURE_FRAUD",
    summary: "Detecta fraude de captura de cartão",
    syntax: "CARD_CAPTURE_FRAUD(merchantId) IS_TRUE",
    syntaxExplanation: "Merchant captura dados e tenta usar/replicar.",
    story: "Merchant com histórico de skimming = alta suspeita.",
    problem: "Como detectar merchants comprometidos?",
    goldenTip: "💎 Combine com chargeback spike e MCC de risco."
  },

  CASH_INTENSIVE_RATIO: {
    name: "CASH_INTENSIVE_RATIO",
    summary: "Mede proporção de operações em dinheiro",
    syntax: "CASH_INTENSIVE_RATIO(customerId, MONTH_1) GT 0.7",
    syntaxExplanation: "Mais de 70% em cash pode indicar lavagem.",
    story: "Empresa diz ser digital mas 80% cash = inconsistente.",
    problem: "Como identificar negócios intensivos em cash?",
    goldenTip: "💎 Cash-heavy é típico em restaurantes, bares, estacionamentos."
  },

  CHANNEL_SWITCH_PATTERN: {
    name: "CHANNEL_SWITCH_PATTERN",
    summary: "Detecta mudança de canal de uso",
    syntax: "CHANNEL_SWITCH_PATTERN(customerId) IS_TRUE",
    syntaxExplanation: "Ex: app → web → call center em curto período.",
    story: "Conta comprometida troca canal para burlar controles.",
    problem: "Como identificar mudanças de canal suspeitas?",
    goldenTip: "💎 Mudança de canal + device novo = alto risco."
  },

  CHANNEL_USAGE_ANOMALY: {
    name: "CHANNEL_USAGE_ANOMALY",
    summary: "Detecta anomalia no uso de canais",
    syntax: "CHANNEL_USAGE_ANOMALY(customerId) IS_TRUE",
    syntaxExplanation: "Uso de canal fora do perfil (ex: app 2% → 90%).",
    story: "Cliente quase nunca usa web, mas fez 10 TXs via web hoje.",
    problem: "Como detectar mudança de comportamento por canal?",
    goldenTip: "💎 Ajuste baseline por segmento e sazonalidade."
  },

  CHARGEBACK_RATE_GT: {
    name: "CHARGEBACK_RATE_GT",
    summary: "Verifica se taxa de chargeback excede limite",
    syntax: "CHARGEBACK_RATE_GT(merchantId, 1.0)",
    syntaxExplanation: "Taxa > 1% indica risco elevado.",
    story: "Merchant com 3% de chargeback = muito acima do tolerável.",
    problem: "Como monitorar qualidade de merchants?",
    goldenTip: "💎 Use thresholds de bandeiras para evitar penalidades."
  },

  CIRCULAR_TRANSFER_DETECTION: {
    name: "CIRCULAR_TRANSFER_DETECTION",
    summary: "Detecta transferências em círculo (A→B→C→A)",
    syntax: "CIRCULAR_TRANSFER_DETECTION(network, DAY_7) IS_TRUE",
    syntaxExplanation: "Ciclos de transferências indicam layering.",
    story: "Dinheiro sai e retorna em 24h por múltiplas contas.",
    problem: "Como detectar lavagem por circularidade?",
    goldenTip: "💎 Combine com graph analytics para detectar ciclos."
  },

  CLICK_VELOCITY_GT: {
    name: "CLICK_VELOCITY_GT",
    summary: "Detecta velocidade de cliques acima do normal",
    syntax: "CLICK_VELOCITY_GT(session, MINUTE_1) GT 100",
    syntaxExplanation: "Mais de 100 cliques/minuto indica bot.",
    story: "Bot dispara 300 cliques/minuto em anúncios.",
    problem: "Como detectar click fraud?",
    goldenTip: "💎 Combine com IP reputation e ausência de mouse movement."
  },

  COEFFICIENT_VARIATION_GT: {
    name: "COEFFICIENT_VARIATION_GT",
    summary: "Verifica se coeficiente de variação excede limite",
    syntax: "COEFFICIENT_VARIATION_GT(values, 0.7)",
    syntaxExplanation: "CV = desvio padrão / média. > 0.7 = alta dispersão.",
    story: "Valores muito variáveis em curto período.",
    problem: "Como medir variabilidade relativa?",
    goldenTip: "💎 CV é bom para comparar dispersão entre escalas diferentes."
  },

  CONSORTIUM_NEGATIVE_FILE_CHECK: {
    name: "CONSORTIUM_NEGATIVE_FILE_CHECK",
    summary: "Consulta arquivo negativo de consórcio (fraudes compartilhadas)",
    syntax: "CONSORTIUM_NEGATIVE_FILE_CHECK(entity) HIT",
    syntaxExplanation: "Lista compartilhada entre instituições com eventos de fraude.",
    story: "CPF já fraudou em outra instituição = hit.",
    problem: "Como usar inteligência compartilhada?",
    goldenTip: "💎 Use consórcios para reduzir 'fraude itinerante'."
  },

  CONTAINS_SUSPICIOUS_KEYWORDS: {
    name: "CONTAINS_SUSPICIOUS_KEYWORDS",
    summary: "Detecta palavras suspeitas em texto",
    syntax: "CONTAINS_SUSPICIOUS_KEYWORDS(description) IS_TRUE",
    syntaxExplanation: "Busca termos como 'test', 'fraude', 'chargeback', etc.",
    story: "Descrição de pagamento contém 'test123'.",
    problem: "Como detectar conteúdo suspeito em descrições?",
    goldenTip: "💎 Mantenha lista de keywords atualizada e contextualizada."
  },

  CONTEXT: {
    name: "CONTEXT",
    summary: "Avalia contexto agregado do evento/usuário",
    syntax: "CONTEXT(transaction) MATCHES_PROFILE",
    syntaxExplanation: "Resumo de sinais (device, geo, valor, horário).",
    story: "Contexto geral é inconsistente com histórico do cliente.",
    problem: "Como avaliar risco de forma holística?",
    goldenTip: "💎 Use contexto para reduzir falsos positivos isolados."
  },

  CORRELATION_ANOMALY: {
    name: "CORRELATION_ANOMALY",
    summary: "Detecta anomalia em correlações esperadas",
    syntax: "CORRELATION_ANOMALY(metricA, metricB) IS_TRUE",
    syntaxExplanation: "Relação entre métricas quebra o padrão histórico.",
    story: "Aumento de volume sem aumento de usuários = anomalia.",
    problem: "Como detectar relações anormais entre variáveis?",
    goldenTip: "💎 Correlacione métricas de volume, valor e frequência."
  },

  CORRESPONDENT_ANOMALY: {
    name: "CORRESPONDENT_ANOMALY",
    summary: "Detecta anomalia em correspondentes bancários",
    syntax: "CORRESPONDENT_ANOMALY(correspondentId) IS_TRUE",
    syntaxExplanation: "Correspondente fora do perfil histórico.",
    story: "Correspondente com volume 5x maior que o normal.",
    problem: "Como monitorar correspondentes?",
    goldenTip: "💎 Correspondentes são vetores comuns de fraude e lavagem."
  },

  COUNT_CRYPTO_TXN_LAST_N_DAYS: {
    name: "COUNT_CRYPTO_TXN_LAST_N_DAYS",
    summary: "Conta transações cripto nos últimos N dias",
    syntax: "COUNT_CRYPTO_TXN_LAST_N_DAYS(customerId, 30) GT 5",
    syntaxExplanation: "Conta volume de operações cripto por período.",
    story: "Cliente fez 10 operações cripto em 7 dias.",
    problem: "Como monitorar exposição cripto?",
    goldenTip: "💎 Combine com IS_CRYPTO_RANSOM_AMOUNT para alertas graves."
  },

  COUNT_DISTINCT_ACCOUNTS: {
    name: "COUNT_DISTINCT_ACCOUNTS",
    summary: "Conta número de contas distintas envolvidas",
    syntax: "COUNT_DISTINCT_ACCOUNTS(network, DAY_7) GT 20",
    syntaxExplanation: "Quantas contas diferentes aparecem no período.",
    story: "Múltiplas contas recebendo/mandando para o mesmo usuário.",
    problem: "Como medir dispersão de contas?",
    goldenTip: "💎 Fan-out alto = possível distribuição para mulas."
  },

  COUNT_DISTINCT_COUNTRIES_LAST_N_DAYS: {
    name: "COUNT_DISTINCT_COUNTRIES_LAST_N_DAYS",
    summary: "Conta países distintos em N dias",
    syntax: "COUNT_DISTINCT_COUNTRIES_LAST_N_DAYS(customerId, 30) GT 5",
    syntaxExplanation: "Número de países envolvidos em transações.",
    story: "Cliente operou em 7 países em 30 dias.",
    problem: "Como medir dispersão internacional?",
    goldenTip: "💎 Combinar com IMPOSSIBLE_TRAVEL."
  },

  COUNT_DISTINCT_COUNTRIES_LAST_N_HOURS: {
    name: "COUNT_DISTINCT_COUNTRIES_LAST_N_HOURS",
    summary: "Conta países distintos em N horas",
    syntax: "COUNT_DISTINCT_COUNTRIES_LAST_N_HOURS(customerId, 24) GT 2",
    syntaxExplanation: "Detecta atividade multi-país em curto período.",
    story: "Transações BR e US em 3 horas.",
    problem: "Como detectar uso internacional suspeito?",
    goldenTip: "💎 Curto período + múltiplos países = alto risco."
  },

  COUNT_DISTINCT_INSTRUMENTS_LAST_N_DAYS: {
    name: "COUNT_DISTINCT_INSTRUMENTS_LAST_N_DAYS",
    summary: "Conta instrumentos distintos (cartões/contas) em N dias",
    syntax: "COUNT_DISTINCT_INSTRUMENTS_LAST_N_DAYS(customerId, 30) GT 3",
    syntaxExplanation: "Muitos instrumentos usados em curto período.",
    story: "Cliente usou 5 cartões diferentes em 2 semanas.",
    problem: "Como detectar múltiplos instrumentos suspeitos?",
    goldenTip: "💎 Combine com DEVICE_ACCOUNT_RATIO."
  },

  COUNT_DISTINCT_MERCHANTS_LAST_N_DAYS: {
    name: "COUNT_DISTINCT_MERCHANTS_LAST_N_DAYS",
    summary: "Conta merchants distintos em N dias",
    syntax: "COUNT_DISTINCT_MERCHANTS_LAST_N_DAYS(customerId, 30) GT 20",
    syntaxExplanation: "Número de lojas diferentes usadas.",
    story: "Cliente fez compras em 50 merchants em 1 mês.",
    problem: "Como detectar dispersão de consumo?",
    goldenTip: "💎 Dispersão alta pode indicar card testing."
  },

  COUNT_DISTINCT_MERCHANTS_LAST_N_HOURS: {
    name: "COUNT_DISTINCT_MERCHANTS_LAST_N_HOURS",
    summary: "Conta merchants distintos em N horas",
    syntax: "COUNT_DISTINCT_MERCHANTS_LAST_N_HOURS(customerId, 6) GT 10",
    syntaxExplanation: "Muitas lojas diferentes em poucas horas.",
    story: "10 merchants em 2 horas = suspeito.",
    problem: "Como detectar spree fraud?",
    goldenTip: "💎 Combine com TRANSACTION_COUNT_PER_CARD_HOUR."
  },

  COUNT_DISTINCT_PANS_LAST_N_HOURS: {
    name: "COUNT_DISTINCT_PANS_LAST_N_HOURS",
    summary: "Conta PANs distintos usados em N horas",
    syntax: "COUNT_DISTINCT_PANS_LAST_N_HOURS(deviceId, 24) GT 3",
    syntaxExplanation: "Vários cartões no mesmo device.",
    story: "Um device usa 5 cartões em 1 dia.",
    problem: "Como detectar device compartilhado para fraude?",
    goldenTip: "💎 Device com muitos PANs = suspeito de card testing."
  },

  COUNT_DISTINCT_PAYERS_LAST_N_DAYS: {
    name: "COUNT_DISTINCT_PAYERS_LAST_N_DAYS",
    summary: "Conta pagadores distintos em N dias",
    syntax: "COUNT_DISTINCT_PAYERS_LAST_N_DAYS(beneficiaryId, 30) GT 10",
    syntaxExplanation: "Muitos pagadores para o mesmo destinatário.",
    story: "Conta recebendo de 50 pagadores diferentes.",
    problem: "Como detectar contas 'mulas'?",
    goldenTip: "💎 Muitos pagadores distintos = risco de lavagem."
  },

  COUNT_DISTINCT_USER_AGENTS_LAST_N_HOURS: {
    name: "COUNT_DISTINCT_USER_AGENTS_LAST_N_HOURS",
    summary: "Conta user agents distintos em N horas",
    syntax: "COUNT_DISTINCT_USER_AGENTS_LAST_N_HOURS(accountId, 24) GT 3",
    syntaxExplanation: "Vários browsers/dispositivos em curto período.",
    story: "Conta acessada por 5 user agents em 1 dia.",
    problem: "Como detectar compartilhamento de conta?",
    goldenTip: "💎 User agents variados + IPs diferentes = ATO provável."
  },

  COUNT_FAILURES_LAST_N_HOURS: {
    name: "COUNT_FAILURES_LAST_N_HOURS",
    summary: "Conta falhas nos últimos N horas",
    syntax: "COUNT_FAILURES_LAST_N_HOURS(accountId, 24) GT 5",
    syntaxExplanation: "Falhas de login/3DS/OTP em janela de tempo.",
    story: "8 falhas de login em 1 hora.",
    problem: "Como detectar tentativa de ataque?",
    goldenTip: "💎 Muitas falhas seguidas = ataque de força bruta."
  },

  COUNT_MFA_ABANDONMENTS: {
    name: "COUNT_MFA_ABANDONMENTS",
    summary: "Conta abandonos em MFA",
    syntax: "COUNT_MFA_ABANDONMENTS(userId, DAY_7) GT 2",
    syntaxExplanation: "Usuário inicia MFA e abandona.",
    story: "3 abandonos de MFA em 1 semana.",
    problem: "Como detectar tentativas de bypass?",
    goldenTip: "💎 Abandono repetido pode indicar tentativa de engenharia social."
  },

  COUNT_MFA_DENIALS_LAST_N_HOURS: {
    name: "COUNT_MFA_DENIALS_LAST_N_HOURS",
    summary: "Conta negações de MFA em N horas",
    syntax: "COUNT_MFA_DENIALS_LAST_N_HOURS(userId, 24) GT 1",
    syntaxExplanation: "Usuário recebeu push e negou.",
    story: "Usuário negou MFA 2x em 2 horas.",
    problem: "Como detectar ATO com push?",
    goldenTip: "💎 MFA denial é sinal forte de ATO."
  },

  COUNT_UNIQUE_BENEFICIARIES_LAST_N_DAYS: {
    name: "COUNT_UNIQUE_BENEFICIARIES_LAST_N_DAYS",
    summary: "Conta beneficiários únicos em N dias",
    syntax: "COUNT_UNIQUE_BENEFICIARIES_LAST_N_DAYS(customerId, 30) GT 5",
    syntaxExplanation: "Muitos destinatários novos em pouco tempo.",
    story: "Cliente transferiu para 12 pessoas diferentes em 1 mês.",
    problem: "Como detectar dispersão de transferências?",
    goldenTip: "💎 Destinatários únicos altos = mule distribution."
  },

  COUNT_UNIQUE_IPS_LAST_N_HOURS: {
    name: "COUNT_UNIQUE_IPS_LAST_N_HOURS",
    summary: "Conta IPs únicos em N horas",
    syntax: "COUNT_UNIQUE_IPS_LAST_N_HOURS(userId, 24) GT 3",
    syntaxExplanation: "Muitos IPs acessando a mesma conta.",
    story: "Conta acessada de 5 IPs em 1 dia.",
    problem: "Como detectar compartilhamento ou sequestro de conta?",
    goldenTip: "💎 IPs múltiplos + device novo = risco alto."
  },

  CPF_SSN_VALIDATION: {
    name: "CPF_SSN_VALIDATION",
    summary: "Valida CPF/SSN conforme país",
    syntax: "CPF_SSN_VALIDATION(document, 'BR') IS_VALID",
    syntaxExplanation: "Aplica algoritmo de dígito verificador.",
    story: "CPF inválido detectado no onboarding.",
    problem: "Como validar documentos nacionais?",
    goldenTip: "💎 Validação matemática não garante existência. Use bases oficiais."
  },

  CREDIT_FILE_THIN: {
    name: "CREDIT_FILE_THIN",
    summary: "Detecta credit file fino (pouco histórico)",
    syntax: "CREDIT_FILE_THIN(customerId) IS_TRUE",
    syntaxExplanation: "Poucas linhas no bureau = perfil pouco confiável.",
    story: "Cliente sem histórico de crédito = risco maior.",
    problem: "Como identificar clientes sem histórico financeiro?",
    goldenTip: "💎 Thin file + alto valor = aumentar fricção."
  },

  CREDITOR_NAME_VALIDATION: {
    name: "CREDITOR_NAME_VALIDATION",
    summary: "Valida nome do credor/beneficiário",
    syntax: "CREDITOR_NAME_VALIDATION(name) IS_VALID",
    syntaxExplanation: "Verifica formato, caracteres e existência básica.",
    story: "Nome do credor com caracteres inválidos = erro.",
    problem: "Como validar dados do credor?",
    goldenTip: "💎 Combine com NAME_SIMILARITY e CoP."
  },

  CRYPTO_PUMP_DUMP_DETECTION: {
    name: "CRYPTO_PUMP_DUMP_DETECTION",
    summary: "Detecta padrão de pump-and-dump em cripto",
    syntax: "CRYPTO_PUMP_DUMP_DETECTION(transactions) IS_TRUE",
    syntaxExplanation: "Volume anormal + movimentos rápidos.",
    story: "Token sobe 300% em 2h com volume artificial.",
    problem: "Como identificar manipulação de mercado cripto?",
    goldenTip: "💎 Pump/dump = volume repentino + queda rápida."
  },

  CVV_FAILURE_VELOCITY: {
    name: "CVV_FAILURE_VELOCITY",
    summary: "Detecta falhas de CVV em alta velocidade",
    syntax: "CVV_FAILURE_VELOCITY(cardId, HOUR_1) GT 3",
    syntaxExplanation: "Muitas tentativas de CVV erradas.",
    story: "Fraudador tenta diferentes CVVs para acertar.",
    problem: "Como detectar tentativas de adivinhação de CVV?",
    goldenTip: "💎 3 falhas de CVV = bloquear temporariamente."
  },

  DAILY_LIMIT_PROXIMITY: {
    name: "DAILY_LIMIT_PROXIMITY",
    summary: "Verifica proximidade do limite diário",
    syntax: "DAILY_LIMIT_PROXIMITY(accountId) GT 0.9",
    syntaxExplanation: "Consumo de 90% do limite diário.",
    story: "Conta atingindo limite máximo = possível drenagem.",
    problem: "Como detectar aproximação de limites?",
    goldenTip: "💎 Avisar cliente ao chegar em 80% evita bloqueios surpresa."
  },

  DATE_AFTER: {
    name: "DATE_AFTER",
    summary: "Verifica se data é posterior a outra",
    syntax: "transaction.date DATE_AFTER '2024-01-01'",
    syntaxExplanation: "Comparação de datas sem considerar hora.",
    story: "Filtrar transações após 1º de janeiro.",
    problem: "Como filtrar por datas posteriores?",
    goldenTip: "💎 Use DATE_BEFORE para o oposto."
  },

  DATE_BEFORE: {
    name: "DATE_BEFORE",
    summary: "Verifica se data é anterior a outra",
    syntax: "transaction.date DATE_BEFORE '2024-01-01'",
    syntaxExplanation: "Comparação de datas sem considerar hora.",
    story: "Selecionar eventos antes de uma data de corte.",
    problem: "Como filtrar por datas anteriores?",
    goldenTip: "💎 Combine com DATE_AFTER para intervalos."
  },

  DAY_OF_WEEK_IN: {
    name: "DAY_OF_WEEK_IN",
    summary: "Verifica se dia da semana está em um conjunto",
    syntax: "transaction.date DAY_OF_WEEK_IN ['SATURDAY','SUNDAY']",
    syntaxExplanation: "Retorna true se for fim de semana.",
    story: "Regras específicas para sábado/domingo.",
    problem: "Como aplicar regras por dia da semana?",
    goldenTip: "💎 Use com BUSINESS_HOURS_DEVIATION para granularidade."
  },

  DAYS_SINCE_LAST_ACTIVITY: {
    name: "DAYS_SINCE_LAST_ACTIVITY",
    summary: "Calcula dias desde última atividade",
    syntax: "DAYS_SINCE_LAST_ACTIVITY(customerId) GT 180",
    syntaxExplanation: "Dias desde última transação/login.",
    story: "Conta inativa há 1 ano volta a operar.",
    problem: "Como identificar contas dormentes?",
    goldenTip: "💎 Dormant + transação alta = alto risco."
  },

  DECIMAL_PLACES_GT: {
    name: "DECIMAL_PLACES_GT",
    summary: "Verifica se número tem mais casas decimais que o normal",
    syntax: "DECIMAL_PLACES_GT(amount, 2)",
    syntaxExplanation: "Valores financeiros normalmente têm 2 casas.",
    story: "Valor com 5 casas decimais indica manipulação.",
    problem: "Como detectar valores com precisão anômala?",
    goldenTip: "💎 Cripto pode ter muitas casas. Use regra por produto."
  },

  DEVICE_ACCOUNT_RATIO: {
    name: "DEVICE_ACCOUNT_RATIO",
    summary: "Mede relação de contas por device",
    syntax: "DEVICE_ACCOUNT_RATIO(deviceId, DAY_30) GT 5",
    syntaxExplanation: "Device usado por muitas contas diferentes.",
    story: "Mesmo device acessa 12 contas = fraude organizada.",
    problem: "Como detectar device compartilhado?",
    goldenTip: "💎 Device com muitas contas = provável farm."
  },

  DEVICE_CHANGED_IN_SESSION: {
    name: "DEVICE_CHANGED_IN_SESSION",
    summary: "Detecta mudança de device dentro da sessão",
    syntax: "DEVICE_CHANGED_IN_SESSION(session) IS_TRUE",
    syntaxExplanation: "Fingerprint mudou durante sessão.",
    story: "Token roubado e usado em outro device.",
    problem: "Como detectar session hijack?",
    goldenTip: "💎 Session binding reduz risco de troca de device."
  },

  DEVICE_FINGERPRINT_CONSISTENCY_CHECK: {
    name: "DEVICE_FINGERPRINT_CONSISTENCY_CHECK",
    summary: "Verifica consistência do fingerprint do device",
    syntax: "DEVICE_FINGERPRINT_CONSISTENCY_CHECK(device) IS_CONSISTENT",
    syntaxExplanation: "Compara atributos: canvas, fonts, webgl, etc.",
    story: "Canvas diz Windows, fonts indicam Mac = inconsistente.",
    problem: "Como detectar spoofing de fingerprint?",
    goldenTip: "💎 Inconsistência sugere anti-detect ou VM."
  },

  DEVICE_MEMORY_ANOMALY: {
    name: "DEVICE_MEMORY_ANOMALY",
    summary: "Detecta memória do device anormal",
    syntax: "DEVICE_MEMORY_ANOMALY(device) IS_TRUE",
    syntaxExplanation: "Memória reportada não condiz com perfil do device.",
    story: "iPhone reportando 1TB RAM = spoofing.",
    problem: "Como detectar atributos falsos?",
    goldenTip: "💎 Combine com HARDWARE_CONCURRENCY_MISMATCH."
  },

  DEVICE_TRUST_SCORE: {
    name: "DEVICE_TRUST_SCORE",
    summary: "Score de confiança do device",
    syntax: "DEVICE_TRUST_SCORE(deviceId) LT 30",
    syntaxExplanation: "Score baixo = device novo ou suspeito.",
    story: "Device recém-criado com score 10.",
    problem: "Como decidir confiança de device?",
    goldenTip: "💎 Score aumenta com uso legítimo contínuo."
  },

  DOMAIN_IN_LIST: {
    name: "DOMAIN_IN_LIST",
    summary: "Verifica se domínio está em lista",
    syntax: "DOMAIN_IN_LIST(email, 'disposable_domains') IS_TRUE",
    syntaxExplanation: "Domínio de email temporário ou bloqueado.",
    story: "Email @tempmail.com = alto risco.",
    problem: "Como bloquear domínios descartáveis?",
    goldenTip: "💎 Domínios descartáveis são usados em fraudes de onboarding."
  },

  DORA_INCIDENT_SEVERITY: {
    name: "DORA_INCIDENT_SEVERITY",
    summary: "Classifica severidade de incidentes (DORA/EU)",
    syntax: "DORA_INCIDENT_SEVERITY(incident) EQ 'MAJOR'",
    syntaxExplanation: "Classificação para reporte regulatório.",
    story: "Indisponibilidade > 2h = major incident.",
    problem: "Como classificar incidentes operacionais?",
    goldenTip: "💎 DORA exige reporte rápido para incidentes graves."
  },

  DORMANCY_ALERT_VELOCITY: {
    name: "DORMANCY_ALERT_VELOCITY",
    summary: "Detecta reativação de conta dormente com volume alto",
    syntax: "DORMANCY_ALERT_VELOCITY(customerId) IS_TRUE",
    syntaxExplanation: "Conta sem atividade e de repente alta frequência.",
    story: "Conta inativa 6 meses faz 10 TXs em 1 hora.",
    problem: "Como detectar reativação suspeita?",
    goldenTip: "💎 Dormant + spike = alerta vermelho."
  },

  DORMANCY_REVIVAL: {
    name: "DORMANCY_REVIVAL",
    summary: "Detecta revival de conta dormente",
    syntax: "DORMANCY_REVIVAL(customerId) IS_TRUE",
    syntaxExplanation: "Atividade após longo período de inatividade.",
    story: "Conta inativa 1 ano volta a operar.",
    problem: "Como identificar contas que “voltam do nada”?",
    goldenTip: "💎 Exija reautenticação forte em contas dormentes."
  },

  ECBSV_SSN_VALIDATION: {
    name: "ECBSV_SSN_VALIDATION",
    summary: "Valida SSN conforme regras ECB/SEPA",
    syntax: "ECBSV_SSN_VALIDATION(ssn) IS_VALID",
    syntaxExplanation: "Validação de formato e dígitos de SSN.",
    story: "SSN inválido detectado na abertura de conta.",
    problem: "Como validar documentos europeus?",
    goldenTip: "💎 Sempre validar formato e país emissor."
  },

  ECLAT_ITEMSET: {
    name: "ECLAT_ITEMSET",
    summary: "Detecta itemsets frequentes (ECLAT algorithm)",
    syntax: "ECLAT_ITEMSET(events, MIN_SUPPORT=0.1) HAS_PATTERN",
    syntaxExplanation: "Descobre combinações recorrentes de sinais.",
    story: "Device novo + IP datacenter + horário noturno = pattern frequente.",
    problem: "Como descobrir padrões recorrentes automaticamente?",
    goldenTip: "💎 Use ECLAT para mineração de regras antes do Apriori."
  },

  ECOMMERCE_NO_AVS: {
    name: "ECOMMERCE_NO_AVS",
    summary: "Detecta transação e-commerce sem AVS",
    syntax: "ECOMMERCE_NO_AVS(transaction) IS_TRUE",
    syntaxExplanation: "Sem verificação de endereço (AVS) aumenta risco.",
    story: "E-commerce processa sem AVS = maior chargeback.",
    problem: "Como identificar risco por ausência de AVS?",
    goldenTip: "💎 Sem AVS em valor alto = exigir 3DS."
  },

  EIDAS_ASSURANCE_LEVEL: {
    name: "EIDAS_ASSURANCE_LEVEL",
    summary: "Verifica nível de garantia eIDAS",
    syntax: "EIDAS_ASSURANCE_LEVEL(identity) EQ 'HIGH'",
    syntaxExplanation: "eIDAS: LOW, SUBSTANTIAL, HIGH.",
    story: "Ação crítica exige nível HIGH.",
    problem: "Como aplicar requisitos de identidade na UE?",
    goldenTip: "💎 Para ações críticas, exija SUBSTANTIAL ou HIGH."
  },

  EMAIL_DOMAIN_AGE: {
    name: "EMAIL_DOMAIN_AGE",
    summary: "Calcula idade do domínio de email",
    syntax: "EMAIL_DOMAIN_AGE(email) LT 30",
    syntaxExplanation: "Domínio recém-criado = mais risco.",
    story: "Email de domínio criado ontem.",
    problem: "Como detectar domínios novos?",
    goldenTip: "💎 Domínio < 30 dias é forte indicador de fraude."
  },

  EMAIL_DOMAIN_AGE_LT_DAYS: {
    name: "EMAIL_DOMAIN_AGE_LT_DAYS",
    summary: "Verifica se domínio tem menos de N dias",
    syntax: "EMAIL_DOMAIN_AGE_LT_DAYS(email, 30) IS_TRUE",
    syntaxExplanation: "Atalho para regra de domínio novo.",
    story: "Domínio com 5 dias de idade.",
    problem: "Como aplicar cutoff direto de idade?",
    goldenTip: "💎 Combine com reputação do domínio."
  },

  EMAIL_PHONE_MISMATCH: {
    name: "EMAIL_PHONE_MISMATCH",
    summary: "Detecta incompatibilidade entre email e telefone",
    syntax: "EMAIL_PHONE_MISMATCH(email, phone) IS_TRUE",
    syntaxExplanation: "País do domínio/email não condiz com DDI do telefone.",
    story: "Email @ru com telefone +55.",
    problem: "Como detectar inconsistência de dados?",
    goldenTip: "💎 Mismatch é sinal, mas pode ser expatriado. Use contexto."
  }
};
