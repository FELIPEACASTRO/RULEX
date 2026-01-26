/**
 * OPERATOR_SPECS_COMPLETE - PARTE 4
 * Continuação da documentação ULTRA DIDÁTICA
 * 
 * BSL/SANCTIONS, ADDRESS/NAME, PLATFORM, STRING_ADVANCED, LIST_CHECKING
 */

import type { OperatorSpec } from './operatorSpecs';

// ═══════════════════════════════════════════════════════════════════════════════════════
// PARTE 17: OPERADORES BSL/SANCTIONS (20 operadores - Compliance & Listas)
// ═══════════════════════════════════════════════════════════════════════════════════════

export const BSL_SANCTIONS_SPECS: Record<string, OperatorSpec> = {
  OFAC_LIST_CHECK: {
    name: "OFAC_LIST_CHECK",
    summary: "Verifica se entidade está na lista OFAC (EUA)",
    syntax: "OFAC_LIST_CHECK(name, country) MATCH",
    syntaxExplanation: "Consulta lista OFAC SDN (Specially Designated Nationals). Match = bloqueio.",
    story: "Nome + país batem com entrada OFAC = TX bloqueada + SAR obrigatório.",
    problem: "Como cumprir sanções OFAC dos EUA?",
    goldenTip: "💎 OFAC tem múltiplas listas:\n• SDN (principais)\n• SSI (Rússia)\n• FSE (foreign sanctions evaders)\nConsulte TODAS.",
    engineBehavior: {
      description: "Consulta OFAC:",
      steps: [
        "1. Normaliza nome (remove acentos, aliases)",
        "2. Fuzzy match contra lista SDN",
        "3. Verifica país/endereço",
        "4. Score de confiança do match",
        "5. Se score > 85% → MATCH"
      ],
      cautions: ["Falsos positivos comuns. Mohammed, Jose, etc. Revisar manualmente."]
    }
  },

  PEP_LIST_CHECK: {
    name: "PEP_LIST_CHECK",
    summary: "Verifica se pessoa é PEP (Politically Exposed Person)",
    syntax: "PEP_LIST_CHECK(name, country) IS_PEP",
    syntaxExplanation: "Consulta bases de PEPs. PEP = EDD (Enhanced Due Diligence) obrigatório.",
    story: "Cliente é ex-ministro = PEP = monitoramento reforçado.",
    problem: "Como identificar pessoas politicamente expostas?",
    goldenTip: "💎 PEP inclui:\n• Políticos\n• Familiares de políticos\n• Associados próximos\nPermanece PEP por anos após deixar cargo.",
    realScenarios: [
      {
        title: "Detecção de PEP em Onboarding",
        context: "Novo cliente abre conta",
        problem: "Cliente pode ser PEP e não declarar",
        solution: "PEP_LIST_CHECK automático contra bases internacionais",
        impact: "Evita multas de compliance. EDD aplicado para PEPs."
      }
    ]
  },

  SANCTIONS_COUNTRY_CHECK: {
    name: "SANCTIONS_COUNTRY_CHECK",
    summary: "Verifica se país está sob SANÇÕES",
    syntax: "SANCTIONS_COUNTRY_CHECK(country) SANCTIONED",
    syntaxExplanation: "País sob sanções internacionais (OFAC, UE, ONU).",
    story: "TX para Coreia do Norte = bloqueio total.",
    problem: "Como bloquear TXs para países sancionados?",
    goldenTip: "💎 Países totalmente sancionados:\n• Coreia do Norte\n• Irã\n• Síria\n• Cuba (parcial)\n• Crimeia\nLista muda - atualizar frequentemente."
  },

  EU_SANCTIONS_CHECK: {
    name: "EU_SANCTIONS_CHECK",
    summary: "Verifica lista de sanções da UNIÃO EUROPEIA",
    syntax: "EU_SANCTIONS_CHECK(name, country) MATCH",
    syntaxExplanation: "Consulta lista consolidada de sanções da UE.",
    story: "Oligarca russo na lista UE = bloqueio na Europa.",
    problem: "Como cumprir sanções europeias?",
    goldenTip: "💎 Sanções UE podem diferir de OFAC. Verificar AMBAS para compliance global."
  },

  UN_SANCTIONS_CHECK: {
    name: "UN_SANCTIONS_CHECK",
    summary: "Verifica lista de sanções da ONU",
    syntax: "UN_SANCTIONS_CHECK(name) MATCH",
    syntaxExplanation: "Consulta lista consolidada de sanções do Conselho de Segurança da ONU.",
    story: "Terrorista na lista ONU = bloqueio global.",
    problem: "Como cumprir sanções internacionais da ONU?",
    goldenTip: "💎 Sanções ONU são obrigatórias para todos estados-membros."
  },

  ADVERSE_MEDIA_CHECK: {
    name: "ADVERSE_MEDIA_CHECK",
    summary: "Verifica menções NEGATIVAS na mídia",
    syntax: "ADVERSE_MEDIA_CHECK(name) HAS_HITS",
    syntaxExplanation: "Busca notícias negativas: fraude, corrupção, crimes, etc.",
    story: "Cliente mencionado em investigação de lavagem de dinheiro na mídia.",
    problem: "Como identificar clientes com problemas reputacionais?",
    goldenTip: "💎 Adverse media é indicador precoce. Notícia hoje = sanção amanhã."
  },

  WATCH_LIST_CHECK: {
    name: "WATCH_LIST_CHECK",
    summary: "Verifica múltiplas LISTAS DE OBSERVAÇÃO",
    syntax: "WATCH_LIST_CHECK(name) MATCH_ANY",
    syntaxExplanation: "Consulta consolidada: OFAC, EU, UN, PEP, Interpol, etc.",
    story: "One-stop-check em todas as principais listas.",
    problem: "Como fazer verificação completa de compliance?",
    goldenTip: "💎 Consolide todas as listas em uma verificação. Mais eficiente + menos falhas."
  },

  INTERNAL_BLACKLIST_CHECK: {
    name: "INTERNAL_BLACKLIST_CHECK",
    summary: "Verifica LISTA NEGRA interna da instituição",
    syntax: "INTERNAL_BLACKLIST_CHECK(customerId) IS_BLACKLISTED",
    syntaxExplanation: "Cliente está na nossa lista interna de bloqueio.",
    story: "Cliente fraudou há 2 anos = está na blacklist interna.",
    problem: "Como manter e consultar lista interna de bloqueio?",
    goldenTip: "💎 Blacklist interna: mais ágil que listas externas. Fraudador confirmado → adiciona imediatamente."
  },

  RELATED_PARTY_SANCTIONS: {
    name: "RELATED_PARTY_SANCTIONS",
    summary: "Verifica se PARTES RELACIONADAS estão sancionadas",
    syntax: "RELATED_PARTY_SANCTIONS(customerId) HAS_SANCTIONED_RELATION",
    syntaxExplanation: "Cônjuge, sócio, familiar em lista de sanções?",
    story: "Cliente limpo, mas esposa na lista OFAC = risco.",
    problem: "Como verificar rede de relacionamentos?",
    goldenTip: "💎 Sanções secundárias: quem negocia com sancionado também pode ser sancionado."
  },

  DUAL_USE_GOODS_CHECK: {
    name: "DUAL_USE_GOODS_CHECK",
    summary: "Verifica se TX envolve bens de USO DUAL (militar/civil)",
    syntax: "DUAL_USE_GOODS_CHECK(transaction) HAS_DUAL_USE",
    syntaxExplanation: "Produtos que podem ter uso militar requerem licença de exportação.",
    story: "Venda de software de criptografia para país sancionado = dual use.",
    problem: "Como identificar transações de bens controlados?",
    goldenTip: "💎 Dual use: químicos, software, equipamentos, tecnologia nuclear, etc."
  },

  SECONDARY_SANCTIONS_RISK: {
    name: "SECONDARY_SANCTIONS_RISK",
    summary: "Avalia risco de SANÇÕES SECUNDÁRIAS",
    syntax: "SECONDARY_SANCTIONS_RISK(transaction) RISK_LEVEL GT MEDIUM",
    syntaxExplanation: "TX com entidade que negocia com sancionados = risco secundário.",
    story: "Empresa chinesa que fornece para Irã = você pode ser sancionado por negociar com ela.",
    problem: "Como avaliar risco de sanções secundárias?",
    goldenTip: "💎 Sanções secundárias: EUA podem sancionar não-americanos por negociar com sancionados."
  },

  SANCTIONS_SCREENING_SCORE: {
    name: "SANCTIONS_SCREENING_SCORE",
    summary: "Score consolidado de SCREENING de sanções",
    syntax: "SANCTIONS_SCREENING_SCORE(entity) GT 70",
    syntaxExplanation: "Score 0-100 baseado em múltiplas listas e critérios.",
    story: "Score 85 = múltiplos hits em listas = alto risco.",
    problem: "Como ter métrica única de risco de sanções?",
    goldenTip: "💎 Score combina:\n• Match em listas\n• País de risco\n• PEP status\n• Adverse media\n• Conexões suspeitas"
  },

  COMPLIANCE_REVIEW_REQUIRED: {
    name: "COMPLIANCE_REVIEW_REQUIRED",
    summary: "Determina se REVISÃO DE COMPLIANCE é necessária",
    syntax: "COMPLIANCE_REVIEW_REQUIRED(transaction) IS_TRUE",
    syntaxExplanation: "TX requer revisão manual do time de compliance.",
    story: "TX de $1M para empresa em país de risco = review obrigatório.",
    problem: "Como escalar casos para revisão humana?",
    goldenTip: "💎 Auto-approve até certo risco. Acima = humano decide."
  },

  SAR_FILING_REQUIRED: {
    name: "SAR_FILING_REQUIRED",
    summary: "Determina se SAR (Suspicious Activity Report) é obrigatório",
    syntax: "SAR_FILING_REQUIRED(case) IS_TRUE",
    syntaxExplanation: "Atividade suspeita detectada = SAR obrigatório ao regulador.",
    story: "Padrão de lavagem detectado = SAR em 30 dias.",
    problem: "Como determinar obrigação de reportar?",
    goldenTip: "💎 SAR é obrigatório quando há suspeita razoável. Não reportar = crime."
  },

  CTR_FILING_REQUIRED: {
    name: "CTR_FILING_REQUIRED",
    summary: "Determina se CTR (Currency Transaction Report) é obrigatório",
    syntax: "CTR_FILING_REQUIRED(cashTransaction) IS_TRUE",
    syntaxExplanation: "TX em dinheiro > $10,000 = CTR obrigatório (EUA).",
    story: "Depósito de $15,000 em cash = CTR automático.",
    problem: "Como cumprir requisitos de CTR?",
    goldenTip: "💎 CTR é automático para cash > $10k. Structuring para evitar = crime."
  },

  ENHANCED_DUE_DILIGENCE: {
    name: "ENHANCED_DUE_DILIGENCE",
    summary: "Determina necessidade de EDD (Enhanced Due Diligence)",
    syntax: "ENHANCED_DUE_DILIGENCE(customer) REQUIRED",
    syntaxExplanation: "Cliente de alto risco requer verificação aprofundada.",
    story: "PEP + país de alto risco = EDD obrigatório.",
    problem: "Como determinar nível de due diligence?",
    goldenTip: "💎 EDD triggers:\n• PEP\n• País alto risco\n• Valor alto\n• Estrutura complexa\n• Atividade incomum"
  },

  ONGOING_MONITORING_LEVEL: {
    name: "ONGOING_MONITORING_LEVEL",
    summary: "Determina NÍVEL de monitoramento contínuo",
    syntax: "ONGOING_MONITORING_LEVEL(customer) EQ HIGH",
    syntaxExplanation: "Cliente de risco = monitoramento mais frequente.",
    story: "Cliente PEP = revisão trimestral em vez de anual.",
    problem: "Como calibrar frequência de monitoramento?",
    goldenTip: "💎 Níveis:\n• LOW: anual\n• MEDIUM: semestral\n• HIGH: trimestral\n• CRITICAL: contínuo"
  },

  SOURCE_OF_FUNDS_VERIFICATION: {
    name: "SOURCE_OF_FUNDS_VERIFICATION",
    summary: "Verifica ORIGEM DOS RECURSOS",
    syntax: "SOURCE_OF_FUNDS_VERIFICATION(transaction) VERIFIED",
    syntaxExplanation: "Documentação de origem dos recursos foi verificada?",
    story: "TX de $500k = precisa provar de onde veio o dinheiro.",
    problem: "Como verificar origem lícita de recursos?",
    goldenTip: "💎 SOF para valores altos: holerite, contrato venda imóvel, herança documentada."
  },

  SOURCE_OF_WEALTH_VERIFICATION: {
    name: "SOURCE_OF_WEALTH_VERIFICATION",
    summary: "Verifica ORIGEM DO PATRIMÔNIO",
    syntax: "SOURCE_OF_WEALTH_VERIFICATION(customer) VERIFIED",
    syntaxExplanation: "Patrimônio total do cliente é compatível com perfil declarado?",
    story: "Cliente declara ser professor mas tem patrimônio de R$ 10M = investigar.",
    problem: "Como verificar compatibilidade de patrimônio?",
    goldenTip: "💎 SOW diferente de SOF:\n• SOF: de onde veio ESTA TX\n• SOW: de onde veio PATRIMÔNIO TOTAL"
  },

  ULTIMATE_BENEFICIAL_OWNER: {
    name: "ULTIMATE_BENEFICIAL_OWNER",
    summary: "Identifica UBO (Ultimate Beneficial Owner)",
    syntax: "ULTIMATE_BENEFICIAL_OWNER(company) IDENTIFIED",
    syntaxExplanation: "Quem é o dono REAL por trás de estrutura societária?",
    story: "Empresa de empresa de empresa... quem é o dono pessoa física no final?",
    problem: "Como identificar beneficiários finais?",
    goldenTip: "💎 UBO: pessoa física que controla ≥25% ou tem controle efetivo. Obrigatório identificar."
  }
};

// ═══════════════════════════════════════════════════════════════════════════════════════
// PARTE 18: OPERADORES ADDRESS/NAME (15 operadores - Validação de Dados)
// ═══════════════════════════════════════════════════════════════════════════════════════

export const ADDRESS_NAME_SPECS: Record<string, OperatorSpec> = {
  ADDRESS_VERIFICATION: {
    name: "ADDRESS_VERIFICATION",
    summary: "Verifica se ENDEREÇO é válido e existe",
    syntax: "ADDRESS_VERIFICATION(address) IS_VALID",
    syntaxExplanation: "Valida endereço contra base de CEPs/correios.",
    story: "Endereço 'Rua ABC 123' não existe no CEP informado = inválido.",
    problem: "Como validar endereços?",
    goldenTip: "💎 Validação em cascata:\n1. Formato\n2. CEP existe\n3. Logradouro existe no CEP\n4. Número plausível"
  },

  ADDRESS_STANDARDIZATION: {
    name: "ADDRESS_STANDARDIZATION",
    summary: "PADRONIZA endereço para formato canônico",
    syntax: "ADDRESS_STANDARDIZATION(address) STANDARDIZED",
    syntaxExplanation: "Corrige abreviações, formata corretamente.",
    story: "'Av Paulista' → 'Avenida Paulista'. 'R. Augusta' → 'Rua Augusta'.",
    problem: "Como normalizar endereços para comparação?",
    goldenTip: "💎 Padronização permite matching: 'Av.' = 'Avenida' = 'AV'."
  },

  ADDRESS_MISMATCH: {
    name: "ADDRESS_MISMATCH",
    summary: "Detecta INCOMPATIBILIDADE de endereços",
    syntax: "ADDRESS_MISMATCH(billing, shipping) IS_TRUE",
    syntaxExplanation: "Endereço de cobrança diferente do de entrega = risco.",
    story: "Cobrança em SP, entrega em AM = possível fraude.",
    problem: "Como identificar divergência de endereços?",
    goldenTip: "💎 Mismatch não é sempre fraude. Pode ser presente. Mas requer atenção."
  },

  ADDRESS_HIGH_RISK_AREA: {
    name: "ADDRESS_HIGH_RISK_AREA",
    summary: "Verifica se endereço está em ÁREA DE ALTO RISCO",
    syntax: "ADDRESS_HIGH_RISK_AREA(address) IS_TRUE",
    syntaxExplanation: "Endereço em região com alta taxa de fraude/chargebacks.",
    story: "Certos CEPs têm taxa de fraude 10x maior que média.",
    problem: "Como usar geolocalização para risco?",
    goldenTip: "💎 Atualize lista de áreas de risco com dados reais de fraude."
  },

  ADDRESS_COMMERCIAL_VS_RESIDENTIAL: {
    name: "ADDRESS_COMMERCIAL_VS_RESIDENTIAL",
    summary: "Classifica endereço como COMERCIAL ou RESIDENCIAL",
    syntax: "ADDRESS_COMMERCIAL_VS_RESIDENTIAL(address) EQ COMMERCIAL",
    syntaxExplanation: "Endereço é de empresa ou residência?",
    story: "Compra de TV 85' para entrega em escritório em shopping = suspeito.",
    problem: "Como identificar tipo de endereço?",
    goldenTip: "💎 Comercial: sala, loja, galpão, shopping. Residencial: casa, apto, condomínio."
  },

  ADDRESS_PO_BOX: {
    name: "ADDRESS_PO_BOX",
    summary: "Detecta se endereço é CAIXA POSTAL",
    syntax: "ADDRESS_PO_BOX(address) IS_TRUE",
    syntaxExplanation: "Endereço é caixa postal (não é endereço físico real).",
    story: "Entrega para caixa postal = não consegue verificar quem recebe.",
    problem: "Como identificar caixas postais?",
    goldenTip: "💎 PO Box, Caixa Postal, Apartado = não é endereço verificável."
  },

  ADDRESS_TEMPORARY: {
    name: "ADDRESS_TEMPORARY",
    summary: "Detecta endereço TEMPORÁRIO (hotel, Airbnb, etc)",
    syntax: "ADDRESS_TEMPORARY(address) IS_TRUE",
    syntaxExplanation: "Endereço é de hospedagem temporária.",
    story: "Entrega em hotel 5 estrelas = possível fraudador de passagem.",
    problem: "Como identificar endereços não permanentes?",
    goldenTip: "💎 Hotéis, hostels, Airbnb = alto risco. Cliente de passagem."
  },

  NAME_VERIFICATION: {
    name: "NAME_VERIFICATION",
    summary: "Verifica se NOME é válido",
    syntax: "NAME_VERIFICATION(name) IS_VALID",
    syntaxExplanation: "Nome parece real? Não é 'Teste Teste' ou 'ASDFGH'?",
    story: "'João da Silva' = válido. 'XXX YYY' = inválido.",
    problem: "Como detectar nomes fake/teste?",
    goldenTip: "💎 Patterns suspeitos:\n• Nomes muito curtos (AA BB)\n• Repetição (João João)\n• Caracteres estranhos\n• Nomes famosos fake"
  },

  NAME_SIMILARITY_SCORE: {
    name: "NAME_SIMILARITY_SCORE",
    summary: "Calcula SIMILARIDADE entre dois nomes",
    syntax: "NAME_SIMILARITY_SCORE(name1, name2) GT 85",
    syntaxExplanation: "Score de 0-100. 100 = idêntico. 85 = muito similar.",
    story: "'João Silva' vs 'Joao Silva' = 95 (só falta acento).",
    problem: "Como comparar nomes com variações?",
    goldenTip: "💎 Algoritmos: Levenshtein, Jaro-Winkler, Soundex. Combine para melhor resultado.",
    engineBehavior: {
      description: "Cálculo de similaridade:",
      steps: [
        "1. Normaliza ambos (remove acentos, lowercase)",
        "2. Calcula Levenshtein distance",
        "3. Calcula Jaro-Winkler similarity",
        "4. Calcula Soundex (fonética)",
        "5. Score final = média ponderada"
      ]
    }
  },

  NAME_MATCH_BENEFICIARY: {
    name: "NAME_MATCH_BENEFICIARY",
    summary: "Verifica se nome CONFERE com beneficiário",
    syntax: "NAME_MATCH_BENEFICIARY(senderName, recipientAccountName) MATCH",
    syntaxExplanation: "Nome informado confere com titular da conta destino?",
    story: "Transfere para 'Maria Santos' mas conta é de 'José Silva' = mismatch.",
    problem: "Como verificar nome do beneficiário?",
    goldenTip: "💎 CoP (Confirmation of Payee) obrigatório em muitos países. Reduz APP fraud."
  },

  NAME_FUZZY_MATCH: {
    name: "NAME_FUZZY_MATCH",
    summary: "Match FUZZY (aproximado) de nomes",
    syntax: "NAME_FUZZY_MATCH(searchName, databaseName) GT 80",
    syntaxExplanation: "Match considerando erros de digitação, variações.",
    story: "'Joao' encontra 'João', 'JOAO', 'Jo@o'.",
    problem: "Como encontrar nomes com variações/erros?",
    goldenTip: "💎 Fuzzy match essencial para sanções. 'Osama' = 'Usama' = 'Oussama'."
  },

  CPF_VALIDATION: {
    name: "CPF_VALIDATION",
    summary: "Valida CPF brasileiro (dígitos verificadores)",
    syntax: "CPF_VALIDATION(cpf) IS_VALID",
    syntaxExplanation: "CPF válido matematicamente (2 dígitos verificadores corretos).",
    story: "CPF 123.456.789-09 = válido ou inválido?",
    problem: "Como validar CPF?",
    goldenTip: "💎 CPF válido ≠ CPF existente. Validação matemática é só 1º passo."
  },

  CNPJ_VALIDATION: {
    name: "CNPJ_VALIDATION",
    summary: "Valida CNPJ brasileiro (dígitos verificadores)",
    syntax: "CNPJ_VALIDATION(cnpj) IS_VALID",
    syntaxExplanation: "CNPJ válido matematicamente + existe na Receita Federal.",
    story: "CNPJ de empresa fantasma = inválido na RF.",
    problem: "Como validar CNPJ?",
    goldenTip: "💎 Validar CNPJ na RF: situação cadastral, data abertura, atividade."
  },

  DOCUMENT_EXPIRY_CHECK: {
    name: "DOCUMENT_EXPIRY_CHECK",
    summary: "Verifica se DOCUMENTO está vencido",
    syntax: "DOCUMENT_EXPIRY_CHECK(document) IS_EXPIRED",
    syntaxExplanation: "RG, CNH, passaporte vencido?",
    story: "CNH vencida há 2 anos = documento inválido.",
    problem: "Como verificar validade de documentos?",
    goldenTip: "💎 Documento vencido = identidade não confirmada. Solicitar atualização."
  },

  DOCUMENT_FRAUD_DETECTION: {
    name: "DOCUMENT_FRAUD_DETECTION",
    summary: "Detecta FRAUDE em documento",
    syntax: "DOCUMENT_FRAUD_DETECTION(documentImage) FRAUD_DETECTED",
    syntaxExplanation: "Análise de imagem do documento: adulteração, fotoshop, etc.",
    story: "RG com foto colada, data alterada = fraude detectada.",
    problem: "Como detectar documentos falsificados?",
    goldenTip: "💎 Verificações:\n• Fontes consistentes\n• Hologramas\n• Luz UV virtual\n• Metadata da imagem\n• Compression artifacts"
  }
};

// ═══════════════════════════════════════════════════════════════════════════════════════
// PARTE 19: OPERADORES PLATFORM/INFRASTRUCTURE (15 operadores)
// ═══════════════════════════════════════════════════════════════════════════════════════

export const PLATFORM_SPECS: Record<string, OperatorSpec> = {
  API_RATE_LIMIT_EXCEEDED: {
    name: "API_RATE_LIMIT_EXCEEDED",
    summary: "Detecta excesso de RATE LIMIT de API",
    syntax: "API_RATE_LIMIT_EXCEEDED(apiKey) IS_TRUE",
    syntaxExplanation: "API key excedeu limite de requests/minuto.",
    story: "API key fazendo 10.000 req/min quando limite é 100 = abuso.",
    problem: "Como detectar abuso de API?",
    goldenTip: "💎 Rate limiting por:\n• IP\n• API key\n• User\n• Endpoint\nCombine para proteção robusta."
  },

  BOT_DETECTION: {
    name: "BOT_DETECTION",
    summary: "Detecta comportamento de BOT automatizado",
    syntax: "BOT_DETECTION(session) IS_BOT",
    syntaxExplanation: "Comportamento não-humano: velocidade, padrões, headless.",
    story: "100 TXs em 10 segundos, sem movimento de mouse = bot.",
    problem: "Como diferenciar humano de bot?",
    goldenTip: "💎 Sinais de bot:\n• Velocidade impossível\n• Padrões regulares\n• Sem eventos de mouse\n• User agent suspeito\n• Headless browser"
  },

  CAPTCHA_REQUIRED: {
    name: "CAPTCHA_REQUIRED",
    summary: "Determina se CAPTCHA é necessário",
    syntax: "CAPTCHA_REQUIRED(session) IS_TRUE",
    syntaxExplanation: "Sessão suspeita → challenge com CAPTCHA.",
    story: "3 tentativas de login falhadas → CAPTCHA na 4ª.",
    problem: "Como introduzir fricção para bots?",
    goldenTip: "💎 CAPTCHA progressivo:\n• Invisible (score-based)\n• Checkbox\n• Image challenge\n• Audio"
  },

  SESSION_HIJACK_DETECTION: {
    name: "SESSION_HIJACK_DETECTION",
    summary: "Detecta possível SEQUESTRO DE SESSÃO",
    syntax: "SESSION_HIJACK_DETECTION(session) HIJACK_DETECTED",
    syntaxExplanation: "Sessão mudou de características (IP, device) = hijack.",
    story: "Sessão em SP de repente opera de NY = token roubado.",
    problem: "Como detectar sessões roubadas?",
    goldenTip: "💎 Session binding: vincule sessão a fingerprint. Mudança = reautenticar."
  },

  CONCURRENT_SESSION_LIMIT: {
    name: "CONCURRENT_SESSION_LIMIT",
    summary: "Verifica LIMITE de sessões simultâneas",
    syntax: "CONCURRENT_SESSION_LIMIT(userId) EXCEEDED",
    syntaxExplanation: "Usuário com mais de N sessões ativas = suspeito.",
    story: "10 sessões ativas do mesmo usuário = compartilhamento de conta?",
    problem: "Como limitar sessões simultâneas?",
    goldenTip: "💎 Limites por tier:\n• Free: 1 sessão\n• Premium: 3 sessões\n• Enterprise: ilimitado"
  },

  IP_REPUTATION_CHECK: {
    name: "IP_REPUTATION_CHECK",
    summary: "Verifica REPUTAÇÃO do IP",
    syntax: "IP_REPUTATION_CHECK(ip) REPUTATION_SCORE LT 30",
    syntaxExplanation: "IP em listas de spam, proxies, tor, etc.",
    story: "IP de data center conhecido por ataques = reputação baixa.",
    problem: "Como avaliar risco de IPs?",
    goldenTip: "💎 IP reputation considera:\n• Histórico de spam\n• É proxy/VPN/Tor?\n• É data center?\n• Atividade passada"
  },

  DATACENTER_IP: {
    name: "DATACENTER_IP",
    summary: "Detecta se IP é de DATA CENTER",
    syntax: "DATACENTER_IP(ip) IS_TRUE",
    syntaxExplanation: "IP pertence a cloud (AWS, Azure, GCP) em vez de ISP residencial.",
    story: "Usuário 'normal' com IP da AWS = suspeito.",
    problem: "Como identificar IPs não-residenciais?",
    goldenTip: "💎 Data center IP = 90%+ é bot ou proxy. Raros usuários legítimos."
  },

  HOSTING_PROVIDER_IP: {
    name: "HOSTING_PROVIDER_IP",
    summary: "Detecta se IP é de PROVEDOR DE HOSTING",
    syntax: "HOSTING_PROVIDER_IP(ip) IS_TRUE",
    syntaxExplanation: "IP de empresa de hosting web.",
    story: "Usuário com IP de GoDaddy/DigitalOcean = proxy.",
    problem: "Como identificar IPs de hosting?",
    goldenTip: "💎 Hosting IP ≈ proxy em 99% dos casos para fraude."
  },

  REQUEST_FINGERPRINT_ANOMALY: {
    name: "REQUEST_FINGERPRINT_ANOMALY",
    summary: "Detecta ANOMALIA no fingerprint do request",
    syntax: "REQUEST_FINGERPRINT_ANOMALY(request) IS_TRUE",
    syntaxExplanation: "Headers, TLS fingerprint, JS capabilities inconsistentes.",
    story: "User-Agent diz Chrome mas TLS fingerprint é curl = spoofing.",
    problem: "Como detectar requests falsificados?",
    goldenTip: "💎 Verifique:\n• User-Agent vs TLS ja3\n• Headers esperados\n• Accept-Language\n• Order dos headers"
  },

  AUTOMATION_TOOL_DETECTED: {
    name: "AUTOMATION_TOOL_DETECTED",
    summary: "Detecta uso de FERRAMENTA DE AUTOMAÇÃO",
    syntax: "AUTOMATION_TOOL_DETECTED(session) IS_TRUE",
    syntaxExplanation: "Selenium, Puppeteer, Playwright detectados.",
    story: "navigator.webdriver = true = automação detectada.",
    problem: "Como detectar browsers automatizados?",
    goldenTip: "💎 Sinais de automação:\n• webdriver flag\n• Missing plugins\n• Headless indicators\n• CDP protocol"
  },

  CREDENTIAL_STUFFING_PATTERN: {
    name: "CREDENTIAL_STUFFING_PATTERN",
    summary: "Detecta padrão de CREDENTIAL STUFFING",
    syntax: "CREDENTIAL_STUFFING_PATTERN(ip) IS_TRUE",
    syntaxExplanation: "Múltiplos logins falhados com diferentes usuários do mesmo IP.",
    story: "1000 tentativas de login com 1000 usuários diferentes = credential stuffing.",
    problem: "Como detectar ataques de credential stuffing?",
    goldenTip: "💎 Patterns:\n• Muitos usuários, 1 IP\n• Taxa de falha alta (>80%)\n• Velocidade alta\n• Sequencial"
  },

  ACCOUNT_ENUMERATION_PATTERN: {
    name: "ACCOUNT_ENUMERATION_PATTERN",
    summary: "Detecta tentativa de ENUMERAÇÃO de contas",
    syntax: "ACCOUNT_ENUMERATION_PATTERN(ip) IS_TRUE",
    syntaxExplanation: "Testando se emails/usernames existem no sistema.",
    story: "Verificando 'joao@', 'maria@', 'jose@'... para descobrir contas válidas.",
    problem: "Como prevenir enumeration attacks?",
    goldenTip: "💎 Defense: sempre retornar mesma mensagem ('email ou senha inválidos')."
  },

  SCRAPING_DETECTION: {
    name: "SCRAPING_DETECTION",
    summary: "Detecta SCRAPING (coleta automatizada de dados)",
    syntax: "SCRAPING_DETECTION(session) SCRAPING_DETECTED",
    syntaxExplanation: "Padrão de navegação indica coleta de dados.",
    story: "Acessa 1000 páginas de produto sem comprar nada = scraper.",
    problem: "Como detectar scrapers?",
    goldenTip: "💎 Sinais de scraping:\n• Muitas páginas, pouca interação\n• Sem JS/CSS\n• Padrão sequencial\n• User-Agent genérico"
  },

  CLICK_FRAUD_DETECTION: {
    name: "CLICK_FRAUD_DETECTION",
    summary: "Detecta FRAUDE DE CLIQUES (ads)",
    syntax: "CLICK_FRAUD_DETECTION(clickEvent) IS_FRAUD",
    syntaxExplanation: "Cliques em anúncios que não são de usuários reais.",
    story: "100 cliques do mesmo IP em anúncio = click fraud.",
    problem: "Como detectar cliques fraudulentos?",
    goldenTip: "💎 Click fraud patterns:\n• Velocidade impossível\n• Mesmo IP\n• Sem conversão\n• Bounce imediato"
  },

  REFERRER_FRAUD_DETECTION: {
    name: "REFERRER_FRAUD_DETECTION",
    summary: "Detecta FRAUDE DE REFERÊNCIA",
    syntax: "REFERRER_FRAUD_DETECTION(request) FRAUD_DETECTED",
    syntaxExplanation: "Referrer forjado para parecer vir de fonte legítima.",
    story: "Diz que veio do Google mas IP e padrão não conferem.",
    problem: "Como detectar referrers falsos?",
    goldenTip: "💎 Valide referrer com:\n• Timing\n• Geolocalização\n• User behavior\n• HTTP headers"
  }
};

// ═══════════════════════════════════════════════════════════════════════════════════════
// PARTE 20: OPERADORES STRING_ADVANCED (12 operadores)
// ═══════════════════════════════════════════════════════════════════════════════════════

export const STRING_ADVANCED_SPECS: Record<string, OperatorSpec> = {
  LEVENSHTEIN_DISTANCE_LT: {
    name: "LEVENSHTEIN_DISTANCE_LT",
    summary: "Verifica se distância de LEVENSHTEIN é menor que N",
    syntax: "LEVENSHTEIN_DISTANCE_LT(str1, str2, 3)",
    syntaxExplanation: "Distância = número de edições (inserir, deletar, substituir). < 3 = muito similar.",
    story: "'João' vs 'Joao' = distância 1 (só o acento). 'João' vs 'Maria' = distância 5.",
    problem: "Como medir similaridade com tolerância a erros?",
    goldenTip: "💎 Levenshtein < 3 para nomes curtos, < 5 para nomes longos."
  },

  JARO_WINKLER_SIMILARITY_GT: {
    name: "JARO_WINKLER_SIMILARITY_GT",
    summary: "Verifica se similaridade JARO-WINKLER é maior que threshold",
    syntax: "JARO_WINKLER_SIMILARITY_GT(str1, str2, 0.85)",
    syntaxExplanation: "Score 0-1. > 0.85 = muito similar. Favorece matches no início.",
    story: "'Johnson' vs 'Jonhson' = 0.97 (erro no meio). 'John' vs 'Mary' = 0.0.",
    problem: "Como encontrar nomes parecidos?",
    goldenTip: "💎 Jaro-Winkler é melhor para nomes que Levenshtein (favorece prefixo comum)."
  },

  SOUNDEX_MATCH: {
    name: "SOUNDEX_MATCH",
    summary: "Verifica se duas strings têm mesmo SOUNDEX (som parecido)",
    syntax: "SOUNDEX_MATCH(name1, name2) IS_TRUE",
    syntaxExplanation: "Soundex codifica sons. 'Smith' e 'Smythe' têm mesmo Soundex.",
    story: "'Robert' e 'Rupert' soam parecido = mesmo Soundex.",
    problem: "Como encontrar nomes que SOAM parecido?",
    goldenTip: "💎 Soundex funciona para inglês. Para português, use Metaphone brasileiro."
  },

  METAPHONE_MATCH: {
    name: "METAPHONE_MATCH",
    summary: "Verifica se strings têm mesmo METAPHONE (fonético avançado)",
    syntax: "METAPHONE_MATCH(name1, name2) IS_TRUE",
    syntaxExplanation: "Metaphone é mais preciso que Soundex para fonética.",
    story: "'Katherine' e 'Catherine' = mesmo Metaphone.",
    problem: "Como fazer matching fonético preciso?",
    goldenTip: "💎 Double Metaphone suporta múltiplas pronúncias (origem do nome)."
  },

  NGRAM_SIMILARITY_GT: {
    name: "NGRAM_SIMILARITY_GT",
    summary: "Verifica similaridade por N-GRAMS",
    syntax: "NGRAM_SIMILARITY_GT(str1, str2, 2, 0.7)",
    syntaxExplanation: "Divide em n-grams e compara overlap. 2-gram de 'hello' = 'he', 'el', 'll', 'lo'.",
    story: "'Banco Brasil' vs 'Banco do Brasil' = alta similaridade de n-grams.",
    problem: "Como comparar strings com palavras adicionais/faltando?",
    goldenTip: "💎 N-grams são bons para:\n• Nomes com 'do', 'da', 'de'\n• Variações de espaçamento\n• Abreviações"
  },

  COSINE_SIMILARITY_GT: {
    name: "COSINE_SIMILARITY_GT",
    summary: "Verifica similaridade de COSSENO entre textos",
    syntax: "COSINE_SIMILARITY_GT(text1, text2, 0.8)",
    syntaxExplanation: "Converte para vetores e calcula ângulo. 1 = idêntico, 0 = nada em comum.",
    story: "Descrições de produtos similares = cosine similarity alta.",
    problem: "Como comparar textos longos?",
    goldenTip: "💎 Cosine similarity é TF-IDF based. Bom para documentos, não para nomes curtos."
  },

  REGEX_EXTRACT: {
    name: "REGEX_EXTRACT",
    summary: "EXTRAI padrão de string usando regex",
    syntax: "REGEX_EXTRACT(text, '\\\\d{3}\\\\.\\\\d{3}\\\\.\\\\d{3}-\\\\d{2}')",
    syntaxExplanation: "Extrai CPF de texto: 'Meu CPF é 123.456.789-09' → '123.456.789-09'.",
    story: "Extrair número de telefone, CPF, CNPJ de texto livre.",
    problem: "Como extrair dados estruturados de texto?",
    goldenTip: "💎 Use grupos de captura para extrair partes específicas."
  },

  STRING_NORMALIZE: {
    name: "STRING_NORMALIZE",
    summary: "NORMALIZA string (remove acentos, lowercase, trim)",
    syntax: "STRING_NORMALIZE(text)",
    syntaxExplanation: "'  João da SILVA  ' → 'joao da silva'.",
    story: "Normalizar antes de comparar para evitar falsos negativos.",
    problem: "Como padronizar strings para comparação?",
    goldenTip: "💎 Normalização:\n1. Trim\n2. Lowercase\n3. Remove acentos\n4. Remove pontuação\n5. Collapse whitespace"
  },

  CONTAINS_PROFANITY: {
    name: "CONTAINS_PROFANITY",
    summary: "Detecta PALAVRÕES/obscenidades no texto",
    syntax: "CONTAINS_PROFANITY(text) IS_TRUE",
    syntaxExplanation: "Busca por palavras ofensivas em lista.",
    story: "Nome de beneficiário com palavrão = rejeitar.",
    problem: "Como filtrar conteúdo ofensivo?",
    goldenTip: "💎 Fraudadores usam palavrões para testar. Pode indicar card testing."
  },

  CONTAINS_TEST_DATA: {
    name: "CONTAINS_TEST_DATA",
    summary: "Detecta DADOS DE TESTE no conteúdo",
    syntax: "CONTAINS_TEST_DATA(text) IS_TRUE",
    syntaxExplanation: "Detecta 'teste', 'test', '123456', 'XXXX', etc.",
    story: "Nome 'Teste Teste' ou endereço '123 Test St' = dados de teste.",
    problem: "Como identificar dados não reais?",
    goldenTip: "💎 Test data patterns:\n• 'teste', 'test'\n• '123', 'abc'\n• 'xxx', 'yyy'\n• 'asdf'\n• 'John Doe'"
  },

  EMAIL_FORMAT_VALID: {
    name: "EMAIL_FORMAT_VALID",
    summary: "Verifica se EMAIL tem formato válido",
    syntax: "EMAIL_FORMAT_VALID(email) IS_TRUE",
    syntaxExplanation: "Valida formato: user@domain.tld",
    story: "'joao@gmail.com' = válido. 'joao@' = inválido.",
    problem: "Como validar formato de email?",
    goldenTip: "💎 Formato válido ≠ email existe. Validar formato é só 1º passo."
  },

  PHONE_FORMAT_VALID: {
    name: "PHONE_FORMAT_VALID",
    summary: "Verifica se TELEFONE tem formato válido",
    syntax: "PHONE_FORMAT_VALID(phone, 'BR') IS_TRUE",
    syntaxExplanation: "Valida formato para país especificado.",
    story: "'+55 11 99999-9999' = válido BR. '1234' = inválido.",
    problem: "Como validar formato de telefone?",
    goldenTip: "💎 Use libphonenumber para validação robusta multi-país."
  }
};

// ═══════════════════════════════════════════════════════════════════════════════════════
// PARTE 21: OPERADORES LIST_CHECKING (10 operadores)
// ═══════════════════════════════════════════════════════════════════════════════════════

export const LIST_CHECKING_SPECS: Record<string, OperatorSpec> = {
  IN_WHITELIST: {
    name: "IN_WHITELIST",
    summary: "Verifica se valor está na LISTA BRANCA",
    syntax: "IN_WHITELIST(merchantId, 'trusted_merchants') IS_TRUE",
    syntaxExplanation: "Valor está em lista de permitidos.",
    story: "Merchant ID está na lista de confiáveis = aprovação facilitada.",
    problem: "Como dar tratamento especial para entidades confiáveis?",
    goldenTip: "💎 Whitelist deve ter critérios de entrada e revisão periódica."
  },

  IN_BLACKLIST: {
    name: "IN_BLACKLIST",
    summary: "Verifica se valor está na LISTA NEGRA",
    syntax: "IN_BLACKLIST(email, 'fraud_emails') IS_TRUE",
    syntaxExplanation: "Valor está em lista de bloqueados.",
    story: "Email já usado em fraude confirmada = na blacklist.",
    problem: "Como bloquear entidades conhecidamente ruins?",
    goldenTip: "💎 Blacklist deve ter processo de remoção (appeals) e TTL."
  },

  IN_GREYLIST: {
    name: "IN_GREYLIST",
    summary: "Verifica se valor está na LISTA CINZA (suspeitos)",
    syntax: "IN_GREYLIST(ip, 'suspicious_ips') IS_TRUE",
    syntaxExplanation: "Valor está em lista de suspeitos (não confirmados).",
    story: "IP com atividade suspeita mas não confirmada = greylist.",
    problem: "Como tratar entidades suspeitas mas não confirmadas?",
    goldenTip: "💎 Greylist: monitoramento reforçado, não bloqueio. Pode virar white ou black."
  },

  LIST_LOOKUP: {
    name: "LIST_LOOKUP",
    summary: "Busca valor em LISTA genérica",
    syntax: "LIST_LOOKUP(value, 'my_custom_list') FOUND",
    syntaxExplanation: "Busca valor em lista customizada.",
    story: "Verificar se CEP está em lista de áreas de risco.",
    problem: "Como consultar listas customizadas?",
    goldenTip: "💎 Listas devem ter:\n• Nome descritivo\n• Documentação\n• Owner\n• Data de atualização"
  },

  LIST_ADD: {
    name: "LIST_ADD",
    summary: "ADICIONA valor a uma lista",
    syntax: "LIST_ADD(email, 'suspicious_emails')",
    syntaxExplanation: "Adiciona valor à lista especificada.",
    story: "Email flagged 3x = adicionar à lista de suspeitos.",
    problem: "Como atualizar listas dinamicamente?",
    goldenTip: "💎 Automatize adições com regras. Ex: 3 flags = auto-add to greylist."
  },

  LIST_REMOVE: {
    name: "LIST_REMOVE",
    summary: "REMOVE valor de uma lista",
    syntax: "LIST_REMOVE(email, 'suspicious_emails')",
    syntaxExplanation: "Remove valor da lista especificada.",
    story: "Cliente proveu documentação = remover da greylist.",
    problem: "Como permitir saída de listas?",
    goldenTip: "💎 Remoção deve ter:\n• Justificativa\n• Aprovação\n• Log de auditoria"
  },

  LIST_TTL_CHECK: {
    name: "LIST_TTL_CHECK",
    summary: "Verifica TTL (tempo de vida) de entrada em lista",
    syntax: "LIST_TTL_CHECK(entry, 'blacklist') EXPIRED",
    syntaxExplanation: "Entrada na lista expirou? (TTL passado).",
    story: "Blacklist entry de 2019 = provavelmente expirada.",
    problem: "Como gerenciar expiração de entradas?",
    goldenTip: "💎 TTL por lista:\n• Blacklist: 2-5 anos\n• Greylist: 30-90 dias\n• Whitelist: 1 ano + revisão"
  },

  LIST_COUNT: {
    name: "LIST_COUNT",
    summary: "Conta quantas vezes valor aparece em lista",
    syntax: "LIST_COUNT(email, 'all_lists') GT 0",
    syntaxExplanation: "Quantas listas contêm este valor?",
    story: "Email em 5 listas diferentes = muito flagged.",
    problem: "Como medir exposição a listas?",
    goldenTip: "💎 Multi-list hit = risco multiplicado. Score baseado em número de listas."
  },

  SHARED_LIST_CHECK: {
    name: "SHARED_LIST_CHECK",
    summary: "Verifica LISTA COMPARTILHADA entre instituições",
    syntax: "SHARED_LIST_CHECK(cardBin, 'industry_shared') FOUND",
    syntaxExplanation: "Consulta lista compartilhada entre múltiplas instituições.",
    story: "BIN marcado como fraudulento por outros bancos = indústria shared.",
    problem: "Como usar inteligência compartilhada?",
    goldenTip: "💎 Shared lists multiplicam detecção. Fraud em banco A avisa banco B."
  },

  CONSORTIUM_DATA_CHECK: {
    name: "CONSORTIUM_DATA_CHECK",
    summary: "Consulta dados de CONSÓRCIO anti-fraude",
    syntax: "CONSORTIUM_DATA_CHECK(device, 'device_consortium') HAS_FLAG",
    syntaxExplanation: "Device flagged por qualquer membro do consórcio?",
    story: "Device usado em fraude em outra instituição = flagged no consortium.",
    problem: "Como usar inteligência coletiva?",
    goldenTip: "💎 Consórcios: Ethoca, Verifi, Mastercard Decision Intelligence."
  }
};
