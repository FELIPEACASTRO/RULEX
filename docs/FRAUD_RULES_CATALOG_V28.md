# Catálogo de Regras de Fraude - Migration V28

**Data de Criação:** 2026-01-06  
**Fonte:** RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md  
**Referências:** FATF, Europol, FinCEN, NIST, FBI  
**Total de Regras:** 15 regras parametrizáveis

---

## Visão Geral

Este catálogo documenta as regras de fraude implementadas na migration V28, baseadas em padrões documentados por organizações internacionais de combate à fraude e lavagem de dinheiro.

Todas as regras são **parametrizáveis** e utilizam o modelo de **regras complexas** do RULEX, permitindo ajustes finos sem necessidade de alteração de código.

---

## Categorias de Regras

### 1. Card Testing (CT) - 4 regras

Detectam tentativas de validação de cartões roubados através de múltiplas transações de teste.

#### CT-001: Múltiplas Transações Pequenas
- **Rule Key:** `CT_001_MULTIPLE_SMALL_TRANSACTIONS`
- **Decisão:** BLOCK
- **Score Impact:** 85
- **Prioridade:** 95
- **Lógica:**
  - Contagem de transações no mesmo PAN >= 5 em 5 minutos
  - E valor da transação < R$ 10,00
- **Campos Utilizados:** `pan`, `transactionAmount`
- **Padrão de Fraude:** Fraudadores testam cartões roubados com valores muito baixos para evitar detecção inicial.

#### CT-002: Múltiplos Merchants
- **Rule Key:** `CT_002_MULTIPLE_MERCHANTS`
- **Decisão:** REVIEW
- **Score Impact:** 80
- **Prioridade:** 90
- **Lógica:**
  - Contagem de merchants distintos >= 5 em 10 minutos
- **Campos Utilizados:** `merchantId`
- **Padrão de Fraude:** Uso automatizado de cartão roubado em múltiplos estabelecimentos simultaneamente.

#### CT-003: Valores Crescentes (Escada)
- **Rule Key:** `CT_003_ESCALATING_AMOUNTS`
- **Decisão:** REVIEW
- **Score Impact:** 75
- **Prioridade:** 85
- **Lógica:**
  - Contagem de transações >= 3 em 15 minutos
  - E valor atual > média histórica do PAN
- **Campos Utilizados:** `pan`, `transactionAmount`
- **Padrão de Fraude:** Fraudadores testam limites do cartão com valores crescentes.

#### CT-004: Falhas de Autenticação + Sucesso
- **Rule Key:** `CT_004_AUTH_FAILURES_THEN_SUCCESS`
- **Decisão:** BLOCK
- **Score Impact:** 90
- **Prioridade:** 95
- **Lógica:**
  - CVV2 Response = 'N' (falha)
  - E contagem de falhas >= 3 em 10 minutos
- **Campos Utilizados:** `cvv2Response`, `pan`
- **Padrão de Fraude:** Tentativa de descoberta de CVV por força bruta.

---

### 2. Account Takeover (ATO) - 1 regra

Detectam tentativas de sequestro de conta através de mudanças comportamentais.

#### ATO-003: Novo Device + Nova Geolocalização
- **Rule Key:** `ATO_003_NEW_DEVICE_NEW_GEO`
- **Decisão:** STEP_UP (solicitar autenticação adicional)
- **Score Impact:** 75
- **Prioridade:** 80
- **Lógica:**
  - Terminal ID nunca visto antes para este cliente (últimos 30 dias)
  - E país do merchant diferente do histórico do cliente
- **Campos Utilizados:** `terminalId`, `merchantCountryCode`, `customerAcctNumber`
- **Padrão de Fraude:** Fraudador usa cartão roubado em novo dispositivo e localização.

---

### 3. Transações de Alto Risco (TR) - 3 regras

Detectam transações com características de alto risco de fraude.

#### TR-001: Internacional + Alto Valor + Primeira Vez
- **Rule Key:** `TR_001_INTL_HIGH_VALUE_FIRST_TIME`
- **Decisão:** REVIEW
- **Score Impact:** 80
- **Prioridade:** 85
- **Lógica:**
  - País do merchant ≠ país do adquirente (transação internacional)
  - E valor >= R$ 1.000,00
  - E primeira transação internacional do cliente
- **Campos Utilizados:** `merchantCountryCode`, `acquirerCountry`, `transactionAmount`, `customerAcctNumber`
- **Padrão de Fraude:** Primeira transação internacional de alto valor é altamente suspeita.

#### TR-002: MCC de Alto Risco + Valor Alto
- **Rule Key:** `TR_002_HIGH_RISK_MCC_HIGH_VALUE`
- **Decisão:** REVIEW
- **Score Impact:** 75
- **Prioridade:** 80
- **Lógica:**
  - MCC em lista de alto risco (5094, 5122, 5912, 5962, 5993, 6051, 7273, 7995)
  - E valor >= R$ 500,00
- **Campos Utilizados:** `mcc`, `transactionAmount`
- **Padrão de Fraude:** MCCs de gift cards, cripto, apostas e joias são alvos frequentes de fraude.
- **MCCs de Alto Risco:**
  - 5094: Joias, relógios, pedras preciosas
  - 5122: Drogas, suplementos
  - 5912: Farmácias
  - 5962: Vendas diretas/telemarketing
  - 5993: Lojas de charutos
  - 6051: Criptomoedas
  - 7273: Serviços de namoro/acompanhantes
  - 7995: Apostas/cassinos

#### TR-003: CNP sem 3D Secure
- **Rule Key:** `TR_003_CNP_WITHOUT_3DS`
- **Decisão:** REVIEW
- **Score Impact:** 70
- **Prioridade:** 75
- **Lógica:**
  - POS Entry Mode indica CNP (81, 10, 79)
  - E ECI Indicator < 5 (sem 3DS)
  - E valor >= R$ 200,00
- **Campos Utilizados:** `posEntryMode`, `eciIndicator`, `transactionAmount`
- **Padrão de Fraude:** Transações Card Not Present sem autenticação forte são vulneráveis.

---

### 4. Velocidade Anômala (VA) - 2 regras

Detectam padrões de velocidade anômala de transações.

#### VA-001: Alta Velocidade de Transações
- **Rule Key:** `VA_001_HIGH_VELOCITY`
- **Decisão:** REVIEW
- **Score Impact:** 75
- **Prioridade:** 80
- **Lógica:**
  - Contagem de transações >= 10 em 1 hora
- **Campos Utilizados:** `pan`
- **Padrão de Fraude:** Uso automatizado de cartão roubado gera velocidade anômala.

#### VA-002: Alta Soma de Valores em Curto Período
- **Rule Key:** `VA_002_HIGH_AMOUNT_VELOCITY`
- **Decisão:** REVIEW
- **Score Impact:** 80
- **Prioridade:** 85
- **Lógica:**
  - Soma de valores >= R$ 5.000,00 em 24 horas
- **Campos Utilizados:** `transactionAmount`, `pan`
- **Padrão de Fraude:** Esvaziamento rápido de conta comprometida.

---

### 5. Padrões Comportamentais Anômalos (PA) - 2 regras

Detectam mudanças comportamentais que indicam fraude.

#### PA-001: Transação em Horário Incomum
- **Rule Key:** `PA_001_UNUSUAL_TIME`
- **Decisão:** REVIEW
- **Score Impact:** 60
- **Prioridade:** 65
- **Lógica:**
  - Horário entre 02:00 e 05:00
  - E valor >= R$ 100,00
- **Campos Utilizados:** `transactionTime`, `transactionAmount`
- **Padrão de Fraude:** Transações na madrugada são estatisticamente mais fraudulentas.

#### PA-002: Mudança Abrupta de Padrão de Gasto
- **Rule Key:** `PA_002_SPENDING_PATTERN_CHANGE`
- **Decisão:** REVIEW
- **Score Impact:** 70
- **Prioridade:** 75
- **Lógica:**
  - Valor atual > 3x a média dos últimos 30 dias
- **Campos Utilizados:** `transactionAmount`, `customerAcctNumber`
- **Padrão de Fraude:** Mudança abrupta no ticket médio indica comprometimento.

---

## Mapeamento de Campos do Payload

As regras utilizam os seguintes campos do `TransactionRequest`:

| Campo | Tipo | Uso nas Regras |
|---|---|---|
| `pan` | String | Identificação do cartão para contadores de velocidade |
| `transactionAmount` | BigDecimal | Valor da transação para limiares e agregações |
| `merchantId` | String | Identificação do estabelecimento |
| `merchantCountryCode` | String | País do merchant para detecção de transações internacionais |
| `mcc` | Integer | Código de categoria do merchant para detecção de alto risco |
| `customerAcctNumber` | Long | Identificação do cliente para perfil comportamental |
| `terminalId` | String | Identificação do terminal para detecção de novos devices |
| `posEntryMode` | String | Modo de entrada do cartão (CNP vs CP) |
| `eciIndicator` | Integer | Indicador de autenticação 3D Secure |
| `cvv2Response` | String | Resultado da validação de CVV2 |
| `transactionTime` | Integer | Horário da transação (formato HHMMSS) |
| `acquirerCountry` | String | País do adquirente |

---

## Operadores Avançados Utilizados

As regras utilizam os novos operadores de agregação temporal implementados:

- `COUNT_LAST_N_HOURS`: Contagem de transações em janela temporal
- `SUM_LAST_N_DAYS`: Soma de valores em janela temporal
- `AVG_LAST_N_DAYS`: Média de valores em janela temporal
- `COUNT_DISTINCT_MERCHANTS_LAST_N_DAYS`: Contagem de merchants únicos
- `COUNT_DISTINCT_COUNTRIES_LAST_N_HOURS`: Contagem de países únicos

---

## Decisões e Ações

| Decisão | Significado | Ação Recomendada |
|---|---|---|
| `BLOCK` | Bloquear transação imediatamente | Negar autorização |
| `REVIEW` | Enviar para revisão manual | Segurar transação para análise |
| `STEP_UP` | Solicitar autenticação adicional | Acionar 3DS, SMS, ou biometria |
| `APPROVE` | Aprovar transação | Autorizar normalmente |

---

## Próximos Passos

1. **Validação em Shadow Mode:** Ativar as regras em modo sombra por 7 dias para calibração.
2. **Ajuste de Limiares:** Ajustar valores e janelas temporais baseado em falsos positivos.
3. **Expansão do Catálogo:** Implementar as demais regras do arquivo RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md.
4. **Integração com Listas:** Criar listas parametrizáveis de MCCs de alto risco, países bloqueados, etc.

---

## Referências

- **FATF Recommendations:** https://www.fatf-gafi.org/publications/fatfrecommendations/
- **Europol Payment Fraud Threat Landscape:** https://www.europol.europa.eu/
- **FinCEN Advisories:** https://www.fincen.gov/resources/advisoriesbulletinsfact-sheets
- **NIST SP 800-63a:** https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-63a.pdf
- **FBI IC3 Annual Report:** https://www.fbi.gov/file-repository/ic3-annual-report-2022.pdf
