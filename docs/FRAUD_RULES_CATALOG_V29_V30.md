# Catálogo de Regras de Fraude Avançadas (V29 e V30)

**Data:** 2026-01-06
**Autor:** Manus AI
**Fonte:** `RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md`

Este documento detalha as **40 novas regras de fraude** implementadas nas migrations `V29` e `V30`, cobrindo categorias avançadas como Identidade Sintética, Lavagem de Dinheiro (AML), Account Takeover (ATO), Fraude de Reembolso e muito mais.

---

## Migration V29: Regras Avançadas de Fraude (25 Regras)

### Categoria: Identidade Sintética (SI)

| Chave da Regra | Nome | Descrição | Decisão | Score |
|---|---|---|---|---|
| `SI_001_CREDIT_BUILDING_VELOCITY` | SI - Velocidade de Construção de Crédito | Detecta velocidade anômala na construção de crédito, indicando identidade sintética. | `REVIEW` | 75 |
| `SI_002_INCONSISTENT_PERSONAL_DATA` | SI - Dados Pessoais Inconsistentes | Detecta inconsistência entre dados pessoais (idade, CPF, endereço), indicando identidade sintética. | `REVIEW` | 80 |
| `SI_003_MULTIPLE_ACCOUNTS_SAME_DEVICE` | SI - Múltiplas Contas no Mesmo Device | Detecta múltiplas contas criadas a partir do mesmo device, indicando fazenda de contas sintéticas. | `REVIEW` | 85 |
| `SI_004_CPF_GENERATED_RECENTLY` | SI - CPF Gerado Recentemente | Detecta CPF gerado recentemente com alta atividade, indicando identidade sintética. | `REVIEW` | 70 |
| `SI_005_NO_SOCIAL_MEDIA_PRESENCE` | SI - Ausência de Presença em Redes Sociais | Detecta ausência de presença em redes sociais para um perfil ativo, indicando identidade sintética. | `REVIEW` | 60 |

### Categoria: Fraude de Reembolso (RF)

| Chave da Regra | Nome | Descrição | Decisão | Score |
|---|---|---|---|---|
| `RF_001_EXCESSIVE_REFUNDS_SAME_MERCHANT` | RF - Reembolsos Excessivos no Mesmo Merchant | Detecta múltiplos pedidos de reembolso para o mesmo merchant, indicando fraude de reembolso. | `REVIEW` | 70 |
| `RF_002_REFUND_AFTER_DELIVERY_CONFIRMATION` | RF - Reembolso Após Confirmação de Entrega | Detecta pedido de reembolso após a confirmação de entrega, indicando fraude de item não recebido. | `REVIEW` | 80 |
| `RF_003_HIGH_VALUE_REFUND_NEW_CUSTOMER` | RF - Reembolso de Alto Valor para Novo Cliente | Detecta pedido de reembolso de alto valor para um novo cliente, indicando fraude de reembolso. | `REVIEW` | 75 |
| `RF_004_MULTIPLE_REFUNDS_DIFFERENT_CARDS` | RF - Múltiplos Reembolsos em Cartões Diferentes | Detecta múltiplos pedidos de reembolso para cartões diferentes, indicando fraude de reembolso. | `REVIEW` | 85 |
| `RF_005_REFUND_CLAIM_WITH_PROXY_IP` | RF - Pedido de Reembolso com IP de Proxy | Detecta pedido de reembolso feito a partir de um IP de proxy/VPN, indicando tentativa de ofuscação. | `REVIEW` | 70 |

### Categoria: Fraude Amiga (FF)

| Chave da Regra | Nome | Descrição | Decisão | Score |
|---|---|---|---|---|
| `FF_001_CHARGEBACK_AFTER_SUCCESSFUL_3DS` | FF - Chargeback Após 3D Secure | Detecta chargeback em transação com 3D Secure, indicando possível fraude amiga. | `REVIEW` | 60 |
| `FF_002_MULTIPLE_CHARGEBACKS_LOW_VALUE` | FF - Múltiplos Chargebacks de Baixo Valor | Detecta múltiplos chargebacks de baixo valor, indicando abuso do sistema. | `REVIEW` | 65 |
| `FF_003_CHARGEBACK_FROM_KNOWN_GOOD_CUSTOMER` | FF - Chargeback de Cliente com Bom Histórico | Detecta chargeback de cliente com bom histórico, indicando possível fraude amiga. | `REVIEW` | 50 |
| `FF_004_CHARGEBACK_ON_DIGITAL_GOODS` | FF - Chargeback em Bens Digitais | Detecta chargeback em bens digitais (software, jogos, etc.), onde a fraude amiga é comum. | `REVIEW` | 70 |
| `FF_005_CHARGEBACK_REASON_INCONSISTENT` | FF - Motivo do Chargeback Inconsistente | Detecta motivo de chargeback inconsistente com o histórico do cliente. | `REVIEW` | 60 |

### Categoria: Fraude de Triangulação (TF)

| Chave da Regra | Nome | Descrição | Decisão | Score |
|---|---|---|---|---|
| `TF_001_SHIPPING_ADDRESS_DIFFERENT_FROM_BILLING` | TF - Endereço de Entrega Diferente do Faturamento | Detecta endereço de entrega diferente do de faturamento, um padrão comum em fraude de triangulação. | `REVIEW` | 60 |
| `TF_002_MULTIPLE_ORDERS_DIFFERENT_CUSTOMERS_SAME_ADDRESS` | TF - Múltiplos Pedidos, Clientes Diferentes, Mesmo Endereço | Detecta múltiplos pedidos de clientes diferentes para o mesmo endereço de entrega. | `REVIEW` | 80 |
| `TF_003_HIGH_VELOCITY_OF_ORDERS_NEW_MERCHANT` | TF - Alta Velocidade de Pedidos em Novo Merchant | Detecta alta velocidade de pedidos em um merchant recém-cadastrado. | `REVIEW` | 75 |
| `TF_004_PAYMENT_WITH_DIFFERENT_CARDS_SAME_DEVICE` | TF - Pagamento com Cartões Diferentes no Mesmo Device | Detecta pagamento com cartões diferentes a partir do mesmo device. | `REVIEW` | 85 |
| `TF_005_ORDER_FROM_HIGH_RISK_COUNTRY_FOR_LOW_RISK_PRODUCT` | TF - Pedido de País de Alto Risco para Produto de Baixo Risco | Detecta pedido de país de alto risco para um produto de baixo risco, um padrão de triangulação. | `REVIEW` | 70 |

### Categoria: Bust-Out (BO)

| Chave da Regra | Nome | Descrição | Decisão | Score |
|---|---|---|---|---|
| `BO_001_SUDDEN_INCREASE_IN_CREDIT_USAGE` | BO - Aumento Súbito no Uso do Crédito | Detecta aumento súbito no uso do limite de crédito, um padrão de bust-out. | `REVIEW` | 80 |
| `BO_002_MAX_OUT_CREDIT_LIMIT_QUICKLY` | BO - Esgotamento Rápido do Limite de Crédito | Detecta esgotamento rápido do limite de crédito após um período de bom comportamento. | `REVIEW` | 90 |
| `BO_003_MULTIPLE_CASH_ADVANCES` | BO - Múltiplos Adiantamentos em Dinheiro | Detecta múltiplos adiantamentos em dinheiro (cash advance) em curto período. | `REVIEW` | 85 |
| `BO_004_CHANGE_OF_ADDRESS_BEFORE_BUST_OUT` | BO - Mudança de Endereço Antes do Bust-Out | Detecta mudança de endereço pouco antes de um aumento súbito no uso do crédito. | `REVIEW` | 75 |
| `BO_005_NO_PAYMENTS_AFTER_HIGH_USAGE` | BO - Ausência de Pagamentos Após Alto Uso | Detecta ausência de pagamentos após um período de alto uso do crédito. | `REVIEW` | 95 |

---

## Migration V30: Regras Avançadas de AML e ATO (15 Regras)

### Categoria: Anti-Money Laundering (AML)

| Chave da Regra | Nome | Descrição | Decisão | Score |
|---|---|---|---|---|
| `AML_001_HIGH_RISK_COUNTRY_RECURRING` | AML - Transações Recorrentes para País de Alto Risco | Detecta transações recorrentes para países na lista FATF de alto risco. | `REVIEW` | 85 |
| `AML_002_TBML_PROXY_INCOHERENT_PAYMENTS` | AML - TBML Proxy (Pagamentos Incoerentes) | Detecta pagamentos comerciais com valores incoerentes com o perfil do cliente. | `REVIEW` | 80 |
| `AML_003_RAPID_LAYERING` | AML - Movimentação Rápida entre Contas (Layering) | Detecta movimentação rápida de valores entre múltiplas contas. | `REVIEW` | 90 |
| `AML_004_MULTI_INSTRUMENT_INTENSIVE_USE` | AML - Uso Intenso de Múltiplos Instrumentos | Detecta uso intenso de múltiplos instrumentos para dificultar rastreamento. | `REVIEW` | 80 |
| `AML_005_SANCTIONS_SCREENING_STRONG_MATCH` | AML - Screening de Sanções (Match Forte) | Detecta match forte em listas de sanções (OFAC, ONU, UE). | `BLOCK` | 100 |
| `AML_006_STRUCTURING_CTR_AVOIDANCE` | AML - Estruturação para Evitar CTR | Detecta estruturação de transações para evitar reporte de CTR. | `REVIEW` | 85 |
| `AML_007_RANSOMWARE_PAYMENT_PATTERN` | AML - Padrão de Pagamento de Ransomware | Detecta padrões de pagamento típicos de ransomware. | `BLOCK` | 95 |
| `AML_008_POTENTIAL_MULE_ACCOUNT` | AML - Conta Potencial Mula | Detecta conta com padrão de mula: muitos pagadores diferentes + retiradas rápidas. | `REVIEW` | 90 |
| `AML_009_PROFILE_INCONSISTENCY_INCOME_VOLUME` | AML - Inconsistência de Perfil (Renda vs Volume) | Detecta inconsistência entre perfil de baixa renda e volume alto de transações. | `REVIEW` | 80 |
| `AML_010_INCOHERENT_PURPOSE` | AML - Finalidade Incoerente | Detecta transações com descrição/finalidade suspeita ou incoerente. | `REVIEW` | 70 |

### Categoria: Account Takeover (ATO)

| Chave da Regra | Nome | Descrição | Decisão | Score |
|---|---|---|---|---|
| `ATO_006_MFA_FATIGUE_ATTACK` | Account Takeover - MFA Fatigue Attack | Detecta ataque de MFA fatigue: múltiplos prompts negados seguidos de 1 aceito. | `BLOCK` | 95 |
| `ATO_007_SESSION_DEVICE_CHANGE` | Account Takeover - Mudança de Device na Sessão | Detecta mudança de device ou fingerprint no meio de uma sessão ativa. | `BLOCK` | 90 |
| `ATO_008_DATACENTER_LOGIN_FINANCIAL_ACTION` | Account Takeover - Login de Datacenter + Ação Financeira | Detecta login de datacenter/proxy seguido de ação financeira. | `BLOCK` | 90 |
| `ATO_009_DORMANT_ACCOUNT_REACTIVATION` | Account Takeover - Reativação de Conta Inativa | Detecta conta inativa por longo período que é reativada e realiza transação imediata. | `REVIEW` | 85 |
| `ATO_010_INCOMPATIBLE_USER_AGENTS` | Account Takeover - User-Agents Incompatíveis | Detecta tentativas de acesso com múltiplos user-agents incompatíveis. | `REVIEW` | 80 |

---

**Total de Regras Implementadas:** 40

O RULEX agora possui um dos catálogos de regras de fraude mais completos e avançados do mercado, cobrindo desde padrões simples de card testing até esquemas complexos de lavagem de dinheiro e fraude de identidade sintéticos.
