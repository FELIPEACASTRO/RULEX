# RULEX - Documentação Técnica Completa

## Sistema de Regras Duras para Transações de Crédito

### Visão Geral

**RULEX** é uma plataforma completa de análise e prevenção de fraudes em transações de crédito. O sistema implementa um motor de regras duras configurável que avalia transações em tempo real, classifica-as como Aprovada, Suspeita de Fraude ou Fraude, e registra todas as decisões para auditoria e compliance.

**Arquitetura:**
- **Backend**: Java 21 + Spring Boot 3.2.1 + PostgreSQL
- **Frontend**: React 19 + TypeScript + Tailwind CSS
- **API**: REST com tRPC para integração frontend-backend

---

## Arquitetura do Sistema

### Camadas da Aplicação

```
┌─────────────────────────────────────────────────────────────┐
│                     Frontend React                          │
│  (Dashboard, Transações, Regras, Auditoria)                │
└──────────────────────┬──────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────┐
│                   API REST (Java)                           │
│  /api/transactions  /api/rules  /api/audit  /api/metrics   │
└──────────────────────┬──────────────────────────────────────┘
                       │
        ┌──────────────┼──────────────┐
        ▼              ▼              ▼
    ┌────────┐   ┌──────────┐   ┌──────────┐
    │ Motor  │   │ Serviços │   │ Auditoria│
    │ Regras │   │ Negócio  │   │ Compliance
    └────────┘   └──────────┘   └──────────┘
        │              │              │
        └──────────────┼──────────────┘
                       ▼
        ┌──────────────────────────────┐
        │    PostgreSQL Database       │
        │ (Transações, Decisões,       │
        │  Regras, Auditoria)          │
        └──────────────────────────────┘
```

---

## Especificação dos Parâmetros do JSON

### Estrutura da Requisição de Transação

#### Identificação

| Campo | Tipo | Obrigatório | Descrição |
|-------|------|-------------|-----------|
| `externalTransactionId` | String | Sim | ID único da transação no sistema externo |
| `customerIdFromHeader` | String | Sim | ID do cliente (header da requisição) |
| `customerAcctNumber` | Long | Sim | Número da conta do cliente |
| `pan` | String | Sim | Número do cartão (tokenizado/mascarado) |
| `merchantId` | String | Não | ID do estabelecimento |
| `merchantName` | String | Não | Nome do estabelecimento |
| `clientIdFromHeader` | String | Não | ID do cliente (header alternativo) |

#### Valores e Datas

| Campo | Tipo | Obrigatório | Descrição |
|-------|------|-------------|-----------|
| `transactionAmount` | BigDecimal | Sim | Valor da transação |
| `transactionDate` | Integer | Sim | Data da transação (YYYYMMDD) |
| `transactionTime` | Integer | Sim | Hora da transação (HHMMSS) |
| `gmtOffset` | String | Não | Offset GMT da transação |
| `transactionCurrencyCode` | Integer | Sim | Código da moeda (986=BRL) |
| `transactionCurrencyConversionRate` | BigDecimal | Não | Taxa de conversão |

#### Localização

| Campo | Tipo | Obrigatório | Descrição |
|-------|------|-------------|-----------|
| `merchantCountryCode` | String | Não | Código do país do merchant |
| `merchantCity` | String | Não | Cidade do merchant |
| `merchantState` | String | Não | Estado do merchant |
| `merchantPostalCode` | String | Não | CEP do merchant |

#### Categoria e Tipo

| Campo | Tipo | Obrigatório | Descrição |
|-------|------|-------------|-----------|
| `mcc` | Integer | Sim | Merchant Category Code |
| `posEntryMode` | String | Não | Modo de entrada (E=E-commerce, C=Chip, etc) |
| `customerPresent` | String | Não | Cliente presente (Y/N) |
| `workflow` | String | Não | Tipo de workflow |
| `recordType` | String | Não | Tipo de registro |

#### Segurança e Autenticação

| Campo | Tipo | Obrigatório | Descrição |
|-------|------|-------------|-----------|
| `consumerAuthenticationScore` | Integer | Sim | Score de autenticação (0-999) |
| `externalScore3` | Integer | Sim | Score externo (0-999) |
| `cavvResult` | Integer | Sim | Resultado CAVV (3D Secure) |
| `cryptogramValid` | String | Não | Criptograma válido (V=Válido) |
| `cvv2Response` | String | Não | Resposta CVV2 (M=Match, N=No Match) |
| `cvv2Present` | String | Não | CVV2 presente (Y/N) |
| `pinVerifyCode` | String | Não | Código de verificação do PIN |
| `cvvVerifyCode` | String | Não | Código de verificação do CVV |
| `eciIndicator` | Integer | Sim | Indicador ECI (E-commerce) |
| `atcCard` | Integer | Sim | Application Transaction Counter (cartão) |
| `atcHost` | Integer | Sim | Application Transaction Counter (host) |
| `tokenAssuranceLevel` | Integer | Sim | Nível de segurança do token |
| `tokenizationIndicator` | String | Não | Indicador de tokenização |

#### Crédito

| Campo | Tipo | Obrigatório | Descrição |
|-------|------|-------------|-----------|
| `availableCredit` | BigDecimal | Sim | Crédito disponível |
| `cardCashBalance` | BigDecimal | Sim | Saldo em dinheiro do cartão |
| `cardDelinquentAmount` | BigDecimal | Sim | Valor em atraso |

---

## Regras Implementadas

### Regras de Segurança

#### 1. LOW_AUTHENTICATION_SCORE
- **Tipo**: SECURITY
- **Descrição**: Bloqueia transações com score de autenticação baixo
- **Threshold Padrão**: 50
- **Peso**: 25%
- **Classificação**: SUSPICIOUS
- **Lógica**: `consumerAuthenticationScore < threshold`

#### 2. LOW_EXTERNAL_SCORE
- **Tipo**: SECURITY
- **Descrição**: Bloqueia transações com score externo baixo
- **Threshold Padrão**: 50
- **Peso**: 25%
- **Classificação**: SUSPICIOUS
- **Lógica**: `externalScore3 < threshold`

#### 3. INVALID_CAVV
- **Tipo**: SECURITY
- **Descrição**: Bloqueia transações com CAVV inválido (3D Secure)
- **Threshold Padrão**: 0
- **Peso**: 40%
- **Classificação**: FRAUD
- **Lógica**: `cavvResult != 0`

#### 4. INVALID_CRYPTOGRAM
- **Tipo**: SECURITY
- **Descrição**: Bloqueia transações com criptograma do chip inválido
- **Threshold Padrão**: N/A
- **Peso**: 35%
- **Classificação**: FRAUD
- **Lógica**: `cryptogramValid != "V"`

#### 5. CVV_MISMATCH
- **Tipo**: SECURITY
- **Descrição**: Bloqueia transações com CVV não correspondente
- **Threshold Padrão**: N/A
- **Peso**: 30%
- **Classificação**: SUSPICIOUS
- **Lógica**: `cvv2Response == "N"`

#### 6. PIN_VERIFICATION_FAILED
- **Tipo**: SECURITY
- **Descrição**: Bloqueia transações com falha na verificação do PIN
- **Threshold Padrão**: N/A
- **Peso**: 40%
- **Classificação**: FRAUD
- **Lógica**: `pinVerifyCode == "I"`

### Regras de Contexto

#### 7. HIGH_TRANSACTION_AMOUNT
- **Tipo**: CONTEXT
- **Descrição**: Bloqueia transações com valor acima do threshold
- **Threshold Padrão**: 5000.00
- **Peso**: 20%
- **Classificação**: SUSPICIOUS
- **Lógica**: `transactionAmount > threshold`

#### 8. HIGH_RISK_MCC
- **Tipo**: CONTEXT
- **Descrição**: Bloqueia transações de MCCs de alto risco
- **Threshold Padrão**: N/A
- **Peso**: 25%
- **Classificação**: SUSPICIOUS
- **MCCs de Alto Risco**: 7995 (Gambling), 6211 (Securities), 6051 (Crypto), 7273 (Dating), 7994 (Video)
- **Lógica**: `mcc in [7995, 6211, 6051, 7273, 7994]`

#### 9. INTERNATIONAL_TRANSACTION
- **Tipo**: CONTEXT
- **Descrição**: Bloqueia transações internacionais
- **Threshold Padrão**: N/A
- **Peso**: 15%
- **Classificação**: SUSPICIOUS
- **Lógica**: `merchantCountryCode != "076"` (Brasil)

#### 10. CARD_NOT_PRESENT
- **Tipo**: CONTEXT
- **Descrição**: Bloqueia transações sem cartão presente
- **Threshold Padrão**: N/A
- **Peso**: 20%
- **Classificação**: SUSPICIOUS
- **Lógica**: `customerPresent != "Y"`

### Regras Adicionais

#### 11. CVV_PIN_LIMIT_EXCEEDED
- **Tipo**: SECURITY
- **Descrição**: Bloqueia quando limite de tentativas foi excedido
- **Threshold Padrão**: N/A
- **Peso**: 35%
- **Classificação**: FRAUD
- **Lógica**: `cvvVerifyCode == "1"`

#### 12. OFFLINE_PIN_FAILED
- **Tipo**: SECURITY
- **Descrição**: Bloqueia quando PIN offline falha
- **Threshold Padrão**: N/A
- **Peso**: 40%
- **Classificação**: FRAUD
- **Lógica**: `cvvVerifyCode == "1"`

---

## Motor de Regras Duras

### Algoritmo de Avaliação

1. **Carregamento de Regras**: Sistema carrega todas as regras habilitadas do banco de dados
2. **Avaliação**: Para cada regra, verifica se a condição é atendida
3. **Scoring**: Calcula contribuição de cada regra: `weight × threshold / 100`
4. **Normalização**: Normaliza score para 0-100
5. **Classificação**: Classifica baseado no score final

### Cálculo do Score de Risco

```
Score Total = Σ (weight × threshold / 100) para cada regra acionada
Score Normalizado = min(Score Total, 100)

Classificação:
- Score 0-30: APPROVED (Aprovada)
- Score 30-70: SUSPICIOUS (Suspeita)
- Score 70-100: FRAUD (Fraude)
```

### Exemplo de Cálculo

```json
Transação:
{
  "consumerAuthenticationScore": 30,
  "externalScore3": 40,
  "cavvResult": 1,
  "cryptogramValid": "V",
  "cvv2Response": "M",
  "transactionAmount": 150.00,
  "mcc": 3121,
  "customerPresent": "Y",
  "merchantCountryCode": "076"
}

Regras Acionadas:
1. LOW_AUTHENTICATION_SCORE (30 < 50): weight=25, contribuição=25
2. LOW_EXTERNAL_SCORE (40 < 50): weight=25, contribuição=25
3. INVALID_CAVV (1 != 0): weight=40, contribuição=40

Score Total = 25 + 25 + 40 = 90
Classificação = FRAUD (score >= 70)
```

---

## Endpoints da API REST

### Transações

#### POST /api/transactions/analyze
Analisa uma transação e retorna a classificação.

**Requisição:**
```bash
curl -X POST http://localhost:8080/api/transactions/analyze \
  -H "Content-Type: application/json" \
  -d '{
    "externalTransactionId": "txn_123456",
    "customerIdFromHeader": "cust_001",
    "customerAcctNumber": 1033550704,
    "pan": "LY7x6tYaSty0817777",
    "transactionAmount": 150.00,
    "transactionDate": 20250216,
    "transactionTime": 114130,
    "mcc": 3121,
    "consumerAuthenticationScore": 95,
    "externalScore3": 90,
    "cavvResult": 0,
    "cryptogramValid": "V",
    "cvv2Response": "M",
    "eciIndicator": 5,
    "atcCard": 100,
    "atcHost": 100,
    "tokenAssuranceLevel": 50,
    "availableCredit": 5000.00,
    "cardCashBalance": 1000.00,
    "cardDelinquentAmount": 0.00
  }'
```

**Resposta:**
```json
{
  "transactionId": "txn_123456",
  "classification": "APPROVED",
  "riskScore": 15,
  "rulesApplied": ["LOW_AUTHENTICATION_SCORE"],
  "scoreDetails": {
    "LOW_AUTHENTICATION_SCORE": {
      "triggered": true,
      "weight": 25,
      "contribution": 25
    }
  },
  "reason": "Transação aprovada. Score de risco baixo.",
  "rulesVersion": "1.0.0",
  "processingTime": 45,
  "timestamp": "2025-02-16T11:41:30",
  "success": true
}
```

#### GET /api/transactions
Lista transações com filtros.

**Parâmetros:**
- `customerId` (opcional): ID do cliente
- `merchantId` (opcional): ID do merchant
- `mcc` (opcional): MCC
- `minAmount` (opcional): Valor mínimo
- `maxAmount` (opcional): Valor máximo
- `startDate` (opcional): Data inicial (ISO)
- `endDate` (opcional): Data final (ISO)
- `page` (padrão: 0): Número da página
- `size` (padrão: 20): Tamanho da página

**Exemplo:**
```bash
curl "http://localhost:8080/api/transactions?customerId=cust_001&page=0&size=20"
```

#### GET /api/transactions/{id}
Obtém detalhes de uma transação.

#### GET /api/transactions/external/{externalId}
Obtém transação pelo ID externo.

---

### Regras

#### GET /api/rules
Lista todas as regras configuradas.

**Parâmetros:**
- `page` (padrão: 0): Número da página
- `size` (padrão: 20): Tamanho da página

#### POST /api/rules
Cria uma nova regra.

**Requisição:**
```json
{
  "ruleName": "LOW_AUTHENTICATION_SCORE",
  "description": "Bloqueia transações com score de autenticação baixo",
  "ruleType": "SECURITY",
  "threshold": 50,
  "weight": 25,
  "enabled": true,
  "classification": "SUSPICIOUS"
}
```

#### PUT /api/rules/{id}
Atualiza uma regra existente.

#### DELETE /api/rules/{id}
Deleta uma regra.

#### PATCH /api/rules/{id}/toggle
Ativa ou desativa uma regra.

#### GET /api/rules/enabled/{enabled}
Lista regras por status de habilitação.

---

### Auditoria

#### GET /api/audit
Lista logs de auditoria com filtros.

**Parâmetros:**
- `actionType` (opcional): Tipo de ação
- `result` (opcional): SUCCESS ou FAILURE
- `startDate` (opcional): Data inicial
- `endDate` (opcional): Data final
- `page` (padrão: 0): Número da página
- `size` (padrão: 20): Tamanho da página

#### GET /api/audit/transaction/{transactionId}
Obtém logs de auditoria para uma transação específica.

---

### Métricas

#### GET /api/metrics
Obtém métricas gerais do sistema.

**Parâmetros:**
- `period` (padrão: 24h): 1h, 24h, 7d, 30d, 90d

**Resposta:**
```json
{
  "totalTransactions": 1500,
  "approvedTransactions": 1350,
  "suspiciousTransactions": 120,
  "fraudTransactions": 30,
  "approvalRate": 90.00,
  "fraudRate": 2.00,
  "suspiciousRate": 8.00,
  "totalVolume": 225000.00,
  "averageTransactionAmount": 150.00,
  "period": "24h",
  "timestamp": "2025-02-16T11:41:30"
}
```

#### GET /api/metrics/mcc
Métricas agrupadas por MCC.

#### GET /api/metrics/merchant
Métricas agrupadas por merchant.

#### GET /api/metrics/timeline
Timeline de métricas com granularidade configurável.

---

## Banco de Dados PostgreSQL

### Tabelas Principais

#### transactions
Armazena todas as transações processadas.

```sql
CREATE TABLE transactions (
  id BIGSERIAL PRIMARY KEY,
  external_transaction_id VARCHAR(64) UNIQUE NOT NULL,
  customer_id_from_header VARCHAR(64) NOT NULL,
  customer_acct_number BIGINT NOT NULL,
  pan VARCHAR(64) NOT NULL,
  merchant_id VARCHAR(64),
  merchant_name VARCHAR(255),
  transaction_amount NUMERIC(15,2) NOT NULL,
  transaction_date INTEGER NOT NULL,
  transaction_time INTEGER NOT NULL,
  -- ... outros campos
  created_at TIMESTAMP NOT NULL,
  updated_at TIMESTAMP NOT NULL
);

CREATE INDEX idx_customer_id ON transactions(customer_id_from_header);
CREATE INDEX idx_merchant_id ON transactions(merchant_id);
CREATE INDEX idx_transaction_date ON transactions(transaction_date);
CREATE INDEX idx_external_transaction_id ON transactions(external_transaction_id);
```

#### transaction_decisions
Armazena as decisões tomadas para cada transação.

```sql
CREATE TABLE transaction_decisions (
  id BIGSERIAL PRIMARY KEY,
  transaction_id BIGINT NOT NULL REFERENCES transactions(id),
  classification VARCHAR(20) NOT NULL,
  risk_score INTEGER NOT NULL,
  rules_applied TEXT,
  score_details TEXT,
  reason TEXT,
  rules_version VARCHAR(20),
  created_at TIMESTAMP NOT NULL
);

CREATE INDEX idx_transaction_id ON transaction_decisions(transaction_id);
CREATE INDEX idx_classification ON transaction_decisions(classification);
CREATE INDEX idx_decision_date ON transaction_decisions(created_at);
```

#### rule_configurations
Armazena as configurações dinâmicas de regras.

```sql
CREATE TABLE rule_configurations (
  id BIGSERIAL PRIMARY KEY,
  rule_name VARCHAR(100) UNIQUE NOT NULL,
  description TEXT,
  rule_type VARCHAR(20) NOT NULL,
  threshold INTEGER NOT NULL,
  weight INTEGER NOT NULL,
  enabled BOOLEAN NOT NULL,
  classification VARCHAR(20) NOT NULL,
  parameters TEXT,
  version INTEGER NOT NULL,
  created_at TIMESTAMP NOT NULL,
  updated_at TIMESTAMP NOT NULL
);

CREATE INDEX idx_rule_name ON rule_configurations(rule_name);
CREATE INDEX idx_enabled ON rule_configurations(enabled);
```

#### audit_logs
Armazena todos os logs de auditoria para compliance.

```sql
CREATE TABLE audit_logs (
  id BIGSERIAL PRIMARY KEY,
  transaction_id BIGINT,
  action_type VARCHAR(50) NOT NULL,
  description TEXT,
  details TEXT,
  performed_by VARCHAR(100),
  result VARCHAR(20) NOT NULL,
  error_message TEXT,
  source_ip VARCHAR(45),
  created_at TIMESTAMP NOT NULL
);

CREATE INDEX idx_transaction_id ON audit_logs(transaction_id);
CREATE INDEX idx_action_type ON audit_logs(action_type);
CREATE INDEX idx_audit_date ON audit_logs(created_at);
```

---

## Frontend React

### Estrutura de Páginas

#### Dashboard
- Métricas em tempo real (aprovação, fraude, suspeita)
- Gráficos de distribuição e tendência temporal
- KPIs principais
- Status do sistema

#### Transações
- Tabela de transações com paginação
- Filtros por cliente, merchant, valor, data
- Visualização de detalhes
- Score de risco com código de cores

#### Regras
- CRUD completo de regras
- Ativar/desativar regras
- Ajuste dinâmico de threshold e weight
- Histórico de versões

#### Auditoria
- Histórico de todas as ações
- Filtros por tipo de ação e resultado
- Rastreamento de alterações
- Compliance e conformidade

---

## Configuração Dinâmica de Regras

### Sem Necessidade de Redeploy

As regras podem ser ajustadas via API sem necessidade de reiniciar a aplicação:

1. **Threshold**: Valor limite para ativação da regra
2. **Weight**: Peso da regra no cálculo do score (0-100)
3. **Enabled**: Ativar/desativar a regra
4. **Classification**: Classificação que a regra pode gerar

**Exemplo de Atualização:**
```bash
curl -X PUT http://localhost:8080/api/rules/1 \
  -H "Content-Type: application/json" \
  -d '{
    "ruleName": "LOW_AUTHENTICATION_SCORE",
    "description": "Bloqueia transações com score baixo",
    "ruleType": "SECURITY",
    "threshold": 60,
    "weight": 30,
    "enabled": true,
    "classification": "SUSPICIOUS"
  }'
```

---

## Auditoria e Compliance

### Registro de Ações

Todas as ações são registradas automaticamente:

- **TRANSACTION_PROCESSED**: Quando uma transação é analisada
- **RULE_CREATED**: Quando uma regra é criada
- **RULE_UPDATED**: Quando uma regra é atualizada
- **RULE_DELETED**: Quando uma regra é deletada
- **CONFIG_CHANGED**: Quando configurações são alteradas

### Informações Registradas

Para cada ação, o sistema registra:
- Timestamp exato
- Tipo de ação
- Descrição
- Detalhes em JSON
- Usuário que realizou a ação
- Resultado (SUCCESS/FAILURE)
- Mensagem de erro (se houver)
- IP de origem

### Relatórios de Compliance

O sistema permite gerar relatórios para compliance:
- Taxa de aprovação/fraude por período
- Distribuição de decisões por regra
- Histórico de alterações de configuração
- Rastreamento de transações suspeitas

---

## Performance e Escalabilidade

### Métricas de Performance

- **Processamento de Transação**: < 100ms
- **Throughput**: 1000+ transações/segundo
- **Latência da API**: < 50ms (p95)
- **Cache de Regras**: Habilitado por padrão

### Otimizações

1. **Índices de Banco de Dados**: Otimizados para queries frequentes
2. **Cache de Regras**: Regras são cacheadas em memória
3. **Paginação**: Implementada em todas as listagens
4. **Queries Otimizadas**: Uso de índices e lazy loading

---

## Segurança

### Validação de Entrada

- Validação de todos os parâmetros via anotações `@Valid`
- Rejeição de valores nulos obrigatórios
- Validação de ranges e formatos

### Tratamento de Erros

- Tratamento global de exceções
- Respostas de erro padronizadas
- Logs detalhados para debugging

### CORS

- Configurado para permitir requisições do frontend
- Métodos permitidos: GET, POST, PUT, DELETE, PATCH

---

## Deployment

### Requisitos

- Java 21+
- Maven 3.8+
- PostgreSQL 12+
- Node.js 22+

### Passos de Deployment

1. **Backend Java**:
```bash
cd backend
mvn clean install
java -jar target/rulex-fraud-detection-1.0.0.jar
```

2. **Frontend React**:
```bash
cd client
pnpm install
pnpm build
pnpm preview
```

3. **Banco de Dados**:
```bash
createdb rulex_db
# Tabelas são criadas automaticamente via Hibernate
```

---

## Próximos Passos

1. Implementar autenticação OAuth2
2. Adicionar rate limiting
3. Implementar cache distribuído (Redis)
4. Adicionar métricas Prometheus
5. Implementar webhooks para notificações
6. Adicionar machine learning para detecção de anomalias
7. Implementar alertas em tempo real

---

## Suporte e Contato

Para dúvidas ou problemas, consulte a documentação técnica ou entre em contato com o time de desenvolvimento.

**Versão**: 1.0.0  
**Data**: 16 de Fevereiro de 2025  
**Autor**: RULEX Development Team
