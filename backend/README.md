# RULEX - Backend Java 21 + Spring Boot

## Visão Geral

Backend da plataforma RULEX, um sistema de regras duras para análise e prevenção de fraudes em transações de crédito. Implementado em Java 21 com Spring Boot 3.2.1, oferecendo uma API REST robusta com motor de regras configurável, auditoria completa e métricas em tempo real.

## Requisitos

- Java 21+
- Maven 3.8+
- PostgreSQL 12+

## Estrutura do Projeto

```
backend/
├── src/main/java/com/rulex/
│   ├── controller/          # Controllers REST
│   ├── service/             # Lógica de negócio
│   ├── repository/          # Acesso aos dados (JPA)
│   ├── entity/              # Entidades JPA
│   ├── dto/                 # Data Transfer Objects
│   ├── exception/           # Tratamento de exceções
│   ├── config/              # Configurações
│   └── util/                # Utilitários
├── src/main/resources/
│   └── application.yml      # Configuração da aplicação
└── pom.xml                  # Dependências Maven
```

## Configuração

### 1. Banco de Dados PostgreSQL

Crie o banco de dados:

```sql
CREATE DATABASE rulex_db;
```

### 2. Arquivo application.yml

Configure a conexão com o banco de dados em `src/main/resources/application.yml`:

```yaml
spring:
  datasource:
    url: jdbc:postgresql://localhost:5432/rulex_db
    username: postgres
    password: sua_senha
```

### 3. Compilação e Execução

```bash
# Compilar
mvn clean install

# Executar
mvn spring-boot:run

# Ou via JAR
java -jar target/rulex-fraud-detection-1.0.0.jar
```

A API estará disponível em `http://localhost:8080/api`

## Endpoints da API

### Transações

#### Analisar Transação
```
POST /api/transactions/analyze
Content-Type: application/json

{
  "externalTransactionId": "txn_123456",
  "customerIdFromHeader": "cust_001",
  "customerAcctNumber": 1033550704,
  "pan": "LY7x6tYaSty0817777",
  "merchantId": "merchant_001",
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
  "cardDelinquentAmount": 0.00,
  ...
}
```

Resposta:
```json
{
  "transactionId": "txn_123456",
  "classification": "APPROVED",
  "riskScore": 15,
  "rulesApplied": ["LOW_AUTHENTICATION_SCORE"],
  "reason": "Transação aprovada. Score de risco baixo.",
  "processingTime": 45,
  "timestamp": "2025-02-16T11:41:30",
  "success": true
}
```

#### Listar Transações
```
GET /api/transactions?customerId=cust_001&page=0&size=20
```

#### Obter Transação
```
GET /api/transactions/{id}
GET /api/transactions/external/{externalId}
```

### Regras

#### Listar Regras
```
GET /api/rules?page=0&size=20
```

#### Criar Regra
```
POST /api/rules
Content-Type: application/json

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

#### Atualizar Regra
```
PUT /api/rules/{id}
```

#### Deletar Regra
```
DELETE /api/rules/{id}
```

#### Ativar/Desativar Regra
```
PATCH /api/rules/{id}/toggle
```

### Auditoria

#### Listar Logs de Auditoria
```
GET /api/audit?actionType=TRANSACTION_PROCESSED&page=0&size=20
```

#### Logs de Transação Específica
```
GET /api/audit/transaction/{transactionId}
```

### Métricas

#### Métricas Gerais
```
GET /api/metrics?period=24h
```

Resposta:
```json
{
  "totalTransactions": 1500,
  "approvedTransactions": 1350,
  "suspiciousTransactions": 120,
  "fraudTransactions": 30,
  "approvalRate": 90.00,
  "fraudRate": 2.00,
  "suspiciousRate": 8.00,
  "period": "24h",
  "timestamp": "2025-02-16T11:41:30"
}
```

#### Métricas por MCC
```
GET /api/metrics/mcc?period=24h
```

#### Métricas por Merchant
```
GET /api/metrics/merchant?period=24h
```

#### Timeline de Métricas
```
GET /api/metrics/timeline?granularity=hourly
```

## Regras Implementadas

O motor de regras avalia as seguintes regras:

| Regra | Tipo | Descrição |
|-------|------|-----------|
| `LOW_AUTHENTICATION_SCORE` | SECURITY | Score de autenticação abaixo do threshold |
| `LOW_EXTERNAL_SCORE` | SECURITY | Score externo abaixo do threshold |
| `INVALID_CAVV` | SECURITY | CAVV inválido (3D Secure) |
| `INVALID_CRYPTOGRAM` | SECURITY | Criptograma do chip inválido |
| `CVV_MISMATCH` | SECURITY | CVV não corresponde |
| `HIGH_TRANSACTION_AMOUNT` | CONTEXT | Valor da transação acima do threshold |
| `HIGH_RISK_MCC` | CONTEXT | MCC de alto risco |
| `INTERNATIONAL_TRANSACTION` | CONTEXT | Transação internacional |
| `CARD_NOT_PRESENT` | CONTEXT | Cartão não presente |
| `PIN_VERIFICATION_FAILED` | SECURITY | Falha na verificação do PIN |
| `CVV_PIN_LIMIT_EXCEEDED` | SECURITY | Limite de tentativas excedido |
| `OFFLINE_PIN_FAILED` | SECURITY | Falha no PIN offline |

## Classificação de Transações

As transações são classificadas em três categorias baseadas no score de risco:

- **APPROVED** (0-30): Transação aprovada
- **SUSPICIOUS** (30-70): Transação suspeita, requer revisão
- **FRAUD** (70-100): Transação bloqueada como fraude

## Configuração Dinâmica de Regras

As regras podem ser ajustadas via API sem necessidade de redeploy:

- **Threshold**: Valor limite para ativação da regra
- **Weight**: Peso da regra no cálculo do score (0-100)
- **Enabled**: Ativar/desativar a regra
- **Classification**: Classificação que a regra pode gerar

## Auditoria e Compliance

Todas as ações são registradas para auditoria:

- Processamento de transações
- Criação/atualização/deleção de regras
- Alterações de configuração
- Erros e falhas

## Testes

```bash
# Executar testes
mvn test

# Com cobertura
mvn clean test jacoco:report
```

## Logs

Os logs são configurados em `application.yml`:

```yaml
logging:
  level:
    root: INFO
    com.rulex: DEBUG
```

## Performance

- **Processamento de transação**: < 100ms
- **Suporta**: 1000+ transações/segundo
- **Cache de regras**: Habilitado por padrão
- **Índices de banco de dados**: Otimizados para queries frequentes

## Segurança

- Validação de entrada em todos os endpoints
- Tratamento global de exceções
- CORS configurado para frontend
- Logs de auditoria completos

## Próximos Passos

1. Implementar autenticação OAuth2
2. Adicionar rate limiting
3. Implementar cache distribuído (Redis)
4. Adicionar métricas Prometheus
5. Implementar webhooks para notificações

## Suporte

Para dúvidas ou problemas, consulte a documentação técnica ou entre em contato com o time de desenvolvimento.
