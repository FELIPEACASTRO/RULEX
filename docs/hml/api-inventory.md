# Inventário de API (código como verdade)

## Contrato de erro (padrão)
A API Java padroniza respostas de erro via `GlobalExceptionHandler`.

Formato:
```json
{
  "timestamp": "2025-01-01T12:00:00Z",
  "status": 400,
  "error": "Bad Request",
  "message": "Falha de validação",
  "path": "/api/transactions"
}
```

Códigos principais:
- `400` validação/argumentos inválidos e datas em formato inválido.
- `404` recurso inexistente (quando serviços lançam `NotFoundException`).
- `409` conflito/estado inválido (quando serviços lançam `IllegalStateException`).
- `500` erro inesperado.

---

## API Java (Spring Boot)
Base URL: `{{java_api_url}}` (ex.: `http://localhost:8080/api`)

### Transactions
- `POST /transactions/analyze`
  - Auth: não aplicável (não há autenticação no controller)
  - Request: `TransactionRequest`
    - obrigatórios (`@NotBlank/@NotNull`):
      - `externalTransactionId`, `customerIdFromHeader`, `customerAcctNumber`, `pan`
      - `transactionCurrencyCode`, `transactionAmount`, `transactionDate`, `transactionTime`, `mcc`
      - `consumerAuthenticationScore`, `externalScore3`, `cavvResult`
      - `eciIndicator`, `atcCard`, `atcHost`, `tokenAssuranceLevel`
      - `availableCredit`, `cardCashBalance`, `cardDelinquentAmount`
  - Response: `TransactionResponse`
  - Observações:
    - **Idempotente** por `externalTransactionId` (repetição retorna a decisão persistida).

- `POST /transactions/analyze-advanced`
  - Request: `TransactionRequest`
  - Response: `TransactionResponse` (com `rulesetVersion = "advanced"`)

- `GET /transactions`
  - Query params:
    - `customerId`, `merchantId`, `mcc`, `minAmount`, `maxAmount`
    - `startDate`, `endDate` em `ISO_DATE_TIME` (ex.: `2025-01-01T00:00:00`)
  - Response: `Page<TransactionResponse>`

- `GET /transactions/{id}`
  - Response: `TransactionResponse`

- `GET /transactions/external/{externalId}`
  - Response: `TransactionResponse`

### Rules
- `GET /rules` (paginado)
- `GET /rules/{id}`
- `POST /rules` (cria)
- `PUT /rules/{id}` (atualiza)
- `DELETE /rules/{id}`
- `PATCH /rules/{id}/toggle`
- `GET /rules/enabled/{enabled}`
- `GET /rules/{id}/history`

Request/Response principal:
- `RuleConfigurationDTO`:
  - obrigatórios: `ruleName`, `ruleType`, `threshold`, `weight`, `enabled`, `classification`, `conditions`, `logicOperator`
  - `conditions`: lista de `{ field, operator, value }`.

### Audit
- `GET /audit`
  - Query params: `actionType`, `result`, `startDate`, `endDate` (ISO_DATE_TIME)
  - Response: `Page<AuditLogDTO>`

- `GET /audit/transaction/{transactionId}`
  - Response: `Page<AuditLogDTO>`

### Metrics
- `GET /metrics` (query: `period` opcional)
- `GET /metrics/mcc` (query: `period` opcional)
- `GET /metrics/merchant` (query: `period` opcional)
- `GET /metrics/timeline` (query: `granularity` opcional)

### Homolog (Rules/RuleSets/Simulations)
- `POST /homolog/rules` (header opcional `X-Actor-Email`)
- `GET /homolog/rules/{ruleId}/latest`
- `POST /homolog/rules/versions/{ruleVersionId}/publish`
- `POST /homolog/rules/{ruleId}/rollback/{version}`

- `POST /homolog/rulesets` (cria draft)
- `POST /homolog/rulesets/versions/{ruleSetVersionId}/publish`
- `POST /homolog/rulesets/activate`

- `POST /homolog/simulations/run`

---

## API Node (Express + tRPC)
Base URL: `{{node_base_url}}` (ex.: `http://localhost:3000`)

### OAuth
- `GET /api/oauth/callback?code=...&state=...`
  - Define cookie de sessão e redireciona para `/`.

### tRPC
Mount: `POST/GET /api/trpc/*` (via `@trpc/server/adapters/express`, transformer `superjson`).

Procedures (router path):
- `system.health` (public, query; input `{ timestamp: number }`)
- `system.notifyOwner` (admin, mutation; input `{ title: string, content: string }`)

- `auth.me` (public, query)
- `auth.logout` (public, mutation)

- `rules.list` (public, query)
- `rules.listActive` (public, query)
- `rules.getById` (public, query; input `{ id: number }`)
- `rules.create` (protected)
- `rules.update` (protected)
- `rules.delete` (protected)
- `rules.toggle` (protected)
- `rules.history` (public, query; input `{ ruleId: number }`)

- `audit.list` (public, query; input opcional `{ limit?: number }`)
- `metrics.get` (public, query)

Auth tRPC:
- `protectedProcedure` exige `ctx.user` (via cookie de sessão).
- `adminProcedure` exige `ctx.user.role === "admin"`.
