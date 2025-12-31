# ENDPOINTS REAL MAP - RULEX

## Fonte
Mapeado a partir dos Controllers Java em:
- `backend/src/main/java/com/rulex/controller/`
- `backend/src/main/java/com/rulex/v31/`

**Context-Path:** `/api` (configurado em application.properties)

---

## 1. Regras Simples (`/api/rules`)

| Método | Endpoint | Descrição | Role |
|--------|----------|-----------|------|
| GET | `/rules` | Listar todas as regras | ANALYST, ADMIN |
| GET | `/rules/{id}` | Buscar regra por ID | ANALYST, ADMIN |
| POST | `/rules` | Criar nova regra | ADMIN |
| PUT | `/rules/{id}` | Atualizar regra | ADMIN |
| DELETE | `/rules/{id}` | Deletar regra | ADMIN |
| GET | `/rules/enabled/{enabled}` | Listar por status | ANALYST, ADMIN |
| GET | `/rules/{id}/history` | Histórico de versões | ANALYST, ADMIN |

### Exemplo: Criar Regra
```bash
curl -X POST http://localhost:8080/api/rules \
  -H "Content-Type: application/json" \
  -u admin:admin123 \
  -d '{
    "ruleName": "HIGH_AMOUNT_RULE",
    "description": "Transações acima de R$5000",
    "ruleType": "SECURITY",
    "classification": "SUSPICIOUS",
    "threshold": 0,
    "weight": 50,
    "enabled": true,
    "logicOperator": "AND",
    "conditions": [
      {"field": "transactionAmount", "operator": "GT", "value": "500000"}
    ]
  }'
```

---

## 2. Regras Complexas - CRUD (`/api/complex-rules`)

| Método | Endpoint | Descrição | Role |
|--------|----------|-----------|------|
| GET | `/complex-rules` | Listar regras complexas | ANALYST, ADMIN |
| GET | `/complex-rules/{id}` | Buscar por ID | ANALYST, ADMIN |
| GET | `/complex-rules/key/{key}` | Buscar por chave | ANALYST, ADMIN |
| POST | `/complex-rules` | Criar regra complexa | ADMIN |
| PUT | `/complex-rules/{id}` | Atualizar regra | ADMIN |
| DELETE | `/complex-rules/{id}` | Deletar regra | ADMIN |
| POST | `/complex-rules/{id}/duplicate` | Duplicar regra | ADMIN |
| POST | `/complex-rules/validate` | Validar sem salvar | ANALYST, ADMIN |

### Exemplo: Criar Regra Complexa
```bash
curl -X POST http://localhost:8080/api/complex-rules \
  -H "Content-Type: application/json" \
  -u admin:admin123 \
  -d '{
    "key": "EXTREME_RULE_001",
    "title": "Regra Extrema de Fraude",
    "description": "Detecta padrões complexos",
    "status": "DRAFT",
    "priority": 100,
    "severity": 80,
    "decision": "SUSPEITA_DE_FRAUDE",
    "enabled": true,
    "rootConditionGroup": {
      "logicOperator": "AND",
      "conditions": [
        {"fieldName": "transactionAmount", "operator": "GT", "valueType": "NUMBER", "valueSingle": "1000"}
      ],
      "children": []
    }
  }'
```

---

## 3. Regras Complexas - Estrutura (`/api/v1/complex-rules`)

| Método | Endpoint | Descrição | Role |
|--------|----------|-----------|------|
| POST | `/{ruleVersionId}/conditions` | Salvar grupo de condições | ADMIN |
| GET | `/{ruleVersionId}/conditions` | Buscar grupo de condições | ANALYST, ADMIN |
| POST | `/{ruleVersionId}/expressions` | Salvar expressões | ADMIN |
| GET | `/{ruleVersionId}/expressions` | Buscar expressões | ANALYST, ADMIN |
| POST | `/{ruleVersionId}/variables` | Salvar variáveis | ADMIN |
| GET | `/{ruleVersionId}/variables` | Buscar variáveis | ANALYST, ADMIN |
| POST | `/{ruleVersionId}/actions` | Salvar ações | ADMIN |
| GET | `/{ruleVersionId}/actions` | Buscar ações | ANALYST, ADMIN |
| GET | `/{ruleVersionId}/fields` | Campos usados | ANALYST, ADMIN |
| GET | `/{ruleVersionId}/validate-depth` | Validar profundidade | ANALYST, ADMIN |
| POST | `/{ruleVersionId}/evaluate` | Avaliar com payload | ADMIN |
| DELETE | `/{ruleVersionId}` | Deletar estrutura | ADMIN |

### Templates
| Método | Endpoint | Descrição | Role |
|--------|----------|-----------|------|
| GET | `/templates` | Listar todos | ANALYST, ADMIN |
| GET | `/templates/system` | Templates do sistema | ANALYST, ADMIN |
| GET | `/templates/category/{category}` | Por categoria | ANALYST, ADMIN |
| GET | `/templates/{name}` | Por nome | ANALYST, ADMIN |

---

## 4. Avaliação (`/api/evaluate`, `/api/transactions`)

| Método | Endpoint | Descrição | Role |
|--------|----------|-----------|------|
| POST | `/evaluate` | Avaliar transação | PUBLIC |
| POST | `/transactions/analyze` | Analisar transação | PUBLIC |
| POST | `/transactions/analyze-advanced` | Análise avançada | PUBLIC |
| GET | `/transactions` | Listar transações | ANALYST, ADMIN |
| GET | `/transactions/{id}` | Buscar por ID | ANALYST, ADMIN |
| GET | `/transactions/external/{externalId}` | Buscar por ID externo | ANALYST, ADMIN |
| GET | `/transactions/export` | Exportar | ANALYST, ADMIN |

### Exemplo: Avaliar Transação
```bash
curl -X POST http://localhost:8080/api/evaluate \
  -H "Content-Type: application/json" \
  -d '{
    "externalTransactionId": "TXN-001",
    "customerIdFromHeader": "CUST-123",
    "pan": "4111111111111111",
    "transactionAmount": 100000,
    "transactionCurrencyCode": 986,
    "merchantId": "MERCH-001",
    "merchantCountryCode": "076",
    "mcc": 5411
  }'
```

---

## 5. Simulação (`/api/rules/simulation`)

| Método | Endpoint | Descrição | Role |
|--------|----------|-----------|------|
| POST | `/rules/simulation/test` | Testar regra | ADMIN |
| POST | `/rules/simulation/backtest/{ruleId}` | Backtest | ADMIN |
| POST | `/rules/simulation/compare` | Comparar regras | ADMIN |
| POST | `/rules/simulation/batch` | Batch de transações | ADMIN |

---

## 6. Validação e Lint (`/api/rules`)

| Método | Endpoint | Descrição | Role |
|--------|----------|-----------|------|
| POST | `/rules/validate` | Validar regra | ANALYST, ADMIN |
| POST | `/rules/lint` | Lint de regra | ANALYST, ADMIN |
| POST | `/rules/simulate` | Simular regra | ADMIN |

---

## 7. Auditoria (`/api/audit`)

| Método | Endpoint | Descrição | Role |
|--------|----------|-----------|------|
| GET | `/audit` | Listar logs de auditoria | ANALYST, ADMIN |
| GET | `/audit/export` | Exportar logs | ANALYST, ADMIN |
| GET | `/audit/transaction/{transactionId}` | Por transação | ANALYST, ADMIN |

---

## 8. Métricas (`/api/metrics`, `/api/rules/metrics`)

| Método | Endpoint | Descrição | Role |
|--------|----------|-----------|------|
| GET | `/metrics` | Métricas gerais | ANALYST, ADMIN |
| GET | `/metrics/mcc` | Por MCC | ANALYST, ADMIN |
| GET | `/metrics/merchant` | Por merchant | ANALYST, ADMIN |
| GET | `/metrics/timeline` | Timeline | ANALYST, ADMIN |
| GET | `/rules/metrics/dashboard` | Dashboard | ANALYST, ADMIN |
| GET | `/rules/metrics/{ruleId}` | Por regra | ANALYST, ADMIN |
| GET | `/rules/metrics/all` | Todas as regras | ANALYST, ADMIN |
| POST | `/rules/metrics/{ruleId}/false-positive` | Marcar FP | ADMIN |
| POST | `/rules/metrics/{ruleId}/true-positive` | Marcar TP | ADMIN |

---

## 9. Export/Import (`/api/v1/rules/export-import`)

| Método | Endpoint | Descrição | Role |
|--------|----------|-----------|------|
| GET | `/export` | Exportar todas | ADMIN |
| POST | `/export/selective` | Exportar selecionadas | ADMIN |
| GET | `/export/complex` | Exportar complexas | ADMIN |
| POST | `/import` | Importar JSON | ADMIN |
| POST | `/import/file` | Importar arquivo | ADMIN |
| POST | `/validate` | Validar import | ADMIN |
| GET | `/template/simple` | Template simples | ANALYST, ADMIN |
| GET | `/template/complex` | Template complexo | ANALYST, ADMIN |

---

## 10. Homologação (`/api/homolog`)

| Método | Endpoint | Descrição | Role |
|--------|----------|-----------|------|
| POST | `/homolog/rules` | Criar regra | ADMIN |
| GET | `/homolog/rules/{ruleId}/latest` | Última versão | ADMIN |
| POST | `/homolog/rules/versions/{versionId}/publish` | Publicar | ADMIN |
| POST | `/homolog/rules/{ruleId}/rollback/{version}` | Rollback | ADMIN |
| POST | `/homolog/rulesets` | Criar ruleset | ADMIN |
| POST | `/homolog/rulesets/versions/{versionId}/publish` | Publicar | ADMIN |
| POST | `/homolog/rulesets/activate` | Ativar | ADMIN |
| POST | `/homolog/simulations/run` | Executar simulação | ADMIN |

---

## 11. Field Dictionary (`/api/field-dictionary`)

| Método | Endpoint | Descrição | Role |
|--------|----------|-----------|------|
| GET | `/field-dictionary` | Listar campos | ANALYST, ADMIN |

---

## 12. Aprovações (`/api/rules/approvals`)

| Método | Endpoint | Descrição | Role |
|--------|----------|-----------|------|
| POST | `/create` | Solicitar criação | ADMIN |
| POST | `/update/{ruleId}` | Solicitar update | ADMIN |
| POST | `/delete/{ruleId}` | Solicitar delete | ADMIN |
| POST | `/{id}/approve` | Aprovar | ADMIN |
| POST | `/{id}/reject` | Rejeitar | ADMIN |
| POST | `/{id}/cancel` | Cancelar | ADMIN |
| GET | `/pending` | Listar pendentes | ADMIN |
| GET | `/pending/page` | Paginado | ADMIN |
| GET | `/pending/count` | Contar pendentes | ADMIN |
| GET | `/{id}` | Buscar por ID | ADMIN |
| GET | `/history/{ruleId}` | Histórico | ADMIN |

---

## Autenticação

**Tipo:** HTTP Basic Auth

**Usuários (dev):**
- `admin:admin123` → Role: ADMIN
- `analyst:analyst123` → Role: ANALYST

**Headers:**
```
Authorization: Basic YWRtaW46YWRtaW4xMjM=
Content-Type: application/json
```

---

## Códigos de Resposta

| Código | Significado |
|--------|-------------|
| 200 | Sucesso |
| 201 | Criado |
| 204 | Sem conteúdo (delete) |
| 400 | Requisição inválida |
| 401 | Não autenticado |
| 403 | Não autorizado |
| 404 | Não encontrado |
| 409 | Conflito (versão) |
| 500 | Erro interno |
