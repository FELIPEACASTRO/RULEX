# ENDPOINTS REAL MAP - RULEX

## Data da Auditoria
2024-12-31T23:05:00Z

## Base URL
- Context Path: `/api`
- Full URL: `http://localhost:8080/api`

---

## 1. RULES (RuleController)

### GET /api/rules
Lista todas as regras com paginação.

```bash
curl -u admin:rulex http://localhost:8080/api/rules
```

**Response:** `Page<RuleConfiguration>`

### GET /api/rules/{id}
Busca regra por ID.

```bash
curl -u admin:rulex http://localhost:8080/api/rules/1
```

### POST /api/rules
Cria nova regra. **Requer ADMIN.**

```bash
curl -u admin:rulex -X POST \
  -H "Content-Type: application/json" \
  -d '{
    "ruleName": "HIGH_AMOUNT",
    "ruleType": "SECURITY",
    "classification": "SUSPICIOUS",
    "logicOperator": "AND",
    "conditions": [{"field": "transactionAmount", "operator": "GT", "value": "10000"}]
  }' \
  http://localhost:8080/api/rules
```

### PUT /api/rules/{id}
Atualiza regra existente. **Requer ADMIN + version para optimistic locking.**

```bash
curl -u admin:rulex -X PUT \
  -H "Content-Type: application/json" \
  -d '{
    "ruleName": "HIGH_AMOUNT_UPDATED",
    "ruleType": "SECURITY",
    "classification": "FRAUD",
    "logicOperator": "AND",
    "conditions": [],
    "version": 1
  }' \
  http://localhost:8080/api/rules/1
```

### DELETE /api/rules/{id}
Deleta regra. **Requer ADMIN.**

```bash
curl -u admin:rulex -X DELETE http://localhost:8080/api/rules/1
```

---

## 2. COMPLEX RULES (ComplexRuleCrudController)

### GET /api/complex-rules
Lista regras complexas.

```bash
curl -u admin:rulex http://localhost:8080/api/complex-rules
```

### GET /api/complex-rules/{id}
Busca regra complexa por ID.

### GET /api/complex-rules/key/{key}
Busca regra complexa por chave.

### POST /api/complex-rules
Cria regra complexa. **Requer ADMIN.**

### PUT /api/complex-rules/{id}
Atualiza regra complexa. **Requer ADMIN.**

### DELETE /api/complex-rules/{id}
Deleta regra complexa. **Requer ADMIN.**

### POST /api/complex-rules/{id}/duplicate
Duplica regra complexa. **Requer ADMIN.**

### POST /api/complex-rules/validate
Valida regra sem salvar.

---

## 3. EVALUATE (EvaluateController)

### POST /api/evaluate
Avalia transação contra regras. **Público.**

```bash
curl -X POST \
  -H "Content-Type: application/json" \
  -d '{
    "externalTransactionId": "TXN123",
    "transactionAmount": 15000,
    "mcc": 5411
  }' \
  http://localhost:8080/api/evaluate
```

**Response:**
```json
{
  "transactionId": "TXN123",
  "classification": "SUSPICIOUS",
  "riskScore": 75,
  "triggeredRules": [
    {"name": "HIGH_AMOUNT", "weight": 50, "contribution": 50}
  ]
}
```

---

## 4. TRANSACTIONS (TransactionController)

### POST /api/transactions/analyze
Analisa transação. **Público.**

### POST /api/transactions/analyze-advanced
Análise avançada com contexto derivado. **Público.**

### GET /api/transactions
Lista transações. **Requer ANALYST ou ADMIN.**

### GET /api/transactions/{id}
Busca transação por ID.

### GET /api/transactions/external/{externalId}
Busca por ID externo.

### GET /api/transactions/export
Exporta transações.

---

## 5. AUDIT (AuditController)

### GET /api/audit
Lista logs de auditoria. **Requer ANALYST ou ADMIN.**

### GET /api/audit/export
Exporta logs de auditoria.

### GET /api/audit/transaction/{transactionId}
Logs de uma transação específica.

---

## 6. SIMULATION (RuleSimulationController)

### POST /api/rules/simulation/test
Testa regra com transação de exemplo. **Requer ADMIN.**

### POST /api/rules/simulation/backtest/{ruleId}
Backtest de regra com histórico.

### POST /api/rules/simulation/compare
Compara duas versões de regra.

### POST /api/rules/simulation/batch
Simulação em lote.

---

## 7. METRICS (MetricsController)

### GET /api/metrics
Métricas gerais. **Requer ANALYST ou ADMIN.**

### GET /api/metrics/mcc
Métricas por MCC.

### GET /api/metrics/merchant
Métricas por merchant.

### GET /api/metrics/timeline
Timeline de métricas.

---

## 8. FIELD DICTIONARY (FieldDictionaryController)

### GET /api/field-dictionary
Lista campos disponíveis. **Requer ANALYST ou ADMIN.**

```bash
curl -u analyst:rulex "http://localhost:8080/api/field-dictionary?workflow=BRZLCREDIT&recordType=CRTRAN25&portfolio=*"
```

---

## 9. APPROVALS (RuleApprovalController)

### POST /api/rules/approvals/create
Solicita aprovação para criar regra.

### POST /api/rules/approvals/update/{ruleId}
Solicita aprovação para atualizar.

### POST /api/rules/approvals/delete/{ruleId}
Solicita aprovação para deletar.

### POST /api/rules/approvals/{id}/approve
Aprova solicitação. **Requer ADMIN.**

### POST /api/rules/approvals/{id}/reject
Rejeita solicitação.

### GET /api/rules/approvals/pending
Lista solicitações pendentes.

---

## 10. EXPORT/IMPORT (RuleExportImportController)

### GET /api/v1/rules/export-import/export
Exporta todas as regras.

### POST /api/v1/rules/export-import/import
Importa regras.

### GET /api/v1/rules/export-import/template/simple
Template de regra simples.

### GET /api/v1/rules/export-import/template/complex
Template de regra complexa.

---

## 11. HOMOLOG (HomologRuleController, HomologRuleSetController)

### POST /api/homolog/rules
Cria regra em homologação. **Requer ADMIN.**

### GET /api/homolog/rules/{ruleId}/latest
Última versão da regra.

### POST /api/homolog/rules/versions/{ruleVersionId}/publish
Publica versão.

### POST /api/homolog/rules/{ruleId}/rollback/{version}
Rollback para versão anterior.

### POST /api/homolog/rulesets
Cria ruleset.

### POST /api/homolog/rulesets/activate
Ativa ruleset.

### POST /api/homolog/simulations/run
Executa simulação em homologação.

---

## RBAC Summary

| Endpoint Pattern | ANALYST | ADMIN |
|-----------------|---------|-------|
| GET /api/rules/** | ✅ | ✅ |
| POST/PUT/DELETE /api/rules/** | ❌ | ✅ |
| GET /api/transactions/** | ✅ | ✅ |
| GET /api/audit/** | ✅ | ✅ |
| GET /api/metrics/** | ✅ | ✅ |
| POST /api/evaluate | ✅ (público) | ✅ |
| /api/homolog/** | ❌ | ✅ |

---

## Última Atualização
2024-12-31T23:05:00Z
