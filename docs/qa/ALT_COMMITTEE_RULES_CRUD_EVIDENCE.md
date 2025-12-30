# ALT_COMMITTEE_RULES_CRUD_EVIDENCE.md
## Evidências de CRUD de Regras - Auditoria Comitê Alternativo

**Data:** 2025-12-30
**Auditor:** Comitê Alternativo de Auditoria

---

## RESUMO DE TESTES

| Operação | Status | Evidência |
|----------|--------|-----------|
| CREATE | ✅ PASS | Seção 1 |
| PUBLISH | ✅ PASS | Seção 2 |
| CREATE RULESET | ✅ PASS | Seção 3 |
| PUBLISH RULESET | ✅ PASS | Seção 4 |
| ACTIVATE | ✅ PASS | Seção 5 |
| SIMULATE (alto) | ✅ PASS | Seção 6 |
| ROLLBACK | ✅ PASS | Seção 7 |
| SIMULATE (baixo) | ✅ PASS | Seção 8 |

---

## 1. CREATE - Regra Simples

### Request
```bash
curl -X POST -u admin:rulex http://localhost:8080/api/homolog/rules \
  -H "Content-Type: application/json" \
  -H "X-Actor-Email: admin@rulex.local" \
  -d '{
    "key": "AUDIT_RULE_001",
    "title": "Regra de Auditoria - Valor Alto",
    "priority": 100,
    "severity": 70,
    "decision": "SUSPEITA_DE_FRAUDE",
    "reasonTemplate": "Transação com valor acima de R$5000",
    "fieldsUsed": ["transactionAmount"],
    "logic": "AND",
    "conditions": [
      {"field": "transactionAmount", "operator": "GT", "value": "5000"}
    ],
    "enabled": true
  }'
```

### Response ✅
```json
{
  "id": "5b0edc12-60c6-4e3d-85ec-d09fccf106d8",
  "ruleId": "7ca5636f-5f29-4b73-b183-d97991bcb48f",
  "key": "AUDIT_RULE_001",
  "version": 1,
  "status": "DRAFT",
  "decision": "SUSPEITA_DE_FRAUDE"
}
```

---

## 2. PUBLISH

### Request
```bash
curl -X POST -u admin:rulex \
  "http://localhost:8080/api/homolog/rules/versions/5b0edc12-60c6-4e3d-85ec-d09fccf106d8/publish"
```

### Response ✅
```json
{
  "id": "5b0edc12-60c6-4e3d-85ec-d09fccf106d8",
  "status": "PUBLISHED"
}
```

---

## 3. CREATE RULESET

### Request
```bash
curl -X POST -u admin:rulex http://localhost:8080/api/homolog/rulesets \
  -H "Content-Type: application/json" \
  -d '{
    "key": "AUDIT_RULESET_001",
    "title": "RuleSet de Auditoria",
    "ruleVersionIds": ["5b0edc12-60c6-4e3d-85ec-d09fccf106d8"],
    "notes": "RuleSet para testes"
  }'
```

### Response ✅
```json
{
  "id": "e95590f5-8bc0-427e-9108-7525638f263d",
  "status": "DRAFT",
  "ruleVersionIds": ["5b0edc12-60c6-4e3d-85ec-d09fccf106d8"]
}
```

---

## 4. PUBLISH RULESET

### Response ✅
```json
{
  "id": "e95590f5-8bc0-427e-9108-7525638f263d",
  "status": "PUBLISHED"
}
```

---

## 5. ACTIVATE

### Request
```bash
curl -X POST -u admin:rulex http://localhost:8080/api/homolog/rulesets/activate \
  -d '{"ruleSetVersionId": "e95590f5-8bc0-427e-9108-7525638f263d"}'
```

### Response ✅
```
HTTP 200 OK
```

---

## 6. SIMULATE - Valor ALTO (deve disparar)

### Request
```bash
curl -X POST -u admin:rulex http://localhost:8080/api/homolog/simulations/run \
  -d '{
    "payload": {
      "externalTransactionId": "TXN-AUDIT-001",
      "customerIdFromHeader": "CPF12345678901",
      "customerAcctNumber": 1234567890,
      "pan": "4111111111111111",
      "transactionAmount": 15000,
      "transactionCurrencyCode": 986
    }
  }'
```

### Response ✅
```json
{
  "decision": "SUSPEITA_DE_FRAUDE",
  "riskScore": 70,
  "triggeredRules": [
    {
      "name": "AUDIT_RULE_001",
      "weight": 70,
      "contribution": 70,
      "detail": "Transação com valor acima de R$5000"
    }
  ],
  "explain": {
    "triggeredCount": 1
  }
}
```

---

## 7. ROLLBACK

### Request
```bash
curl -X POST -u admin:rulex \
  "http://localhost:8080/api/homolog/rules/7ca5636f-5f29-4b73-b183-d97991bcb48f/rollback/1"
```

### Response ✅
```json
{
  "id": "7d93a3e3-68f5-41af-9efa-3f24f9006f06",
  "version": 2,
  "status": "DRAFT"
}
```

---

## 8. SIMULATE - Valor BAIXO (não deve disparar)

### Request
```bash
# transactionAmount: 1000 (abaixo de 5000)
```

### Response ✅
```json
{
  "decision": "APROVADO",
  "riskScore": 0,
  "triggeredRules": [],
  "explain": {
    "triggeredCount": 0
  }
}
```

---

## EVIDÊNCIA DE PERSISTÊNCIA (DB)

```sql
-- Regras
SELECT * FROM rules;
-- 1 row: AUDIT_RULE_001

-- Versões
SELECT * FROM rule_versions;
-- 2 rows: v1 PUBLISHED, v2 DRAFT

-- Simulações
SELECT COUNT(*) FROM simulation_runs;
-- 2 simulações

-- Audit Log
SELECT COUNT(*) FROM audit_log;
-- 8 entradas
```

---

## CONCLUSÃO

**CICLO COMPLETO VALIDADO:**
```
CRIAR → PUBLICAR → RULESET → ATIVAR → SIMULAR → ROLLBACK → SIMULAR
  ✅       ✅         ✅        ✅        ✅         ✅         ✅
```

**STATUS: GATE 6 - PASS**

---

**Documento gerado pelo Comitê Alternativo de Auditoria**
