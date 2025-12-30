# MEGACOMMITTEE_RULES_CRUD_EVIDENCE.md
## Evidências de CRUD de Regras - MEGA COMITÊ

**Data:** 2025-12-30
**Auditor:** MEGA COMITÊ de Auditoria

---

## RESUMO DE TESTES

| Operação | Status | Evidência |
|----------|--------|-----------|
| CREATE BÁSICA | ✅ PASS | Seção 1 |
| CREATE COMPOSTA | ✅ PASS | Seção 2 |
| CREATE OR | ✅ PASS | Seção 3 |
| PUBLISH | ✅ PASS | Seção 4 |
| CREATE RULESET | ✅ PASS | Seção 5 |
| PUBLISH RULESET | ✅ PASS | Seção 5 |
| ACTIVATE | ✅ PASS | Seção 5 |
| SIMULATE (alto) | ✅ PASS | Seção 6 |
| SIMULATE (baixo) | ✅ PASS | Seção 7 |
| ROLLBACK | ✅ PASS | Seção 8 |
| PERSISTÊNCIA DB | ✅ PASS | Seção 9 |
| AUDIT TRAIL | ✅ PASS | Seção 10 |

---

## 1. CREATE - Regra BÁSICA (comparação GT)

### Request
```bash
curl -X POST -u admin:rulex http://localhost:8080/api/homolog/rules \
  -d '{
    "key": "MEGA_BASIC_001",
    "title": "Regra Básica - Valor Alto",
    "priority": 100,
    "severity": 70,
    "decision": "SUSPEITA_DE_FRAUDE",
    "reasonTemplate": "Transação com valor > R$5000",
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
  "id": "b408fd27-a0c7-4da1-a240-420e5ede3ac4",
  "key": "MEGA_BASIC_001",
  "version": 1,
  "status": "DRAFT",
  "decision": "SUSPEITA_DE_FRAUDE"
}
```

---

## 2. CREATE - Regra COMPOSTA (AND + múltiplas condições)

### Response ✅
```json
{
  "id": "00372a55-43c4-411d-b33f-efb9e39b6b4a",
  "key": "MEGA_COMPOSITE_001",
  "version": 1,
  "status": "DRAFT",
  "decision": "FRAUDE",
  "logic": "AND"
}
```

**Condições:**
- transactionAmount > 10000
- merchantCountryCode != 076
- mcc IN (7995, 6211, 6051, 7273)

---

## 3. CREATE - Regra com lógica OR

### Response ✅
```json
{
  "id": "bd51929b-71b8-4320-b687-6b171acdcf6e",
  "key": "MEGA_OR_001",
  "version": 1,
  "status": "DRAFT",
  "logic": "OR"
}
```

**Condições:**
- consumerAuthenticationScore < 30 **OR**
- transactionAmount > 50000

---

## 4. PUBLISH

### Response ✅
```json
{
  "id": "b408fd27-a0c7-4da1-a240-420e5ede3ac4",
  "status": "PUBLISHED"
}
```

---

## 5. RULESET (CREATE + PUBLISH + ACTIVATE)

### CREATE Response ✅
```json
{
  "id": "9c17adb2-c25b-4b2c-8d16-089c84f057d6",
  "status": "DRAFT",
  "ruleVersionIds": ["b408fd27-a0c7-4da1-a240-420e5ede3ac4"]
}
```

### PUBLISH Response ✅
```json
{"status": "PUBLISHED"}
```

### ACTIVATE ✅
```
HTTP 200 OK
```

---

## 6. SIMULATE - Valor ALTO (deve disparar)

### Request
```json
{
  "payload": {
    "transactionAmount": 15000,
    "merchantCountryCode": "076",
    "mcc": "5411"
  }
}
```

### Response ✅
```json
{
  "decision": "SUSPEITA_DE_FRAUDE",
  "riskScore": 70,
  "triggeredRules": [
    {
      "name": "MEGA_BASIC_001",
      "weight": 70,
      "contribution": 70,
      "detail": "Transação com valor > R$5000"
    }
  ],
  "explain": {
    "triggeredCount": 1
  }
}
```

---

## 7. SIMULATE - Valor BAIXO (NÃO deve disparar)

### Request
```json
{
  "payload": {
    "transactionAmount": 1000
  }
}
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

## 8. ROLLBACK

### Response ✅
```json
{
  "id": "12249dbb-1cf0-4fef-a649-7a58b3a0bdc7",
  "version": 2,
  "status": "DRAFT"
}
```

---

## 9. PERSISTÊNCIA DB

### Regras
```sql
SELECT key, title FROM rules;
```
```
        key         |                           title                            
--------------------+------------------------------------------------------------
 MEGA_BASIC_001     | Regra Básica - Valor Alto
 MEGA_COMPOSITE_001 | Regra Composta - Alto Valor + País Estrangeiro + MCC Risco
 MEGA_OR_001        | Regra OR - Score Baixo OU Valor Extremo
(3 rows)
```

### Versões
```sql
SELECT r.key, rv.version, rv.status, rv.decision FROM rule_versions rv JOIN rules r ON rv.rule_id = r.id;
```
```
        key         | version |  status   |      decision      
--------------------+---------+-----------+--------------------
 MEGA_BASIC_001     |       1 | PUBLISHED | SUSPEITA_DE_FRAUDE
 MEGA_BASIC_001     |       2 | DRAFT     | SUSPEITA_DE_FRAUDE
 MEGA_COMPOSITE_001 |       1 | DRAFT     | FRAUDE
 MEGA_OR_001        |       1 | DRAFT     | SUSPEITA_DE_FRAUDE
(4 rows)
```

---

## 10. AUDIT TRAIL

```sql
SELECT action_type, entity_type, result FROM audit_log ORDER BY created_at DESC LIMIT 10;
```
```
    action_type    |    entity_type    | result  
-------------------+-------------------+---------
 RULE_UPDATED      | rules             | SUCCESS
 SIMULATION_RUN    | simulation_runs   | SUCCESS
 SIMULATION_RUN    | simulation_runs   | SUCCESS
 RULESET_ACTIVATED | active_rule_set   | SUCCESS
 RULESET_PUBLISHED | rule_set_versions | SUCCESS
 RULE_CREATED      | rule_sets         | SUCCESS
 RULE_PUBLISHED    | rule_versions     | SUCCESS
 RULE_CREATED      | rule              | SUCCESS
 RULE_CREATED      | rule              | SUCCESS
 RULE_CREATED      | rule              | SUCCESS
(10 rows)
```

---

## CONCLUSÃO

**CICLO COMPLETO VALIDADO:**
```
CRIAR → PUBLICAR → RULESET → ATIVAR → SIMULAR → ROLLBACK
  ✅       ✅         ✅        ✅        ✅         ✅
```

**STATUS: GATE 6 - PASS**

---

**Documento gerado pelo MEGA COMITÊ de Auditoria**
