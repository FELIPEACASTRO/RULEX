# ENDPOINTS REAL MAP - RULEX

## Última Atualização: 2024-12-31

---

## 1. Regras Simples (`/api/rules`)

### 1.1 Listar Regras
```bash
curl -X GET "http://localhost:8080/api/rules" \
  -H "Authorization: Basic YWRtaW46cnVsZXg=" \
  -H "Content-Type: application/json"
```

**Response:**
```json
{
  "content": [
    {
      "id": 1,
      "ruleName": "HIGH_AMOUNT_RULE",
      "description": "Bloqueia transações acima de R$ 10.000",
      "ruleType": "SECURITY",
      "classification": "SUSPICIOUS",
      "threshold": 1000000,
      "weight": 80,
      "enabled": true,
      "conditionsJson": "[{\"field\":\"transactionAmount\",\"operator\":\"GT\",\"value\":\"1000000\"}]",
      "logicOperator": "AND"
    }
  ],
  "totalElements": 1,
  "totalPages": 1
}
```

### 1.2 Criar Regra
```bash
curl -X POST "http://localhost:8080/api/rules" \
  -H "Authorization: Basic YWRtaW46cnVsZXg=" \
  -H "Content-Type: application/json" \
  -d '{
    "ruleName": "HIGH_AMOUNT_RULE",
    "description": "Bloqueia transações acima de R$ 10.000",
    "ruleType": "SECURITY",
    "classification": "SUSPICIOUS",
    "threshold": 1000000,
    "weight": 80,
    "enabled": true,
    "conditionsJson": "[{\"field\":\"transactionAmount\",\"operator\":\"GT\",\"value\":\"1000000\"}]",
    "logicOperator": "AND"
  }'
```

### 1.3 Atualizar Regra
```bash
curl -X PUT "http://localhost:8080/api/rules/1" \
  -H "Authorization: Basic YWRtaW46cnVsZXg=" \
  -H "Content-Type: application/json" \
  -d '{
    "ruleName": "HIGH_AMOUNT_RULE",
    "description": "Atualizado",
    "ruleType": "SECURITY",
    "classification": "FRAUD",
    "threshold": 500000,
    "weight": 90,
    "enabled": true
  }'
```

### 1.4 Deletar Regra
```bash
curl -X DELETE "http://localhost:8080/api/rules/1" \
  -H "Authorization: Basic YWRtaW46cnVsZXg="
```

### 1.5 Toggle Enabled
```bash
curl -X PUT "http://localhost:8080/api/rules/1/toggle" \
  -H "Authorization: Basic YWRtaW46cnVsZXg="
```

---

## 2. Regras Complexas (`/api/v1/complex-rules`)

### 2.1 Listar Regras Complexas
```bash
curl -X GET "http://localhost:8080/api/v1/complex-rules" \
  -H "Authorization: Basic YWRtaW46cnVsZXg=" \
  -H "Content-Type: application/json"
```

### 2.2 Criar Regra Complexa
```bash
curl -X POST "http://localhost:8080/api/v1/complex-rules" \
  -H "Authorization: Basic YWRtaW46cnVsZXg=" \
  -H "Content-Type: application/json" \
  -d '{
    "key": "GAMBLING_INTERNATIONAL",
    "title": "Gambling Internacional",
    "description": "Detecta transações de gambling fora do Brasil",
    "status": "DRAFT",
    "priority": 90,
    "severity": 85,
    "decision": "FRAUDE",
    "reasonTemplate": "Transação de gambling no país ${merchantCountryCode}",
    "enabled": true,
    "rootConditionGroup": {
      "logicOperator": "AND",
      "conditions": [
        {
          "fieldName": "mcc",
          "operator": "IN",
          "valueType": "NUMBER",
          "valueArray": ["7995", "7994"]
        },
        {
          "fieldName": "merchantCountryCode",
          "operator": "NEQ",
          "valueType": "STRING",
          "valueSingle": "076"
        }
      ],
      "children": []
    }
  }'
```

### 2.3 Validar Regra (sem salvar)
```bash
curl -X POST "http://localhost:8080/api/v1/complex-rules/validate" \
  -H "Authorization: Basic YWRtaW46cnVsZXg=" \
  -H "Content-Type: application/json" \
  -d '{
    "key": "TEST_RULE",
    "title": "Test",
    "rootConditionGroup": {
      "logicOperator": "AND",
      "conditions": [
        {
          "fieldName": "transactionAmount",
          "operator": "GT",
          "valueSingle": "1000"
        }
      ]
    }
  }'
```

### 2.4 Duplicar Regra
```bash
curl -X POST "http://localhost:8080/api/v1/complex-rules/{id}/duplicate?newKey=COPY_RULE" \
  -H "Authorization: Basic YWRtaW46cnVsZXg="
```

---

## 3. Complex Rules CRUD (`/api/complex-rules`)

**Nota:** Endpoint alternativo para CRUD de regras complexas.

### 3.1 Listar
```bash
curl -X GET "http://localhost:8080/api/complex-rules" \
  -H "Authorization: Basic YWRtaW46cnVsZXg="
```

### 3.2 Buscar por Key
```bash
curl -X GET "http://localhost:8080/api/complex-rules/key/GAMBLING_INTERNATIONAL" \
  -H "Authorization: Basic YWRtaW46cnVsZXg="
```

---

## 4. Avaliação de Transações (`/api/evaluate`)

### 4.1 Avaliar Transação
```bash
curl -X POST "http://localhost:8080/api/evaluate" \
  -H "Authorization: Basic YWRtaW46cnVsZXg=" \
  -H "Content-Type: application/json" \
  -d '{
    "externalTransactionId": "TXN-001",
    "customerIdFromHeader": "CUST-001",
    "customerAcctNumber": 123456789,
    "pan": "4111111111111111",
    "merchantId": "MERCH-001",
    "merchantName": "Test Merchant",
    "merchantCity": "SAO PAULO",
    "merchantState": "SP",
    "merchantCountryCode": "076",
    "transactionAmount": 100000,
    "transactionCurrencyCode": 986,
    "transactionDate": 20241231,
    "transactionTime": 143000,
    "mcc": 5411,
    "consumerAuthenticationScore": 80,
    "externalScore3": 70,
    "cavvResult": 0,
    "eciIndicator": 5,
    "atcCard": 100,
    "atcHost": 100,
    "tokenAssuranceLevel": 1,
    "availableCredit": 500000,
    "cardCashBalance": 0,
    "cardDelinquentAmount": 0
  }'
```

**Response:**
```json
{
  "transactionId": "TXN-001",
  "classification": "APPROVED",
  "riskScore": 0,
  "reason": "Transação aprovada. Nenhuma regra crítica foi acionada.",
  "rulesetVersion": "1.0",
  "processingTimeMs": 45,
  "timestamp": "2024-12-31T14:30:00Z",
  "ruleHits": [],
  "popups": []
}
```

---

## 5. Transações (`/api/transactions`)

### 5.1 Analisar Transação
```bash
curl -X POST "http://localhost:8080/api/transactions/analyze" \
  -H "Authorization: Basic YWRtaW46cnVsZXg=" \
  -H "Content-Type: application/json" \
  -d '{ ... mesmo payload do /evaluate ... }'
```

### 5.2 Listar Transações
```bash
curl -X GET "http://localhost:8080/api/transactions?page=0&size=20" \
  -H "Authorization: Basic YWRtaW46cnVsZXg="
```

### 5.3 Buscar por ID
```bash
curl -X GET "http://localhost:8080/api/transactions/TXN-001" \
  -H "Authorization: Basic YWRtaW46cnVsZXg="
```

---

## 6. Simulação (`/api/rules/simulation`)

### 6.1 Executar Simulação
```bash
curl -X POST "http://localhost:8080/api/rules/simulation/run" \
  -H "Authorization: Basic YWRtaW46cnVsZXg=" \
  -H "Content-Type: application/json" \
  -d '{
    "ruleIds": [1, 2, 3],
    "transactionIds": ["TXN-001", "TXN-002"]
  }'
```

---

## 7. Auditoria (`/api/audit`)

### 7.1 Listar Logs de Auditoria
```bash
curl -X GET "http://localhost:8080/api/audit?page=0&size=20" \
  -H "Authorization: Basic YWRtaW46cnVsZXg="
```

### 7.2 Exportar Auditoria
```bash
curl -X GET "http://localhost:8080/api/audit/export?format=csv" \
  -H "Authorization: Basic YWRtaW46cnVsZXg="
```

---

## 8. Aprovações (`/api/rules/approvals`)

### 8.1 Listar Pendentes
```bash
curl -X GET "http://localhost:8080/api/rules/approvals/pending" \
  -H "Authorization: Basic YWRtaW46cnVsZXg="
```

### 8.2 Aprovar
```bash
curl -X POST "http://localhost:8080/api/rules/approvals/1/approve" \
  -H "Authorization: Basic YWRtaW46cnVsZXg=" \
  -H "Content-Type: application/json" \
  -d '{"comment": "Aprovado após revisão"}'
```

### 8.3 Rejeitar
```bash
curl -X POST "http://localhost:8080/api/rules/approvals/1/reject" \
  -H "Authorization: Basic YWRtaW46cnVsZXg=" \
  -H "Content-Type: application/json" \
  -d '{"comment": "Regra muito permissiva"}'
```

---

## 9. Métricas (`/api/rules/metrics`)

### 9.1 Métricas de Regras
```bash
curl -X GET "http://localhost:8080/api/rules/metrics" \
  -H "Authorization: Basic YWRtaW46cnVsZXg="
```

---

## 10. Homolog (`/api/homolog/*`)

### 10.1 Listar Rulesets
```bash
curl -X GET "http://localhost:8080/api/homolog/rulesets" \
  -H "Authorization: Basic YWRtaW46cnVsZXg="
```

### 10.2 Listar Regras de um Ruleset
```bash
curl -X GET "http://localhost:8080/api/homolog/rules?rulesetId=1" \
  -H "Authorization: Basic YWRtaW46cnVsZXg="
```

---

## Autenticação

Todos os endpoints requerem autenticação Basic Auth:
- **Username:** admin
- **Password:** rulex
- **Header:** `Authorization: Basic YWRtaW46cnVsZXg=`

---

## Códigos de Resposta

| Código | Significado |
|--------|-------------|
| 200 | Sucesso |
| 201 | Criado |
| 400 | Requisição inválida |
| 401 | Não autorizado |
| 403 | Proibido |
| 404 | Não encontrado |
| 409 | Conflito (ex: chave duplicada) |
| 500 | Erro interno |
