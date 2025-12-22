# PLANO DE TESTES - HOMOLOGACAO RULEX v2.0

**Data**: 2025-12-19  
**Versao**: 2.0  
**Status**: DOCUMENTO PARA HOMOLOGACAO  
**Painel**: 10 Especialistas QA

---

## PARTE 1: VISAO GERAL E ESTRATEGIA

### 1.1 Escopo

| Item | Descricao |
|------|-----------|
| **Aplicacao** | RULEX - Motor de Regras Duras para Deteccao de Fraude |
| **Backend** | Java 21 + Spring Boot 3.x |
| **Frontend** | React 19 + Vite + TypeScript |
| **Banco** | PostgreSQL + Flyway migrations |
| **OpenAPI** | `openapi/rulex.yaml` |
| **Regras Duras** | 28 regras implementadas em AdvancedRuleEngineService |

### 1.2 Fora de Escopo

- Testes de Machine Learning (nao existe no sistema)
- Testes de integracao com sistemas externos reais (apenas mocks)
- Testes de producao (apenas homologacao)
- Alteracao do payload de entrada (RESTRICAO CRITICA)

### 1.3 Riscos Principais

| Risco | Probabilidade | Impacto | Mitigacao |
|-------|---------------|---------|-----------|
| Falha de idempotencia | Alta | Critico | Testar replay de payloads |
| Vazamento de PAN | Media | Critico | Validar mascaramento |
| Conflito de regras | Media | Alto | Testar precedencia |
| Concorrencia | Alta | Alto | Testes de carga |
| SQL Injection | Baixa | Critico | Testes OWASP |

### 1.4 Premissas

1. Payload de entrada NAO PODE ser alterado
2. Backend Java exposto na porta 8080
3. Frontend estatico servido na porta 5000
4. PostgreSQL disponivel via Testcontainers ou Docker Compose
5. Sem autenticacao real no frontend (mock auth)

### 1.5 Ambientes

| Ambiente | Descricao | URL |
|----------|-----------|-----|
| DEV | Desenvolvimento local | localhost:8080 (API), localhost:5000 (UI) |
| TEST | Testes automatizados | Testcontainers |
| HOMOLOG | Homologacao | A definir |

---

## PARTE 2: MATRIZ DE TESTES

| Camada | Tipo | Objetivo | Ferramenta | Prioridade | Qtd Min |
|--------|------|----------|------------|------------|---------|
| API | Contrato | Validar OpenAPI | RestAssured/JUnit | P0 | 40 |
| API | Funcional | Status codes, erros | RestAssured | P0 | 20 |
| Motor | Regras | 28 regras duras | JUnit | P0 | 35 |
| Motor | Concorrencia | Thread-safety | JMeter/JUnit | P0 | 10 |
| DB | Migrations | Flyway idempotente | Testcontainers | P0 | 20 |
| DB | Integridade | FK, constraints | SQL/JUnit | P0 | 10 |
| Seguranca | OWASP | SQLi, XSS, etc | OWASP ZAP | P0 | 25 |
| Seguranca | RBAC | ADMIN vs ANALYST | RestAssured | P0 | 10 |
| Frontend | E2E | Fluxos criticos | Playwright | P0 | 25 |
| Frontend | Componentes | React Testing Library | Vitest | P1 | 15 |
| Performance | Carga | Latencia, throughput | JMeter/k6 | P0 | 12 |
| Resiliencia | Falhas | DB down, timeout | Testcontainers | P0 | 12 |
| Observabilidade | Logs | Correlation ID, audit | JUnit | P0 | 15 |

---

## PARTE 3: SUITE P0 - OBRIGATORIA (REPROVA SE FALTAR)

### 3.1 TESTES DE API/CONTRATO (40 testes)

#### TC-API-001: POST /api/transactions/analyze - Sucesso
```
ID: TC-API-001
Objetivo: Validar analise de transacao com payload valido
Risco: Alto
Prioridade: P0
Pre-condicao: Regra RISK_MCC_7995 cadastrada
Passos:
  1. Enviar POST para /api/transactions/analyze
  2. Payload com mcc=7995
Dados:
  {
    "externalTransactionId": "tx-test-001",
    "customerIdFromHeader": "cust-1",
    "customerAcctNumber": 1234567890,
    "pan": "4111111111111111",
    "transactionAmount": 100.00,
    "transactionDate": 20251219,
    "transactionTime": 120000,
    "transactionCurrencyCode": 986,
    "mcc": 7995,
    "consumerAuthenticationScore": 200,
    "externalScore3": 200,
    "cavvResult": 0,
    "eciIndicator": 5,
    "atcCard": 1,
    "atcHost": 1,
    "tokenAssuranceLevel": 80,
    "availableCredit": 1000.00,
    "cardCashBalance": 0.00,
    "cardDelinquentAmount": 0.00
  }
Resultado Esperado:
  - Status: 200 OK
  - Body.classification: "FRAUD"
  - Body.triggeredRules[0].name: "RISK_MCC_7995"
  - Body.riskScore >= 60
  - Body.success: true
Evidencia: JSON response + logs
Automacao: Sim
Dono: API
```

#### TC-API-002: POST /api/transactions/analyze - Idempotencia
```
ID: TC-API-002
Objetivo: Validar que mesmo payload retorna resultado cacheado
Risco: Critico
Prioridade: P0
Pre-condicao: Transacao tx-test-001 ja processada
Passos:
  1. Enviar POST com mesmo externalTransactionId
  2. Verificar que nao duplica no banco
Dados: Mesmo payload de TC-API-001
Resultado Esperado:
  - Status: 200 OK
  - Mesmo transactionId retornado
  - Tabela transactions NAO tem duplicata
Evidencia: Response + COUNT(*) no banco
Automacao: Sim
Dono: API
```

#### TC-API-003: POST /api/transactions/analyze - Campos obrigatorios faltando
```
ID: TC-API-003
Objetivo: Validar erro quando campos required faltam
Risco: Alto
Prioridade: P0
Pre-condicao: Nenhuma
Passos:
  1. Enviar POST sem externalTransactionId
Dados:
  {
    "customerIdFromHeader": "cust-1",
    "pan": "4111111111111111"
    // faltando externalTransactionId e outros
  }
Resultado Esperado:
  - Status: 400 Bad Request
  - Body contem mensagem de erro
  - NAO contem stacktrace
Evidencia: JSON response
Automacao: Sim
Dono: API
```

#### TC-API-004: GET /api/transactions - Listagem com paginacao
```
ID: TC-API-004
Objetivo: Validar listagem paginada
Risco: Medio
Prioridade: P0
Pre-condicao: 25 transacoes no banco
Passos:
  1. GET /api/transactions?page=0&size=10
  2. GET /api/transactions?page=1&size=10
  3. GET /api/transactions?page=2&size=10
Resultado Esperado:
  - Pagina 0: 10 itens
  - Pagina 1: 10 itens
  - Pagina 2: 5 itens
  - totalElements: 25
  - totalPages: 3
Evidencia: JSON response
Automacao: Sim
Dono: API
```

#### TC-API-005: GET /api/transactions/{id} - Transacao inexistente
```
ID: TC-API-005
Objetivo: Validar 404 para ID inexistente
Risco: Baixo
Prioridade: P0
Pre-condicao: ID 999999 nao existe
Passos:
  1. GET /api/transactions/999999
Resultado Esperado:
  - Status: 404 Not Found
  - Body padronizado
Evidencia: JSON response
Automacao: Sim
Dono: API
```

#### TC-API-006: POST /api/rules - Criar regra
```
ID: TC-API-006
Objetivo: Validar criacao de regra
Risco: Alto
Prioridade: P0
Pre-condicao: Nenhuma
Passos:
  1. POST /api/rules com payload valido
Dados:
  {
    "ruleName": "TEST_RULE_001",
    "description": "Regra de teste",
    "ruleType": "SECURITY",
    "weight": 50,
    "enabled": true,
    "classification": "SUSPICIOUS",
    "conditions": [{"field": "mcc", "operator": "==", "value": "5411"}],
    "logicOperator": "AND"
  }
Resultado Esperado:
  - Status: 201 Created
  - Body.id existe
  - Body.ruleName: "TEST_RULE_001"
Evidencia: JSON response
Automacao: Sim
Dono: API
```

#### TC-API-007: POST /api/rules - Nome duplicado
```
ID: TC-API-007
Objetivo: Validar erro para regra duplicada
Risco: Medio
Prioridade: P0
Pre-condicao: Regra TEST_RULE_001 ja existe
Passos:
  1. POST /api/rules com mesmo ruleName
Resultado Esperado:
  - Status: 409 Conflict
Evidencia: JSON response
Automacao: Sim
Dono: API
```

#### TC-API-008: PUT /api/rules/{id} - Atualizar regra
```
ID: TC-API-008
Objetivo: Validar atualizacao de regra
Risco: Alto
Prioridade: P0
Pre-condicao: Regra existe
Passos:
  1. PUT /api/rules/{id} com weight alterado
Resultado Esperado:
  - Status: 200 OK
  - Body.weight: novo valor
  - Body.version incrementado
Evidencia: JSON response + audit log
Automacao: Sim
Dono: API
```

#### TC-API-009: DELETE /api/rules/{id}
```
ID: TC-API-009
Objetivo: Validar exclusao de regra
Risco: Alto
Prioridade: P0
Pre-condicao: Regra existe
Passos:
  1. DELETE /api/rules/{id}
  2. GET /api/rules/{id}
Resultado Esperado:
  - DELETE: 204 No Content
  - GET: 404 Not Found
Evidencia: Responses
Automacao: Sim
Dono: API
```

#### TC-API-010: PATCH /api/rules/{id}/toggle
```
ID: TC-API-010
Objetivo: Validar toggle de enabled
Risco: Medio
Prioridade: P0
Pre-condicao: Regra com enabled=true
Passos:
  1. PATCH /api/rules/{id}/toggle
  2. Verificar enabled=false
  3. PATCH novamente
  4. Verificar enabled=true
Resultado Esperado:
  - Toggle funciona corretamente
Evidencia: JSON responses
Automacao: Sim
Dono: API
```

#### TC-API-011 a TC-API-020: Status Codes
```
ID: TC-API-011 - TC-API-020
Objetivo: Validar todos os status codes da API
Casos:
  - 200: GET sucesso
  - 201: POST criacao
  - 204: DELETE sucesso
  - 400: Payload invalido
  - 401: Sem autenticacao (quando implementado)
  - 403: Sem permissao (quando implementado)
  - 404: Recurso nao encontrado
  - 409: Conflito (duplicata)
  - 422: Validacao de negocio falhou
  - 500: Erro interno (sem stacktrace)
Automacao: Sim
Dono: API
```

#### TC-API-021 a TC-API-030: Validacao de Schema OpenAPI
```
ID: TC-API-021 - TC-API-030
Objetivo: Validar conformidade com OpenAPI spec
Casos:
  - Tipos de campos corretos
  - Campos obrigatorios presentes
  - Formatos (date-time, int64)
  - Enums validos (APPROVED, SUSPICIOUS, FRAUD)
  - Arrays com items corretos
  - Nested objects corretos
Ferramenta: OpenAPI4J / Spectral
Automacao: Sim
Dono: API
```

#### TC-API-031: OPTIONS /api/* - CORS Preflight
```
ID: TC-API-031
Objetivo: Validar CORS preflight
Risco: Alto
Prioridade: P0
Passos:
  1. OPTIONS /api/transactions/analyze
  2. Verificar headers CORS
Resultado Esperado:
  - Access-Control-Allow-Origin presente
  - Access-Control-Allow-Methods presente
  - Access-Control-Allow-Headers presente
Evidencia: Headers
Automacao: Sim
Dono: API
```

#### TC-API-032 a TC-API-040: Filtros e Ordenacao
```
ID: TC-API-032 - TC-API-040
Objetivo: Validar filtros da listagem
Casos:
  - GET /api/transactions?customerId=X
  - GET /api/transactions?merchantId=Y
  - GET /api/transactions?mcc=7995
  - GET /api/transactions?minAmount=100&maxAmount=500
  - GET /api/transactions?startDate=2025-01-01T00:00:00Z
  - GET /api/transactions?page=0&size=5
  - GET /api/audit?actionType=TRANSACTION_PROCESSED
  - GET /api/metrics?period=24h
  - GET /api/metrics/timeline?granularity=hour
Automacao: Sim
Dono: API
```

---

### 3.2 TESTES DE MOTOR DE REGRAS (35 testes)

#### TC-RULE-001: EMV_SECURITY_CHECK - Positivo
```
ID: TC-RULE-001
Objetivo: EMV invalido + valor > 1000 = SUSPICIOUS
Risco: Alto
Prioridade: P0
Dados:
  - cardAipStatic: "N"
  - cardAipDynamic: "N"
  - cardAipVerify: "N"
  - transactionAmount: 1500.00
Resultado Esperado:
  - classification: SUSPICIOUS
  - triggeredRules contem EMV_SECURITY_CHECK
Automacao: Sim
Dono: Motor
```

#### TC-RULE-002: EMV_SECURITY_CHECK - Negativo
```
ID: TC-RULE-002
Objetivo: EMV valido nao dispara regra
Dados:
  - cardAipStatic: "Y"
  - cardAipDynamic: "Y"
  - cardAipVerify: "Y"
  - transactionAmount: 1500.00
Resultado Esperado:
  - EMV_SECURITY_CHECK NAO dispara
Automacao: Sim
Dono: Motor
```

#### TC-RULE-003: TERMINAL_VERIFICATION_FAILED
```
ID: TC-RULE-003
Objetivo: Falha de verificacao = FRAUD
Dados:
  - terminalVerificationResults: "FAIL"
Resultado Esperado:
  - classification: FRAUD
  - triggeredRules contem TERMINAL_VERIFICATION_FAILED
Automacao: Sim
Dono: Motor
```

#### TC-RULE-004: EXPIRED_CARD
```
ID: TC-RULE-004
Objetivo: Cartao expirado = FRAUD
Dados:
  - cardExpireDate: 20240101 (passado)
  - transactionDate: 20251219 (presente)
Resultado Esperado:
  - classification: FRAUD
  - triggeredRules contem EXPIRED_CARD
Automacao: Sim
Dono: Motor
```

#### TC-RULE-005: SUSPICIOUS_TRANSACTION_TYPE
```
ID: TC-RULE-005
Objetivo: Reversal com valor alto = SUSPICIOUS
Dados:
  - transactionType: "R"
  - transactionAmount: 5000.00
Resultado Esperado:
  - classification: SUSPICIOUS
Automacao: Sim
Dono: Motor
```

#### TC-RULE-006 a TC-RULE-028: Todas as 28 Regras
```
Para cada regra em AdvancedRuleEngineService:
  - EMV_SECURITY_CHECK
  - TERMINAL_VERIFICATION_FAILED
  - EXPIRED_CARD
  - SUSPICIOUS_TRANSACTION_TYPE
  - UNUSUAL_CARD_MEDIA
  - SUSPICIOUS_TERMINAL
  - ECOMMERCE_NO_AVS
  - POS_SECURITY_MISSING
  - CARD_CAPTURE_FRAUD
  - PIN_CVV_LIMIT_EXCEEDED
  - OFFLINE_PIN_FAILED
  - MISSING_CVV2_HIGH_RISK
  - CUSTOM_INDICATOR_FRAUD
  - PROCESSING_LAG_ANOMALY
  - TIMEZONE_NORMALIZED_CHECK
  - DUPLICATE_TRANSACTION
  - SUSPICIOUS_MERCHANT_POSTAL
  - SUSPICIOUS_TOKEN
  - UNEXPECTED_CURRENCY
  - ANOMALOUS_CONVERSION_RATE
  - INCOHERENT_AUTH_SEQUENCE
  - INCOHERENT_CONTEXT
  - CONTRADICTORY_AUTHORIZATION
  - SUSPICIOUS_ACQUIRER
  - ACQUIRER_COUNTRY_MISMATCH
  - COMBINED_SCORE_CHECK
  - VELOCITY_CHECK_CONSOLIDATED
  - CUSTOM_INDICATORS_COMPREHENSIVE

Cada regra deve ter:
  - Teste positivo (dispara)
  - Teste negativo (nao dispara)
  - Teste de borda (limites)
Automacao: Sim
Dono: Motor
```

#### TC-RULE-029: Conflito de Regras - Precedencia
```
ID: TC-RULE-029
Objetivo: Verificar qual regra vence quando multiplas disparam
Dados: Payload que dispara EMV_SECURITY_CHECK (SUSPICIOUS) e EXPIRED_CARD (FRAUD)
Resultado Esperado:
  - classification: FRAUD (mais severo vence)
  - triggeredRules contem ambas
Automacao: Sim
Dono: Motor
```

#### TC-RULE-030: Determinismo
```
ID: TC-RULE-030
Objetivo: Mesma entrada = mesmo resultado sempre
Passos:
  1. Executar 100x com mesmo payload
  2. Verificar todos resultados identicos
Resultado Esperado:
  - 100% dos resultados iguais
Automacao: Sim
Dono: Motor
```

#### TC-RULE-031: Campos Null/Ausentes
```
ID: TC-RULE-031
Objetivo: Regras tratam campos null gracefully
Dados: Payload com cardAipStatic=null
Resultado Esperado:
  - Sem NullPointerException
  - Regra considera como invalido
Automacao: Sim
Dono: Motor
```

#### TC-RULE-032: Concorrencia 50 execucoes simultaneas
```
ID: TC-RULE-032
Objetivo: Motor e thread-safe
Passos:
  1. Disparar 50 threads simultaneas
  2. Cada thread processa payload unico
Resultado Esperado:
  - Todas completam sem erro
  - Resultados consistentes
  - Sem race condition
Automacao: Sim (JUnit + ExecutorService)
Dono: Motor
```

#### TC-RULE-033 a TC-RULE-035: Edge Cases
```
TC-RULE-033: Valor zero (transactionAmount=0)
TC-RULE-034: Valor muito alto (transactionAmount=999999999.99)
TC-RULE-035: Data futura (transactionDate > hoje)
```

---

### 3.3 TESTES DE BANCO DE DADOS (20 testes)

#### TC-DB-001: Fresh Install
```
ID: TC-DB-001
Objetivo: DB vazio -> Flyway aplica todas migrations
Passos:
  1. Subir container PostgreSQL vazio
  2. Iniciar aplicacao
  3. Verificar tabelas criadas
Resultado Esperado:
  - transactions existe
  - transaction_decisions existe
  - rule_configurations existe
  - rule_configuration_history existe
  - audit_logs existe
  - flyway_schema_history existe
Automacao: Sim (Testcontainers)
Dono: DB
```

#### TC-DB-002: Re-execucao Idempotente
```
ID: TC-DB-002
Objetivo: Migrations nao quebram em re-execucao
Passos:
  1. Executar mvn flyway:migrate 2x
Resultado Esperado:
  - Sem erro
  - Schema inalterado
Automacao: Sim
Dono: DB
```

#### TC-DB-003: Constraint NOT NULL
```
ID: TC-DB-003
Objetivo: Campos obrigatorios enforced
Passos:
  1. INSERT em transactions sem external_transaction_id
Resultado Esperado:
  - Erro de constraint
Automacao: Sim
Dono: DB
```

#### TC-DB-004: Constraint UNIQUE
```
ID: TC-DB-004
Objetivo: external_transaction_id e unico
Passos:
  1. INSERT com external_transaction_id=X
  2. INSERT com external_transaction_id=X novamente
Resultado Esperado:
  - Segundo INSERT falha
Automacao: Sim
Dono: DB
```

#### TC-DB-005: Constraint FK
```
ID: TC-DB-005
Objetivo: transaction_decisions.transaction_id referencia transactions
Passos:
  1. INSERT em transaction_decisions com transaction_id inexistente
Resultado Esperado:
  - Erro de FK
Automacao: Sim
Dono: DB
```

#### TC-DB-006: Constraint CHECK classification
```
ID: TC-DB-006
Objetivo: classification aceita apenas APPROVED/SUSPICIOUS/FRAUD
Passos:
  1. INSERT com classification='INVALID'
Resultado Esperado:
  - Erro de CHECK constraint
Automacao: Sim
Dono: DB
```

#### TC-DB-007: Indices existem
```
ID: TC-DB-007
Objetivo: Validar indices criados
SQL:
  SELECT indexname FROM pg_indexes 
  WHERE tablename = 'transactions';
Resultado Esperado:
  - idx_customer_id
  - idx_merchant_id
  - idx_transaction_date
Automacao: Sim
Dono: DB
```

#### TC-DB-008: Transacao Rollback
```
ID: TC-DB-008
Objetivo: Erro no meio da operacao faz rollback
Passos:
  1. Inserir transaction
  2. Simular erro antes de inserir decision
  3. Verificar que transaction tambem foi rollback
Resultado Esperado:
  - Nenhum registro orfao
Automacao: Sim
Dono: DB
```

#### TC-DB-009: Audit Log Append-Only
```
ID: TC-DB-009
Objetivo: Audit logs nao podem ser deletados/alterados
Passos:
  1. Inserir audit log
  2. Tentar UPDATE (se aplicavel)
  3. Tentar DELETE (se aplicavel)
Resultado Esperado:
  - Operacoes bloqueadas ou auditadas
Automacao: Sim
Dono: DB
```

#### TC-DB-010 a TC-DB-020: Integridade e Concorrencia
```
TC-DB-010: rule_configuration_history armazena versoes
TC-DB-011: Timezone consistente (UTC)
TC-DB-012: NUMERIC precision para valores monetarios
TC-DB-013: VARCHAR limits respeitados
TC-DB-014: Deadlock handling (10 threads)
TC-DB-015: Connection pool exhaustion
TC-DB-016: Long running query timeout
TC-DB-017: Explain analyze queries criticas
TC-DB-018: Blob/TEXT fields funcionam
TC-DB-019: Cascade delete (se aplicavel)
TC-DB-020: Data retention policy (se existe)
```

---

### 3.4 TESTES DE SEGURANCA (25 testes)

#### TC-SEC-001: SQL Injection - Filtros
```
ID: TC-SEC-001
Objetivo: Prevenir SQLi em parametros de filtro
Dados:
  - GET /api/transactions?customerId='; DROP TABLE transactions;--
Resultado Esperado:
  - Status 400 ou lista vazia
  - Tabela NAO foi dropada
Automacao: Sim (OWASP ZAP)
Dono: Security
```

#### TC-SEC-002: SQL Injection - Path Param
```
ID: TC-SEC-002
Objetivo: Prevenir SQLi em path params
Dados:
  - GET /api/transactions/1 OR 1=1
Resultado Esperado:
  - Status 400
Automacao: Sim
Dono: Security
```

#### TC-SEC-003: XSS Refletido
```
ID: TC-SEC-003
Objetivo: Prevenir XSS em campos de input
Dados:
  - merchantName: "<script>alert('xss')</script>"
Resultado Esperado:
  - Valor escapado ou sanitizado no response
  - Script NAO executa no frontend
Automacao: Sim
Dono: Security
```

#### TC-SEC-004: XSS Armazenado
```
ID: TC-SEC-004
Objetivo: Prevenir XSS em dados persistidos
Dados:
  - Criar regra com description: "<img onerror='alert(1)' src='x'>"
  - Listar regras
Resultado Esperado:
  - HTML escapado
Automacao: Sim
Dono: Security
```

#### TC-SEC-005: PAN Masking em Response
```
ID: TC-SEC-005
Objetivo: PAN nunca aparece em claro na response
Passos:
  1. Analisar transacao com PAN completo
  2. Verificar response
Resultado Esperado:
  - PAN mascarado (ex: 411111******1111)
Automacao: Sim
Dono: Security
```

#### TC-SEC-006: PAN Masking em Logs
```
ID: TC-SEC-006
Objetivo: PAN nunca aparece nos logs
Passos:
  1. Analisar transacao
  2. Grep nos logs por PAN completo
Resultado Esperado:
  - PAN NAO encontrado
Automacao: Sim
Dono: Security
```

#### TC-SEC-007: PAN Masking no Banco
```
ID: TC-SEC-007
Objetivo: PAN armazenado mascarado
Passos:
  1. SELECT pan FROM transactions
Resultado Esperado:
  - Valor mascarado
Automacao: Sim
Dono: Security
```

#### TC-SEC-008: Erro 500 sem Stacktrace
```
ID: TC-SEC-008
Objetivo: Erros internos nao vazam detalhes
Passos:
  1. Forcar erro interno
  2. Verificar response
Resultado Esperado:
  - Mensagem generica
  - Sem stacktrace
  - Sem nomes de classes/metodos
Automacao: Sim
Dono: Security
```

#### TC-SEC-009: CORS Policy
```
ID: TC-SEC-009
Objetivo: Apenas origens permitidas aceitas
Passos:
  1. Request com Origin: https://malicious.com
  2. Verificar se bloqueado
Resultado Esperado:
  - Sem Access-Control-Allow-Origin para origem maliciosa
Automacao: Sim
Dono: Security
```

#### TC-SEC-010 a TC-SEC-015: RBAC (quando implementado)
```
TC-SEC-010: ADMIN pode criar regras
TC-SEC-011: ANALYST NAO pode criar regras (403)
TC-SEC-012: ANALYST pode listar transacoes
TC-SEC-013: Sem token = 401
TC-SEC-014: Token expirado = 401
TC-SEC-015: Bypass attempt via query param
```

#### TC-SEC-016 a TC-SEC-020: Headers de Seguranca
```
TC-SEC-016: X-Content-Type-Options: nosniff
TC-SEC-017: X-Frame-Options: DENY
TC-SEC-018: Content-Security-Policy presente
TC-SEC-019: Strict-Transport-Security (HSTS)
TC-SEC-020: Cache-Control para dados sensiveis
```

#### TC-SEC-021 a TC-SEC-025: Outros
```
TC-SEC-021: Rate limiting (GAP se nao existe)
TC-SEC-022: Bruteforce protection
TC-SEC-023: Session fixation
TC-SEC-024: Dependencias vulneraveis (npm audit, mvn dependency-check)
TC-SEC-025: Secrets nao hardcoded no codigo
```

---

### 3.5 TESTES DE FRONTEND/UI (25 testes)

#### TC-UI-001: Dashboard carrega
```
ID: TC-UI-001
Objetivo: Dashboard exibe metricas
Passos:
  1. Acessar /
  2. Verificar componentes
Resultado Esperado:
  - Metricas visiveis
  - Graficos carregam
  - Sem erros no console
Automacao: Sim (Playwright)
Dono: Frontend
```

#### TC-UI-002: Lista Transacoes
```
ID: TC-UI-002
Objetivo: Listar transacoes com paginacao
Passos:
  1. Acessar /transactions
  2. Navegar entre paginas
Resultado Esperado:
  - Tabela exibe dados
  - Paginacao funciona
Automacao: Sim
Dono: Frontend
```

#### TC-UI-003: Detalhes da Transacao
```
ID: TC-UI-003
Objetivo: Ver detalhes de transacao
Passos:
  1. Clicar em transacao na lista
  2. Verificar detalhes
Resultado Esperado:
  - Dados exibidos corretamente
  - PAN mascarado
Automacao: Sim
Dono: Frontend
```

#### TC-UI-004: CRUD de Regras
```
ID: TC-UI-004
Objetivo: Criar, editar, deletar regra
Passos:
  1. Criar regra via formulario
  2. Editar regra
  3. Deletar regra
Resultado Esperado:
  - Todas operacoes funcionam
  - Feedback visual correto
Automacao: Sim
Dono: Frontend
```

#### TC-UI-005: Simulador de Transacoes
```
ID: TC-UI-005
Objetivo: Simular transacao no frontend
Passos:
  1. Acessar simulador
  2. Preencher formulario
  3. Submeter
Resultado Esperado:
  - Resultado exibido
  - Regras disparadas mostradas
Automacao: Sim
Dono: Frontend
```

#### TC-UI-006: Estado de Loading
```
ID: TC-UI-006
Objetivo: Loading indicators funcionam
Passos:
  1. Disparar operacao assincrona
  2. Verificar loading
Resultado Esperado:
  - Spinner ou skeleton exibido
Automacao: Sim
Dono: Frontend
```

#### TC-UI-007: Estado de Erro
```
ID: TC-UI-007
Objetivo: Erros exibidos corretamente
Passos:
  1. Forcar erro de API
  2. Verificar UI
Resultado Esperado:
  - Mensagem de erro amigavel
  - Opcao de retry
Automacao: Sim
Dono: Frontend
```

#### TC-UI-008: Estado Vazio
```
ID: TC-UI-008
Objetivo: UI trata listas vazias
Passos:
  1. Acessar transacoes sem dados
Resultado Esperado:
  - Mensagem "Nenhuma transacao"
  - Sem quebra visual
Automacao: Sim
Dono: Frontend
```

#### TC-UI-009: Tema Claro/Escuro
```
ID: TC-UI-009
Objetivo: Toggle de tema funciona
Passos:
  1. Alternar tema
Resultado Esperado:
  - Cores mudam
  - Preferencia persistida
Automacao: Sim
Dono: Frontend
```

#### TC-UI-010: Responsividade
```
ID: TC-UI-010
Objetivo: UI funciona em diferentes tamanhos
Passos:
  1. Testar em 1920x1080
  2. Testar em 1024x768
  3. Testar em 768x1024 (tablet)
Resultado Esperado:
  - Layout adapta
  - Sem overflow
Automacao: Sim
Dono: Frontend
```

#### TC-UI-011 a TC-UI-020: Fluxos Criticos
```
TC-UI-011: Filtrar transacoes por data
TC-UI-012: Filtrar transacoes por status
TC-UI-013: Exportar dados (se existir)
TC-UI-014: Pagina de auditoria
TC-UI-015: Toggle de regra
TC-UI-016: Validacao de formularios
TC-UI-017: Navegacao por teclado
TC-UI-018: Mensagens de confirmacao
TC-UI-019: Refresh de pagina mantem estado
TC-UI-020: Browser back button
```

#### TC-UI-021 a TC-UI-025: Cross-Browser e Edge Cases
```
TC-UI-021: Chrome
TC-UI-022: Firefox
TC-UI-023: Edge
TC-UI-024: Duplo clique em submit
TC-UI-025: Network slow (timeout handling)
```

---

### 3.6 TESTES DE OBSERVABILIDADE (15 testes)

#### TC-OBS-001: Audit Log Criado
```
ID: TC-OBS-001
Objetivo: Cada transacao gera audit log
Passos:
  1. Analisar transacao
  2. Consultar audit_logs
Resultado Esperado:
  - Registro com TRANSACTION_PROCESSED
  - Campos preenchidos
Automacao: Sim
Dono: Observability
```

#### TC-OBS-002: Correlation ID
```
ID: TC-OBS-002
Objetivo: Requests tem correlation ID
Passos:
  1. Enviar request
  2. Verificar logs e response headers
Resultado Esperado:
  - X-Correlation-ID ou similar
  - Mesmo ID nos logs
Automacao: Sim
Dono: Observability
```

#### TC-OBS-003: Log Level Correto
```
ID: TC-OBS-003
Objetivo: Sem DEBUG em homolog/prod
Passos:
  1. Verificar logs em ambiente homolog
Resultado Esperado:
  - Apenas INFO, WARN, ERROR
  - Sem DEBUG
Automacao: Sim
Dono: Observability
```

#### TC-OBS-004 a TC-OBS-010: Logs e Audit
```
TC-OBS-004: Log de criacao de regra
TC-OBS-005: Log de atualizacao de regra
TC-OBS-006: Log de delecao de regra
TC-OBS-007: Log de erro com stacktrace (interno)
TC-OBS-008: Audit consultavel por periodo
TC-OBS-009: Audit consultavel por tipo
TC-OBS-010: Timestamps em UTC
```

#### TC-OBS-011 a TC-OBS-015: Health e Metricas
```
TC-OBS-011: Health endpoint existe (GAP se nao)
TC-OBS-012: Readiness endpoint existe
TC-OBS-013: Metricas de latencia expostas
TC-OBS-014: Metricas de erro expostas
TC-OBS-015: Metricas de throughput
```

---

### 3.7 TESTES DE PERFORMANCE (12 testes)

#### TC-PERF-001: Latencia P50/P95/P99
```
ID: TC-PERF-001
Objetivo: Medir latencia em carga normal
Ferramenta: k6/JMeter
Carga: 100 RPS por 5 min
Resultado Esperado:
  - P50 < 100ms
  - P95 < 300ms
  - P99 < 500ms
Automacao: Sim
Dono: Performance
```

#### TC-PERF-002: Throughput
```
ID: TC-PERF-002
Objetivo: Medir capacidade maxima
Ferramenta: k6
Carga: Rampa ate erro
Resultado Esperado:
  - Documentar RPS maximo
  - Sem erro ate 500 RPS
Automacao: Sim
Dono: Performance
```

#### TC-PERF-003: Spike Test
```
ID: TC-PERF-003
Objetivo: Sistema aguenta picos
Carga: 0 -> 1000 RPS instantaneo
Resultado Esperado:
  - Sistema nao cai
  - Recovery < 30s
Automacao: Sim
Dono: Performance
```

#### TC-PERF-004: Soak Test
```
ID: TC-PERF-004
Objetivo: Sem memory leak
Carga: 50 RPS por 60 min
Resultado Esperado:
  - Memoria estavel
  - Sem degradacao
Automacao: Sim
Dono: Performance
```

#### TC-PERF-005 a TC-PERF-012: Cenarios Especificos
```
TC-PERF-005: Payload grande (campos max length)
TC-PERF-006: Muitas regras ativas (100+)
TC-PERF-007: Paginacao com muitos dados
TC-PERF-008: Filtros complexos
TC-PERF-009: Connection pool behavior
TC-PERF-010: GC pauses
TC-PERF-011: CPU under load
TC-PERF-012: DB connections under load
```

---

### 3.8 TESTES DE RESILIENCIA (12 testes)

#### TC-RES-001: DB Down
```
ID: TC-RES-001
Objetivo: Sistema trata DB indisponivel
Passos:
  1. Parar PostgreSQL
  2. Enviar request
Resultado Esperado:
  - Status 503 ou 500
  - Mensagem de erro clara
  - Sem crash do app
Automacao: Sim (Testcontainers)
Dono: Resiliencia
```

#### TC-RES-002: DB Timeout
```
ID: TC-RES-002
Objetivo: Query longa nao trava sistema
Passos:
  1. Simular query lenta
  2. Verificar timeout
Resultado Esperado:
  - Timeout apos X segundos
  - Erro tratado
Automacao: Sim
Dono: Resiliencia
```

#### TC-RES-003: Erro 5xx Recovery
```
ID: TC-RES-003
Objetivo: Sistema se recupera apos erro
Passos:
  1. Causar erros 5xx
  2. Corrigir causa
  3. Verificar recovery
Resultado Esperado:
  - Sistema volta ao normal
Automacao: Sim
Dono: Resiliencia
```

#### TC-RES-004: Requisicoes Duplicadas
```
ID: TC-RES-004
Objetivo: Replay nao causa side effects
Passos:
  1. Enviar request
  2. Reenviar identico
Resultado Esperado:
  - Resultado idempotente
  - Sem duplicata no banco
Automacao: Sim
Dono: Resiliencia
```

#### TC-RES-005 a TC-RES-012: Outros Cenarios
```
TC-RES-005: Network timeout
TC-RES-006: Conexao cortada no meio
TC-RES-007: Retry manual sem duplicar
TC-RES-008: Container restart
TC-RES-009: Memory pressure
TC-RES-010: Disk full (logs)
TC-RES-011: Pool exhaustion
TC-RES-012: Graceful shutdown
```

---

## PARTE 4: SUITE P1 (ROBUSTEZ)

| ID | Teste | Area | Prioridade |
|----|-------|------|------------|
| TC-P1-001 | Lazy loading de componentes | Frontend | P1 |
| TC-P1-002 | Cache de regras | Backend | P1 |
| TC-P1-003 | Metricas Prometheus | Observability | P1 |
| TC-P1-004 | Alertas configurados | DevOps | P1 |
| TC-P1-005 | Backup/restore testado | DBA | P1 |
| TC-P1-006 | Rollback de migration | DBA | P1 |
| TC-P1-007 | Acessibilidade WCAG 2.1 | Frontend | P1 |
| TC-P1-008 | Performance profiling | Backend | P1 |
| TC-P1-009 | Security headers completos | Security | P1 |
| TC-P1-010 | Rate limiting | Security | P1 |

---

## PARTE 5: SUITE P2 (NICE-TO-HAVE)

| ID | Teste | Area | Prioridade |
|----|-------|------|------------|
| TC-P2-001 | Storybook components | Frontend | P2 |
| TC-P2-002 | Visual regression | Frontend | P2 |
| TC-P2-003 | i18n/l10n | Frontend | P2 |
| TC-P2-004 | Offline mode | Frontend | P2 |
| TC-P2-005 | PWA features | Frontend | P2 |
| TC-P2-006 | GraphQL endpoint | Backend | P2 |
| TC-P2-007 | WebSocket updates | Backend | P2 |
| TC-P2-008 | Chaos engineering | Resiliencia | P2 |

---

## PARTE 6: TESTES QUE NORMALMENTE AS PESSOAS ESQUECEM (30+)

### 6.1 API/Network
| ID | Teste | Esquecido Por |
|----|-------|---------------|
| TC-ESQ-001 | OPTIONS preflight CORS | Frontend dev |
| TC-ESQ-002 | Content-Type header obrigatorio | API dev |
| TC-ESQ-003 | Accept header validation | API dev |
| TC-ESQ-004 | Query params com caracteres especiais | QA |
| TC-ESQ-005 | Path traversal (../../) | Security |
| TC-ESQ-006 | Request body muito grande | API dev |
| TC-ESQ-007 | Campos extras no JSON ignorados | API dev |
| TC-ESQ-008 | Campos fora de ordem no JSON | API dev |
| TC-ESQ-009 | UTF-8 encoding | Everyone |
| TC-ESQ-010 | Numeros com virgula vs ponto | Localization |

### 6.2 Datas/Numeros
| ID | Teste | Esquecido Por |
|----|-------|---------------|
| TC-ESQ-011 | Timezone DST (horario verao) | Backend dev |
| TC-ESQ-012 | Data na virada meia-noite | Backend dev |
| TC-ESQ-013 | Ano bissexto (29/02) | Backend dev |
| TC-ESQ-014 | Moeda com 3 casas decimais | DBA |
| TC-ESQ-015 | Valor zero vs null | Backend dev |
| TC-ESQ-016 | Arredondamento HALF_UP | Backend dev |
| TC-ESQ-017 | Clock skew servidor | DevOps |

### 6.3 Strings/Encoding
| ID | Teste | Esquecido Por |
|----|-------|---------------|
| TC-ESQ-018 | Unicode/emoji em nomes | Frontend dev |
| TC-ESQ-019 | String vazia vs null | Backend dev |
| TC-ESQ-020 | Espacos no inicio/fim | QA |
| TC-ESQ-021 | Case sensitivity em filtros | Backend dev |
| TC-ESQ-022 | String muito longa (10000 chars) | QA |
| TC-ESQ-023 | Caracteres de controle (\n, \t) | Security |

### 6.4 Concorrencia/Estado
| ID | Teste | Esquecido Por |
|----|-------|---------------|
| TC-ESQ-024 | Race condition em regras | Backend dev |
| TC-ESQ-025 | Duplo clique em submit | Frontend dev |
| TC-ESQ-026 | Refresh no meio do fluxo | Frontend dev |
| TC-ESQ-027 | Browser back button | Frontend dev |
| TC-ESQ-028 | Sessao expirada durante operacao | Frontend dev |
| TC-ESQ-029 | Cache indevido no frontend | Frontend dev |
| TC-ESQ-030 | Ordenacao deterministica | Backend dev |

### 6.5 Infra/Config
| ID | Teste | Esquecido Por |
|----|-------|---------------|
| TC-ESQ-031 | Env var faltando = falha segura | DevOps |
| TC-ESQ-032 | Build em maquina limpa | DevOps |
| TC-ESQ-033 | Migration em DB com dados | DBA |
| TC-ESQ-034 | Dependencias vulneraveis | Security |
| TC-ESQ-035 | Logs com dados sensiveis | Security |
| TC-ESQ-036 | Rollback manual de deploy | DevOps |

---

## PARTE 7: GAPS CRITICOS IDENTIFICADOS

### GAP-001: Testes E2E Automatizados
```
Impacto: ALTO
Descricao: Sem Playwright/Cypress para fluxos criticos
Como Testar: Nao possivel ate implementar
Correcao Recomendada:
  1. Instalar Playwright
  2. Criar suite minima de 10 testes E2E
  3. Integrar no CI/CD
Prazo Sugerido: 2 semanas
```

### GAP-002: Rate Limiting
```
Impacto: ALTO
Descricao: Sem protecao contra abuso de API
Como Testar: Enviar 1000 requests em 1 segundo
Correcao Recomendada:
  1. Implementar rate limiter no Spring Boot
  2. Limitar por IP e por usuario
  3. Retornar 429 Too Many Requests
Prazo Sugerido: 1 semana
```

### GAP-003: Health/Readiness Endpoints
```
Impacto: MEDIO
Descricao: Sem endpoints para Kubernetes/load balancer
Como Testar: GET /actuator/health deve retornar 200
Correcao Recomendada:
  1. Habilitar Spring Boot Actuator
  2. Configurar health indicators
Prazo Sugerido: 1 dia
```

### GAP-004: Metricas Prometheus
```
Impacto: MEDIO
Descricao: Sem exportacao de metricas
Como Testar: GET /actuator/prometheus deve retornar metricas
Correcao Recomendada:
  1. Adicionar micrometer-registry-prometheus
  2. Configurar metricas customizadas
Prazo Sugerido: 1 semana
```

### GAP-005: Security Headers
```
Impacto: MEDIO
Descricao: Headers de seguranca incompletos
Como Testar: Inspecionar response headers
Correcao Recomendada:
  1. Adicionar X-Content-Type-Options
  2. Adicionar X-Frame-Options
  3. Configurar CSP
Prazo Sugerido: 1 dia
```

### GAP-006: RBAC no Backend
```
Impacto: ALTO
Descricao: Autenticacao mock no frontend
Como Testar: Endpoints devem validar roles
Correcao Recomendada:
  1. Implementar Spring Security
  2. Configurar Basic Auth + JWT
  3. Definir roles ADMIN/ANALYST
Prazo Sugerido: 2 semanas
```

### GAP-007: Pipeline CI/CD
```
Impacto: ALTO
Descricao: Sem .github/workflows definido
Como Testar: Push deve disparar pipeline
Correcao Recomendada:
  1. Criar workflow de CI
  2. Incluir lint, test, build
  3. Gate de cobertura minima
Prazo Sugerido: 1 semana
```

---

## PARTE 8: EXEMPLOS PRATICOS

### 8.1 cURL - Analyze Transaction
```bash
curl -X POST http://localhost:8080/api/transactions/analyze \
  -H "Content-Type: application/json" \
  -d '{
    "externalTransactionId": "tx-curl-001",
    "customerIdFromHeader": "cust-1",
    "customerAcctNumber": 1234567890,
    "pan": "4111111111111111",
    "transactionAmount": 100.00,
    "transactionDate": 20251219,
    "transactionTime": 120000,
    "transactionCurrencyCode": 986,
    "mcc": 7995,
    "consumerAuthenticationScore": 200,
    "externalScore3": 200,
    "cavvResult": 0,
    "eciIndicator": 5,
    "atcCard": 1,
    "atcHost": 1,
    "tokenAssuranceLevel": 80,
    "availableCredit": 1000.00,
    "cardCashBalance": 0.00,
    "cardDelinquentAmount": 0.00
  }'
```

### 8.2 cURL - List Transactions
```bash
curl -X GET "http://localhost:8080/api/transactions?page=0&size=10" \
  -H "Accept: application/json"
```

### 8.3 cURL - Create Rule
```bash
curl -X POST http://localhost:8080/api/rules \
  -H "Content-Type: application/json" \
  -d '{
    "ruleName": "HIGH_AMOUNT_ALERT",
    "description": "Alerta para transacoes acima de 10000",
    "ruleType": "CONTEXT",
    "weight": 40,
    "enabled": true,
    "classification": "SUSPICIOUS",
    "conditions": [{"field": "transactionAmount", "operator": ">", "value": "10000"}],
    "logicOperator": "AND"
  }'
```

### 8.4 Payload Invalido (Teste de Erro)
```json
{
  "externalTransactionId": "",
  "pan": "invalid"
}
```
**Expected**: 400 Bad Request

### 8.5 Assert Esperado (JUnit)
```java
assertThat(response.getStatusCode()).isEqualTo(HttpStatus.OK);
assertThat(response.getBody().getClassification()).isEqualTo("FRAUD");
assertThat(response.getBody().getTriggeredRules()).hasSize(1);
assertThat(response.getBody().getTriggeredRules().get(0).getName()).isEqualTo("RISK_MCC_7995");
```

---

## PARTE 9: MASSA DE DADOS DE TESTE

### 9.1 Transacoes de Exemplo
```sql
-- Transacao APPROVED
INSERT INTO transactions (...) VALUES (
  'tx-test-approved', 'cust-1', 1234567890, '411111******1111',
  'm-1', 'Supermercado', 50.00, 20251219, 120000, 986, 5411, ...
);

-- Transacao FRAUD (MCC alto risco)
INSERT INTO transactions (...) VALUES (
  'tx-test-fraud', 'cust-2', 9876543210, '555555******4444',
  'm-2', 'Cassino', 5000.00, 20251219, 130000, 986, 7995, ...
);

-- Transacao SUSPICIOUS (EMV falho)
INSERT INTO transactions (...) VALUES (
  'tx-test-suspicious', 'cust-3', 1111222233, '400000******0001',
  'm-3', 'Ecommerce', 1500.00, 20251219, 140000, 986, 5999,
  cardAipStatic='N', cardAipDynamic='N', ...
);
```

### 9.2 Regras de Teste
```sql
INSERT INTO rule_configurations (rule_name, rule_type, weight, enabled, classification, conditions_json, logic_operator)
VALUES 
  ('TEST_HIGH_AMOUNT', 'CONTEXT', 50, true, 'SUSPICIOUS', 
   '[{"field":"transactionAmount","operator":">","value":"5000"}]', 'AND'),
  ('TEST_MCC_CASINO', 'SECURITY', 80, true, 'FRAUD',
   '[{"field":"mcc","operator":"==","value":"7995"}]', 'AND');
```

### 9.3 Estrategia de Limpeza
```sql
-- Executar antes de cada suite de testes
DELETE FROM audit_logs;
DELETE FROM transaction_decisions;
DELETE FROM transactions;
DELETE FROM rule_configuration_history;
DELETE FROM rule_configurations;
```

---

## PARTE 10: CHECKLIST GO/NO-GO

### Criterios Obrigatorios (TODOS devem ser SIM)

| # | Criterio | Status | Evidencia |
|---|----------|--------|-----------|
| 1 | Todos testes P0 passando | [ ] | Relatorio JUnit |
| 2 | Zero bugs bloqueadores | [ ] | Jira/Issues |
| 3 | Cobertura minima 80% | [ ] | JaCoCo report |
| 4 | Sem vulnerabilidades criticas | [ ] | OWASP report |
| 5 | PAN mascarado em todos outputs | [ ] | Evidencia manual |
| 6 | Idempotencia validada | [ ] | Teste TC-API-002 |
| 7 | Concorrencia validada (50 threads) | [ ] | Teste TC-RULE-032 |
| 8 | Latencia P99 < 500ms | [ ] | k6 report |
| 9 | DB migrations aplicam clean | [ ] | Teste TC-DB-001 |
| 10 | Sem DEBUG logs em homolog | [ ] | Verificacao manual |

### Criterios Recomendados

| # | Criterio | Status | Impacto |
|---|----------|--------|---------|
| 11 | Testes E2E implementados | [ ] | Alto |
| 12 | Rate limiting implementado | [ ] | Alto |
| 13 | Health endpoints configurados | [ ] | Medio |
| 14 | Pipeline CI/CD documentado | [ ] | Alto |
| 15 | RBAC implementado | [ ] | Alto |

### Veredito

| Condicao | Resultado |
|----------|-----------|
| Todos criterios 1-10 = SIM | **GO** |
| Qualquer criterio 1-10 = NAO | **NO-GO** |
| Criterios 1-10 = SIM, gaps P1 identificados | **GO COM RESSALVAS** |

---

## PARTE 11: RASTREABILIDADE

### Endpoints vs Testes
| Endpoint | Metodo | Testes |
|----------|--------|--------|
| /api/transactions/analyze | POST | TC-API-001, TC-API-002, TC-API-003 |
| /api/transactions/analyze-advanced | POST | TC-API-011 |
| /api/transactions | GET | TC-API-004, TC-API-032 |
| /api/transactions/{id} | GET | TC-API-005 |
| /api/transactions/external/{id} | GET | TC-API-012 |
| /api/rules | GET, POST | TC-API-006, TC-API-007 |
| /api/rules/{id} | GET, PUT, DELETE | TC-API-008, TC-API-009 |
| /api/rules/{id}/toggle | PATCH | TC-API-010 |
| /api/audit | GET | TC-API-033 |
| /api/metrics | GET | TC-API-034 |

### Regras vs Testes
| Regra | Testes |
|-------|--------|
| EMV_SECURITY_CHECK | TC-RULE-001, TC-RULE-002 |
| TERMINAL_VERIFICATION_FAILED | TC-RULE-003 |
| EXPIRED_CARD | TC-RULE-004 |
| ... (28 regras) | TC-RULE-XXX |

---

**FIM DO DOCUMENTO**

*Gerado pelo Painel de 10 Especialistas QA*
*Data: 2025-12-19*
*Status: PRONTO PARA HOMOLOGACAO*
