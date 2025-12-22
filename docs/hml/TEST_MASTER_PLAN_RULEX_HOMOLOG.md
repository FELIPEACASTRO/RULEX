# RULEX — TEST MASTER PLAN (HOMOLOGAÇÃO) — QUADRUPLE CHECK AUDITOR

Versão do documento: v1.0 (2025-12-22)

**Escopo do sistema**
- Backend: Java 21 + Spring Boot (context-path `/api`)
- Frontend: React + Vite + React Query
- DB: Postgres + Flyway (migrations em [backend/src/main/resources/db/migration](backend/src/main/resources/db/migration))
- Contrato: OpenAPI em [openapi/rulex.yaml](openapi/rulex.yaml) (atenção: existem endpoints no código que não estão no OpenAPI)
- Segurança para homologação: HTTP Basic + RBAC (roles `ADMIN`, `ANALYST`)

**Restrição crítica (não negociável)**
- O payload de entrada de transação **não pode ser alterado**.

---

## 0) EXECUTIVE SUMMARY (decisão GO/NO-GO)

**Status atual baseado em evidências executadas nesta sessão**
- Backend unit/integration (Surefire): `mvn test` ✅ (51 testes, 0 falhas; BUILD SUCCESS)
- Frontend unit/component (Vitest): `pnpm test` ✅ ([client/src/pages/Rules.test.tsx](client/src/pages/Rules.test.tsx): 4/4)

**Decisão atual para HOMOLOGAÇÃO**: **NO-GO (bloqueado)**

**Motivos P0 (críticos) — não atendidos por falta de evidência executada e/ou gaps de contrato/observabilidade**
1) **OpenAPI incompleto vs. endpoints reais** (faltam endpoints como `/api/evaluate`, `/api/rules/validate|lint|simulate`, `/api/field-dictionary`, `/api/homolog/**`, `/api/audit/transaction/{id}`, `/api/rules/enabled/{enabled}`, `/api/rules/{id}/history`). Sem isso, não existe validação contratual “endpoint a endpoint” completa.
2) **Observabilidade P0 incompleta**: não há evidência de endpoints de health/readiness/liveness (não encontrado Actuator/health no código) → GAP crítico para homolog.
3) **Segurança P0 não testada ponta a ponta** (401 vs 403, RBAC por endpoint, preflight CORS com security enabled, bypass attempts, brute force). Existe ainda um risco de matcher com prefixo `/api` no Spring Security vs `context-path=/api` (ver GAP-SEC-001).
4) **Performance e Resiliência P0 não executadas** (k6/soak/spike, DB down, timeouts, recovery). Sem isso, não é possível declarar GO.

> Este plano define uma suíte P0 completa e executável. A decisão passa para **GO** somente após execução 100% P0 com evidências salvas e sem vazamentos de dados sensíveis.

---

## 1) MODO QUADRUPLE CHECK (4 checks obrigatórios)

### CHECK 1 — COBERTURA TOTAL (Completeness)
- Garantia: nenhuma área sem teste. Matriz de cobertura em seção 3.

### CHECK 2 — RISCO & IMPACTO (Risk-based)
- Matriz Risco x Impacto x Probabilidade em seção 4.
- **Regra**: qualquer P0 sem teste executável → **GAP CRÍTICO**.

### CHECK 3 — RASTREABILIDADE (Traceability)
- Traceability Matrix (endpoint/regra/requisito → Test IDs) em seção 7.
- Regra: cada endpoint deve ter (positivo, negativo, auth 401/403, contrato/schema, auditoria/log quando aplicável).
- Regras duras: (ativa, não ativa, borda, nulos/ausentes/""/0, conflito/precedência).

### CHECK 4 — EVIDÊNCIA & REPRODUTIBILIDADE (Proof)
- Para cada teste/suite: evidência obrigatória (logs, requests/responses, dumps DB, relatórios de cobertura, outputs de performance).
- Sem evidência objetiva → **teste inválido**.

---

## 2) AMBIENTES, PERFIS E PRÉ-REQUISITOS

### 2.1 Ambientes
- **DEV**: execução local do dev (rápida), tolera segurança desabilitada para smoke
- **TEST**: CI/local com DB isolado, sem dados reais
- **HML (Homologação)**: segurança habilitada, logs INFO, dados sintéticos, carga controlada

### 2.2 Variáveis/Config (mínimo)
Backend (Spring):
- `SPRING_DATASOURCE_URL`, `SPRING_DATASOURCE_USERNAME`, `SPRING_DATASOURCE_PASSWORD`
- `RULEX_SECURITY_ENABLED` (default `true`)
- `RULEX_ADMIN_USERNAME`, `RULEX_ADMIN_PASSWORD`
- `RULEX_ANALYST_USERNAME`, `RULEX_ANALYST_PASSWORD`

Frontend:
- `VITE_JAVA_API_URL`
- `VITE_API_BASIC_AUTH` (formato `user:pass`, para ambientes com Basic)

### 2.3 Dependências
- Java 21
- Maven
- Node + pnpm
- Docker (para Postgres/Testcontainers/k6)

### 2.4 Dados de teste (regras)
- Dados **sintéticos**, nunca PAN real. Sempre usar strings “PAN fake” (ex.: `4111111111111111`) e validar mascaramento.
- Dataset base recomendado em [fixtures](fixtures) (transações e casos de borda).

### 2.5 Reset/Cleanup
- DB: cada suite P0 roda em schema/DB isolado (Testcontainers ou docker-compose dedicado), com `flyway clean` **apenas** em ambientes não-prod.
- Auditoria/logs: exportar e versionar artefatos por execução.

---

## 3) MATRIZ DE COBERTURA (CHECK 1) — Área → Tests → Evidência

Tabela (colunas mínimas exigidas):

| Área | Subárea | Tipo de teste | Ferramenta | Qtd (P0) | Prioridade | Evidência | Automatizável (S/N) |
|---|---|---|---|---:|---|---|:---:|
| Build/CI | Formatter, unit tests | build gate + unit | Maven Surefire, Spotless | 1 pipeline | P0 | logs CI + [backend/target/surefire-reports](backend/target/surefire-reports) | S |
| UI | Regras (CRUD/toggle) | component | Vitest + Testing Library | 10 | P0 | junit/vitest report + snapshots | S |
| UI | Fluxos E2E críticos | e2e | Playwright | 40 | P0 | vídeo/screenshots + trace | S |
| API | Contrato OpenAPI | contract fuzz + schema | Schemathesis | 60 | P0 | junit xml + requests/responses | S |
| API | Endpoints fora do OpenAPI | contract gap tests | Postman/Newman ou custom | 20 | P0 | logs + json | S |
| Segurança | Basic + RBAC | authz/authn | Schemathesis + scripts | 40 | P0 | respostas 401/403 + headers | S |
| Regras duras | Base (12) + Advanced (28) | param tests | JUnit5 parameterized | 60+ | P0 | relatório JUnit + vetores usados | S |
| Regras duras | Determinismo + concorrência | stress/concurrency | JUnit + executor | 5 | P0 | logs + métricas de divergência | S |
| DB/Flyway | Fresh install | migration | Flyway + Testcontainers | 10 | P0 | log flyway + schema dump | S |
| DB/Flyway | Upgrade com dados | migration | Flyway + Testcontainers | 10 | P0 | dump antes/depois | S |
| DB | Constraints/rollback | integration | JUnit + repositories | 10 | P0 | evidência de rollback | S |
| Observabilidade | Logs/Correlation | integration | log capture + grep | 20 | P0 | arquivo de log + query | S |
| Performance | Latência/throughput | load | k6 | 20 | P0 | relatório k6 + CSV/JSON | S |
| Resiliência | DB down/timeouts/restart | chaos-ish | docker + scripts | 20 | P0 | logs + estado DB consistente | S |
| Compatibilidade | Encoding/UTF-8/Unicode | contract + UI | Schemathesis + Playwright | 10 | P0 | payloads com unicode + respostas | S |

**Completeness GAP (automático)**
- Qualquer linha com `Qtd (P0)` definida e ferramenta inexistente no repo deve gerar um GAP com correção mínima.

---

## 4) MATRIZ RISCO x IMPACTO x PROBABILIDADE (CHECK 2)

Escalas: Impacto (1–5), Probabilidade (1–5). Criticidade = Impacto × Probabilidade.

| Risco | Descrição | Impacto | Prob | Criticidade | Prioridade |
|---|---|---:|---:|---:|---|
| R-API-001 | Contrato divergente (OpenAPI ≠ runtime) | 5 | 4 | 20 | P0 |
| R-SEC-001 | RBAC incorreto (401/403 e roles) | 5 | 4 | 20 | P0 |
| R-PII-001 | Vazamento PAN/PII em log/response/DB raw | 5 | 3 | 15 | P0 |
| R-RULE-001 | Regra dura não determinística | 5 | 3 | 15 | P0 |
| R-DB-001 | Migração falhar em upgrade | 5 | 3 | 15 | P0 |
| R-PERF-001 | P95/P99 fora do limite sob carga | 4 | 4 | 16 | P0 |
| R-RES-001 | Inconsistência após falha DB/restart | 5 | 3 | 15 | P0 |
| R-OBS-001 | Sem health/readiness → indisponibilidade invisível | 4 | 4 | 16 | P0 |
| R-UI-001 | UI sem tratamento de timeout/401/500 | 4 | 3 | 12 | P0 |
| R-SCA-001 | Dependência com CVE crítica | 5 | 2 | 10 | P0 |

**Regra de auditoria**
- Qualquer risco P0 sem testes executáveis e evidência → **GAP CRÍTICO**.

---

## 5) TEST STRATEGY — automação e execução (pipeline)

### 5.1 Sequência recomendada (gates)
1) **Build/Unit**
   - Backend: `mvn -f backend/pom.xml test`
   - Frontend: `pnpm -C client test`
2) **DB + Migrations (isolado)**
   - Subir Postgres limpo; aplicar Flyway; validar schema.
3) **API Contract (OpenAPI)**
   - Executar Schemathesis contra backend rodando.
4) **Security (401/403/RBAC + bypass)**
5) **Rules Engine (vetores)**
6) **UI E2E**
7) **Observabilidade**
8) **Performance (k6)**
9) **Resiliência (falhas controladas)**
10) **SCA (deps)**

### 5.2 Gates de aprovação (limiares)
- P0: **100% passing** (obrigatório)
- P1: ≥ 95% passing (apenas se falha não for PII/security/data-loss)
- P2: best-effort, não bloqueia homolog se documentado
- Coverage mínimo (definição para homolog):
  - Backend: ≥ 80% linhas e ≥ 70% branches (Jacoco)
  - Frontend: ≥ 70% linhas (Vitest coverage)
- Vulnerabilidades:
  - 0 CVE crítica/alta em runtime (senão NO-GO)
- Performance (definição inicial — ajustar com SLO real):
  - Endpoints críticos: P95 ≤ 250ms, P99 ≤ 800ms em carga nominal
- Segurança/PII:
  - **0 vazamentos** de PAN/PII em logs/respostas/DB raw

---

## 6) SUÍTE P0 (OBRIGATÓRIA) — catálogos executáveis + evidências

### A) API/CONTRATO — mínimo 60 testes (P0)
Baseado em [openapi/rulex.yaml](openapi/rulex.yaml) + endpoints reais no código.

**Ferramenta padrão (contrato)**: Schemathesis (OpenAPI 3)
- Execução (exemplo):
  - `schemathesis run openapi/rulex.yaml --base-url http://localhost:8080 --report junitxml=artifacts/api/junit.xml --hypothesis-deadline=2000`

**Catálogo mínimo de 60 IDs (API-P0-001…060)**
Regra: 5 testes por endpoint OpenAPI (12 endpoints × 5 = 60).

Para cada endpoint abaixo, executar:
- (1) Positivo 2xx
- (2) Negativo 400/422 (payload inválido)
- (3) Auth: 401 sem credenciais (se endpoint protegido)
- (4) Auth: 403 role errada (se aplicável)
- (5) Contrato: validação schema response + content-type UTF-8

Endpoints OpenAPI:
1) POST `/api/transactions/analyze` → API-P0-001…005
2) POST `/api/transactions/analyze-advanced` → API-P0-006…010
3) GET `/api/transactions` → API-P0-011…015
4) GET `/api/transactions/{id}` → API-P0-016…020
5) GET `/api/transactions/external/{externalId}` → API-P0-021…025
6) GET `/api/rules` → API-P0-026…030
7) POST `/api/rules` → API-P0-031…035
8) GET `/api/rules/{id}` → API-P0-036…040
9) PUT `/api/rules/{id}` → API-P0-041…045
10) DELETE `/api/rules/{id}` → API-P0-046…050
11) PATCH `/api/rules/{id}/toggle` → API-P0-051…055
12) GET `/api/audit` → API-P0-056…060

**Obrigatórios adicionais (P0) — fora do OpenAPI (contrato ausente → GAP se não documentar/validar)**
- POST `/api/evaluate`
- POST `/api/rules/validate`
- POST `/api/rules/lint`
- POST `/api/rules/simulate`
- GET `/api/field-dictionary`
- `/api/homolog/**`
- GET `/api/audit/transaction/{transactionId}`
- GET `/api/rules/enabled/{enabled}`
- GET `/api/rules/{id}/history`

Evidências P0 (API):
- [artifacts/api/junit.xml](artifacts/api/junit.xml)
- amostras de request/response (mín. 1 por endpoint) em [artifacts/api/samples](artifacts/api/samples)
- logs do backend do intervalo do teste em [artifacts/logs/backend_api.log](artifacts/logs/backend_api.log)

### B) MOTOR DE REGRAS DURAS — mínimo 60 testes (P0)

**Regras base (RuleEngineService) — 12 chaves**
- LOW_AUTHENTICATION_SCORE
- LOW_EXTERNAL_SCORE
- INVALID_CAVV
- INVALID_CRYPTOGRAM
- CVV_MISMATCH
- HIGH_TRANSACTION_AMOUNT
- HIGH_RISK_MCC
- INTERNATIONAL_TRANSACTION
- CARD_NOT_PRESENT
- PIN_VERIFICATION_FAILED
- CVV_PIN_LIMIT_EXCEEDED
- OFFLINE_PIN_FAILED

**Regras avançadas (AdvancedRuleEngineService) — 28 chaves**
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

**Catálogo mínimo de 60 IDs (RULE-P0-001…060)**
Estratégia: 2 testes por regra para 30 regras (60) como mínimo “core”.
- Para homologação real, expandir com borda/nulos/precedência (P1/P0-Plus).

Formato por regra (mínimo core):
- (a) ativa (dispara) → RULE-P0-xxx-A
- (b) não ativa (não dispara) → RULE-P0-xxx-B

Evidências:
- relatório JUnit
- vetores usados (JSON/YAML) versionados
- logs: não conter PAN em claro
- auditoria persistida (se habilitada): consulta posterior

Requisitos adicionais P0 (cross-cutting):
- Determinismo: mesma entrada → mesmo output (RULE-P0-DET-001)
- Null vs ausente vs "" vs 0 em campos críticos (RULE-P0-NULL-001..005)
- Timezone/virada de dia (RULE-P0-TZ-001..003)
- Concorrência 50–200 execuções simultâneas (RULE-P0-CONC-001)
- Auditoria por execução (RULE-P0-AUD-001)
- Mascaramento PAN/PII em logs/respostas (RULE-P0-PII-001)

### C) BANCO/MIGRATIONS — mínimo 30 testes (P0)

Migrations detectadas:
- V1__init.sql
- V2__core_schema.sql
- V3__extend_workflow_length.sql
- V4__raw_hash_idempotency.sql
- V5__raw_as_received.sql
- V6__v31_exec_log_field_dictionary.sql
- V7__v31_exec_log_dedup.sql

Catálogo DB-P0-001..030:
- Fresh install DB vazio → Flyway OK (10)
- Upgrade com dados (aplicar V1..Vx, inserir dados, aplicar Vx+1..V7) (10)
- Constraints + rollback + índices básicos (10)

Evidências:
- `flyway info` / logs
- dump schema (pg_dump --schema-only)
- dumps antes/depois (upgrade)
- evidência de unique/index de idempotência (V4) funcionando

### D) SEGURANÇA — mínimo 40 testes (P0)

Base: HTTP Basic + roles ADMIN/ANALYST. Matriz RBAC derivada do [backend/src/main/java/com/rulex/config/SecurityConfig.java](backend/src/main/java/com/rulex/config/SecurityConfig.java).

Catálogo SEC-P0-001..040 (exemplos obrigatórios):
- 401 sem credenciais para endpoints protegidos
- 403 com role insuficiente
- Confirmar endpoints public (permitAll) realmente públicos
- Bypass: método alternativo, path similar, double slash
- SQLi/XSS/header injection/log injection (mín. 10)
- Sensitive data exposure (PAN/PII) em response/log/DB raw
- SCA: `pnpm audit`, `mvn -DskipTests dependency:tree` + OWASP dep-check (se adotado)
- Bruteforce básico (se sem mitigação → GAP)

Evidências:
- respostas 401/403
- headers de segurança (se ausente → GAP)
- relatórios SCA

### E) FRONTEND (UI/E2E) — mínimo 40 testes (P0)

Ferramenta recomendada: Playwright.

Catálogo UI-P0-001..040:
- Fluxos críticos ponta a ponta: Regras (listar/criar/editar/toggle/deletar), Transações (analisar/listar/detalhar), Auditoria, Métricas, Homologação (se exposta na UI)
- Estados: loading/erro/vazio
- 401/403/500/timeout e UX
- Double submit/duplo clique
- Refresh no meio do fluxo
- Acessibilidade mínima (labels, foco, teclado)
- Cross-browser mínimo (Chromium + WebKit)

Evidências:
- screenshots, traces, vídeos
- payloads e responses capturados

### F) OBSERVABILIDADE/OPERAÇÃO — mínimo 20 testes (P0)

Catálogo OBS-P0-001..020:
- Logs nível INFO em HML
- Correlation ID por request (se não existir → GAP)
- Health/readiness endpoints (se não existir → GAP CRÍTICO)
- Auditoria: trilha completa inclusive em falhas parciais

Evidências:
- arquivo de log + filtro por transactionId/externalTransactionId
- export audit (JSON) por execução

### G) PERFORMANCE — mínimo 20 testes (P0)

Ferramenta: k6.

Catálogo PERF-P0-001..020:
- P50/P95/P99 para endpoints críticos
- Throughput e saturação
- Spike
- Soak 30–60min
- Payload grande (sem alterar schema)
- CPU/mem e vazamento
- DB sob carga

Evidências:
- relatório k6 (HTML/JSON)
- CSV de latências
- snapshots de recursos (docker stats)

### H) RESILIÊNCIA / FALHAS — mínimo 20 testes (P0)

Catálogo RES-P0-001..020:
- DB down durante request
- Rede instável/timeouts
- Retry/replay sob falha
- Recovery após restart
- Falha parcial no meio de gravação/auditoria (sem meia gravação)

Evidências:
- logs do backend + estado do DB consistente
- prova de idempotência (mesmo payload → mesma decisão)

---

## 7) TRACEABILITY MATRIX (CHECK 3)

### 7.1 Endpoints (OpenAPI) → Test IDs

| Endpoint | Requisito mínimo | Test IDs (P0) |
|---|---|---|
| POST /api/transactions/analyze | +/-, contrato, auth quando aplicável, audit | API-P0-001..005 + SEC-P0-* + OBS-P0-* |
| POST /api/transactions/analyze-advanced | idem + advanced rules | API-P0-006..010 + RULE-P0-* |
| GET /api/transactions | paginação, filtros, auth | API-P0-011..015 + SEC-P0-* |
| GET /api/transactions/{id} | 200/404, auth | API-P0-016..020 |
| GET /api/transactions/external/{externalId} | 200/404, auth | API-P0-021..025 |
| GET /api/rules | paginação, auth | API-P0-026..030 |
| POST /api/rules | 201/400/409, auth | API-P0-031..035 |
| GET /api/rules/{id} | 200/404, auth | API-P0-036..040 |
| PUT /api/rules/{id} | 200/400/409, auth | API-P0-041..045 |
| DELETE /api/rules/{id} | 204/404, auth | API-P0-046..050 |
| PATCH /api/rules/{id}/toggle | 200/404, auth | API-P0-051..055 |
| GET /api/audit | filtros, auth | API-P0-056..060 |

### 7.2 Endpoints reais (não documentados no OpenAPI) → Test IDs

| Endpoint (código) | Status | Test IDs | Nota |
|---|---|---|---|
| POST /api/evaluate | **GAP de contrato** | API-EXT-P0-001..005 | adicionar ao OpenAPI |
| POST /api/rules/validate | **GAP de contrato** | API-EXT-P0-006..010 | adicionar ao OpenAPI |
| POST /api/rules/lint | **GAP de contrato** | API-EXT-P0-011..015 | adicionar ao OpenAPI |
| POST /api/rules/simulate | **GAP de contrato** | API-EXT-P0-016..020 | adicionar ao OpenAPI |
| GET /api/field-dictionary | **GAP de contrato** | API-EXT-P0-021..025 | adicionar ao OpenAPI |
| /api/homolog/** | **GAP de contrato** | API-EXT-P0-026..040 | adicionar ao OpenAPI |
| GET /api/audit/transaction/{id} | **GAP de contrato** | API-EXT-P0-041..045 | adicionar ao OpenAPI |
| GET /api/rules/enabled/{enabled} | **GAP de contrato** | API-EXT-P0-046..050 | adicionar ao OpenAPI |
| GET /api/rules/{id}/history | **GAP de contrato** | API-EXT-P0-051..055 | adicionar ao OpenAPI |

### 7.3 Regras duras → Test IDs

Regra: cada regra deve ter (ativa, não ativa, borda, nulos/ausentes/""/0, precedência).
- P0 core garante (ativa + não ativa) para pelo menos 30 regras (60 testes).
- P1/P0-Plus cobre borda/nulos/precedência para todas.

---

## 8) EVIDÊNCIA & REPRODUTIBILIDADE (CHECK 4)

### 8.1 Estrutura de artefatos (obrigatória)
- [artifacts](artifacts)
  - [artifacts/api](artifacts/api) (junit xml + samples)
  - [artifacts/ui](artifacts/ui) (playwright traces + screenshots + vídeos)
  - [artifacts/perf](artifacts/perf) (k6 json/csv/html)
  - [artifacts/db](artifacts/db) (schema dumps + before/after)
  - [artifacts/logs](artifacts/logs) (backend logs por suite)
  - [artifacts/sca](artifacts/sca) (audit reports)

### 8.2 Evidência mínima por teste
- Identificador do teste (ex.: API-P0-031)
- Request (método, URL, headers relevantes, body)
- Response (status, headers, body)
- Timestamp + versão do build
- Referência ao log correlacionado
- Se gravar DB: query/dump que prova persistência

---

## 9) SUÍTES P1 e P2

### P1 (robustez)
- Expansão de bordas por regra (todas as 40 regras × 5 variações)
- Compatibilidade avançada (timezone, locale, formatos)
- UI acessibilidade aprofundada
- Performance em cenários alternativos

### P2 (nice-to-have)
- Exploratório guiado
- Chaos avançado
- Segurança aprofundada (OWASP ASVS mapeado)

---

## 10) “TESTES QUE AS PESSOAS ESQUECEM” (mín. 50)

1) OPTIONS preflight (CORS)
2) 401 vs 403 correto
3) Null vs ausente vs "" vs 0
4) Unicode/acentos/emoji + UTF-8
5) Datas: virada de dia, DST, timezone
6) Double submit / duplo clique
7) Refresh no meio do fluxo
8) Ordem dos campos JSON diferente
9) Campos extras no JSON (deve falhar se `fail-on-unknown-properties=true`)
10) Content-Type errado (`text/plain`, `application/xml`)
11) Body vazio
12) JSON inválido (truncado)
13) Números fora do range (int64 overflow)
14) Valores negativos (amount, scores)
15) Decimais com vírgula vs ponto
16) Moeda e arredondamento
17) Strings longas (10k)
18) Caracteres de controle (`\n`, `\r`, `\t`)
19) Header injection
20) Log injection
21) SQLi em query params
22) XSS refletido
23) XSS armazenado (se existir persistência)
24) CORS com origin não permitido
25) CORS com credentials
26) Cache indevido de respostas
27) Rate limit inexistente (registrar GAP)
28) Replay/idempotência com mesmo payload
29) Concurrency 50–200 simultâneos no motor
30) Race condition em toggle de regra
31) Deletar regra referenciada
32) Auditoria em caso de erro 4xx
33) Auditoria em caso de erro 5xx
34) Transação parcial (rollback)
35) DB pool saturado
36) DB down durante request
37) Restart do backend durante carga
38) Timeout no cliente
39) Retry manual no UI
40) Persistência de estado de filtros
41) Paginação fora do range
42) `page=-1`, `size=0`, `size=10000`
43) Path traversal (se houver download)
44) `//` no path
45) URL encoding estranho
46) `Accept: */*` vs `application/json`
47) `charset` diferente
48) Compressão (gzip) se habilitada
49) Logging de stacktrace em erro (não pode vazar)
50) Vazamento de PAN em DB raw store

---

## 11) GAPS + CORREÇÃO MÍNIMA PARA HOMOLOGAÇÃO

### GAP-API-001 — OpenAPI incompleto vs endpoints reais
- Impacto: contrato não validável; clientes quebram; homolog inválida
- Como reproduzir: comparar [openapi/rulex.yaml](openapi/rulex.yaml) com controllers reais
- Teste que prova: API-EXT-P0-* (falha por ausência de schema)
- Correção mínima: atualizar OpenAPI para incluir endpoints reais + schemas
- Prioridade: P0

### GAP-API-002 — Divergência do contrato de /api/rules (paginado vs array)
- Impacto: quebra de client; inconsistência de integração
- Como reproduzir: OpenAPI descreve `PageRule`, mas frontend (ver [client/src/lib/javaApi.ts](client/src/lib/javaApi.ts)) espera array
- Teste que prova: API-P0-026..030 (schema) + UI-P0-* (integração)
- Correção mínima: alinhar implementação e OpenAPI (sem alterar payload de transação). Definir padrão: manter paginação e ajustar client.
- Prioridade: P0

### GAP-OBS-001 — Health/Readiness endpoints ausentes
- Impacto: operação cega em HML; sem probe
- Como reproduzir: não existem rotas /actuator/health etc no código
- Teste que prova: OBS-P0-0xx
- Correção mínima: incluir Spring Boot Actuator + endpoints health/readiness (protegidos)
- Prioridade: P0

### GAP-SEC-001 — Risco de match incorreto em SecurityFilterChain
- Impacto: endpoints permitAll podem virar auth-required (ou o oposto)
- Como reproduzir: `server.servlet.context-path=/api` e matchers em [backend/src/main/java/com/rulex/config/SecurityConfig.java](backend/src/main/java/com/rulex/config/SecurityConfig.java) usam prefixo `/api/...`
- Teste que prova: SEC-P0-0xx (public endpoints sem auth)
- Correção mínima: ajustar matchers para paths corretos (sem alterar payload)
- Prioridade: P0

### GAP-SEC-002 — Security headers não evidenciados
- Impacto: hardening incompleto
- Teste que prova: SEC-P0-0xx (headers)
- Correção mínima: configurar headers (CSP, XFO, XCTO, Referrer-Policy, etc.)
- Prioridade: P1 (vira P0 se requisito corporativo)

### GAP-PII-001 — Persistência de payload raw pode conter PAN
- Impacto: compliance/PCI
- Teste que prova: RULE-P0-PII-001 + DB-P0-* (inspecionar raw store)
- Correção mínima: sanitizar/maskar antes de persistir raw (ou criptografar/segregar)
- Prioridade: P0

---

## 12) COMO EXECUTAR (prático, reproduzível)

### 12.1 Backend
- `mvn -f backend/pom.xml test`
- (recomendado) `mvn -f backend/pom.xml verify`

Evidência:
- [backend/target/surefire-reports](backend/target/surefire-reports)

### 12.2 Frontend
- `pnpm -C client test`

Evidência:
- output vitest + snapshots

### 12.3 DB + Flyway
- Subir Postgres via docker-compose (repo) e aplicar migrations (Flyway em startup)
- Dump schema: `pg_dump --schema-only`

### 12.4 API Contract
- Rodar backend e executar Schemathesis contra [openapi/rulex.yaml](openapi/rulex.yaml)

### 12.5 E2E
- Playwright (a implementar se não existir)

### 12.6 Performance
- k6 (a implementar se não existir)

### 12.7 Resiliência
- scripts controlados com docker stop/start do DB

---

## 13) GO/NO-GO (final)

**GO somente se**
- P0 100% pass
- Evidências salvas em [artifacts](artifacts)
- 0 vazamentos de dados sensíveis
- 0 CVEs críticas/altas
- Performance dentro dos limites

**NO-GO se**
- qualquer P0 falhar
- qualquer GAP crítico aberto (OpenAPI incompleto, health/readiness ausente, RBAC incorreto, vazamento PII)

---

FIM DO DOCUMENTO
