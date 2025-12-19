# Votação Consolidada do Painel Multidisciplinar (imparcial, baseada em evidência)

**Data**: 2025-12-19  
**Projeto**: RULEX — Motor de Regras Duras Bancárias  
**Versão avaliada**: estado atual do repositório em `/workspace`  

---

## PASSO 0 — Inventário do repositório

### Árvore de diretórios (diretórios apenas)

- `/workspace/.manus/`
- `/workspace/attached_assets/`
- `/workspace/audit/`
- `/workspace/backend/`
  - `src/main/java/com/rulex/` (`controller/`, `service/`, `repository/`, `entity/`, `dto/`, `homolog/`, `config/`, `api/`, `util/`)
  - `src/main/resources/` (`application.yml`, `db/migration/`)
  - `src/test/java/com/rulex/` (`service/`, `controller/`, `homolog/`, `architecture/`, `testsupport/`)
- `/workspace/client/`
  - `src/` (`components/`, `components/ui/`, `pages/`, `lib/`, `contexts/`, `hooks/`)
- `/workspace/docs/`
  - `hml/`
  - `review/`
- `/workspace/Insomnia/`
- `/workspace/openapi/`
- `/workspace/patches/`
- `/workspace/shared/`

### Contagem de arquivos por tipo (walk do workspace; exclui `.git`, `node_modules`, `dist`, `build`, `target`)

| Tipo | Quantidade |
|---|---:|
| Java | 118 |
| TSX | 77 |
| TS | 14 |
| SQL | 3 |
| JSON | 11 |
| YAML/YML | 5 |
| MD | 39 |

### Componentes identificados

- **Backend (Java/Spring Boot + Postgres/Flyway)**: `backend/`
- **Frontend (React/Vite)**: `client/` + `vite.config.ts`
- **Banco**:
  - **PostgreSQL**: migrations em `backend/src/main/resources/db/migration/*.sql`
- **Testes**:
  - Unitários Java: `backend/src/test/java/com/rulex/service/*Test.java`
  - Integração/API (nomes `*IT.java`, com Postgres/Testcontainers): `backend/src/test/java/com/rulex/**/**IT.java`
- **Config/scripts**:
  - OpenAPI: `openapi/rulex.yaml`
  - Script Node (validação de regras): `validate-rules.mjs` (**atenção: desatualizado vs payload Java**) 

---

## PASSO 1 — Extração de funcionalidades

### Endpoints (Spring Boot) — basePath real: `/api` (context-path)

Evidência: `backend/src/main/resources/application.yml` define `server.servlet.context-path: /api`.

- **Transações** (`backend/src/main/java/com/rulex/controller/TransactionController.java`)
  - `POST /api/transactions/analyze`
  - `POST /api/transactions/analyze-advanced`
  - `GET /api/transactions`
  - `GET /api/transactions/{id}`
  - `GET /api/transactions/external/{externalId}`
- **Avaliação com popups** (`backend/src/main/java/com/rulex/controller/EvaluateController.java`)
  - `POST /api/evaluate`
- **Regras (CRUD + toggle + history + enabled)** (`backend/src/main/java/com/rulex/controller/RuleController.java`)
  - `GET /api/rules`
  - `POST /api/rules`
  - `GET /api/rules/{id}`
  - `PUT /api/rules/{id}`
  - `DELETE /api/rules/{id}`
  - `PATCH /api/rules/{id}/toggle`
  - `GET /api/rules/enabled/{enabled}`
  - `GET /api/rules/{id}/history`
- **Auditoria** (`backend/src/main/java/com/rulex/controller/AuditController.java`)
  - `GET /api/audit`
  - `GET /api/audit/transaction/{transactionId}`
- **Métricas** (`backend/src/main/java/com/rulex/controller/MetricsController.java`)
  - `GET /api/metrics`
  - `GET /api/metrics/mcc`
  - `GET /api/metrics/merchant`
  - `GET /api/metrics/timeline`
- **Homolog (versionamento + ruleset + simulação)**
  - `POST /api/homolog/rules`
  - `GET /api/homolog/rules/{ruleId}/latest`
  - `POST /api/homolog/rules/versions/{ruleVersionId}/publish`
  - `POST /api/homolog/rules/{ruleId}/rollback/{version}`
  - `POST /api/homolog/rulesets`
  - `POST /api/homolog/rulesets/versions/{ruleSetVersionId}/publish`
  - `POST /api/homolog/rulesets/activate`
  - `POST /api/homolog/simulations/run`

### Conceito “Popup de Regra → 1..N Regras”

**Encontrado** no endpoint `/api/evaluate`:
- DTO: `backend/src/main/java/com/rulex/dto/EvaluateResponse.java` contém `popups: List<PopupDTO>`.
- Implementação: `backend/src/main/java/com/rulex/service/RuleEngineService.java` agrega `List<RuleHitDTO>` em `PopupDTO` (`aggregatePopups`).

### Fluxos críticos (P0/P1)

- **P0**: analisar transação (persistir + decisão + auditoria) — `POST /api/transactions/analyze`.
- **P0**: CRUD/toggle de regras (impacta decisão em tempo real) — `/api/rules/*`.
- **P0**: `/api/evaluate` (retorna decisão + hits + popups agregados).
- **P1**: fluxo de homologação (criar/publish/activate/simular ruleset) — `/api/homolog/*`.
- **P1**: auditoria e métricas — `/api/audit`, `/api/metrics/*`.

---

## PASSO 2 — Extração REAL das regras duras

### Onde as regras estão

- **Motor core (configurável via banco)**:
  - Avaliação: `backend/src/main/java/com/rulex/service/RuleEngineService.java` (`evaluateRules` → `evaluateRuleGeneric`).
  - Persistência: tabela `rule_configurations` com `conditions_json` e `logic_operator` (`backend/src/main/resources/db/migration/V2__core_schema.sql`).
  - Operadores suportados no core: `== != > < >= <= IN NOT_IN CONTAINS NOT_CONTAINS`.

- **Fallback “legado por nome” (12 regras hardcoded)**: `RuleEngineService.evaluateRuleGeneric` (switch por `ruleName`).
  - Exemplos: `LOW_AUTHENTICATION_SCORE`, `INVALID_CAVV`, `HIGH_RISK_MCC`, `OFFLINE_PIN_FAILED`, etc.

- **Motor avançado (28 regras determinísticas)**:
  - Implementação: `backend/src/main/java/com/rulex/service/AdvancedRuleEngineService.java`.
  - Testes unitários: `backend/src/test/java/com/rulex/service/AdvancedRuleEngineServiceTest.java`.

- **Motor de homolog (versionado por ruleset, com DSL e JSONB)**:
  - Use case: `backend/src/main/java/com/rulex/homolog/usecase/HomologRuleSetUseCase.java`.
  - DSL evaluator: `backend/src/main/java/com/rulex/homolog/adapter/RuleDslEvaluatorAdapter.java`.
  - Operadores homolog: `EQ NEQ GT GTE LT LTE IN NOT_IN CONTAINS STARTS_WITH ENDS_WITH IS_NULL NOT_NULL`.
  - Persistência: `backend/src/main/resources/db/migration/V1__init.sql`.

### Lista objetiva das 28 regras “advanced” (nomes + evidência)

Evidência: `backend/src/main/java/com/rulex/service/AdvancedRuleEngineService.java` (método `executeAllAdvancedRulesDetailed`).

1. `EMV_SECURITY_CHECK`
2. `TERMINAL_VERIFICATION_FAILED`
3. `EXPIRED_CARD`
4. `SUSPICIOUS_TRANSACTION_TYPE`
5. `UNUSUAL_CARD_MEDIA`
6. `SUSPICIOUS_TERMINAL`
7. `ECOMMERCE_NO_AVS`
8. `POS_SECURITY_MISSING`
9. `CARD_CAPTURE_FRAUD`
10. `PIN_CVV_LIMIT_EXCEEDED`
11. `OFFLINE_PIN_FAILED`
12. `MISSING_CVV2_HIGH_RISK`
13. `CUSTOM_INDICATOR_FRAUD`
14. `PROCESSING_LAG_ANOMALY`
15. `TIMEZONE_NORMALIZED_CHECK`
16. `DUPLICATE_TRANSACTION`
17. `SUSPICIOUS_MERCHANT_POSTAL`
18. `SUSPICIOUS_TOKEN`
19. `UNEXPECTED_CURRENCY`
20. `ANOMALOUS_CONVERSION_RATE`
21. `INCOHERENT_AUTH_SEQUENCE`
22. `INCOHERENT_CONTEXT`
23. `CONTRADICTORY_AUTHORIZATION`
24. `SUSPICIOUS_ACQUIRER`
25. `ACQUIRER_COUNTRY_MISMATCH`
26. `COMBINED_SCORE_CHECK`
27. `VELOCITY_CHECK_CONSOLIDATED`
28. `CUSTOM_INDICATORS_COMPREHENSIVE`

### Regras inexistentes mas “esperadas” (GAP)

- **GAP P1**: `fixtures/crtran.json` como baseline único de payload (referenciado em IT + Insomnia) não existe.
- **GAP P1**: “lista de 60 regras” só em documentação não prova implementação (necessário mapear doc→código/regra versionada/configurada).

---

## PASSO 3 — Votos individuais (0–10) e justificativa

As notas detalhadas por especialista estão em: `docs/review/notas_por_especialista.md`.

---

## PASSO 4 — QA & Homologação

### Evidências encontradas

- **Testes unitários (Java)**: `backend/src/test/java/com/rulex/service/*Test.java`.
- **Testes de arquitetura (ArchUnit)**: `backend/src/test/java/com/rulex/architecture/CleanArchitectureRulesTest.java`.
- **Testes de integração (API + Postgres/Testcontainers) existem**: `backend/src/test/java/com/rulex/controller/*IT.java`, `backend/src/test/java/com/rulex/homolog/HomologSimulationIT.java`.
- **Mas**: `backend/pom.xml` não evidencia execução automática desses `*IT.java` (sem `maven-failsafe-plugin`).

### Bloqueadores de homologação

- **P0**: lockfile inconsistente (`pnpm install --frozen-lockfile` falha) — ver `docs/review/matriz_gaps_riscos.md`.
- **P0**: inventários/scripts apontando para módulos ausentes (gera risco de build/inconsistência) — ver `docs/review/matriz_gaps_riscos.md`.

---

## PASSO 5 — Insomnia (homologação manual)

### Evidência

- Diretório existe: `Insomnia/`
- Coleção: `Insomnia/rulex-hml.insomnia.json`

### Observação crítica

A coleção contém requests para “Node tRPC” e menciona `node_base_url`, porém **não há evidência de implementação Node/server no repositório atual** (não existe diretório `server/`). Logo, a parte “Node tRPC” da coleção deve ser tratada como **GAP** até existir código.

---

## Consolidação da votação (pesos fixos)

| ESPECIALISTA | NOTA | PESO | SCORE PONDERADO | Principal argumento (resumo) |
|---|---:|---:|---:|---|
| Negócio (Crédito/Fraude) | 7.2 | 1.3 | 9.36 | Regras determinísticas e popup 1..N; risco por duplicidade core vs homolog |
| Product Owner Técnico | 6.0 | 1.0 | 6.00 | Contratos e inventários divergentes; OpenAPI incompleta |
| Arquiteto de Software | 7.0 | 1.2 | 8.40 | Arquitetura boa, mas dois motores e DSLs diferentes |
| UX Designer | 6.0 | 1.0 | 6.00 | UX existe, mas “simulação local” pode mascarar defeitos |
| UI Designer | 6.5 | 0.9 | 5.85 | Componentes ok; faltam artefatos de consistência (tokens) |
| Product Designer | 6.0 | 0.9 | 5.40 | Fluxos existem, mas parte avançada depende de integração não evidenciada |
| Backend Engineer Java | 8.2 | 1.2 | 9.84 | Core robusto, idempotência, regras genéricas + 28 advanced com testes |
| Frontend Engineer React | 4.5 | 1.0 | 4.50 | Contrato/cliente divergente e referência a servidor inexistente |
| DBA / PostgreSQL | 7.2 | 1.1 | 7.92 | Migrations sólidas; JSONB no homolog; TEXT no core é tradeoff |
| QA Engineer (Lead) | 5.0 | 1.3 | 6.50 | ITs existem mas execução não evidenciada; lockfile quebra install |
| AppSec / Segurança (OWASP + LGPD) | 4.8 | 1.2 | 5.76 | PAN masking ok; sem auth e credenciais default em compose |
| DevOps / SRE | 5.2 | 1.0 | 5.20 | Compose ok; sem CI e sem install reprodutível |

### Cálculo

- **Soma dos pesos**: 13.1
- **Soma dos scores ponderados**: 80.73
- **Média ponderada final**: **80.73 / 13.1 = 6.16**

### Divergências relevantes

- **Backend vs Frontend/QA**: backend é tecnicamente sólido, mas frontend/pipeline têm bloqueadores (lockfile e integrações incoerentes).

### Top 3 maiores riscos

1. **P0**: instalação/pipeline Node/Front não reprodutível (lockfile).
2. **P1**: UI/cliente com contrato divergente pode mascarar defeitos reais.
3. **P1**: API Java sem autenticação (ambiente bancário exige controle).

### Top 3 maiores gaps

1. **P0**: lockfile inconsistente (`pnpm install --frozen-lockfile` falha).
2. **P0**: inventários/scripts inconsistentes; referências a módulos ausentes.
3. **P1**: OpenAPI não cobre endpoints críticos (`/evaluate`, `/homolog/*`).
