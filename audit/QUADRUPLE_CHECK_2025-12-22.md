# QUADRUPLE CHECK — RULEX (4 PASSADAS) — 2025-12-22

Branch: `appmod/java-upgrade-20251222160646`

## Regras de auditoria (ANTI-ALUCINAÇÃO)
- **Somente** fatos comprováveis por artefatos no repositório (código, migrations, OpenAPI, testes).
- Quando algo **não** puder ser comprovado, registrar exatamente:
  - **NÃO ENCONTRADO NO CÓDIGO**
  - **NÃO ENCONTRADO NO BANCO** (migrations/schema)
  - **NÃO ENCONTRADO NO OPENAPI**
  - e abrir **GAP obrigatório** na PASSADA 2.

---

# PASSADA 1 — STATE OF THE WORLD REPORT (DB/BACK/FRONT/FEATURES)

## 1) Runtime / Config
- O Java Core roda em `:8080` e define `context-path: /api`.
  - Evidência: `backend/src/main/resources/application.yml#L38-L41`.
- Flyway habilitado com migrations em `classpath:db/migration`.
  - Evidência: `backend/src/main/resources/application.yml#L33-L36`.

## 2) Banco (schema via Flyway)
### 2.1 Core transacional (V2)
- Tabelas core:
  - `transactions` (inclui `external_transaction_id`, `pan`, `transaction_amount`, `transaction_date`, `transaction_time`, etc.)
  - `transaction_decisions`
  - `rule_configurations`
  - `rule_configuration_history`
  - `audit_logs`
  - Evidência: `backend/src/main/resources/db/migration/V2__core_schema.sql#L6-L205`.

### 2.2 Anti-tamper / idempotência / raw store (V4 + V5)
- Introduz `payload_raw_hash` em `transactions`/`transaction_decisions` e cria `transaction_raw_store`.
  - Evidência: `backend/src/main/resources/db/migration/V4__raw_hash_idempotency.sql#L1-L25`.
- Evolui raw store para guardar **bytes “as received”** (`payload_raw_bytes BYTEA`) e torna `external_transaction_id` a PK.
  - Evidência: `backend/src/main/resources/db/migration/V5__raw_as_received.sql#L1-L44`.

### 2.3 Enterprise / features (V8 + V9)
- Tabelas enterprise para operadores avançados:
  - `feature_store`, `velocity_store`, `graph_edges`, `geo_polygons`, `holidays`, `payload_field_dictionary`, `executions`, `execution_reasons`.
  - Evidência: `backend/src/main/resources/db/migration/V8__enterprise_side_tables.sql#L1-L118`.
- Catálogo de features `feature_definitions` com seeds (PAYLOAD_FIELD/TEMPORAL/VELOCITY/GRAPH/GEO/TEXT/SCHEMA/DERIVED/CONTEXTUAL).
  - Evidência: `backend/src/main/resources/db/migration/V9__feature_definitions.sql#L1-L210`.
- Funções auxiliares de retenção:
  - `clean_old_velocity_data(retention_days)`
  - `clean_expired_features()`
  - Evidência: `backend/src/main/resources/db/migration/V9__feature_definitions.sql#L211-L260`.

### 2.4 Temporal/canônico para velocity (V11)
- Coluna `transaction_ts_utc` em `transactions` e índices para consultas determinísticas.
  - Evidência: `backend/src/main/resources/db/migration/V11__transactions_event_ts_and_velocity_updater.sql#L1-L14`.

### 2.5 Feriados BR 2025 (V12)
- Seed de feriados nacionais e alguns estaduais (UF) em `holidays`.
  - Evidência: `backend/src/main/resources/db/migration/V12__seed_holidays_br_2025.sql#L1-L45`.

## 3) Backend (Java / REST)
### 3.1 Base path
- Controllers montados sem `/api` (context path faz o prefixo).
  - Evidência: `backend/src/main/resources/application.yml#L38-L41`.

### 3.2 Endpoints principais (controllers)
- Transações:
  - `POST /api/transactions/analyze` e `POST /api/transactions/analyze-advanced`.
  - `GET /api/transactions`, `GET /api/transactions/{id}`, `GET /api/transactions/external/{externalId}`.
  - Evidência controller: `backend/src/main/java/com/rulex/controller/TransactionController.java#L26-L126`.
- Regras (CRUD + histórico):
  - Evidência controller: `backend/src/main/java/com/rulex/controller/RuleController.java#L17-L110`.
- Auditoria (logs core):
  - Evidência controller: `backend/src/main/java/com/rulex/controller/AuditController.java#L16-L77`.
- Auditoria V3.1 (execuções):
  - Endpoint read-only “safe-by-default” (não retorna payload, só hashes/decisions).
  - Evidência: `backend/src/main/java/com/rulex/v31/execlog/RuleExecutionLogController.java#L8-L45`.
- Métricas:
  - Evidência: `backend/src/main/java/com/rulex/controller/MetricsController.java#L11-L60`.
- Feature Catalog:
  - Evidência: `backend/src/main/java/com/rulex/v31/feature/FeatureCatalogController.java#L15-L130`.
- Homologação (simulações/rulesets/rules):
  - Evidência (simulação): `backend/src/main/java/com/rulex/controller/homolog/HomologSimulationController.java#L10-L28`.

### 3.3 Segurança (RBAC HTTP Basic)
- Quando `rulex.security.enabled=true`, aplica HTTP Basic com roles `ADMIN` e `ANALYST`.
- Permissões:
  - **Público**: `POST /transactions/analyze`, `POST /transactions/analyze-advanced`, `POST /evaluate`.
  - **Analyst/ADMIN** (read-only): `GET /transactions/**`, `GET /rules/**`, `GET /audit/**`, `GET /metrics/**`, `GET /field-dictionary/**`, `GET /feature-catalog/**`.
  - **ADMIN-only**: `POST /rules/simulate`, `/homolog/**`, `/rules/**` (mutations).
  - Evidência: `backend/src/main/java/com/rulex/config/SecurityConfig.java#L67-L120`.

### 3.4 Captura de payload bruto
- Filtro captura bytes do request body para endpoints críticos.
  - Evidência: `backend/src/main/java/com/rulex/api/RawPayloadCaptureFilter.java#L12-L44`.
- Persistência do raw store (BYTEA) via serviço.
  - Evidência: `backend/src/main/java/com/rulex/service/TransactionRawStoreService.java#L18-L55`.
  - Evidência entidade: `backend/src/main/java/com/rulex/entity/TransactionRawStore.java#L15-L67`.

### 3.5 Mascaramento / minimização
- Utilitário de mascaramento de PAN (primeiros 6 + últimos 4).
  - Evidência: `backend/src/main/java/com/rulex/util/PanMaskingUtil.java#L1-L20`.
- Sanitização no fluxo de homolog (PAN mascarado).
  - Evidência: `backend/src/main/java/com/rulex/homolog/adapter/PayloadSanitizerAdapter.java#L12-L45`.
- Trilha de features usada armazena apenas hash (SHA-256) do valor.
  - Evidência: `backend/src/main/java/com/rulex/v31/trace/FeatureUsed.java#L1-L17` e `backend/src/main/java/com/rulex/v31/trace/FeatureUsageCollector.java#L10-L69`.

## 4) Features (engine + providers)
- Provider DB consulta: `feature_store`, `velocity_store`, `holidays`, `graph_edges`.
  - Evidência: `backend/src/main/java/com/rulex/v31/features/DbFeatureProvider.java#L9-L175`.
- Feriados (determinístico): `isHoliday(country, uf, date)` consulta `holidays`.
  - Evidência: `backend/src/main/java/com/rulex/v31/features/DbFeatureProvider.java#L52-L101`.
- Velocity updater determinístico: agrega `transactions.transaction_ts_utc` e persiste snapshots em `velocity_store`.
  - Evidência: `backend/src/main/java/com/rulex/v31/features/VelocityStoreUpdater.java#L10-L152`.

## 5) OpenAPI (contrato)
- OpenAPI define endpoints com prefixo `/api/*` (compatível com `context-path: /api`).
  - Evidência (exemplos): `openapi/rulex.yaml#L9-L113` e `openapi/rulex.yaml#L329-L398`.

## 6) Frontend (client)
- Rotas do app (wouter): `/transactions`, `/rules`, `/audit`, `/simulator`, `/features`.
  - Evidência: `client/src/App.tsx#L15-L28`.
- Chamadas HTTP do client para o Java API usam `/api/*`.
  - Evidência (exemplos): `client/src/lib/javaApi.ts#L378-L415` e `client/src/pages/Transactions.tsx#L52`.

---

# PASSADA 2 — GAP REGISTER (P0/P1/P2)

> Regra: todo “NÃO ENCONTRADO ...” vira GAP.

| GAP ID | Severidade | Item | Estado | Evidência | Ação recomendada |
|---|---|---|---|---|---|
| G-001 | P0 | Criptografia em repouso para `transaction_raw_store.payload_raw_bytes` | **NÃO ENCONTRADO NO CÓDIGO / BANCO** | Raw store guarda `payload_raw_bytes BYTEA`: `backend/src/main/resources/db/migration/V5__raw_as_received.sql#L6-L15` e entidade: `backend/src/main/java/com/rulex/entity/TransactionRawStore.java#L28-L40`. Uso de `pgcrypto` existe, mas uso de `pgp_*encrypt`/chaves **não aparece nas migrations**: `backend/src/main/resources/db/migration/V1__init.sql#L8`. | Definir política: (a) criptografar campo no app (KMS/Vault) **ou** (b) `pgp_sym_encrypt` com rotação de chave + segregação de acesso; atualizar auditoria/governança. |
| G-002 | P0 | Retenção / purge do raw store (`transaction_raw_store`) | **NÃO ENCONTRADO NO BANCO** | Há funções de retenção para `velocity_store`/`feature_store`: `backend/src/main/resources/db/migration/V9__feature_definitions.sql#L211-L260`. Não há função/política equivalente para `transaction_raw_store` nas migrations lidas (V4/V5). | Criar retenção explícita (por dias ou por volume) + job/operacionalização; documentar LGPD/PCI (base legal/tempo). |
| G-003 | P1 | Endpoints `/api/auth/*` referenciados no client | **NÃO ENCONTRADO NO CÓDIGO / OPENAPI** | Referência no client: `client/src/_core/hooks/useAuth.ts#L20-L57` e `client/src/_core/hooks/useAuth.ts#L155-L171`. Busca por `/api/auth` retorna apenas esse arquivo (não há evidência em backend/openapi). | Remover/encapsular feature se não usada **ou** implementar endpoints e atualizar OpenAPI. |

---

# PASSADA 3 — IMPLEMENTAÇÃO TOTAL + TRACE MATRIX (Feature/Operator → DB → Back → Front → Test → Evidence)

## 3.1 Matriz (alto nível por capability)

| Capability | DB | Back (Controller/Service) | Front | Testes | Evidências |
|---|---|---|---|---|---|
| Analisar transação | `transactions`, `transaction_decisions`, `transaction_raw_store` | `TransactionController` | `javaApi.analyzeTransaction` | `TransactionAnalyzeIT` (existente) | `backend/src/main/java/com/rulex/controller/TransactionController.java#L26-L60`; `backend/src/main/resources/db/migration/V2__core_schema.sql#L6-L111`; `backend/src/main/resources/db/migration/V5__raw_as_received.sql#L6-L44` |
| Analisar transação (advanced) | idem | `TransactionController` → `AdvancedRuleEngineService` | `javaApi.analyzeTransactionAdvanced` | `AdvancedRuleEngineServiceTest` | `backend/src/main/java/com/rulex/controller/TransactionController.java#L120-L168` |
| Catálogo de features | `feature_definitions` | `FeatureCatalogController` | Página `/features` | `FeatureCatalogControllerIT` | `backend/src/main/resources/db/migration/V9__feature_definitions.sql#L1-L80`; `backend/src/main/java/com/rulex/v31/feature/FeatureCatalogController.java#L15-L130`; `backend/src/test/java/com/rulex/v31/feature/FeatureCatalogControllerIT.java#L16-L120` |
| Velocity store (read) | `velocity_store` | `DbFeatureProvider` | (indireto via rules) | `VelocityStoreE2EIT` | `backend/src/main/resources/db/migration/V8__enterprise_side_tables.sql#L42-L60`; `backend/src/main/java/com/rulex/v31/features/DbFeatureProvider.java#L103-L139`; `backend/src/test/java/com/rulex/v31/features/VelocityStoreE2EIT.java#L18-L165` |
| Velocity store (write/updater) | `transactions.transaction_ts_utc`, `velocity_store` | `VelocityStoreUpdater` | NÃO ENCONTRADO NO FRONT | (não há E2E do updater) | `backend/src/main/resources/db/migration/V11__transactions_event_ts_and_velocity_updater.sql#L1-L14`; `backend/src/main/java/com/rulex/v31/features/VelocityStoreUpdater.java#L10-L152` |
| Auditoria (core) | `audit_logs` | `AuditController` | Página `/audit` | (existente) | `backend/src/main/resources/db/migration/V2__core_schema.sql#L151-L205`; `backend/src/main/java/com/rulex/controller/AuditController.java#L16-L77` |
| Auditoria (execuções v3.1) | `executions`, `execution_reasons` | `RuleExecutionLogController` | (front: Audit/console) | `RuleExecutionLogIT` | `backend/src/main/resources/db/migration/V8__enterprise_side_tables.sql#L18-L41`; `backend/src/main/java/com/rulex/v31/execlog/RuleExecutionLogController.java#L8-L45` |
| RBAC | N/A | `SecurityConfig` | N/A | `SecurityRbacTest` | `backend/src/main/java/com/rulex/config/SecurityConfig.java#L67-L120`; `backend/src/test/java/com/rulex/security/SecurityRbacIT.java#L18-L126` |
| Feriados BR | `holidays` | `DbFeatureProvider.isHoliday` | N/A | `EnterpriseFunctionsIT` (existente) | `backend/src/main/resources/db/migration/V8__enterprise_side_tables.sql#L90-L118`; `backend/src/main/resources/db/migration/V12__seed_holidays_br_2025.sql#L1-L45`; `backend/src/main/java/com/rulex/v31/features/DbFeatureProvider.java#L52-L101` |

> Observação: onde aparecer “NÃO ENCONTRADO …” acima, isso deve virar GAP adicional se for requisito do escopo.

---

# PASSADA 4 — PROVA FINAL / HOMOLOGAÇÃO (GO/NO-GO)

## Evidência de execução (focado para prova)
- Comando executado (PowerShell, pasta `backend/`):
  - `mvn "-Dtest=VelocityStoreE2EIT,FeatureCatalogControllerIT,FlywayMigrationsTest,SecurityRbacTest" test *> target\\PASSADA4_focused_test.log`
- Resultado do comando:
  - `LASTEXITCODE=0`
  - Log completo: `backend/target/PASSADA4_focused_test.log`

Trecho (tail) do log, contendo o sumário do Surefire e o status final do Maven:

```
[INFO] Results:
[INFO]
[INFO] Tests run: 30, Failures: 0, Errors: 0, Skipped: 0
[INFO]
[INFO] ------------------------------------------------------------------------
[INFO] BUILD SUCCESS
[INFO] ------------------------------------------------------------------------
```

## Gates (baseados em evidência no repo)
- **Gate A — Contrato (OpenAPI ↔ Controllers)**: GO (há `context-path: /api` e controllers mapeiam `/transactions`, `/rules`, `/audit`, etc.).
  - Evidência: `backend/src/main/resources/application.yml#L38-L41` + `openapi/rulex.yaml#L9-L113` + controllers citados na PASSADA 1.
- **Gate B — Migrations Flyway aplicam**: GO (há teste dedicado).
  - Evidência: `backend/src/test/java/com/rulex/db/FlywayMigrationsIT.java#L1-L55`.
- **Gate C — RBAC mínimo**: GO (há testes de RBAC e config explícita).
  - Evidência: `backend/src/main/java/com/rulex/config/SecurityConfig.java#L67-L120` + `backend/src/test/java/com/rulex/security/SecurityRbacIT.java#L18-L126`.
- **Gate D — Dados sensíveis (raw payload)**: **NO-GO enquanto G-001 e G-002 não forem resolvidos**.
  - Evidência: raw store BYTEA (V5) + ausência de criptografia/retention explícitas conforme GAPs.
- **Gate E — Auth endpoints do front**: GO condicional.
  - Se feature de `/api/auth/*` estiver em uso em produção: **NO-GO** até resolver G-003.
  - Se não estiver em uso: GO após remover referências/feature flag.
  - Evidência: `client/src/_core/hooks/useAuth.ts#L20-L57`.

## Resultado
- Status recomendado: **NO-GO** (existem GAPs P0 abertos: G-001, G-002).

---

## Apêndice: Evidência de testes (mínimo)
- Feature Catalog IT: `backend/src/test/java/com/rulex/v31/feature/FeatureCatalogControllerIT.java#L16-L120`.
- Velocity Store E2E: `backend/src/test/java/com/rulex/v31/features/VelocityStoreE2EIT.java#L18-L165`.
- Flyway migrations: `backend/src/test/java/com/rulex/db/FlywayMigrationsIT.java#L1-L55`.
- RBAC: `backend/src/test/java/com/rulex/security/SecurityRbacIT.java#L18-L126`.
