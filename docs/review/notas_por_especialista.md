# Notas por Especialista — Painel Multidisciplinar (evidência no código)

**Data**: 2025-12-19  
**Projeto**: RULEX — Motor de Regras Duras Bancárias  
**Regra de imparcialidade**: afirmação sem evidência no código = **GAP**

---

## 1) Especialista de Negócio (Crédito/Fraude) — **Nota: 7.2/10**

### Pontos fortes (com evidência)
- **Motor determinístico (sem ML), com regras “duras” e trilha**: `backend/src/main/java/com/rulex/service/RuleEngineService.java`, `backend/src/main/java/com/rulex/service/AdvancedRuleEngineService.java`
- **Classificação final padronizada (APPROVED/SUSPICIOUS/FRAUD)**: `backend/src/main/java/com/rulex/entity/TransactionDecision.java`
- **Popup 1..N regras (agregação por classificação) no endpoint de avaliação**: `backend/src/main/java/com/rulex/dto/EvaluateResponse.java`, `backend/src/main/java/com/rulex/service/RuleEngineService.java` (método `aggregatePopups`)

### Pontos fracos (com evidência)
- **Dois modelos de regras convivendo (core vs homolog)**, aumentando risco de divergência de negócio (mesma “regra” pode existir em dois lugares):
  - Core: `backend/src/main/java/com/rulex/service/RuleEngineService.java` + tabela `rule_configurations` (`backend/src/main/resources/db/migration/V2__core_schema.sql`)
  - Homolog: `backend/src/main/java/com/rulex/homolog/usecase/HomologRuleSetUseCase.java` + tabelas `rule_versions/rule_sets` (`backend/src/main/resources/db/migration/V1__init.sql`)
- **Lista de 60 regras em docs não é prova de implementação** (existe documentação, mas não é evidência de execução em runtime): `REGRAS_DURAS_60_IMPLEMENTACAO.md` (doc)

### Gaps (não encontrado)
- **GAP P1**: payload oficial `fixtures/crtran.json` (referenciado em testes/Insomnia) não existe no repo (ver `backend/src/test/java/com/rulex/controller/CrtranBaselineIT.java`, `Insomnia/README.md`).

### Riscos
- **P1**: divergência operacional (operadores e DSL diferentes entre core e homolog) pode gerar inconsistência de decisão.

---

## 2) Product Owner Técnico — **Nota: 6.0/10**

### Pontos fortes
- **OpenAPI existe e gera cliente**: `openapi/rulex.yaml`, `client/src/lib/api.generated.ts`.
- **Coleção Insomnia para fluxo de HML (Java)**: `Insomnia/rulex-hml.insomnia.json`.

### Pontos fracos
- **OpenAPI não cobre endpoints críticos existentes** (`/api/evaluate`, `/api/homolog/*`, `/api/rules/*history*`, `/api/rules/enabled/*`):
  - Implementação: controllers em `backend/src/main/java/com/rulex/controller/**`
  - Spec: `openapi/rulex.yaml`
- **Docs/scripts de inventário conflitam com o repo atual** (referências a módulos ausentes): `audit/inventory_git_ls_files.txt`.

### Gaps
- **GAP P1**: critérios de aceite formais (BDD/Gherkin) — não encontrado.

### Riscos
- **P1**: homologação guiada por spec incompleta tende a “passar” sem cobrir fluxos reais.

---

## 3) Arquiteto de Software — **Nota: 7.0/10**

### Pontos fortes
- **Separação clara entre camadas e responsabilidades no backend** (controllers/services/repositories): `backend/src/main/java/com/rulex/controller/`, `backend/src/main/java/com/rulex/service/`, `backend/src/main/java/com/rulex/repository/`.
- **Arquitetura do módulo de homolog (ports/adapters/usecase)**: `backend/src/main/java/com/rulex/homolog/`.
- **Teste de arquitetura (ArchUnit) presente**: `backend/src/test/java/com/rulex/architecture/CleanArchitectureRulesTest.java`.

### Pontos fracos
- **Duplicidade de motores de regra** (core vs homolog) sem “single source of truth” (ver seção do Negócio).
- **Contrato de operadores diferente**:
  - Core usa `==`, `IN`, etc: `backend/src/main/java/com/rulex/service/RuleEngineService.java` + `openapi/rulex.yaml`.
  - Homolog usa `EQ`, `GTE`, etc: `backend/src/main/java/com/rulex/homolog/adapter/RuleDslEvaluatorAdapter.java`.

### Gaps
- **GAP P2**: ADRs (Architecture Decision Records) formais — não encontrado.

### Riscos
- **P2**: evolução futura pode quebrar consistência se o time não decidir qual motor é o “oficial”.

---

## 4) UX Designer — **Nota: 6.0/10**

### Pontos fortes
- **Fluxos principais de operação no SPA existem** (regras, transações, auditoria, simulador): `client/src/App.tsx`, `client/src/pages/*`.

### Pontos fracos
- **Simulador “degrada” para simulação local quando API falha** (pode mascarar defeitos reais do backend durante HML): `client/src/pages/TransactionSimulator.tsx` (função `simulateAnalysis`).
- **Dependência de endpoints não existentes no backend pode gerar UX quebrada** (ex.: health/realtime/export): `client/src/lib/javaApi.ts` vs controllers Java.

### Gaps
- **GAP P2**: evidência de testes de usabilidade / métricas de UX — não encontrado.

### Riscos
- **P2**: operador pode interpretar “resultado” (simulado) como real em ambiente de homologação.

---

## 5) UI Designer — **Nota: 6.5/10**

### Pontos fortes
- **Design system e componentes reutilizáveis**: `client/src/components/ui/*`, `DESIGN_SYSTEM.md`.

### Pontos fracos
- **Inconsistência potencial entre telas** (há múltiplas páginas “Professional”, “Didactic”, “Advanced”): `client/src/pages/*`.

### Gaps
- **GAP P3**: tokens formais (cores/tipografia/spacing) como fonte única — não encontrado como artefato isolado.

### Riscos
- **P3**: evolução de UI tende a drift visual sem tokens/Storybook.

---

## 6) Product Designer — **Nota: 6.0/10**

### Pontos fortes
- **Oferta de funcionalidades** (CRUD regras, análise, auditoria, métricas): `client/src/pages/Rules.tsx`, `client/src/pages/TransactionsProfessional.tsx`, `client/src/pages/Audit.tsx`.

### Pontos fracos
- **Funcionalidades “avançadas” podem estar inconsistentes** (RulesAdvanced depende de tRPC e client não evidenciado no repo atual): `client/src/pages/RulesAdvanced.tsx`, `client/src/lib/trpc.ts`.

### Gaps
- **GAP P2**: jornadas, onboarding, treinamento de operadores — não encontrado.

### Riscos
- **P2**: parte do produto pode não ser operável em HML sem ajustes/correções de integração.

---

## 7) Backend Engineer Java — **Nota: 8.2/10**

### Pontos fortes
- **Idempotência por `externalTransactionId` com mitigação de condição de corrida**: `backend/src/main/java/com/rulex/service/RuleEngineService.java`.
- **Regras genéricas configuráveis (conditions_json + operadores) com validação de campos**:
  - Avaliação: `RuleEngineService.evaluateCondition(...)`.
  - Validação de fields: `backend/src/main/java/com/rulex/service/RuleConfigurationService.java`.
- **Máscara de PAN antes de persistir**: `backend/src/main/java/com/rulex/util/PanMaskingUtil.java` e persistência em `RuleEngineService.convertRequestToEntity(...)`.
- **Motor avançado com 28 regras e testes unitários**: `backend/src/main/java/com/rulex/service/AdvancedRuleEngineService.java`, `backend/src/test/java/com/rulex/service/AdvancedRuleEngineServiceTest.java`.

### Pontos fracos
- **Exceções de validação viram “Falha de validação” genérica** (perde detalhes do campo): `backend/src/main/java/com/rulex/api/GlobalExceptionHandler.java`.
- **Persistência de detalhes em TEXT (não JSONB) no core**: `backend/src/main/resources/db/migration/V2__core_schema.sql`.

### Gaps
- **GAP P2**: execução automática de testes `*IT.java` no build Maven padrão (sem Failsafe no `pom.xml`).

### Riscos
- **P2**: diagnósticos de validação podem ficar pobres em HML (dificulta correção de payloads).

---

## 8) Frontend Engineer React — **Nota: 4.5/10**

### Pontos fortes
- **Infra de UI com shadcn/ui e componentes**: `client/src/components/ui/*`.
- **OpenAPI client gerado disponível**: `client/src/lib/api.generated.ts`.

### Pontos fracos
- **Cliente manual `javaApi.ts` está divergente do backend** (tipos e endpoints): `client/src/lib/javaApi.ts`.
- **Dependência de tRPC sem evidência do servidor no repo atual**:
  - Import: `client/src/lib/trpc.ts` (referencia `../../../server/routers`).
  - Uso: `client/src/pages/RulesAdvanced.tsx`.

### Gaps
- **GAP P0**: build/TS check confiável do frontend (há `check: tsc --noEmit` em `package.json`, mas o repo contém referências a módulos ausentes e lockfile inconsistente — ver `docs/review/matriz_gaps_riscos.md`).

### Riscos
- **P0**: pipeline do frontend quebra antes da homologação (instalação e/ou typecheck).

---

## 9) DBA / PostgreSQL — **Nota: 7.2/10**

### Pontos fortes
- **Migrations Flyway estruturadas**: `backend/src/main/resources/db/migration/V1__init.sql`, `V2__core_schema.sql`, `V3__extend_workflow_length.sql`.
- **Constraints e índices relevantes** (unicidade de `external_transaction_id`, checks de enums): `V2__core_schema.sql`.
- **Uso de JSONB no módulo de homolog** (condições, logs): `V1__init.sql` (`conditions_json JSONB`, `decision_log.payload_json JSONB`, etc).

### Pontos fracos
- **Campos JSON em TEXT no core**: `V2__core_schema.sql` (`rules_applied TEXT`, `score_details TEXT`).

### Gaps
- **GAP P2**: política de retenção/expurgo, backup/restore documentado — não encontrado.

### Riscos
- **P2**: crescimento de tabelas (transactions/audit) sem estratégia de retenção.

---

## 10) QA Engineer (Lead) — **Nota: 5.0/10**

### Pontos fortes
- **Testes unitários do motor core e do motor avançado**:
  - `backend/src/test/java/com/rulex/service/RuleEngineServiceTest.java`
  - `backend/src/test/java/com/rulex/service/AdvancedRuleEngineServiceTest.java`
- **Testes de integração existem (E2E API + Postgres/Testcontainers)**: `backend/src/test/java/com/rulex/controller/RulePopupE2EIT.java`, `backend/src/test/java/com/rulex/homolog/HomologSimulationIT.java`.

### Pontos fracos
- **Lockfile inconsistente impede instalação reprodutível de dependências Node**: `pnpm-lock.yaml` vs `package.json` (ver erro `ERR_PNPM_OUTDATED_LOCKFILE`).
- **Evidência de execução automática de ITs não está no `pom.xml`** (sem failsafe).

### Gaps
- **GAP P1**: E2E de navegação SPA (Playwright/Cypress) — não encontrado.
- **GAP P1**: `fixtures/crtran.json` (baseline de payload) — não encontrado.

### Riscos
- **P0**: pipeline do frontend/Node pode falhar antes da suíte de testes (instalação).

---

## 11) AppSec / Segurança (OWASP + LGPD) — **Nota: 4.8/10**

### Pontos fortes
- **Máscara de PAN antes de persistir/auditar**:
  - Core: `backend/src/main/java/com/rulex/util/PanMaskingUtil.java`.
  - Homolog: `backend/src/main/java/com/rulex/homolog/adapter/PayloadSanitizerAdapter.java`.
- **CORS configurável por variável**: `backend/src/main/java/com/rulex/config/CorsConfig.java`.

### Pontos fracos
- **Sem autenticação/autorização no backend Java**: ausência de `spring-boot-starter-security` no `backend/pom.xml` e controllers abertos.
- **Credenciais default no Docker Compose**: `docker-compose.yml` (`POSTGRES_PASSWORD: postgres`).

### Gaps
- **GAP P1**: evidência de SAST/DAST/dep scanning no CI — não encontrado.
- **GAP P2**: política LGPD (retenção/consentimento) implementada no código — não encontrado.

### Riscos
- **P1**: para ambiente bancário, API aberta é vetor de abuso/exfiltração se ambiente não estiver isolado.

---

## 12) DevOps / SRE — **Nota: 5.2/10**

### Pontos fortes
- **Docker Compose com Postgres + backend + web**: `docker-compose.yml`.
- **Backend com Flyway e `ddl-auto: validate`** (bom para governança de schema): `backend/src/main/resources/application.yml`.

### Pontos fracos
- **Lockfile inconsistente quebra “instalação reprodutível”** (P0): `pnpm-lock.yaml`.
- **Ausência de pipeline CI no repo** (sem `.github/workflows`): evidência por inexistência no tree.

### Gaps
- **GAP P1**: CI/CD (build/test/lint) — não encontrado.
- **GAP P2**: observabilidade (Prometheus/Grafana/alerting) — não encontrado.

### Riscos
- **P0**: sem instalação reprodutível, qualquer CI/CD é instável.
