# Matriz de Gaps e Riscos (baseada em evidência no código)

**Data**: 2025-12-19  
**Projeto**: RULEX — Motor de Regras Duras Bancárias  
**Escopo avaliado**: código em `/workspace` (backend Java + frontend React + docs/scripts)

---

## Classificação

- **P0**: bloqueador de homologação (quebra build, impede executar fluxos P0, ou cria risco crítico imediato)
- **P1**: alto risco (homologável apenas com mitigação/contorno formal)
- **P2**: médio risco
- **P3**: baixo risco

---

## Gaps (não encontrado no código = GAP)

### Gaps P0

| ID | GAP | Evidência (arquivo/caminho) | Impacto |
|---|---|---|---|
| G-001 | **Lockfile inconsistente — `pnpm install --frozen-lockfile` falha** | Evidência de execução: `pnpm -C /workspace install --frozen-lockfile` retornou `ERR_PNPM_OUTDATED_LOCKFILE` (lock não bate com `package.json`). Arquivos: `pnpm-lock.yaml`, `package.json` | CI/instalação reprodutível quebra; inviabiliza pipeline confiável para HML |
| G-002 | **Divergência grave entre “inventários” e código real (docs/scripts citam `server/` e `drizzle/` que não existem no repo atual)** | `audit/inventory_git_ls_files.txt` lista `server/*` e `drizzle/*`, mas diretórios **não existem** no FS (`/workspace/server` e `/workspace/drizzle` não existem). Além disso, `client/src/lib/trpc.ts` importa tipo de `../../../server/routers` | Risco de build quebrar / artefatos inconsistentes; confunde homologação e rastreabilidade |

### Gaps P1

| ID | GAP | Evidência (arquivo/caminho) | Impacto |
|---|---|---|---|
| G-003 | **OpenAPI incompleta vs endpoints reais** | Spec em `openapi/rulex.yaml` **não inclui** `/api/evaluate` e **não inclui** `/api/homolog/*`; também não descreve `/api/rules/enabled/{enabled}` e `/api/rules/{id}/history` | Cliente/contrato de HML incompleto; dificulta homologação e automação |
| G-004 | **Integração/contrato Frontend↔Backend inconsistente em pelo menos um cliente** | `client/src/lib/javaApi.ts` define tipos e endpoints que não batem com o backend (ex.: `TransactionResponse.totalScore` vs backend usa `riskScore`; endpoints `/api/health`, `/api/metrics/realtime`, `/api/audit/export` não existem nos controllers Java) | Fluxos UI podem quebrar ou exigir mocks/“simulação local” |
| G-005 | **`crtran.json` como base de testes/hml é referenciado mas não existe no repo** | `backend/src/test/java/com/rulex/controller/CrtranBaselineIT.java` procura `fixtures/crtran.json` e `Insomnia/README.md` referencia `fixtures/crtran.json`. `Glob **/fixtures/crtran.json` não encontrou arquivo | Baseline de payload não reprodutível; homologação manual perde “fonte de verdade” |
| G-006 | **Testes de integração (IT/E2E) existem, mas não há evidência de execução automática no build padrão Maven** | Há testes `*IT.java` (ex.: `backend/src/test/java/com/rulex/controller/RulePopupE2EIT.java`), mas `backend/pom.xml` não contém `maven-failsafe-plugin`; `mvn test` roda surefire (por padrão) | Cobertura de integração pode ficar “morta” no CI |

### Gaps P2

| ID | GAP | Evidência (arquivo/caminho) | Impacto |
|---|---|---|---|
| G-007 | **Ausência de autenticação/autorização no backend Java (API aberta)** | `backend/pom.xml` não inclui `spring-boot-starter-security`; controllers não exigem auth (ex.: `backend/src/main/java/com/rulex/controller/*`) | Para domínio bancário: risco alto em ambientes não isolados (LGPD/abuso) |
| G-008 | **Dois modelos de “motor de regras” coexistindo (core vs homolog) sem contrato único** | Core: `RuleEngineService` + `rule_configurations` (`V2__core_schema.sql`). Homolog: `HomologRuleSetUseCase` + `rule_versions/rule_sets` (`V1__init.sql`) com DSL e operadores diferentes | Complexidade, risco de divergência funcional e duplicidade de esforço |
| G-009 | **Scripts de validação desatualizados em relação ao payload real** | `validate-rules.mjs` lista campos como `cvv2EntryLimitExceeded`, `pinEntryLimitExceeded` etc, que **não existem** em `backend/src/main/java/com/rulex/dto/TransactionRequest.java` | Ferramentas internas podem gerar falsa confiança |

---

## Riscos (com severidade)

### Riscos P0

| ID | Risco | Evidência | Mitigação recomendada |
|---|---|---|---|
| R-001 | **Homologação com pipeline não reprodutível (Node/Front)** | `pnpm-lock.yaml` não sincroniza com `package.json` (ver G-001) | Corrigir lockfile e exigir `--frozen-lockfile` no CI |

### Riscos P1

| ID | Risco | Evidência | Mitigação recomendada |
|---|---|---|---|
| R-002 | **Homologação de UI pode não refletir backend real (tipos/endpoints divergentes)** | `client/src/lib/javaApi.ts` e `client/src/pages/TransactionSimulator.tsx` têm contrato divergente do backend | Padronizar consumo via `client/src/lib/api.generated.ts` (OpenAPI) e ajustar endpoints reais |
| R-003 | **Exposição indevida de APIs em ambiente não controlado** | Ausência de Spring Security; CORS amplo em `backend/src/main/java/com/rulex/config/CorsConfig.java` | Autenticação/RBAC mínima para HML e segregação de rede |
| R-004 | **Integração de testes de integração pode ficar “silenciosa” no CI** | `*IT.java` sem evidência de execução automática | Adicionar Failsafe (IT) e pipeline que rode `verify` |

### Riscos P2

| ID | Risco | Evidência | Mitigação recomendada |
|---|---|---|---|
| R-005 | **Divergência entre OpenAPI e implementação ao longo do tempo** | OpenAPI não cobre `/evaluate` e `/homolog/*` | Atualizar spec e regenerar `api.generated.ts` |
| R-006 | **Persistência de JSON em TEXT em parte do core** | `transaction_decisions.rules_applied TEXT` e `score_details TEXT` em `V2__core_schema.sql` | Considerar JSONB/estruturar consultas futuras (ou manter e documentar tradeoff) |

---

## Top 3 Gaps (por impacto)

1. **G-001 (P0)**: lockfile inconsistente (`pnpm install --frozen-lockfile` falha).
2. **G-002 (P0)**: inventários/scripts apontam para módulos não existentes (`server/`, `drizzle/`), e há referência de import no client.
3. **G-003 (P1)**: OpenAPI não cobre endpoints críticos (`/evaluate`, `/homolog/*`).

## Top 3 Riscos

1. **R-001 (P0)**: pipeline não reprodutível para frontend/Node.
2. **R-002 (P1)**: UI e “clientes” com contrato divergente podem mascarar defeitos.
3. **R-003 (P1)**: ausência de autenticação/autorização para um domínio bancário.
