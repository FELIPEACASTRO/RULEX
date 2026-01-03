# ARCHITECTURE_MAP (FASE 0)

Este documento descreve a arquitetura e os fluxos principais do RULEX **com evidência do repositório**. Qualquer afirmação sem evidência é marcada como **VALIDAR**.

## 1) Visão geral

- Backend: Spring Boot (context-path `/api`).
- Frontend: React/Vite (não detalhado aqui — foco FASE 0 backend + contratos).
- Persistência: PostgreSQL + Flyway.
- Dois paradigmas de regras coexistem:
  - **Regras “simples”**: tabela `rule_configurations` com `conditions_json` (engine em `RuleEngineService`).
  - **Regras “complexas”**: grupos aninhados + condições + ações (migrations V8/V12/V18).

**EVIDÊNCIA**
- Context-path `/api`: [backend/src/main/resources/application.yml](../backend/src/main/resources/application.yml#L41-L44)
- `rule_configurations` schema: [backend/src/main/resources/db/migration/V2__core_schema.sql](../backend/src/main/resources/db/migration/V2__core_schema.sql#L84-L140)
- Complex rules schema/enums: [backend/src/main/resources/db/migration/V8__complex_rules_support.sql](../backend/src/main/resources/db/migration/V8__complex_rules_support.sql#L1-L120)
- Complex rules CRUD table: [backend/src/main/resources/db/migration/V12__complex_rules_crud.sql](../backend/src/main/resources/db/migration/V12__complex_rules_crud.sql#L1-L60)

## 2) Segurança e acesso (RBAC)

- Feature-flag: `rulex.security.enabled`.
- Quando `enabled=false`: permite tudo e desliga CSRF.
- Quando `enabled=true`:
  - Auth HTTP Basic.
  - CSRF via cookie `XSRF-TOKEN`, mas com **exceção de CSRF** para endpoints públicos de análise.
  - Regras de autorização:
    - POST público: `/transactions/analyze`, `/transactions/analyze-advanced`, `/evaluate`.
    - GET `/transactions/**`, `/rules/**`, `/audit/**`, `/metrics/**`, `/field-dictionary/**`, `/complex-rules/**`: `ANALYST` ou `ADMIN`.
    - Mutations (e.g. `/rules/**`, `/complex-rules/**`, `/homolog/**`): `ADMIN`.

**EVIDÊNCIA**
- Config e regras de auth/CSRF: [backend/src/main/java/com/rulex/config/SecurityConfig.java](../backend/src/main/java/com/rulex/config/SecurityConfig.java#L18-L134)
- Propriedades `rulex.security.*`: [backend/src/main/java/com/rulex/config/RulexSecurityProperties.java](../backend/src/main/java/com/rulex/config/RulexSecurityProperties.java#L1-L10)
- Defaults em config: [backend/src/main/resources/application.yml](../backend/src/main/resources/application.yml#L86-L103)

## 3) Fluxo de análise “simples” (POST /transactions/analyze)

**Objetivo:** receber `TransactionRequest`, capturar bytes crus do HTTP body, persistir payload raw (para idempotência/anti-tamper), executar regras simples e retornar decisão.

1. Entrada: `POST /api/transactions/analyze`.
2. Captura de bytes crus: lê atributo `RawPayloadCaptureFilter.RAW_BYTES_ATTR` do request.
3. Engine:
   - Calcula hash SHA-256 dos bytes crus.
   - Anti-tamper: se já existir `externalTransactionId` com hash diferente, retorna **FRAUD**.
   - Idempotência: se já existir com mesmo hash, retorna decisão anterior.
   - Caso novo: persiste raw store e `transactions`, avalia regras, salva decisão.

**EVIDÊNCIA**
- Endpoint e captura raw bytes: [backend/src/main/java/com/rulex/controller/TransactionController.java](../backend/src/main/java/com/rulex/controller/TransactionController.java#L32-L49)
- Engine: hash + raw store + idempotência + anti-tamper: [backend/src/main/java/com/rulex/service/RuleEngineService.java](../backend/src/main/java/com/rulex/service/RuleEngineService.java#L88-L170)

## 4) Endpoint “evaluate raw” (POST /evaluate)

**Objetivo:** avaliar uma requisição “raw” e retornar decisão final + hits/popups agregados.

1. Entrada: `POST /api/evaluate`.
2. Lê `rawBody` e bytes crus capturados por `RawPayloadCaptureFilter`.
3. Delegação para `ruleEngineService.evaluateRaw(rawBody, rawBytes, contentType)`.

**EVIDÊNCIA**
- Controller: [backend/src/main/java/com/rulex/controller/EvaluateController.java](../backend/src/main/java/com/rulex/controller/EvaluateController.java#L1-L31)

## 5) Regras simples: separação Decision vs Shadow/Canary + pre-checks

- Pre-checks determinísticos (curto-circuito): bloom-filter blacklist em PAN/Merchant/Customer/Device (terminalId).
- Shadow mode:
  - `shadowModeEnabled` flag.
  - Regras podem ser `DISABLED` (normal), `SHADOW` (avaliar async) ou `CANARY` (percentual selecionado; resto vira shadow).
  - Regras shadow/canary (não selecionadas) são avaliadas **assíncronas** e não alteram decisão.

**EVIDÊNCIA**
- Pipeline: pre-checks + separação decision/shadow + async: [backend/src/main/java/com/rulex/service/RuleEngineService.java](../backend/src/main/java/com/rulex/service/RuleEngineService.java#L480-L705)

## 6) CRUD e governança (regras simples)

- CRUD básico: `/api/rules` (listar, obter, criar, atualizar, deletar, toggle, enabled list, history).
- Workflow “4 olhos” (aprovação): `/api/rules/approvals/*`.

**EVIDÊNCIA**
- CRUD: [backend/src/main/java/com/rulex/controller/RuleController.java](../backend/src/main/java/com/rulex/controller/RuleController.java#L1-L120)
- Aprovações: [backend/src/main/java/com/rulex/controller/RuleApprovalController.java](../backend/src/main/java/com/rulex/controller/RuleApprovalController.java#L1-L140)

## 7) Export/Import (v1)

- Existe um controller com `@RequestMapping("/api/v1/rules/export-import")`.
- Atenção: como o app tem context-path `/api`, o path final fica `/api/api/v1/rules/export-import/...` **a menos** que exista alguma configuração especial (NÃO ENCONTREI EVIDÊNCIA de override).

**EVIDÊNCIA**
- Context-path: [backend/src/main/resources/application.yml](../backend/src/main/resources/application.yml#L41-L44)
- Controller: [backend/src/main/java/com/rulex/controller/RuleExportImportController.java](../backend/src/main/java/com/rulex/controller/RuleExportImportController.java#L1-L30)

## 8) Observabilidade: auditoria e métricas

- Auditoria (consulta e export): `/api/audit`, `/api/audit/export`, `/api/audit/transaction/{transactionId}`.
- Métricas gerais: `/api/metrics`, `/api/metrics/mcc`, `/api/metrics/merchant`, `/api/metrics/timeline`.
- Métricas de regras + feedback FP/TP: `/api/rules/metrics/*`.

**EVIDÊNCIA**
- AuditController: [backend/src/main/java/com/rulex/controller/AuditController.java](../backend/src/main/java/com/rulex/controller/AuditController.java#L1-L130)
- MetricsController: [backend/src/main/java/com/rulex/controller/MetricsController.java](../backend/src/main/java/com/rulex/controller/MetricsController.java#L1-L70)
- RuleMetricsController: [backend/src/main/java/com/rulex/controller/RuleMetricsController.java](../backend/src/main/java/com/rulex/controller/RuleMetricsController.java#L1-L120)

## 9) Logs de execução (append-only)

- Existe `rule_execution_log` (Flyway V6) para registrar eventos `EVALUATE|SIMULATE|ANTI_TAMPER`.
- Implementação best-effort: ignora violação de índice (dedupe).

**EVIDÊNCIA**
- Schema: [backend/src/main/resources/db/migration/V6__v31_exec_log_field_dictionary.sql](../backend/src/main/resources/db/migration/V6__v31_exec_log_field_dictionary.sql#L52-L120)
- Service: [backend/src/main/java/com/rulex/v31/execlog/RuleExecutionLogService.java](../backend/src/main/java/com/rulex/v31/execlog/RuleExecutionLogService.java#L1-L120)

## 10) Nuances / inconsistências observadas (para FASE 0)

1) Shadow mode “legado” em tabela `rules` (V20) vs shadow mode operacional em `rule_configurations` (engine usa `RuleConfiguration.getShadowMode()`).
- Isso sugere evolução de schema/entidades (VALIDAR se `rules` ainda é usado em runtime).

**EVIDÊNCIA**
- V20 altera tabela `rules`: [backend/src/main/resources/db/migration/V20__shadow_mode_and_device_fingerprinting.sql](../backend/src/main/resources/db/migration/V20__shadow_mode_and_device_fingerprinting.sql#L1-L20)
- Engine usa `rule.getShadowMode()` em `RuleConfiguration`: [backend/src/main/java/com/rulex/service/RuleEngineService.java](../backend/src/main/java/com/rulex/service/RuleEngineService.java#L520-L610)
