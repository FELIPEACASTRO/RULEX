# DB_SCHEMA_RULES (FASE 0)

Objetivo: mapear as tabelas/enums/migrations que fundamentam regras, execução e governança.

## 1) Core transacional

### 1.1 transactions

Armazena os campos principais de transação, com `external_transaction_id` único.

**EVIDÊNCIA**
- Tabela e constraints/índices: [backend/src/main/resources/db/migration/V2__core_schema.sql](../backend/src/main/resources/db/migration/V2__core_schema.sql#L9-L56)

### 1.2 transaction_decisions

Decisão ligada a `transactions(id)` com classificação e score.

**EVIDÊNCIA**
- Tabela e check constraints: [backend/src/main/resources/db/migration/V2__core_schema.sql](../backend/src/main/resources/db/migration/V2__core_schema.sql#L60-L116)

## 2) Regras simples (rule_configurations)

- Configuração de regra (habilitada, classificação, weight, threshold, conditions_json).
- Histórico append-only em `rule_configuration_history`.

**EVIDÊNCIA**
- `rule_configurations`: [backend/src/main/resources/db/migration/V2__core_schema.sql](../backend/src/main/resources/db/migration/V2__core_schema.sql#L84-L140)
- `rule_configuration_history`: [backend/src/main/resources/db/migration/V2__core_schema.sql](../backend/src/main/resources/db/migration/V2__core_schema.sql#L142-L168)

## 3) Auditoria

- `audit_logs` (append-only) com action_type/result/source_ip/etc.

**EVIDÊNCIA**
- `audit_logs` schema: [backend/src/main/resources/db/migration/V2__core_schema.sql](../backend/src/main/resources/db/migration/V2__core_schema.sql#L171-L240)

## 4) Field dictionary + execution log (V3.1)

### 4.1 field_dictionary

Catálogo de campos (json_path, data_type, domínios, operadores, funções, constraints).

**EVIDÊNCIA**
- Schema: [backend/src/main/resources/db/migration/V6__v31_exec_log_field_dictionary.sql](../backend/src/main/resources/db/migration/V6__v31_exec_log_field_dictionary.sql#L18-L51)

### 4.2 rule_execution_log

Log append-only com:
- `event_type` (`EVALUATE|SIMULATE|ANTI_TAMPER`)
- `payload_raw_hash` e `attempted_payload_hash`
- `decision` e `risk_score`
- JSONBs de fired/path/why-not/context/error

**EVIDÊNCIA**
- Schema + índice de dedupe: [backend/src/main/resources/db/migration/V6__v31_exec_log_field_dictionary.sql](../backend/src/main/resources/db/migration/V6__v31_exec_log_field_dictionary.sql#L52-L120)

## 5) Regras complexas

### 5.1 Grupos e condições

- `rule_condition_groups` suporta nesting e lógica por enum `group_logic_operator`.
- `rule_conditions` usa enum `condition_operator` (muito mais amplo que regras simples).

**EVIDÊNCIA**
- Enums e tabelas: [backend/src/main/resources/db/migration/V8__complex_rules_support.sql](../backend/src/main/resources/db/migration/V8__complex_rules_support.sql#L1-L120)

### 5.2 Ações e templates

- `rule_actions` com enum `rule_action_type`.
- `rule_templates` com `template_config` JSONB.

**EVIDÊNCIA**
- Ações/templates: [backend/src/main/resources/db/migration/V8__complex_rules_support.sql](../backend/src/main/resources/db/migration/V8__complex_rules_support.sql#L120-L220)

### 5.3 Tabela principal complex_rules + vínculo dos grupos

- `complex_rules` guarda metadados e enable/status/decision.
- `rule_condition_groups` ganha `complex_rule_id` e `rule_version_id` vira nullable.
- Constraint posterior garante que grupos pertencem a alguma regra.

**EVIDÊNCIA**
- `complex_rules` + alterações de FK: [backend/src/main/resources/db/migration/V12__complex_rules_crud.sql](../backend/src/main/resources/db/migration/V12__complex_rules_crud.sql#L1-L80)
- Constraint “has parent”: [backend/src/main/resources/db/migration/V18__enable_condition_groups_constraint.sql](../backend/src/main/resources/db/migration/V18__enable_condition_groups_constraint.sql#L1-L55)

## 6) Velocidade

- Contadores e logs: `velocity_counters`, `velocity_transaction_log` + função `get_velocity_stats`.

**EVIDÊNCIA**
- Migration: [backend/src/main/resources/db/migration/V14__velocity_counters.sql](../backend/src/main/resources/db/migration/V14__velocity_counters.sql#L1-L120)
- Operadores velocity no enum: [backend/src/main/resources/db/migration/V15__add_velocity_operators.sql](../backend/src/main/resources/db/migration/V15__add_velocity_operators.sql#L1-L15)

## 7) Shadow mode / device fingerprinting (legado)

Há migração que altera tabela `rules` e cria `shadow_evaluation_log` e outras estruturas (device fingerprinting, impossible travel tracking etc.).

**EVIDÊNCIA**
- Alterações em `rules` (shadow columns): [backend/src/main/resources/db/migration/V20__shadow_mode_and_device_fingerprinting.sql](../backend/src/main/resources/db/migration/V20__shadow_mode_and_device_fingerprinting.sql#L1-L20)
- Shadow evaluation log: [backend/src/main/resources/db/migration/V20__shadow_mode_and_device_fingerprinting.sql](../backend/src/main/resources/db/migration/V20__shadow_mode_and_device_fingerprinting.sql#L21-L65)

**VALIDAR**
- Se a tabela `rules` ainda é usada em runtime (há evidência de `rule_configurations` para regras simples).
