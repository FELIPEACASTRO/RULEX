# RULE_ENGINE_CAPABILITIES (FASE 0)

Este documento descreve **o que o engine suporta** (regras simples e complexas) com evidência no repositório.

## 1) Regras simples (rule_configurations + conditions_json)

### 1.1 Execução e governança

- As regras simples são carregadas de `RuleConfigurationRepository.findByEnabled(true)` ou por ordenação otimizada.
- Existe um índice de pré-condições (required fields) para permitir “skip” conservador em regras AND quando faltam campos.

**EVIDÊNCIA**
- Execução + regra otimizada vs normal: [backend/src/main/java/com/rulex/service/RuleEngineService.java](../backend/src/main/java/com/rulex/service/RuleEngineService.java#L486-L515)
- CandidateIndex (skip conservador): [backend/src/main/java/com/rulex/service/RuleEngineService.java](../backend/src/main/java/com/rulex/service/RuleEngineService.java#L1520-L1620)

### 1.2 Operadores suportados (regras simples)

O método `evaluateCondition` suporta:

**Operadores unários**
- `IS_NULL`, `IS_NOT_NULL`, `IS_TRUE`, `IS_FALSE`

**Numéricos**
- `EQ`, `NE`, `GT`, `LT`, `GTE`, `LTE`, `IN`, `NOT_IN`, `BETWEEN`, `NOT_BETWEEN`

**Strings**
- `EQ`, `NE`, `CONTAINS`, `NOT_CONTAINS`, `STARTS_WITH`, `ENDS_WITH`, `MATCHES_REGEX`, `IN`, `NOT_IN`

**Compatibilidade de operadores simbólicos**
- `==`, `!=`, `>`, `<`, `>=`, `<=` são normalizados.

**EVIDÊNCIA**
- Unários + numéricos + strings + normalização: [backend/src/main/java/com/rulex/service/RuleEngineService.java](../backend/src/main/java/com/rulex/service/RuleEngineService.java#L1638-L1760)

### 1.3 Funções suportadas em “left expressions” (regras simples)

O engine reconhece algumas expressões no campo (lado esquerdo), extraindo dependências e aplicando funções:
- `ABS(x)`
- `LEN(x)`
- `LOWER(x)`
- `UPPER(x)`
- `TRIM(x)`
- `ABS_DIFF(a,b)`
- `COALESCE(field, default)` (dependências)

**EVIDÊNCIA**
- Extração de dependências (unary/abs/abs_diff/coalesce): [backend/src/main/java/com/rulex/service/RuleEngineService.java](../backend/src/main/java/com/rulex/service/RuleEngineService.java#L1588-L1660)
- Parsing e execução de computed left values: [backend/src/main/java/com/rulex/service/RuleEngineService.java](../backend/src/main/java/com/rulex/service/RuleEngineService.java#L1820-L1870)

### 1.4 Regex (regras simples): implementação atual vs hardening disponível

- `MATCHES_REGEX` no engine compila `Pattern.compile(rawRegex)` e faz `.find()`.
- Existe um utilitário `RegexValidator` com proteções anti-ReDoS (tamanho/denylist/complexidade/grupos/timeout) mas **não há evidência no trecho lido** de que o engine o utilize.

**EVIDÊNCIA**
- Engine regex atual: [backend/src/main/java/com/rulex/service/RuleEngineService.java](../backend/src/main/java/com/rulex/service/RuleEngineService.java#L1740-L1760)
- Regex hardening util: [backend/src/main/java/com/rulex/util/RegexValidator.java](../backend/src/main/java/com/rulex/util/RegexValidator.java#L1-L120)

**GAP (FASE 0)**
- Validar/decidir: conectar `MATCHES_REGEX` ao `RegexValidator.safeCompile/matchWithTimeout`.

## 2) Recursos “V4.0” em regras simples

### 2.1 Bloom filter blacklist (pre-check determinístico)

Antes de avaliar regras, o engine executa pre-checks determinísticos com bloom filter:
- PAN
- merchantId
- customerIdFromHeader
- deviceId (terminalId)

Se encontrado, retorna imediatamente `FRAUD`.

**EVIDÊNCIA**
- Pre-check bloom + curto-circuito: [backend/src/main/java/com/rulex/service/RuleEngineService.java](../backend/src/main/java/com/rulex/service/RuleEngineService.java#L570-L705)

### 2.2 Shadow mode e canary

- Regras podem ser:
  - `DISABLED` (normal)
  - `SHADOW` (avaliar async, não impacta decisão)
  - `CANARY` (percentual entra como decision, resto shadow)

**EVIDÊNCIA**
- Separação decision/shadow/canary + async: [backend/src/main/java/com/rulex/service/RuleEngineService.java](../backend/src/main/java/com/rulex/service/RuleEngineService.java#L520-L650)

## 3) Regras complexas (DB-first)

O suporte a regras complexas é definido por migrations com:
- `rule_condition_groups` com lógica aninhada e operadores (`AND/OR/NOT/XOR/NAND/NOR`).
- `rule_conditions` com enum `condition_operator` amplo.
- `rule_actions` com enum `rule_action_type`.
- `complex_rules` como tabela principal de metadados.
- Constraint para garantir vínculo: grupo pertence a `rule_version_id` ou `complex_rule_id`.

**EVIDÊNCIA**
- Enums e tabelas base: [backend/src/main/resources/db/migration/V8__complex_rules_support.sql](../backend/src/main/resources/db/migration/V8__complex_rules_support.sql#L1-L190)
- CRUD `complex_rules` e FK/nullable: [backend/src/main/resources/db/migration/V12__complex_rules_crud.sql](../backend/src/main/resources/db/migration/V12__complex_rules_crud.sql#L1-L60)
- Constraint “has parent”: [backend/src/main/resources/db/migration/V18__enable_condition_groups_constraint.sql](../backend/src/main/resources/db/migration/V18__enable_condition_groups_constraint.sql#L1-L55)

### 3.1 Operadores de velocity (complex rules)

O enum `condition_operator` inclui operadores `VELOCITY_*` (COUNT/SUM/AVG/DISTINCT com GT/LT).

**EVIDÊNCIA**
- Migration adicionando VELOCITY ops: [backend/src/main/resources/db/migration/V15__add_velocity_operators.sql](../backend/src/main/resources/db/migration/V15__add_velocity_operators.sql#L1-L15)

## 4) Velocidade (storage)

Há schema para contadores e log de transações para agregações:
- `velocity_counters`
- `velocity_transaction_log`
- função `get_velocity_stats`

**EVIDÊNCIA**
- Migration velocity: [backend/src/main/resources/db/migration/V14__velocity_counters.sql](../backend/src/main/resources/db/migration/V14__velocity_counters.sql#L1-L120)

## 5) Logs de execução (append-only)

- `rule_execution_log` registra `EVALUATE` e `ANTI_TAMPER` em transação nova (REQUIRES_NEW), com dedupe via índice.

**EVIDÊNCIA**
- Schema execution log: [backend/src/main/resources/db/migration/V6__v31_exec_log_field_dictionary.sql](../backend/src/main/resources/db/migration/V6__v31_exec_log_field_dictionary.sql#L52-L120)
- Service: [backend/src/main/java/com/rulex/v31/execlog/RuleExecutionLogService.java](../backend/src/main/java/com/rulex/v31/execlog/RuleExecutionLogService.java#L1-L110)
