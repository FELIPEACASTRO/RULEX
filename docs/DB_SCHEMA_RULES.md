# RULEX - Schema do Banco de Dados (Regras)

**Versão:** 1.0.0  
**Data:** 2025-01-03  
**Banco:** PostgreSQL 16.x  
**Migrations:** Flyway (V1-V22)

---

## 1. Visão Geral

Este documento descreve as tabelas relacionadas ao motor de regras do RULEX.

---

## 2. Tipos Enumerados (ENUMs)

### 2.1 Decisões
```sql
CREATE TYPE decision_outcome AS ENUM ('APROVADO', 'SUSPEITA_DE_FRAUDE', 'FRAUDE');
```

### 2.2 Status de Regra
```sql
CREATE TYPE rule_status AS ENUM ('DRAFT', 'PUBLISHED', 'DEPRECATED');
```

### 2.3 Operadores Lógicos
```sql
CREATE TYPE logic_operator AS ENUM ('AND', 'OR');
CREATE TYPE group_logic_operator AS ENUM ('AND', 'OR', 'NOT', 'XOR', 'NAND', 'NOR');
```

### 2.4 Operadores de Condição
```sql
CREATE TYPE condition_operator AS ENUM (
  -- Comparação básica
  'EQ', 'NEQ', 'GT', 'GTE', 'LT', 'LTE',
  -- Listas
  'IN', 'NOT_IN',
  -- Strings
  'CONTAINS', 'NOT_CONTAINS', 'STARTS_WITH', 'ENDS_WITH', 'REGEX', 'NOT_REGEX',
  -- Nulos
  'IS_NULL', 'NOT_NULL',
  -- Booleanos
  'IS_TRUE', 'IS_FALSE',
  -- Range
  'BETWEEN', 'NOT_BETWEEN',
  -- Comparação entre campos
  'FIELD_EQ', 'FIELD_NEQ', 'FIELD_GT', 'FIELD_GTE', 'FIELD_LT', 'FIELD_LTE',
  -- Data/Tempo
  'DATE_BEFORE', 'DATE_AFTER', 'DATE_BETWEEN', 'TIME_BEFORE', 'TIME_AFTER', 'TIME_BETWEEN',
  -- Array
  'ARRAY_CONTAINS', 'ARRAY_NOT_CONTAINS', 'ARRAY_SIZE_EQ', 'ARRAY_SIZE_GT', 'ARRAY_SIZE_LT',
  -- Matemáticas
  'MOD_EQ', 'MOD_NEQ',
  -- Geolocalização
  'GEO_DISTANCE_LT', 'GEO_DISTANCE_GT', 'GEO_IN_POLYGON',
  -- Velocity
  'VELOCITY_COUNT_GT', 'VELOCITY_COUNT_LT', 'VELOCITY_SUM_GT', 'VELOCITY_SUM_LT',
  'VELOCITY_AVG_GT', 'VELOCITY_AVG_LT', 'VELOCITY_DISTINCT_GT', 'VELOCITY_DISTINCT_LT'
);
```

### 2.5 Tipos de Valor
```sql
CREATE TYPE condition_value_type AS ENUM (
  'STRING', 'NUMBER', 'BOOLEAN', 'DATE', 'TIME', 'DATETIME', 
  'ARRAY_STRING', 'ARRAY_NUMBER', 'FIELD_REFERENCE', 'EXPRESSION', 
  'GEO_POINT', 'GEO_POLYGON'
);
```

### 2.6 Tipos de Ação
```sql
CREATE TYPE rule_action_type AS ENUM (
  'SET_DECISION', 'SET_SCORE', 'ADD_TAG', 'REMOVE_TAG', 
  'SET_VARIABLE', 'CALL_WEBHOOK', 'SEND_NOTIFICATION',
  'BLOCK_TRANSACTION', 'FLAG_FOR_REVIEW', 'ESCALATE'
);
```

---

## 3. Tabelas de Regras Simples

### 3.1 rule_configurations
```sql
CREATE TABLE rule_configurations (
  id BIGSERIAL PRIMARY KEY,
  rule_name VARCHAR(100) NOT NULL UNIQUE,
  description TEXT,
  rule_type VARCHAR(20) NOT NULL CHECK (rule_type IN ('SECURITY','CONTEXT','VELOCITY','ANOMALY')),
  threshold INTEGER NOT NULL,
  weight INTEGER NOT NULL CHECK (weight >= 0 AND weight <= 100),
  enabled BOOLEAN NOT NULL DEFAULT TRUE,
  classification VARCHAR(20) NOT NULL CHECK (classification IN ('APPROVED','SUSPICIOUS','FRAUD')),
  parameters TEXT,
  conditions_json JSONB,
  logic_operator VARCHAR(3) NOT NULL DEFAULT 'AND' CHECK (logic_operator IN ('AND','OR')),
  shadow_mode VARCHAR(20) DEFAULT 'DISABLED',
  canary_percentage INTEGER DEFAULT 0,
  version INTEGER NOT NULL DEFAULT 1,
  created_at TIMESTAMP NOT NULL DEFAULT now(),
  updated_at TIMESTAMP NOT NULL DEFAULT now()
);
```

**Índices:**
- `idx_rule_name` ON (rule_name)
- `idx_enabled` ON (enabled)

### 3.2 rule_configuration_history
```sql
CREATE TABLE rule_configuration_history (
  id BIGSERIAL PRIMARY KEY,
  rule_id BIGINT NOT NULL,
  rule_name VARCHAR(100) NOT NULL,
  version INTEGER NOT NULL,
  previous_json TEXT,
  current_json TEXT,
  performed_by VARCHAR(100),
  created_at TIMESTAMP NOT NULL DEFAULT now()
);
```

---

## 4. Tabelas de Regras Complexas

### 4.1 complex_rules (Header)
```sql
CREATE TABLE complex_rules (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  key TEXT NOT NULL UNIQUE,
  title TEXT NOT NULL,
  description TEXT,
  version INTEGER NOT NULL DEFAULT 1,
  status rule_status NOT NULL DEFAULT 'DRAFT',
  priority INTEGER NOT NULL DEFAULT 0,
  severity INTEGER NOT NULL CHECK (severity >= 0 AND severity <= 100),
  decision decision_outcome NOT NULL,
  reason_template TEXT,
  enabled BOOLEAN NOT NULL DEFAULT TRUE,
  created_by UUID REFERENCES users(id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
```

### 4.2 rule_condition_groups (Grupos Aninhados)
```sql
CREATE TABLE rule_condition_groups (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  rule_version_id UUID NOT NULL REFERENCES rule_versions(id) ON DELETE CASCADE,
  parent_group_id UUID REFERENCES rule_condition_groups(id) ON DELETE CASCADE,
  logic_operator group_logic_operator NOT NULL DEFAULT 'AND',
  position INT NOT NULL DEFAULT 0,
  name TEXT,
  description TEXT,
  enabled BOOLEAN NOT NULL DEFAULT TRUE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  
  CONSTRAINT chk_no_self_reference CHECK (id != parent_group_id)
);
```

**Trigger para evitar loops:**
```sql
CREATE TRIGGER trg_check_group_hierarchy
BEFORE INSERT OR UPDATE ON rule_condition_groups
FOR EACH ROW EXECUTE FUNCTION check_group_hierarchy();
```

### 4.3 rule_conditions (Condições Individuais)
```sql
CREATE TABLE rule_conditions (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  group_id UUID NOT NULL REFERENCES rule_condition_groups(id) ON DELETE CASCADE,
  position INT NOT NULL DEFAULT 0,
  
  -- Campo a ser avaliado
  field_name TEXT NOT NULL,
  field_path TEXT,
  
  -- Operador
  operator condition_operator NOT NULL,
  
  -- Valores
  value_type condition_value_type NOT NULL DEFAULT 'STRING',
  value_single TEXT,
  value_array TEXT[],
  value_min TEXT,
  value_max TEXT,
  value_field_ref TEXT,
  value_expression TEXT,
  
  -- Configurações
  case_sensitive BOOLEAN NOT NULL DEFAULT TRUE,
  negate BOOLEAN NOT NULL DEFAULT FALSE,
  enabled BOOLEAN NOT NULL DEFAULT TRUE,
  
  -- Metadados
  description TEXT,
  error_message TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
```

### 4.4 rule_expressions (Expressões Calculadas)
```sql
CREATE TABLE rule_expressions (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  rule_version_id UUID NOT NULL REFERENCES rule_versions(id) ON DELETE CASCADE,
  name TEXT NOT NULL,
  expression TEXT NOT NULL,
  result_type condition_value_type NOT NULL DEFAULT 'NUMBER',
  description TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  
  UNIQUE(rule_version_id, name)
);
```

### 4.5 rule_context_variables (Variáveis de Contexto)
```sql
CREATE TABLE rule_context_variables (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  rule_version_id UUID NOT NULL REFERENCES rule_versions(id) ON DELETE CASCADE,
  name TEXT NOT NULL,
  source_type TEXT NOT NULL, -- 'PAYLOAD', 'EXPRESSION', 'LOOKUP', 'AGGREGATION'
  source_config JSONB NOT NULL,
  default_value TEXT,
  description TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  
  UNIQUE(rule_version_id, name)
);
```

### 4.6 rule_actions (Ações)
```sql
CREATE TABLE rule_actions (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  rule_version_id UUID NOT NULL REFERENCES rule_versions(id) ON DELETE CASCADE,
  action_type rule_action_type NOT NULL,
  action_config JSONB NOT NULL,
  position INT NOT NULL DEFAULT 0,
  condition_group_id UUID REFERENCES rule_condition_groups(id),
  enabled BOOLEAN NOT NULL DEFAULT TRUE,
  description TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
```

---

## 5. Tabelas de Versionamento

### 5.1 rules (Header)
```sql
CREATE TABLE rules (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  key TEXT NOT NULL UNIQUE,
  title TEXT NOT NULL,
  created_by UUID REFERENCES users(id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
```

### 5.2 rule_versions
```sql
CREATE TABLE rule_versions (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  rule_id UUID NOT NULL REFERENCES rules(id) ON DELETE CASCADE,
  version INT NOT NULL,
  status rule_status NOT NULL DEFAULT 'DRAFT',
  priority INT NOT NULL CHECK (priority >= 0),
  severity INT NOT NULL CHECK (severity >= 0 AND severity <= 100),
  decision decision_outcome NOT NULL,
  reason_template TEXT NOT NULL,
  fields_used TEXT[] NOT NULL,
  logic logic_operator NOT NULL,
  conditions_json JSONB NOT NULL,
  enabled BOOLEAN NOT NULL DEFAULT TRUE,
  created_by UUID REFERENCES users(id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  
  UNIQUE (rule_id, version)
);
```

### 5.3 rule_sets
```sql
CREATE TABLE rule_sets (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  key TEXT NOT NULL UNIQUE,
  title TEXT NOT NULL,
  created_by UUID REFERENCES users(id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
```

### 5.4 rule_set_versions
```sql
CREATE TABLE rule_set_versions (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  rule_set_id UUID NOT NULL REFERENCES rule_sets(id) ON DELETE CASCADE,
  version INT NOT NULL,
  status rule_status NOT NULL DEFAULT 'DRAFT',
  notes TEXT,
  created_by UUID REFERENCES users(id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  
  UNIQUE (rule_set_id, version)
);
```

### 5.5 rule_set_version_items
```sql
CREATE TABLE rule_set_version_items (
  rule_set_version_id UUID NOT NULL REFERENCES rule_set_versions(id) ON DELETE CASCADE,
  rule_version_id UUID NOT NULL REFERENCES rule_versions(id) ON DELETE RESTRICT,
  sort_order INT NOT NULL DEFAULT 0,
  
  PRIMARY KEY (rule_set_version_id, rule_version_id)
);
```

### 5.6 active_rule_set (Singleton)
```sql
CREATE TABLE active_rule_set (
  id SMALLINT PRIMARY KEY DEFAULT 1,
  rule_set_version_id UUID NOT NULL REFERENCES rule_set_versions(id),
  activated_by UUID REFERENCES users(id),
  activated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  
  CHECK (id = 1)
);
```

---

## 6. Tabelas de Velocity

### 6.1 velocity_counters
```sql
CREATE TABLE velocity_counters (
  id BIGSERIAL PRIMARY KEY,
  key_type VARCHAR(50) NOT NULL,
  key_value VARCHAR(255) NOT NULL,
  window_type VARCHAR(20) NOT NULL,
  window_start TIMESTAMP WITH TIME ZONE NOT NULL,
  window_end TIMESTAMP WITH TIME ZONE NOT NULL,
  
  -- Contadores
  transaction_count INTEGER DEFAULT 0,
  total_amount NUMERIC(18,2) DEFAULT 0,
  avg_amount NUMERIC(18,2) DEFAULT 0,
  min_amount NUMERIC(18,2),
  max_amount NUMERIC(18,2),
  
  -- Contadores de decisão
  approved_count INTEGER DEFAULT 0,
  suspicious_count INTEGER DEFAULT 0,
  fraud_count INTEGER DEFAULT 0,
  
  -- Contadores de características
  distinct_merchants INTEGER DEFAULT 0,
  distinct_mccs INTEGER DEFAULT 0,
  distinct_countries INTEGER DEFAULT 0,
  
  -- Metadados
  last_transaction_id VARCHAR(64),
  last_transaction_at TIMESTAMP WITH TIME ZONE,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  
  CONSTRAINT uk_velocity_counter UNIQUE (key_type, key_value, window_type, window_start)
);
```

### 6.2 velocity_transaction_log
```sql
CREATE TABLE velocity_transaction_log (
  id BIGSERIAL PRIMARY KEY,
  external_transaction_id VARCHAR(64) NOT NULL UNIQUE,
  pan_hash VARCHAR(64) NOT NULL,
  customer_id VARCHAR(64),
  merchant_id VARCHAR(64),
  amount NUMERIC(18,2) NOT NULL,
  currency_code INTEGER,
  mcc INTEGER,
  merchant_country VARCHAR(10),
  decision VARCHAR(20),
  risk_score INTEGER,
  transaction_at TIMESTAMP WITH TIME ZONE NOT NULL,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);
```

**Função de agregação:**
```sql
CREATE FUNCTION get_velocity_stats(
  p_key_type VARCHAR,
  p_key_value VARCHAR,
  p_window_minutes INTEGER
) RETURNS TABLE (
  transaction_count BIGINT,
  total_amount NUMERIC,
  avg_amount NUMERIC,
  min_amount NUMERIC,
  max_amount NUMERIC,
  distinct_merchants BIGINT,
  distinct_mccs BIGINT
);
```

---

## 7. Tabelas de Geolocalização

### 7.1 geo_reference
```sql
CREATE TABLE geo_reference (
  id BIGSERIAL PRIMARY KEY,
  country_code VARCHAR(10) NOT NULL,
  state_code VARCHAR(10),
  city_name VARCHAR(255),
  latitude NUMERIC(10,6) NOT NULL,
  longitude NUMERIC(10,6) NOT NULL,
  population INTEGER,
  is_capital BOOLEAN DEFAULT FALSE,
  timezone VARCHAR(50),
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);
```

### 7.2 geo_polygon
```sql
CREATE TABLE geo_polygon (
  id BIGSERIAL PRIMARY KEY,
  name VARCHAR(100) NOT NULL UNIQUE,
  description TEXT,
  polygon_points TEXT NOT NULL, -- JSON array of [lat, lon] pairs
  min_lat NUMERIC(10,6),
  max_lat NUMERIC(10,6),
  min_lon NUMERIC(10,6),
  max_lon NUMERIC(10,6),
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);
```

---

## 8. Tabelas de Auditoria

### 8.1 decision_log
```sql
CREATE TABLE decision_log (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  external_transaction_id TEXT NOT NULL,
  customer_id TEXT NOT NULL,
  merchant_id TEXT,
  amount NUMERIC(18,2),
  currency_code INT,
  decision decision_outcome NOT NULL,
  risk_score INT NOT NULL CHECK (risk_score >= 0 AND risk_score <= 100),
  triggered_rules_json JSONB NOT NULL,
  explain_json JSONB NOT NULL,
  payload_json JSONB NOT NULL,
  pan_masked TEXT,
  ruleset_version_id UUID REFERENCES rule_set_versions(id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
```

### 8.2 audit_log
```sql
CREATE TABLE audit_log (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  action_type audit_action_type NOT NULL,
  entity_type TEXT,
  entity_id UUID,
  performed_by UUID REFERENCES users(id),
  diff_json JSONB,
  details_json JSONB,
  result audit_result NOT NULL,
  error_message TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
```

### 8.3 rule_execution_details
```sql
CREATE TABLE rule_execution_details (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  decision_log_id UUID NOT NULL REFERENCES decision_log(id) ON DELETE CASCADE,
  rule_version_id UUID NOT NULL REFERENCES rule_versions(id),
  condition_id UUID,
  group_id UUID,
  field_name TEXT,
  field_value TEXT,
  operator TEXT,
  expected_value TEXT,
  result BOOLEAN NOT NULL,
  execution_time_ms INT,
  error_message TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
```

---

## 9. Tabelas de Suporte

### 9.1 bin_lookup
```sql
CREATE TABLE bin_lookup (
  id BIGSERIAL PRIMARY KEY,
  bin VARCHAR(8) NOT NULL UNIQUE,
  brand VARCHAR(50),
  type VARCHAR(50),
  category VARCHAR(50),
  issuer VARCHAR(255),
  country_code VARCHAR(10),
  country_name VARCHAR(100),
  is_prepaid BOOLEAN DEFAULT FALSE,
  is_commercial BOOLEAN DEFAULT FALSE,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);
```

### 9.2 mcc_categories
```sql
CREATE TABLE mcc_categories (
  id BIGSERIAL PRIMARY KEY,
  mcc INTEGER NOT NULL UNIQUE,
  description VARCHAR(255) NOT NULL,
  category VARCHAR(100),
  risk_level VARCHAR(20) DEFAULT 'NORMAL',
  is_high_risk BOOLEAN DEFAULT FALSE,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);
```

### 9.3 rule_templates
```sql
CREATE TABLE rule_templates (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  name TEXT NOT NULL UNIQUE,
  category TEXT NOT NULL,
  description TEXT,
  template_config JSONB NOT NULL,
  is_system BOOLEAN NOT NULL DEFAULT FALSE,
  created_by UUID REFERENCES users(id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
```

---

## 10. Diagrama ER (Simplificado)

```
┌─────────────────┐     ┌─────────────────────┐
│     rules       │────<│   rule_versions     │
└─────────────────┘     └─────────────────────┘
                                  │
                                  │
                        ┌─────────┴─────────┐
                        │                   │
              ┌─────────▼─────────┐  ┌──────▼──────────────┐
              │rule_condition_groups│  │   rule_actions     │
              └─────────┬─────────┘  └────────────────────┘
                        │
                        │ (self-reference)
                        │
              ┌─────────▼─────────┐
              │  rule_conditions  │
              └───────────────────┘


┌─────────────────┐     ┌─────────────────────┐
│   rule_sets     │────<│ rule_set_versions   │
└─────────────────┘     └─────────────────────┘
                                  │
                        ┌─────────▼─────────┐
                        │rule_set_version_items│
                        └─────────┬─────────┘
                                  │
                        ┌─────────▼─────────┐
                        │  rule_versions    │
                        └───────────────────┘


┌─────────────────────┐
│  active_rule_set    │──────> rule_set_versions
│  (singleton id=1)   │
└─────────────────────┘
```

---

## 11. Referências

- **V1__init.sql**: Schema inicial (RBAC, rules, audit)
- **V2__core_schema.sql**: Transações e rule_configurations
- **V8__complex_rules_support.sql**: Regras complexas
- **V14__velocity_counters.sql**: Sistema de velocity
- **V15__add_velocity_operators.sql**: Operadores velocity
- **V13__geo_reference_table.sql**: Geolocalização
- **V22__fraud_detection_rules_seed.sql**: Seed de regras iniciais
- **V23__web_research_fraud_rules.sql**: 14 regras de pesquisa web (Card Testing, Scams/APP, Mule, ATO, CNP, Velocity, ECI/3DS)
