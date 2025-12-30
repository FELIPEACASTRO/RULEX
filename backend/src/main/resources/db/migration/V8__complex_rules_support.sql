-- V8__complex_rules_support.sql
-- Suporte a regras complexas com grupos aninhados e lógica avançada

-- =========================================
-- NOVA ESTRUTURA: CONDITION GROUPS (Grupos de Condições Aninhados)
-- =========================================

-- Tipo para operadores lógicos de grupo
CREATE TYPE group_logic_operator AS ENUM ('AND', 'OR', 'NOT', 'XOR', 'NAND', 'NOR');

-- Tabela de grupos de condições (suporta aninhamento)
CREATE TABLE IF NOT EXISTS rule_condition_groups (
  id              UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  rule_version_id UUID NOT NULL REFERENCES rule_versions(id) ON DELETE CASCADE,
  parent_group_id UUID REFERENCES rule_condition_groups(id) ON DELETE CASCADE,
  logic_operator  group_logic_operator NOT NULL DEFAULT 'AND',
  position        INT NOT NULL DEFAULT 0,
  name            TEXT,
  description     TEXT,
  enabled         BOOLEAN NOT NULL DEFAULT TRUE,
  created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
  
  -- Constraint para evitar loops infinitos (grupo não pode ser pai de si mesmo)
  CONSTRAINT chk_no_self_reference CHECK (id != parent_group_id)
);

-- Índices para performance
CREATE INDEX idx_condition_groups_rule_version ON rule_condition_groups(rule_version_id);
CREATE INDEX idx_condition_groups_parent ON rule_condition_groups(parent_group_id);

-- =========================================
-- NOVA ESTRUTURA: CONDITIONS (Condições Individuais)
-- =========================================

-- Tipo para operadores de comparação expandidos
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
  -- Funções de data/tempo
  'DATE_BEFORE', 'DATE_AFTER', 'DATE_BETWEEN', 'TIME_BEFORE', 'TIME_AFTER', 'TIME_BETWEEN',
  -- Funções de lista/array
  'ARRAY_CONTAINS', 'ARRAY_NOT_CONTAINS', 'ARRAY_SIZE_EQ', 'ARRAY_SIZE_GT', 'ARRAY_SIZE_LT',
  -- Funções matemáticas
  'MOD_EQ', 'MOD_NEQ',
  -- Geolocalização
  'GEO_DISTANCE_LT', 'GEO_DISTANCE_GT', 'GEO_IN_POLYGON'
);

-- Tipo para tipo de valor
CREATE TYPE condition_value_type AS ENUM (
  'STRING', 'NUMBER', 'BOOLEAN', 'DATE', 'TIME', 'DATETIME', 
  'ARRAY_STRING', 'ARRAY_NUMBER', 'FIELD_REFERENCE', 'EXPRESSION', 'GEO_POINT', 'GEO_POLYGON'
);

-- Tabela de condições individuais
CREATE TABLE IF NOT EXISTS rule_conditions (
  id              UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  group_id        UUID NOT NULL REFERENCES rule_condition_groups(id) ON DELETE CASCADE,
  position        INT NOT NULL DEFAULT 0,
  
  -- Campo a ser avaliado
  field_name      TEXT NOT NULL,
  field_path      TEXT, -- Para campos aninhados (ex: "customer.address.city")
  
  -- Operador
  operator        condition_operator NOT NULL,
  
  -- Valor(es) para comparação
  value_type      condition_value_type NOT NULL DEFAULT 'STRING',
  value_single    TEXT,
  value_array     TEXT[], -- Para operadores IN, NOT_IN, etc
  value_min       TEXT, -- Para BETWEEN
  value_max       TEXT, -- Para BETWEEN
  value_field_ref TEXT, -- Para comparação entre campos
  value_expression TEXT, -- Para expressões calculadas
  
  -- Configurações adicionais
  case_sensitive  BOOLEAN NOT NULL DEFAULT TRUE,
  negate          BOOLEAN NOT NULL DEFAULT FALSE, -- Inverte o resultado
  enabled         BOOLEAN NOT NULL DEFAULT TRUE,
  
  -- Metadados
  description     TEXT,
  error_message   TEXT, -- Mensagem customizada quando a condição falha
  
  created_at      TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- Índices para performance
CREATE INDEX idx_conditions_group ON rule_conditions(group_id);
CREATE INDEX idx_conditions_field ON rule_conditions(field_name);

-- =========================================
-- TABELA DE EXPRESSÕES CALCULADAS
-- =========================================

CREATE TABLE IF NOT EXISTS rule_expressions (
  id              UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  rule_version_id UUID NOT NULL REFERENCES rule_versions(id) ON DELETE CASCADE,
  name            TEXT NOT NULL,
  expression      TEXT NOT NULL, -- Ex: "transactionAmount * 1.1" ou "field1 + field2"
  result_type     condition_value_type NOT NULL DEFAULT 'NUMBER',
  description     TEXT,
  created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
  
  UNIQUE(rule_version_id, name)
);

CREATE INDEX idx_expressions_rule_version ON rule_expressions(rule_version_id);

-- =========================================
-- TABELA DE VARIÁVEIS DE CONTEXTO
-- =========================================

CREATE TABLE IF NOT EXISTS rule_context_variables (
  id              UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  rule_version_id UUID NOT NULL REFERENCES rule_versions(id) ON DELETE CASCADE,
  name            TEXT NOT NULL,
  source_type     TEXT NOT NULL, -- 'PAYLOAD', 'EXPRESSION', 'LOOKUP', 'AGGREGATION'
  source_config   JSONB NOT NULL, -- Configuração específica do tipo
  default_value   TEXT,
  description     TEXT,
  created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
  
  UNIQUE(rule_version_id, name)
);

CREATE INDEX idx_context_vars_rule_version ON rule_context_variables(rule_version_id);

-- =========================================
-- TABELA DE AÇÕES DA REGRA
-- =========================================

CREATE TYPE rule_action_type AS ENUM (
  'SET_DECISION', 'SET_SCORE', 'ADD_TAG', 'REMOVE_TAG', 
  'SET_VARIABLE', 'CALL_WEBHOOK', 'SEND_NOTIFICATION',
  'BLOCK_TRANSACTION', 'FLAG_FOR_REVIEW', 'ESCALATE'
);

CREATE TABLE IF NOT EXISTS rule_actions (
  id              UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  rule_version_id UUID NOT NULL REFERENCES rule_versions(id) ON DELETE CASCADE,
  action_type     rule_action_type NOT NULL,
  action_config   JSONB NOT NULL, -- Configuração específica da ação
  position        INT NOT NULL DEFAULT 0,
  condition_group_id UUID REFERENCES rule_condition_groups(id) ON DELETE SET NULL, -- Ação condicional
  enabled         BOOLEAN NOT NULL DEFAULT TRUE,
  description     TEXT,
  created_at      TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX idx_actions_rule_version ON rule_actions(rule_version_id);

-- =========================================
-- TABELA DE TEMPLATES DE REGRAS
-- =========================================

CREATE TABLE IF NOT EXISTS rule_templates (
  id              UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  name            TEXT NOT NULL UNIQUE,
  category        TEXT NOT NULL,
  description     TEXT,
  template_config JSONB NOT NULL, -- Configuração completa do template
  is_system       BOOLEAN NOT NULL DEFAULT FALSE, -- Templates do sistema vs usuário
  created_by      UUID REFERENCES users(id) ON DELETE SET NULL,
  created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at      TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX idx_templates_category ON rule_templates(category);

-- =========================================
-- TABELA DE HISTÓRICO DE EXECUÇÃO DE CONDIÇÕES
-- =========================================

CREATE TABLE IF NOT EXISTS rule_execution_details (
  id                UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  decision_log_id   UUID NOT NULL REFERENCES decision_log(id) ON DELETE CASCADE,
  rule_version_id   UUID NOT NULL REFERENCES rule_versions(id) ON DELETE CASCADE,
  condition_id      UUID, -- Pode ser null se for grupo
  group_id          UUID,
  field_name        TEXT,
  field_value       TEXT,
  operator          TEXT,
  expected_value    TEXT,
  result            BOOLEAN NOT NULL,
  execution_time_ms INT,
  error_message     TEXT,
  created_at        TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX idx_exec_details_decision_log ON rule_execution_details(decision_log_id);
CREATE INDEX idx_exec_details_rule_version ON rule_execution_details(rule_version_id);

-- =========================================
-- FUNÇÃO PARA VALIDAR ESTRUTURA DE GRUPOS (evitar loops)
-- =========================================

CREATE OR REPLACE FUNCTION check_group_hierarchy()
RETURNS TRIGGER AS $$
DECLARE
  current_parent UUID;
  depth INT := 0;
  max_depth INT := 10; -- Limite de profundidade
BEGIN
  IF NEW.parent_group_id IS NULL THEN
    RETURN NEW;
  END IF;
  
  current_parent := NEW.parent_group_id;
  
  WHILE current_parent IS NOT NULL AND depth < max_depth LOOP
    IF current_parent = NEW.id THEN
      RAISE EXCEPTION 'Circular reference detected in condition groups';
    END IF;
    
    SELECT parent_group_id INTO current_parent
    FROM rule_condition_groups
    WHERE id = current_parent;
    
    depth := depth + 1;
  END LOOP;
  
  IF depth >= max_depth THEN
    RAISE EXCEPTION 'Maximum nesting depth exceeded for condition groups';
  END IF;
  
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_check_group_hierarchy
BEFORE INSERT OR UPDATE ON rule_condition_groups
FOR EACH ROW EXECUTE FUNCTION check_group_hierarchy();

-- =========================================
-- INSERIR TEMPLATES DE SISTEMA
-- =========================================

INSERT INTO rule_templates (name, category, description, template_config, is_system) VALUES
(
  'HIGH_VALUE_INTERNATIONAL',
  'FRAUD_DETECTION',
  'Detecta transações de alto valor em países estrangeiros',
  '{
    "groups": [
      {
        "logic": "AND",
        "conditions": [
          {"field": "transactionAmount", "operator": "GT", "value": "5000"},
          {"field": "merchantCountryCode", "operator": "NEQ", "value": "076"}
        ]
      }
    ],
    "decision": "SUSPEITA_DE_FRAUDE",
    "severity": 70
  }'::jsonb,
  TRUE
),
(
  'LOW_SCORE_HIGH_RISK_MCC',
  'FRAUD_DETECTION', 
  'Score baixo combinado com MCC de alto risco',
  '{
    "groups": [
      {
        "logic": "AND",
        "conditions": [
          {"field": "consumerAuthenticationScore", "operator": "LT", "value": "50"},
          {"field": "mcc", "operator": "IN", "value": ["7995", "6211", "6051", "7273"]}
        ]
      }
    ],
    "decision": "FRAUDE",
    "severity": 85
  }'::jsonb,
  TRUE
),
(
  'COMPLEX_VELOCITY_CHECK',
  'VELOCITY',
  'Verificação complexa de velocidade com múltiplos critérios',
  '{
    "groups": [
      {
        "logic": "OR",
        "children": [
          {
            "logic": "AND",
            "conditions": [
              {"field": "transactionAmount", "operator": "GT", "value": "1000"},
              {"field": "transactionsLast24h", "operator": "GT", "value": "5"}
            ]
          },
          {
            "logic": "AND", 
            "conditions": [
              {"field": "transactionAmount", "operator": "GT", "value": "500"},
              {"field": "transactionsLast1h", "operator": "GT", "value": "3"}
            ]
          }
        ]
      }
    ],
    "decision": "SUSPEITA_DE_FRAUDE",
    "severity": 60
  }'::jsonb,
  TRUE
)
ON CONFLICT (name) DO NOTHING;

-- =========================================
-- COMENTÁRIOS PARA DOCUMENTAÇÃO
-- =========================================

COMMENT ON TABLE rule_condition_groups IS 'Grupos de condições que suportam aninhamento ilimitado com operadores lógicos';
COMMENT ON TABLE rule_conditions IS 'Condições individuais com suporte a operadores avançados e comparação entre campos';
COMMENT ON TABLE rule_expressions IS 'Expressões calculadas que podem ser usadas como valores em condições';
COMMENT ON TABLE rule_context_variables IS 'Variáveis de contexto que podem ser derivadas do payload ou calculadas';
COMMENT ON TABLE rule_actions IS 'Ações a serem executadas quando a regra é acionada';
COMMENT ON TABLE rule_templates IS 'Templates de regras pré-configuradas para facilitar a criação';
COMMENT ON TABLE rule_execution_details IS 'Detalhes granulares da execução de cada condição para auditoria';
