-- V1__init.sql
-- Schema inicial (homologável) do RULEX - Antifraude Cartão de Crédito
-- Fonte da verdade: migrations (Flyway). Não usar ddl-auto para criar/alterar schema.

-- =========================================
-- EXTENSÕES
-- =========================================
CREATE EXTENSION IF NOT EXISTS pgcrypto;

-- =========================================
-- RBAC (users/roles)
-- =========================================
CREATE TABLE IF NOT EXISTS roles (
  id            UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  name          TEXT NOT NULL UNIQUE,
  created_at    TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS users (
  id            UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  email         TEXT NOT NULL UNIQUE,
  display_name  TEXT NOT NULL,
  password_hash TEXT NOT NULL,
  enabled       BOOLEAN NOT NULL DEFAULT TRUE,
  created_at    TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS user_roles (
  user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  role_id UUID NOT NULL REFERENCES roles(id) ON DELETE RESTRICT,
  PRIMARY KEY (user_id, role_id)
);

-- =========================================
-- RULES + VERSIONAMENTO
-- =========================================
CREATE TYPE decision_outcome AS ENUM ('APROVADO', 'SUSPEITA_DE_FRAUDE', 'FRAUDE');
CREATE TYPE rule_status AS ENUM ('DRAFT', 'PUBLISHED', 'DEPRECATED');
CREATE TYPE logic_operator AS ENUM ('AND', 'OR');

CREATE TABLE IF NOT EXISTS rules (
  id           UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  key          TEXT NOT NULL UNIQUE,
  title        TEXT NOT NULL,
  created_by   UUID REFERENCES users(id) ON DELETE SET NULL,
  created_at   TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS rule_versions (
  id              UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  rule_id         UUID NOT NULL REFERENCES rules(id) ON DELETE CASCADE,
  version         INT  NOT NULL,
  status          rule_status NOT NULL DEFAULT 'DRAFT',
  priority        INT NOT NULL CHECK (priority >= 0),
  severity        INT NOT NULL CHECK (severity >= 0 AND severity <= 100),
  decision        decision_outcome NOT NULL,
  reason_template TEXT NOT NULL,
  fields_used     TEXT[] NOT NULL,
  logic           logic_operator NOT NULL,
  conditions_json JSONB NOT NULL,
  enabled         BOOLEAN NOT NULL DEFAULT TRUE,
  created_by      UUID REFERENCES users(id) ON DELETE SET NULL,
  created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (rule_id, version)
);

CREATE INDEX IF NOT EXISTS idx_rule_versions_rule_id ON rule_versions(rule_id);
CREATE INDEX IF NOT EXISTS idx_rule_versions_status ON rule_versions(status);

CREATE TABLE IF NOT EXISTS rule_sets (
  id          UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  key         TEXT NOT NULL UNIQUE,
  title       TEXT NOT NULL,
  created_by  UUID REFERENCES users(id) ON DELETE SET NULL,
  created_at  TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS rule_set_versions (
  id            UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  rule_set_id   UUID NOT NULL REFERENCES rule_sets(id) ON DELETE CASCADE,
  version       INT NOT NULL,
  status        rule_status NOT NULL DEFAULT 'DRAFT',
  notes         TEXT,
  created_by    UUID REFERENCES users(id) ON DELETE SET NULL,
  created_at    TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (rule_set_id, version)
);

CREATE INDEX IF NOT EXISTS idx_rule_set_versions_set_id ON rule_set_versions(rule_set_id);

CREATE TABLE IF NOT EXISTS rule_set_version_items (
  rule_set_version_id UUID NOT NULL REFERENCES rule_set_versions(id) ON DELETE CASCADE,
  rule_version_id     UUID NOT NULL REFERENCES rule_versions(id) ON DELETE RESTRICT,
  sort_order          INT NOT NULL DEFAULT 0,
  PRIMARY KEY (rule_set_version_id, rule_version_id)
);

CREATE TABLE IF NOT EXISTS active_rule_set (
  id                  SMALLINT PRIMARY KEY DEFAULT 1,
  rule_set_version_id UUID NOT NULL REFERENCES rule_set_versions(id) ON DELETE RESTRICT,
  activated_by        UUID REFERENCES users(id) ON DELETE SET NULL,
  activated_at        TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (id = 1)
);

-- =========================================
-- AUDIT / DECISION / SIMULATION
-- =========================================
CREATE TYPE audit_action_type AS ENUM (
  'RULE_CREATED',
  'RULE_UPDATED',
  'RULE_DELETED',
  'RULE_PUBLISHED',
  'RULESET_PUBLISHED',
  'RULESET_ACTIVATED',
  'TRANSACTION_ANALYZED',
  'SIMULATION_RUN'
);

CREATE TYPE audit_result AS ENUM ('SUCCESS', 'FAILURE');

CREATE TABLE IF NOT EXISTS audit_log (
  id            UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  action_type   audit_action_type NOT NULL,
  entity_type   TEXT,
  entity_id     UUID,
  performed_by  UUID REFERENCES users(id) ON DELETE SET NULL,
  diff_json     JSONB,
  details_json  JSONB,
  result        audit_result NOT NULL,
  error_message TEXT,
  created_at    TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_audit_log_created_at ON audit_log(created_at);
CREATE INDEX IF NOT EXISTS idx_audit_log_action_type ON audit_log(action_type);

CREATE TABLE IF NOT EXISTS decision_log (
  id                   UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  external_transaction_id TEXT NOT NULL,
  customer_id          TEXT NOT NULL,
  merchant_id          TEXT,
  amount               NUMERIC(18,2),
  currency_code        INT,
  decision             decision_outcome NOT NULL,
  risk_score           INT NOT NULL CHECK (risk_score >= 0 AND risk_score <= 100),
  triggered_rules_json JSONB NOT NULL,
  explain_json         JSONB NOT NULL,
  payload_json         JSONB NOT NULL,
  pan_masked           TEXT,
  ruleset_version_id   UUID REFERENCES rule_set_versions(id) ON DELETE SET NULL,
  created_at           TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_decision_log_customer_id ON decision_log(customer_id);
CREATE INDEX IF NOT EXISTS idx_decision_log_created_at ON decision_log(created_at);

CREATE TABLE IF NOT EXISTS simulation_runs (
  id                 UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  name               TEXT,
  requested_by       UUID REFERENCES users(id) ON DELETE SET NULL,
  rule_set_version_id UUID REFERENCES rule_set_versions(id) ON DELETE SET NULL,
  payload_json       JSONB NOT NULL,
  decision           decision_outcome NOT NULL,
  risk_score         INT NOT NULL CHECK (risk_score >= 0 AND risk_score <= 100),
  triggered_rules_json JSONB NOT NULL,
  explain_json       JSONB NOT NULL,
  created_at         TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- =========================================
-- SEED mínimo (roles/users)
-- =========================================
INSERT INTO roles(name) VALUES ('ADMIN') ON CONFLICT DO NOTHING;
INSERT INTO roles(name) VALUES ('ANALYST') ON CONFLICT DO NOTHING;

-- Senha seed: "rulex" (hash BCrypt placeholder). Trocar em homologação.
-- Como não adicionamos Spring Security agora, mantemos hash como texto.
INSERT INTO users(email, display_name, password_hash, enabled)
VALUES ('admin@rulex.local', 'Administrador RULEX', 'bcrypt:$2a$10$placeholder_admin_rulex', TRUE)
ON CONFLICT DO NOTHING;

-- vincula admin a ADMIN
INSERT INTO user_roles(user_id, role_id)
SELECT u.id, r.id
FROM users u
JOIN roles r ON r.name = 'ADMIN'
WHERE u.email = 'admin@rulex.local'
ON CONFLICT DO NOTHING;
