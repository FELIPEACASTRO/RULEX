-- V9: Melhorias de Auditoria e Compliance
-- Adiciona campos para rastreabilidade completa de ações

-- Adicionar colunas de auditoria na tabela de histórico de regras
ALTER TABLE rule_configuration_history 
ADD COLUMN IF NOT EXISTS client_ip VARCHAR(45);

ALTER TABLE rule_configuration_history 
ADD COLUMN IF NOT EXISTS user_agent VARCHAR(500);

ALTER TABLE rule_configuration_history 
ADD COLUMN IF NOT EXISTS action_type VARCHAR(20);

-- Atualizar registros existentes com action_type baseado nos dados
UPDATE rule_configuration_history 
SET action_type = CASE 
    WHEN previous_json IS NULL AND current_json IS NOT NULL THEN 'CREATE'
    WHEN previous_json IS NOT NULL AND current_json IS NULL THEN 'DELETE'
    ELSE 'UPDATE'
END
WHERE action_type IS NULL;

-- Adicionar coluna source_ip na tabela de audit_logs se não existir
ALTER TABLE audit_logs 
ADD COLUMN IF NOT EXISTS source_ip VARCHAR(45);

-- Criar índices para consultas de compliance
CREATE INDEX IF NOT EXISTS idx_rule_hist_performed_by ON rule_configuration_history(performed_by);
CREATE INDEX IF NOT EXISTS idx_rule_hist_action_type ON rule_configuration_history(action_type);
CREATE INDEX IF NOT EXISTS idx_audit_logs_performed_by ON audit_logs(performed_by);

-- Criar tabela de aprovações de regras (workflow 4 olhos)
CREATE TABLE IF NOT EXISTS rule_approvals (
    id BIGSERIAL PRIMARY KEY,
    rule_id BIGINT NOT NULL,
    rule_name VARCHAR(100) NOT NULL,
    action_type VARCHAR(20) NOT NULL, -- CREATE, UPDATE, DELETE, TOGGLE
    requested_by VARCHAR(100) NOT NULL,
    requested_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    approved_by VARCHAR(100),
    approved_at TIMESTAMP,
    rejected_by VARCHAR(100),
    rejected_at TIMESTAMP,
    status VARCHAR(20) NOT NULL DEFAULT 'PENDING', -- PENDING, APPROVED, REJECTED, CANCELLED
    payload_json TEXT, -- JSON com os dados da alteração proposta
    comments TEXT,
    client_ip VARCHAR(45),
    CONSTRAINT fk_rule_approvals_rule FOREIGN KEY (rule_id) REFERENCES rule_configurations(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_rule_approvals_status ON rule_approvals(status);
CREATE INDEX IF NOT EXISTS idx_rule_approvals_rule_id ON rule_approvals(rule_id);
CREATE INDEX IF NOT EXISTS idx_rule_approvals_requested_by ON rule_approvals(requested_by);

-- Criar tabela de métricas de regras
CREATE TABLE IF NOT EXISTS rule_metrics (
    id BIGSERIAL PRIMARY KEY,
    rule_id BIGINT NOT NULL,
    rule_name VARCHAR(100) NOT NULL,
    metric_date DATE NOT NULL,
    total_evaluations BIGINT DEFAULT 0,
    total_triggered BIGINT DEFAULT 0,
    true_positives BIGINT DEFAULT 0,
    false_positives BIGINT DEFAULT 0,
    total_amount_blocked DECIMAL(19,2) DEFAULT 0,
    avg_processing_time_ms DECIMAL(10,2) DEFAULT 0,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT fk_rule_metrics_rule FOREIGN KEY (rule_id) REFERENCES rule_configurations(id) ON DELETE CASCADE,
    CONSTRAINT uk_rule_metrics_rule_date UNIQUE (rule_id, metric_date)
);

CREATE INDEX IF NOT EXISTS idx_rule_metrics_date ON rule_metrics(metric_date);
CREATE INDEX IF NOT EXISTS idx_rule_metrics_rule_id ON rule_metrics(rule_id);

-- Criar tabela de listas (blacklist/whitelist)
CREATE TABLE IF NOT EXISTS rule_lists (
    id BIGSERIAL PRIMARY KEY,
    list_name VARCHAR(100) NOT NULL UNIQUE,
    list_type VARCHAR(20) NOT NULL, -- BLACKLIST, WHITELIST, GREYLIST
    entity_type VARCHAR(50) NOT NULL, -- PAN, MERCHANT_ID, CUSTOMER_ID, MCC, COUNTRY, IP
    description TEXT,
    is_active BOOLEAN DEFAULT TRUE,
    created_by VARCHAR(100) NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS rule_list_entries (
    id BIGSERIAL PRIMARY KEY,
    list_id BIGINT NOT NULL,
    entry_value VARCHAR(500) NOT NULL,
    reason TEXT,
    expires_at TIMESTAMP,
    added_by VARCHAR(100) NOT NULL,
    added_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    is_active BOOLEAN DEFAULT TRUE,
    CONSTRAINT fk_list_entries_list FOREIGN KEY (list_id) REFERENCES rule_lists(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_list_entries_list_id ON rule_list_entries(list_id);
CREATE INDEX IF NOT EXISTS idx_list_entries_value ON rule_list_entries(entry_value);
CREATE INDEX IF NOT EXISTS idx_list_entries_active ON rule_list_entries(is_active);

-- Adicionar soft delete na tabela de regras
ALTER TABLE rule_configurations 
ADD COLUMN IF NOT EXISTS deleted_at TIMESTAMP;

ALTER TABLE rule_configurations 
ADD COLUMN IF NOT EXISTS deleted_by VARCHAR(100);

CREATE INDEX IF NOT EXISTS idx_rule_config_deleted ON rule_configurations(deleted_at);

-- Comentários para documentação
COMMENT ON TABLE rule_approvals IS 'Workflow de aprovação de regras (4 olhos) para compliance';
COMMENT ON TABLE rule_metrics IS 'Métricas de performance e eficácia das regras';
COMMENT ON TABLE rule_lists IS 'Listas de bloqueio/permissão para regras';
COMMENT ON TABLE rule_list_entries IS 'Entradas das listas de bloqueio/permissão';
COMMENT ON COLUMN rule_configuration_history.client_ip IS 'IP do cliente que realizou a ação';
COMMENT ON COLUMN rule_configuration_history.action_type IS 'Tipo de ação: CREATE, UPDATE, DELETE';
