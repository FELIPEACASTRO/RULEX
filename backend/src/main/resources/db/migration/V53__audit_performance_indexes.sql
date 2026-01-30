-- V53__audit_performance_indexes.sql
-- GAP-009 FIX: Índices adicionais para melhorar performance de consultas de auditoria
-- Data: 2025-01-30
-- Autor: Devin AI

-- =========================================
-- ÍNDICES PARA AUDIT_LOG
-- =========================================

-- Índice composto para consultas por tipo de entidade e período
-- Usado em: GET /api/audit?entityType=RULE&from=2025-01-01&to=2025-01-31
CREATE INDEX IF NOT EXISTS idx_audit_log_entity_type_created 
ON audit_log(entity_type, created_at DESC);

-- Índice para busca por usuário que executou a ação
-- Usado em: GET /api/audit?performedBy=user-uuid
CREATE INDEX IF NOT EXISTS idx_audit_log_performed_by 
ON audit_log(performed_by) 
WHERE performed_by IS NOT NULL;

-- Índice para busca por entidade específica
-- Usado em: GET /api/audit?entityId=rule-uuid
CREATE INDEX IF NOT EXISTS idx_audit_log_entity_id 
ON audit_log(entity_id) 
WHERE entity_id IS NOT NULL;

-- Índice composto para consultas de auditoria por ação e resultado
-- Usado em: GET /api/audit?actionType=RULE_CREATED&result=SUCCESS
CREATE INDEX IF NOT EXISTS idx_audit_log_action_result 
ON audit_log(action_type, result, created_at DESC);

-- =========================================
-- ÍNDICES PARA DECISION_LOG
-- =========================================

-- Índice para busca por merchant
-- Usado em: Análise de fraude por merchant
CREATE INDEX IF NOT EXISTS idx_decision_log_merchant_id 
ON decision_log(merchant_id) 
WHERE merchant_id IS NOT NULL;

-- Índice composto para consultas por decisão e período
-- Usado em: GET /api/decisions?decision=FRAUDE&from=2025-01-01
CREATE INDEX IF NOT EXISTS idx_decision_log_decision_created 
ON decision_log(decision, created_at DESC);

-- Índice para busca por score de risco alto
-- Usado em: Alertas de transações de alto risco
CREATE INDEX IF NOT EXISTS idx_decision_log_high_risk 
ON decision_log(risk_score, created_at DESC) 
WHERE risk_score >= 70;

-- Índice para busca por transação externa
-- Usado em: Reconciliação com sistemas externos
CREATE INDEX IF NOT EXISTS idx_decision_log_external_tx 
ON decision_log(external_transaction_id);

-- =========================================
-- ÍNDICES PARA ACCESS_LOG
-- =========================================

-- Índice para busca por método HTTP e status
-- Usado em: Análise de erros por endpoint
CREATE INDEX IF NOT EXISTS idx_access_log_method_status 
ON access_log(method, status_code, created_at DESC)
WHERE status_code >= 400;

-- Índice para busca por IP de origem
-- Usado em: Detecção de ataques/abuso
CREATE INDEX IF NOT EXISTS idx_access_log_ip 
ON access_log(client_ip, created_at DESC)
WHERE client_ip IS NOT NULL;

-- =========================================
-- ÍNDICES PARA RULE_EXECUTION_DETAILS
-- =========================================

-- Índice para busca por regra específica
-- Usado em: Análise de performance de regras
CREATE INDEX IF NOT EXISTS idx_rule_exec_details_rule_id 
ON rule_execution_details(rule_id, created_at DESC)
WHERE rule_id IS NOT NULL;

-- Índice para busca por regras que dispararam
-- Usado em: Análise de efetividade de regras
CREATE INDEX IF NOT EXISTS idx_rule_exec_details_triggered 
ON rule_execution_details(triggered, created_at DESC)
WHERE triggered = true;

-- =========================================
-- COMENTÁRIOS
-- =========================================
COMMENT ON INDEX idx_audit_log_entity_type_created IS 'GAP-009: Otimiza consultas de auditoria por tipo de entidade e período';
COMMENT ON INDEX idx_audit_log_performed_by IS 'GAP-009: Otimiza consultas de auditoria por usuário';
COMMENT ON INDEX idx_decision_log_high_risk IS 'GAP-009: Otimiza alertas de transações de alto risco (score >= 70)';
