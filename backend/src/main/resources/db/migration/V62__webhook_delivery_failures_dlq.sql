-- V62__webhook_delivery_failures_dlq.sql
-- RES-003 FIX: Dead Letter Queue para webhooks falhados
-- Data: 2025-01-30

-- =========================================
-- TABELA: webhook_delivery_failures
-- =========================================
-- Armazena webhooks que falharam para reprocessamento

CREATE TABLE IF NOT EXISTS webhook_delivery_failures (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    
    -- Dados do webhook
    webhook_url VARCHAR(2048) NOT NULL,
    payload JSONB,
    headers JSONB,
    
    -- Contexto
    rule_id UUID,
    transaction_id VARCHAR(100),
    
    -- Status e retry
    status VARCHAR(20) NOT NULL DEFAULT 'PENDING',
    retry_count INTEGER NOT NULL DEFAULT 0,
    max_retries INTEGER NOT NULL DEFAULT 5,
    
    -- Informações de erro
    last_error TEXT,
    last_status_code INTEGER,
    
    -- Timestamps
    next_retry_at TIMESTAMPTZ,
    last_attempt_at TIMESTAMPTZ,
    resolved_at TIMESTAMPTZ,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Comentários
COMMENT ON TABLE webhook_delivery_failures IS 'Dead Letter Queue para webhooks falhados (RES-003)';
COMMENT ON COLUMN webhook_delivery_failures.status IS 'PENDING, RETRYING, SUCCESS, FAILED, EXPIRED';
COMMENT ON COLUMN webhook_delivery_failures.retry_count IS 'Número de tentativas realizadas';
COMMENT ON COLUMN webhook_delivery_failures.next_retry_at IS 'Próxima tentativa agendada (backoff exponencial)';

-- =========================================
-- ÍNDICES
-- =========================================

-- Índice para buscar webhooks pendentes de retry
CREATE INDEX IF NOT EXISTS idx_webhook_dlq_pending 
ON webhook_delivery_failures(status, next_retry_at) 
WHERE status IN ('PENDING', 'RETRYING');

-- Índice para buscar por transação
CREATE INDEX IF NOT EXISTS idx_webhook_dlq_transaction 
ON webhook_delivery_failures(transaction_id) 
WHERE transaction_id IS NOT NULL;

-- Índice para buscar por regra
CREATE INDEX IF NOT EXISTS idx_webhook_dlq_rule 
ON webhook_delivery_failures(rule_id) 
WHERE rule_id IS NOT NULL;

-- Índice para limpeza de registros antigos
CREATE INDEX IF NOT EXISTS idx_webhook_dlq_cleanup 
ON webhook_delivery_failures(status, created_at);

-- =========================================
-- CONSTRAINT de status válido
-- =========================================
ALTER TABLE webhook_delivery_failures 
ADD CONSTRAINT chk_webhook_dlq_status 
CHECK (status IN ('PENDING', 'RETRYING', 'SUCCESS', 'FAILED', 'EXPIRED'));
