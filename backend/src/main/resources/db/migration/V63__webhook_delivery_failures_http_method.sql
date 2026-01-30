-- V63__webhook_delivery_failures_http_method.sql
-- RES-003 FIX: Adiciona método HTTP para reprocessamento correto de DLQ
-- Data: 2026-01-30

ALTER TABLE webhook_delivery_failures
ADD COLUMN IF NOT EXISTS http_method VARCHAR(10) NOT NULL DEFAULT 'POST';

COMMENT ON COLUMN webhook_delivery_failures.http_method IS 'Método HTTP do webhook/external service (POST/GET)';
