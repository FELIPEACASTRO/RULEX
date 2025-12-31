-- V10: Melhorias de tipos derivados e performance
-- Migração idempotente para:
-- 1. Converter conditions_json de TEXT para JSONB
-- 2. Adicionar coluna transaction_ts (TIMESTAMPTZ derivada)
-- 3. Criar índices compostos para anti-tamper

-- =========================================
-- 1. CONVERTER conditions_json TEXT → JSONB
-- =========================================
-- Verifica se a coluna existe e é TEXT antes de converter
DO $$
BEGIN
    -- Verificar se a coluna existe e é do tipo TEXT
    IF EXISTS (
        SELECT 1 
        FROM information_schema.columns 
        WHERE table_name = 'rule_configurations' 
        AND column_name = 'conditions_json'
        AND data_type = 'text'
    ) THEN
        -- Criar coluna temporária JSONB
        ALTER TABLE rule_configurations 
        ADD COLUMN IF NOT EXISTS conditions_jsonb JSONB;
        
        -- Migrar dados válidos (JSON válido) para a nova coluna
        UPDATE rule_configurations 
        SET conditions_jsonb = CASE 
            WHEN conditions_json IS NULL THEN NULL
            WHEN conditions_json = '' THEN '[]'::jsonb
            WHEN conditions_json ~ '^\s*[\[\{]' THEN conditions_json::jsonb
            ELSE ('["' || conditions_json || '"]')::jsonb
        END
        WHERE conditions_jsonb IS NULL;
        
        -- Dropar coluna antiga e renomear
        ALTER TABLE rule_configurations DROP COLUMN conditions_json;
        ALTER TABLE rule_configurations RENAME COLUMN conditions_jsonb TO conditions_json;
        
        RAISE NOTICE 'conditions_json convertido de TEXT para JSONB com sucesso';
    ELSE
        RAISE NOTICE 'conditions_json já é JSONB ou não existe';
    END IF;
END $$;

-- Criar índice GIN para buscas em JSONB (se não existir)
CREATE INDEX IF NOT EXISTS idx_rule_config_conditions_gin 
ON rule_configurations USING GIN (conditions_json);

-- =========================================
-- 2. ADICIONAR COLUNA transaction_ts (TIMESTAMPTZ derivada)
-- =========================================
-- Esta coluna combina transaction_date + transaction_time + gmt_offset
-- para permitir queries temporais eficientes

ALTER TABLE transactions 
ADD COLUMN IF NOT EXISTS transaction_ts TIMESTAMPTZ;

-- Função para derivar timestamp a partir de date/time/offset
CREATE OR REPLACE FUNCTION derive_transaction_ts(
    p_date INTEGER,
    p_time INTEGER,
    p_gmt_offset VARCHAR(10)
) RETURNS TIMESTAMPTZ AS $$
DECLARE
    v_date_str VARCHAR(8);
    v_time_str VARCHAR(6);
    v_offset_hours INTEGER;
    v_offset_minutes INTEGER;
    v_offset_str VARCHAR(6);
    v_timestamp TIMESTAMPTZ;
BEGIN
    -- Validar inputs
    IF p_date IS NULL OR p_time IS NULL THEN
        RETURN NULL;
    END IF;
    
    -- Formatar date e time
    v_date_str := LPAD(p_date::TEXT, 8, '0');
    v_time_str := LPAD(p_time::TEXT, 6, '0');
    
    -- Parsear offset (formato -0300 ou +0530)
    IF p_gmt_offset IS NOT NULL AND LENGTH(TRIM(p_gmt_offset)) = 5 THEN
        v_offset_hours := SUBSTRING(p_gmt_offset FROM 2 FOR 2)::INTEGER;
        v_offset_minutes := SUBSTRING(p_gmt_offset FROM 4 FOR 2)::INTEGER;
        IF SUBSTRING(p_gmt_offset FROM 1 FOR 1) = '-' THEN
            v_offset_str := '-' || LPAD(v_offset_hours::TEXT, 2, '0') || ':' || LPAD(v_offset_minutes::TEXT, 2, '0');
        ELSE
            v_offset_str := '+' || LPAD(v_offset_hours::TEXT, 2, '0') || ':' || LPAD(v_offset_minutes::TEXT, 2, '0');
        END IF;
    ELSE
        v_offset_str := '+00:00'; -- UTC como fallback
    END IF;
    
    -- Construir timestamp
    BEGIN
        v_timestamp := (
            SUBSTRING(v_date_str FROM 1 FOR 4) || '-' ||
            SUBSTRING(v_date_str FROM 5 FOR 2) || '-' ||
            SUBSTRING(v_date_str FROM 7 FOR 2) || ' ' ||
            SUBSTRING(v_time_str FROM 1 FOR 2) || ':' ||
            SUBSTRING(v_time_str FROM 3 FOR 2) || ':' ||
            SUBSTRING(v_time_str FROM 5 FOR 2) ||
            v_offset_str
        )::TIMESTAMPTZ;
        RETURN v_timestamp;
    EXCEPTION WHEN OTHERS THEN
        RETURN NULL;
    END;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- Popular coluna para registros existentes
UPDATE transactions 
SET transaction_ts = derive_transaction_ts(transaction_date, transaction_time, gmt_offset)
WHERE transaction_ts IS NULL 
AND transaction_date IS NOT NULL 
AND transaction_time IS NOT NULL;

-- Criar índice para queries temporais
CREATE INDEX IF NOT EXISTS idx_transactions_ts 
ON transactions (transaction_ts);

-- Criar índice composto para range queries
CREATE INDEX IF NOT EXISTS idx_transactions_ts_classification 
ON transaction_decisions (created_at, classification);

-- =========================================
-- 3. ÍNDICES COMPOSTOS PARA ANTI-TAMPER
-- =========================================
-- Índice composto para lookup rápido de idempotência
CREATE INDEX IF NOT EXISTS idx_transactions_ext_id_hash 
ON transactions (external_transaction_id, payload_raw_hash)
WHERE payload_raw_hash IS NOT NULL;

-- Índice para raw store (já existe, mas garantir)
CREATE INDEX IF NOT EXISTS idx_raw_store_ext_id_hash 
ON transaction_raw_store (external_transaction_id, payload_raw_hash);

-- =========================================
-- 4. TRIGGER PARA POPULAR transaction_ts AUTOMATICAMENTE
-- =========================================
CREATE OR REPLACE FUNCTION trigger_derive_transaction_ts()
RETURNS TRIGGER AS $$
BEGIN
    IF NEW.transaction_ts IS NULL AND NEW.transaction_date IS NOT NULL AND NEW.transaction_time IS NOT NULL THEN
        NEW.transaction_ts := derive_transaction_ts(NEW.transaction_date, NEW.transaction_time, NEW.gmt_offset);
    END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trg_derive_transaction_ts ON transactions;
CREATE TRIGGER trg_derive_transaction_ts
BEFORE INSERT OR UPDATE ON transactions
FOR EACH ROW EXECUTE FUNCTION trigger_derive_transaction_ts();

-- =========================================
-- COMENTÁRIOS
-- =========================================
COMMENT ON COLUMN transactions.transaction_ts IS 'Timestamp derivado de transaction_date + transaction_time + gmt_offset para queries temporais';
COMMENT ON FUNCTION derive_transaction_ts IS 'Deriva TIMESTAMPTZ a partir de date (YYYYMMDD), time (HHMMSS) e gmt_offset (-0300)';
COMMENT ON INDEX idx_rule_config_conditions_gin IS 'Índice GIN para buscas em conditions_json JSONB';
COMMENT ON INDEX idx_transactions_ext_id_hash IS 'Índice composto para anti-tamper/idempotência';
