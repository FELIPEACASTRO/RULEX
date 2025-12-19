-- V3.1: Store raw payload "as received" (bytes) and make external_transaction_id the primary key.

-- Create a new table with the desired shape.
CREATE TABLE IF NOT EXISTS transaction_raw_store_v31 (
  external_transaction_id VARCHAR(64) PRIMARY KEY,
  payload_raw_hash VARCHAR(64) NOT NULL,
  payload_raw_bytes BYTEA NOT NULL,
  content_type VARCHAR(100),
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_transaction_raw_store_v31_hash
  ON transaction_raw_store_v31 (payload_raw_hash);

-- Best-effort migration from the previous table (which stored canonical JSON text).
-- NOTE: This cannot reconstruct the original HTTP raw bytes; it preserves what we had.
DO $$
BEGIN
  IF EXISTS (
    SELECT 1
    FROM information_schema.tables
    WHERE table_name = 'transaction_raw_store'
  ) THEN
    INSERT INTO transaction_raw_store_v31 (
      external_transaction_id,
      payload_raw_hash,
      payload_raw_bytes,
      content_type,
      created_at
    )
    SELECT
      external_transaction_id,
      payload_raw_hash,
      convert_to(payload_raw_json, 'UTF8'),
      'application/json; charset=utf-8',
      created_at
    FROM transaction_raw_store
    ON CONFLICT (external_transaction_id) DO NOTHING;

    DROP TABLE transaction_raw_store;
  END IF;
END $$;

ALTER TABLE transaction_raw_store_v31 RENAME TO transaction_raw_store;
