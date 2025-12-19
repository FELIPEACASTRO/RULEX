-- Adds deterministic payload hash support, raw payload storage, and decision idempotency keys.

ALTER TABLE transactions
  ADD COLUMN IF NOT EXISTS payload_raw_hash VARCHAR(64);

ALTER TABLE transaction_decisions
  ADD COLUMN IF NOT EXISTS external_transaction_id VARCHAR(64);

ALTER TABLE transaction_decisions
  ADD COLUMN IF NOT EXISTS payload_raw_hash VARCHAR(64);

CREATE UNIQUE INDEX IF NOT EXISTS uq_transaction_decisions_extid_hash
  ON transaction_decisions (external_transaction_id, payload_raw_hash)
  WHERE payload_raw_hash IS NOT NULL;

CREATE INDEX IF NOT EXISTS idx_transaction_decisions_extid
  ON transaction_decisions (external_transaction_id);

CREATE TABLE IF NOT EXISTS transaction_raw_store (
  id BIGSERIAL PRIMARY KEY,
  external_transaction_id VARCHAR(64) NOT NULL,
  payload_raw_hash VARCHAR(64) NOT NULL,
  payload_raw_json TEXT NOT NULL,
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_transaction_raw_store_extid
  ON transaction_raw_store (external_transaction_id);

CREATE INDEX IF NOT EXISTS idx_transaction_raw_store_hash
  ON transaction_raw_store (payload_raw_hash);
