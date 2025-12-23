-- V11__transactions_event_ts_and_velocity_updater.sql
-- Adds canonical event timestamp for deterministic temporal/velocity features.
-- Does NOT alter inbound payload contract.

ALTER TABLE transactions
  ADD COLUMN IF NOT EXISTS transaction_ts_utc TIMESTAMPTZ;

CREATE INDEX IF NOT EXISTS idx_transactions_pan_ts
  ON transactions (pan, transaction_ts_utc);

CREATE INDEX IF NOT EXISTS idx_transactions_ts
  ON transactions (transaction_ts_utc);
