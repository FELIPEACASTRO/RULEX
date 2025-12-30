-- R4: Rollback V4__raw_hash_idempotency.sql
-- WARNING: This will drop columns and indexes. Run with caution.

-- Drop the unique index on payload_hash
DROP INDEX IF EXISTS uq_transaction_raw_payload_hash;

-- Drop the payload_hash column
ALTER TABLE transaction_raw DROP COLUMN IF EXISTS payload_hash;
