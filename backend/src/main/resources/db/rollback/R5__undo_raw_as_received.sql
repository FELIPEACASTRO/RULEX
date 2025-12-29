-- R5: Rollback V5__raw_as_received.sql
-- WARNING: This will drop columns and lose data. Run with caution.

-- Drop the raw_as_received column from transaction_raw table
ALTER TABLE transaction_raw DROP COLUMN IF EXISTS raw_as_received;
