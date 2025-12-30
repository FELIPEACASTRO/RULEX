-- R7: Rollback V7__v31_exec_log_dedup.sql
-- WARNING: This will remove the dedup index. Run with caution.

-- Drop the new dedupe index
DROP INDEX IF EXISTS uq_rule_execution_log_dedup;

-- Recreate the original strict constraint (if it existed)
-- Note: The original index name was uq_rule_execution_log_extid_normal
-- You may need to adjust based on your original schema
CREATE UNIQUE INDEX IF NOT EXISTS uq_rule_execution_log_extid_normal
  ON rule_execution_log (external_transaction_id)
  WHERE external_transaction_id IS NOT NULL AND event_type = 'NORMAL';

-- Note: Cannot remove enum value 'IDEMPOTENT_REPLAY' in PostgreSQL
-- Enum values cannot be removed, only added
-- This is a known PostgreSQL limitation
