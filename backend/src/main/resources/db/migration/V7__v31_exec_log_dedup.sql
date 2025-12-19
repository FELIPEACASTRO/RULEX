-- V7__v31_exec_log_dedup.sql
-- V3.1: allow multiple events per external_transaction_id with safe deduping.

DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM pg_type WHERE typname = 'execution_event_type') THEN
    -- Add new event type for idempotent replays (safe auditing).
    IF NOT EXISTS (
      SELECT 1
      FROM pg_enum e
      JOIN pg_type t ON t.oid = e.enumtypid
      WHERE t.typname = 'execution_event_type' AND e.enumlabel = 'IDEMPOTENT_REPLAY'
    ) THEN
      ALTER TYPE execution_event_type ADD VALUE 'IDEMPOTENT_REPLAY';
    END IF;
  END IF;
END $$;

-- Replace the overly-strict "exactly 1 normal row" constraint with a dedupe key that
-- still prevents unlimited identical log spam while allowing multiple event types.
DROP INDEX IF EXISTS uq_rule_execution_log_extid_normal;

-- Deduplicate by (extId, payload hash, event type, tamper flag, attempted hash).
-- Use COALESCE on attempted hash so NULL does not bypass the unique constraint.
CREATE UNIQUE INDEX IF NOT EXISTS uq_rule_execution_log_dedup
  ON rule_execution_log (
    external_transaction_id,
    payload_raw_hash,
    event_type,
    is_tamper,
    COALESCE(attempted_payload_hash, '')
  )
  WHERE external_transaction_id IS NOT NULL;
