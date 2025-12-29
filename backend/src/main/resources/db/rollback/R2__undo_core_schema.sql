-- R2: Rollback V2__core_schema.sql
-- WARNING: This will drop all core tables and lose ALL data. Run with EXTREME caution.

-- Drop tables in reverse dependency order
DROP TABLE IF EXISTS decision_log CASCADE;
DROP TABLE IF EXISTS transaction_raw CASCADE;
DROP TABLE IF EXISTS rules CASCADE;
DROP TABLE IF EXISTS rule_sets CASCADE;

-- Drop any sequences
DROP SEQUENCE IF EXISTS decision_log_id_seq CASCADE;
DROP SEQUENCE IF EXISTS transaction_raw_id_seq CASCADE;
DROP SEQUENCE IF EXISTS rules_id_seq CASCADE;
DROP SEQUENCE IF EXISTS rule_sets_id_seq CASCADE;
