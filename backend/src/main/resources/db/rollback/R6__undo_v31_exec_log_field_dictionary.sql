-- R6: Rollback V6__v31_exec_log_field_dictionary.sql
-- WARNING: This will drop tables and lose data. Run with caution.

-- Drop the field dictionary table
DROP TABLE IF EXISTS field_dictionary CASCADE;

-- Drop the rule execution log table
DROP TABLE IF EXISTS rule_execution_log CASCADE;

-- Drop the enum type
DROP TYPE IF EXISTS execution_event_type CASCADE;
