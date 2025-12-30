-- R3: Rollback V3__extend_workflow_length.sql
-- WARNING: This may truncate data if workflow values exceed original length.

-- Revert workflow column to original length (assuming it was VARCHAR(50))
-- Note: This may fail if existing data exceeds the new limit
ALTER TABLE rules ALTER COLUMN workflow TYPE VARCHAR(50);
ALTER TABLE decision_log ALTER COLUMN workflow TYPE VARCHAR(50);
