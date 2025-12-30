-- R1: Rollback V1__init.sql
-- WARNING: This will drop ALL database objects. Run with EXTREME caution.
-- This should only be used to completely reset the database.

-- Drop all tables created in V1
DROP TABLE IF EXISTS audit_log CASCADE;
DROP TABLE IF EXISTS users CASCADE;
DROP TABLE IF EXISTS roles CASCADE;

-- Drop any extensions added
-- DROP EXTENSION IF EXISTS "uuid-ossp";

-- Note: After running this, the database will be empty.
-- You will need to re-run all migrations from V1.
