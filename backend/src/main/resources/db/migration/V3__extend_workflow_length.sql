-- V3__extend_workflow_length.sql
-- Permite valores reais de workflow (ex.: "BRZLCREDIT") sem truncamento.

ALTER TABLE transactions
  ALTER COLUMN workflow TYPE VARCHAR(32);
