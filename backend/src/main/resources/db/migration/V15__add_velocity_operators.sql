-- V15__add_velocity_operators.sql
-- Adiciona operadores VELOCITY ao enum condition_operator
-- Estes operadores permitem agregações temporais (COUNT, SUM, AVG, DISTINCT)

-- Adicionar novos valores ao enum condition_operator
-- PostgreSQL requer ALTER TYPE ... ADD VALUE para cada valor

ALTER TYPE condition_operator ADD VALUE IF NOT EXISTS 'VELOCITY_COUNT_GT';
ALTER TYPE condition_operator ADD VALUE IF NOT EXISTS 'VELOCITY_COUNT_LT';
ALTER TYPE condition_operator ADD VALUE IF NOT EXISTS 'VELOCITY_SUM_GT';
ALTER TYPE condition_operator ADD VALUE IF NOT EXISTS 'VELOCITY_SUM_LT';
ALTER TYPE condition_operator ADD VALUE IF NOT EXISTS 'VELOCITY_AVG_GT';
ALTER TYPE condition_operator ADD VALUE IF NOT EXISTS 'VELOCITY_AVG_LT';
ALTER TYPE condition_operator ADD VALUE IF NOT EXISTS 'VELOCITY_DISTINCT_GT';
ALTER TYPE condition_operator ADD VALUE IF NOT EXISTS 'VELOCITY_DISTINCT_LT';

-- Comentário para documentação
COMMENT ON TYPE condition_operator IS 'Operadores de comparação para condições de regras. Inclui operadores básicos, strings, arrays, geo, e velocity (agregações temporais).';
