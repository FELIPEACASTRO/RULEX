-- V17__fix_geo_reference_id_type.sql
-- Corrige o tipo da coluna id de geo_reference de SERIAL para BIGSERIAL
-- para alinhar com a entidade JPA que usa Long

-- Alterar o tipo da coluna id
ALTER TABLE geo_reference ALTER COLUMN id TYPE BIGINT;

-- Alterar a sequence para BIGINT também
ALTER SEQUENCE geo_reference_id_seq AS BIGINT;
