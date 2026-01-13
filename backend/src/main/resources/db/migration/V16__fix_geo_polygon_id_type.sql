-- V16__fix_geo_polygon_id_type.sql
-- Corrige o tipo da coluna id de geo_polygon de SERIAL para BIGSERIAL
-- para alinhar com a entidade JPA que usa Long

-- Alterar o tipo da coluna id
ALTER TABLE geo_polygon ALTER COLUMN id TYPE BIGINT;

-- Alterar a sequence para BIGINT também
ALTER SEQUENCE geo_polygon_id_seq AS BIGINT;
