-- V33: Aumenta tamanho da coluna pos_entry_mode de 1 para 10 caracteres
-- Motivo: POS entry mode codes podem ter até 3 dígitos (ex: 051, 071, 901)

ALTER TABLE transactions 
ALTER COLUMN pos_entry_mode TYPE VARCHAR(10);
