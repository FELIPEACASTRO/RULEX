-- V12: Seed Brazilian national holidays for 2025
-- These are used by IS_HOLIDAY and IS_BUSINESS_DAY functions in the rule engine.
-- National holidays (uf IS NULL) apply to all states.

INSERT INTO holidays (country, uf, date, name) VALUES
-- National holidays 2025 (Brazil)
('BR', NULL, '2025-01-01', 'Confraternização Universal'),
('BR', NULL, '2025-04-18', 'Sexta-feira Santa'),
('BR', NULL, '2025-04-20', 'Páscoa'),
('BR', NULL, '2025-04-21', 'Tiradentes'),
('BR', NULL, '2025-05-01', 'Dia do Trabalho'),
('BR', NULL, '2025-06-19', 'Corpus Christi'),
('BR', NULL, '2025-09-07', 'Independência do Brasil'),
('BR', NULL, '2025-10-12', 'Nossa Senhora Aparecida'),
('BR', NULL, '2025-11-02', 'Finados'),
('BR', NULL, '2025-11-15', 'Proclamação da República'),
('BR', NULL, '2025-12-25', 'Natal'),

-- Carnival 2025 (not official national holiday but bank holiday)
('BR', NULL, '2025-03-03', 'Carnaval (Segunda)'),
('BR', NULL, '2025-03-04', 'Carnaval (Terça)'),

-- State-specific holidays (examples for major states)
-- São Paulo
('BR', 'SP', '2025-01-25', 'Aniversário de São Paulo'),
('BR', 'SP', '2025-07-09', 'Revolução Constitucionalista'),

-- Rio de Janeiro
('BR', 'RJ', '2025-01-20', 'São Sebastião'),
('BR', 'RJ', '2025-04-23', 'São Jorge'),
('BR', 'RJ', '2025-11-20', 'Dia da Consciência Negra'),

-- Minas Gerais
('BR', 'MG', '2025-04-21', 'Data Magna de MG'),

-- Bahia
('BR', 'BA', '2025-07-02', 'Independência da Bahia'),

-- Rio Grande do Sul
('BR', 'RS', '2025-09-20', 'Revolução Farroupilha'),

-- Pernambuco
('BR', 'PE', '2025-03-06', 'Revolução Pernambucana'),

-- Paraná
('BR', 'PR', '2025-12-19', 'Emancipação do Paraná')

ON CONFLICT (country, uf, date) DO NOTHING;
