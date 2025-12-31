-- V13: Tabela de referência geográfica para enriquecimento de localização
-- Permite mapear merchantCity/merchantState/merchantCountryCode para coordenadas aproximadas

-- Tabela de referência geográfica (centroid de cidades/regiões)
CREATE TABLE IF NOT EXISTS geo_reference (
    id SERIAL PRIMARY KEY,
    country_code VARCHAR(3) NOT NULL,           -- Código ISO 3166-1 numérico (ex: 076 = Brasil)
    country_alpha2 VARCHAR(2),                   -- Código ISO 3166-1 alpha-2 (ex: BR)
    country_alpha3 VARCHAR(3),                   -- Código ISO 3166-1 alpha-3 (ex: BRA)
    country_name VARCHAR(100),                   -- Nome do país
    state_code VARCHAR(10),                      -- Código do estado (ex: SP, RJ)
    state_name VARCHAR(100),                     -- Nome do estado
    city_name VARCHAR(200),                      -- Nome da cidade
    latitude DECIMAL(10, 7) NOT NULL,            -- Latitude do centroid
    longitude DECIMAL(10, 7) NOT NULL,           -- Longitude do centroid
    timezone VARCHAR(50),                        -- Timezone (ex: America/Sao_Paulo)
    population INTEGER,                          -- População aproximada
    is_capital BOOLEAN DEFAULT FALSE,            -- Se é capital
    region VARCHAR(50),                          -- Região (ex: Sudeste, Sul)
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    
    CONSTRAINT uk_geo_reference_location UNIQUE (country_code, state_code, city_name)
);

-- Índices para busca eficiente
CREATE INDEX IF NOT EXISTS idx_geo_reference_country ON geo_reference(country_code);
CREATE INDEX IF NOT EXISTS idx_geo_reference_state ON geo_reference(country_code, state_code);
CREATE INDEX IF NOT EXISTS idx_geo_reference_city ON geo_reference(city_name);
CREATE INDEX IF NOT EXISTS idx_geo_reference_coords ON geo_reference(latitude, longitude);

-- Seed de dados básicos (principais cidades do Brasil)
INSERT INTO geo_reference (country_code, country_alpha2, country_alpha3, country_name, state_code, state_name, city_name, latitude, longitude, timezone, population, is_capital, region) VALUES
-- Capitais
('076', 'BR', 'BRA', 'Brasil', 'SP', 'São Paulo', 'SAO PAULO', -23.5505, -46.6333, 'America/Sao_Paulo', 12325232, FALSE, 'Sudeste'),
('076', 'BR', 'BRA', 'Brasil', 'RJ', 'Rio de Janeiro', 'RIO DE JANEIRO', -22.9068, -43.1729, 'America/Sao_Paulo', 6747815, FALSE, 'Sudeste'),
('076', 'BR', 'BRA', 'Brasil', 'DF', 'Distrito Federal', 'BRASILIA', -15.7942, -47.8822, 'America/Sao_Paulo', 3055149, TRUE, 'Centro-Oeste'),
('076', 'BR', 'BRA', 'Brasil', 'BA', 'Bahia', 'SALVADOR', -12.9714, -38.5014, 'America/Bahia', 2886698, FALSE, 'Nordeste'),
('076', 'BR', 'BRA', 'Brasil', 'CE', 'Ceará', 'FORTALEZA', -3.7172, -38.5433, 'America/Fortaleza', 2686612, FALSE, 'Nordeste'),
('076', 'BR', 'BRA', 'Brasil', 'MG', 'Minas Gerais', 'BELO HORIZONTE', -19.9167, -43.9345, 'America/Sao_Paulo', 2521564, FALSE, 'Sudeste'),
('076', 'BR', 'BRA', 'Brasil', 'AM', 'Amazonas', 'MANAUS', -3.1190, -60.0217, 'America/Manaus', 2219580, FALSE, 'Norte'),
('076', 'BR', 'BRA', 'Brasil', 'PR', 'Paraná', 'CURITIBA', -25.4290, -49.2671, 'America/Sao_Paulo', 1948626, FALSE, 'Sul'),
('076', 'BR', 'BRA', 'Brasil', 'PE', 'Pernambuco', 'RECIFE', -8.0476, -34.8770, 'America/Recife', 1653461, FALSE, 'Nordeste'),
('076', 'BR', 'BRA', 'Brasil', 'RS', 'Rio Grande do Sul', 'PORTO ALEGRE', -30.0346, -51.2177, 'America/Sao_Paulo', 1488252, FALSE, 'Sul'),
('076', 'BR', 'BRA', 'Brasil', 'GO', 'Goiás', 'GOIANIA', -16.6869, -49.2648, 'America/Sao_Paulo', 1536097, FALSE, 'Centro-Oeste'),
('076', 'BR', 'BRA', 'Brasil', 'PA', 'Pará', 'BELEM', -1.4558, -48.4902, 'America/Belem', 1499641, FALSE, 'Norte'),
('076', 'BR', 'BRA', 'Brasil', 'MA', 'Maranhão', 'SAO LUIS', -2.5307, -44.3068, 'America/Fortaleza', 1108975, FALSE, 'Nordeste'),
('076', 'BR', 'BRA', 'Brasil', 'SC', 'Santa Catarina', 'FLORIANOPOLIS', -27.5954, -48.5480, 'America/Sao_Paulo', 508826, FALSE, 'Sul'),
('076', 'BR', 'BRA', 'Brasil', 'ES', 'Espírito Santo', 'VITORIA', -20.2976, -40.2958, 'America/Sao_Paulo', 365855, FALSE, 'Sudeste'),

-- Outras cidades importantes
('076', 'BR', 'BRA', 'Brasil', 'SP', 'São Paulo', 'CAMPINAS', -22.9099, -47.0626, 'America/Sao_Paulo', 1213792, FALSE, 'Sudeste'),
('076', 'BR', 'BRA', 'Brasil', 'SP', 'São Paulo', 'GUARULHOS', -23.4543, -46.5337, 'America/Sao_Paulo', 1392121, FALSE, 'Sudeste'),
('076', 'BR', 'BRA', 'Brasil', 'SP', 'São Paulo', 'SANTOS', -23.9608, -46.3336, 'America/Sao_Paulo', 433656, FALSE, 'Sudeste'),
('076', 'BR', 'BRA', 'Brasil', 'RJ', 'Rio de Janeiro', 'NITEROI', -22.8833, -43.1036, 'America/Sao_Paulo', 515317, FALSE, 'Sudeste'),
('076', 'BR', 'BRA', 'Brasil', 'MG', 'Minas Gerais', 'UBERLANDIA', -18.9186, -48.2772, 'America/Sao_Paulo', 699097, FALSE, 'Sudeste'),

-- Países vizinhos (centroids)
('032', 'AR', 'ARG', 'Argentina', NULL, NULL, 'BUENOS AIRES', -34.6037, -58.3816, 'America/Argentina/Buenos_Aires', 15000000, TRUE, NULL),
('858', 'UY', 'URY', 'Uruguai', NULL, NULL, 'MONTEVIDEO', -34.9011, -56.1645, 'America/Montevideo', 1800000, TRUE, NULL),
('600', 'PY', 'PRY', 'Paraguai', NULL, NULL, 'ASUNCION', -25.2637, -57.5759, 'America/Asuncion', 525000, TRUE, NULL),
('152', 'CL', 'CHL', 'Chile', NULL, NULL, 'SANTIAGO', -33.4489, -70.6693, 'America/Santiago', 6000000, TRUE, NULL),
('068', 'BO', 'BOL', 'Bolívia', NULL, NULL, 'LA PAZ', -16.4897, -68.1193, 'America/La_Paz', 900000, TRUE, NULL),
('604', 'PE', 'PER', 'Peru', NULL, NULL, 'LIMA', -12.0464, -77.0428, 'America/Lima', 10000000, TRUE, NULL),
('170', 'CO', 'COL', 'Colômbia', NULL, NULL, 'BOGOTA', 4.7110, -74.0721, 'America/Bogota', 8000000, TRUE, NULL),
('840', 'US', 'USA', 'Estados Unidos', NULL, NULL, 'NEW YORK', 40.7128, -74.0060, 'America/New_York', 8336817, FALSE, NULL),
('840', 'US', 'USA', 'Estados Unidos', NULL, NULL, 'MIAMI', 25.7617, -80.1918, 'America/New_York', 467963, FALSE, NULL),
('840', 'US', 'USA', 'Estados Unidos', NULL, NULL, 'LOS ANGELES', 34.0522, -118.2437, 'America/Los_Angeles', 3979576, FALSE, NULL),

-- Países europeus (centroids)
('620', 'PT', 'PRT', 'Portugal', NULL, NULL, 'LISBOA', 38.7223, -9.1393, 'Europe/Lisbon', 505526, TRUE, NULL),
('724', 'ES', 'ESP', 'Espanha', NULL, NULL, 'MADRID', 40.4168, -3.7038, 'Europe/Madrid', 3223334, TRUE, NULL),
('250', 'FR', 'FRA', 'França', NULL, NULL, 'PARIS', 48.8566, 2.3522, 'Europe/Paris', 2161000, TRUE, NULL),
('826', 'GB', 'GBR', 'Reino Unido', NULL, NULL, 'LONDON', 51.5074, -0.1278, 'Europe/London', 8982000, TRUE, NULL),
('276', 'DE', 'DEU', 'Alemanha', NULL, NULL, 'BERLIN', 52.5200, 13.4050, 'Europe/Berlin', 3644826, TRUE, NULL),
('380', 'IT', 'ITA', 'Itália', NULL, NULL, 'ROMA', 41.9028, 12.4964, 'Europe/Rome', 2873000, TRUE, NULL)

ON CONFLICT (country_code, state_code, city_name) DO UPDATE SET
    latitude = EXCLUDED.latitude,
    longitude = EXCLUDED.longitude,
    population = EXCLUDED.population,
    updated_at = CURRENT_TIMESTAMP;

-- Tabela de polígonos geográficos (para GEO_IN_POLYGON)
CREATE TABLE IF NOT EXISTS geo_polygon (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL UNIQUE,          -- Nome do polígono (ex: BRASIL, SAO_PAULO_ESTADO)
    description VARCHAR(500),                    -- Descrição
    polygon_type VARCHAR(50) NOT NULL,           -- Tipo: COUNTRY, STATE, CITY, CUSTOM
    country_code VARCHAR(3),                     -- Código do país (se aplicável)
    state_code VARCHAR(10),                      -- Código do estado (se aplicável)
    -- Polígono armazenado como JSON array de pontos [[lat1,lon1],[lat2,lon2],...]
    polygon_points JSONB NOT NULL,
    -- Bounding box para otimização de busca
    min_lat DECIMAL(10, 7),
    max_lat DECIMAL(10, 7),
    min_lon DECIMAL(10, 7),
    max_lon DECIMAL(10, 7),
    enabled BOOLEAN DEFAULT TRUE,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Índices para polígonos
CREATE INDEX IF NOT EXISTS idx_geo_polygon_name ON geo_polygon(name);
CREATE INDEX IF NOT EXISTS idx_geo_polygon_type ON geo_polygon(polygon_type);
CREATE INDEX IF NOT EXISTS idx_geo_polygon_bbox ON geo_polygon(min_lat, max_lat, min_lon, max_lon);

-- Seed de polígonos básicos (bounding boxes simplificados)
INSERT INTO geo_polygon (name, description, polygon_type, country_code, polygon_points, min_lat, max_lat, min_lon, max_lon) VALUES
('BRASIL', 'Território brasileiro (bounding box)', 'COUNTRY', '076', 
 '[[-33.75, -73.99], [-33.75, -28.85], [5.27, -28.85], [5.27, -73.99], [-33.75, -73.99]]'::jsonb,
 -33.75, 5.27, -73.99, -28.85),
 
('SAO_PAULO_ESTADO', 'Estado de São Paulo (bounding box)', 'STATE', '076',
 '[[-25.31, -53.11], [-25.31, -44.16], [-19.78, -44.16], [-19.78, -53.11], [-25.31, -53.11]]'::jsonb,
 -25.31, -19.78, -53.11, -44.16),

('RIO_DE_JANEIRO_ESTADO', 'Estado do Rio de Janeiro (bounding box)', 'STATE', '076',
 '[[-23.37, -44.89], [-23.37, -40.96], [-20.76, -40.96], [-20.76, -44.89], [-23.37, -44.89]]'::jsonb,
 -23.37, -20.76, -44.89, -40.96),

('AMERICA_DO_SUL', 'América do Sul (bounding box)', 'CUSTOM', NULL,
 '[[-56.0, -82.0], [-56.0, -34.0], [13.0, -34.0], [13.0, -82.0], [-56.0, -82.0]]'::jsonb,
 -56.0, 13.0, -82.0, -34.0)

ON CONFLICT (name) DO UPDATE SET
    polygon_points = EXCLUDED.polygon_points,
    min_lat = EXCLUDED.min_lat,
    max_lat = EXCLUDED.max_lat,
    min_lon = EXCLUDED.min_lon,
    max_lon = EXCLUDED.max_lon,
    updated_at = CURRENT_TIMESTAMP;

-- Comentários
COMMENT ON TABLE geo_reference IS 'Tabela de referência geográfica para enriquecimento de localização de transações';
COMMENT ON TABLE geo_polygon IS 'Polígonos geográficos para verificação de GEO_IN_POLYGON';
COMMENT ON COLUMN geo_reference.country_code IS 'Código ISO 3166-1 numérico (ex: 076 = Brasil)';
COMMENT ON COLUMN geo_polygon.polygon_points IS 'Array JSON de pontos [lat, lon] formando o polígono fechado';
