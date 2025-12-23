# Feature Catalog (RULEX)

Fonte da verdade (prova material):
- Migrations Flyway:
  - `backend/src/main/resources/db/migration/V9__feature_definitions.sql` (tabela `feature_definitions` + seeds)
  - `backend/src/main/resources/db/migration/V8__enterprise_side_tables.sql` (tabelas `feature_store`, `velocity_store`, `holidays`, `graph_edges`, etc.)
- API Backend:
  - `backend/src/main/java/com/rulex/v31/feature/FeatureCatalogController.java`

## Objetivo

Expor, de forma determinística e auditável, o catálogo de features disponíveis para construção de regras duras (hard rules).

Regras críticas:
- Sem ML/AutoML/auto-tuning.
- Mesma entrada → mesma saída.
- O payload de entrada não é alterado para fins de avaliação.

## Endpoint

`GET /api/feature-catalog`

Filtros (query params):
- `featureType` (ex.: `TEMPORAL`, `VELOCITY`, `PAYLOAD_FIELD`)
- `entityType` (ex.: `card`, `merchant`, `customer`)
- `source` (ex.: `payload`, `velocity_store`, `feature_store`, `runtime`)

Outros endpoints:
- `GET /api/feature-catalog/{featureName}`
- `GET /api/feature-catalog/types`
- `GET /api/feature-catalog/entity-types`
- `GET /api/feature-catalog/sources`

## Contrato de FeatureDefinition

O backend retorna uma lista de objetos com os seguintes campos (ver `FeatureCatalogController.toResponse()`):
- `featureName`
- `featureType`
- `entityType`
- `windowName`
- `formula`
- `description`
- `source`
- `dataType`
- `allowedOperators`
- `refreshStrategy`
- `version`

## Exemplos de features (prova via seeds)

Os seeds estão em `backend/src/main/resources/db/migration/V9__feature_definitions.sql`.

- Temporais (exemplos): `is_weekend`, `is_holiday_br`, `is_business_day_br`, `in_high_risk_hours`
- Velocity/contadores (exemplos): `txn_count_1h`, `txn_count_24h`, `txn_sum_1h`, `txn_sum_24h`, `unique_merchants_24h`, `unique_countries_24h`

## Observação de auditoria

A auditoria de execução (incluindo `featuresUsed`) está implementada no fluxo V3.1 de simulação/avaliação e é persistida no modelo de auditoria V3.1 (ver `backend/src/main/resources/db/migration/V6__v31_exec_log_field_dictionary.sql` e `backend/src/main/resources/db/migration/V10__governance_thresholds_lists_audit.sql`).
