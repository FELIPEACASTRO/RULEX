# Central de Diagramas — Solução RULEX (PASSADA 2: Consistência)

Relatório gerado automaticamente a partir de cruzamento entre fontes do repositório.

- Gerado em: 2026-01-16T21:57:12.321Z
- Context path (backend): `/api`
- JSON: `docs/DIAGRAMS_RULEX_PASSADA2.json`

## Contagens

- Backend endpoints (inventário + prefixo): 90
- OpenAPI endpoints: 18
- Insomnia requests: 113
- E2E hits contendo /api: 0
- Frontend hits contendo /api (não-test): 103

## Gaps (Backend ↔ OpenAPI)

- Backend não documentado no OpenAPI: 72
- OpenAPI sem endpoint correspondente no Backend: 0

**Backend → faltando no OpenAPI (amostra)**
- DELETE /api/complex-rules/{}
- DELETE /api/v1/complex-rules/{}
- GET /api/audit/export/csv
- GET /api/audit/transaction/{}
- GET /api/complex-rules
- GET /api/complex-rules/key/{}
- GET /api/complex-rules/{}
- GET /api/field-dictionary
- GET /api/homolog/rules/{}/latest
- GET /api/rules/approvals/history/{}
- GET /api/rules/approvals/pending
- GET /api/rules/approvals/pending/count
- GET /api/rules/approvals/pending/page
- GET /api/rules/approvals/{}
- GET /api/rules/enabled/{}
- GET /api/rules/metrics/all
- GET /api/rules/metrics/dashboard
- GET /api/rules/metrics/{}
- GET /api/rules/{}/history
- GET /api/transactions/export/csv
- GET /api/v1/complex-rules/templates
- GET /api/v1/complex-rules/templates/category/{}
- GET /api/v1/complex-rules/templates/system
- GET /api/v1/complex-rules/templates/{}
- GET /api/v1/complex-rules/{}/actions
- GET /api/v1/complex-rules/{}/conditions
- GET /api/v1/complex-rules/{}/expressions
- GET /api/v1/complex-rules/{}/fields
- GET /api/v1/complex-rules/{}/validate-depth
- GET /api/v1/complex-rules/{}/variables
- GET /api/v1/rules/export-import/export
- GET /api/v1/rules/export-import/export/complex
- GET /api/v1/rules/export-import/template/complex
- GET /api/v1/rules/export-import/template/simple
- PATCH /api/complex-rules/{}/toggle
- POST /api/complex-rules
- POST /api/complex-rules/validate
- POST /api/complex-rules/{}/duplicate
- POST /api/evaluate
- POST /api/evaluate/raw
- POST /api/homolog/rules
- POST /api/homolog/rules/versions/{}/publish
- POST /api/homolog/rules/{}/rollback/{}
- POST /api/homolog/rulesets
- POST /api/homolog/rulesets/activate
- POST /api/homolog/rulesets/versions/{}/publish
- POST /api/homolog/simulations/run
- POST /api/rules/approvals/create
- POST /api/rules/approvals/delete/{}
- POST /api/rules/approvals/update/{}
- … +22 outros

**OpenAPI → faltando no Backend (amostra)**

## Gaps (Backend ↔ Insomnia)

- Backend não coberto na coleção Insomnia: 2
- Insomnia sem endpoint correspondente no Backend: 0

**Backend → faltando no Insomnia (amostra)**
- GET /api/audit/export
- GET /api/transactions/export

**Insomnia → faltando no Backend (amostra)**

## Sinais (methodless)

- Backend paths sem match em E2E (apenas presença de "/api"): 80
- Backend paths sem match em Frontend (apenas presença de "/api"): 61

## Notas

- Comparações usam normalização por segmentos (variáveis {id} e {{ _.var }} viram {}).
- E2E/Frontend nesta etapa são heurísticos: apenas detectam literais contendo /api.
