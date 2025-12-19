# Matriz de Cobertura — HML

Legenda:
- **COBERTO**: existe request no Insomnia + evidência de resposta esperada (e/ou teste automatizado relevante).
- **PARCIAL**: existe request, mas faltam cenários negativos/idempotência/evidência.
- **NÃO COBERTO**: sem request/evidência.

## 1) API Java (Spring)

| Grupo | Endpoint | Cobertura | Evidência |
|---|---|---:|---|
| Transactions | POST /api/transactions/analyze | COBERTO | Insomnia (idempotência + validação) |
| Transactions | POST /api/transactions/analyze-advanced | COBERTO | Insomnia |
| Transactions | GET /api/transactions | COBERTO | Insomnia (filtros + datas inválidas) |
| Transactions | GET /api/transactions/{id} | COBERTO | Insomnia (404 estruturado) |
| Transactions | GET /api/transactions/external/{externalId} | COBERTO | Insomnia (404 estruturado) |
| Rules | GET /api/rules | COBERTO | Insomnia |
| Rules | GET /api/rules/{id} | COBERTO | Insomnia (404 estruturado) |
| Rules | POST /api/rules | COBERTO | Insomnia (400 validação, 409 conflito) |
| Rules | PUT /api/rules/{id} | COBERTO | Insomnia |
| Rules | DELETE /api/rules/{id} | COBERTO | Insomnia |
| Rules | PATCH /api/rules/{id}/toggle | COBERTO | Insomnia |
| Rules | GET /api/rules/enabled/{enabled} | COBERTO | Insomnia |
| Rules | GET /api/rules/{id}/history | COBERTO | Insomnia |
| Audit | GET /api/audit | COBERTO | Insomnia (datas inválidas) |
| Audit | GET /api/audit/transaction/{transactionId} | COBERTO | Insomnia |
| Metrics | GET /api/metrics | COBERTO | Insomnia |
| Metrics | GET /api/metrics/mcc | COBERTO | Insomnia |
| Metrics | GET /api/metrics/merchant | COBERTO | Insomnia |
| Metrics | GET /api/metrics/timeline | COBERTO | Insomnia |
| Homolog | POST /api/homolog/rules | COBERTO | Insomnia |
| Homolog | GET /api/homolog/rules/{ruleId}/latest | COBERTO | Insomnia (404 estruturado) |
| Homolog | POST /api/homolog/rules/versions/{ruleVersionId}/publish | COBERTO | Insomnia |
| Homolog | POST /api/homolog/rules/{ruleId}/rollback/{version} | COBERTO | Insomnia |
| Homolog | POST /api/homolog/rulesets | COBERTO | Insomnia |
| Homolog | POST /api/homolog/rulesets/versions/{ruleSetVersionId}/publish | COBERTO | Insomnia |
| Homolog | POST /api/homolog/rulesets/activate | COBERTO | Insomnia |
| Homolog | POST /api/homolog/simulations/run | COBERTO | Insomnia |

## 2) API Node (tRPC)

| Grupo | Procedure | Cobertura | Evidência |
|---|---|---:|---|
| system | system.health | COBERTO | Insomnia (tRPC query) |
| system | system.notifyOwner | COBERTO | Insomnia (requer admin cookie) |
| auth | auth.me | COBERTO | Insomnia (com/sem cookie) |
| auth | auth.logout | COBERTO | Insomnia |
| rules | rules.list | COBERTO | Insomnia |
| rules | rules.listActive | COBERTO | Insomnia |
| rules | rules.getById | COBERTO | Insomnia |
| rules | rules.create | COBERTO | Insomnia (requer cookie) |
| rules | rules.update | COBERTO | Insomnia (requer cookie) |
| rules | rules.delete | COBERTO | Insomnia (requer cookie) |
| rules | rules.toggle | COBERTO | Insomnia (requer cookie) |
| rules | rules.history | COBERTO | Insomnia |
| audit | audit.list | COBERTO | Insomnia |
| metrics | metrics.get | COBERTO | Insomnia |

### Bloqueios P0/P1
- P0: nenhum identificado após padronização de erros e idempotência no Java.
- P1: autenticação/admin do tRPC depende de cookie via OAuth; evidência de admin pode exigir seed de usuário com role admin.
