# DEVIN EXECUTION PLAN - RULEX HARDCORE AUDIT

## Objetivo
Auditar e implementar/ajustar/corrigir RULEX para atingir 10/10 em todos os domínios.

## Passadas

### PASSADA 1 - AUDITORIA ESTÁTICA ✅ COMPLETA
- [x] Mapear operadores do builder avançado (52 operadores)
- [x] Mapear operadores do popup simples (52 operadores após commit 8fc0d41)
- [x] Verificar engines (homolog/v31/complex)
- [x] Auditar persistência (V1-V16 migrations)
- [x] Mapear RBAC (ADMIN/ANALYST roles)
- [x] Verificar OpenAPI/Swagger

**Entregáveis:**
- ✅ docs/qa/EXTREME_CAPABILITIES_MAP.md
- ✅ docs/qa/ENDPOINTS_REAL_MAP.md
- ✅ docs/qa/SECURITY_RBAC_MAP.md
- ✅ docs/qa/HARDCORE_SCORECARD.md
- ✅ docs/qa/GAPS_REGISTER.md

### PASSADA 2 - AUDITORIA DE INTEGRAÇÃO ⏳ EM ANDAMENTO
- [x] Subir stack com DB zerado
- [x] Validar Flyway migrations (V1-V16 OK)
- [x] Testar CRUD regras simples (POST/GET OK)
- [ ] Testar CRUD regras complexas (PUT/DELETE pendente)
- [x] Testar simulação (POST /api/evaluate OK)
- [x] Testar RBAC (401/403/200 OK)

### PASSADA 3 - IMPLEMENTAÇÃO 🔲 NÃO INICIADA
- [ ] Frontend: popup simples → avançado (GAP-P0-01)
- [ ] Backend: consistência V12/V13 vs entidades (GAP-P0-03)
- [ ] APIs: padronizar paths
- [ ] GEO + VELOCITY: provar ponta-a-ponta
- [x] Limites anti-abuso (commit 88753c6)
- [x] Concorrência/versionamento (commit 2fcef9b)

### PASSADA 4 - EXTREME RULES TEST SUITE 🔲 NÃO INICIADA
- [ ] Unit tests para cada operador
- [ ] Integration tests (API + DB)
- [ ] Contract tests (OpenAPI)
- [ ] E2E Playwright
- [ ] 15+ regras extremamente complexas

## Gaps Críticos (P0)
| ID | Descrição | Status |
|----|-----------|--------|
| GAP-P0-01 | RuleFormDialog incompleto | ❌ Aberto |
| GAP-P0-02 | Popup simples operadores | ✅ Fechado (8fc0d41) |
| GAP-P0-03 | Constraint CHECK V12 comentada | ❌ Aberto |
| GAP-P0-04 | Optimistic locking | ✅ Fechado (2fcef9b) |

## Status Atual
**Passada 2 em andamento** - Validando integração ponta-a-ponta.

## Próximos Passos
1. Completar testes de integração (PUT/DELETE)
2. Implementar RuleFormDialog completo (GAP-P0-01)
3. Ativar constraint CHECK em V12 (GAP-P0-03)
4. Criar suite de testes E2E

## Última Atualização
2024-12-31T22:15:00Z
