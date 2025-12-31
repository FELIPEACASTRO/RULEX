# DEVIN EXECUTION PLAN - RULEX HARDCORE AUDIT

## Objetivo
Auditar e implementar/ajustar/corrigir RULEX para atingir 10/10 em todos os domínios.

## Passadas

### PASSADA 1 - AUDITORIA ESTÁTICA ⏳
- [ ] Mapear operadores do builder avançado
- [ ] Mapear operadores do popup simples
- [ ] Verificar engines (homolog/v31/complex)
- [ ] Auditar persistência (tables + migrations + constraints)
- [ ] Mapear RBAC (roles x endpoints)
- [ ] Verificar OpenAPI/Swagger

**Entregáveis:**
- docs/qa/EXTREME_CAPABILITIES_MAP.md
- docs/qa/ENDPOINTS_REAL_MAP.md
- docs/qa/SECURITY_RBAC_MAP.md
- docs/qa/HARDCORE_SCORECARD.md
- docs/qa/GAPS_REGISTER.md

### PASSADA 2 - AUDITORIA DE INTEGRAÇÃO
- [ ] Subir stack com DB zerado
- [ ] Validar Flyway migrations
- [ ] Testar CRUD regras simples
- [ ] Testar CRUD regras complexas
- [ ] Testar simulação
- [ ] Testar RBAC (401/403/200)

### PASSADA 3 - IMPLEMENTAÇÃO
- [ ] Frontend: popup simples → avançado
- [ ] Backend: consistência V12/V13 vs entidades
- [ ] APIs: padronizar paths
- [ ] GEO + VELOCITY: provar ponta-a-ponta
- [ ] Limites anti-abuso
- [ ] Concorrência/versionamento

### PASSADA 4 - EXTREME RULES TEST SUITE
- [ ] Unit tests para cada operador
- [ ] Integration tests (API + DB)
- [ ] Contract tests (OpenAPI)
- [ ] E2E Playwright
- [ ] 15+ regras extremamente complexas

## Status Atual
**Passada 1 em andamento** - Mapeando capacidades reais do sistema.

## Última Atualização
2024-12-31T21:00:00Z
