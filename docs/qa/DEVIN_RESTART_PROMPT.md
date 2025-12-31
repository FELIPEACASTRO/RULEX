# DEVIN RESTART PROMPT - RULEX HARDCORE AUDIT

## Contexto Rápido
Você é o Devin retomando uma auditoria HARDCORE do RULEX. O objetivo é atingir 10/10 em todos os domínios.

## Estado Atual
- **Branch**: cursor/rulex-project-review-1c58
- **Git Status**: CLEAN (commit 8fc0d41)
- **Passada Atual**: 2 (Integração) - finalizando
- **Scorecard**: 7.9/10
- **Gaps P0**: 2 abertos (GAP-P0-01, GAP-P0-03), 2 fechados

## O Que Foi Feito
1. ✅ PASSADA 1 - Auditoria Estática completa
   - 52 operadores mapeados (backend + frontend alinhados)
   - EXTREME_CAPABILITIES_MAP.md atualizado
   - ENDPOINTS_REAL_MAP.md criado
   - SECURITY_RBAC_MAP.md criado
   - HARDCORE_SCORECARD.md criado

2. ⏳ PASSADA 2 - Auditoria de Integração (quase completa)
   - Docker Compose + Postgres funcionando
   - Flyway V1-V16 aplicadas
   - CRUD regras simples funcionando (POST/GET OK)
   - Motor de regras funcionando
   - RBAC (401/403/200) verificado
   - **PENDENTE**: PUT/DELETE regras

3. ✅ Implementações já feitas
   - GAP-P0-02: Operadores avançados no popup (commit 8fc0d41)
   - GAP-P0-04: Optimistic locking (commit 2fcef9b)
   - GAP-P1-01: Limites anti-abuso (commit 88753c6)

## Próximos Passos
1. **Completar PASSADA 2**: Testar PUT/DELETE regras
2. **GAP-P0-01**: Implementar RuleFormDialog completo
3. **GAP-P0-03**: Ativar constraint CHECK em V12 (com backfill)
4. **PASSADA 4**: Criar suite de testes E2E

## Arquivos Chave
- Backend operators: `backend/src/main/java/com/rulex/entity/complex/RuleCondition.java`
- Frontend operators: `client/src/components/RuleFormDialog/types.ts` (atualizado!)
- Security config: `backend/src/main/java/com/rulex/config/SecurityConfig.java`
- Migrations: `backend/src/main/resources/db/migration/`

## Comandos Úteis
```bash
# Verificar status
cd ~/repos/RULEX && git status

# Rodar testes frontend (198 testes)
cd ~/repos/RULEX && pnpm test --run

# Rodar testes backend
cd ~/repos/RULEX/backend && mvn test

# Subir ambiente
cd ~/repos/RULEX && docker compose up -d --build
```

## Restrições
- NÃO alterar payload de entrada
- MANTER git limpo
- PROVAR tudo com evidências

## Última Atualização
2024-12-31T22:15:00Z
