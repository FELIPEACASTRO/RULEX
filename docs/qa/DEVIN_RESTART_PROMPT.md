# DEVIN RESTART PROMPT - RULEX HARDCORE AUDIT

## Contexto Rápido
Você é o Devin retomando uma auditoria HARDCORE do RULEX. O objetivo é atingir 10/10 em todos os domínios.

## Estado Atual
- **Branch**: cursor/rulex-project-review-1c58
- **Git Status**: CLEAN (commit c525a79)
- **Passada Atual**: 3 (Implementação) - quase completa
- **Scorecard**: 8.1/10
- **Gaps P0**: 1 aberto (GAP-P0-03), 3 fechados

## O Que Foi Feito
1. ✅ PASSADA 1 - Auditoria Estática completa
2. ✅ PASSADA 2 - Auditoria de Integração completa
   - Docker Compose + Postgres + Flyway V1-V17
   - CRUD completo (POST/GET/PUT/DELETE)
   - Optimistic locking funcionando (409 em conflito)
   - RBAC verificado (401/403/200)

3. ✅ Implementações concluídas
   - GAP-P0-01: RuleFormDialog completo (commit b9444c9)
   - GAP-P0-02: 52 operadores no popup (commit 8fc0d41)
   - GAP-P0-04: Optimistic locking (commit a92f167)
   - GAP-P1-01: Limites anti-abuso (commit 88753c6)

## Próximos Passos
1. **GAP-P0-03**: Ativar constraint CHECK em V12 (com backfill)
2. **PASSADA 4**: Criar suite de testes E2E
3. **GAP-P1-02**: Testes E2E Playwright

## Arquivos Chave
- RuleFormDialog: `client/src/components/RuleFormDialog/RuleFormDialog.tsx`
- Backend operators: `backend/src/main/java/com/rulex/entity/complex/RuleCondition.java`
- Migrations: `backend/src/main/resources/db/migration/`

## Comandos Úteis
```bash
# Verificar status
cd ~/repos/RULEX && git status

# Rodar testes frontend (198 testes)
cd ~/repos/RULEX && pnpm test --run

# Rodar testes backend (198 testes)
cd ~/repos/RULEX && mvn -f backend/pom.xml test

# Subir ambiente
cd ~/repos/RULEX && docker compose up -d --build
```

## Restrições
- NÃO alterar payload de entrada
- MANTER git limpo
- PROVAR tudo com evidências

## Última Atualização
2024-12-31T22:45:00Z
