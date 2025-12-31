# DEVIN RESTART PROMPT - RULEX HARDCORE AUDIT

## Contexto Rápido
Você é o Devin retomando uma auditoria HARDCORE do RULEX. O objetivo é atingir 10/10 em todos os domínios.

## Estado Atual
- **Branch**: cursor/rulex-project-review-1c58
- **Git Status**: CLEAN (commit fcc0fbb)
- **Passada Atual**: 3 (Implementação) - em andamento
- **Scorecard**: 7.9/10 (melhorou de 7.7)
- **Gaps**: 14 abertos, 2 fechados

## O Que Foi Feito
1. ✅ PASSADA 1 - Auditoria Estática completa
   - EXTREME_CAPABILITIES_MAP.md atualizado
   - ENDPOINTS_REAL_MAP.md criado
   - SECURITY_RBAC_MAP.md criado
   - HARDCORE_SCORECARD.md criado

2. ✅ PASSADA 2 - Auditoria de Integração completa
   - Docker Compose + Postgres funcionando
   - Flyway V1-V16 aplicadas
   - CRUD regras simples funcionando
   - Motor de regras funcionando
   - RBAC (401/403/200) verificado

3. ⏳ PASSADA 3 - Implementação (parcial)
   - ✅ GAP-P0-04: Optimistic locking implementado
   - ✅ GAP-P1-01: Limites anti-abuso implementados

## Próximos Passos (PASSADA 3 - continuar)
1. **GAP-P0-01**: Implementar RuleFormDialog ou integrar ComplexRuleBuilder
2. **GAP-P0-02**: Adicionar operadores avançados ao popup simples
3. **GAP-P0-03**: Ativar constraint CHECK em V12 (com backfill)
4. **GAP-P1-02**: Criar E2E Playwright completo
5. **GAP-P1-03**: Adicionar testes unitários para cada operador

## Arquivos Chave
- Backend operators: `backend/src/main/java/com/rulex/entity/complex/RuleCondition.java`
- Frontend operators: `client/src/components/ComplexRuleBuilder/types.ts`
- Security config: `backend/src/main/java/com/rulex/config/SecurityConfig.java`
- Migrations: `backend/src/main/resources/db/migration/`

## Comandos Úteis
```bash
# Verificar status
cd ~/repos/RULEX && git status

# Rodar testes frontend
cd ~/repos/RULEX && pnpm test

# Rodar testes backend
cd ~/repos/RULEX/backend && mvn test

# Subir ambiente
cd ~/repos/RULEX && docker compose up -d --build
```

## Restrições
- NÃO alterar payload de entrada
- MANTER git limpo
- PROVAR tudo com evidências
