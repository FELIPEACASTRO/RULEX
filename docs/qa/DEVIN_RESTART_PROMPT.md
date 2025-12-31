# DEVIN RESTART PROMPT - RULEX HARDCORE AUDIT

## Contexto Rápido
Você é o Devin retomando uma auditoria HARDCORE do RULEX. O objetivo é atingir 10/10 em todos os domínios.

## Estado Atual
- **Branch**: cursor/rulex-project-review-1c58
- **Git Status**: CLEAN (commit e1c6dec)
- **Passada Atual**: 3 (Implementação)
- **Scorecard**: 7.7/10 (precisa chegar a 10/10)

## O Que Foi Feito
1. ✅ PASSADA 1 - Auditoria Estática completa
   - EXTREME_CAPABILITIES_MAP.md atualizado
   - ENDPOINTS_REAL_MAP.md criado
   - SECURITY_RBAC_MAP.md criado
   - HARDCORE_SCORECARD.md criado (7.7/10)
   - GAPS_REGISTER.md criado (16 gaps: 4 P0, 7 P1, 5 P2)

2. ✅ PASSADA 2 - Auditoria de Integração completa
   - Docker Compose + Postgres funcionando
   - Flyway V1-V16 aplicadas
   - CRUD regras simples funcionando
   - Motor de regras funcionando
   - RBAC (401/403/200) verificado

## Próximos Passos (PASSADA 3)
1. **GAP-P0-01**: Implementar RuleFormDialog ou integrar ComplexRuleBuilder
2. **GAP-P0-02**: Adicionar operadores avançados ao popup simples
3. **GAP-P0-03**: Ativar constraint CHECK em V12 (com backfill)
4. **GAP-P0-04**: Adicionar @Version para optimistic locking
5. **GAP-P1-01**: Implementar limites anti-abuso

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
