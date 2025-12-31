# DEVIN RESTART PROMPT - RULEX HARDCORE AUDIT

## Contexto Rápido
Você é o Devin retomando uma auditoria HARDCORE do RULEX. O objetivo é atingir 10/10 em todos os domínios.

## Estado Atual
- **Branch**: cursor/rulex-project-review-1c58
- **Git Status**: CLEAN (commit 7c7c6c8)
- **Passada Atual**: 1 (Auditoria Estática)
- **Próximo Passo**: Completar mapeamento de capacidades

## O Que Foi Feito
1. ✅ Commit das mudanças pendentes (V15 migration, RuleConditionGroup entity)
2. ✅ Criação dos arquivos de checkpoint
3. ⏳ Iniciando mapeamento de operadores

## Próximos Passos Imediatos
1. Completar EXTREME_CAPABILITIES_MAP.md com evidências do código
2. Criar ENDPOINTS_REAL_MAP.md
3. Criar SECURITY_RBAC_MAP.md
4. Criar HARDCORE_SCORECARD.md
5. Criar GAPS_REGISTER.md

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
