# DEVIN RESTART PROMPT - RULEX

## Contexto Rápido
Você está auditando o RULEX, um motor de regras de fraude com React + Spring Boot.

## Estado Atual (2024-12-31T23:25:00Z)

### ✅ Concluído
- PASSADA 1: Auditoria estática completa
- PASSADA 2: Auditoria de integração completa
- PASSADA 3: Implementação de P0 completa
- Stack rodando: `docker compose up -d`
- 396 testes passando (198 FE + 198 BE)
- Score: 8.7/10

### ⏳ Em Andamento
- PASSADA 4: Test suite (expandir E2E)

### 🔲 Pendente para 10/10
1. Expandir E2E Playwright (CRUD completo, RBAC)
2. Criar testes unitários por operador (50 operadores)

## Comandos Úteis

```bash
# Verificar stack
cd ~/repos/RULEX && docker compose ps

# Rodar testes
cd ~/repos/RULEX && pnpm test --run
cd ~/repos/RULEX && mvn -f backend/pom.xml test

# Verificar migrations
docker exec rulex-postgres-1 psql -U postgres -d rulex_db -c "SELECT version, description FROM flyway_schema_history ORDER BY installed_rank;"

# Testar RBAC
curl -s -o /dev/null -w "%{http_code}" http://localhost:8080/api/rules  # 401
curl -s -o /dev/null -w "%{http_code}" -u analyst:rulex http://localhost:8080/api/rules  # 200
curl -s -o /dev/null -w "%{http_code}" -u analyst:rulex -X POST http://localhost:8080/api/rules  # 403
```

## Arquivos Importantes

- `docs/qa/HARDCORE_SCORECARD.md` - Score atual
- `docs/qa/GAPS_REGISTER.md` - Gaps abertos/fechados
- `docs/qa/DEVIN_PROGRESS.md` - Progresso detalhado
- `docs/qa/EXTREME_CAPABILITIES_MAP.md` - Capacidades do sistema

## Próximo Passo

Para atingir 10/10:
1. Criar testes E2E para CRUD completo de regras
2. Criar testes unitários para cada operador

## Credenciais Dev
- Admin: admin/rulex
- Analyst: analyst/rulex

## Git Status
Branch: cursor/rulex-project-review-1c58
Working tree: clean
