# DEVIN EXECUTION PLAN - RULEX HARDCORE AUDIT v2

## Objetivo
Auditar e implementar/ajustar/corrigir RULEX para atingir 10/10 em todos os domínios.

## Estado Atual (2024-12-31T22:55:00Z)

### Stack
- ✅ Docker Compose rodando (postgres, backend, web)
- ✅ Flyway V1-V18 aplicadas com sucesso
- ✅ Backend: 198 testes passando
- ✅ Frontend: 198 testes passando
- ✅ RBAC funcionando (401/403/200)

### Gaps Fechados
| ID | Descrição | Commit |
|----|-----------|--------|
| GAP-P0-01 | RuleFormDialog implementado | b9444c9 |
| GAP-P0-02 | 52 operadores no popup | 8fc0d41 |
| GAP-P0-03 | Constraint CHECK ativada | V18 |
| GAP-P0-04 | Optimistic locking | a92f167 |
| GAP-P1-01 | Limites anti-abuso | 88753c6 |

### Gaps Abertos (Prioridade)

#### P0 - Crítico
| ID | Descrição | Ação |
|----|-----------|------|
| NOVO | RuleFormDialog não integrado em Rules.tsx | Integrar componente |

#### P1 - Importante
| ID | Descrição | Ação |
|----|-----------|------|
| GAP-P1-02 | Falta E2E Playwright | Criar testes |
| GAP-P1-03 | Falta testes unitários por operador | Criar testes |
| GAP-P1-06 | Frontend 401/403 | Verificar/implementar |
| GAP-P1-07 | Preview JSON | Verificar se já existe |

#### P2 - Desejável (Skip para 10/10)
| ID | Descrição | Decisão |
|----|-----------|---------|
| GAP-P2-01 | JWT vs Basic Auth | Skip (produção) |
| GAP-P2-02 | OpenTelemetry | Skip (observability) |
| GAP-P2-03 | Dashboards Grafana | Skip (observability) |

---

## Plano de Ação para 10/10

### FASE 1: Integração RuleFormDialog (P0)
1. [ ] Integrar RuleFormDialog na página Rules.tsx
2. [ ] Testar criação de regra via dialog
3. [ ] Testar edição de regra via dialog
4. [ ] Verificar preview JSON funciona

### FASE 2: Testes E2E (P1)
1. [ ] Criar testes Playwright para CRUD de regras
2. [ ] Criar testes para RBAC (403/200)
3. [ ] Criar testes para regras complexas

### FASE 3: Testes Unitários de Operadores (P1)
1. [ ] Criar testes para operadores básicos (EQ, NEQ, GT, etc.)
2. [ ] Criar testes para operadores de string (CONTAINS, REGEX, etc.)
3. [ ] Criar testes para operadores GEO
4. [ ] Criar testes para operadores VELOCITY

### FASE 4: Validação Final
1. [ ] Rodar todos os testes (FE + BE + E2E)
2. [ ] Atualizar scorecard final
3. [ ] Documentar evidências

---

## Critérios de Aceite (10/10)
- [ ] docker-compose up com DB zerado funciona
- [ ] CRUD completo de regras simples e complexas
- [ ] RuleFormDialog integrado e funcional
- [ ] RBAC provado com testes
- [ ] Testes automatizados todos verdes
- [ ] Scorecard final: todos domínios 10/10

---

## Última Atualização
2024-12-31T22:55:00Z
