# DEVIN RUNBOOK - RULEX MODO HARDCORE 10/10

## Última Atualização: 2024-12-31 18:36 UTC

---

## PLANO POR FASES

### F0 - AUDITORIA DEVASTADORA ✅ EM PROGRESSO
**Status:** Mapeamento inicial completo
**Gates:**
- [x] Mapear controllers/endpoints
- [x] Mapear operadores backend
- [x] Mapear operadores frontend
- [x] Identificar gaps
- [ ] Gerar documentação de capacidades reais
- [ ] Gerar DEVIN_GAPS.md completo

### F1 - POPUP/MODAL 10/10
**Status:** Pendente
**Gates:**
- [ ] Matriz de compatibilidade (tipo × operador × formato)
- [ ] Validações UI sincronizadas com backend
- [ ] Acessibilidade WCAG/ARIA
- [ ] Segurança REGEX (ReDoS mitigation)
- [ ] Testes unitários do validador

### F2 - INTEGRAÇÃO FRONT ↔ BACK ↔ DB
**Status:** Pendente
**Gates:**
- [ ] Fluxo criar regra (curl + UI + DB)
- [ ] Fluxo editar regra
- [ ] Fluxo deletar regra
- [ ] Fluxo simular
- [ ] Coerência de contrato

### F3 - EXTREME RULES (15+)
**Status:** Pendente
**Gates:**
- [ ] 15+ regras extremas criadas
- [ ] Nesting profundo testado
- [ ] Edge cases cobertos

### F4 - VELOCITY/AGREGAÇÃO
**Status:** ✅ IMPLEMENTADO (sessão anterior)
**Gates:**
- [x] VelocityService criado
- [x] Tabelas velocity_counters e velocity_transaction_log
- [x] COUNT/SUM/AVG por janela temporal
- [ ] Testes de integração

### F5 - GEO
**Status:** ✅ IMPLEMENTADO (sessão anterior)
**Gates:**
- [x] GeoService criado
- [x] Tabelas geo_reference e geo_polygon
- [x] GEO_DISTANCE_LT/GT implementado
- [x] GEO_IN_POLYGON implementado
- [ ] Testes de integração
- [ ] Frontend suporte a GEO operators

### F6 - TESTES EXTREMOS
**Status:** Pendente
**Gates:**
- [ ] Combinatorial t-way
- [ ] Unit tests por operador
- [ ] Integration tests
- [ ] E2E Playwright

---

## COMO RODAR/REPRODUZIR

```bash
# 1. Verificar estado
cd ~/repos/RULEX && git status

# 2. Testes frontend
cd ~/repos/RULEX && pnpm test

# 3. Testes backend
cd ~/repos/RULEX/backend && mvn test

# 4. Lint
cd ~/repos/RULEX && pnpm check
cd ~/repos/RULEX/backend && mvn spotless:check

# 5. Build completo
cd ~/repos/RULEX && pnpm build
cd ~/repos/RULEX/backend && mvn package -DskipTests

# 6. Docker (se disponível)
cd ~/repos/RULEX && cp .env.example .env && docker compose up -d --build
```

---

## ÚLTIMO CHECKPOINT CONCLUÍDO

**Fase:** F0 - Auditoria (parcial)
**Data:** 2024-12-31 18:36 UTC
**O que foi feito:**
- Mapeamento de controllers (16 controllers)
- Mapeamento de operadores backend (42 operadores)
- Mapeamento de operadores frontend (gaps identificados)
- Testes passando: 165 frontend + 171 backend = 336 total

---

## PRÓXIMO PASSO

1. Completar documentação de capacidades reais
2. Gerar DEVIN_GAPS.md com todos os gaps P0/P1/P2
3. Iniciar F1 - Popup/Modal 10/10

---

## GAPS CRÍTICOS IDENTIFICADOS

| ID | Severidade | Descrição |
|----|------------|-----------|
| GAP-001 | P0 | Frontend ComplexRuleBuilder falta GEO_* operators |
| GAP-002 | P0 | Frontend usa MATCHES_REGEX vs backend REGEX |
| GAP-003 | P0 | Frontend usa IS_NOT_NULL vs backend NOT_NULL |
| GAP-004 | P1 | RuleFormDialog (simples) falta maioria dos operadores avançados |
| GAP-005 | P1 | Sem validação de ReDoS no REGEX |
| GAP-006 | P2 | Sem testes E2E Playwright |

---

## ARQUIVOS DE CONTROLE

- `/AGENTS.md` - Guia para agentes ✅
- `/DEVIN_RUNBOOK.md` - Este arquivo ✅
- `/DEVIN_GAPS.md` - Lista de gaps (criar)
- `/DEVIN_COMMAND_LOG.md` - Log de comandos (criar)
