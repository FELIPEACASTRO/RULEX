# RULE_MODAL_TESTS.md - Documentação de Testes do Modal de Regras

**Data:** 2024-12-31  
**Versão:** 1.0.0

---

## 1. TESTES UNITÁRIOS

### Arquivo: `client/src/pages/Rules.test.tsx`

| Teste | Descrição | Status |
|-------|-----------|--------|
| `creates a rule via popup and posts all required fields` | Cria regra com todos os campos obrigatórios | ✅ PASS |
| `edits a rule via popup; ruleName is read-only; uses PUT` | Edita regra, verifica ruleName desabilitado | ✅ PASS |
| `toggles a rule enabled/disabled via PATCH` | Toggle de status via PATCH | ✅ PASS |
| `snapshot: popup visual regression` | Snapshot do modal | ✅ PASS |

### Cobertura de Validações

| Validação | Testada? | Arquivo |
|-----------|----------|---------|
| ruleName obrigatório | ✅ | Rules.test.tsx |
| ruleName formato UPPER_SNAKE_CASE | ⚠️ Parcial | schema.ts (regex) |
| threshold 0-1000 | ⚠️ Parcial | schema.ts |
| weight 0-100 | ⚠️ Parcial | schema.ts |
| REGEX válida | ✅ | schema.ts (refine) |
| BETWEEN formato | ✅ | schema.ts (refine) |
| IN/NOT_IN lista | ✅ | schema.ts (refine) |

---

## 2. TESTES E2E (Playwright)

### Arquivo: `e2e/rules.spec.ts`

| Teste | Descrição | Status |
|-------|-----------|--------|
| `navigates to rules page` | Navegação para página de regras | ✅ PASS |
| `displays rules list` | Exibe lista de regras | ✅ PASS |
| `can open create rule dialog` | Abre modal de criação | ✅ PASS |
| `rules have toggle switch` | Verifica toggle switches | ✅ PASS |

### Testes E2E Recomendados (Futuros)

- [ ] Criar regra válida e verificar na lista
- [ ] Tentar salvar regra com nome inválido (validação)
- [ ] Tentar salvar regra com REGEX inválida
- [ ] Editar regra existente
- [ ] Deletar regra com confirmação
- [ ] Fechar modal com alterações não salvas (warning)
- [ ] Simular transação com regra criada

---

## 3. EXECUÇÃO DOS TESTES

### Comandos

```bash
# Testes unitários
pnpm test

# Testes E2E (requer backend rodando)
pnpm exec playwright test

# Testes com cobertura
pnpm test -- --coverage
```

### Resultados da Última Execução

```
 Test Files  9 passed (9)
      Tests  29 passed (29)
   Duration  2.63s
```

---

## 4. MOCKS UTILIZADOS

### mockRulesApi

```typescript
function mockRulesApi(initialContent: any[] = []) {
  // Simula endpoints:
  // - GET /api/rules (lista)
  // - POST /api/rules (criar)
  // - PUT /api/rules/{id} (atualizar)
  // - PATCH /api/rules/{id}/toggle (toggle)
  // - GET /api/field-dictionary (campos)
}
```

---

## 5. GAPS DE TESTE IDENTIFICADOS

### Alta Prioridade

1. **Validação de condições** - Testar validateValueByOperator
2. **Conflito de versão** - Testar resposta 409
3. **Unsaved changes** - Testar AlertDialog de warning

### Média Prioridade

1. **Limite de condições** - Testar MAX_CONDITIONS
2. **Erros de API** - Testar mensagens amigáveis
3. **Loading states** - Testar spinner durante save

---

## 6. PRÓXIMOS PASSOS

1. Adicionar testes para validateValueByOperator
2. Adicionar testes E2E para fluxo completo
3. Aumentar cobertura de código para >80%
4. Adicionar testes de acessibilidade (axe-core)

---

**Última atualização:** 2024-12-31T13:00:00Z
