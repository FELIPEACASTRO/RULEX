# RULE_MODAL_TEST_PLAN.md - Plano de Testes

**Data:** 2024-12-31
**Versão:** 1.0

---

## 1. VISÃO GERAL

### 1.1 Escopo

Testes do modal de criação e edição de regras de fraude no sistema RULEX.

### 1.2 Tipos de Teste

| Tipo | Framework | Localização |
|------|-----------|-------------|
| Unit | Vitest | `client/src/components/RuleFormDialog/schema.test.ts` |
| Integration | Vitest + RTL | `client/src/pages/Rules.test.tsx` |
| E2E | Playwright | `e2e/rules.spec.ts` |

---

## 2. TESTES UNITÁRIOS (schema.test.ts)

### 2.1 Cobertura

| Categoria | Testes | Status |
|-----------|--------|--------|
| STR - Strings Maliciosas | 9 | ✅ PASS |
| NUM - Números Extremos | 5 | ✅ PASS |
| REG - Regex Maliciosas | 4 | ✅ PASS |
| BET - Operador BETWEEN | 8 | ✅ PASS |
| IN - Operador IN/NOT_IN | 6 | ✅ PASS |
| UNA - Operadores Unários | 5 | ✅ PASS |
| FLD - Campos e Tipos | 2 | ✅ PASS |
| JSON - Parameters | 5 | ✅ PASS |
| Casos Válidos | 2 | ✅ PASS |
| Limites | 3 | ✅ PASS |
| **TOTAL** | **49** | **✅ PASS** |

### 2.2 Casos de Teste

#### STR - Strings Maliciosas
- STR-01: ruleName vazio
- STR-02: ruleName whitespace
- STR-03: ruleName unicode invisível
- STR-04: ruleName muito longo (>100)
- STR-05: ruleName lowercase
- STR-06: ruleName começa com número
- STR-07: ruleName com hífen
- STR-08: ruleName com espaço
- STR-11: description muito longa (>500)

#### NUM - Números Extremos
- NUM-01: threshold negativo
- NUM-02: threshold > 1000
- NUM-05: threshold decimal
- NUM-07: weight negativo
- NUM-08: weight > 100

#### REG - Regex Maliciosas
- REG-01: regex inválida
- REG-02: regex ReDoS (aceita - sem proteção)
- REG-03: regex match-all
- REG-06: regex null byte

#### BET - Operador BETWEEN
- BET-01: valores invertidos
- BET-02: apenas 1 valor
- BET-03: 3 valores
- BET-04: não numérico para campo number
- BET-05: formato alternativo (..)
- BET-06: negativo
- BET-07: com espaço (trim)
- BET-08: valores vazios

#### IN - Operador IN/NOT_IN
- IN-01: lista vazia
- IN-02: 1 item
- IN-04: vazio no meio
- IN-05: formato array
- IN-06: strings
- IN-09: não numérico para campo number

#### UNA - Operadores Unários
- UNA-01: IS_NULL com valor
- UNA-02: IS_NOT_NULL com valor
- UNA-03: IS_TRUE com valor
- UNA-04: IS_FALSE com valor
- UNA-05: IS_NULL sem valor

#### FLD - Campos e Tipos
- FLD-01: campo vazio
- FLD-03: valor não numérico para comparação

#### JSON - Parameters
- JSON-01: JSON inválido
- JSON-02: null
- JSON-03: array vazio
- JSON-04: objeto vazio
- JSON-05: aspas simples

---

## 3. TESTES DE INTEGRAÇÃO (Rules.test.tsx)

### 3.1 Cobertura

| Teste | Descrição | Status |
|-------|-----------|--------|
| creates a rule via popup | Criar regra com todos os campos | ✅ PASS |
| edits a rule via popup | Editar regra, ruleName readonly | ✅ PASS |
| toggles a rule | Toggle enabled/disabled via PATCH | ✅ PASS |
| snapshot | Regressão visual do modal | ✅ PASS |

### 3.2 Verificações

- POST /api/rules com payload correto
- PUT /api/rules/{id} com payload correto
- PATCH /api/rules/{id}/toggle
- Dialog fecha após sucesso
- ruleName desabilitado em edição

---

## 4. TESTES E2E (rules.spec.ts)

### 4.1 Cobertura

| Teste | Descrição | Status |
|-------|-----------|--------|
| navigates to rules page | Navegação para /rules | ✅ PASS |
| displays rules list | Lista de regras visível | ✅ PASS |
| can open create rule dialog | Abrir modal de criação | ✅ PASS |
| rules have toggle switch | Switches de enable/disable | ✅ PASS |

### 4.2 Fluxo Completo (a implementar)

- [ ] Criar regra válida completa
- [ ] Validar mensagens de erro
- [ ] Editar regra existente
- [ ] Deletar regra com confirmação
- [ ] Toggle enable/disable
- [ ] Verificar persistência na listagem

---

## 5. COMANDOS DE EXECUÇÃO

```bash
# Testes unitários
cd ~/repos/RULEX/client && pnpm test --run

# Testes unitários com watch
cd ~/repos/RULEX/client && pnpm test

# Testes E2E
cd ~/repos/RULEX && pnpm exec playwright test

# Testes E2E com UI
cd ~/repos/RULEX && pnpm exec playwright test --ui

# Build
cd ~/repos/RULEX/client && pnpm build

# Type check
cd ~/repos/RULEX && pnpm check
```

---

## 6. CRITÉRIOS DE ACEITAÇÃO

### 6.1 Unitários
- [x] 100% dos testes passando
- [x] Cobertura de casos adversariais
- [x] Validação de todos os operadores

### 6.2 Integração
- [x] CRUD completo testado
- [x] Payload correto enviado
- [x] Estados do modal corretos

### 6.3 E2E
- [x] Navegação funcional
- [x] Modal abre/fecha
- [ ] Fluxo completo de criação (parcial)

### 6.4 Build
- [x] Build sem erros
- [x] TypeScript sem erros
- [x] Sem warnings críticos

---

**Última atualização:** 2024-12-31 14:15 UTC
