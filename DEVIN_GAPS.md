# DEVIN GAPS - RULEX

## Última Atualização: 2024-12-31 18:37 UTC

---

## RESUMO

| Severidade | Total | Fechados |
|------------|-------|----------|
| P0 (Crítico) | 3 | 3 ✅ |
| P1 (Alto) | 4 | 1 |
| P2 (Médio) | 3 | 0 |

---

## P0 - CRÍTICOS (Bloqueiam 10/10)

### GAP-001: Frontend ComplexRuleBuilder falta operadores GEO ✅ FECHADO
**Severidade:** P0
**Componente:** Frontend - ComplexRuleBuilder
**Arquivo:** `client/src/components/ComplexRuleBuilder/types.ts`

**Repro:**
```typescript
// types.ts linha ~25
export type ComparisonOperator =
  // ... não inclui GEO_DISTANCE_LT, GEO_DISTANCE_GT, GEO_IN_POLYGON
```

**Impacto:** Usuário não consegue criar regras GEO pelo frontend, mesmo que backend suporte.

**Fix:** Adicionar operadores GEO ao tipo ComparisonOperator e COMPARISON_OPERATORS array.

**Evidência de Fechamento:**
```bash
# Commit: 4ebe969
# Arquivo: client/src/components/ComplexRuleBuilder/types.ts
# Operadores adicionados: GEO_DISTANCE_LT, GEO_DISTANCE_GT, GEO_IN_POLYGON
# Testes: 165 PASS
```

---

### GAP-002: Inconsistência de nomenclatura REGEX ✅ FECHADO
**Severidade:** P0
**Componente:** Frontend vs Backend

**Repro:**
- Frontend usa: `MATCHES_REGEX`
- Backend usa: `REGEX`

**Arquivos:**
- Frontend: `client/src/components/ComplexRuleBuilder/types.ts`
- Frontend: `client/src/components/RuleFormDialog/schema.ts`
- Backend: `backend/src/main/java/com/rulex/dto/complex/ConditionDTO.java`

**Impacto:** Regras criadas no frontend com MATCHES_REGEX falham no backend.

**Fix:** Alinhar nomenclatura (preferir REGEX do backend).

**Evidência de Fechamento:**
```bash
# Commit: 4ebe969
# Adicionado REGEX e NOT_REGEX ao frontend
# MATCHES_REGEX mantido como legacy para compatibilidade
# Testes: 165 PASS
```

---

### GAP-003: Inconsistência de nomenclatura NULL operators ✅ FECHADO
**Severidade:** P0
**Componente:** Frontend vs Backend

**Repro:**
- Frontend usa: `IS_NOT_NULL`
- Backend usa: `NOT_NULL`

**Impacto:** Regras com IS_NOT_NULL falham no backend.

**Fix:** Alinhar nomenclatura.

**Evidência de Fechamento:**
```bash
# Commit: 4ebe969
# Adicionado NOT_NULL ao frontend
# IS_NOT_NULL mantido como legacy para compatibilidade
# Testes: 165 PASS
```

---

## P1 - ALTOS (Afetam funcionalidade significativa)

### GAP-004: RuleFormDialog (simples) falta operadores avançados
**Severidade:** P1
**Componente:** Frontend - RuleFormDialog

**Repro:**
```typescript
// schema.ts
const conditionOperators = [
  'EQ', 'NE', 'GT', 'LT', 'GTE', 'LTE',
  'IN', 'NOT_IN', 'BETWEEN', 'NOT_BETWEEN',
  'CONTAINS', 'NOT_CONTAINS', 'STARTS_WITH', 'ENDS_WITH', 'MATCHES_REGEX',
  'IS_NULL', 'IS_NOT_NULL', 'IS_TRUE', 'IS_FALSE',
  // Faltam: FIELD_*, DATE_*, TIME_*, ARRAY_*, MOD_*, GEO_*, NOT_REGEX
]
```

**Impacto:** Regras simples não podem usar operadores avançados.

**Fix:** Adicionar operadores faltantes ou documentar que regras simples são limitadas.

**Evidência de Fechamento:** Pendente

---

### GAP-005: Sem validação de ReDoS no REGEX
**Severidade:** P1
**Componente:** Backend + Frontend

**Repro:** Usuário pode inserir regex maliciosa como `(a+)+$` que causa catastrophic backtracking.

**Impacto:** DoS potencial no servidor.

**Fix:**
1. Limitar tamanho do pattern
2. Timeout na execução
3. Denylist de construções perigosas
4. Considerar RE2/J

**Evidência de Fechamento:** Pendente

---

### GAP-006: Frontend usa NE vs backend NEQ
**Severidade:** P1
**Componente:** Frontend - RuleFormDialog

**Repro:**
- Frontend schema.ts usa: `NE`
- Backend usa: `NEQ`

**Impacto:** Regras com NE podem falhar no backend.

**Fix:** Alinhar nomenclatura.

**Evidência de Fechamento:** Pendente

---

### GAP-007: VelocityService não integrado ao ComplexRuleEvaluator
**Severidade:** P1
**Componente:** Backend

**Repro:** VelocityService existe mas não é usado pelo engine de regras.

**Impacto:** Regras de velocity não funcionam.

**Fix:** Integrar VelocityService ao ComplexRuleEvaluator via context variables.

**Evidência de Fechamento:** Pendente

---

## P2 - MÉDIOS (Melhorias importantes)

### GAP-008: Sem testes E2E Playwright funcionais
**Severidade:** P2
**Componente:** Testes

**Repro:** E2E requer Docker que não está disponível no ambiente.

**Impacto:** Fluxos completos não são testados automaticamente.

**Fix:** Criar testes E2E e documentar como rodar com Docker.

**Evidência de Fechamento:** Pendente

---

### GAP-009: Sem preview JSON no modal de criação
**Severidade:** P2
**Componente:** Frontend

**Repro:** Usuário não vê o JSON final antes de salvar.

**Impacto:** UX ruim, erros só descobertos após salvar.

**Fix:** Adicionar tab/seção de preview JSON.

**Evidência de Fechamento:** Pendente

---

### GAP-010: Acessibilidade WCAG/ARIA incompleta nos modais
**Severidade:** P2
**Componente:** Frontend

**Repro:** Modais podem não ter focus trap, aria-labels corretos.

**Impacto:** Usuários com deficiência visual têm dificuldade.

**Fix:** Auditar e corrigir acessibilidade.

**Evidência de Fechamento:** Pendente

---

## TEMPLATE PARA NOVOS GAPS

```markdown
### GAP-XXX: [Título]
**Severidade:** P0/P1/P2
**Componente:** [Componente afetado]

**Repro:**
[Como reproduzir]

**Impacto:**
[Qual o impacto]

**Fix:**
[Como corrigir]

**Evidência de Fechamento:**
[Comando/teste que prova que foi corrigido]
```
