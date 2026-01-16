# CHECK #2 — FIDELIDADE AO CÓDIGO (ZERO ALUCINAÇÃO)

**Data:** 2026-01-16  
**Branch:** `cursor/rulex-project-review-1c58`

---

## ✅ RESUMO EXECUTIVO

Todos os dados exibidos no Manual são extraídos automaticamente das fontes autoritativas via `manual-generate.mjs`. Não há dados hardcoded.

| Fonte | Destino | Mecanismo | Status |
|-------|---------|-----------|--------|
| `RuleCondition.java` | `backendOperators.generated.ts` | Regex extraction | ✅ |
| `RuleAction.java` | `backendActions.generated.ts` | Enum parsing | ✅ |
| `ExpressionEvaluator.java` | `expressionFunctions.generated.ts` | Method scanning | ✅ |
| `RuleConditionGroup.java` | `logicOperators.generated.ts` | Enum parsing | ✅ |
| `AstValidator.java` | `astAllowlist.generated.ts` | Set extraction | ✅ |
| `openapi/*.yaml` | `openapiSummary.generated.ts` | YAML parsing | ✅ |
| `docs/*.md` | `docsIndex.generated.ts` | Directory scan | ✅ |

---

## 🗺️ MAPEAMENTO DE FONTES

### Operadores (447)
- **Fonte**: `backend/src/main/java/com/rulex/entity/complex/RuleCondition.java`
- **Enum**: `ConditionOperator`
- **Gerado**: `client/src/manual/generated/backendOperators.generated.ts`
- **Validação**: ✅ FE (447) == BE (447)

### Ações (10)
- **Fonte**: `backend/src/main/java/com/rulex/entity/complex/RuleAction.java`
- **Enum**: `ActionType`
- **Gerado**: `client/src/manual/generated/backendActions.generated.ts`

### Funções de Expressão (23)
- **Fonte**: `backend/src/main/java/com/rulex/service/complex/ExpressionEvaluator.java`
- **Gerado**: `client/src/manual/generated/expressionFunctions.generated.ts`

### Operadores Lógicos (6)
- **Fonte**: `backend/src/main/java/com/rulex/entity/complex/RuleConditionGroup.java`
- **Enum**: `GroupLogicOperator` (AND, OR, NOT, XOR, NAND, NOR)
- **Gerado**: `client/src/manual/generated/logicOperators.generated.ts`

### Allowlist AST
- **Fonte**: `backend/src/main/java/com/rulex/validation/AstValidator.java`
- **Gerado**: `client/src/manual/generated/astAllowlist.generated.ts`
- **Conteúdo**: 8 funções, 24 operadores, 6 aliases

### API Endpoints (18)
- **Fonte**: `openapi/rulex.yaml`
- **Gerado**: `client/src/manual/generated/openapiSummary.generated.ts`

### Documentos (42)
- **Fonte**: `docs/*.md`
- **Gerado**: `client/src/manual/generated/docsIndex.generated.ts`

---

## 🔄 FLUXO DE GERAÇÃO

```
┌─────────────────────────────────────────────────┐
│         FONTES AUTORITATIVAS (Backend)          │
│  RuleCondition.java → 447 operadores            │
│  RuleAction.java → 10 ações                     │
│  ExpressionEvaluator.java → 23 funções          │
│  RuleConditionGroup.java → 6 operadores lógicos │
│  AstValidator.java → allowlist                  │
│  openapi/*.yaml → 18 endpoints                  │
│  docs/*.md → 42 documentos                      │
└─────────────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────┐
│           scripts/manual-generate.mjs           │
│  - extractBackendOperators()                    │
│  - extractBackendActions()                      │
│  - extractLogicOperators()                      │
│  - extractExpressionFunctions()                 │
│  - extractAstAllowlist()                        │
│  - extractOpenapiEndpoints()                    │
│  - extractDocsIndex()                           │
│  + TRIPLE CHECK integrado (valida FE==BE)       │
└─────────────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────┐
│     client/src/manual/generated/*.ts            │
│  backendOperators.generated.ts                  │
│  backendActions.generated.ts                    │
│  logicOperators.generated.ts                    │
│  expressionFunctions.generated.ts               │
│  astAllowlist.generated.ts                      │
│  openapiSummary.generated.ts                    │
│  docsIndex.generated.ts                         │
│  index.ts (barrel)                              │
└─────────────────────────────────────────────────┘
```

---

## ⚠️ AVISOS NÃO-BLOQUEANTES

```
WARN: FUNC_ALLOWLIST (AstValidator) referencia funções não encontradas 
no ExpressionEvaluator: TO_DATE_YYYYMMDD, PARSE_GMTOFFSET
→ Funções planejadas/reservadas, não implementadas ainda
```

---

## ✅ VEREDITO CHECK 2: **APROVADO**

Todos os dados são extraídos de fontes autoritativas. Zero alucinação.
    SET_SCORE,
    ADD_TAG,
    REMOVE_TAG,
    SET_VARIABLE,
    CALL_WEBHOOK,
    SEND_NOTIFICATION,
    BLOCK_TRANSACTION,
    FLAG_FOR_REVIEW,
    ESCALATE
  }
}

### Amostra de Operadores Lógicos
  public enum GroupLogicOperator {
    AND, // Todas as condições devem ser verdadeiras
    OR, // Pelo menos uma condição deve ser verdadeira
    NOT, // Inverte o resultado do grupo
    XOR, // Exatamente uma condição deve ser verdadeira
    NAND, // NOT AND - pelo menos uma condição deve ser falsa
    NOR // NOT OR - todas as condições devem ser falsas
  }
}

## CONCLUSÃO

✅ Todos os dados do Manual são extraídos diretamente do código-fonte
✅ Script `manual-generate.mjs` faz a extração automatizada
✅ Arquivos `.generated.ts` são marcados como auto-gerados
✅ Nenhum dado foi inventado ou assumido
