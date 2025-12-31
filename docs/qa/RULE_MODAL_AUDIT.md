# RULE_MODAL_AUDIT.md - Auditoria Completa do Modal de Regras

**Data:** 2024-12-31
**Versão:** 1.0
**Status:** EM ANDAMENTO

---

## 1. MAPA DE ARQUIVOS

### 1.1 Frontend - Componentes do Modal

| Arquivo | Função | Linhas |
|---------|--------|--------|
| `client/src/pages/Rules.tsx` | Página principal com modal inline (Dialog) | ~700 |
| `client/src/components/RuleFormDialog/index.tsx` | Exports (componente principal TODO) | ~20 |
| `client/src/components/RuleFormDialog/schema.ts` | Schema Zod + validações | ~280 |
| `client/src/components/RuleFormDialog/types.ts` | Tipos, constantes, operadores | ~130 |
| `client/src/components/RuleFormDialog/useRuleForm.ts` | Hook de gerenciamento do form | ~200 |
| `client/src/components/DeleteRuleDialog.tsx` | Dialog de confirmação de delete | ~50 |

### 1.2 Frontend - API e Tipos

| Arquivo | Função |
|---------|--------|
| `client/src/lib/javaApi.ts` | Cliente API (fetch wrapper) |
| `client/src/lib/api.generated.ts` | Tipos gerados (se existir) |

### 1.3 Backend - Entidades e DTOs

| Arquivo | Função |
|---------|--------|
| `backend/src/main/java/com/rulex/entity/RuleConfiguration.java` | Entidade JPA |
| `backend/src/main/java/com/rulex/dto/RuleConfigurationDTO.java` | DTO de request/response |
| `backend/src/main/java/com/rulex/dto/RuleConditionDTO.java` | DTO de condição |

### 1.4 Backend - Controllers e Services

| Arquivo | Função |
|---------|--------|
| `backend/src/main/java/com/rulex/controller/RuleController.java` | REST Controller |
| `backend/src/main/java/com/rulex/service/RuleConfigurationService.java` | Service de CRUD |
| `backend/src/main/java/com/rulex/service/complex/RuleValidationService.java` | Validação de regras |

### 1.5 Testes

| Arquivo | Tipo |
|---------|------|
| `client/src/pages/Rules.test.tsx` | Unit/Integration (Vitest) |
| `e2e/rules.spec.ts` | E2E (Playwright) |
| `backend/src/test/java/com/rulex/service/RuleEngineServiceTest.java` | Backend Unit |
| `backend/src/test/java/com/rulex/service/complex/RuleValidationServiceTest.java` | Backend Unit |

---

## 2. DIAGRAMA DE FLUXO DO MODAL

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           FLUXO: CRIAR REGRA                                │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  [Botão "Nova Regra"]                                                       │
│         │                                                                   │
│         ▼                                                                   │
│  ┌─────────────────┐                                                        │
│  │ setShowDialog   │──► Dialog abre                                         │
│  │ setEditingRule  │    (mode: create)                                      │
│  │ (null)          │                                                        │
│  └─────────────────┘                                                        │
│         │                                                                   │
│         ▼                                                                   │
│  ┌─────────────────────────────────────────────────────────────────┐       │
│  │                    DIALOG CONTENT                                │       │
│  │  ┌─────────────────────────────────────────────────────────┐    │       │
│  │  │ ruleName (required, UPPER_SNAKE_CASE)                   │    │       │
│  │  │ description (optional)                                   │    │       │
│  │  │ ruleType (SECURITY|CONTEXT|VELOCITY|ANOMALY)            │    │       │
│  │  │ classification (APPROVED|SUSPICIOUS|FRAUD)               │    │       │
│  │  │ threshold (0-1000)                                       │    │       │
│  │  │ weight (0-100)                                           │    │       │
│  │  │ parameters (JSON opcional)                               │    │       │
│  │  │ logicOperator (AND|OR)                                   │    │       │
│  │  │ conditions[] (field, operator, value)                    │    │       │
│  │  └─────────────────────────────────────────────────────────┘    │       │
│  └─────────────────────────────────────────────────────────────────┘       │
│         │                                                                   │
│         ▼                                                                   │
│  [Botão "Criar"]                                                            │
│         │                                                                   │
│         ▼                                                                   │
│  ┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐       │
│  │ validateForm()  │────►│ POST /api/rules │────►│ Backend valida  │       │
│  │ (client-side)   │     │                 │     │ + persiste      │       │
│  └─────────────────┘     └─────────────────┘     └─────────────────┘       │
│         │                        │                       │                  │
│         ▼                        ▼                       ▼                  │
│  ┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐       │
│  │ Erro validação  │     │ HTTP 201        │     │ HTTP 4xx/5xx    │       │
│  │ toast.error()   │     │ toast.success() │     │ toast.error()   │       │
│  │ NÃO fecha modal │     │ Fecha modal     │     │ NÃO fecha modal │       │
│  └─────────────────┘     │ Invalida cache  │     └─────────────────┘       │
│                          └─────────────────┘                                │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────────────┐
│                           FLUXO: EDITAR REGRA                               │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  [Botão "Editar" na linha]                                                  │
│         │                                                                   │
│         ▼                                                                   │
│  ┌─────────────────┐                                                        │
│  │ handleEdit(rule)│──► Dialog abre                                         │
│  │ setEditingRule  │    (mode: edit)                                        │
│  │ setFormData     │    ruleName DISABLED                                   │
│  └─────────────────┘                                                        │
│         │                                                                   │
│         ▼                                                                   │
│  [Botão "Atualizar"]                                                        │
│         │                                                                   │
│         ▼                                                                   │
│  ┌─────────────────┐     ┌─────────────────────┐     ┌─────────────────┐   │
│  │ validateForm()  │────►│ PUT /api/rules/{id} │────►│ Backend valida  │   │
│  │ (client-side)   │     │ + version (OL)      │     │ + persiste      │   │
│  └─────────────────┘     └─────────────────────┘     └─────────────────┘   │
│                                  │                           │              │
│                                  ▼                           ▼              │
│                          ┌─────────────────┐         ┌─────────────────┐   │
│                          │ HTTP 200        │         │ HTTP 409        │   │
│                          │ toast.success() │         │ Conflito versão │   │
│                          │ Fecha modal     │         │ toast.error()   │   │
│                          └─────────────────┘         └─────────────────┘   │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## 3. ESTADOS DO MODAL

| Estado | Descrição | Transições |
|--------|-----------|------------|
| `closed` | Modal fechado | → `creating`, `editing` |
| `creating` | Criando nova regra | → `saving`, `closed` |
| `editing` | Editando regra existente | → `saving`, `closed` |
| `saving` | Requisição em andamento | → `closed` (sucesso), `creating`/`editing` (erro) |
| `dirty` | Formulário com alterações não salvas | → `saving`, `confirmDiscard` |
| `confirmDiscard` | AlertDialog de descarte | → `closed`, `creating`/`editing` |

---

## 4. PAYLOAD DE ENTRADA (CONTRATO IMUTÁVEL)

### 4.1 POST /api/rules (Criar)

```typescript
{
  ruleName: string;           // REQUIRED, UPPER_SNAKE_CASE, unique
  description?: string | null;
  ruleType: "SECURITY" | "CONTEXT" | "VELOCITY" | "ANOMALY";
  classification: "APPROVED" | "SUSPICIOUS" | "FRAUD";
  threshold: number;          // 0-1000
  weight: number;             // 0-100
  enabled: boolean;
  parameters?: string | null; // JSON string
  conditions: Array<{
    field: string;            // REQUIRED
    operator: string;         // REQUIRED
    value: string;            // REQUIRED (pode ser "" para unários)
  }>;
  logicOperator: "AND" | "OR";
}
```

### 4.2 PUT /api/rules/{id} (Atualizar)

```typescript
// Mesmo payload + version para optimistic locking
{
  ...createPayload,
  version?: number;
}
```

### 4.3 PATCH /api/rules/{id}/toggle

```typescript
{
  enabled: boolean;
}
```

---

## 5. OPERADORES SUPORTADOS

### 5.1 Operadores por Categoria

| Categoria | Operadores | Requer Value |
|-----------|------------|--------------|
| Comparação | EQ, NE, GT, LT, GTE, LTE | Sim (1 valor) |
| Lista | IN, NOT_IN | Sim (lista) |
| Range | BETWEEN, NOT_BETWEEN | Sim (2 valores) |
| String | CONTAINS, NOT_CONTAINS, STARTS_WITH, ENDS_WITH | Sim (1 valor) |
| Regex | MATCHES_REGEX | Sim (regex válida) |
| Unários | IS_NULL, IS_NOT_NULL, IS_TRUE, IS_FALSE | Não |
| Legado | ==, !=, >, <, >=, <= | Sim (1 valor) |

### 5.2 Operadores por Tipo de Campo

| Tipo | Operadores Permitidos |
|------|----------------------|
| number | EQ, NE, GT, LT, GTE, LTE, IN, NOT_IN, BETWEEN, NOT_BETWEEN, IS_NULL, IS_NOT_NULL |
| string | EQ, NE, IN, NOT_IN, CONTAINS, NOT_CONTAINS, STARTS_WITH, ENDS_WITH, MATCHES_REGEX, IS_NULL, IS_NOT_NULL |
| boolean | IS_TRUE, IS_FALSE, IS_NULL, IS_NOT_NULL |
| date | EQ, NE, GT, LT, GTE, LTE, BETWEEN, NOT_BETWEEN, IS_NULL, IS_NOT_NULL |

---

## 6. VALIDAÇÕES EXISTENTES (FRONTEND)

### 6.1 Schema Zod (`schema.ts`)

| Campo | Validação |
|-------|-----------|
| ruleName | min(3), max(100), regex `/^[A-Z][A-Z0-9_]*$/` |
| description | max(500), optional |
| ruleType | enum |
| classification | enum |
| threshold | int, 0-1000 |
| weight | int, 0-100 |
| enabled | boolean |
| parameters | JSON válido se preenchido |
| conditions | array de conditionSchema |
| logicOperator | enum AND/OR |

### 6.2 conditionSchema

| Campo | Validação |
|-------|-----------|
| field | min(1), max(100) |
| operator | enum |
| value | string (refine para unários, REGEX, BETWEEN, IN) |

### 6.3 Validações Inline (`Rules.tsx`)

- `validateForm()`: Valida antes de submit
- `validateValueByOperator()`: Valida valor por operador
- Limite de condições: MAX_CONDITIONS = 20

---

## 7. FEATURES DE UX EXISTENTES

| Feature | Status | Localização |
|---------|--------|-------------|
| Dirty state tracking | ✅ Implementado | `isDirty`, `updateFormData()` |
| Unsaved changes warning | ✅ Implementado | `showUnsavedWarning`, AlertDialog |
| Delete confirmation | ✅ Implementado | `deleteConfirmId`, AlertDialog |
| Loading state (save) | ✅ Implementado | `saveRule.isPending` |
| Error messages inline | ✅ Implementado | `validationErrors` |
| aria-invalid | ✅ Implementado | Inputs com erro |
| aria-describedby | ✅ Implementado | Mensagens de erro |
| Disabled ruleName on edit | ✅ Implementado | `disabled={!!editingRule}` |
| Version conflict handling | ✅ Implementado | HTTP 409 detection |
| Field dictionary (API) | ✅ Implementado | `fieldDictionaryQuery` |
| Fallback fields | ✅ Implementado | `FALLBACK_FIELDS` |
| Operator filtering by type | ✅ Implementado | `OPERATORS_BY_TYPE` |

---

## 8. GAPS IDENTIFICADOS (PRELIMINAR)

### 8.1 Críticos (P0)

| ID | Descrição | Evidência |
|----|-----------|-----------|
| P0-01 | Regex ReDoS não validado | `schema.ts` apenas compila, não verifica complexidade |
| P0-02 | BETWEEN não valida ordem (min < max) | Validação parcial em `validateValueByOperator` |
| P0-03 | IN/NOT_IN não valida duplicatas | Não há verificação |
| P0-04 | XSS em exibição de regex/motivo | Não há sanitização explícita |
| P0-05 | Double-click pode duplicar request | Não há debounce/disable durante save |

### 8.2 Altos (P1)

| ID | Descrição | Evidência |
|----|-----------|-----------|
| P1-01 | Whitespace/unicode invisível não tratado | `trim()` básico apenas |
| P1-02 | Números extremos não validados | NaN, Infinity, overflow |
| P1-03 | Limite de tamanho de lista IN não definido | Pode travar UI |
| P1-04 | Focus trap não implementado | Dialog padrão Radix |
| P1-05 | ESC não fecha com confirmação dirty | Comportamento inconsistente |
| P1-06 | Preview da regra final ausente | Não existe |
| P1-07 | Simulação de regra ausente no modal | Existe em página separada |

### 8.3 Médios (P2)

| ID | Descrição | Evidência |
|----|-----------|-----------|
| P2-01 | Microcopy pode melhorar | Placeholders genéricos |
| P2-02 | Agrupamento visual de campos | Layout flat |
| P2-03 | Tooltips de ajuda ausentes | Sem explicações inline |
| P2-04 | Auto-save não implementado | Pode perder dados |
| P2-05 | Histórico de versões no modal | Só via API separada |

---

## 9. PRÓXIMOS PASSOS

1. **Passada 2 - Red Team**: Criar bateria de testes adversariais
2. **Passada 3 - Implementar correções**: Refatorar validações e UX
3. **Passada 4 - Provar**: Criar/atualizar testes e documentar evidências

---

**Última atualização:** 2024-12-31 13:40 UTC
