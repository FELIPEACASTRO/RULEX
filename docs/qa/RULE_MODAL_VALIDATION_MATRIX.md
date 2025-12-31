# RULE_MODAL_VALIDATION_MATRIX.md - Matriz de Validação

**Data:** 2024-12-31
**Versão:** 1.0

---

## 1. MATRIZ: TIPO DE CAMPO × OPERADOR × REQUISITOS DE VALUE

### Legenda
- ✅ = Combinação válida
- ❌ = Combinação inválida (deve ser bloqueada)
- 0 = Não requer value (operador unário)
- 1 = Requer 1 valor
- 2 = Requer 2 valores (min, max)
- L = Requer lista (1+ itens)
- R = Requer regex válida

---

### 1.1 Campos Numéricos (number)

| Operador | Permitido | Value | Validação |
|----------|-----------|-------|-----------|
| EQ | ✅ | 1 | Deve ser número |
| NE | ✅ | 1 | Deve ser número |
| GT | ✅ | 1 | Deve ser número |
| LT | ✅ | 1 | Deve ser número |
| GTE | ✅ | 1 | Deve ser número |
| LTE | ✅ | 1 | Deve ser número |
| IN | ✅ | L | Lista de números |
| NOT_IN | ✅ | L | Lista de números |
| BETWEEN | ✅ | 2 | Dois números, min < max |
| NOT_BETWEEN | ✅ | 2 | Dois números, min < max |
| CONTAINS | ❌ | - | Não aplicável |
| NOT_CONTAINS | ❌ | - | Não aplicável |
| STARTS_WITH | ❌ | - | Não aplicável |
| ENDS_WITH | ❌ | - | Não aplicável |
| MATCHES_REGEX | ❌ | - | Não aplicável |
| IS_NULL | ✅ | 0 | - |
| IS_NOT_NULL | ✅ | 0 | - |
| IS_TRUE | ❌ | - | Não aplicável |
| IS_FALSE | ❌ | - | Não aplicável |

---

### 1.2 Campos de Texto (string)

| Operador | Permitido | Value | Validação |
|----------|-----------|-------|-----------|
| EQ | ✅ | 1 | Qualquer string |
| NE | ✅ | 1 | Qualquer string |
| GT | ❌ | - | Não recomendado |
| LT | ❌ | - | Não recomendado |
| GTE | ❌ | - | Não recomendado |
| LTE | ❌ | - | Não recomendado |
| IN | ✅ | L | Lista de strings |
| NOT_IN | ✅ | L | Lista de strings |
| BETWEEN | ❌ | - | Não aplicável |
| NOT_BETWEEN | ❌ | - | Não aplicável |
| CONTAINS | ✅ | 1 | Substring não vazia |
| NOT_CONTAINS | ✅ | 1 | Substring não vazia |
| STARTS_WITH | ✅ | 1 | Prefixo não vazio |
| ENDS_WITH | ✅ | 1 | Sufixo não vazio |
| MATCHES_REGEX | ✅ | R | Regex válida |
| IS_NULL | ✅ | 0 | - |
| IS_NOT_NULL | ✅ | 0 | - |
| IS_TRUE | ❌ | - | Não aplicável |
| IS_FALSE | ❌ | - | Não aplicável |

---

### 1.3 Campos Booleanos (boolean)

| Operador | Permitido | Value | Validação |
|----------|-----------|-------|-----------|
| EQ | ❌ | - | Use IS_TRUE/IS_FALSE |
| NE | ❌ | - | Use IS_TRUE/IS_FALSE |
| GT | ❌ | - | Não aplicável |
| LT | ❌ | - | Não aplicável |
| GTE | ❌ | - | Não aplicável |
| LTE | ❌ | - | Não aplicável |
| IN | ❌ | - | Não aplicável |
| NOT_IN | ❌ | - | Não aplicável |
| BETWEEN | ❌ | - | Não aplicável |
| NOT_BETWEEN | ❌ | - | Não aplicável |
| CONTAINS | ❌ | - | Não aplicável |
| NOT_CONTAINS | ❌ | - | Não aplicável |
| STARTS_WITH | ❌ | - | Não aplicável |
| ENDS_WITH | ❌ | - | Não aplicável |
| MATCHES_REGEX | ❌ | - | Não aplicável |
| IS_NULL | ✅ | 0 | - |
| IS_NOT_NULL | ✅ | 0 | - |
| IS_TRUE | ✅ | 0 | - |
| IS_FALSE | ✅ | 0 | - |

---

### 1.4 Campos de Data (date)

| Operador | Permitido | Value | Validação |
|----------|-----------|-------|-----------|
| EQ | ✅ | 1 | YYYYMMDD ou ISO |
| NE | ✅ | 1 | YYYYMMDD ou ISO |
| GT | ✅ | 1 | YYYYMMDD ou ISO |
| LT | ✅ | 1 | YYYYMMDD ou ISO |
| GTE | ✅ | 1 | YYYYMMDD ou ISO |
| LTE | ✅ | 1 | YYYYMMDD ou ISO |
| IN | ❌ | - | Não recomendado |
| NOT_IN | ❌ | - | Não recomendado |
| BETWEEN | ✅ | 2 | Duas datas, início < fim |
| NOT_BETWEEN | ✅ | 2 | Duas datas, início < fim |
| CONTAINS | ❌ | - | Não aplicável |
| NOT_CONTAINS | ❌ | - | Não aplicável |
| STARTS_WITH | ❌ | - | Não aplicável |
| ENDS_WITH | ❌ | - | Não aplicável |
| MATCHES_REGEX | ❌ | - | Não aplicável |
| IS_NULL | ✅ | 0 | - |
| IS_NOT_NULL | ✅ | 0 | - |
| IS_TRUE | ❌ | - | Não aplicável |
| IS_FALSE | ❌ | - | Não aplicável |

---

## 2. IMPLEMENTAÇÃO NO CÓDIGO

### 2.1 Localização

**Arquivo:** `client/src/components/RuleFormDialog/types.ts`

```typescript
// Mapeamento de tipo de campo para operadores permitidos
export const OPERATORS_BY_TYPE: Record<string, ConditionOperator[]> = {
  number: ['EQ', 'NE', 'GT', 'LT', 'GTE', 'LTE', 'IN', 'NOT_IN', 'BETWEEN', 'NOT_BETWEEN', 'IS_NULL', 'IS_NOT_NULL'],
  string: ['EQ', 'NE', 'IN', 'NOT_IN', 'CONTAINS', 'NOT_CONTAINS', 'STARTS_WITH', 'ENDS_WITH', 'MATCHES_REGEX', 'IS_NULL', 'IS_NOT_NULL'],
  boolean: ['IS_TRUE', 'IS_FALSE', 'IS_NULL', 'IS_NOT_NULL'],
  date: ['EQ', 'NE', 'GT', 'LT', 'GTE', 'LTE', 'BETWEEN', 'NOT_BETWEEN', 'IS_NULL', 'IS_NOT_NULL'],
};
```

### 2.2 Uso no Hook

**Arquivo:** `client/src/components/RuleFormDialog/useRuleForm.ts`

```typescript
const getOperatorsForField = useCallback((fieldName: string) => {
  const field = availableFields.find(f => f.value === fieldName);
  
  // Se o campo tem operadores específicos da API, usar esses
  if (field?.allowedOperators && field.allowedOperators.length > 0) {
    return OPERATORS.filter(op => field.allowedOperators!.includes(op.value));
  }
  
  // Senão, usar operadores baseados no tipo
  const fieldType = field?.type || 'string';
  const allowedOps = OPERATORS_BY_TYPE[fieldType] || OPERATORS_BY_TYPE.string;
  return OPERATORS.filter(op => allowedOps.includes(op.value));
}, [availableFields]);
```

---

## 3. VALIDAÇÃO DE VALUE POR OPERADOR

### 3.1 Requisitos

| Operador | Requisitos de Value |
|----------|---------------------|
| EQ, NE | 1 valor, não vazio |
| GT, LT, GTE, LTE | 1 valor, numérico se campo number |
| IN, NOT_IN | Lista com 1+ itens, sem vazios |
| BETWEEN, NOT_BETWEEN | 2 valores, ambos não vazios, min ≤ max |
| CONTAINS, NOT_CONTAINS | 1 valor, não vazio |
| STARTS_WITH, ENDS_WITH | 1 valor, não vazio |
| MATCHES_REGEX | Regex sintaticamente válida |
| IS_NULL, IS_NOT_NULL | Nenhum (ignorar value) |
| IS_TRUE, IS_FALSE | Nenhum (ignorar value) |

### 3.2 Implementação

**Arquivo:** `client/src/components/RuleFormDialog/schema.ts`

```typescript
export function validateValueByOperator(
  operator: string,
  value: string,
  fieldType?: 'string' | 'number' | 'boolean' | 'date'
): string | null {
  // Operadores unários não precisam de valor
  if (UNARY_OPERATORS.includes(operator as any)) {
    return null;
  }
  // ... validações específicas por operador
}
```

---

## 4. CAMPOS DO FIELD DICTIONARY

### 4.1 Campos Padrão (Fallback)

| Campo | Tipo | Categoria |
|-------|------|-----------|
| customerIdFromHeader | string | Identificação |
| merchantId | string | Identificação |
| pan | string | Identificação |
| externalTransactionId | string | Identificação |
| transactionAmount | number | Valores |
| transactionDate | number | Valores |
| transactionTime | number | Valores |
| transactionCurrencyCode | number | Valores |
| merchantCountryCode | string | Localização |
| merchantCity | string | Localização |
| merchantState | string | Localização |
| merchantPostalCode | string | Localização |
| consumerAuthenticationScore | number | Segurança |
| externalScore3 | number | Segurança |
| cavvResult | number | Segurança |
| cryptogramValid | string | Segurança |
| cvv2Response | string | Segurança |
| eciIndicator | number | Segurança |
| mcc | number | Categoria |
| posEntryMode | string | Categoria |
| customerPresent | string | Categoria |

---

## 5. LIMITES E CONSTRAINTS

| Constraint | Valor | Localização |
|------------|-------|-------------|
| MAX_CONDITIONS | 20 | schema.ts |
| ruleName.minLength | 3 | schema.ts |
| ruleName.maxLength | 100 | schema.ts |
| description.maxLength | 500 | schema.ts |
| threshold.min | 0 | schema.ts |
| threshold.max | 1000 | schema.ts |
| weight.min | 0 | schema.ts |
| weight.max | 100 | schema.ts |
| field.maxLength | 100 | schema.ts |

---

**Última atualização:** 2024-12-31 14:10 UTC
