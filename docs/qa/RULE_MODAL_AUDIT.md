# RULE_MODAL_AUDIT.md - Auditoria do Modal de Regras RULEX

**Data:** 2024-12-31  
**Versão:** 1.0.0  
**Autor:** Comitê Multidisciplinar (Staff FE, UX/UI, A11y, QA E2E, BE/API, Regras Duras, SRE)

---

## 1. ARQUIVOS ENVOLVIDOS

### Frontend - Componentes do Modal

| Arquivo | Descrição | LOC |
|---------|-----------|-----|
| `client/src/pages/Rules.tsx` | Página principal com modal inline (Dialog) | ~450 |
| `client/src/pages/RulesDidactic.tsx` | Versão didática com modal avançado | ~1800 |
| `client/src/components/RuleFormDialog/index.tsx` | Export do componente (não implementado) | ~20 |
| `client/src/components/RuleFormDialog/schema.ts` | Schema Zod de validação | ~180 |
| `client/src/components/RuleFormDialog/types.ts` | Tipos e constantes | ~150 |
| `client/src/components/RuleFormDialog/useRuleForm.ts` | Hook de gerenciamento do form | ~220 |

### Backend - Contratos e Endpoints

| Arquivo | Descrição |
|---------|-----------|
| `backend/.../controller/RuleController.java` | REST Controller CRUD |
| `backend/.../dto/RuleConfigurationDTO.java` | DTO com validações Jakarta |
| `backend/.../dto/RuleConditionDTO.java` | DTO de condição |

### Testes Existentes

| Arquivo | Cobertura |
|---------|-----------|
| `client/src/pages/Rules.test.tsx` | Unit tests do modal (create, edit, toggle) |
| `e2e/rules.spec.ts` | E2E básico (navegação, abertura do modal) |

---

## 2. FLUXOS MAPEADOS

### 2.1 Fluxo de Criação de Regra

```
[Usuário] → Clica "Nova Regra"
    ↓
[Dialog] → Abre com formData vazio (defaultValues)
    ↓
[Usuário] → Preenche campos:
    - ruleName (obrigatório, padrão: UPPER_SNAKE_CASE)
    - description (opcional)
    - ruleType (SECURITY|CONTEXT|VELOCITY|ANOMALY)
    - classification (APPROVED|SUSPICIOUS|FRAUD)
    - threshold (0-1000)
    - weight (0-100)
    - enabled (checkbox)
    - parameters (JSON opcional, usado em VELOCITY)
    - conditions[] (campo, operador, valor)
    - logicOperator (AND|OR)
    ↓
[Usuário] → Clica "Criar"
    ↓
[Frontend] → POST /api/rules com payload
    ↓
[Backend] → Valida e persiste
    ↓
[Frontend] → Toast sucesso + fecha dialog + invalida query
```

### 2.2 Fluxo de Edição de Regra

```
[Usuário] → Clica ícone "Editar" na linha da regra
    ↓
[Dialog] → Abre com formData preenchido da regra existente
    ↓
[Campo ruleName] → DESABILITADO (não pode alterar nome)
    ↓
[Usuário] → Altera campos permitidos
    ↓
[Usuário] → Clica "Atualizar"
    ↓
[Frontend] → PUT /api/rules/{id} com payload
    ↓
[Backend] → Valida e atualiza (incrementa version)
    ↓
[Frontend] → Toast sucesso + fecha dialog + invalida query
```

### 2.3 Fluxo de Toggle (Ativar/Desativar)

```
[Usuário] → Clica ícone "Toggle" na linha
    ↓
[Frontend] → PATCH /api/rules/{id}/toggle
    ↓
[Backend] → Inverte enabled, incrementa version
    ↓
[Frontend] → Invalida query (lista atualiza)
```

---

## 3. ESTADOS DO MODAL

| Estado | Descrição | Implementado? |
|--------|-----------|---------------|
| `idle` | Modal fechado | ✅ |
| `creating` | Modal aberto para nova regra | ✅ |
| `editing` | Modal aberto com regra existente | ✅ |
| `loading` | Salvando/carregando | ⚠️ Parcial |
| `error` | Erro de validação ou API | ⚠️ Parcial |
| `success` | Operação concluída | ✅ (toast) |
| `dirty` | Alterações não salvas | ❌ Não implementado |
| `conflict` | Versão desatualizada | ❌ Não implementado |

---

## 4. PAYLOAD DO BACKEND (Contrato)

### RuleConfigurationDTO (POST/PUT)

```typescript
{
  ruleName: string;        // @NotBlank, padrão UPPER_SNAKE_CASE
  description?: string;    // Opcional
  ruleType: string;        // @NotNull: SECURITY|CONTEXT|VELOCITY|ANOMALY
  threshold: number;       // @NotNull, @Min(0)
  weight: number;          // @NotNull, @Min(0), @Max(100)
  enabled: boolean;        // @NotNull
  classification: string;  // @NotNull: APPROVED|SUSPICIOUS|FRAUD
  parameters?: string;     // Opcional, JSON string
  conditions: RuleConditionDTO[]; // @NotNull (pode ser vazio)
  logicOperator: string;   // @NotBlank: AND|OR
}
```

### RuleConditionDTO

```typescript
{
  field: string;    // @NotBlank
  operator: string; // @NotBlank
  value: string;    // @NotNull (pode ser "" para operadores unários)
}
```

---

## 5. OPERADORES SUPORTADOS

### Operadores Canônicos (Backend)

| Operador | Descrição | Requer Valor? | Tipos Válidos |
|----------|-----------|---------------|---------------|
| `EQ` | Igual | ✅ | string, number, date |
| `NE` | Diferente | ✅ | string, number, date |
| `GT` | Maior que | ✅ | number, date |
| `LT` | Menor que | ✅ | number, date |
| `GTE` | Maior ou igual | ✅ | number, date |
| `LTE` | Menor ou igual | ✅ | number, date |
| `IN` | Na lista | ✅ | string, number |
| `NOT_IN` | Fora da lista | ✅ | string, number |
| `BETWEEN` | Entre valores | ✅ | number, date |
| `NOT_BETWEEN` | Fora do intervalo | ✅ | number, date |
| `CONTAINS` | Contém substring | ✅ | string |
| `NOT_CONTAINS` | Não contém | ✅ | string |
| `STARTS_WITH` | Começa com | ✅ | string |
| `ENDS_WITH` | Termina com | ✅ | string |
| `MATCHES_REGEX` | Regex match | ✅ | string |
| `IS_NULL` | É nulo | ❌ | todos |
| `IS_NOT_NULL` | Não é nulo | ❌ | todos |
| `IS_TRUE` | É verdadeiro | ❌ | boolean |
| `IS_FALSE` | É falso | ❌ | boolean |

### Operadores Legados (Compatibilidade)

| Legado | Canônico |
|--------|----------|
| `==` | `EQ` |
| `!=` | `NE` |
| `>` | `GT` |
| `<` | `LT` |
| `>=` | `GTE` |
| `<=` | `LTE` |

---

## 6. CAMPOS DO FIELD DICTIONARY

O frontend busca campos disponíveis via `/api/field-dictionary`. Campos fallback estão definidos em `types.ts`:

### Categorias de Campos

- **Identificação:** customerIdFromHeader, merchantId, pan, externalTransactionId
- **Valores:** transactionAmount, transactionDate, transactionTime, transactionCurrencyCode
- **Localização:** merchantCountryCode, merchantCity, merchantState, merchantPostalCode
- **Segurança:** consumerAuthenticationScore, externalScore3, cavvResult, cryptogramValid, cvv2Response, eciIndicator
- **Categoria:** mcc, posEntryMode, customerPresent

---

## 7. LISTA INICIAL DE PROBLEMAS IDENTIFICADOS

### P0 - Críticos (Quebra funcional / Regra inválida / Perda de dados)

| ID | Problema | Arquivo | Impacto |
|----|----------|---------|---------|
| P0-01 | Sem validação de REGEX válida no operador MATCHES_REGEX | Rules.tsx | Regra inválida pode ser salva |
| P0-02 | Sem validação de formato BETWEEN (min,max ou min..max) | Rules.tsx | Erro silencioso no backend |
| P0-03 | Sem validação de lista IN/NOT_IN (formato JSON array) | Rules.tsx | Erro de parsing no backend |
| P0-04 | Sem warning de unsaved changes ao fechar modal | Rules.tsx | Perda de dados |
| P0-05 | Sem tratamento de conflito de versão (optimistic locking) | Rules.tsx | Sobrescrita de alterações |

### P1 - Importantes (UX ruim / Validação incompleta / Acessibilidade)

| ID | Problema | Arquivo | Impacto |
|----|----------|---------|---------|
| P1-01 | Validação de ruleName não aplicada em tempo real | Rules.tsx | UX confusa |
| P1-02 | Sem feedback visual de campo inválido | Rules.tsx | Usuário não sabe o que corrigir |
| P1-03 | Sem limite de condições (pode criar centenas) | Rules.tsx | Performance/UX |
| P1-04 | Operadores não filtrados por tipo de campo | Rules.tsx | Permite combinações inválidas |
| P1-05 | Sem focus trap no modal | Rules.tsx | Acessibilidade |
| P1-06 | Sem aria-labels adequados | Rules.tsx | Screen readers |
| P1-07 | Mensagens de erro técnicas (não amigáveis) | Rules.tsx | UX |
| P1-08 | Sem loading state durante save | Rules.tsx | UX |
| P1-09 | Sem confirmação ao deletar (usa confirm() nativo) | Rules.tsx | UX inconsistente |
| P1-10 | Select nativo sem estilização consistente | Rules.tsx | UI |

### P2 - Polimento (Consistência / Performance)

| ID | Problema | Arquivo | Impacto |
|----|----------|---------|---------|
| P2-01 | Componente RuleFormDialog não utilizado | RuleFormDialog/* | Código morto |
| P2-02 | Duplicação de lógica entre Rules.tsx e RulesDidactic.tsx | Ambos | Manutenibilidade |
| P2-03 | Sem debounce na validação de campos | Rules.tsx | Performance |
| P2-04 | Re-renders desnecessários ao digitar | Rules.tsx | Performance |
| P2-05 | Sem skeleton loading na lista de regras | Rules.tsx | UX |
| P2-06 | Sem paginação na lista de regras | Rules.tsx | Performance com muitas regras |
| P2-07 | Sem ordenação/filtro na lista | Rules.tsx | UX |
| P2-08 | Sem preview da regra antes de salvar | Rules.tsx | UX |

---

## 8. PRÓXIMOS PASSOS

1. **Criar RULE_MODAL_GAPS.md** com priorização detalhada
2. **Implementar correções P0** (críticas)
3. **Implementar correções P1** (importantes)
4. **Refatorar para usar RuleFormDialog** componentizado
5. **Adicionar testes unitários** para validações
6. **Adicionar testes E2E** para fluxos completos
7. **Documentar em RULE_MODAL_TESTS.md**
8. **Rodar suíte completa e documentar em RULE_MODAL_RUN_LOGS.md**

---

## 9. EVIDÊNCIAS DE CÓDIGO

### Validação Atual (schema.ts)

```typescript
// Validação de ruleName - OK mas não aplicada em tempo real
ruleName: z
  .string()
  .min(3, 'Nome deve ter pelo menos 3 caracteres')
  .max(100, 'Nome deve ter no máximo 100 caracteres')
  .regex(
    /^[A-Z][A-Z0-9_]*$/,
    'Nome deve começar com letra maiúscula e conter apenas letras maiúsculas, números e underscores'
  ),
```

### Condição sem validação de valor por operador (Rules.tsx)

```typescript
// Linha ~280 - Apenas verifica se é unário, não valida formato do valor
const isUnary = ['IS_NULL', 'IS_NOT_NULL', 'IS_TRUE', 'IS_FALSE'].includes(c.operator);
// FALTA: validação de BETWEEN (2 valores), IN (array), REGEX (regex válida)
```

### Sem unsaved changes warning

```typescript
// Não existe implementação de:
// - beforeunload event
// - onOpenChange com verificação de dirty state
// - Confirmação ao fechar com alterações
```

---

**Status:** Auditoria inicial completa. Próximo: RULE_MODAL_GAPS.md
