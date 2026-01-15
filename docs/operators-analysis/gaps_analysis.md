# 🔍 Análise de Gaps - Operadores RULEX

> **Data:** 2026-01-15
> **Versão:** 1.0
> **Analista:** Devin AI

---

## 📊 Resumo de Gaps Identificados

| Tipo de Gap | Quantidade | Severidade |
|-------------|------------|------------|
| Operadores faltando no FrontEnd | 5 | 🟠 ALTA |
| Operadores faltando no BackEnd | 1 | 🔴 CRÍTICA |
| Operadores faltando no PostgreSQL | 1 | 🟡 MÉDIA |
| ValueTypes (não são operadores) | 11 | 🟢 INFO |
| Nomenclatura inconsistente | 7 | 🟡 MÉDIA |

---

## 🔴 GAPS CRÍTICOS

### GAP-001: PIG_BUTCHERING_INDICATOR falta no BackEnd Entity

**Severidade:** 🔴 CRÍTICA

**Descrição:**
O operador `PIG_BUTCHERING_INDICATOR` existe no FrontEnd e PostgreSQL, mas **NÃO** está definido no enum `ConditionOperator` do BackEnd.

**Localização:**
- ✅ FrontEnd: `client/src/lib/operators.ts:linha ~450`
- ❌ BackEnd: `backend/src/main/java/com/rulex/entity/complex/RuleCondition.java` - FALTA
- ✅ PostgreSQL: `V34__add_v31_plus_operators.sql`

**Impacto:**
- Usuário pode criar regra no FrontEnd
- Ao salvar, BackEnd retorna erro de validação
- Regra não é persistida

**Recomendação:**
```java
// Adicionar em RuleCondition.java no enum ConditionOperator
// Seção: Emerging Fraud Types
PIG_BUTCHERING_INDICATOR, // Indicador de pig butchering scam
```

**Tempo Estimado:** 15 minutos

---

## 🟠 GAPS ALTOS

### GAP-002: HAS_FAILED_3DS_LAST_N_MINUTES falta no FrontEnd

**Severidade:** 🟠 ALTA

**Descrição:**
Operador existe no BackEnd mas não está disponível na UI.

**Localização:**
- ❌ FrontEnd: FALTA em `operators.ts`
- ✅ BackEnd: `RuleCondition.java`
- ✅ PostgreSQL: `V34__add_v31_plus_operators.sql`

**Impacto:**
- Operador funcional no BackEnd
- Usuário não consegue usar via UI
- Pode usar via API direta

**Recomendação:**
```typescript
// Adicionar em operators.ts
{ value: 'HAS_FAILED_3DS_LAST_N_MINUTES', label: 'Has Failed 3DS Last N Minutes', 
  description: 'Verifica se houve falha 3DS nos últimos N minutos', 
  requiresValue: true, category: 'Fraude Avançada' },
```

---

### GAP-003: PACS008_FIELD_VALIDATION falta no FrontEnd

**Severidade:** 🟠 ALTA

**Descrição:**
Operador de validação ISO 20022 PACS.008 não disponível na UI.

**Localização:**
- ❌ FrontEnd: FALTA
- ✅ BackEnd: `RuleCondition.java`
- ✅ PostgreSQL: `V34__add_v31_plus_operators.sql`

**Recomendação:**
```typescript
{ value: 'PACS008_FIELD_VALIDATION', label: 'PACS.008 Field Validation', 
  description: 'Validação de campos ISO 20022 PACS.008', 
  requiresValue: true, category: 'Regulatory' },
```

---

### GAP-004: PLT_DS2_RULE_ENGINE falta no FrontEnd

**Severidade:** 🟠 ALTA

**Descrição:**
Operador PLT DS2 não disponível na UI.

**Localização:**
- ❌ FrontEnd: FALTA
- ✅ BackEnd: `RuleCondition.java`
- ✅ PostgreSQL: `V34__add_v31_plus_operators.sql`

**Recomendação:**
```typescript
{ value: 'PLT_DS2_RULE_ENGINE', label: 'PLT DS2 Rule Engine', 
  description: 'Motor de regras PLT DS2', 
  requiresValue: true, category: 'PLT' },
```

---

### GAP-005: PSD3_COP_NAME_MATCH falta no FrontEnd

**Severidade:** 🟠 ALTA

**Descrição:**
Operador PSD3 Confirmation of Payee não disponível na UI.

**Localização:**
- ❌ FrontEnd: FALTA
- ✅ BackEnd: `RuleCondition.java`
- ✅ PostgreSQL: `V34__add_v31_plus_operators.sql`

**Recomendação:**
```typescript
{ value: 'PSD3_COP_NAME_MATCH', label: 'PSD3 CoP Name Match', 
  description: 'Verificação de nome PSD3 Confirmation of Payee', 
  requiresValue: true, category: 'Regulatory' },
```

---

### GAP-006: SCA_DYNAMIC_3DS_ROUTING falta no FrontEnd

**Severidade:** 🟠 ALTA

**Descrição:**
Operador SCA Dynamic 3DS Routing não disponível na UI.

**Localização:**
- ❌ FrontEnd: FALTA
- ✅ BackEnd: `RuleCondition.java`
- ✅ PostgreSQL: `V34__add_v31_plus_operators.sql`

**Recomendação:**
```typescript
{ value: 'SCA_DYNAMIC_3DS_ROUTING', label: 'SCA Dynamic 3DS Routing', 
  description: 'Roteamento dinâmico 3DS para SCA', 
  requiresValue: true, category: 'SCA' },
```

---

## 🟡 GAPS MÉDIOS

### GAP-007: Nomenclatura Inconsistente - Operadores Parciais no FrontEnd

**Severidade:** 🟡 MÉDIA

**Descrição:**
Alguns operadores no FrontEnd têm nomes truncados ou parciais:

| FrontEnd | BackEnd | Correto |
|----------|---------|---------|
| `HAS_FAILED_` | `HAS_FAILED_3DS_LAST_N_MINUTES` | BackEnd |
| `NEO` | `NEO4J_*` | BackEnd |
| `PACS` | `PACS008_FIELD_VALIDATION` | BackEnd |
| `PLT_DS` | `PLT_DS2_RULE_ENGINE` | BackEnd |
| `PSD` | `PSD3_COP_NAME_MATCH` | BackEnd |
| `SCA_DYNAMIC_` | `SCA_DYNAMIC_3DS_ROUTING` | BackEnd |

**Impacto:**
- Confusão na manutenção
- Possíveis erros de mapeamento
- Inconsistência na documentação

**Recomendação:**
Corrigir os nomes no FrontEnd para corresponder exatamente ao BackEnd.

---

## 🟢 INFO: ValueTypes (Não são Gaps)

Os seguintes itens aparecem no BackEnd mas **NÃO são operadores**, são **tipos de valor** (`ConditionValueType`):

| ValueType | Descrição |
|-----------|-----------|
| STRING | Tipo string |
| NUMBER | Tipo numérico |
| BOOLEAN | Tipo booleano |
| DATE | Tipo data |
| TIME | Tipo hora |
| DATETIME | Tipo data/hora |
| ARRAY_STRING | Array de strings |
| ARRAY_NUMBER | Array de números |
| FIELD_REFERENCE | Referência a campo |
| EXPRESSION | Expressão |
| GEO_POINT | Ponto geográfico |

**Ação:** Nenhuma necessária. Estes são tipos de valor, não operadores.

---

## 📈 Plano de Ação

### Semana 1: Resolver Gaps Críticos e Altos

| Prioridade | Gap | Ação | Responsável | Tempo |
|------------|-----|------|-------------|-------|
| 🔴 P0 | GAP-001 | Adicionar PIG_BUTCHERING_INDICATOR ao BackEnd | Backend Dev | 15min |
| 🟠 P1 | GAP-002 | Adicionar HAS_FAILED_3DS_LAST_N_MINUTES ao FrontEnd | Frontend Dev | 10min |
| 🟠 P1 | GAP-003 | Adicionar PACS008_FIELD_VALIDATION ao FrontEnd | Frontend Dev | 10min |
| 🟠 P1 | GAP-004 | Adicionar PLT_DS2_RULE_ENGINE ao FrontEnd | Frontend Dev | 10min |
| 🟠 P1 | GAP-005 | Adicionar PSD3_COP_NAME_MATCH ao FrontEnd | Frontend Dev | 10min |
| 🟠 P1 | GAP-006 | Adicionar SCA_DYNAMIC_3DS_ROUTING ao FrontEnd | Frontend Dev | 10min |

### Semana 2: Resolver Gaps Médios

| Prioridade | Gap | Ação | Responsável | Tempo |
|------------|-----|------|-------------|-------|
| 🟡 P2 | GAP-007 | Corrigir nomenclatura inconsistente | Full Stack | 30min |

---

## ✅ Verificação Pós-Correção

Após implementar as correções, executar:

```bash
# Testes de sincronização do BackEnd
cd ~/repos/RULEX && mvn -f backend/pom.xml test -Dtest=OperatorSyncTest

# Testes do FrontEnd
cd ~/repos/RULEX && pnpm test -- --run

# Verificar git status
git status
```

---

## 📊 Métricas de Conformidade Atual

| Camada | Operadores | Conformes | % |
|--------|------------|-----------|---|
| FrontEnd | 448 | 443 | 98.9% |
| BackEnd | 457 | 456 | 99.8% |
| PostgreSQL | 448 | 447 | 99.8% |
| Redis | 17 | 17 | 100% |
| Neo4j | 18 | 18 | 100% |
| **MÉDIA** | **-** | **-** | **99.7%** |

---

## 🔗 Referências

- [operators_inventory.md](./operators_inventory.md)
- [conformidade_matriz.csv](./conformidade_matriz.csv)
- [validation_report.md](./validation_report.md)
- [consistency_report.md](./consistency_report.md)
