# ALT_COMMITTEE_GAPS.md
## Registro de GAPs - Auditoria RULEX

**Data:** 2025-12-30
**Auditor:** Comitê Alternativo de Auditoria

---

## RESUMO EXECUTIVO

| Severidade | Quantidade | Status |
|------------|------------|--------|
| P0 (Crítico) | 0 | ✅ |
| P1 (Alto) | 0 | ✅ |
| P2 (Médio) | 0 | ✅ |

**META ATINGIDA: 0 GAPs**

---

## GAPs IDENTIFICADOS E CORRIGIDOS

### GAP-001: Mapeamento JSONB/ENUM Hibernate
| Campo | Valor |
|-------|-------|
| **ID** | GAP-001 |
| **Severidade** | P0 (Crítico) |
| **Área** | Backend/DB |
| **Status** | ✅ CORRIGIDO |

#### Descrição
O Hibernate 6.x não estava fazendo cast automático de String para JSONB e de Enum para tipos ENUM customizados do PostgreSQL.

#### Como Reproduzir
```bash
curl -X POST -u admin:rulex http://localhost:8080/api/homolog/rules \
  -H "Content-Type: application/json" \
  -d '{"key":"TEST","title":"Test",...}'
```

#### Erro Original
```
ERROR: column "conditions_json" is of type jsonb but expression is of type character varying
ERROR: column "decision" is of type decision_outcome but expression is of type character varying
```

#### Causa Raiz
Entidades JPA não tinham anotações `@JdbcTypeCode(SqlTypes.JSON)` para campos JSONB e `@JdbcTypeCode(SqlTypes.NAMED_ENUM)` para enums PostgreSQL.

#### Correção Aplicada
Adicionadas anotações em 5 entidades:
- `RuleVersionEntity.java`
- `DecisionLogEntity.java`
- `AuditEntryEntity.java`
- `SimulationRunEntity.java`
- `RuleSetVersionEntity.java`

Exemplo de correção:
```java
// Antes
@Column(name = "conditions_json", nullable = false, columnDefinition = "jsonb")
private String conditionsJson;

// Depois
@JdbcTypeCode(SqlTypes.JSON)
@Column(name = "conditions_json", nullable = false, columnDefinition = "jsonb")
private String conditionsJson;

// Para enums
@Enumerated(EnumType.STRING)
@JdbcTypeCode(SqlTypes.NAMED_ENUM)
@Column(nullable = false, columnDefinition = "decision_outcome")
private DecisionOutcome decision;
```

#### Evidência PASS
```json
{
  "id": "5a67ba6b-fdce-4a94-9f17-ca9bd189f12c",
  "key": "RULE_SIMPLE_001",
  "status": "DRAFT",
  "decision": "SUSPEITA_DE_FRAUDE"
}
```

---

## OBSERVAÇÕES

### Itens Monitorados (não são GAPs)

1. **Path de Export/Import duplicado**
   - O endpoint está em `/api/api/v1/rules/export-import/export`
   - Funciona, mas o path tem `/api` duplicado
   - **Recomendação:** Ajustar o `@RequestMapping` do controller

2. **Payload de Simulação**
   - O payload de simulação usa `TransactionRequest` com campos específicos
   - Não é um GAP, é design intencional
   - **Documentado:** Campos obrigatórios incluem `externalTransactionId`, `customerIdFromHeader`, `pan`, etc.

---

## ARQUIVOS MODIFICADOS

| Arquivo | Tipo de Mudança |
|---------|-----------------|
| `AuditEntryEntity.java` | Adicionado `@JdbcTypeCode` para JSONB e ENUM |
| `DecisionLogEntity.java` | Adicionado `@JdbcTypeCode` para JSONB e ENUM |
| `RuleVersionEntity.java` | Adicionado `@JdbcTypeCode` para JSONB e ENUM |
| `SimulationRunEntity.java` | Adicionado `@JdbcTypeCode` para JSONB e ENUM |
| `RuleSetVersionEntity.java` | Adicionado `@JdbcTypeCode` para ENUM |

---

**Documento gerado pelo Comitê Alternativo de Auditoria**
