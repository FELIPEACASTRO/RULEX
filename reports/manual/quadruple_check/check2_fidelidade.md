# CHECK #2 — FIDELIDADE AO CÓDIGO (ZERO ALUCINAÇÃO)

Data: 2026-01-16 15:35:39 UTC

## MAPEAMENTO DE FONTES

### Operadores
- **Fonte primária**: `backend/src/main/java/com/rulex/entity/complex/RuleCondition.java`
- **Enum**: `ConditionOperator`
- **Arquivo gerado**: `client/src/manual/generated/backendOperators.generated.ts`
- **Script gerador**: `scripts/manual-generate.mjs`

### Ações
- **Fonte primária**: `backend/src/main/java/com/rulex/entity/complex/RuleAction.java`
- **Enum**: `ActionType`
- **Arquivo gerado**: `client/src/manual/generated/backendActions.generated.ts`

### Funções de Expressão
- **Fonte primária**: `backend/src/main/java/com/rulex/service/complex/ExpressionEvaluator.java`
- **Arquivo gerado**: `client/src/manual/generated/expressionFunctions.generated.ts`

### Operadores Lógicos
- **Fonte primária**: `backend/src/main/java/com/rulex/entity/complex/RuleConditionGroup.java`
- **Enum**: `GroupLogicOperator`
- **Arquivo gerado**: `client/src/manual/generated/logicOperators.generated.ts`

### API Endpoints
- **Fonte primária**: `openapi/rulex.yaml`
- **Arquivo gerado**: `client/src/manual/generated/openapiSummary.generated.ts`

### Documentos
- **Fonte primária**: `docs/*.md`
- **Arquivo gerado**: `client/src/manual/generated/docsIndex.generated.ts`

### Campos do Payload
- **Fonte primária**: `client/src/manual/manualData.ts` (FIELD_LABELS)
- **Baseado em**: `backend/src/main/java/com/rulex/dto/TransactionRequest.java`

## EVIDÊNCIAS DE EXTRAÇÃO

### Amostra de Operadores (primeiros 10 do enum)
  public enum ConditionOperator {
    // Comparação básica
    EQ,
    NEQ,
    GT,
    GTE,
    LT,
    LTE,
    // Listas
    IN,
    NOT_IN,
    // Strings
    CONTAINS,
    NOT_CONTAINS,
    STARTS_WITH,

### Amostra de Ações (enum ActionType)
  public enum ActionType {
    SET_DECISION,
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
