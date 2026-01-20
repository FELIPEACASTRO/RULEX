# Plano de Refatoracao Consolidado (Devin + Validacao Local)

## Objetivo
Consolidar a analise do Devin com a validacao local para orientar a refatoracao do
`ComplexRuleEvaluator` e do `RuleEngineService`, sem alterar contratos e com migracao
incremental segura.

## Validacao local (valores confirmados)

### ComplexRuleEvaluator
- Linhas totais: 9.230.
- `case ... ->` encontrados no arquivo: **512**.
- `case` unicos: **504**.
- Metodos `private boolean evaluate*`: **472**.

### Avaliadores modulares
- Arquivos em `backend/src/main/java/com/rulex/service/complex/evaluator`: **31**.
- Referencias a `ConditionOperator.X` nos avaliadores: **517**.
- Operadores unicos referenciados nos avaliadores: **496**.

> Nota: o plano do Devin acerta em `472` metodos `evaluate*`, `31` avaliadores e
> `~517` referencias a operadores, mas erra o total de `cases` no switch (o valor
> correto e 512, com 504 unicos).

## Convergencia entre os planos

### Pontos de concordancia
1. O `ComplexRuleEvaluator` deve migrar para o `OperatorEvaluatorRegistry` como
   estrategia principal, reduzindo o switch central.
2. O `RuleEngineService` deve ser dividido por responsabilidades, mantendo o
   mesmo contrato externo.

### Ajustes necessarios na proposta do Devin
1. Substituir a contagem de 569 cases por 512 (504 unicos).
2. Tratar metas de linhas como consequencia (nao objetivo), priorizando coesao.
3. Planejar migracao por blocos funcionais com acoplamentos conhecidos
   (ex: velocity/geo/aml) e nao apenas por categoria nominal.

## Plano incremental (sem gaps)

### Fase 1: Mapeamento e inventario
- Executar o script `scripts/validate-operator-coverage.sh` para gerar contagens
  e listas de operadores duplicados (switch + avaliadores).
- Catalogar operadores ainda nao cobertos pelo registry.

### Fase 2: Migracao por blocos (baixo risco primeiro)
1. Basicos (EQ/NEQ/GT/GTE/LT/LTE, IN/NOT_IN, BETWEEN, IS_NULL/NOT_NULL/IS_TRUE/IS_FALSE).
2. Strings (CONTAINS, STARTS_WITH, REGEX, etc.).
3. Data/Hora (DATE_*, TIME_*, WEEKEND/HOLIDAY).

### Fase 3: Migracao por blocos (alto acoplamento)
1. Velocity e agregacoes temporais.
2. GEO e operadores derivados.
3. AML/FATF, Graph/Network, Device e Regulatory.

### Fase 4: Remocao do switch
- Manter apenas `delegateToRegistry` e erro claro para operadores nao registrados.

### Fase 5: Split do RuleEngineService
- Extrair:
  - TransactionIngestionService
  - RuleEvaluationOrchestrator
  - DecisionPersistenceService
  - AuditLoggingService
- Manter `RuleEngineService` como facade.

## Testes obrigatorios
- Testes unitarios por avaliador migrado.
- Testes de regressao para fluxos criticos (analyze/evaluate).
- Validacao de performance para evitar regressao.

## Evidencias
- Switch e delegacao via registry no `ComplexRuleEvaluator`.
- Fluxo completo de ingestao, idempotencia e auditoria no `RuleEngineService`.

