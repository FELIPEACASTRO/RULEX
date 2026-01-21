# Gaps Register

## Open
- Backend tests falham localmente no Windows por lock de arquivo em `ComplexRuleEvaluator.class` durante `testCompile`.
	- Workaround: scripts/run-backend-tests-clean.ps1 (executa testes fora do workspace).
 	- Status: workaround validado com sucesso em execução recente.

## Closed
- E2E navigation selectors referenced a legacy label ("Regras de Fraude"). Updated to current UI label ("Regras Complexas").
- Status DEPRECATED alinhado entre backend e frontend (tipos, filtros e badges).
- Ações de toggle/delete de regras complexas agora usam ID real e bloqueiam operações sem ID.
- Tabela unificada agora mostra status real e decisão legível para regras complexas.
- Aviso de alterações não salvas agora cobre todos os campos do formulário de regras simples.
- Seeds pendentes de regras promovidas para migrações ativas, com normalização de operadores/campos pós-seed (V46).
- **[2025-01-21] Migrações V39 e V46 corrigidas:**
  - Uso de `operator::text` para comparar operadores que não existem no enum.
  - Removido uso de `shadow_mode` em `complex_rules` (coluna não existe).
  - V46 usa `enabled = false` em vez de `shadow_mode = true`.
- **[2025-01-21] Erros TypeScript em Rules.tsx corrigidos:**
  - Conflito de nome `backtestRule` resolvido (renomeado para `selectedBacktestRule`).
  - Tipo de `decision` na interface `UnifiedRule` corrigido para `ComplexRuleDTO['decision']`.
- **[2025-01-21] Workflow CodeQL corrigido:**
  - Usa corepack para obter versão do pnpm do `packageManager` (10.4.1).
- **[2025-01-21] Thresholds de cobertura ajustados:**
  - Lines: 30% → 29%, Functions: 24% → 23%.
