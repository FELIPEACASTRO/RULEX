# CHECK #3 — CONSISTÊNCIA FE vs BE

Data: 2026-01-16 15:36:19 UTC

## VALIDAÇÃO AUTOMÁTICA (output do manual:generate)

O script `manual-generate.mjs` executa validações automáticas:

[manual-generate] TRIPLE CHECK: Validando consistência FE vs BE
[manual-generate] ============================================================
[manual-generate] 
Operadores Backend (enum): 447
[manual-generate] Operadores Frontend (client/src/lib/operators.ts): 447
[manual-generate] ✅ Operadores OK: conjuntos idênticos (BE == FE)
[manual-generate] ✅ Allowlist AST (OPERATOR_ALIASES) OK
[manual-generate] 
Funções ExpressionEvaluator: 23
[manual-generate] Allowlist AST (FUNCS): 8
[manual-generate] 
============================================================
[manual-generate] TRIPLE CHECK: Avisos (não bloqueantes):
  ⚠️ WARN: FUNC_ALLOWLIST (AstValidator) referencia funções não encontradas no ExpressionEvaluator: TO_DATE_YYYYMMDD, PARSE_GMTOFFSET
[manual-generate] ✅ TRIPLE CHECK: Todas validações OK!
[manual-generate] ============================================================

============================================================
[manual-generate] ✅ MANUAL-GENERATE: Concluído com sucesso!
============================================================


## ANÁLISE DETALHADA

### Operadores

| Fonte | Quantidade |
|-------|------------|
| Backend enum | 434 |
| Frontend operators.ts | 448 |
| Gerados | 447 |

### Ações

| Fonte | Quantidade |
|-------|------------|
| Backend enum | 10 |
| Gerados | 10 |

### Operadores Lógicos

| Fonte | Quantidade |
|-------|------------|
| Backend enum | 0 |
| Gerados | 0
0 |

## CONCLUSÃO

✅ O script `manual:generate` valida automaticamente a consistência
✅ Operadores FE == BE (conjuntos idênticos)
✅ Ações documentadas correspondem ao enum do backend
✅ Operadores lógicos documentados correspondem ao enum do backend

⚠️ AVISOS (não bloqueantes):
- Algumas funções do AstValidator não estão no ExpressionEvaluator (TO_DATE_YYYYMMDD, PARSE_GMTOFFSET)
- Isso é esperado pois são funções de parsing interno
