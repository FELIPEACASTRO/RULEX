# Implementacao da Tela "Manual" (Frontend)

## Objetivo
Adicionar a pagina **Manual** no frontend, acessivel pela rota `/manual` e pelo menu lateral, com conteudo **derivado diretamente do codigo** (sem inventar operacoes/operadores).

## Fonte unica de conteudo
A fonte unica foi centralizada em:
- [client/src/manual/manualData.ts](client/src/manual/manualData.ts)

Esse arquivo importa e reexporta apenas constantes que ja existem no codigo:
- RuleFormDialog: `RULE_TYPES`, `CLASSIFICATIONS`, `LOGIC_OPERATORS`, `OPERATORS`, `UNARY_OPERATORS`, `FIELD_REF_OPERATORS`, `OPERATORS_BY_TYPE`
- ComplexRuleBuilder: `LOGIC_OPERATORS`, `COMPARISON_OPERATORS`, `VALUE_TYPES`

A pagina do manual consome **somente** `MANUAL_DATA`.

## UI / Rota / Menu
- Rota adicionada em: [client/src/App.tsx](client/src/App.tsx)
- Item de menu adicionado em: [client/src/components/DashboardLayout.tsx](client/src/components/DashboardLayout.tsx)
- Pagina nova em: [client/src/pages/Manual.tsx](client/src/pages/Manual.tsx)

A pagina possui:
- Abas para separar "Regras complexas" e "Regras (formulario)"
- Tabelas com busca local (filtro por codigo/label/descricao)
- Contadores (quantidades) diretamente calculados a partir das listas reais

## Testes
Teste unitario adicionado em:
- [client/src/pages/Manual.test.tsx](client/src/pages/Manual.test.tsx)

Comandos:
- `pnpm test --run`

## Build
Comando:
- `pnpm build`

## Branch
Trabalho realizado na branch:
- `feat/manual-page`
