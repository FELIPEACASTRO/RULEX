# IMPLEMENTATION NOTES — Manual do RULEX

**Data**: 2026-01-16
**Autor**: Devin AI
**Branch**: cursor/rulex-project-review-1c58

## COMO MANTER O MANUAL ATUALIZADO

### 1. Quando adicionar novos operadores

1. Adicione o operador no enum `ConditionOperator` em:
   - `backend/src/main/java/com/rulex/entity/complex/RuleCondition.java`

2. Implemente a lógica no `ComplexRuleEvaluator`:
   - `backend/src/main/java/com/rulex/service/complex/ComplexRuleEvaluator.java`

3. Adicione no frontend:
   - `client/src/lib/operators.ts`

4. Regenere os arquivos do Manual:
   ```bash
   pnpm manual:generate
   ```

5. Verifique se passou:
   ```bash
   pnpm manual:check
   ```

### 2. Quando adicionar novas ações

1. Adicione a ação no enum `ActionType` em:
   - `backend/src/main/java/com/rulex/entity/complex/RuleAction.java`

2. Regenere:
   ```bash
   pnpm manual:generate
   ```

### 3. Quando adicionar novos endpoints

1. Atualize o OpenAPI:
   - `openapi/rulex.yaml`

2. Regenere:
   ```bash
   pnpm manual:generate
   ```

### 4. Quando adicionar novos documentos

1. Crie o arquivo Markdown em:
   - `docs/`

2. Regenere:
   ```bash
   pnpm manual:generate
   ```

## ESTRUTURA DO MANUAL

```
client/src/
├── manual/
│   ├── generated/           # Arquivos auto-gerados (NÃO EDITAR)
│   │   ├── backendOperators.generated.ts
│   │   ├── backendActions.generated.ts
│   │   ├── logicOperators.generated.ts
│   │   ├── expressionFunctions.generated.ts
│   │   ├── astAllowlist.generated.ts
│   │   ├── openapiSummary.generated.ts
│   │   ├── docsIndex.generated.ts
│   │   └── index.ts
│   ├── docs/                # Cópias de docs para o Vite
│   ├── ActionsCatalog.tsx
│   ├── ApiCatalog.tsx
│   ├── ComplexRulesGuide.tsx
│   ├── DbCatalog.tsx
│   ├── FieldDictionary.tsx
│   ├── FunctionsCatalog.tsx
│   ├── InfraRunbook.tsx
│   ├── OperatorCatalog.tsx
│   ├── QaAndE2EGuide.tsx
│   ├── SystemMap.tsx
│   ├── TemplatesGallery.tsx
│   ├── index.ts
│   └── manualData.ts        # Dados estáticos do Manual
├── pages/
│   └── Manual.tsx           # Página principal do Manual
scripts/
└── manual-generate.mjs      # Script de geração
```

## COMANDOS ÚTEIS

```bash
# Gerar arquivos do Manual
pnpm manual:generate

# Verificar tudo (generate + test + build)
pnpm manual:check

# Rodar apenas testes do Manual
pnpm test -- client/src/pages/Manual.test.tsx

# TypeScript check
pnpm check
```

## TROUBLESHOOTING

### Erro: "Operadores FE != BE"

O script `manual:generate` valida que os operadores do frontend e backend são idênticos.
Se houver divergência:

1. Verifique se adicionou o operador em ambos os lugares
2. Verifique se o nome está exatamente igual (case-sensitive)

### Erro: "TypeScript error in generated files"

Os arquivos gerados usam `[...new Set()]` que requer:
- `target: "ES2020"` ou superior no tsconfig.json
- `downlevelIteration: true` no tsconfig.json

### Erro: "Build failed"

1. Limpe o cache do TypeScript:
   ```bash
   rm -rf node_modules/typescript/tsbuildinfo
   ```

2. Reinstale dependências:
   ```bash
   pnpm install
   ```

3. Tente novamente:
   ```bash
   pnpm build
   ```

## MÉTRICAS ATUAIS

| Métrica | Valor |
|---------|-------|
| Operadores | 447 |
| Ações | 10 |
| Funções | 23 |
| Endpoints | 18 |
| Documentos | 41 |
| Migrations | 35 |
| Testes Frontend | 411 |
| Testes Backend | 279 |

---

*Última atualização: 2026-01-16*
