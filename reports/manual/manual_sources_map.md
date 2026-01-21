# MANUAL SOURCES MAP — RULEX

**Data**: 2026-01-16
**Branch**: cursor/rulex-project-review-1c58

## MAPEAMENTO COMPLETO: SEÇÃO → FONTE → EVIDÊNCIA

### 1. Visão Geral
| Conteúdo | Fonte | Arquivo |
|----------|-------|---------|
| Estatísticas | manualData.ts | `client/src/manual/manualData.ts` |
| Descrição do sistema | README.md | `README.md` |

### 2. Mapa do Sistema (Telas/Rotas)
| Conteúdo | Fonte | Arquivo |
|----------|-------|---------|
| Rotas | App.tsx | `client/src/App.tsx` |
| Menu | DashboardLayout.tsx | `client/src/components/DashboardLayout.tsx` |

### 3. Infra/Runbook
| Conteúdo | Fonte | Arquivo |
|----------|-------|---------|
| Docker Compose | docker-compose.yml | `docker-compose.yml` |
| Scripts | package.json | `package.json` |
| Variáveis | .env.example | `.env.example` |

### 4. Fluxo Runtime
| Conteúdo | Fonte | Arquivo |
|----------|-------|---------|
| Tipos de Regra | manualData.ts | `client/src/manual/manualData.ts` |
| Classificações | manualData.ts | `client/src/manual/manualData.ts` |
| Status | manualData.ts | `client/src/manual/manualData.ts` |

### 5. Payload (Campos)
| Conteúdo | Fonte | Arquivo |
|----------|-------|---------|
| FIELD_LABELS | manualData.ts | `client/src/manual/manualData.ts` |
| TransactionRequest | DTO Java | `backend/src/main/java/com/rulex/dto/TransactionRequest.java` |

### 6. Regras Simples
| Conteúdo | Fonte | Arquivo |
|----------|-------|---------|
| Estrutura | manualData.ts | `client/src/manual/manualData.ts` |
| Exemplos | MANUAL_TEMPLATES | `client/src/manual/manualData.ts` |

### 7. Regras Complexas
| Conteúdo | Fonte | Arquivo |
|----------|-------|---------|
| Grupos | RuleConditionGroup.java | `backend/src/main/java/com/rulex/entity/complex/RuleConditionGroup.java` |
| Operadores Lógicos | logicOperators.generated.ts | `client/src/manual/generated/logicOperators.generated.ts` |
| ValueTypes | manualData.ts | `client/src/manual/manualData.ts` |

### 8. Operadores (447)
| Conteúdo | Fonte | Arquivo |
|----------|-------|---------|
| Enum ConditionOperator | RuleCondition.java | `backend/src/main/java/com/rulex/entity/complex/RuleCondition.java` |
| Gerado | backendOperators.generated.ts | `client/src/manual/generated/backendOperators.generated.ts` |
| Frontend | operators.ts | `client/src/lib/operators.ts` |
| Categorias | OPERATOR_CATEGORY_EXPLANATIONS | `client/src/manual/manualData.ts` |

### 9. Funções/Expressões (23)
| Conteúdo | Fonte | Arquivo |
|----------|-------|---------|
| ExpressionEvaluator | ExpressionEvaluator.java | `backend/src/main/java/com/rulex/service/complex/ExpressionEvaluator.java` |
| Gerado | expressionFunctions.generated.ts | `client/src/manual/generated/expressionFunctions.generated.ts` |
| Allowlist AST | AstValidator.java | `backend/src/main/java/com/rulex/v31/ast/AstValidator.java` |

### 10. Ações (10)
| Conteúdo | Fonte | Arquivo |
|----------|-------|---------|
| Enum ActionType | RuleAction.java | `backend/src/main/java/com/rulex/entity/complex/RuleAction.java` |
| Gerado | backendActions.generated.ts | `client/src/manual/generated/backendActions.generated.ts` |

### 11. API (18 endpoints)
| Conteúdo | Fonte | Arquivo |
|----------|-------|---------|
| OpenAPI | rulex.yaml | `openapi/rulex.yaml` |
| Gerado | openapiSummary.generated.ts | `client/src/manual/generated/openapiSummary.generated.ts` |

### 12. Banco de Dados (35 migrations)
| Conteúdo | Fonte | Arquivo |
|----------|-------|---------|
| Migrations | V*.sql | `backend/src/main/resources/db/migration/` |
| Schema | V1__init.sql | `backend/src/main/resources/db/migration/V1__init.sql` |

### 13. Templates/Exemplos
| Conteúdo | Fonte | Arquivo |
|----------|-------|---------|
| MANUAL_TEMPLATES | manualData.ts | `client/src/manual/manualData.ts` |

### 14. QA/E2E
| Conteúdo | Fonte | Arquivo |
|----------|-------|---------|
| Testes Frontend | *.test.tsx | `client/src/**/*.test.tsx` |
| Testes Backend | *Test.java | `backend/src/test/java/**/*Test.java` |
| E2E | e2e/*.spec.ts | `e2e/*.spec.ts` |

### 15. FAQ
| Conteúdo | Fonte | Arquivo |
|----------|-------|---------|
| Perguntas | Manual.tsx | `client/src/pages/Manual.tsx` (hardcoded) |

### 16. Glossário
| Conteúdo | Fonte | Arquivo |
|----------|-------|---------|
| Termos | Manual.tsx | `client/src/pages/Manual.tsx` (hardcoded) |

---

## SCRIPT DE GERAÇÃO

O script `scripts/manual-generate.mjs` é responsável por:
1. Extrair operadores do enum Java
2. Extrair ações do enum Java
3. Extrair funções do ExpressionEvaluator
4. Extrair endpoints do OpenAPI
5. Indexar documentos Markdown
6. Validar consistência FE vs BE

**Comando**: `pnpm manual:generate`

---

## ARQUIVOS GERADOS

| Arquivo | Conteúdo | Linhas |
|---------|----------|--------|
| backendOperators.generated.ts | 447 operadores | 2251 |
| backendActions.generated.ts | 10 ações | 64 |
| logicOperators.generated.ts | 6 operadores lógicos | 42 |
| expressionFunctions.generated.ts | 23 funções | 203 |
| astAllowlist.generated.ts | Allowlist do AST | 53 |
| openapiSummary.generated.ts | 18 endpoints | 243 |
| docsIndex.generated.ts | 41 documentos | 307 |
| index.ts | Re-exports | 10 |

**Total**: 3173 linhas de código gerado automaticamente
