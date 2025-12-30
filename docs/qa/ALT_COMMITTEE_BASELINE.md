# ALT_COMMITTEE_BASELINE.md
## Mapa do Sistema RULEX - Auditoria Comitê Alternativo

**Data:** 2025-12-30
**Auditor:** Comitê Alternativo de Auditoria

---

## 1. ESTRUTURA DO SISTEMA

### 1.1 Módulos
```
RULEX/
├── backend/                    # Spring Boot (Java 21)
│   ├── src/main/java/com/rulex/
│   │   ├── controller/homolog/ # Controllers de homologação
│   │   ├── entity/homolog/     # Entidades JPA
│   │   ├── service/            # Serviços de negócio
│   │   └── homolog/            # Use cases (Clean Architecture)
│   └── src/main/resources/
│       └── db/migration/       # Flyway migrations (V1-V9)
├── client/src/                 # React/Vite Frontend
├── e2e/                        # Testes E2E (Playwright)
├── scripts/                    # Scripts de automação
│   └── validate.sh             # Validação completa
└── docs/qa/                    # Documentação de QA
```

### 1.2 Stack Tecnológico
| Componente | Tecnologia | Versão |
|------------|------------|--------|
| Backend | Spring Boot | 3.5.9 |
| Java | OpenJDK | 21 |
| ORM | Hibernate | 6.x |
| Frontend | React + Vite | - |
| Banco | PostgreSQL | 16 (Alpine) |
| Testes | JUnit, Vitest, Playwright | - |
| Container | Docker Compose | - |

---

## 2. ENDPOINTS DE HOMOLOGAÇÃO

### 2.1 Regras
| Método | Endpoint | Descrição |
|--------|----------|-----------|
| POST | `/api/homolog/rules` | Criar regra + versão 1 (DRAFT) |
| GET | `/api/homolog/rules/{ruleId}/latest` | Obter última versão |
| POST | `/api/homolog/rules/versions/{id}/publish` | Publicar versão |
| POST | `/api/homolog/rules/{ruleId}/rollback/{version}` | Rollback |

### 2.2 RuleSets
| Método | Endpoint | Descrição |
|--------|----------|-----------|
| POST | `/api/homolog/rulesets` | Criar RuleSet |
| POST | `/api/homolog/rulesets/versions/{id}/publish` | Publicar |
| POST | `/api/homolog/rulesets/activate` | Ativar |

### 2.3 Simulação
| Método | Endpoint | Descrição |
|--------|----------|-----------|
| POST | `/api/homolog/simulations/run` | Executar simulação |

### 2.4 Export/Import
| Método | Endpoint | Descrição |
|--------|----------|-----------|
| GET | `/api/api/v1/rules/export-import/export` | Exportar regras |
| POST | `/api/api/v1/rules/export-import/import` | Importar regras |

---

## 3. SCHEMA DO BANCO (PRINCIPAIS TABELAS)

### 3.1 Regras
- `rules` - Metadados da regra
- `rule_versions` - Versões com conditions_json (JSONB)
- `rule_condition_groups` - Grupos aninhados (V8)
- `rule_conditions` - Condições individuais (V8)

### 3.2 RuleSets
- `rule_sets` - Metadados do conjunto
- `rule_set_versions` - Versões do conjunto
- `rule_set_version_items` - Itens do conjunto
- `active_rule_set` - RuleSet ativo

### 3.3 Execução/Auditoria
- `simulation_runs` - Execuções de simulação
- `decision_log` - Log de decisões
- `audit_log` - Trilha de auditoria

---

## 4. AUTENTICAÇÃO

- **Tipo:** HTTP Basic
- **Usuário Admin:** admin / rulex
- **Header opcional:** `X-Actor-Email: admin@rulex.local`

---

## 5. COMANDOS DE VALIDAÇÃO

```bash
# Validação completa
./scripts/validate.sh

# Backend
cd backend && mvn -q test -Pcoverage

# Frontend
pnpm test && pnpm check && pnpm build

# E2E
docker compose up -d --build
pnpm exec playwright test
docker compose down -v
```

---

**Documento gerado pelo Comitê Alternativo de Auditoria**
