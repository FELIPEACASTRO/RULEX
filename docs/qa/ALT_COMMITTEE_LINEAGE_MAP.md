# ALT_COMMITTEE_LINEAGE_MAP.md
## Mapa de Linhagem do Sistema RULEX

**Data:** 2025-12-30
**Auditor:** Comitê Alternativo de Auditoria

---

## 1. ESTRUTURA DO SISTEMA

### 1.1 Módulos Principais
```
RULEX/
├── backend/          # Spring Boot (Java 21) - Motor de Regras
├── client/src/       # React/Vite - Frontend
├── e2e/              # Testes E2E (Playwright)
└── docs/qa/          # Documentação de QA
```

### 1.2 Stack Tecnológico
- **Backend:** Spring Boot 3.5.9, Java 21, Hibernate 6.x
- **Frontend:** React, Vite, TypeScript
- **Banco:** PostgreSQL 16 (Alpine)
- **Containerização:** Docker Compose

---

## 2. FLUXO DE REGRAS (LINHAGEM)

### 2.1 Criação de Regra
```
[Frontend] → POST /api/homolog/rules
    ↓
[HomologRuleController] → HomologRuleApplicationService
    ↓
[HomologRuleUseCase] → Validação + Persistência
    ↓
[RuleEntity + RuleVersionEntity] → PostgreSQL (rules + rule_versions)
```

### 2.2 Publicação de Regra
```
[Frontend] → POST /api/homolog/rules/versions/{id}/publish
    ↓
[HomologRuleController] → HomologRuleApplicationService.publish()
    ↓
[RuleVersionEntity.status] → DRAFT → PUBLISHED
    ↓
[AuditEntryEntity] → Registro de auditoria
```

### 2.3 Criação de RuleSet
```
[Frontend] → POST /api/homolog/rulesets
    ↓
[HomologRuleSetController] → HomologRuleSetApplicationService
    ↓
[RuleSetEntity + RuleSetVersionEntity + RuleSetVersionItemEntity]
    ↓
PostgreSQL (rule_sets + rule_set_versions + rule_set_version_items)
```

### 2.4 Simulação/Execução
```
[Frontend] → POST /api/homolog/simulations/run
    ↓
[HomologSimulationController] → HomologSimulationApplicationService
    ↓
[ActiveRuleSetEntity] → Carrega RuleSet ativo
    ↓
[RuleEngineService] → Avalia regras contra payload
    ↓
[SimulationRunEntity + DecisionLogEntity] → Persiste resultado
```

---

## 3. TABELAS PRINCIPAIS (SCHEMA)

### 3.1 Regras
| Tabela | Descrição |
|--------|-----------|
| `rules` | Metadados da regra (key, title) |
| `rule_versions` | Versões da regra (conditions_json, status, priority) |
| `rule_condition_groups` | Grupos de condições aninhados (V8) |
| `rule_conditions` | Condições individuais (V8) |

### 3.2 RuleSets
| Tabela | Descrição |
|--------|-----------|
| `rule_sets` | Metadados do conjunto de regras |
| `rule_set_versions` | Versões do conjunto |
| `rule_set_version_items` | Itens (regras) do conjunto |
| `active_rule_set` | RuleSet atualmente ativo |

### 3.3 Execução/Auditoria
| Tabela | Descrição |
|--------|-----------|
| `simulation_runs` | Execuções de simulação |
| `decision_log` | Log de decisões |
| `audit_log` | Trilha de auditoria |
| `rule_execution_log` | Log detalhado de execução (V31) |

---

## 4. TIPOS ENUM DO POSTGRESQL

```sql
-- Enums customizados
CREATE TYPE decision_outcome AS ENUM ('APROVADO', 'SUSPEITA_DE_FRAUDE', 'FRAUDE');
CREATE TYPE rule_status AS ENUM ('DRAFT', 'PUBLISHED', 'DEPRECATED');
CREATE TYPE logic_operator AS ENUM ('AND', 'OR');
CREATE TYPE audit_action_type AS ENUM (...);
CREATE TYPE audit_result AS ENUM ('SUCCESS', 'FAILURE');
```

---

## 5. ENDPOINTS PRINCIPAIS

### 5.1 Regras (Homolog)
| Método | Endpoint | Descrição |
|--------|----------|-----------|
| POST | `/api/homolog/rules` | Criar regra |
| GET | `/api/homolog/rules/{id}/latest` | Obter última versão |
| POST | `/api/homolog/rules/versions/{id}/publish` | Publicar |
| POST | `/api/homolog/rules/{id}/rollback/{version}` | Rollback |

### 5.2 RuleSets
| Método | Endpoint | Descrição |
|--------|----------|-----------|
| POST | `/api/homolog/rulesets` | Criar RuleSet |
| POST | `/api/homolog/rulesets/versions/{id}/publish` | Publicar |
| POST | `/api/homolog/rulesets/activate` | Ativar |

### 5.3 Simulação
| Método | Endpoint | Descrição |
|--------|----------|-----------|
| POST | `/api/homolog/simulations/run` | Executar simulação |

### 5.4 Export/Import
| Método | Endpoint | Descrição |
|--------|----------|-----------|
| GET | `/api/api/v1/rules/export-import/export` | Exportar regras |
| POST | `/api/api/v1/rules/export-import/import` | Importar regras |

---

## 6. BACKUP/RESTORE

### 6.1 Mecanismo de Export
- **Formato:** YAML ou JSON
- **Endpoint:** `GET /api/api/v1/rules/export-import/export?format=yaml`
- **Conteúdo:** Todas as regras com metadados, versões, condições

### 6.2 Mecanismo de Import
- **Endpoint:** `POST /api/api/v1/rules/export-import/import`
- **Validação:** Verifica conflitos de key antes de importar

---

## 7. EVIDÊNCIAS DE FUNCIONAMENTO

### 7.1 Criação de Regra ✅
```json
{
  "id": "5a67ba6b-fdce-4a94-9f17-ca9bd189f12c",
  "key": "RULE_SIMPLE_001",
  "status": "DRAFT"
}
```

### 7.2 Publicação ✅
```
Status: DRAFT → PUBLISHED
```

### 7.3 Simulação ✅
```json
{
  "decision": "SUSPEITA_DE_FRAUDE",
  "riskScore": 70,
  "triggeredRules": [{"name": "RULE_SIMPLE_001", "weight": 70}]
}
```

---

**Documento gerado pelo Comitê Alternativo de Auditoria**
