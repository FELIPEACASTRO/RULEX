# Central de Diagramas — Solução RULEX (Relatório)

Este relatório é gerado automaticamente a partir do repositório (PASSADA 1: inventário).

- Gerado em: 2026-01-16T21:28:52.991Z
- Fonte: `RULEX`
- Inventário (JSON): `docs/DIAGRAMS_RULEX_INVENTORY.json`

## PASSADA 1 — Inventário (evidência do repositório)

### Backend (Spring Boot)

- Controllers (@RestController/@Controller): 16
- Endpoints (mapeamentos detectados): 90
- Services (@Service): 43
- Entities (@Entity): 57
- Repositories (extends JpaRepository/CrudRepository): 57
- Flyway migrations: 35
- Arquivos de config (resources): 4

**OpenAPI (springdoc)**
- Dependência encontrada: backend/pom.xml#L118

**Controllers detectados (arquivo → basePath → endpoints)**
- backend/src/main/java/com/rulex/controller/AuditController.java#L30 — basePath=`/audit` — endpoints=4
- backend/src/main/java/com/rulex/controller/complex/ComplexRuleController.java#L20 — basePath=`/v1/complex-rules` — endpoints=16
- backend/src/main/java/com/rulex/controller/ComplexRuleCrudController.java#L21 — basePath=`/complex-rules` — endpoints=9
- backend/src/main/java/com/rulex/controller/EvaluateController.java#L26 — basePath=`/evaluate` — endpoints=2
- backend/src/main/java/com/rulex/controller/homolog/HomologRuleController.java#L12 — basePath=`/homolog/rules` — endpoints=4
- backend/src/main/java/com/rulex/controller/homolog/HomologRuleSetController.java#L9 — basePath=`/homolog/rulesets` — endpoints=3
- backend/src/main/java/com/rulex/controller/homolog/HomologSimulationController.java#L10 — basePath=`/homolog/simulations` — endpoints=1
- backend/src/main/java/com/rulex/controller/MetricsController.java#L14 — basePath=`/metrics` — endpoints=4
- backend/src/main/java/com/rulex/controller/RuleApprovalController.java#L26 — basePath=`/rules/approvals` — endpoints=11
- backend/src/main/java/com/rulex/controller/RuleController.java#L22 — basePath=`/rules` — endpoints=8
- backend/src/main/java/com/rulex/controller/RuleExportImportController.java#L19 — basePath=`/v1/rules/export-import` — endpoints=8
- backend/src/main/java/com/rulex/controller/RuleMetricsController.java#L19 — basePath=`/rules/metrics` — endpoints=5
- backend/src/main/java/com/rulex/controller/RuleSimulationController.java#L24 — basePath=`/rules/simulation` — endpoints=4
- backend/src/main/java/com/rulex/controller/TransactionController.java#L40 — basePath=`/transactions` — endpoints=7
- backend/src/main/java/com/rulex/v31/field/FieldDictionaryController.java#L14 — basePath=`/field-dictionary` — endpoints=1
- backend/src/main/java/com/rulex/v31/rules/RulesV31Controller.java#L19 — basePath=`/rules` — endpoints=3

### Banco de dados (Flyway)

**Migrations (ordem lexical / versão)**
- backend/src/main/resources/db/migration/V1__init.sql (V1__init)
- backend/src/main/resources/db/migration/V10__derived_context_improvements.sql (V10__derived_context_improvements)
- backend/src/main/resources/db/migration/V11__bin_lookup_table.sql (V11__bin_lookup_table)
- backend/src/main/resources/db/migration/V12__complex_rules_crud.sql (V12__complex_rules_crud)
- backend/src/main/resources/db/migration/V13__geo_reference_table.sql (V13__geo_reference_table)
- backend/src/main/resources/db/migration/V14__velocity_counters.sql (V14__velocity_counters)
- backend/src/main/resources/db/migration/V15__add_velocity_operators.sql (V15__add_velocity_operators)
- backend/src/main/resources/db/migration/V16__fix_geo_polygon_id_type.sql (V16__fix_geo_polygon_id_type)
- backend/src/main/resources/db/migration/V17__fix_geo_reference_id_type.sql (V17__fix_geo_reference_id_type)
- backend/src/main/resources/db/migration/V18__enable_condition_groups_constraint.sql (V18__enable_condition_groups_constraint)
- backend/src/main/resources/db/migration/V19__access_log_table.sql (V19__access_log_table)
- backend/src/main/resources/db/migration/V2__core_schema.sql (V2__core_schema)
- backend/src/main/resources/db/migration/V20__shadow_mode_and_device_fingerprinting.sql (V20__shadow_mode_and_device_fingerprinting)
- backend/src/main/resources/db/migration/V21__rule_configurations_shadow_mode.sql (V21__rule_configurations_shadow_mode)
- backend/src/main/resources/db/migration/V22__fraud_detection_rules_seed.sql (V22__fraud_detection_rules_seed)
- backend/src/main/resources/db/migration/V23__web_research_fraud_rules.sql (V23__web_research_fraud_rules)
- backend/src/main/resources/db/migration/V24__regras_fraude_portugues_completo.sql (V24__regras_fraude_portugues_completo)
- backend/src/main/resources/db/migration/V25__additional_fraud_rules_200plus.sql (V25__additional_fraud_rules_200plus)
- backend/src/main/resources/db/migration/V26__fix_complex_rules_conditions.sql (V26__fix_complex_rules_conditions)
- backend/src/main/resources/db/migration/V27__migrate_hardcoded_advanced_rules.sql (V27__migrate_hardcoded_advanced_rules)
- backend/src/main/resources/db/migration/V28__add_missing_condition_operators.sql (V28__add_missing_condition_operators)
- backend/src/main/resources/db/migration/V29__insert_advanced_fraud_rules_catalog.sql (V29__insert_advanced_fraud_rules_catalog)
- backend/src/main/resources/db/migration/V3__extend_workflow_length.sql (V3__extend_workflow_length)
- backend/src/main/resources/db/migration/V30__insert_aml_ato_advanced_rules.sql (V30__insert_aml_ato_advanced_rules)
- backend/src/main/resources/db/migration/V31__add_velocity_extended_fields.sql (V31__add_velocity_extended_fields)
- backend/src/main/resources/db/migration/V32__add_missing_tables_for_operators.sql (V32__add_missing_tables_for_operators)
- backend/src/main/resources/db/migration/V33__fix_pos_entry_mode_length.sql (V33__fix_pos_entry_mode_length)
- backend/src/main/resources/db/migration/V34__add_v31_plus_operators.sql (V34__add_v31_plus_operators)
- backend/src/main/resources/db/migration/V35__add_velocity_temporal_indexes.sql (V35__add_velocity_temporal_indexes)
- backend/src/main/resources/db/migration/V4__raw_hash_idempotency.sql (V4__raw_hash_idempotency)
- backend/src/main/resources/db/migration/V5__raw_as_received.sql (V5__raw_as_received)
- backend/src/main/resources/db/migration/V6__v31_exec_log_field_dictionary.sql (V6__v31_exec_log_field_dictionary)
- backend/src/main/resources/db/migration/V7__v31_exec_log_dedup.sql (V7__v31_exec_log_dedup)
- backend/src/main/resources/db/migration/V8__complex_rules_support.sql (V8__complex_rules_support)
- backend/src/main/resources/db/migration/V9__audit_compliance_enhancements.sql (V9__audit_compliance_enhancements)

### Frontend (React)

- Pages (client/src/pages): 27
**Pages detectadas**
- client/src/pages/Audit.test.tsx
- client/src/pages/Audit.tsx
- client/src/pages/ComplexRules.tsx
- client/src/pages/ComponentShowcase.tsx
- client/src/pages/Dashboard.test.tsx
- client/src/pages/Dashboard.tsx
- client/src/pages/DashboardProfessional.tsx
- client/src/pages/Diagrams.tsx
- client/src/pages/DiagramsHub.tsx
- client/src/pages/Home.test.tsx
- client/src/pages/Home.tsx
- client/src/pages/Login.test.tsx
- client/src/pages/Login.tsx
- client/src/pages/Manual.test.tsx
- client/src/pages/Manual.tsx
- client/src/pages/Monitoring.tsx
- client/src/pages/NotFound.test.tsx
- client/src/pages/NotFound.tsx
- client/src/pages/Rules.test.tsx
- client/src/pages/Rules.tsx
- client/src/pages/RulesAdvanced.tsx
- client/src/pages/RulesDidactic.tsx
- client/src/pages/Settings.tsx
- client/src/pages/Transactions.test.tsx
- client/src/pages/Transactions.tsx
- client/src/pages/TransactionSimulator.tsx
- client/src/pages/TransactionsProfessional.tsx

### Infra/Execução

- docker-compose services: 5
**docker-compose.yml → services**
- postgres (docker-compose.yml#L2)
- redis (docker-compose.yml#L18)
- neo4j (docker-compose.yml#L30)
- backend (docker-compose.yml#L50)
- web (docker-compose.yml#L85)

### OpenAPI Specs

- Specs em openapi/: 1
- openapi/rulex.yaml

## Próximas passadas (não executadas nesta etapa)

- PASSADA 2 (consistência cruzada): cruzar rotas frontend ↔ controllers ↔ OpenAPI ↔ Insomnia ↔ e2e.
- PASSADA 3 (validação executável): rodar build/test e validar que os diagramas-instância gerados batem com a execução real.

## Observações (anti-alucinação)

- Este inventário é puramente estático (regex/heurísticas). Nenhum diagrama foi ‘inventado’ nesta fase.
- Alguns endpoints podem ser contados duas vezes se houver anotações compostas; a PASSADA 2 normaliza.
