# RULEX — Checklist de Diagramas e Fluxogramas

Gerado em: 2026-01-17T15:35:05.490Z

Este arquivo é gerado automaticamente a partir do catálogo real usado na tela 'Central de Diagramas do RULEX'.

Regerar: `pnpm diagrams:sync` (ou `pnpm diagrams:checklist`)

## Resumo

- Total: 422
- Por origem: solution=213, template=209
- Por status: OK=422, PENDENTE=0
- Por renderer: mermaid=343, graph=58, matrix=9, bpmn=5, dmn=3, dfd=3, fallback=1

## Catálogo completo (1 por 1)

Legenda: `Implementado` = rendererStatus OK. `Fiel` = origin=solution + verified=true + evidência/notes.

| Origem | Verificado | Implementado | Renderer | Status | Notação | Categoria | Nome | ID | Sample |
|---|---|---|---|---|---|---|---|---|---|
| solution | SIM | SIM | mermaid | OK | FLOWCHART | processos | Fluxo real: /analyze (RULEX) | RULEX/FLOW_analyze | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Sequência real: FE → API → Engine → DB | RULEX/SEQ_analyze | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | C4 | arquitetura | C4 (Container): RULEX | RULEX/C4_container | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | ER (Core): Transaction ↔ Rule ↔ Evaluation | RULEX/ER_core | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | API Controller: RulesV31Controller | RULEX/CTRL_rulesv31controller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /rules/validate | RULEX/EP_post-rules-validate-rulesv31controller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /rules/lint | RULEX/EP_post-rules-lint-rulesv31controller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /rules/simulate | RULEX/EP_post-rules-simulate-rulesv31controller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | API Controller: FieldDictionaryController | RULEX/CTRL_fielddictionarycontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /field-dictionary | RULEX/EP_get-field-dictionary-fielddictionarycontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | API Controller: AuditController | RULEX/CTRL_auditcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /audit | RULEX/EP_get-audit-auditcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /audit/export | RULEX/EP_get-audit-export-auditcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /audit/export/csv | RULEX/EP_get-audit-export-csv-auditcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /audit/transaction/{transactionId} | RULEX/EP_get-audit-transaction-transactionid-auditcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | API Controller: ComplexRuleCrudController | RULEX/CTRL_complexrulecrudcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /complex-rules | RULEX/EP_get-complex-rules-complexrulecrudcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /complex-rules/{id} | RULEX/EP_get-complex-rules-id-complexrulecrudcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /complex-rules/key/{key} | RULEX/EP_get-complex-rules-key-key-complexrulecrudcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /complex-rules | RULEX/EP_post-complex-rules-complexrulecrudcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: PUT /complex-rules/{id} | RULEX/EP_put-complex-rules-id-complexrulecrudcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: DELETE /complex-rules/{id} | RULEX/EP_delete-complex-rules-id-complexrulecrudcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: PATCH /complex-rules/{id}/toggle | RULEX/EP_patch-complex-rules-id-toggle-complexrulecrudcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /complex-rules/{id}/duplicate | RULEX/EP_post-complex-rules-id-duplicate-complexrulecrudcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /complex-rules/validate | RULEX/EP_post-complex-rules-validate-complexrulecrudcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | API Controller: EvaluateController | RULEX/CTRL_evaluatecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /evaluate | RULEX/EP_post-evaluate-evaluatecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /evaluate/raw | RULEX/EP_post-evaluate-raw-evaluatecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | API Controller: MetricsController | RULEX/CTRL_metricscontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /metrics | RULEX/EP_get-metrics-metricscontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /metrics/mcc | RULEX/EP_get-metrics-mcc-metricscontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /metrics/merchant | RULEX/EP_get-metrics-merchant-metricscontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /metrics/timeline | RULEX/EP_get-metrics-timeline-metricscontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | API Controller: RuleApprovalController | RULEX/CTRL_ruleapprovalcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /rules/approvals/create | RULEX/EP_post-rules-approvals-create-ruleapprovalcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /rules/approvals/update/{ruleId} | RULEX/EP_post-rules-approvals-update-ruleid-ruleapprovalcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /rules/approvals/delete/{ruleId} | RULEX/EP_post-rules-approvals-delete-ruleid-ruleapprovalcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /rules/approvals/{id}/approve | RULEX/EP_post-rules-approvals-id-approve-ruleapprovalcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /rules/approvals/{id}/reject | RULEX/EP_post-rules-approvals-id-reject-ruleapprovalcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /rules/approvals/{id}/cancel | RULEX/EP_post-rules-approvals-id-cancel-ruleapprovalcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /rules/approvals/pending | RULEX/EP_get-rules-approvals-pending-ruleapprovalcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /rules/approvals/pending/page | RULEX/EP_get-rules-approvals-pending-page-ruleapprovalcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /rules/approvals/pending/count | RULEX/EP_get-rules-approvals-pending-count-ruleapprovalcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /rules/approvals/{id} | RULEX/EP_get-rules-approvals-id-ruleapprovalcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /rules/approvals/history/{ruleId} | RULEX/EP_get-rules-approvals-history-ruleid-ruleapprovalcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | API Controller: RuleController | RULEX/CTRL_rulecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /rules | RULEX/EP_get-rules-rulecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /rules/{id} | RULEX/EP_get-rules-id-rulecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /rules | RULEX/EP_post-rules-rulecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: PUT /rules/{id} | RULEX/EP_put-rules-id-rulecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: DELETE /rules/{id} | RULEX/EP_delete-rules-id-rulecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: PATCH /rules/{id}/toggle | RULEX/EP_patch-rules-id-toggle-rulecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /rules/enabled/{enabled} | RULEX/EP_get-rules-enabled-enabled-rulecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /rules/{id}/history | RULEX/EP_get-rules-id-history-rulecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | API Controller: RuleExportImportController | RULEX/CTRL_ruleexportimportcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /v1/rules/export-import/export | RULEX/EP_get-v1-rules-export-import-export-ruleexportimportcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /v1/rules/export-import/export/selective | RULEX/EP_post-v1-rules-export-import-export-selective-ruleexportimportcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /v1/rules/export-import/export/complex | RULEX/EP_get-v1-rules-export-import-export-complex-ruleexportimportcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /v1/rules/export-import/import | RULEX/EP_post-v1-rules-export-import-import-ruleexportimportcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /v1/rules/export-import/import/file | RULEX/EP_post-v1-rules-export-import-import-file-ruleexportimportcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /v1/rules/export-import/validate | RULEX/EP_post-v1-rules-export-import-validate-ruleexportimportcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /v1/rules/export-import/template/simple | RULEX/EP_get-v1-rules-export-import-template-simple-ruleexportimportcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /v1/rules/export-import/template/complex | RULEX/EP_get-v1-rules-export-import-template-complex-ruleexportimportcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | API Controller: RuleMetricsController | RULEX/CTRL_rulemetricscontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /rules/metrics/dashboard | RULEX/EP_get-rules-metrics-dashboard-rulemetricscontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /rules/metrics/{ruleId} | RULEX/EP_get-rules-metrics-ruleid-rulemetricscontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /rules/metrics/all | RULEX/EP_get-rules-metrics-all-rulemetricscontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /rules/metrics/{ruleId}/false-positive | RULEX/EP_post-rules-metrics-ruleid-false-positive-rulemetricscontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /rules/metrics/{ruleId}/true-positive | RULEX/EP_post-rules-metrics-ruleid-true-positive-rulemetricscontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | API Controller: RuleSimulationController | RULEX/CTRL_rulesimulationcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /rules/simulation/test | RULEX/EP_post-rules-simulation-test-rulesimulationcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /rules/simulation/backtest/{ruleId} | RULEX/EP_post-rules-simulation-backtest-ruleid-rulesimulationcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /rules/simulation/compare | RULEX/EP_post-rules-simulation-compare-rulesimulationcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /rules/simulation/batch | RULEX/EP_post-rules-simulation-batch-rulesimulationcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | API Controller: TransactionController | RULEX/CTRL_transactioncontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /transactions/analyze | RULEX/EP_post-transactions-analyze-transactioncontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /transactions | RULEX/EP_get-transactions-transactioncontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /transactions/export | RULEX/EP_get-transactions-export-transactioncontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /transactions/export/csv | RULEX/EP_get-transactions-export-csv-transactioncontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /transactions/{id} | RULEX/EP_get-transactions-id-transactioncontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /transactions/external/{externalId} | RULEX/EP_get-transactions-external-externalid-transactioncontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /transactions/analyze-advanced | RULEX/EP_post-transactions-analyze-advanced-transactioncontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | API Controller: HomologRuleController | RULEX/CTRL_homologrulecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /homolog/rules | RULEX/EP_post-homolog-rules-homologrulecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /homolog/rules/{ruleId}/latest | RULEX/EP_get-homolog-rules-ruleid-latest-homologrulecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /homolog/rules/versions/{ruleVersionId}/publish | RULEX/EP_post-homolog-rules-versions-ruleversionid-publish-homologrulecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /homolog/rules/{ruleId}/rollback/{version} | RULEX/EP_post-homolog-rules-ruleid-rollback-version-homologrulecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | API Controller: HomologRuleSetController | RULEX/CTRL_homologrulesetcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /homolog/rulesets | RULEX/EP_post-homolog-rulesets-homologrulesetcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /homolog/rulesets/versions/{ruleSetVersionId}/publish | RULEX/EP_post-homolog-rulesets-versions-rulesetversionid-publish-homologrulesetcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /homolog/rulesets/activate | RULEX/EP_post-homolog-rulesets-activate-homologrulesetcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | API Controller: HomologSimulationController | RULEX/CTRL_homologsimulationcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /homolog/simulations/run | RULEX/EP_post-homolog-simulations-run-homologsimulationcontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | API Controller: ComplexRuleController | RULEX/CTRL_complexrulecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /v1/complex-rules/{ruleVersionId}/conditions | RULEX/EP_post-v1-complex-rules-ruleversionid-conditions-complexrulecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /v1/complex-rules/{ruleVersionId}/conditions | RULEX/EP_get-v1-complex-rules-ruleversionid-conditions-complexrulecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /v1/complex-rules/{ruleVersionId}/expressions | RULEX/EP_post-v1-complex-rules-ruleversionid-expressions-complexrulecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /v1/complex-rules/{ruleVersionId}/expressions | RULEX/EP_get-v1-complex-rules-ruleversionid-expressions-complexrulecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /v1/complex-rules/{ruleVersionId}/variables | RULEX/EP_post-v1-complex-rules-ruleversionid-variables-complexrulecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /v1/complex-rules/{ruleVersionId}/variables | RULEX/EP_get-v1-complex-rules-ruleversionid-variables-complexrulecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /v1/complex-rules/{ruleVersionId}/actions | RULEX/EP_post-v1-complex-rules-ruleversionid-actions-complexrulecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /v1/complex-rules/{ruleVersionId}/actions | RULEX/EP_get-v1-complex-rules-ruleversionid-actions-complexrulecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /v1/complex-rules/{ruleVersionId}/fields | RULEX/EP_get-v1-complex-rules-ruleversionid-fields-complexrulecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /v1/complex-rules/{ruleVersionId}/validate-depth | RULEX/EP_get-v1-complex-rules-ruleversionid-validate-depth-complexrulecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /v1/complex-rules/templates | RULEX/EP_get-v1-complex-rules-templates-complexrulecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /v1/complex-rules/templates/system | RULEX/EP_get-v1-complex-rules-templates-system-complexrulecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /v1/complex-rules/templates/category/{category} | RULEX/EP_get-v1-complex-rules-templates-category-category-complexrulecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: GET /v1/complex-rules/templates/{name} | RULEX/EP_get-v1-complex-rules-templates-name-complexrulecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: POST /v1/complex-rules/{ruleVersionId}/evaluate | RULEX/EP_post-v1-complex-rules-ruleversionid-evaluate-complexrulecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | Endpoint: DELETE /v1/complex-rules/{ruleVersionId} | RULEX/EP_delete-v1-complex-rules-ruleversionid-complexrulecontroller | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: FieldDictionaryService | RULEX/SVC_fielddictionaryservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: RuleExecutionLogService | RULEX/SVC_ruleexecutionlogservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: ABTestingService | RULEX/SVC_abtestingservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: AccessLogService | RULEX/SVC_accesslogservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: AdvancedRuleEngineService | RULEX/SVC_advancedruleengineservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: AuditQueryService | RULEX/SVC_auditqueryservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: AuditService | RULEX/SVC_auditservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: BloomFilterService | RULEX/SVC_bloomfilterservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: DatabaseRuleExecutorService | RULEX/SVC_databaseruleexecutorservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: DeviceFingerprintService | RULEX/SVC_devicefingerprintservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: EnrichmentService | RULEX/SVC_enrichmentservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: FuzzyLogicService | RULEX/SVC_fuzzylogicservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: GeoService | RULEX/SVC_geoservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: ImpossibleTravelService | RULEX/SVC_impossibletravelservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: MetricsService | RULEX/SVC_metricsservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: Neo4jGraphService | RULEX/SVC_neo4jgraphservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: OperatorDataService | RULEX/SVC_operatordataservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: RedisVelocityCacheService | RULEX/SVC_redisvelocitycacheservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: RedisVelocityService | RULEX/SVC_redisvelocityservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: RuleApprovalService | RULEX/SVC_ruleapprovalservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: RuleConfigurationService | RULEX/SVC_ruleconfigurationservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: RuleEngineUseCase | RULEX/SVC_ruleengineusecase | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: RuleExportImportService | RULEX/SVC_ruleexportimportservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: RuleMetricsService | RULEX/SVC_rulemetricsservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: RuleOrderingService | RULEX/SVC_ruleorderingservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: RuleSimulationService | RULEX/SVC_rulesimulationservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: SecurityContextService | RULEX/SVC_securitycontextservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: ShadowModeService | RULEX/SVC_shadowmodeservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: StatisticalAnalysisService | RULEX/SVC_statisticalanalysisservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: StringSimilarityService | RULEX/SVC_stringsimilarityservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: TransactionQueryService | RULEX/SVC_transactionqueryservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: TransactionRawStoreService | RULEX/SVC_transactionrawstoreservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: VelocityService | RULEX/SVC_velocityservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: VelocityServiceFacade | RULEX/SVC_velocityservicefacade | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: TransactionEnrichmentFacade | RULEX/SVC_transactionenrichmentfacade | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: ComplexRuleCrudService | RULEX/SVC_complexrulecrudservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: ComplexRuleExecutionService | RULEX/SVC_complexruleexecutionservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: ComplexRuleService | RULEX/SVC_complexruleservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: RuleValidationService | RULEX/SVC_rulevalidationservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: HomologRuleApplicationService | RULEX/SVC_homologruleapplicationservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service: HomologRuleSetApplicationService | RULEX/SVC_homologrulesetapplicationservice | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: FieldDictionaryEntity | RULEX/ENT_fielddictionaryentity | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: RuleExecutionLogEntity | RULEX/ENT_ruleexecutionlogentity | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: AccessLog | RULEX/ENT_accesslog | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: AuditLog | RULEX/ENT_auditlog | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: AuthenticationFailure | RULEX/ENT_authenticationfailure | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: BinLookup | RULEX/ENT_binlookup | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: BloomFilterMetadata | RULEX/ENT_bloomfiltermetadata | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: CustomerAccountInfo | RULEX/ENT_customeraccountinfo | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: CustomerBeneficiaryHistory | RULEX/ENT_customerbeneficiaryhistory | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: CustomerChargebackHistory | RULEX/ENT_customerchargebackhistory | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: CustomerIncomingTransfer | RULEX/ENT_customerincomingtransfer | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: CustomerLastTransaction | RULEX/ENT_customerlasttransaction | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: DeviceFingerprint | RULEX/ENT_devicefingerprint | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: DevicePanAssociation | RULEX/ENT_devicepanassociation | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: GeoPolygon | RULEX/ENT_geopolygon | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: GeoReference | RULEX/ENT_georeference | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: Holiday | RULEX/ENT_holiday | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: MccCategory | RULEX/ENT_mcccategory | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: MerchantChargeback | RULEX/ENT_merchantchargeback | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: PanLocationHistory | RULEX/ENT_panlocationhistory | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: RefdataVersion | RULEX/ENT_refdataversion | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: RuleAbTest | RULEX/ENT_ruleabtest | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: RuleAbTestAssignment | RULEX/ENT_ruleabtestassignment | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: RuleApproval | RULEX/ENT_ruleapproval | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: RuleConfiguration | RULEX/ENT_ruleconfiguration | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: RuleConfigurationHistory | RULEX/ENT_ruleconfigurationhistory | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: RuleList | RULEX/ENT_rulelist | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: RuleListEntry | RULEX/ENT_rulelistentry | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: RuleMetrics | RULEX/ENT_rulemetrics | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: ShadowEvaluationLog | RULEX/ENT_shadowevaluationlog | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: Transaction | RULEX/ENT_transaction | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: TransactionDecision | RULEX/ENT_transactiondecision | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: TransactionRawStore | RULEX/ENT_transactionrawstore | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: VelocityCounter | RULEX/ENT_velocitycounter | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: VelocityMetrics | RULEX/ENT_velocitymetrics | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: VelocityTransactionLog | RULEX/ENT_velocitytransactionlog | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: VoipPhoneRange | RULEX/ENT_voipphonerange | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: ActiveRuleSetEntity | RULEX/ENT_activerulesetentity | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: AuditEntryEntity | RULEX/ENT_auditentryentity | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: DecisionLogEntity | RULEX/ENT_decisionlogentity | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: RoleEntity | RULEX/ENT_roleentity | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: RuleEntity | RULEX/ENT_ruleentity | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: RuleSetEntity | RULEX/ENT_rulesetentity | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: RuleSetVersionEntity | RULEX/ENT_rulesetversionentity | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: RuleSetVersionItemEntity | RULEX/ENT_rulesetversionitementity | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: RuleVersionEntity | RULEX/ENT_ruleversionentity | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: SimulationRunEntity | RULEX/ENT_simulationrunentity | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: UserEntity | RULEX/ENT_userentity | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: UserRoleEntity | RULEX/ENT_userroleentity | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: ComplexRule | RULEX/ENT_complexrule | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: RuleAction | RULEX/ENT_ruleaction | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: RuleCondition | RULEX/ENT_rulecondition | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: RuleConditionGroup | RULEX/ENT_ruleconditiongroup | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: RuleContextVariable | RULEX/ENT_rulecontextvariable | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: RuleExecutionDetail | RULEX/ENT_ruleexecutiondetail | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: RuleExpression | RULEX/ENT_ruleexpression | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | ER | dados_postgres | Entity: RuleTemplate | RULEX/ENT_ruletemplate | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | TREE | frontend | Frontend: Pages inventory | RULEX/FE_pages | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | C4 | infra | Infra: docker-compose services | RULEX/INFRA_docker_compose | inline:mermaid |
| solution | SIM | SIM | mermaid | OK | UML | api | API: OpenAPI specs | RULEX/API_openapi_specs | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | FLOWCHART | processos | Fluxograma | FLOWCHART/fluxograma | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | FLOWCHART | processos | Fluxograma ANSI/ISO | FLOWCHART/fluxograma-ansi-iso | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | FLOWCHART | processos | Fluxograma Funcional | FLOWCHART/fluxograma-funcional | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | FLOWCHART | processos | Diagrama com Raias | FLOWCHART/diagrama-com-raias | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | PROCESS | processos | Diagrama de Fluxo de Trabalho | PROCESS/diagrama-de-fluxo-de-trabalho | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | PROCESS | processos | Diagrama de Fluxo de Processo | PROCESS/diagrama-de-fluxo-de-processo | inline:mermaid |
| template | NÃO | SIM | bpmn | OK | BPMN | processos | BPMN - Processo | BPMN/bpmn-processo | inline:bpmn |
| template | NÃO | SIM | bpmn | OK | BPMN | processos | BPMN - Colaboração | BPMN/bpmn-colaboracao | inline:bpmn |
| template | NÃO | SIM | bpmn | OK | BPMN | processos | BPMN - Coreografia | BPMN/bpmn-coreografia | inline:bpmn |
| template | NÃO | SIM | bpmn | OK | BPMN | processos | BPMN - Conversação | BPMN/bpmn-conversacao | inline:bpmn |
| template | NÃO | SIM | bpmn | OK | BPMN | processos | BPMN - Subprocesso de Evento | BPMN/bpmn-subprocesso-de-evento | inline:bpmn |
| template | NÃO | SIM | fallback | OK | EPC | processos | EPC - Cadeia de Processos Orientada a Eventos | EPC/epc-cadeia-de-processos-orientada-a-eventos | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | PROCESS | processos | Diagrama SIPOC | PROCESS/diagrama-sipoc | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | PROCESS | processos | Mapa de Fluxo de Valor | PROCESS/mapa-de-fluxo-de-valor | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | PROCESS | processos | Service Blueprint | PROCESS/service-blueprint | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | PROCESS | processos | Mapa de Jornada do Cliente | PROCESS/mapa-de-jornada-do-cliente | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | PROCESS | processos | Mapa de Histórias | PROCESS/mapa-de-historias | inline:mermaid |
| template | NÃO | SIM | matrix | OK | MATRIX | processos | Matriz RACI | MATRIX/matriz-raci | model:json |
| template | NÃO | SIM | matrix | OK | MATRIX | processos | Matriz RASCI | MATRIX/matriz-rasci | model:json |
| template | NÃO | SIM | matrix | OK | MATRIX | processos | Matriz de Responsabilidades | MATRIX/matriz-de-responsabilidades | model:json |
| template | NÃO | SIM | matrix | OK | MATRIX | processos | Matriz de Decisão | MATRIX/matriz-de-decisao | model:json |
| template | NÃO | SIM | mermaid | OK | TREE | processos | Árvore de Decisão | TREE/arvore-de-decisao | inline:mermaid |
| template | NÃO | SIM | matrix | OK | MATRIX | processos | Tabela de Decisão | MATRIX/tabela-de-decisao | model:json |
| template | NÃO | SIM | dmn | OK | DMN | processos | DMN - DRD | DMN/dmn-drd | inline:dmn |
| template | NÃO | SIM | dmn | OK | DMN | processos | DMN - Tabela de Decisão | DMN/dmn-tabela-de-decisao | inline:dmn |
| template | NÃO | SIM | dmn | OK | DMN | processos | DMN - Expressão FEEL | DMN/dmn-expressao-feel | inline:dmn |
| template | NÃO | SIM | mermaid | OK | PROCESS | processos | Diagrama de Regras de Negócio | PROCESS/diagrama-de-regras-de-negocio | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | PROCESS | processos | Event Storming - Big Picture | PROCESS/event-storming-big-picture | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | PROCESS | processos | Event Storming - Process Level | PROCESS/event-storming-process-level | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | PROCESS | processos | Event Storming - Design Level | PROCESS/event-storming-design-level | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | PROCESS | processos | Timeline de Processo | PROCESS/timeline-de-processo | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | PROCESS | processos | Diagrama de Exceções | PROCESS/diagrama-de-excecoes | inline:mermaid |
| template | NÃO | SIM | matrix | OK | MATRIX | processos | Matriz GO/NO-GO | MATRIX/matriz-go-no-go | model:json |
| template | NÃO | SIM | mermaid | OK | UML | uml_estrutural | Diagrama de Classes | UML/diagrama-de-classes | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | UML | uml_estrutural | Diagrama de Objetos | UML/diagrama-de-objetos | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | UML | uml_estrutural | Diagrama de Pacotes | UML/diagrama-de-pacotes | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | UML | uml_estrutural | Diagrama de Componentes | UML/diagrama-de-componentes | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | UML | uml_estrutural | Diagrama de Estrutura Composta | UML/diagrama-de-estrutura-composta | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | UML | uml_estrutural | Diagrama de Implantação | UML/diagrama-de-implantacao | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | UML | uml_estrutural | Diagrama de Perfil | UML/diagrama-de-perfil | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | UML | uml_estrutural | Diagrama de Casos de Uso | UML/diagrama-de-casos-de-uso | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | UML | uml_estrutural | Diagrama de Atividade | UML/diagrama-de-atividade | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | UML | uml_estrutural | Diagrama de Máquina de Estados | UML/diagrama-de-maquina-de-estados | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | UML | uml_estrutural | Diagrama de Sequência | UML/diagrama-de-sequencia | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | UML | uml_estrutural | Diagrama de Comunicação | UML/diagrama-de-comunicacao | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | UML | uml_estrutural | Diagrama de Tempo | UML/diagrama-de-tempo | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | UML | uml_estrutural | Diagrama de Visão Geral de Interação | UML/diagrama-de-visao-geral-de-interacao | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | C4 | arquitetura | C4 - Contexto | C4/c4-contexto | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | C4 | arquitetura | C4 - Contêineres | C4/c4-conteineres | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | C4 | arquitetura | C4 - Componentes | C4/c4-componentes | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | C4 | arquitetura | C4 - Código | C4/c4-codigo | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | C4 | arquitetura | C4 - Dinâmico | C4/c4-dinamico | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | C4 | arquitetura | C4 - Deployment | C4/c4-deployment | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Arquitetura em Camadas | ARCHIMATE/arquitetura-em-camadas | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Arquitetura Hexagonal | ARCHIMATE/arquitetura-hexagonal | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Clean Architecture | ARCHIMATE/clean-architecture | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Onion Architecture | ARCHIMATE/onion-architecture | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Arquitetura de Microserviços | ARCHIMATE/arquitetura-de-microservicos | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Monolito Modular | ARCHIMATE/monolito-modular | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Topologia de Serviços | ARCHIMATE/topologia-de-servicos | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Landscape de Sistemas | ARCHIMATE/landscape-de-sistemas | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Landscape de Integração | ARCHIMATE/landscape-de-integracao | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | API Gateway Architecture | ARCHIMATE/api-gateway-architecture | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | BFF (Backend for Frontend) | ARCHIMATE/bff-backend-for-frontend | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Service Mesh | ARCHIMATE/service-mesh | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Arquitetura Event-Driven | ARCHIMATE/arquitetura-event-driven | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Topologia Pub/Sub | ARCHIMATE/topologia-pub-sub | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | CQRS | ARCHIMATE/cqrs | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Event Sourcing | ARCHIMATE/event-sourcing | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Saga - Orquestração | ARCHIMATE/saga-orquestracao | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Saga - Coreografia | ARCHIMATE/saga-coreografia | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Outbox Pattern | ARCHIMATE/outbox-pattern | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Inbox Pattern | ARCHIMATE/inbox-pattern | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Strangler Fig | ARCHIMATE/strangler-fig | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Anti-Corruption Layer (ACL) | ARCHIMATE/anti-corruption-layer-acl | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Rate Limiting | ARCHIMATE/rate-limiting | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Idempotência | ARCHIMATE/idempotencia | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Retry/Timeout/Circuit Breaker | ARCHIMATE/retry-timeout-circuit-breaker | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Bulkhead Pattern | ARCHIMATE/bulkhead-pattern | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Failover | ARCHIMATE/failover | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Fluxo de Tráfego | ARCHIMATE/fluxo-de-trafego | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Load Balancing | ARCHIMATE/load-balancing | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Blue/Green Deployment | ARCHIMATE/blue-green-deployment | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Canary Deployment | ARCHIMATE/canary-deployment | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Rolling Update | ARCHIMATE/rolling-update | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Feature Flags | ARCHIMATE/feature-flags | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Diagrama de Dependências | ARCHIMATE/diagrama-de-dependencias | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Call Graph | ARCHIMATE/call-graph | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Control Flow Graph (CFG) | ARCHIMATE/control-flow-graph-cfg | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Data Dependency Graph (DDG) | ARCHIMATE/data-dependency-graph-ddg | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | ADR Map | ARCHIMATE/adr-map | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | arquitetura | Diagrama de Contexto | ARCHIMATE/diagrama-de-contexto | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ER | dados_postgres | DER - Entidades e Relacionamentos | ER/der-entidades-e-relacionamentos | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ER | dados_postgres | DER - Crow's Foot | ER/der-crow-s-foot | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ER | dados_postgres | DER - Pé de Galinha (Barker) | ER/der-pe-de-galinha-barker | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ER | dados_postgres | DER - Chen | ER/der-chen | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ER | dados_postgres | Modelo Lógico Relacional | ER/modelo-logico-relacional | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ER | dados_postgres | Modelo Físico (DDL) | ER/modelo-fisico-ddl | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ER | dados_postgres | Schema de Banco de Dados | ER/schema-de-banco-de-dados | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ER | dados_postgres | Diagrama de Índices | ER/diagrama-de-indices | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ER | dados_postgres | Particionamento | ER/particionamento | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ER | dados_postgres | Replicação Master-Replica | ER/replicacao-master-replica | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ER | dados_postgres | Replicação Multi-Master | ER/replicacao-multi-master | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ER | dados_postgres | Sharding | ER/sharding | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ER | dados_postgres | Modelo Star Schema | ER/modelo-star-schema | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ER | dados_postgres | Modelo Snowflake Schema | ER/modelo-snowflake-schema | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ER | dados_postgres | Data Lineage | ER/data-lineage | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ER | dados_postgres | Data Flow - Dados | ER/data-flow-dados | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ER | dados_postgres | Diagrama de Migração | ER/diagrama-de-migracao | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ER | dados_postgres | Backup & Recovery | ER/backup-recovery | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ER | dados_postgres | Diagrama de Views | ER/diagrama-de-views | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ER | dados_postgres | Diagrama de Triggers/Procedures | ER/diagrama-de-triggers-procedures | inline:mermaid |
| template | NÃO | SIM | graph | OK | GRAPH | dados_redis | Estratégia de Cache | GRAPH/estrategia-de-cache | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | dados_redis | Cache Invalidation | GRAPH/cache-invalidation | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | dados_redis | Cache Warming | GRAPH/cache-warming | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | dados_redis | Replicação Redis | GRAPH/replicacao-redis | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | dados_redis | Redis Sentinel | GRAPH/redis-sentinel | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | dados_redis | Redis Cluster | GRAPH/redis-cluster | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | dados_redis | Pub/Sub - Redis | GRAPH/pub-sub-redis | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | dados_redis | Redis Streams | GRAPH/redis-streams | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | dados_redis | Rate Limiting - Redis | GRAPH/rate-limiting-redis | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | dados_redis | Distributed Lock - Redis | GRAPH/distributed-lock-redis | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | dados_redis | Session Store - Redis | GRAPH/session-store-redis | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | dados_redis | Leaderboard - Redis | GRAPH/leaderboard-redis | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | dados_redis | Diagrama de Estruturas de Dados | GRAPH/diagrama-de-estruturas-de-dados | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | dados_redis | Redis Persistence | GRAPH/redis-persistence | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | dados_redis | Redis Multi-Tenancy | GRAPH/redis-multi-tenancy | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | dados_neo4j | Property Graph Model | GRAPH/property-graph-model | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | dados_neo4j | Cypher Query Flow | GRAPH/cypher-query-flow | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | dados_neo4j | Grafo de Conhecimento | GRAPH/grafo-de-conhecimento | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | dados_neo4j | Diagrama de Traversal | GRAPH/diagrama-de-traversal | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | dados_neo4j | Ontologia OWL | GRAPH/ontologia-owl | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | dados_neo4j | RDF Triple Store | GRAPH/rdf-triple-store | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | dados_neo4j | SHACL Shapes | GRAPH/shacl-shapes | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | dados_neo4j | Grafo Social | GRAPH/grafo-social | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | dados_neo4j | Grafo de Dependências | GRAPH/grafo-de-dependencias | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | dados_neo4j | Diagrama de Replicação de Grafo | GRAPH/diagrama-de-replicacao-de-grafo | model:json |
| template | NÃO | SIM | mermaid | OK | UML | frontend | Árvore de Componentes React | UML/arvore-de-componentes-react | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | UML | frontend | Props & Data Flow | UML/props-data-flow | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | UML | frontend | State Management | UML/state-management | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | UML | frontend | Diagrama de Rotas | UML/diagrama-de-rotas | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | PROCESS | frontend | Atomic Design | PROCESS/atomic-design | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | UML | frontend | Diagrama de Hooks | UML/diagrama-de-hooks | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | UML | frontend | Context API | UML/context-api | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | PROCESS | frontend | Wireframe | PROCESS/wireframe | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | PROCESS | frontend | Mockup | PROCESS/mockup | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | PROCESS | frontend | Protótipo Interativo | PROCESS/prototipo-interativo | inline:mermaid |
| template | NÃO | SIM | graph | OK | GRAPH | frontend | User Flow | GRAPH/user-flow | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | frontend | Mapa de Acessibilidade | GRAPH/mapa-de-acessibilidade | model:json |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | infra | Pipeline CI/CD | ARCHIMATE/pipeline-ci-cd | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | infra | GitFlow | ARCHIMATE/gitflow | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | infra | Trunk-Based Development | ARCHIMATE/trunk-based-development | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | infra | Diagrama de Infraestrutura | ARCHIMATE/diagrama-de-infraestrutura | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | infra | Kubernetes Architecture | ARCHIMATE/kubernetes-architecture | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | infra | Helm Chart Structure | ARCHIMATE/helm-chart-structure | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | infra | Service Mesh - Istio | ARCHIMATE/service-mesh-istio | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | infra | Observability Stack | ARCHIMATE/observability-stack | inline:mermaid |
| template | NÃO | SIM | graph | OK | GRAPH | infra | Distributed Tracing | GRAPH/distributed-tracing | model:json |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | infra | Logging Architecture | ARCHIMATE/logging-architecture | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | infra | Metrics & Alerting | ARCHIMATE/metrics-alerting | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | infra | SLO/SLI/SLA | ARCHIMATE/slo-sli-sla | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | PROCESS | infra | Incident Timeline | PROCESS/incident-timeline | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | infra | Disaster Recovery Plan | ARCHIMATE/disaster-recovery-plan | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | ARCHIMATE | infra | Network Topology | ARCHIMATE/network-topology | inline:mermaid |
| template | NÃO | SIM | graph | OK | GRAPH | seguranca | Threat Model | GRAPH/threat-model | model:json |
| template | NÃO | SIM | mermaid | OK | TREE | seguranca | STRIDE per Element | TREE/stride-per-element | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | TREE | seguranca | Attack Tree | TREE/attack-tree | inline:mermaid |
| template | NÃO | SIM | graph | OK | GRAPH | seguranca | Security Architecture | GRAPH/security-architecture | model:json |
| template | NÃO | SIM | mermaid | OK | PROCESS | seguranca | Fluxo de Autenticação | PROCESS/fluxo-de-autenticacao | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | PROCESS | seguranca | OAuth 2.0 Flow | PROCESS/oauth-2-0-flow | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | PROCESS | seguranca | OpenID Connect | PROCESS/openid-connect | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | PROCESS | seguranca | SAML Flow | PROCESS/saml-flow | inline:mermaid |
| template | NÃO | SIM | graph | OK | GRAPH | seguranca | RBAC - Controle de Acesso Baseado em Papéis | GRAPH/rbac-controle-de-acesso-baseado-em-papeis | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | seguranca | ABAC - Controle de Acesso Baseado em Atributos | GRAPH/abac-controle-de-acesso-baseado-em-atributos | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | seguranca | Policy as Code | GRAPH/policy-as-code | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | seguranca | Zero Trust Architecture | GRAPH/zero-trust-architecture | model:json |
| template | NÃO | SIM | dfd | OK | DFD | seguranca | PII Data Flow | DFD/pii-data-flow | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | seguranca | Encryption at Rest/Transit | GRAPH/encryption-at-rest-transit | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | seguranca | Key Management | GRAPH/key-management | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | seguranca | Secrets Management | GRAPH/secrets-management | model:json |
| template | NÃO | SIM | matrix | OK | MATRIX | seguranca | Matriz de Risco | MATRIX/matriz-de-risco | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | seguranca | Security Incident Response | GRAPH/security-incident-response | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | seguranca | Compliance Mapping | GRAPH/compliance-mapping | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | seguranca | Supply Chain Security | GRAPH/supply-chain-security | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | qualidade | Pirâmide de Testes | GRAPH/piramide-de-testes | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | qualidade | Test Coverage Map | GRAPH/test-coverage-map | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | qualidade | Chaos Engineering | GRAPH/chaos-engineering | model:json |
| template | NÃO | SIM | mermaid | OK | TREE | qualidade | Fault Tree Analysis (FTA) | TREE/fault-tree-analysis-fta | inline:mermaid |
| template | NÃO | SIM | matrix | OK | MATRIX | qualidade | FMEA | MATRIX/fmea | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | qualidade | Mutation Testing | GRAPH/mutation-testing | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | qualidade | Load Testing Strategy | GRAPH/load-testing-strategy | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | qualidade | Performance Baseline | GRAPH/performance-baseline | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | qualidade | Capacity Planning | GRAPH/capacity-planning | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | qualidade | Diagrama de Dependabilidade | GRAPH/diagrama-de-dependabilidade | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | qualidade | Reliability Block Diagram (RBD) | GRAPH/reliability-block-diagram-rbd | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | qualidade | Error Budget | GRAPH/error-budget | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | qualidade | Test Data Management | GRAPH/test-data-management | model:json |
| template | NÃO | SIM | matrix | OK | MATRIX | qualidade | Traceability Matrix | MATRIX/traceability-matrix | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | qualidade | Regression Test Suite | GRAPH/regression-test-suite | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | cs_classicos | Autômato Finito (DFA/NFA) | GRAPH/automato-finito-dfa-nfa | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | cs_classicos | Autômato de Pilha (PDA) | GRAPH/automato-de-pilha-pda | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | cs_classicos | Máquina de Turing | GRAPH/maquina-de-turing | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | cs_classicos | Rede de Petri | GRAPH/rede-de-petri | model:json |
| template | NÃO | SIM | dfd | OK | DFD | cs_classicos | DFD - Yourdon/DeMarco | DFD/dfd-yourdon-demarco | model:json |
| template | NÃO | SIM | dfd | OK | DFD | cs_classicos | DFD - Gane–Sarson | DFD/dfd-gane-sarson | model:json |
| template | NÃO | SIM | mermaid | OK | PROCESS | cs_classicos | IDEF0 | PROCESS/idef0 | inline:mermaid |
| template | NÃO | SIM | mermaid | OK | PROCESS | cs_classicos | IDEF3 | PROCESS/idef3 | inline:mermaid |
| template | NÃO | SIM | graph | OK | GRAPH | cs_classicos | Parse Tree / AST | GRAPH/parse-tree-ast | model:json |
| template | NÃO | SIM | graph | OK | GRAPH | cs_classicos | Grafo de Precedência | GRAPH/grafo-de-precedencia | model:json |
