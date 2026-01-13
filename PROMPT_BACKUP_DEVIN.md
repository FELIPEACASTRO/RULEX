# 🔒 PROMPT DE BACKUP COMPLETO - RULEX FRAUD DETECTION ENGINE

> **VERSÃO:** 1.0.0  
> **DATA:** 2026-01-12  
> **FINALIDADE:** Reconstrução completa e fidedigna do sistema RULEX

---

## 📋 INSTRUÇÕES PARA O DEVIN

**ATENÇÃO: LEIA ESTE PROMPT INTEIRO ANTES DE INICIAR QUALQUER AÇÃO.**

Você é um agente de engenharia de software especializado. Sua missão é **ANALISAR E RECONSTRUIR** o sistema RULEX - Motor de Regras Duras para Detecção de Fraudes em Transações de Cartão de Crédito.

### REGRAS OBRIGATÓRIAS:

1. **ANÁLISE EXTREMAMENTE RIGOROSA**: Examine cada arquivo, cada linha, cada palavra
2. **ZERO GAPS**: Nenhuma funcionalidade pode ser omitida ou simplificada
3. **FIDELIDADE ABSOLUTA**: Mantenha EXATAMENTE os mesmos padrões, nomes, estruturas
4. **DOCUMENTAÇÃO COMPLETA**: Registre TUDO que encontrar
5. **VALIDAÇÃO CONTÍNUA**: Teste cada componente após implementação

---

## 🏗️ ARQUITETURA DO SISTEMA

### 1. VISÃO GERAL

**RULEX** é um **Motor de Regras Determinísticas** (Hard Rules) para prevenção de fraudes. **NÃO É ML/AI** - todas as decisões são baseadas em regras explícitas, configuráveis e 100% auditáveis.

#### Stack Tecnológico EXATO:

| Camada | Tecnologia | Versão EXATA |
|--------|------------|--------------|
| **Backend** | Java + Spring Boot | Java 21 LTS / Spring Boot 3.5.9 |
| **Frontend** | React + TypeScript + Vite | React 19.2.1 / Vite 7.1.7 / TypeScript 5.9.3 |
| **Banco de Dados** | PostgreSQL | 16.x (Alpine) |
| **Cache** | Redis | 7.x (Alpine) |
| **Migrations** | Flyway | 11.20.0 |
| **Testes Backend** | JUnit 5 + Testcontainers + ArchUnit | - |
| **Testes Frontend** | Vitest + Testing Library | - |
| **Testes E2E** | Playwright | 1.57.0 |
| **Containerização** | Docker Compose | - |

---

## 📁 ESTRUTURA DE DIRETÓRIOS COMPLETA

```
RULEX/
├── .env.example                    # Variáveis de ambiente
├── .github/                        # GitHub Actions/CI
├── .gitignore
├── .gitleaks.toml                  # Segurança de secrets
├── .gitleaksignore
├── .markdownlint.json
├── .prettierrc
├── .prettierignore
├── AGENTS.md                       # Guia para agentes AI
├── README.md                       # Documentação principal
├── replit.md
├── docker-compose.yml              # Orquestração Docker
├── Dockerfile.web                  # Container frontend
├── package.json                    # Dependências frontend
├── pnpm-lock.yaml
├── tsconfig.json                   # Config TypeScript
├── vite.config.ts                  # Config Vite
├── vitest.config.ts                # Config Vitest
├── playwright.config.ts            # Config Playwright
├── components.json                 # shadcn/ui config
│
├── backend/                        # ══════════════════════════════════
│   ├── Dockerfile
│   ├── pom.xml                     # Maven dependencies
│   ├── README.md
│   └── src/
│       ├── main/
│       │   ├── java/com/rulex/
│       │   │   ├── RulexApplication.java          # Entry point
│       │   │   │
│       │   │   ├── api/                           # API Layer
│       │   │   │   ├── ApiErrorResponse.java
│       │   │   │   ├── CachedBodyHttpServletRequest.java
│       │   │   │   ├── CorrelationId.java
│       │   │   │   ├── CorrelationIdFilter.java
│       │   │   │   ├── NotFoundException.java
│       │   │   │   └── RawPayloadCaptureFilter.java
│       │   │   │
│       │   │   ├── config/                        # Spring Configurations
│       │   │   │   ├── AccessLogFilter.java
│       │   │   │   ├── AsyncConfig.java
│       │   │   │   ├── CacheConfig.java
│       │   │   │   ├── ClockConfig.java
│       │   │   │   ├── CorsConfig.java
│       │   │   │   ├── OpenTelemetryConfig.java
│       │   │   │   ├── RateLimitingFilter.java
│       │   │   │   ├── RulexSecurityProperties.java
│       │   │   │   ├── SecureCookieConfig.java
│       │   │   │   ├── SecurityConfig.java
│       │   │   │   ├── SecurityHeadersConfig.java
│       │   │   │   └── VirtualThreadsConfig.java
│       │   │   │
│       │   │   ├── controller/                    # REST Controllers
│       │   │   │   ├── AuditController.java
│       │   │   │   ├── ComplexRuleCrudController.java
│       │   │   │   ├── EvaluateController.java
│       │   │   │   ├── MetricsController.java
│       │   │   │   ├── RuleApprovalController.java
│       │   │   │   ├── RuleController.java
│       │   │   │   ├── RuleExportImportController.java
│       │   │   │   ├── RuleMetricsController.java
│       │   │   │   ├── RuleSimulationController.java
│       │   │   │   ├── TransactionController.java
│       │   │   │   ├── complex/
│       │   │   │   │   └── ComplexRuleController.java
│       │   │   │   └── homolog/                   # Homologation endpoints
│       │   │   │       └── [...]
│       │   │   │
│       │   │   ├── dto/                           # Data Transfer Objects
│       │   │   │   ├── AuditLogDTO.java
│       │   │   │   ├── EvaluateRequestDTO.java
│       │   │   │   ├── EvaluateResponse.java
│       │   │   │   ├── MetricsDTO.java
│       │   │   │   ├── PopupDTO.java
│       │   │   │   ├── RuleConditionDTO.java
│       │   │   │   ├── RuleConfigurationDTO.java
│       │   │   │   ├── RuleExportDTO.java
│       │   │   │   ├── RuleHitDTO.java
│       │   │   │   ├── TransactionRequest.java
│       │   │   │   ├── TransactionResponse.java
│       │   │   │   ├── TriggeredRuleDTO.java
│       │   │   │   ├── complex/
│       │   │   │   │   ├── ComplexRuleDTO.java
│       │   │   │   │   ├── ConditionDTO.java
│       │   │   │   │   ├── ConditionGroupDTO.java
│       │   │   │   │   ├── ContextVariableDTO.java
│       │   │   │   │   ├── ExpressionDTO.java
│       │   │   │   │   └── RuleActionDTO.java
│       │   │   │   └── homolog/
│       │   │   │
│       │   │   ├── entity/                        # JPA Entities
│       │   │   │   ├── AccessLog.java
│       │   │   │   ├── AuditLog.java
│       │   │   │   ├── BinLookup.java
│       │   │   │   ├── BloomFilterMetadata.java
│       │   │   │   ├── DeviceFingerprint.java
│       │   │   │   ├── DevicePanAssociation.java
│       │   │   │   ├── GeoPolygon.java
│       │   │   │   ├── GeoReference.java
│       │   │   │   ├── MccCategory.java
│       │   │   │   ├── PanLocationHistory.java
│       │   │   │   ├── RefdataVersion.java
│       │   │   │   ├── RuleAbTest.java
│       │   │   │   ├── RuleAbTestAssignment.java
│       │   │   │   ├── RuleApproval.java
│       │   │   │   ├── RuleConfiguration.java
│       │   │   │   ├── RuleConfigurationHistory.java
│       │   │   │   ├── RuleList.java
│       │   │   │   ├── RuleListEntry.java
│       │   │   │   ├── RuleMetrics.java
│       │   │   │   ├── ShadowEvaluationLog.java
│       │   │   │   ├── Transaction.java
│       │   │   │   ├── TransactionDecision.java
│       │   │   │   ├── TransactionRawStore.java
│       │   │   │   ├── VelocityCounter.java
│       │   │   │   ├── VelocityMetrics.java
│       │   │   │   ├── VelocityTransactionLog.java
│       │   │   │   ├── complex/                   # Complex Rules Entities
│       │   │   │   │   ├── ComplexRule.java
│       │   │   │   │   ├── RuleAction.java
│       │   │   │   │   ├── RuleCondition.java
│       │   │   │   │   ├── RuleConditionGroup.java
│       │   │   │   │   ├── RuleContextVariable.java
│       │   │   │   │   ├── RuleExecutionDetail.java
│       │   │   │   │   ├── RuleExpression.java
│       │   │   │   │   └── RuleTemplate.java
│       │   │   │   └── homolog/                   # Homologation Entities
│       │   │   │       ├── ActiveRuleSetEntity.java
│       │   │   │       ├── AuditActionType.java
│       │   │   │       ├── AuditEntryEntity.java
│       │   │   │       ├── AuditResult.java
│       │   │   │       ├── DecisionLogEntity.java
│       │   │   │       ├── DecisionOutcome.java
│       │   │   │       ├── LogicOperator.java
│       │   │   │       ├── RoleEntity.java
│       │   │   │       ├── RuleEntity.java
│       │   │   │       ├── RuleSetEntity.java
│       │   │   │       ├── RuleSetVersionEntity.java
│       │   │   │       ├── RuleSetVersionItemEntity.java
│       │   │   │       ├── RuleStatus.java
│       │   │   │       ├── RuleVersionEntity.java
│       │   │   │       ├── SimulationRunEntity.java
│       │   │   │       ├── UserEntity.java
│       │   │   │       └── UserRoleEntity.java
│       │   │   │
│       │   │   ├── exception/                     # Exception Handling
│       │   │   │   └── GlobalExceptionHandler.java
│       │   │   │
│       │   │   ├── homolog/                       # Hexagonal Architecture Module
│       │   │   │   ├── adapter/
│       │   │   │   ├── application/
│       │   │   │   ├── config/
│       │   │   │   ├── port/
│       │   │   │   └── usecase/
│       │   │   │
│       │   │   ├── repository/                    # JPA Repositories
│       │   │   │   ├── AccessLogRepository.java
│       │   │   │   ├── AuditLogRepository.java
│       │   │   │   ├── BinLookupRepository.java
│       │   │   │   ├── BloomFilterMetadataRepository.java
│       │   │   │   ├── DeviceFingerprintRepository.java
│       │   │   │   ├── DevicePanAssociationRepository.java
│       │   │   │   ├── GeoPolygonRepository.java
│       │   │   │   ├── GeoReferenceRepository.java
│       │   │   │   ├── MccCategoryRepository.java
│       │   │   │   ├── PanLocationHistoryRepository.java
│       │   │   │   ├── RefdataVersionRepository.java
│       │   │   │   ├── RuleAbTestAssignmentRepository.java
│       │   │   │   ├── RuleAbTestRepository.java
│       │   │   │   ├── RuleApprovalRepository.java
│       │   │   │   ├── RuleConfigurationHistoryRepository.java
│       │   │   │   ├── RuleConfigurationRepository.java
│       │   │   │   ├── RuleListEntryRepository.java
│       │   │   │   ├── RuleListRepository.java
│       │   │   │   ├── RuleMetricsRepository.java
│       │   │   │   ├── ShadowEvaluationLogRepository.java
│       │   │   │   ├── TransactionDecisionRepository.java
│       │   │   │   ├── TransactionRawStoreRepository.java
│       │   │   │   ├── TransactionRepository.java
│       │   │   │   ├── VelocityCounterRepository.java
│       │   │   │   ├── VelocityMetricsRepository.java
│       │   │   │   ├── VelocityTransactionLogRepository.java
│       │   │   │   ├── complex/
│       │   │   │   │   ├── ComplexRuleRepository.java
│       │   │   │   │   ├── RuleActionRepository.java
│       │   │   │   │   ├── RuleConditionGroupRepository.java
│       │   │   │   │   ├── RuleConditionRepository.java
│       │   │   │   │   ├── RuleContextVariableRepository.java
│       │   │   │   │   ├── RuleExecutionDetailRepository.java
│       │   │   │   │   ├── RuleExpressionRepository.java
│       │   │   │   │   └── RuleTemplateRepository.java
│       │   │   │   └── homolog/
│       │   │   │
│       │   │   ├── resilience/                    # Circuit Breaker
│       │   │   │   └── DatabaseCircuitBreaker.java
│       │   │   │
│       │   │   ├── service/                       # Business Services
│       │   │   │   ├── ABTestingService.java
│       │   │   │   ├── AccessLogService.java
│       │   │   │   ├── AdvancedRuleEngineService.java
│       │   │   │   ├── AuditQueryService.java
│       │   │   │   ├── AuditService.java
│       │   │   │   ├── BloomFilterService.java
│       │   │   │   ├── DatabaseRuleExecutorService.java
│       │   │   │   ├── DerivedContext.java
│       │   │   │   ├── DeviceFingerprintService.java
│       │   │   │   ├── EnrichmentService.java
│       │   │   │   ├── GeoService.java
│       │   │   │   ├── ImpossibleTravelService.java
│       │   │   │   ├── MetricsService.java
│       │   │   │   ├── ParallelRuleExecutionService.java
│       │   │   │   ├── PayloadHashService.java
│       │   │   │   ├── RedisVelocityCacheService.java
│       │   │   │   ├── RedisVelocityService.java
│       │   │   │   ├── RuleApprovalService.java
│       │   │   │   ├── RuleConfigurationService.java
│       │   │   │   ├── RuleEngineService.java        # CORE ENGINE
│       │   │   │   ├── RuleExportImportService.java
│       │   │   │   ├── RuleMetricsService.java
│       │   │   │   ├── RuleOrderingService.java
│       │   │   │   ├── RuleSimulationService.java
│       │   │   │   ├── RulexMetricsService.java
│       │   │   │   ├── SecurityContextService.java
│       │   │   │   ├── ShadowModeService.java
│       │   │   │   ├── TransactionQueryService.java
│       │   │   │   ├── TransactionRawStoreService.java
│       │   │   │   ├── VelocityService.java
│       │   │   │   ├── VelocityServiceFacade.java
│       │   │   │   ├── complex/                   # Complex Rules Services
│       │   │   │   │   ├── ComplexRuleCrudService.java
│       │   │   │   │   ├── ComplexRuleEvaluator.java   # CORE EVALUATOR
│       │   │   │   │   ├── ComplexRuleExecutionService.java
│       │   │   │   │   ├── ComplexRuleMapper.java
│       │   │   │   │   ├── ComplexRuleService.java
│       │   │   │   │   ├── ExpressionEvaluator.java
│       │   │   │   │   └── RuleValidationService.java
│       │   │   │   └── enrichment/                # Data Enrichment
│       │   │   │       ├── AnomalyEnrichment.java
│       │   │   │       ├── AuthEnrichment.java
│       │   │   │       ├── CardEnrichment.java
│       │   │   │       ├── CustomerEnrichment.java
│       │   │   │       ├── DeviceEnrichment.java
│       │   │   │       ├── GeoEnrichment.java
│       │   │   │       ├── TransactionEnrichmentFacade.java
│       │   │   │       └── VelocityEnrichment.java
│       │   │   │
│       │   │   ├── util/                          # Utilities
│       │   │   │   ├── PanHashUtil.java
│       │   │   │   ├── PanMaskingUtil.java
│       │   │   │   └── RegexValidator.java
│       │   │   │
│       │   │   └── v31/                           # V3.1 Engine (AST + Field Dictionary)
│       │   │       ├── ast/
│       │   │       │   ├── AstEvaluator.java
│       │   │       │   ├── AstValidationError.java
│       │   │       │   ├── AstValidationResult.java
│       │   │       │   └── AstValidator.java
│       │   │       ├── execlog/
│       │   │       │   ├── ExecutionEventType.java
│       │   │       │   ├── RuleExecutionLogEntity.java
│       │   │       │   ├── RuleExecutionLogRepository.java
│       │   │       │   └── RuleExecutionLogService.java
│       │   │       ├── field/
│       │   │       │   ├── FieldDictionaryController.java
│       │   │       │   ├── FieldDictionaryEntity.java
│       │   │       │   ├── FieldDictionaryRepository.java
│       │   │       │   ├── FieldDictionarySeeder.java
│       │   │       │   └── FieldDictionaryService.java
│       │   │       └── rules/
│       │   │           └── RulesV31Controller.java
│       │   │
│       │   └── resources/
│       │       ├── application.yml
│       │       ├── application-dev.yml
│       │       ├── application-prod.yml
│       │       ├── grafana/
│       │       ├── prometheus-alerts.yml
│       │       └── db/
│       │           ├── migration/                 # Flyway Migrations (V1-V30)
│       │           │   ├── V1__init.sql
│       │           │   ├── V2__core_schema.sql
│       │           │   ├── V3__extend_workflow_length.sql
│       │           │   ├── V4__raw_hash_idempotency.sql
│       │           │   ├── V5__raw_as_received.sql
│       │           │   ├── V6__v31_exec_log_field_dictionary.sql
│       │           │   ├── V7__v31_exec_log_dedup.sql
│       │           │   ├── V8__complex_rules_support.sql
│       │           │   ├── V9__audit_compliance_enhancements.sql
│       │           │   ├── V10__derived_context_improvements.sql
│       │           │   ├── V11__bin_lookup_table.sql
│       │           │   ├── V12__complex_rules_crud.sql
│       │           │   ├── V13__geo_reference_table.sql
│       │           │   ├── V14__velocity_counters.sql
│       │           │   ├── V15__add_velocity_operators.sql
│       │           │   ├── V16__fix_geo_polygon_id_type.sql
│       │           │   ├── V17__fix_geo_reference_id_type.sql
│       │           │   ├── V18__enable_condition_groups_constraint.sql
│       │           │   ├── V19__access_log_table.sql
│       │           │   ├── V20__shadow_mode_and_device_fingerprinting.sql
│       │           │   ├── V21__rule_configurations_shadow_mode.sql
│       │           │   ├── V22__fraud_detection_rules_seed.sql
│       │           │   ├── V23__web_research_fraud_rules.sql
│       │           │   ├── V24__regras_fraude_portugues_completo.sql
│       │           │   ├── V25__additional_fraud_rules_200plus.sql
│       │           │   ├── V26__fix_complex_rules_conditions.sql
│       │           │   ├── V27__migrate_hardcoded_advanced_rules.sql
│       │           │   ├── V28__add_missing_condition_operators.sql
│       │           │   ├── V29__insert_advanced_fraud_rules_catalog.sql
│       │           │   └── V30__insert_aml_ato_advanced_rules.sql
│       │           ├── migration_pending/
│       │           └── rollback/                  # Rollback Scripts
│       │               ├── R1__undo_init.sql
│       │               ├── R2__undo_core_schema.sql
│       │               ├── R3__undo_extend_workflow_length.sql
│       │               ├── R4__undo_raw_hash_idempotency.sql
│       │               ├── R5__undo_raw_as_received.sql
│       │               ├── R6__undo_v31_exec_log_field_dictionary.sql
│       │               └── R7__undo_v31_exec_log_dedup.sql
│       │
│       └── test/                                  # Backend Tests
│           ├── java/
│           └── resources/
│
├── client/                         # ══════════════════════════════════
│   ├── index.html
│   ├── public/
│   │   └── _redirects
│   └── src/
│       ├── App.tsx                                # Main Router
│       ├── main.tsx                               # Entry Point
│       ├── const.ts
│       ├── index.css                              # Global Styles
│       │
│       ├── _core/                                 # Core Module
│       │   ├── auth/
│       │   │   └── tokens.ts
│       │   └── hooks/
│       │
│       ├── components/                            # React Components
│       │   ├── AIChatBox.tsx
│       │   ├── CommandPalette.tsx
│       │   ├── DashboardLayout.tsx
│       │   ├── DashboardLayout.test.tsx
│       │   ├── DashboardLayoutSkeleton.tsx
│       │   ├── DeleteRuleDialog.tsx
│       │   ├── ErrorBoundary.tsx
│       │   ├── ErrorBoundary.test.tsx
│       │   ├── KeyboardShortcuts.tsx
│       │   ├── ManusDialog.tsx
│       │   ├── Map.tsx
│       │   ├── RuleBuilder.tsx
│       │   ├── RuleSimulator.tsx
│       │   ├── ThemeToggle.tsx
│       │   │
│       │   ├── ComplexRuleBuilder/                # Complex Rule Builder
│       │   │   ├── index.tsx
│       │   │   ├── ComplexRuleBuilder.test.tsx
│       │   │   ├── ConditionCard.tsx
│       │   │   ├── ConditionGroupCard.tsx
│       │   │   ├── RuleMetadataForm.tsx
│       │   │   ├── RuleNaturalLanguage.tsx
│       │   │   ├── RulePreview.tsx
│       │   │   ├── RuleSimulator.tsx
│       │   │   ├── TemplateSelector.tsx
│       │   │   └── types.ts
│       │   │
│       │   ├── RuleFormDialog/                    # Simple Rule Form
│       │   │   ├── index.tsx
│       │   │   ├── RuleFormDialog.tsx
│       │   │   ├── operators.test.ts
│       │   │   ├── schema.test.ts
│       │   │   ├── schema.ts
│       │   │   ├── types.ts
│       │   │   └── useRuleForm.ts
│       │   │
│       │   └── ui/                                # shadcn/ui Components (57+)
│       │       ├── accordion.tsx
│       │       ├── alert-dialog.tsx
│       │       ├── alert.tsx
│       │       ├── animated-card.tsx
│       │       ├── aspect-ratio.tsx
│       │       ├── avatar.tsx
│       │       ├── badge.tsx
│       │       ├── breadcrumb.tsx
│       │       ├── button-group.tsx
│       │       ├── button.tsx
│       │       ├── calendar.tsx
│       │       ├── card.tsx
│       │       ├── carousel.tsx
│       │       ├── chart.tsx
│       │       ├── checkbox.tsx
│       │       ├── collapsible.tsx
│       │       ├── command.tsx
│       │       ├── context-menu.tsx
│       │       ├── dialog.tsx
│       │       ├── drawer.tsx
│       │       ├── dropdown-menu.tsx
│       │       ├── empty.tsx
│       │       ├── fade-in.tsx
│       │       ├── field.tsx
│       │       ├── form.tsx
│       │       ├── hover-card.tsx
│       │       ├── input-group.tsx
│       │       ├── input-otp.tsx
│       │       ├── input.tsx
│       │       ├── item.tsx
│       │       ├── kbd.tsx
│       │       ├── label.tsx
│       │       ├── loading-spinner.tsx
│       │       ├── menubar.tsx
│       │       ├── navigation-menu.tsx
│       │       ├── pagination.tsx
│       │       ├── popover.tsx
│       │       ├── progress.tsx
│       │       ├── radio-group.tsx
│       │       ├── resizable.tsx
│       │       ├── scroll-area.tsx
│       │       ├── select.tsx
│       │       ├── separator.tsx
│       │       ├── sheet.tsx
│       │       ├── sidebar.tsx
│       │       ├── skeleton.tsx
│       │       ├── slider.tsx
│       │       ├── sonner.tsx
│       │       ├── spinner.tsx
│       │       ├── stagger-children.tsx
│       │       ├── switch.tsx
│       │       ├── table.tsx
│       │       ├── tabs.tsx
│       │       ├── textarea.tsx
│       │       ├── toggle-group.tsx
│       │       ├── toggle.tsx
│       │       └── tooltip.tsx
│       │
│       ├── contexts/                              # React Contexts
│       │   └── ThemeContext.tsx
│       │
│       ├── hooks/                                 # Custom Hooks
│       │   ├── useComposition.ts
│       │   ├── useFocusTrap.ts
│       │   ├── useMobile.tsx
│       │   └── usePersistFn.ts
│       │
│       ├── lib/                                   # Utilities & API
│       │   ├── api.generated.ts                   # OpenAPI Generated Types
│       │   ├── api.ts
│       │   ├── fieldLabels.ts
│       │   ├── javaApi.ts                         # API Client (793+ lines)
│       │   ├── utils.ts
│       │   └── validators/
│       │
│       ├── pages/                                 # Page Components
│       │   ├── Audit.tsx
│       │   ├── Audit.test.tsx
│       │   ├── ComplexRules.tsx
│       │   ├── ComponentShowcase.tsx
│       │   ├── Dashboard.tsx
│       │   ├── Dashboard.test.tsx
│       │   ├── DashboardProfessional.tsx
│       │   ├── Home.tsx
│       │   ├── Home.test.tsx
│       │   ├── Login.tsx
│       │   ├── Login.test.tsx
│       │   ├── NotFound.tsx
│       │   ├── NotFound.test.tsx
│       │   ├── Rules.tsx
│       │   ├── Rules.test.tsx
│       │   ├── RulesAdvanced.tsx
│       │   ├── RulesDidactic.tsx
│       │   ├── Transactions.tsx
│       │   ├── Transactions.test.tsx
│       │   ├── TransactionSimulator.tsx
│       │   ├── TransactionsProfessional.tsx
│       │   └── __snapshots__/
│       │
│       ├── styles/                                # Additional Styles
│       │
│       └── test/                                  # Test Setup
│           └── setup.ts
│
├── e2e/                            # ══════════════════════════════════
│   ├── api-health.spec.ts
│   ├── audit.spec.ts
│   ├── complex-rules.spec.ts
│   ├── dashboard.spec.ts
│   ├── login.spec.ts
│   ├── navigation.spec.ts
│   ├── rbac.spec.ts
│   ├── responsive.spec.ts
│   ├── rules-crud.spec.ts
│   ├── rules.spec.ts
│   └── transactions.spec.ts
│
├── docs/                           # ══════════════════════════════════
│   ├── 01_DOSSIE_URLS_FRAUD_PRODUCTS.md
│   ├── 02_CAPABILITIES_EXTRACTION.md
│   ├── 03_RULES_CATALOG_TOP50.md
│   ├── ANALISE_CAPACIDADE_REGRAS_COMPLEXAS.md
│   ├── ANALISE_URLS_REGRAS_DURAS.md
│   ├── ARCHITECTURE_MAP.md                        # CRITICAL
│   ├── DB_SCHEMA_RULES.md
│   ├── DSL_ADVANCED_AGGREGATIONS.md
│   ├── EXTERNAL_CREDIT_DATASET_RESEARCH.md
│   ├── EXTREME_CAPABILITIES_MAP.md
│   ├── FRAUD_DETECTION_ANALYST_GUIDE.md
│   ├── FRAUD_DETECTION_RULES_DEPLOYED.md
│   ├── FRAUD_RULES_CATALOG_COMPLETE_V31_V35.md
│   ├── FRAUD_RULES_CATALOG_V28.md
│   ├── FRAUD_RULES_CATALOG_V29_V30.md
│   ├── FRAUD_TYPOLOGIES.md
│   ├── GAPS_DA_SOLUCAO.md
│   ├── IMPLEMENTACOES_CAPACIDADE_TOTAL.md
│   ├── IMPLEMENTATION_REPORT.md
│   ├── PAYLOAD_CONTRACT_GUARD.md
│   ├── PAYLOAD_DICTIONARY.md                      # CRITICAL
│   ├── perf-baseline.md
│   ├── perf.md
│   ├── PESQUISA_REGRAS_DURAS_EFICIENTES.md
│   ├── PLANO_IMPLEMENTACAO_CAMPOS_DERIVADOS.md
│   ├── RELATORIO_EVOLUCAO_MOTOR_REGRAS_EFICIENCIA_ML.md
│   ├── RULE_ENGINE_CAPABILITIES.md                # CRITICAL
│   ├── RULES_SCHEMA_AND_FIELDS.md
│   ├── RULEX_REFERENCIA_PARAMETROS_OPERADORES.md  # CRITICAL
│   ├── adr/                                       # Architecture Decision Records
│   │   ├── 0001-clean-architecture.md
│   │   └── 0002-hikaricp-pool-optimization.md
│   └── rules/
│
├── openapi/                        # ══════════════════════════════════
│   └── rulex.yaml                                 # OpenAPI Specification (706 lines)
│
├── perf/                           # ══════════════════════════════════
│   ├── debug-test.js
│   ├── load-test.js
│   └── README.md
│
├── scripts/                        # ══════════════════════════════════
│   ├── build-replit-entry.cjs
│   └── validate.sh
│
└── patches/                        # ══════════════════════════════════
    └── wouter@3.7.1.patch
```

---

## 🎯 ESPECIFICAÇÃO DO MOTOR DE REGRAS

### 1. TIPOS DE REGRAS

#### 1.1 Regras Simples (`rule_configurations`)
- Tabela: `rule_configurations`
- Condições em JSON (`conditions_json`)
- Operador lógico único (AND/OR)
- Ideal para regras diretas e rápidas

#### 1.2 Regras Complexas (`complex_rules`)
- Estrutura hierárquica com grupos aninhados
- Tabelas: `complex_rules` + `rule_condition_groups` + `rule_conditions`
- Suporta até **10 níveis** de profundidade
- Operadores lógicos avançados: AND, OR, NOT, XOR, NAND, NOR
- Suporta **50+ operadores** de comparação

### 2. OPERADORES DE COMPARAÇÃO (COMPLETO)

```java
public enum ConditionOperator {
    // Comparação básica
    EQ, NEQ, GT, GTE, LT, LTE,
    
    // Listas
    IN, NOT_IN,
    
    // Strings
    CONTAINS, NOT_CONTAINS, STARTS_WITH, ENDS_WITH, REGEX, NOT_REGEX,
    
    // Nulos
    IS_NULL, NOT_NULL,
    
    // Booleanos
    IS_TRUE, IS_FALSE,
    
    // Range
    BETWEEN, NOT_BETWEEN,
    
    // Comparação entre campos
    FIELD_EQ, FIELD_NEQ, FIELD_GT, FIELD_GTE, FIELD_LT, FIELD_LTE,
    
    // Data/Tempo
    DATE_BEFORE, DATE_AFTER, DATE_BETWEEN,
    TIME_BEFORE, TIME_AFTER, TIME_BETWEEN,
    
    // Array
    ARRAY_CONTAINS, ARRAY_NOT_CONTAINS, ARRAY_SIZE_EQ, ARRAY_SIZE_GT, ARRAY_SIZE_LT,
    
    // Matemáticos
    MOD_EQ, MOD_NEQ,
    
    // Geolocalização
    GEO_DISTANCE_LT, GEO_DISTANCE_GT, GEO_IN_POLYGON,
    
    // Velocity (agregações temporais)
    VELOCITY_COUNT_GT, VELOCITY_COUNT_LT, VELOCITY_SUM_GT, VELOCITY_SUM_LT,
    VELOCITY_AVG_GT, VELOCITY_AVG_LT, VELOCITY_DISTINCT_GT, VELOCITY_DISTINCT_LT,
    
    // Agregações temporais avançadas (DSL expandida)
    SUM_LAST_N_DAYS, COUNT_LAST_N_HOURS, AVG_LAST_N_DAYS,
    COUNT_DISTINCT_MERCHANTS_LAST_N_DAYS, COUNT_DISTINCT_COUNTRIES_LAST_N_HOURS,
    MAX_AMOUNT_LAST_N_DAYS, MIN_AMOUNT_LAST_N_DAYS,
    
    // Operadores avançados de fraude (Triple Check V36)
    GT_FIELD_MULTIPLIER, DECIMAL_PLACES_GT, EXPIRES_WITHIN_DAYS,
    IS_NEW, IS_FIRST, LT_CURRENT_DATE, GT_CURRENT_DATE,
    NOT_IN_CUSTOMER_HISTORY, IN_CUSTOMER_HISTORY,
    NOT_IN_CUSTOMER_USUAL_HOURS, IN_CUSTOMER_USUAL_HOURS,
    IN_CUSTOMER_CHARGEBACK_MERCHANTS, PERCENTAGE_OF_FIELD,
    HOUR_BETWEEN, DAY_OF_WEEK_IN, IS_WEEKEND, IS_HOLIDAY,
    DISTANCE_FROM_LAST_GT, TIME_SINCE_LAST_LT,
    COUNT_FAILURES_LAST_N_HOURS, SUM_LAST_N_HOURS,
    COUNT_DISTINCT_MERCHANTS_LAST_N_HOURS,
    VELOCITY_SPIKE, AMOUNT_SPIKE, PATTERN_ESCALATION,
    PATTERN_ROUND_NUMBERS, PATTERN_SPLIT_TRANSACTION,
    NOT_IN_HISTORICAL, NAME_SIMILARITY_LT, GTE_PERCENT_OF_LAST_INCOMING
}
```

### 3. OPERADORES LÓGICOS DE GRUPO

```java
public enum GroupLogicOperator {
    AND,   // Todas verdadeiras
    OR,    // Pelo menos uma verdadeira
    NOT,   // Inverte resultado
    XOR,   // Exatamente uma verdadeira
    NAND,  // Pelo menos uma falsa
    NOR    // Todas falsas
}
```

### 4. DECISÕES (OUTCOMES)

```java
public enum DecisionOutcome {
    APROVADO,           // Transação aprovada
    SUSPEITA_DE_FRAUDE, // Transação suspeita (review)
    FRAUDE              // Transação fraudulenta (bloquear)
}
```

### 5. AÇÕES SUPORTADAS

```java
public enum RuleActionType {
    SET_DECISION,      // Define a decisão
    SET_SCORE,         // Define o score de risco
    ADD_TAG,           // Adiciona tag
    REMOVE_TAG,        // Remove tag
    SET_VARIABLE,      // Define variável de contexto
    CALL_WEBHOOK,      // Chama webhook externo
    SEND_NOTIFICATION, // Envia notificação
    BLOCK_TRANSACTION, // Bloqueia transação
    FLAG_FOR_REVIEW,   // Marca para revisão
    ESCALATE           // Escala para nível superior
}
```

---

## 📊 DICIONÁRIO DE PAYLOAD (CAMPOS DE TRANSAÇÃO)

### Campos OBRIGATÓRIOS:

| Campo | Tipo | Descrição |
|-------|------|-----------|
| `externalTransactionId` | String | ID único da transação |
| `customerIdFromHeader` | String | ID do cliente |
| `customerAcctNumber` | Long | Número da conta |
| `pan` | String | Número do cartão (tokenizado) |
| `transactionAmount` | BigDecimal | Valor (centavos) |
| `transactionDate` | Integer | Data (YYYYMMDD) |
| `transactionTime` | Integer | Hora (HHMMSS) |
| `transactionCurrencyCode` | Integer | Código moeda ISO 4217 |
| `mcc` | Integer | Merchant Category Code |
| `consumerAuthenticationScore` | Integer | Score de autenticação (0-999) |
| `externalScore3` | Integer | Score externo (0-999) |
| `cavvResult` | Integer | Resultado CAVV (0-9) |
| `eciIndicator` | Integer | Indicador ECI 3DS (1-7) |
| `atcCard` | Integer | ATC do cartão |
| `atcHost` | Integer | ATC do host |
| `tokenAssuranceLevel` | Integer | Nível garantia token (0-99) |
| `availableCredit` | BigDecimal | Crédito disponível |
| `cardCashBalance` | BigDecimal | Saldo em dinheiro |
| `cardDelinquentAmount` | BigDecimal | Valor em atraso |

### Campos OPCIONAIS (Merchant):

| Campo | Tipo |
|-------|------|
| `merchantId` | String |
| `merchantName` | String |
| `merchantCity` | String |
| `merchantState` | String |
| `merchantCountryCode` | String |
| `merchantPostalCode` | String |
| `onUsMerchantId` | String |

### Campos OPCIONAIS (POS/Terminal):

| Campo | Tipo |
|-------|------|
| `posEntryMode` | String |
| `customerPresent` | String |
| `posOffPremises` | Integer |
| `posCardCapture` | Integer |
| `posSecurity` | Integer |
| `posConditionCode` | String |
| `terminalId` | String |
| `terminalType` | String |
| `terminalEntryCapability` | String |

### Campos OPCIONAIS (Autenticação):

| Campo | Tipo |
|-------|------|
| `cryptogramValid` | String |
| `cvv2Response` | String |
| `cvv2Present` | String |
| `pinVerifyCode` | String |
| `cvvVerifyCode` | String |
| `cvrofflinePinVerificationPerformed` | Integer |
| `cvrofflinePinVerificationFailed` | Integer |
| `cvvPinTryLimitExceeded` | Integer |

---

## 🔌 ENDPOINTS DA API

### Transações
| Método | Endpoint | Descrição |
|--------|----------|-----------|
| POST | `/api/transactions/analyze` | Analisa transação (motor padrão) |
| POST | `/api/transactions/analyze-advanced` | Analisa com regras avançadas |
| GET | `/api/transactions` | Lista transações paginadas |
| GET | `/api/transactions/{id}` | Busca por ID interno |
| GET | `/api/transactions/external/{externalId}` | Busca por ID externo |
| GET | `/api/transactions/export` | Exporta CSV/JSON |

### Regras Simples
| Método | Endpoint | Descrição |
|--------|----------|-----------|
| GET | `/api/rules` | Lista regras paginadas |
| POST | `/api/rules` | Cria regra |
| GET | `/api/rules/{id}` | Busca regra |
| PUT | `/api/rules/{id}` | Atualiza regra |
| DELETE | `/api/rules/{id}` | Remove regra |
| PATCH | `/api/rules/{id}/toggle` | Ativa/desativa |
| GET | `/api/rules/enabled/{enabled}` | Lista por status |
| GET | `/api/rules/{id}/history` | Histórico |

### Regras Complexas
| Método | Endpoint | Descrição |
|--------|----------|-----------|
| GET | `/api/complex-rules` | Lista regras complexas |
| GET | `/api/complex-rules/{id}` | Busca por ID |
| GET | `/api/complex-rules/key/{key}` | Busca por chave |
| POST | `/api/complex-rules` | Cria regra |
| PUT | `/api/complex-rules/{id}` | Atualiza |
| DELETE | `/api/complex-rules/{id}` | Remove |
| POST | `/api/complex-rules/validate` | Valida sem salvar |
| POST | `/api/complex-rules/{id}/duplicate` | Duplica |
| PATCH | `/api/complex-rules/{id}/toggle` | Toggle status |

### Avaliação
| Método | Endpoint | Descrição |
|--------|----------|-----------|
| POST | `/api/evaluate` | Avalia transação (DTO) |
| POST | `/api/evaluate/raw` | Avalia raw JSON (deprecated) |

### Auditoria
| Método | Endpoint | Descrição |
|--------|----------|-----------|
| GET | `/api/audit` | Lista eventos |
| GET | `/api/audit/export` | Exporta CSV/JSON |

### Métricas
| Método | Endpoint | Descrição |
|--------|----------|-----------|
| GET | `/api/metrics` | Métricas gerais |
| GET | `/api/metrics/mcc` | Por MCC |
| GET | `/api/metrics/merchant` | Por merchant |
| GET | `/api/metrics/timeline` | Timeline |

### Field Dictionary
| Método | Endpoint | Descrição |
|--------|----------|-----------|
| GET | `/api/v31/field-dictionary` | Lista campos |

---

## 🗄️ MODELO DE DADOS (TABELAS PRINCIPAIS)

### Regras
- `rule_configurations` - Regras simples
- `complex_rules` - Regras complexas (header)
- `rule_condition_groups` - Grupos de condições
- `rule_conditions` - Condições individuais
- `rules` - Regras versionadas (header)
- `rule_versions` - Versões de regras
- `rule_sets` - Conjuntos de regras
- `rule_set_versions` - Versões de conjuntos
- `active_rule_set` - Conjunto ativo (singleton)

### Suporte
- `velocity_counters` - Contadores pré-computados
- `velocity_transaction_log` - Log para agregações
- `geo_reference` - Referências geográficas
- `geo_polygon` - Polígonos geográficos
- `bin_lookup` - Lookup de BINs
- `mcc_categories` - Categorias MCC
- `device_fingerprint` - Fingerprint de dispositivos
- `pan_location_history` - Histórico de localização

### Auditoria
- `decision_log` - Log de decisões
- `audit_log` - Log de auditoria
- `rule_execution_details` - Detalhes de execução
- `rule_configuration_history` - Histórico de alterações
- `access_log` - Log de acesso
- `shadow_evaluation_log` - Log modo shadow

### RBAC
- `roles` - Papéis (ADMIN, ANALYST)
- `users` - Usuários
- `user_roles` - Relação usuário-papel

---

## 🔧 CONFIGURAÇÕES CRÍTICAS

### docker-compose.yml
```yaml
services:
  postgres:
    image: postgres:16-alpine
    environment:
      POSTGRES_DB: rulex_db
      POSTGRES_USER: postgres
      POSTGRES_PASSWORD: postgres
    ports: ["5432:5432"]
    
  redis:
    image: redis:7-alpine
    ports: ["6379:6379"]
    
  backend:
    build: ./backend
    environment:
      SPRING_PROFILES_ACTIVE: dev
      SPRING_DATASOURCE_URL: jdbc:postgresql://postgres:5432/rulex_db
      RULEX_SECURITY_ENABLED: true
      RULEX_ADMIN_USERNAME: admin
      RULEX_ADMIN_PASSWORD: rulex
    ports: ["8080:8080"]
    
  web:
    build: .
    dockerfile: Dockerfile.web
    environment:
      VITE_API_PROXY_TARGET: http://backend:8080
      VITE_API_BASIC_AUTH: admin:rulex
    ports: ["5173:5173"]
```

### application.yml (Críticos)
```yaml
spring:
  datasource:
    hikari:
      maximum-pool-size: 100
      connection-timeout: 5000
  jpa:
    hibernate:
      ddl-auto: validate
  flyway:
    enabled: true
    
server:
  port: 8080
  servlet:
    context-path: /api
```

### vite.config.ts
```typescript
export default defineConfig({
  plugins: [react(), tailwindcss(), vitePluginManusRuntime()],
  resolve: {
    alias: {
      "@": path.resolve(import.meta.dirname, "client", "src"),
    },
  },
  server: {
    proxy: {
      "/api": {
        target: process.env.VITE_API_PROXY_TARGET ?? "http://localhost:8080",
        changeOrigin: true,
      },
    },
  },
});
```

---

## 🔐 SEGURANÇA

### Autenticação
- HTTP Basic Auth (Spring Security)
- Usuários: `admin:rulex`, `analyst:rulex`
- Roles: `ADMIN`, `ANALYST`

### Proteções
- Rate Limiting (Bucket4j)
- Secure Cookies
- CORS configurado
- Security Headers
- PAN Masking/Hashing (SHA-256)
- RegexValidator (proteção ReDoS)
- Payload size limit (1MB)

---

## 📈 OBSERVABILIDADE

### Métricas (Micrometer + Prometheus)
- `http.server.requests`
- `hikaricp.connections.*`
- `jvm.memory.*`
- Custom: `rulex.transactions.*`, `rulex.rules.*`

### Tracing (OpenTelemetry)
- Distributed tracing habilitado
- Correlation ID em todas as requisições

### Health Checks
- `/api/actuator/health`
- `/api/actuator/prometheus`
- `/api/actuator/metrics`

---

## 🧪 TESTES

### Backend (JUnit 5)
```bash
cd backend && mvn test
```
- Testcontainers para PostgreSQL
- ArchUnit para testes de arquitetura
- Spring Cloud Contract para testes de contrato

### Frontend (Vitest)
```bash
pnpm test
```
- Testing Library
- Coverage com V8

### E2E (Playwright)
```bash
pnpm e2e
```
- Testes de fluxo completo
- Screenshots em falha

---

## 🚀 COMANDOS DE DESENVOLVIMENTO

### Setup
```bash
# Frontend
pnpm install --frozen-lockfile

# Backend
mvn -f backend/pom.xml dependency:resolve
```

### Dev
```bash
# Full stack (Docker)
docker compose up -d --build

# Frontend only
pnpm dev

# Backend only
cd backend && mvn spring-boot:run
```

### Build
```bash
# Frontend
pnpm build

# Backend
mvn -f backend/pom.xml package -DskipTests
```

### Lint
```bash
# Frontend
pnpm check

# Backend
mvn -f backend/pom.xml spotless:check
mvn -f backend/pom.xml spotless:apply  # fix
```

---

## 📋 CHECKLIST DE VALIDAÇÃO PARA DEVIN

### Backend
- [ ] Todas as 30 migrations Flyway executam sem erro
- [ ] Todos os 50+ operadores implementados em `ComplexRuleEvaluator`
- [ ] `RuleEngineService` com anti-tamper/idempotency
- [ ] Todos os controllers com OpenAPI annotations
- [ ] Testes passando: `mvn test`
- [ ] Spotless check: `mvn spotless:check`

### Frontend
- [ ] Todos os 20+ componentes de página renderizam
- [ ] 57+ componentes UI (shadcn) funcionais
- [ ] `ComplexRuleBuilder` com drag-and-drop
- [ ] `javaApi.ts` com todos os endpoints
- [ ] Testes passando: `pnpm test`
- [ ] TypeScript check: `pnpm check`

### Integração
- [ ] Docker Compose sobe todos os serviços
- [ ] Login funciona com `admin:rulex`
- [ ] CRUD de regras simples funcional
- [ ] CRUD de regras complexas funcional
- [ ] Análise de transação retorna decisão
- [ ] Auditoria registra eventos
- [ ] Métricas expostas em Prometheus

### E2E
- [ ] `login.spec.ts` passa
- [ ] `rules-crud.spec.ts` passa
- [ ] `complex-rules.spec.ts` passa
- [ ] `transactions.spec.ts` passa
- [ ] `audit.spec.ts` passa

---

## ⚠️ AVISOS CRÍTICOS

1. **NUNCA** alterar contratos de API sem aprovação
2. **NUNCA** usar ddl-auto para criar/alterar schema (use Flyway)
3. **NUNCA** armazenar PAN em claro
4. **SEMPRE** manter testes atualizados
5. **SEMPRE** documentar ADRs para decisões arquiteturais

---

## 🔗 ARQUIVOS DE REFERÊNCIA CRÍTICOS

1. `docs/ARCHITECTURE_MAP.md` - Mapa completo da arquitetura
2. `docs/PAYLOAD_DICTIONARY.md` - Todos os campos de payload
3. `docs/RULE_ENGINE_CAPABILITIES.md` - Capacidades do motor
4. `docs/RULEX_REFERENCIA_PARAMETROS_OPERADORES.md` - Referência completa
5. `openapi/rulex.yaml` - Especificação OpenAPI
6. `backend/src/main/java/com/rulex/entity/complex/RuleCondition.java` - Todos operadores

---

**FIM DO PROMPT DE BACKUP**

---

*Este documento foi gerado para permitir a reconstrução completa e fiel do sistema RULEX. Qualquer dúvida, consulte os arquivos de documentação no diretório `/docs`.*
