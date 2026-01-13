# 🔍 PROMPT PARA ANÁLISE LINHA-A-LINHA - DEVIN

> **INSTRUÇÃO SUPREMA:** Este prompt instrui o Devin a analisar CADA ARQUIVO, CADA LINHA, CADA PALAVRA do projeto RULEX.

---

## FASE 1: PREPARAÇÃO

### 1.1 Clone e Setup
```bash
# 1. Clone o repositório
git clone https://github.com/FELIPEACASTRO/RULEX.git
cd RULEX

# 2. Checkout na branch correta
git checkout cursor/rulex-project-review-1c58

# 3. Instale dependências frontend
pnpm install --frozen-lockfile

# 4. Instale dependências backend
cd backend && mvn dependency:resolve && cd ..

# 5. Suba o ambiente Docker
docker compose up -d --build
```

---

## FASE 2: ANÁLISE SISTEMÁTICA

### 2.1 BACKEND - ANÁLISE COMPLETA

**ORDEM DE ANÁLISE:**

#### 2.1.1 Entry Point
```
ANALISE LINHA POR LINHA:
- backend/src/main/java/com/rulex/RulexApplication.java
```
Verifique:
- Annotations: `@SpringBootApplication`, `@ComponentScan`, `@EnableAsync`
- Package: `com.rulex`
- Main method exato

#### 2.1.2 Configurações Spring (CRÍTICO)
```
ANALISE PALAVRA POR PALAVRA cada arquivo em:
backend/src/main/java/com/rulex/config/
```
Arquivos:
1. `AccessLogFilter.java` - Logging de acesso
2. `AsyncConfig.java` - Configuração assíncrona
3. `CacheConfig.java` - Configuração de cache
4. `ClockConfig.java` - Clock para testes
5. `CorsConfig.java` - CORS
6. `OpenTelemetryConfig.java` - Tracing
7. `RateLimitingFilter.java` - Rate limiting (Bucket4j)
8. `RulexSecurityProperties.java` - Properties de segurança
9. `SecureCookieConfig.java` - Cookies seguros
10. `SecurityConfig.java` - Spring Security (HTTP Basic)
11. `SecurityHeadersConfig.java` - Headers de segurança
12. `VirtualThreadsConfig.java` - Virtual Threads Java 21

#### 2.1.3 API Layer
```
ANALISE COMPLETAMENTE:
backend/src/main/java/com/rulex/api/
```
- `ApiErrorResponse.java` - Estrutura de erro
- `CachedBodyHttpServletRequest.java` - Cache de body HTTP
- `CorrelationId.java` - ThreadLocal correlation ID
- `CorrelationIdFilter.java` - Filter para correlation
- `NotFoundException.java` - Exception customizada
- `RawPayloadCaptureFilter.java` - Captura payload raw

#### 2.1.4 Controllers (CRÍTICO)
```
ANALISE CADA ENDPOINT, CADA ANNOTATION:
backend/src/main/java/com/rulex/controller/
```
- `AuditController.java`
- `ComplexRuleCrudController.java`
- `EvaluateController.java` - POST /evaluate
- `MetricsController.java`
- `RuleApprovalController.java`
- `RuleController.java` - CRUD /rules
- `RuleExportImportController.java`
- `RuleMetricsController.java`
- `RuleSimulationController.java`
- `TransactionController.java` - /transactions/analyze
- `complex/ComplexRuleController.java`
- `homolog/*` - Endpoints de homologação

#### 2.1.5 DTOs (Data Transfer Objects)
```
ANALISE CADA CAMPO, CADA VALIDATION:
backend/src/main/java/com/rulex/dto/
```
Especialmente:
- `TransactionRequest.java` - 100+ campos
- `TransactionResponse.java`
- `EvaluateRequestDTO.java`
- `EvaluateResponse.java`
- `RuleConfigurationDTO.java`
- `RuleConditionDTO.java`
- `complex/ComplexRuleDTO.java`
- `complex/ConditionDTO.java`
- `complex/ConditionGroupDTO.java`

#### 2.1.6 Entities JPA (CRÍTICO)
```
ANALISE CADA TABELA, CADA COLUNA, CADA INDEX:
backend/src/main/java/com/rulex/entity/
```
Principais:
- `RuleConfiguration.java` - Regras simples
- `Transaction.java` - Transações
- `TransactionDecision.java` - Decisões
- `AuditLog.java` - Auditoria
- `VelocityCounter.java` - Contadores velocity
- `GeoReference.java` - Geo
- `DeviceFingerprint.java` - Device

```
Complex Rules Entities:
backend/src/main/java/com/rulex/entity/complex/
```
- `ComplexRule.java` - Entidade principal
- `RuleCondition.java` - **50+ OPERADORES ENUM**
- `RuleConditionGroup.java` - Grupos aninhados
- `RuleAction.java` - Ações
- `RuleContextVariable.java` - Variáveis
- `RuleExecutionDetail.java` - Detalhes execução
- `RuleExpression.java` - Expressões
- `RuleTemplate.java` - Templates

```
Homolog Entities:
backend/src/main/java/com/rulex/entity/homolog/
```
- `RuleEntity.java`
- `RuleVersionEntity.java`
- `RuleSetEntity.java`
- `DecisionLogEntity.java`
- `UserEntity.java`
- `RoleEntity.java`

#### 2.1.7 Repositories
```
ANALISE CADA QUERY, CADA METHOD:
backend/src/main/java/com/rulex/repository/
```
27+ repositórios JPA

#### 2.1.8 Services (MOTOR CORE)
```
ANÁLISE EXTREMAMENTE DETALHADA:
backend/src/main/java/com/rulex/service/
```

**CRÍTICOS (2000+ linhas cada):**
- `RuleEngineService.java` - Motor principal
- `AdvancedRuleEngineService.java` - Motor avançado

**Complex Services:**
- `complex/ComplexRuleEvaluator.java` - **2222 LINHAS - AVALIAR PALAVRA POR PALAVRA**
- `complex/ComplexRuleService.java`
- `complex/ComplexRuleCrudService.java`
- `complex/ComplexRuleExecutionService.java`
- `complex/ComplexRuleMapper.java`
- `complex/ExpressionEvaluator.java`
- `complex/RuleValidationService.java`

**Support Services:**
- `VelocityService.java` - Agregações temporais
- `GeoService.java` - Geolocalização
- `AuditService.java` - Auditoria
- `BloomFilterService.java` - Bloom filters
- `RedisVelocityService.java` - Cache Redis
- `ShadowModeService.java` - Modo shadow
- `ImpossibleTravelService.java` - Viagem impossível
- `DeviceFingerprintService.java` - Fingerprint
- `EnrichmentService.java` - Enriquecimento

**Enrichment Services:**
- `enrichment/AnomalyEnrichment.java`
- `enrichment/AuthEnrichment.java`
- `enrichment/CardEnrichment.java`
- `enrichment/CustomerEnrichment.java`
- `enrichment/DeviceEnrichment.java`
- `enrichment/GeoEnrichment.java`
- `enrichment/TransactionEnrichmentFacade.java`
- `enrichment/VelocityEnrichment.java`

#### 2.1.9 V3.1 Engine (AST + Field Dictionary)
```
ANALISE:
backend/src/main/java/com/rulex/v31/
```
- `ast/AstEvaluator.java`
- `ast/AstValidator.java`
- `execlog/RuleExecutionLogService.java`
- `field/FieldDictionaryService.java`
- `field/FieldDictionarySeeder.java`
- `rules/RulesV31Controller.java`

#### 2.1.10 Utilities
```
backend/src/main/java/com/rulex/util/
```
- `PanHashUtil.java` - Hash SHA-256 de PAN
- `PanMaskingUtil.java` - Mascaramento de PAN
- `RegexValidator.java` - Proteção ReDoS

#### 2.1.11 Resilience
```
backend/src/main/java/com/rulex/resilience/
```
- `DatabaseCircuitBreaker.java`

#### 2.1.12 Exception Handling
```
backend/src/main/java/com/rulex/exception/
```
- `GlobalExceptionHandler.java`

---

### 2.2 MIGRATIONS FLYWAY (CRÍTICO)

```
ANALISE CADA MIGRATION SQL, CADA CREATE TABLE, CADA INDEX:
backend/src/main/resources/db/migration/
```

| Migration | Descrição |
|-----------|-----------|
| V1__init.sql | Schema inicial, RBAC, enums |
| V2__core_schema.sql | Tabelas core |
| V3__extend_workflow_length.sql | Workflow field |
| V4__raw_hash_idempotency.sql | Anti-tamper |
| V5__raw_as_received.sql | Raw payload storage |
| V6__v31_exec_log_field_dictionary.sql | Field dictionary |
| V7__v31_exec_log_dedup.sql | Dedup execution log |
| V8__complex_rules_support.sql | Complex rules |
| V9__audit_compliance_enhancements.sql | Audit |
| V10__derived_context_improvements.sql | Derived context |
| V11__bin_lookup_table.sql | BIN lookup |
| V12__complex_rules_crud.sql | Complex CRUD |
| V13__geo_reference_table.sql | Geo reference |
| V14__velocity_counters.sql | Velocity counters |
| V15__add_velocity_operators.sql | Velocity operators |
| V16-V17 | Fixes geo types |
| V18__enable_condition_groups_constraint.sql | Constraints |
| V19__access_log_table.sql | Access log |
| V20__shadow_mode_and_device_fingerprinting.sql | Shadow mode |
| V21__rule_configurations_shadow_mode.sql | Shadow config |
| V22__fraud_detection_rules_seed.sql | Seed rules |
| V23__web_research_fraud_rules.sql | Web rules |
| V24__regras_fraude_portugues_completo.sql | PT rules |
| V25__additional_fraud_rules_200plus.sql | 200+ rules |
| V26__fix_complex_rules_conditions.sql | Fix conditions |
| V27__migrate_hardcoded_advanced_rules.sql | Migrate rules |
| V28__add_missing_condition_operators.sql | Missing operators |
| V29__insert_advanced_fraud_rules_catalog.sql | Advanced catalog |
| V30__insert_aml_ato_advanced_rules.sql | AML/ATO rules |

---

### 2.3 FRONTEND - ANÁLISE COMPLETA

#### 2.3.1 Entry Points
```
ANALISE:
client/src/main.tsx
client/src/App.tsx
```

#### 2.3.2 API Client (CRÍTICO)
```
ANALISE PALAVRA POR PALAVRA (793+ linhas):
client/src/lib/javaApi.ts
```
Conteúdo:
- Interfaces TypeScript (TransactionRequest, TransactionResponse, etc.)
- Funções de API (listRules, createRule, analyzeTransaction, etc.)
- Error handling
- Auth handling (Basic Auth)

#### 2.3.3 Complex Rule Builder (CRÍTICO)
```
ANALISE CADA COMPONENTE:
client/src/components/ComplexRuleBuilder/
```
- `index.tsx` - Main component
- `ConditionCard.tsx` - Card de condição
- `ConditionGroupCard.tsx` - Card de grupo
- `RuleMetadataForm.tsx` - Form metadata
- `RuleNaturalLanguage.tsx` - NLP display
- `RulePreview.tsx` - Preview
- `RuleSimulator.tsx` - Simulador
- `TemplateSelector.tsx` - Templates
- `types.ts` - TypeScript types

#### 2.3.4 Rule Form Dialog
```
client/src/components/RuleFormDialog/
```
- `index.tsx`
- `RuleFormDialog.tsx`
- `schema.ts` - Zod validation
- `types.ts`
- `useRuleForm.ts` - Hook

#### 2.3.5 UI Components (57+)
```
LISTE TODOS:
client/src/components/ui/
```
Todos os 57+ componentes shadcn/ui

#### 2.3.6 Pages
```
client/src/pages/
```
- `Audit.tsx`
- `ComplexRules.tsx`
- `Dashboard.tsx`
- `DashboardProfessional.tsx`
- `Home.tsx`
- `Login.tsx`
- `NotFound.tsx`
- `Rules.tsx` (1134 linhas)
- `RulesAdvanced.tsx`
- `RulesDidactic.tsx`
- `Transactions.tsx`
- `TransactionSimulator.tsx`
- `TransactionsProfessional.tsx`

#### 2.3.7 Contexts
```
client/src/contexts/
```
- `ThemeContext.tsx`

#### 2.3.8 Hooks
```
client/src/hooks/
```
- `useComposition.ts`
- `useFocusTrap.ts`
- `useMobile.tsx`
- `usePersistFn.ts`

#### 2.3.9 Core Auth
```
client/src/_core/auth/
```
- `tokens.ts`

---

### 2.4 E2E TESTS

```
ANALISE CADA SPEC:
e2e/
```
- `api-health.spec.ts`
- `audit.spec.ts`
- `complex-rules.spec.ts`
- `dashboard.spec.ts`
- `login.spec.ts`
- `navigation.spec.ts`
- `rbac.spec.ts`
- `responsive.spec.ts`
- `rules-crud.spec.ts`
- `rules.spec.ts`
- `transactions.spec.ts`

---

### 2.5 DOCUMENTAÇÃO

```
LEIA COMPLETAMENTE:
docs/
```
Especialmente:
- `ARCHITECTURE_MAP.md`
- `PAYLOAD_DICTIONARY.md`
- `RULE_ENGINE_CAPABILITIES.md`
- `RULEX_REFERENCIA_PARAMETROS_OPERADORES.md`
- `DB_SCHEMA_RULES.md`
- `DSL_ADVANCED_AGGREGATIONS.md`
- `FRAUD_DETECTION_ANALYST_GUIDE.md`

---

### 2.6 CONFIGURAÇÕES ROOT

```
ANALISE:
```
- `package.json` - Dependencies frontend
- `pom.xml` - Dependencies backend
- `docker-compose.yml` - Docker services
- `vite.config.ts` - Vite config
- `vitest.config.ts` - Test config
- `playwright.config.ts` - E2E config
- `tsconfig.json` - TypeScript
- `openapi/rulex.yaml` - OpenAPI spec (706 linhas)

---

## FASE 3: VALIDAÇÃO

### 3.1 Testes Backend
```bash
cd backend
mvn clean test
# Deve passar TODOS os testes
```

### 3.2 Lint Backend
```bash
mvn spotless:check
# Sem erros de formatação
```

### 3.3 Testes Frontend
```bash
pnpm test
# Deve passar TODOS os testes
```

### 3.4 TypeScript Check
```bash
pnpm check
# Sem erros de tipo
```

### 3.5 E2E Tests
```bash
# Suba o ambiente primeiro
docker compose up -d --build

# Execute E2E
pnpm e2e
```

### 3.6 Validação Manual

1. Acesse http://localhost:5173
2. Login com `admin:rulex`
3. Navegue por todas as páginas
4. Crie uma regra simples
5. Crie uma regra complexa
6. Execute uma simulação
7. Verifique auditoria
8. Verifique métricas

---

## FASE 4: DOCUMENTAÇÃO DO BACKUP

Após análise completa, gere:

1. **Inventário de Arquivos** - Lista completa com linha count
2. **Mapa de Dependências** - Imports entre módulos
3. **Cobertura de Operadores** - Todos os 50+ operadores
4. **Cobertura de Campos** - Todos os 100+ campos payload
5. **API Contract** - Todos os endpoints com schemas
6. **Database Schema** - Todas as tabelas com colunas

---

## ⚠️ ALERTA FINAL

**NÃO PULE NENHUM ARQUIVO.**
**NÃO SIMPLIFIQUE NENHUMA IMPLEMENTAÇÃO.**
**CADA PALAVRA CONTA.**

O objetivo é que, com este backup, seja possível **reconstruir o RULEX do ZERO** com **100% de fidelidade**.

---

**FIM DO PROMPT DE ANÁLISE**
