# Votação Consolidada do Painel Multidisciplinar

**Data**: 2025-12-19  
**Projeto**: RULEX - Motor de Regras Duras para Detecção de Fraude  
**Versão Avaliada**: Commit atual do repositório

---

## PASSO 0 — INVENTÁRIO DO REPOSITÓRIO

### Contagem de Arquivos por Tipo

| Tipo | Quantidade | Evidência |
|------|------------|-----------|
| Java (Backend) | 118 | `find . -name "*.java"` |
| TypeScript/TSX (Frontend) | 77 | `find . -name "*.tsx"` |
| TypeScript (Shared/Server) | 14 | `find . -name "*.ts"` |
| SQL (Migrations) | 3 | `backend/src/main/resources/db/migration/` |
| JSON | 11 | Configs, Insomnia, etc. |
| YAML/YML | 5 | OpenAPI, docker-compose |

### Estrutura Identificada

- **Backend Java**: Spring Boot 3.2.1 + PostgreSQL + Flyway
- **Frontend React**: React 19 + Vite + TailwindCSS + shadcn/ui
- **Database**: PostgreSQL (Flyway migrations V1-V3)
- **Testes Java**: 8 arquivos de teste (unitários + integração)
- **Testes Frontend**: 1 arquivo de teste (Rules.test.tsx)
- **Insomnia**: Coleção completa para homologação (`Insomnia/rulex-hml.insomnia.json`)

---

## PASSO 1 — EXTRAÇÃO DE FUNCIONALIDADES

### Endpoints Identificados

| Endpoint | Método | Controller | Evidência |
|----------|--------|------------|-----------|
| `/api/transactions/analyze` | POST | TransactionController | `TransactionController.java:36` |
| `/api/transactions/analyze-advanced` | POST | TransactionController | `TransactionController.java:112` |
| `/api/transactions` | GET | TransactionController | `TransactionController.java:50` |
| `/api/transactions/{id}` | GET | TransactionController | `TransactionController.java:89` |
| `/api/transactions/external/{externalId}` | GET | TransactionController | `TransactionController.java:100` |
| `/api/rules` | GET, POST | RuleController | `RuleController.java:26,47` |
| `/api/rules/{id}` | GET, PUT, DELETE | RuleController | `RuleController.java:38,58,69` |
| `/api/rules/{id}/toggle` | PATCH | RuleController | `RuleController.java:78` |
| `/api/rules/enabled/{enabled}` | GET | RuleController | `RuleController.java:87` |
| `/api/rules/{id}/history` | GET | RuleController | `RuleController.java:98` |
| `/api/audit` | GET | AuditController | `AuditController.java:28` |
| `/api/audit/transaction/{transactionId}` | GET | AuditController | `AuditController.java:66` |
| `/api/metrics` | GET | MetricsController | `MetricsController.java:21` |
| `/api/metrics/mcc` | GET | MetricsController | `MetricsController.java:31` |
| `/api/metrics/merchant` | GET | MetricsController | `MetricsController.java:41` |
| `/api/metrics/timeline` | GET | MetricsController | `MetricsController.java:51` |
| `/evaluate` | POST | EvaluateController | `EvaluateController.java:24` |
| `/api/homolog/rules` | POST | HomologRuleController | `HomologRuleController.java:22` |
| `/api/homolog/rules/{ruleId}/latest` | GET | HomologRuleController | `HomologRuleController.java:29` |
| `/api/homolog/rulesets` | POST | HomologRuleSetController | `HomologRuleSetController.java:19` |
| `/api/homolog/simulations/run` | POST | HomologSimulationController | `HomologSimulationController.java:20` |

### Motor de Regras Duras

**Evidência**: `RuleEngineService.java` + `AdvancedRuleEngineService.java`

- **Regras Genéricas**: Condições configuráveis via JSON (`conditionsJson`)
- **Regras Legadas**: 12 regras hardcoded por nome (`RuleEngineService.java:239-275`)
- **Regras Avançadas**: 28 regras implementadas (`AdvancedRuleEngineService.java`)

### Conceito Popup → 1..N Regras

**Evidência**: `RuleEngineService.java:443-512` (método `aggregatePopups`)

- ✅ Implementado: Agregação de regras por classificação (FRAUD, SUSPICIOUS, APPROVED)
- ✅ Retorna `PopupDTO` com título, classificação, totalContribution, rules

### Fluxos Críticos (P0/P1)

1. **P0 - Análise de Transação**: `/api/transactions/analyze` → `RuleEngineService.analyzeTransaction()`
2. **P0 - Idempotência**: `externalTransactionId` único (`RuleEngineService.java:52-70`)
3. **P1 - Avaliação com Popups**: `/evaluate` → `RuleEngineService.evaluate()`
4. **P1 - CRUD de Regras**: `/api/rules` (GET, POST, PUT, DELETE, PATCH)

---

## PASSO 2 — EXTRAÇÃO REAL DAS REGRAS DURAS

### Regras Implementadas no Código

#### Regras Legadas (12 regras) - `RuleEngineService.java:239-275`

| Nome | Condição | Campos Usados | Evidência |
|------|----------|---------------|-----------|
| LOW_AUTHENTICATION_SCORE | `consumerAuthenticationScore < threshold` | `consumerAuthenticationScore` | Linha 241-242 |
| LOW_EXTERNAL_SCORE | `externalScore3 < threshold` | `externalScore3` | Linha 244 |
| INVALID_CAVV | `cavvResult != 0` | `cavvResult` | Linha 246 |
| INVALID_CRYPTOGRAM | `cryptogramValid != "V"` | `cryptogramValid` | Linha 248 |
| CVV_MISMATCH | `cvv2Response == "N"` | `cvv2Response` | Linha 250 |
| HIGH_TRANSACTION_AMOUNT | `transactionAmount > threshold` | `transactionAmount` | Linha 252-253 |
| HIGH_RISK_MCC | `mcc IN {7995,6211,6051,7273,7994}` | `mcc` | Linha 255, 278-288 |
| INTERNATIONAL_TRANSACTION | `merchantCountryCode != "076"` | `merchantCountryCode` | Linha 257, 292-295 |
| CARD_NOT_PRESENT | `customerPresent != "Y"` | `customerPresent` | Linha 259 |
| PIN_VERIFICATION_FAILED | `pinVerifyCode == "I"` | `pinVerifyCode` | Linha 261 |
| CVV_PIN_LIMIT_EXCEEDED | `cvvPinTryLimitExceeded == 1` | `cvvPinTryLimitExceeded` | Linha 264-265 |
| OFFLINE_PIN_FAILED | `cvrofflinePinVerificationPerformed == 1 AND cvrofflinePinVerificationFailed == 1` | `cvrofflinePinVerificationPerformed`, `cvrofflinePinVerificationFailed` | Linha 267-269 |

#### Regras Avançadas (28 regras) - `AdvancedRuleEngineService.java`

| # | Nome | Categoria | Método | Evidência |
|---|------|-----------|--------|-----------|
| 1 | EMV_SECURITY_CHECK | EMV_SECURITY | `checkEMVSecurity()` | Linha 57-72 |
| 2 | TERMINAL_VERIFICATION_FAILED | EMV_SECURITY | `checkTerminalVerificationFailed()` | Linha 75-91 |
| 3 | EXPIRED_CARD | TRANSACTION_CONTEXT | `checkExpiredCard()` | Linha 96-110 |
| 4 | SUSPICIOUS_TRANSACTION_TYPE | TRANSACTION_CONTEXT | `checkSuspiciousTransactionType()` | Linha 113-127 |
| 5 | UNUSUAL_CARD_MEDIA | TRANSACTION_CONTEXT | `checkUnusualCardMedia()` | Linha 130-145 |
| 6 | SUSPICIOUS_TERMINAL | TERMINAL_NETWORK | `checkSuspiciousTerminal()` | Linha 153-164 |
| 7 | ECOMMERCE_NO_AVS | TERMINAL_NETWORK | `checkEcommerceNoAVS()` | Linha 167-178 |
| 8 | POS_SECURITY_MISSING | TERMINAL_NETWORK | `checkPOSSecurityMissing()` | Linha 181-192 |
| 9 | CARD_CAPTURE_FRAUD | TERMINAL_NETWORK | `checkCardCaptureFraud()` | Linha 195-210 |
| 10 | PIN_CVV_LIMIT_EXCEEDED | PIN_CVV_VERIFICATION | `checkPinCvvLimitExceeded()` | Linha 215-224 |
| 11 | OFFLINE_PIN_FAILED | PIN_CVV_VERIFICATION | `checkOfflinePinFailed()` | Linha 227-237 |
| 12 | MISSING_CVV2_HIGH_RISK | PIN_CVV_VERIFICATION | `checkMissingCvv2HighRisk()` | Linha 240-251 |
| 13 | CUSTOM_INDICATOR_FRAUD | CUSTOM_INDICATORS | `checkCustomIndicatorFraud()` | Linha 256-268 |
| 14 | PROCESSING_LAG_ANOMALY | TEMPORAL_ADVANCED | `checkProcessingLagAnomaly()` | Linha 273-287 |
| 15 | TIMEZONE_NORMALIZED_CHECK | TEMPORAL_ADVANCED | `checkTimezoneNormalizedCheck()` | Linha 290-305 |
| 16 | DUPLICATE_TRANSACTION | UNIQUE_IDENTIFIERS | `checkDuplicateTransaction()` | Linha 310-323 |
| 17 | SUSPICIOUS_MERCHANT_POSTAL | UNIQUE_IDENTIFIERS | `checkSuspiciousMerchantPostal()` | Linha 326-336 |
| 18 | SUSPICIOUS_TOKEN | UNIQUE_IDENTIFIERS | `checkSuspiciousToken()` | Linha 339-351 |
| 19 | UNEXPECTED_CURRENCY | CURRENCY_CONVERSION | `checkUnexpectedCurrency()` | Linha 356-369 |
| 20 | ANOMALOUS_CONVERSION_RATE | CURRENCY_CONVERSION | `checkAnomalousConversionRate()` | Linha 372-391 |
| 21 | INCOHERENT_AUTH_SEQUENCE | AUTH_SEQUENCE | `checkIncoherentAuthSequence()` | Linha 396-415 |
| 22 | INCOHERENT_CONTEXT | CONTEXT_COHERENCE | `checkIncoherentContext()` | Linha 420-436 |
| 23 | CONTRADICTORY_AUTHORIZATION | AUTHORIZATION_CONTRADICTION | `checkContradictoryAuthorization()` | Linha 441-459 |
| 24 | SUSPICIOUS_ACQUIRER | ACQUIRER_PATTERN | `checkSuspiciousAcquirer()` | Linha 464-477 |
| 25 | ACQUIRER_COUNTRY_MISMATCH | ACQUIRER_PATTERN | `checkAcquirerCountryMismatch()` | Linha 480-495 |
| 26 | COMBINED_SCORE_CHECK | - | `checkCombinedScore()` | Linha 500-520 |
| 27 | VELOCITY_CHECK_CONSOLIDATED | - | `checkVelocityConsolidated()` | Linha 523-558 |
| 28 | CUSTOM_INDICATORS_COMPREHENSIVE | - | `checkCustomIndicatorsComprehensive()` | Linha 561-578 |

**Total de Regras Implementadas**: 12 (legadas) + 28 (avançadas) = **40 regras**

### Regras Documentadas mas NÃO Implementadas (GAP)

**Evidência**: `REGRAS_DURAS_60_IMPLEMENTACAO.md` lista 60+ regras, mas apenas 40 estão implementadas.

**GAP P1**: 20+ regras documentadas não estão implementadas, incluindo:
- Regras de velocity detalhadas (5min, 15min, 1h spikes)
- Regras geográficas (impossibilidade geográfica, múltiplos países)
- Regras de card testing (múltiplas transações pequenas)
- Regras de MCC detalhadas (gambling, crypto, adult content)

---

## Tabela de Votação Consolidada

| # | ESPECIALISTA | NOTA | PESO | SCORE PONDERADO | PRINCIPAL ARGUMENTO |
|---|--------------|------|------|-----------------|---------------------|
| 1 | Negócio (Crédito/Fraude) | 6.5 | 1.3 | 8.45 | 40 regras implementadas vs 60+ documentadas. Falta cobertura completa de padrões de fraude. |
| 2 | Product Owner Técnico | 7.0 | 1.0 | 7.00 | Endpoints bem definidos, Popup→Rules implementado. Falta especificação formal de critérios de aceite. |
| 3 | Arquiteto de Software | 7.5 | 1.2 | 9.00 | Clean Architecture + Hexagonal bem implementado. Duas stacks de banco aumentam complexidade. |
| 4 | UX Designer | 5.5 | 1.0 | 5.50 | Dashboard funcional mas falta feedback visual em operações críticas. Simulador existe mas precisa refinamento. |
| 5 | UI Designer | 6.0 | 0.9 | 5.40 | shadcn/ui bem aplicado. Design system documentado. Falta consistência em componentes customizados. |
| 6 | Product Designer | 6.0 | 0.9 | 5.40 | Fluxos principais cobertos. Falta jornada de onboarding e documentação de personas. |
| 7 | Backend Engineer Java | 8.0 | 1.2 | 9.60 | RuleEngineService robusto, idempotência, auditoria completa. 28 regras avançadas funcionais. Fallback legado pode causar confusão. |
| 8 | Frontend Engineer React | 6.5 | 1.0 | 6.50 | React Query + tRPC bem integrados. Apenas 1 arquivo de teste frontend. Falta tratamento de erros em alguns fluxos. |
| 9 | DBA / PostgreSQL | 7.0 | 1.1 | 7.70 | Schema bem normalizado, índices criados, FK com constraints. Falta particionamento para alto volume. |
| 10 | QA Engineer (Lead) | 5.0 | 1.3 | 6.50 | Testes unitários Java existem (8 arquivos). Testes integração com Testcontainers. **GAP CRÍTICO: Apenas 1 teste frontend. Sem E2E.** |
| 11 | AppSec / Segurança | 5.5 | 1.2 | 6.60 | CORS configurado mas permissivo. PAN masking implementado. **GAP CRÍTICO: Sem Spring Security configurado. Sem autenticação/autorização.** |
| 12 | DevOps / SRE | 6.5 | 1.0 | 6.50 | Docker Compose funcional, health checks. Falta CI/CD pipeline documentado. |

---

## Cálculos

### Soma dos Pesos
1.3 + 1.0 + 1.2 + 1.0 + 0.9 + 0.9 + 1.2 + 1.0 + 1.1 + 1.3 + 1.2 + 1.0 = **13.1**

### Soma dos Scores Ponderados
8.45 + 7.00 + 9.00 + 5.50 + 5.40 + 5.40 + 9.60 + 6.50 + 7.70 + 6.50 + 6.60 + 6.50 = **84.55**

### Média Ponderada Final
**84.55 / 13.1 = 6.45**

---

## Divergências Entre Especialistas

| Área | Divergência | Especialistas |
|------|-------------|---------------|
| Segurança | Backend considera segurança adequada; AppSec identifica gaps críticos | Backend (8.0) vs AppSec (5.5) |
| Testes | Backend considera testes suficientes; QA identifica gaps críticos | Backend (8.0) vs QA (5.0) |
| Regras | Negócio quer 60+ regras; Backend implementou 40 | Negócio (6.5) vs Backend (8.0) |

---

## Top 3 Maiores Riscos

1. **P0 - Ausência de Autenticação/Autorização**: Sistema bancário sem Spring Security configurado. Qualquer requisição pode acessar endpoints críticos.
2. **P1 - Ausência de Testes E2E**: Não há Cypress/Playwright para fluxos críticos. Bugs de integração podem passar despercebidos.
3. **P1 - Regras Documentadas Não Implementadas**: 20+ regras documentadas não estão no código, criando expectativa vs realidade.

---

## Top 3 Maiores Gaps

1. **GAP P0 - Spring Security Não Configurado**: Não encontrado `@EnableWebSecurity`, `SecurityFilterChain`, ou qualquer configuração de autenticação/autorização.
2. **GAP P1 - Testes E2E/Navegação SPA**: Não encontrado Cypress/Playwright. Apenas 1 teste frontend (Rules.test.tsx).
3. **GAP P1 - 20+ Regras Documentadas Não Implementadas**: `REGRAS_DURAS_60_IMPLEMENTACAO.md` lista 60+ regras, mas apenas 40 estão implementadas.

---

## Áreas com Maior Divergência

1. **Segurança**: AppSec identifica gap crítico (P0) de autenticação; Backend considera implementação adequada.
2. **Testes**: QA identifica gap crítico (P1) de E2E; Backend considera testes unitários/integração suficientes.
3. **Regras**: Negócio espera 60+ regras; Backend implementou 40 regras funcionais.
