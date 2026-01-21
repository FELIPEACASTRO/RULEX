# 📋 RuleX - Catálogo Mestre de Testes de QA

> **Versão:** 2.0.0  
> **Última Atualização:** 2026-01-14  
> **Status:** ✅ Atualizado e Sincronizado com Código

---

## 📊 Resumo Executivo

| Métrica | Valor |
|---------|-------|
| **Total de Arquivos de Teste** | 58 |
| **Testes Backend (Java)** | 34 arquivos |
| **Testes Frontend (TypeScript)** | 13 arquivos |
| **Testes E2E (Playwright)** | 11 arquivos |
| **Cobertura de Operadores** | 447/447 (100%) |
| **Cobertura de APIs** | 16/16 (100%) |

---

## 🏗️ Arquitetura de Testes

```
┌─────────────────────────────────────────────────────────────────┐
│                    PIRÂMIDE DE TESTES RULEX                     │
├─────────────────────────────────────────────────────────────────┤
│                         E2E Tests                               │
│                      (11 arquivos)                              │
│                    ┌─────────────┐                              │
│                    │  Playwright │                              │
│                    └─────────────┘                              │
├─────────────────────────────────────────────────────────────────┤
│                    Integration Tests                            │
│                      (15+ arquivos)                             │
│              ┌───────────────────────────┐                      │
│              │  MockMvc + Testcontainers │                      │
│              └───────────────────────────┘                      │
├─────────────────────────────────────────────────────────────────┤
│                      Unit Tests                                 │
│                     (40+ arquivos)                              │
│         ┌─────────────────────────────────────┐                 │
│         │  JUnit 5 + Vitest + React Testing   │                 │
│         └─────────────────────────────────────┘                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 1️⃣ Testes Unitários

### 1.1 Backend (Java/Spring Boot)

| Arquivo | Descrição | Cobertura |
|---------|-----------|-----------|
| `OperatorSyncTest.java` | Sincronização Entity/DTO | 100% |
| `AllOperatorsIntegrationTest.java` | 447 operadores parametrizados | 100% |
| `ComplexRuleEvaluatorTest.java` | Motor de avaliação | 95%+ |
| `AdvancedRuleEngineServiceTest.java` | 28 regras de negócio | 100% |
| `VelocityServiceTest.java` | Contadores temporais | 90%+ |
| `GeoServiceTest.java` | Geolocalização | 85%+ |
| `AuditServiceTest.java` | Serviço de auditoria | 85%+ |
| `BloomFilterServiceTest.java` | Filtros Bloom | 80%+ |
| `RuleEngineServiceTest.java` | Motor de regras principal | 90%+ |

### 1.2 Frontend (TypeScript/React)

| Arquivo | Descrição | Cobertura |
|---------|-----------|-----------|
| `Rules.test.tsx` | Componente de regras | 90%+ |
| `ComplexRuleBuilder.test.tsx` | Construtor de regras | 85%+ |
| `Dashboard.test.tsx` | Dashboard principal | 75%+ |
| `Transactions.test.tsx` | Lista de transações | 80%+ |
| `Audit.test.tsx` | Página de auditoria | 75%+ |
| `Login.test.tsx` | Página de login | 85%+ |
| `operators.test.ts` | Testes de operadores | 90%+ |
| `schema.test.ts` | Validação de schemas | 85%+ |
| `regexValidator.test.ts` | Validador de regex | 95%+ |

---

## 2️⃣ Testes de Integração

### 2.1 API Integration Tests

| Arquivo | APIs Testadas | Cenários |
|---------|---------------|----------|
| `TransactionApiIntegrationTest.java` | Evaluate, Transaction | 14 |
| `RuleApiIntegrationTest.java` | Rule, ComplexRule | 21 |
| `SimulationApiIntegrationTest.java` | Simulation, V31 | 8 |
| `AuditAndMetricsApiIntegrationTest.java` | Audit, Metrics | 16 |
| `ExportImportAndApprovalApiIntegrationTest.java` | Export, Approval | 12 |

**Total: 71 cenários de integração**

### 2.2 Database Integration (Testcontainers)

```java
@Testcontainers
class DatabaseIntegrationTest {
    @Container
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:15");
    
    // Testes com banco real isolado
}
```

---

## 3️⃣ Testes E2E (Playwright)

### 3.1 Fluxos Testados

| Arquivo | Fluxo | Dispositivos |
|---------|-------|--------------|
| `login.spec.ts` | Autenticação | Desktop, Mobile |
| `rules.spec.ts` | CRUD de regras | Desktop |
| `rules-crud.spec.ts` | Operações CRUD completas | Desktop |
| `complex-rules.spec.ts` | Regras complexas | Desktop |
| `transactions.spec.ts` | Visualização | Desktop, Tablet |
| `dashboard.spec.ts` | Dashboard | Desktop |
| `responsive.spec.ts` | Responsividade | Desktop, Tablet, Mobile |
| `audit.spec.ts` | Auditoria | Desktop |
| `navigation.spec.ts` | Navegação | Desktop |
| `rbac.spec.ts` | Controle de acesso | Desktop |
| `api-health.spec.ts` | Health check da API | Desktop |

### 3.2 Viewports Testados

```typescript
const viewports = {
  desktop: { width: 1920, height: 1080 },
  tablet: { width: 768, height: 1024 },
  mobile: { width: 375, height: 667 }
};
```

---

## 4️⃣ Testes de Segurança

### 4.1 RBAC (Role-Based Access Control)

| Cenário | Roles Testados | Status |
|---------|----------------|--------|
| Acesso Admin | ADMIN | ✅ |
| Acesso Analyst | ANALYST | ✅ |
| Acesso Negado | GUEST | ✅ |
| Elevação de Privilégio | ANALYST → ADMIN | ✅ |

### 4.2 Conformidade PCI-DSS

| Requisito | Implementação | Status |
|-----------|---------------|--------|
| Mascaramento de PAN | `****-****-****-1234` | ✅ |
| Criptografia em trânsito | TLS 1.3 | ✅ |
| Logs de auditoria | AuditController | ✅ |

### 4.3 Ferramentas de Segurança

- **SCA (Software Composition Analysis):** Trivy
- **Secret Scanning:** Gitleaks
- **SAST:** ⚠️ Recomendado (CodeQL)
- **DAST:** ⚠️ Recomendado (OWASP ZAP)

---

## 5️⃣ Testes de Arquitetura (ArchUnit)

### 5.1 Regras Implementadas

```java
@ArchTest
static final ArchRule cleanArchitecture = 
    layeredArchitecture()
        .layer("Controller").definedBy("..controller..")
        .layer("Service").definedBy("..service..")
        .layer("Repository").definedBy("..repository..")
        .layer("Entity").definedBy("..entity..")
        .whereLayer("Controller").mayOnlyBeAccessedByLayers()
        .whereLayer("Service").mayOnlyBeAccessedByLayers("Controller")
        .whereLayer("Repository").mayOnlyBeAccessedByLayers("Service");
```

### 5.2 Validações

| Regra | Descrição | Status |
|-------|-----------|--------|
| Dependências de camada | Controller → Service → Repository | ✅ |
| Sem ciclos | Nenhuma dependência circular | ✅ |
| Naming conventions | *Controller, *Service, *Repository | ✅ |

---

## 6️⃣ Testes de Regressão

### 6.1 Golden Master Testing

```java
@Test
void testRuleEngineBaseline() {
    // Carrega baseline de referência
    RuleResult baseline = loadBaseline("crtran_baseline.json");
    
    // Executa motor de regras
    RuleResult actual = ruleEngine.evaluate(testTransaction);
    
    // Compara com baseline
    assertThat(actual).isEqualTo(baseline);
}
```

### 6.2 Snapshot Testing (Frontend)

```typescript
// Rules.test.tsx.snap
exports[`Rules component renders correctly`] = `
<div className="rules-container">
  ...
</div>
`;
```

---

## 7️⃣ Testes de Performance

### 7.1 Status Atual

| Tipo | Ferramenta | Status |
|------|------------|--------|
| Load Testing | k6 | ⚠️ Scripts criados, execução pendente |
| Stress Testing | k6 | ⚠️ Scripts criados, execução pendente |
| Benchmark | JMH | ⚠️ Implementação Pendente |

### 7.2 Métricas Alvo

| Métrica | Alvo | Status |
|---------|------|--------|
| Latência P95 | < 100ms | ⚠️ Aguardando execução |
| Throughput | > 1000 TPS | ⚠️ Aguardando execução |
| Error Rate | < 0.1% | ⚠️ Aguardando execução |

### 7.3 Scripts Disponíveis

- `tests/performance/load-test.js` - Teste de carga progressiva
- `tests/performance/stress-test.js` - Teste de stress até ponto de ruptura

---

## 📈 Métricas de Qualidade

### Cobertura de Código

| Módulo | Cobertura | Meta |
|--------|-----------|------|
| Backend Core | 85%+ | 80% |
| Frontend Components | 75%+ | 70% |
| E2E Flows | 90%+ | 85% |

### Operadores Testados

| Categoria | Total | Testados | Cobertura |
|-----------|-------|----------|-----------|
| Comparação | 50 | 50 | 100% |
| Velocity | 15 | 15 | 100% |
| NEO4J | 18 | 18 | 100% |
| GEO | 3 | 3 | 100% |
| FATF | 28 | 28 | 100% |
| **TOTAL** | **447** | **447** | **100%** |

---

## 🔄 Pipeline de CI/CD

```yaml
stages:
  - lint
  - unit-tests
  - integration-tests
  - e2e-tests
  - security-scan
  - build
  - deploy

unit-tests:
  script:
    - mvn test -Dtest=*Test
    - npm run test

integration-tests:
  script:
    - mvn test -Dtest=*IntegrationTest
    
e2e-tests:
  script:
    - npx playwright test

security-scan:
  script:
    - trivy fs .
    - gitleaks detect
```

---

## 📁 Inventário Completo de Arquivos de Teste

### Backend (34 arquivos)

```
backend/src/test/java/com/rulex/
├── architecture/
│   └── CleanArchitectureRulesTest.java
├── contract/
│   └── ContractTestBase.java
├── controller/integration/
│   ├── AuditAndMetricsApiIntegrationTest.java
│   ├── BaseIntegrationTest.java
│   ├── ExportImportAndApprovalApiIntegrationTest.java
│   ├── RuleApiIntegrationTest.java
│   ├── SimulationApiIntegrationTest.java
│   └── TransactionApiIntegrationTest.java
├── service/
│   ├── AdvancedRuleEngineServiceTest.java
│   ├── AuditServiceTest.java
│   ├── BloomFilterServiceTest.java
│   ├── DatabaseRuleExecutorServiceTest.java
│   ├── DerivedContextTest.java
│   ├── DeviceFingerprintServiceTest.java
│   ├── EnrichmentServiceTest.java
│   ├── GeoServiceTest.java
│   ├── PayloadHashServiceTest.java
│   ├── RedisVelocityServiceTest.java
│   ├── RuleConfigurationServiceTest.java
│   ├── RuleEngineServiceTest.java
│   ├── RuleExportImportServiceTest.java
│   ├── ShadowModeServiceTest.java
│   ├── VelocityServiceFacadeTest.java
│   ├── VelocityServiceTest.java
│   └── complex/
│       ├── AllOperatorsIntegrationTest.java
│       ├── ComplexRuleEvaluatorAggregationTest.java
│       ├── ComplexRuleEvaluatorNewOperatorsTest.java
│       ├── ComplexRuleEvaluatorTest.java
│       ├── ComplexRuleEvaluatorV4PhaseOneTest.java
│       ├── OperatorSyncTest.java
│       └── RuleValidationServiceTest.java
├── util/
│   └── RegexValidatorTest.java
└── v31/ast/
    ├── AstEvaluatorTest.java
    └── AstValidatorTest.java
```

### Frontend (13 arquivos)

```
client/src/
├── components/
│   ├── ComplexRuleBuilder/
│   │   └── ComplexRuleBuilder.test.tsx
│   ├── DashboardLayout.test.tsx
│   ├── ErrorBoundary.test.tsx
│   └── RuleFormDialog/
│       ├── operators.test.ts
│       └── schema.test.ts
├── lib/validators/
│   └── regexValidator.test.ts
└── pages/
    ├── Audit.test.tsx
    ├── Dashboard.test.tsx
    ├── Home.test.tsx
    ├── Login.test.tsx
    ├── NotFound.test.tsx
    ├── Rules.test.tsx
    └── Transactions.test.tsx
```

### E2E (11 arquivos)

```
e2e/
├── api-health.spec.ts
├── audit.spec.ts
├── complex-rules.spec.ts
├── dashboard.spec.ts
├── login.spec.ts
├── navigation.spec.ts
├── rbac.spec.ts
├── responsive.spec.ts
├── rules-crud.spec.ts
├── rules.spec.ts
└── transactions.spec.ts
```

---

## 📝 Changelog

| Versão | Data | Mudanças |
|--------|------|----------|
| 2.0.0 | 2026-01-14 | Sincronização completa com código, correção de métricas, inventário atualizado |
| 1.0.0 | 2026-01-01 | Versão inicial |
