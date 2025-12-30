# QA SYSTEM BLUEPRINT - RULEX

**Data**: 2024-12-29  
**Branch**: cursor/rulex-project-review-1c58  
**Status**: INVENTÁRIO FORENSE COMPLETO

---

## 1. ARQUITETURA DO SISTEMA

### 1.1 Visão Geral

```
┌─────────────────────────────────────────────────────────────────────────┐
│                           RULEX SYSTEM                                   │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  ┌──────────────┐     ┌──────────────┐     ┌──────────────────────────┐ │
│  │   Frontend   │────▶│   Backend    │────▶│      PostgreSQL          │ │
│  │  React 19    │     │  Spring Boot │     │      16-alpine           │ │
│  │  Vite 7.x    │     │  Java 21     │     │      Flyway 11.20        │ │
│  │  Port: 5173  │     │  Port: 8080  │     │      Port: 5432          │ │
│  └──────────────┘     └──────────────┘     └──────────────────────────┘ │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### 1.2 Stack Tecnológico

| Camada | Tecnologia | Versão | Evidência |
|--------|------------|--------|-----------|
| **Frontend** | React | 19.x | `package.json` |
| **Frontend** | Vite | 7.1.7 | `package.json` |
| **Frontend** | TypeScript | 5.9.3 | `package.json` |
| **Frontend** | Tailwind CSS | 4.x | `package.json` |
| **Backend** | Java | 21 | `pom.xml` |
| **Backend** | Spring Boot | 3.5.9 | `pom.xml` |
| **Backend** | Flyway | 11.20.0 | `pom.xml` |
| **Database** | PostgreSQL | 16-alpine | `docker-compose.yml` |
| **ORM** | Hibernate/JPA | (via Spring) | `pom.xml` |

### 1.3 Estrutura de Diretórios

```
RULEX/
├── backend/                    # Java Spring Boot
│   ├── src/main/java/com/rulex/
│   │   ├── homolog/           # Módulo de homologação
│   │   │   ├── adapter/       # Adapters (ports & adapters)
│   │   │   ├── config/        # Configurações
│   │   │   ├── port/          # Interfaces de portas
│   │   │   └── usecase/       # Casos de uso
│   │   ├── v31/               # Versão 3.1 do motor
│   │   │   ├── ast/           # AST para DSL de regras
│   │   │   └── execlog/       # Log de execução
│   │   ├── controller/        # REST Controllers
│   │   ├── service/           # Serviços de negócio
│   │   ├── repository/        # Repositórios JPA
│   │   ├── entity/            # Entidades JPA
│   │   ├── dto/               # DTOs
│   │   └── config/            # Configurações Spring
│   └── src/test/java/         # Testes backend
├── client/                     # Frontend React
│   └── src/
│       ├── components/        # Componentes React
│       ├── pages/             # Páginas
│       ├── hooks/             # Custom hooks
│       └── lib/               # Utilitários
├── e2e/                        # Testes E2E Playwright
├── openapi/                    # Especificação OpenAPI
├── docs/                       # Documentação
└── .github/workflows/          # CI/CD
```

---

## 2. SERVIÇOS DOCKER COMPOSE

### 2.1 Serviços Definidos

| Serviço | Imagem | Porta | Healthcheck | Dependências |
|---------|--------|-------|-------------|--------------|
| `postgres` | postgres:16-alpine | 5432 | `pg_isready` | - |
| `backend` | ./backend/Dockerfile | 8080 | - | postgres (healthy) |
| `web` | ./Dockerfile.web | 5173 | - | backend |

### 2.2 Variáveis de Ambiente

```yaml
# PostgreSQL
POSTGRES_DB: rulex_db
POSTGRES_USER: postgres
POSTGRES_PASSWORD: postgres

# Backend
SPRING_PROFILES_ACTIVE: dev
SPRING_DATASOURCE_URL: jdbc:postgresql://postgres:5432/rulex_db
RULEX_SECURITY_ENABLED: true
RULEX_ADMIN_USERNAME: admin
RULEX_ADMIN_PASSWORD: rulex
RULEX_ANALYST_USERNAME: analyst
RULEX_ANALYST_PASSWORD: rulex

# Frontend
VITE_API_PROXY_TARGET: http://backend:8080
VITE_API_BASIC_AUTH: admin:rulex
```

---

## 3. CONFIGURAÇÃO DO BACKEND

### 3.1 Base Path

- **Context Path**: `/api`
- **Porta**: 8080
- **Profiles**: dev, prod

### 3.2 Endpoints Principais (OpenAPI)

| Método | Endpoint | Descrição |
|--------|----------|-----------|
| POST | `/api/transactions/analyze` | Análise de transação |
| POST | `/api/transactions/analyze-advanced` | Análise avançada |
| GET | `/api/transactions` | Listar transações |
| GET | `/api/transactions/{id}` | Buscar transação |
| GET | `/api/rules` | Listar regras |
| GET | `/api/rules/{id}` | Buscar regra |
| GET | `/api/audit/decisions` | Log de decisões |
| GET | `/api/audit/export` | Exportar auditoria |

### 3.3 Segurança

- **Tipo**: HTTP Basic Authentication
- **Roles**: ADMIN, ANALYST
- **Configurável**: via variáveis de ambiente

---

## 4. MIGRAÇÕES FLYWAY

### 4.1 Migrações Existentes

| Versão | Arquivo | Descrição |
|--------|---------|-----------|
| V1 | `V1__init.sql` | Schema inicial |
| V2 | `V2__core_schema.sql` | Schema core |
| V3 | `V3__extend_workflow_length.sql` | Extensão workflow |
| V4 | `V4__raw_hash_idempotency.sql` | Idempotência hash |
| V5 | `V5__raw_as_received.sql` | Raw as received |
| V6 | `V6__v31_exec_log_field_dictionary.sql` | Dicionário de campos |
| V7 | `V7__v31_exec_log_dedup.sql` | Deduplicação |

### 4.2 Estratégia

- **DDL-Auto**: validate (não cria/altera schema)
- **Locations**: classpath:db/migration
- **Baseline**: não configurado

---

## 5. FERRAMENTAS DE TESTE EXISTENTES

### 5.1 Backend (Java)

| Ferramenta | Uso | Evidência |
|------------|-----|-----------|
| JUnit 5 | Unit/Integration | `pom.xml` |
| Testcontainers | DB Integration | `pom.xml` |
| JaCoCo | Coverage | `pom.xml` (profile) |
| ArchUnit | Architecture | `pom.xml` |
| Spring Boot Test | Integration | `pom.xml` |

### 5.2 Frontend (TypeScript)

| Ferramenta | Uso | Evidência |
|------------|-----|-----------|
| Vitest | Unit tests | `vitest.config.ts` |
| Testing Library | Component tests | `package.json` |
| Playwright | E2E | `playwright.config.ts` |

### 5.3 CI/CD

| Scanner | Tipo | Evidência |
|---------|------|-----------|
| Gitleaks | Secret scanning | `.github/workflows/ci.yml` |
| Trivy | SCA/Vulnerability | `.github/workflows/ci.yml` |

---

## 6. TESTES EXISTENTES

### 6.1 Backend Tests

| Arquivo | Tipo | Descrição |
|---------|------|-----------|
| `RuleEngineServiceTest.java` | Unit | Testes do motor de regras |
| `AdvancedRuleEngineServiceTest.java` | Unit | Testes avançados |
| `AstEvaluatorTest.java` | Unit | Avaliador AST |
| `AstValidatorTest.java` | Unit | Validador AST |
| `TransactionAnalyzeIT.java` | Integration | API de análise |
| `CrtranBaselineIT.java` | Integration | Baseline CRTRAN |
| `RulePopupE2EIT.java` | Integration | Popup de regras |
| `HomologSimulationIT.java` | Integration | Simulação homolog |
| `RuleExecutionLogIT.java` | Integration | Log de execução |
| `FlywayMigrationsIT.java` | Integration | Migrações |
| `SecurityRbacIT.java` | Integration | Segurança RBAC |
| `CleanArchitectureRulesTest.java` | Architecture | Regras arquiteturais |
| `CorePostgresITSupport.java` | Support | Suporte Testcontainers |

### 6.2 Frontend Tests

| Arquivo | Tipo | Descrição |
|---------|------|-----------|
| `Rules.test.tsx` | Component | Testes da página Rules |
| `Rules.test.tsx.snap` | Snapshot | Snapshot da página |

### 6.3 E2E Tests

| Arquivo | Tipo | Descrição |
|---------|------|-----------|
| `login.spec.ts` | E2E | Teste de login |

---

## 7. PONTOS DE TESTE IDENTIFICADOS

### 7.1 Críticos (P0)

1. **Motor de Regras**: 28 regras duras implementadas
2. **Idempotência**: Hash de payload para evitar duplicatas
3. **Segurança**: RBAC com Basic Auth
4. **Migrações**: Flyway com PostgreSQL 16

### 7.2 Altos (P1)

1. **API REST**: Endpoints de análise e auditoria
2. **Validação**: Payload de entrada
3. **Logging**: Decisões de regras
4. **Performance**: Throughput de análise

### 7.3 Médios (P2)

1. **Frontend**: Componentes React
2. **Integração**: Frontend ↔ Backend
3. **Exportação**: CSV/JSON de auditoria

---

## 8. GAPS IDENTIFICADOS

### 8.1 Cobertura de Testes

| Área | Status | Gap |
|------|--------|-----|
| Backend Unit | ✅ Existe | Verificar cobertura |
| Backend Integration | ✅ Existe | Verificar cobertura |
| Frontend Unit | ⚠️ Parcial | Apenas 1 teste |
| E2E | ⚠️ Parcial | Apenas login |
| Performance | ❌ Ausente | Não existe |
| Security DAST | ❌ Ausente | Não existe |
| Mutation Testing | ❌ Ausente | Não existe |

### 8.2 CI/CD

| Item | Status | Gap |
|------|--------|-----|
| Unit Tests | ✅ | - |
| Integration Tests | ✅ | - |
| E2E Tests | ✅ | - |
| Coverage Gates | ⚠️ | Sem thresholds |
| SAST | ❌ | Não existe |
| SBOM | ❌ | Não existe |

---

## 9. PRÓXIMOS PASSOS

1. Executar testes existentes e coletar evidências
2. Implementar testes faltantes
3. Configurar gates de qualidade
4. Gerar relatório GO/NO-GO

---

**Documento gerado automaticamente pelo QA Military Mode**
