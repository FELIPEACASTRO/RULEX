# RULEX - Mapa de Arquitetura Completo

**Versão:** 1.0.0  
**Data:** 2025-01-03  
**Tipo:** Motor de Regras Duras (Hard Rules) para Prevenção a Fraudes

---

## 1. Visão Geral da Solução

O RULEX é um **motor de regras determinísticas** para detecção de fraude em transações de cartão de crédito. **NÃO é um sistema de Machine Learning** - todas as decisões são baseadas em regras explícitas, configuráveis e auditáveis.

### 1.1 Stack Tecnológico

| Camada | Tecnologia | Versão |
|--------|------------|--------|
| Backend | Java + Spring Boot | 21 / 3.5.9 |
| Frontend | React + TypeScript + Vite | 18.x / 5.x |
| Banco de Dados | PostgreSQL | 16.x |
| Migrations | Flyway | 11.20.0 |
| Testes | JUnit 5 + Testcontainers + Playwright | - |
| Containerização | Docker Compose | - |

### 1.2 Estrutura de Diretórios

```
RULEX/
├── backend/                    # API Java Spring Boot
│   ├── src/main/java/com/rulex/
│   │   ├── api/               # DTOs de API
│   │   ├── config/            # Configurações Spring
│   │   ├── controller/        # REST Controllers
│   │   │   ├── complex/       # Regras complexas
│   │   │   └── homolog/       # Endpoints de homologação
│   │   ├── dto/               # Data Transfer Objects
│   │   │   ├── complex/       # DTOs de regras complexas
│   │   │   └── homolog/       # DTOs de homologação
│   │   ├── entity/            # Entidades JPA
│   │   │   ├── complex/       # Entidades de regras complexas
│   │   │   └── homolog/       # Entidades de homologação
│   │   ├── homolog/           # Módulo de homologação (Hexagonal)
│   │   │   ├── adapter/       # Adapters (implementações)
│   │   │   ├── application/   # Application Services
│   │   │   ├── config/        # Configurações
│   │   │   ├── port/          # Ports (interfaces)
│   │   │   └── usecase/       # Use Cases
│   │   ├── repository/        # Repositórios JPA
│   │   ├── service/           # Serviços de negócio
│   │   │   └── complex/       # Serviços de regras complexas
│   │   ├── util/              # Utilitários
│   │   └── v31/               # Motor V3.1 (AST + Field Dictionary)
│   │       ├── ast/           # Avaliador AST
│   │       ├── execlog/       # Log de execução
│   │       ├── field/         # Dicionário de campos
│   │       └── rules/         # Regras V3.1
│   └── src/main/resources/
│       └── db/migration/      # Migrations Flyway (V1-V22)
├── client/                    # Frontend React
│   └── src/
│       ├── components/        # Componentes React
│       │   ├── ComplexRuleBuilder/  # Builder de regras complexas
│       │   └── ui/            # Componentes UI (shadcn)
│       ├── pages/             # Páginas da aplicação
│       ├── lib/               # Utilitários e API client
│       └── hooks/             # React Hooks
├── e2e/                       # Testes E2E (Playwright)
├── docs/                      # Documentação
│   ├── qa/                    # Documentação de QA
│   ├── rules/                 # Documentação de regras
│   └── benchmark_research/    # Pesquisa de benchmarks
└── docker-compose.yml         # Orquestração de containers
```

---

## 2. Arquitetura do Motor de Regras

### 2.1 Tipos de Regras

O RULEX suporta **dois tipos de regras**:

#### 2.1.1 Regras Simples (`rule_configurations`)
- Armazenadas na tabela `rule_configurations`
- Condições em JSON (`conditions_json`)
- Operador lógico único (AND/OR)
- Ideal para regras diretas e rápidas

#### 2.1.2 Regras Complexas (`complex_rules` + `rule_condition_groups` + `rule_conditions`)
- Estrutura hierárquica com grupos aninhados
- Suporta até 10 níveis de profundidade
- Operadores lógicos avançados (AND, OR, NOT, XOR, NAND, NOR)
- Suporta 50+ operadores de comparação
- Ideal para regras sofisticadas

### 2.2 Fluxo de Avaliação

```
┌─────────────────┐
│  TransactionRequest  │
│  (Payload JSON)      │
└─────────┬───────────┘
          │
          ▼
┌─────────────────────────────────────────────────────────┐
│                    RULE ENGINE                           │
│  ┌─────────────────┐    ┌─────────────────────────────┐ │
│  │ RuleEngineService│    │ ComplexRuleExecutionService │ │
│  │ (Regras Simples) │    │ (Regras Complexas)          │ │
│  └────────┬────────┘    └────────────┬────────────────┘ │
│           │                          │                   │
│           ▼                          ▼                   │
│  ┌─────────────────────────────────────────────────────┐│
│  │              ComplexRuleEvaluator                   ││
│  │  - Avalia grupos de condições recursivamente        ││
│  │  - Aplica operadores lógicos                        ││
│  │  - Executa operadores de comparação                 ││
│  └─────────────────────────────────────────────────────┘│
│           │                                              │
│           ▼                                              │
│  ┌─────────────────┐  ┌─────────────────┐               │
│  │  VelocityService │  │   GeoService    │               │
│  │  (Agregações)    │  │  (Geolocalização)│              │
│  └─────────────────┘  └─────────────────┘               │
└─────────────────────────────────────────────────────────┘
          │
          ▼
┌─────────────────────────────────────────────────────────┐
│                      DECISÃO                             │
│  - APROVADO (APPROVED)                                   │
│  - SUSPEITA_DE_FRAUDE (SUSPICIOUS)                       │
│  - FRAUDE (FRAUD)                                        │
│  + Risk Score (0-100)                                    │
│  + Regras disparadas                                     │
│  + Explicação detalhada                                  │
└─────────────────────────────────────────────────────────┘
          │
          ▼
┌─────────────────────────────────────────────────────────┐
│                   PERSISTÊNCIA                           │
│  - decision_log (decisões)                               │
│  - audit_log (auditoria)                                 │
│  - rule_execution_details (detalhes de execução)         │
│  - velocity_transaction_log (log para agregações)        │
└─────────────────────────────────────────────────────────┘
```

### 2.3 Componentes Principais

| Componente | Responsabilidade |
|------------|------------------|
| `ComplexRuleEvaluator` | Avalia grupos de condições aninhados |
| `VelocityService` | Calcula agregações temporais (COUNT, SUM, AVG) |
| `GeoService` | Operações geográficas (distância, polígonos) |
| `AstEvaluator` | Avaliador AST para regras V3.1 |
| `RuleValidationService` | Valida regras antes de salvar |
| `AuditService` | Registra todas as ações para compliance |

---

## 3. Endpoints da API

### 3.1 Regras Simples
| Método | Endpoint | Descrição |
|--------|----------|-----------|
| GET | `/api/rules` | Lista todas as regras |
| POST | `/api/rules` | Cria nova regra |
| PUT | `/api/rules/{id}` | Atualiza regra |
| DELETE | `/api/rules/{id}` | Remove regra |
| PATCH | `/api/rules/{id}/toggle` | Ativa/desativa regra |
| GET | `/api/rules/enabled/{enabled}` | Lista regras por status |

### 3.2 Regras Complexas
| Método | Endpoint | Descrição |
|--------|----------|-----------|
| GET | `/api/complex-rules` | Lista regras complexas |
| GET | `/api/complex-rules/{id}` | Busca regra por ID |
| GET | `/api/complex-rules/key/{key}` | Busca regra por chave |
| POST | `/api/complex-rules` | Cria regra complexa |
| PUT | `/api/complex-rules/{id}` | Atualiza regra |
| DELETE | `/api/complex-rules/{id}` | Remove regra |
| POST | `/api/complex-rules/validate` | Valida sem salvar |
| POST | `/api/complex-rules/{id}/duplicate` | Duplica regra |

### 3.3 Avaliação de Transações
| Método | Endpoint | Descrição |
|--------|----------|-----------|
| POST | `/api/evaluate` | Avalia transação |
| POST | `/api/transactions/analyze` | Analisa transação |
| POST | `/api/homolog/simulations/run` | Executa simulação |

### 3.4 Homologação (Versionamento)
| Método | Endpoint | Descrição |
|--------|----------|-----------|
| POST | `/api/homolog/rules` | Cria regra + versão DRAFT |
| POST | `/api/homolog/rules/versions/{id}/publish` | Publica versão |
| POST | `/api/homolog/rules/{id}/rollback/{version}` | Rollback |
| POST | `/api/homolog/rulesets` | Cria ruleset |
| POST | `/api/homolog/rulesets/activate` | Ativa ruleset |

---

## 4. Modelo de Dados (Resumo)

### 4.1 Tabelas Principais

| Tabela | Descrição |
|--------|-----------|
| `rule_configurations` | Regras simples |
| `complex_rules` | Regras complexas (header) |
| `rule_condition_groups` | Grupos de condições (aninhados) |
| `rule_conditions` | Condições individuais |
| `rules` | Regras versionadas (header) |
| `rule_versions` | Versões de regras |
| `rule_sets` | Conjuntos de regras |
| `rule_set_versions` | Versões de conjuntos |
| `active_rule_set` | Conjunto ativo (singleton) |

### 4.2 Tabelas de Suporte

| Tabela | Descrição |
|--------|-----------|
| `velocity_counters` | Contadores pré-computados |
| `velocity_transaction_log` | Log para agregações |
| `geo_reference` | Referências geográficas |
| `geo_polygon` | Polígonos geográficos |
| `bin_lookup` | Lookup de BINs |
| `mcc_categories` | Categorias MCC |

### 4.3 Tabelas de Auditoria

| Tabela | Descrição |
|--------|-----------|
| `decision_log` | Log de decisões |
| `audit_log` | Log de auditoria |
| `rule_execution_details` | Detalhes de execução |
| `rule_configuration_history` | Histórico de alterações |

---

## 5. Decisões de Arquitetura

### 5.1 Por que Regras Duras (não ML)?

1. **Auditabilidade**: Cada decisão pode ser explicada deterministicamente
2. **Compliance**: Reguladores exigem explicabilidade (LGPD, BACEN)
3. **Controle**: Analistas podem ajustar regras em tempo real
4. **Previsibilidade**: Comportamento consistente e testável
5. **Performance**: Avaliação em milissegundos

### 5.2 Arquitetura Hexagonal (Módulo Homolog)

O módulo de homologação segue arquitetura hexagonal:
- **Ports**: Interfaces que definem contratos
- **Adapters**: Implementações concretas
- **Use Cases**: Lógica de negócio pura
- **Application Services**: Orquestração

### 5.3 Versionamento de Regras

- Cada regra tem múltiplas versões
- Status: DRAFT → PUBLISHED → DEPRECATED
- Rollback cria nova versão DRAFT
- Apenas versões PUBLISHED podem ser ativadas

---

## 6. Integrações

### 6.1 Internas
- **PostgreSQL**: Persistência principal
- **Flyway**: Migrations automáticas
- **Testcontainers**: Testes de integração

### 6.2 Observabilidade
- **Micrometer + Prometheus**: Métricas
- **OpenTelemetry**: Tracing distribuído
- **Actuator**: Health checks

### 6.3 Segurança
- **Spring Security**: Autenticação HTTP Basic
- **RBAC**: Roles (ADMIN, ANALYST)
- **Bucket4j**: Rate limiting

---

## 7. Referências

- [README.md](../README.md) - Visão geral do projeto
- [EXTREME_CAPABILITIES_MAP.md](./EXTREME_CAPABILITIES_MAP.md) - Capacidades do motor
- [RULES_SCHEMA_AND_FIELDS.md](./RULES_SCHEMA_AND_FIELDS.md) - Schema de regras
- [Backend README](../backend/README.md) - Detalhes técnicos do backend
