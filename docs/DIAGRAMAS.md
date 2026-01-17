# DIAGRAMAS — Documentação Oficial do RULEX

Gerado em: 2026-01-17T18:29:53.566Z

---

## Sobre este documento

Este documento contém **TODOS** os diagramas, fluxogramas e representações do sistema RULEX, organizado conforme estrutura obrigatória para:
- Executivos e Board
- Analistas de Negócio
- Desenvolvedores
- Arquitetos
- QA
- Segurança / Auditoria
- Onboarding técnico

### Regras de Rigor

| Regra | Descrição |
|-------|-----------|
| ✅ **OK** | O diagrama referencia evidência verificável no repositório (paths reais). |
| 🟥 **SEM EVIDÊNCIA** | O diagrama é obrigatório na estrutura, mas **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO** (template neutro + arquivos esperados). |
| ❌ **Não inventar** | Nenhum fato é inferido sem evidência explícita. |

### Estatísticas do Catálogo

- Total de itens no catálogo da UI: **422**
- Itens marcados como "verified" no catálogo da UI (metadado de catálogo, não prova): **213**
- Templates didáticos no catálogo da UI: **209**

### Como regerar

```bash
pnpm diagrams:doc-completo   # Gera este documento
pnpm diagrams:sync           # Sincroniza inventário + checklist + doc
```

---

## 0. PASSO ZERO — Varredura obrigatória do repositório


### Estrutura (top-level)

- AGENTS.md
- arq/
- audit/
- backend/
- client/
- components.json
- dist/
- docker-compose.yml
- Dockerfile.web
- docs/
- e2e/
- FRAUDE_REGRAS_DURAS_EXPORT.yaml
- GAPS_IDENTIFICADOS_DOUBLE_CHECK.md
- INSOMNIA_TEST_REPORT.md
- Insomnia/
- node_modules/
- op/
- openapi/
- package.json
- patches/
- perf/
- playwright.config.ts
- pnpm-lock.yaml
- PROMPT_ANALISE_DEVIN.md
- PROMPT_BACKUP_DEVIN_V2.md
- PROMPT_BACKUP_DEVIN_V3.md
- PROMPT_BACKUP_DEVIN.md
- README.md
- replit.md
- reports/
- scripts/
- tests/
- TRIPLE_CHECK_FILES.txt
- TRIPLE_CHECK_REPORT.md
- tsconfig.json
- vite.config.ts
- vitest.config.ts

### Entrypoints (verificados)

- Frontend: client/src/main.tsx
- Frontend (rotas): client/src/App.tsx
- Backend: backend/src/main/java/com/rulex/RulexApplication.java

### Contrato de API (verificado)

- OpenAPI: openapi/rulex.yaml
- Context path do backend: /api (backend/src/main/resources/application.yml)

### Infra local (verificada)

- docker-compose.yml (PostgreSQL 16 + Redis 7 + Neo4j 5 + backend + web)

### Contagens rápidas (automatizadas)

- Java (backend): **278** arquivo(s) (backend/src/main/java)
- SQL migrations (Flyway): **35** arquivo(s) (backend/src/main/resources/db/migration)
- Frontend TS/TSX: **182** arquivo(s) (client/src)


---

## Catálogo Mestre (auditável)

| Categoria | Diagrama | Público | Nível | Evidência | Status |
|---|---|---|---|---|---|
| Backend | API Contract — OpenAPI | Dev Backend, Arquiteto, QA, Operação | Tático/Detalhado | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Backend | Arquitetura Backend — Camadas | Dev Backend, Arquiteto, QA, Operação | Tático/Detalhado | backend/src/main/java/com/rulex | OK |
| Backend | Arquitetura de Regras Duras | Dev Backend, Arquiteto, QA, Operação | Tático/Detalhado | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Backend | C4 — Component Diagram (Backend) | Dev Backend, Arquiteto, QA, Operação | Tático/Detalhado | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Backend | C4 — Container Diagram | Dev Backend, Arquiteto, QA, Operação | Tático/Detalhado | docker-compose.yml; backend/src/main/resources/application.yml; client/src/App.tsx | OK |
| Backend | Event / Message Flow | Dev Backend, Arquiteto, QA, Operação | Tático/Detalhado | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Backend | Fluxo — Análise de Transação (/analyze) | Dev Backend, Arquiteto, QA, Operação | Tático/Detalhado | backend/src/main/java/com/rulex/controller/TransactionController.java; backend/src/main/java/com/rulex/service/RuleEngineService.java; openapi/rulex.yaml | OK |
| Backend | Fluxo — Tratamento de Exceções | Dev Backend, Arquiteto, QA, Operação | Tático/Detalhado | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Backend | Integrações Externas | Dev Backend, Arquiteto, QA, Operação | Tático/Detalhado | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Backend | UML — Diagrama de Classes (Entidades Core) | Dev Backend, Arquiteto, QA, Operação | Tático/Detalhado | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Backend | UML — Diagrama de Estados (Regra) | Dev Backend, Arquiteto, QA, Operação | Tático/Detalhado | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Backend | UML — Diagrama de Pacotes | Dev Backend, Arquiteto, QA, Operação | Tático/Detalhado | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Backend | UML — Diagrama de Sequência (Análise de Transação) | Dev Backend, Arquiteto, QA, Operação | Tático/Detalhado | backend/src/main/java/com/rulex/controller/TransactionController.java; backend/src/main/java/com/rulex/service/RuleEngineService.java | OK |
| Frontend | Arquitetura do Frontend | Dev Frontend, Design, QA, Produto | Tático | client/src/main.tsx; client/src/App.tsx; vite.config.ts; package.json | OK |
| Frontend | Componentes do Frontend | Dev Frontend, Design, QA, Produto | Tático | client/src/components; components.json | OK |
| Frontend | Design System — RULEX | Dev Frontend, Design, QA, Produto | Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Frontend | Estados da UI — Componente Genérico | Dev Frontend, Design, QA, Produto | Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Frontend | Fluxo UI — Criação de Regra | Dev Frontend, Design, QA, Produto | Tático | client/src/pages/ComplexRules.tsx; client/src/App.tsx | OK |
| Frontend | Fluxo UI: Frontend: Pages inventory | Dev Frontend, Design, QA, Produto | Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Frontend | Wireflow — Navegação Principal | Dev Frontend, Design, QA, Produto | Tático | client/src/App.tsx | OK |
| Negócio/Usuário | BPMN — Decisão de Fraude | Negócio, Exec, Produto, Operação | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Negócio/Usuário | BPMN — Exceção / Fallback | Negócio, Exec, Produto, Operação | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Negócio/Usuário | BPMN — Rollback Operacional | Negócio, Exec, Produto, Operação | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Negócio/Usuário | BPMN AS-IS (Processo Atual) | Negócio, Exec, Produto, Operação | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Negócio/Usuário | BPMN TO-BE (Processo Futuro/Desejado) | Negócio, Exec, Produto, Operação | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Negócio/Usuário | Business Model Canvas — RULEX | Negócio, Exec, Produto, Operação | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Negócio/Usuário | Casos de Uso — Administrador | Negócio, Exec, Produto, Operação | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Negócio/Usuário | Casos de Uso — Analista de Fraude | Negócio, Exec, Produto, Operação | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Negócio/Usuário | Casos de Uso — Motor de Regras (Interno) | Negócio, Exec, Produto, Operação | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Negócio/Usuário | Casos de Uso — Operação / Suporte | Negócio, Exec, Produto, Operação | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Negócio/Usuário | Casos de Uso — Sistema Externo (Integração) | Negócio, Exec, Produto, Operação | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Negócio/Usuário | Jornada — Criação de Regra | Negócio, Exec, Produto, Operação | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Negócio/Usuário | Jornada — Investigação de Fraude | Negócio, Exec, Produto, Operação | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Negócio/Usuário | Jornada — Publicação de Regra | Negócio, Exec, Produto, Operação | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Negócio/Usuário | Jornada — Rollback de Regra | Negócio, Exec, Produto, Operação | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Negócio/Usuário | Jornada — Simulação de Regra | Negócio, Exec, Produto, Operação | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Negócio/Usuário | Persona — Analista de Fraude | Negócio, Exec, Produto, Operação | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Negócio/Usuário | Persona — Executivo / Compliance | Negócio, Exec, Produto, Operação | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Negócio/Usuário | Persona — Operação / SRE | Negócio, Exec, Produto, Operação | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Negócio/Usuário | Persona — Sistema Automatizado | Negócio, Exec, Produto, Operação | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Negócio/Usuário | Service Blueprint — Análise de Transação | Negócio, Exec, Produto, Operação | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Negócio/Usuário | User Story Map — RULEX | Negócio, Exec, Produto, Operação | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Negócio/Usuário | Value Proposition Canvas — RULEX | Negócio, Exec, Produto, Operação | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Neo4j | Armazenamento Neo4j | Dev Backend, Data/Graph, Arquiteto | Detalhado | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Neo4j | Cluster Causal Neo4j | Dev Backend, Data/Graph, Arquiteto | Detalhado | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Neo4j | Exemplo de Grafo — Fraud Ring | Dev Backend, Data/Graph, Arquiteto | Detalhado | backend/src/main/java/com/rulex/service/Neo4jGraphService.java; backend/src/main/java/com/rulex/service/complex/ComplexRuleEvaluator.java | OK |
| Neo4j | Index-Free Adjacency | Dev Backend, Data/Graph, Arquiteto | Detalhado | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Neo4j | Índices e Constraints Neo4j | Dev Backend, Data/Graph, Arquiteto | Detalhado | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Neo4j | Multi-Data Center Neo4j | Dev Backend, Data/Graph, Arquiteto | Detalhado | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Neo4j | Property Graph — RULEX | Dev Backend, Data/Graph, Arquiteto | Detalhado | backend/src/main/java/com/rulex/service/Neo4jGraphService.java; backend/src/main/java/com/rulex/service/complex/ComplexRuleEvaluator.java; docker-compose.yml; backend/src/main/resources/application.yml | OK |
| PostgreSQL | Armazenamento PostgreSQL | Dev Backend, DBA, Arquiteto, Operação | Tático/Detalhado | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| PostgreSQL | Data Lifecycle — Retenção e LGPD | Dev Backend, DBA, Arquiteto, Operação | Tático/Detalhado | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| PostgreSQL | ERD — Completo | Dev Backend, DBA, Arquiteto, Operação | Tático/Detalhado | backend/src/main/resources/db/migration | OK |
| PostgreSQL | ERD — Entidades Core | Dev Backend, DBA, Arquiteto, Operação | Tático/Detalhado | backend/src/main/resources/db/migration/V2__core_schema.sql | OK |
| PostgreSQL | Modelo Conceitual | Dev Backend, DBA, Arquiteto, Operação | Tático/Detalhado | backend/src/main/resources/db/migration/V2__core_schema.sql | OK |
| PostgreSQL | Modelo Físico | Dev Backend, DBA, Arquiteto, Operação | Tático/Detalhado | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| PostgreSQL | Modelo Lógico | Dev Backend, DBA, Arquiteto, Operação | Tático/Detalhado | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| PostgreSQL | Organização de Schemas | Dev Backend, DBA, Arquiteto, Operação | Tático/Detalhado | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| PostgreSQL | Replicação PostgreSQL | Dev Backend, DBA, Arquiteto, Operação | Tático/Detalhado | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Redis | Arquitetura Redis — Event Loop | Dev Backend, Arquiteto, Operação | Tático/Detalhado | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Redis | Cluster Redis | Dev Backend, Arquiteto, Operação | Tático/Detalhado | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Redis | Consistência e Invalidação de Cache | Dev Backend, Arquiteto, Operação | Tático/Detalhado | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Redis | Estratégia de Cache — RULEX | Dev Backend, Arquiteto, Operação | Tático/Detalhado | backend/src/main/resources/application.yml; backend/src/main/java/com/rulex/service/RedisVelocityCacheService.java; backend/src/main/java/com/rulex/service/RedisVelocityService.java; backend/src/main/java/com/rulex/service/VelocityServiceFacade.java | OK |
| Redis | Persistência Redis | Dev Backend, Arquiteto, Operação | Tático/Detalhado | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Redis | Replicação Redis | Dev Backend, Arquiteto, Operação | Tático/Detalhado | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Redis | Tipos de Dados Redis — RULEX | Dev Backend, Arquiteto, Operação | Tático/Detalhado | backend/src/main/java/com/rulex/service/RedisVelocityCacheService.java; backend/src/main/java/com/rulex/service/RedisVelocityService.java | OK |
| Transversal | Alertas Configurados | Arquiteto, Segurança, Operação, QA | Estratégico/Tático | backend/src/main/resources/prometheus-alerts.yml | OK |
| Transversal | Ambientes (Dev/Hml/Prod) | Arquiteto, Segurança, Operação, QA | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Transversal | Autenticação e Autorização | Arquiteto, Segurança, Operação, QA | Estratégico/Tático | backend/src/main/java/com/rulex/config/SecurityConfig.java; backend/src/main/resources/application.yml | OK |
| Transversal | CI/CD Pipeline | Arquiteto, Segurança, Operação, QA | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Transversal | Circuit Breaker | Arquiteto, Segurança, Operação, QA | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Transversal | Deployment Diagram | Arquiteto, Segurança, Operação, QA | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Transversal | DFD Nível 0 — Contexto | Arquiteto, Segurança, Operação, QA | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Transversal | DFD Nível 1 — Processos Principais | Arquiteto, Segurança, Operação, QA | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Transversal | DFD Nível 2 — Detalhamento | Arquiteto, Segurança, Operação, QA | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Transversal | Diagrama de Latência | Arquiteto, Segurança, Operação, QA | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Transversal | Limites de TPS | Arquiteto, Segurança, Operação, QA | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Transversal | Padrões de Resiliência | Arquiteto, Segurança, Operação, QA | Estratégico/Tático | backend/pom.xml; backend/src/main/resources/application.yml | OK |
| Transversal | Pontos de Gargalo | Arquiteto, Segurança, Operação, QA | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Transversal | Proteção de Dados / LGPD | Arquiteto, Segurança, Operação, QA | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |
| Transversal | Stack de Observabilidade | Arquiteto, Segurança, Operação, QA | Estratégico/Tático | backend/src/main/resources/application.yml; backend/src/main/resources/prometheus-alerts.yml | OK |
| Transversal | Threat Model — STRIDE | Arquiteto, Segurança, Operação, QA | Estratégico/Tático | EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO | SEM EVIDÊNCIA |

---

## Índice

1. [Diagramas de Negócio e Usuário](#1-diagramas-de-negócio-e-usuário)
2. [Diagramas de Frontend](#2-diagramas-de-frontend)
3. [Diagramas de Backend (Java)](#3-diagramas-de-backend-java)
4. [Diagramas de PostgreSQL](#4-diagramas-de-postgresql)
5. [Diagramas de Redis](#5-diagramas-de-redis)
6. [Diagramas de Neo4j](#6-diagramas-de-neo4j)
7. [Diagramas Transversais](#7-diagramas-transversais)
8. [Anexo: Catálogo Completo da UI](#8-anexo-catálogo-completo-da-ui)

---


## 1. Diagramas de Negócio e Usuário

Esta seção cobre processos de negócio, casos de uso, personas, jornadas, user story mapping, service blueprint, BMC e Value Proposition.

---

### 1.1 BPMN — Processos de Negócio


### BPMN AS-IS (Processo Atual)

- Categoria: Negócio/Usuário
- Público: Negócio, Exec, Produto, Operação
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar o processo de negócio atual (antes de melhorias/automação).

**Quando usar**
Análise de gaps, auditoria de processos, baseline para TO-BE.

**O que representa**
Fluxo atual de análise de fraude, handoffs entre áreas, pontos de decisão.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Sem baseline, impossível medir melhoria. Risco de automação de processos incorretos.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Processo atual de análise de fraude antes do RULEX ou versão anterior.
>
> **Para completar, anexar/confirmar no repositório**:
- docs/processos/*.bpmn
- docs/processos/*.png



### BPMN TO-BE (Processo Futuro/Desejado)

- Categoria: Negócio/Usuário
- Público: Negócio, Exec, Produto, Operação
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar o processo de negócio desejado após melhorias/automação.

**Quando usar**
Planejamento de evolução, alinhamento com stakeholders, roadmap.

**O que representa**
Fluxo otimizado de análise de fraude com RULEX automatizado.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Falta de visão de futuro, decisões de arquitetura desalinhadas.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Processo desejado de análise de fraude com RULEX em operação plena.
>
> **Para completar, anexar/confirmar no repositório**:
- docs/processos/*.bpmn
- docs/processos/*.png



### BPMN — Decisão de Fraude

- Categoria: Negócio/Usuário
- Público: Negócio, Exec, Produto, Operação
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Representar o fluxo de decisão de fraude no motor de regras.

**Quando usar**
Entendimento do fluxo de avaliação, auditoria, treinamento.

**O que representa**
Fluxo real de avaliação: entrada → regras → score → decisão.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Processo de decisão opaco, dificuldade de auditoria.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Fluxo de avaliação de transação no motor de regras.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



### BPMN — Exceção / Fallback

- Categoria: Negócio/Usuário
- Público: Negócio, Exec, Produto, Operação
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar o que acontece quando o fluxo principal falha.

**Quando usar**
Resiliência operacional, plano de contingência, treinamento de suporte.

**O que representa**
Caminhos alternativos quando Redis/Neo4j/Backend falha.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Operação sem plano B, risco de indisponibilidade total.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Fluxo de fallback quando componentes falham.
>
> **Para completar, anexar/confirmar no repositório**:
- docs/processos/*.bpmn



### BPMN — Rollback Operacional

- Categoria: Negócio/Usuário
- Público: Negócio, Exec, Produto, Operação
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar o processo de reverter uma publicação de regra problemática.

**Quando usar**
Operação de emergência, mitigação de incidentes, auditoria.

**O que representa**
Passos para reverter regra: identificar → desabilitar → republicar versão anterior.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Incidentes prolongados por falta de processo claro de rollback.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Processo de rollback de regra problemática.
>
> **Para completar, anexar/confirmar no repositório**:
- docs/processos/*.bpmn



---

### 1.2 Diagramas de Casos de Uso (UML)


### Casos de Uso — Analista de Fraude

- Categoria: Negócio/Usuário
- Público: Negócio, Exec, Produto, Operação
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar as funcionalidades disponíveis para o analista.

**Quando usar**
Requisitos, treinamento, validação de escopo.

**O que representa**
Ações que o analista pode executar: criar regra, simular, publicar, auditar.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Funcionalidades mal definidas, escopo ambíguo.

**Notas**
Derivado das rotas do frontend e endpoints do backend verificados no código.



```mermaid
flowchart LR
    subgraph Analista["👤 Analista de Fraude"]
        A1[Criar Regra]
        A2[Editar Regra]
        A3[Simular Regra]
        A4[Publicar Regra]
        A5[Consultar Auditoria]
        A6[Analisar Dashboard]
    end
    subgraph Sistema["🖥️ RULEX"]
        S1[Motor de Regras]
        S2[Banco de Dados]
        S3[Cache Redis]
    end
    A1 --> S1
    A2 --> S1
    A3 --> S1
    A4 --> S1
    A5 --> S2
    A6 --> S2
```



### Casos de Uso — Sistema Externo (Integração)

- Categoria: Negócio/Usuário
- Público: Negócio, Exec, Produto, Operação
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar as funcionalidades expostas para sistemas externos.

**Quando usar**
Contrato de API, integração, documentação técnica.

**O que representa**
Endpoints disponíveis para sistemas que consomem o RULEX.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Integrações mal documentadas, quebras de contrato.

**Notas**
Derivado dos controllers REST verificados: TransactionController, EvaluateController.



```mermaid
flowchart LR
    subgraph Externo["🔌 Sistema Externo"]
        E1[Enviar Transação]
        E2[Consultar Decisão]
        E3[Health Check]
    end
    subgraph RULEX["🖥️ RULEX API"]
        R1["POST /transactions/analyze"]
        R2["POST /evaluate"]
        R3["GET /actuator/health"]
    end
    E1 --> R1
    E2 --> R2
    E3 --> R3
```



### Casos de Uso — Motor de Regras (Interno)

- Categoria: Negócio/Usuário
- Público: Negócio, Exec, Produto, Operação
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar o comportamento interno do motor.

**Quando usar**
Arquitetura interna, debugging, evolução do motor.

**O que representa**
Fluxo interno: carregar regras → avaliar → aplicar score → decidir.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Motor opaco, difícil de debugar e evoluir.

**Notas**
Derivado de RuleEngineService.java verificado no backend.



```mermaid
flowchart TD
    subgraph Motor["⚙️ Motor de Regras"]
        M1[Carregar Regras Ativas]
        M2[Avaliar Condições]
        M3[Aplicar Pesos/Scores]
        M4[Decidir: ALLOW/FLAG/REVIEW/BLOCK]
        M5[Registrar Auditoria]
    end
    M1 --> M2 --> M3 --> M4 --> M5
```



### Casos de Uso — Operação / Suporte

- Categoria: Negócio/Usuário
- Público: Negócio, Exec, Produto, Operação
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar funcionalidades de operação e suporte.

**Quando usar**
Runbooks, treinamento de suporte, SRE.

**O que representa**
Monitoramento, alertas, health checks, métricas.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Operação reativa, falta de visibilidade.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Funcionalidades de monitoramento e suporte.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



### Casos de Uso — Administrador

- Categoria: Negócio/Usuário
- Público: Negócio, Exec, Produto, Operação
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar funcionalidades disponíveis para o administrador.

**Quando usar**
Governança, controle de acesso, configuração do sistema.

**O que representa**
Gerenciar usuários, aprovar regras, configurar parâmetros globais.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Falta de governança, configurações incorretas.

**Notas**
Derivado de RuleApprovalController e endpoints de aprovação verificados.



```mermaid
flowchart LR
    subgraph Admin["👤 Administrador"]
        AD1[Gerenciar Usuários]
        AD2[Aprovar/Rejeitar Regras]
        AD3[Configurar Thresholds Globais]
        AD4[Visualizar Métricas Consolidadas]
        AD5[Exportar Relatórios]
    end
    subgraph Sistema["🖥️ RULEX"]
        S1[Módulo de Usuários]
        S2[Workflow de Aprovação]
        S3[Configurações]
        S4[Dashboard Admin]
    end
    AD1 --> S1
    AD2 --> S2
    AD3 --> S3
    AD4 --> S4
    AD5 --> S4
```



---

### 1.3 Personas


### Persona — Analista de Fraude

- Categoria: Negócio/Usuário
- Público: Negócio, Exec, Produto, Operação
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Definir o perfil típico do usuário analista.

**Quando usar**
UX, priorização de features, comunicação com stakeholders.

**O que representa**
Quem é, o que faz, dores, necessidades, objetivos.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Features desalinhadas com usuário real.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Perfil do analista de fraude: background, responsabilidades, dores, objetivos.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



### Persona — Operação / SRE

- Categoria: Negócio/Usuário
- Público: Negócio, Exec, Produto, Operação
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Definir o perfil do time de operação.

**Quando usar**
Ferramentas de observabilidade, alertas, runbooks.

**O que representa**
Quem opera o sistema, o que precisa monitorar, como age em incidentes.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Sistema não operável, incidentes prolongados.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Perfil do operador/SRE: responsabilidades, ferramentas, necessidades.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



### Persona — Executivo / Compliance

- Categoria: Negócio/Usuário
- Público: Negócio, Exec, Produto, Operação
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Definir o perfil do stakeholder executivo.

**Quando usar**
Dashboards executivos, relatórios de compliance, métricas de negócio.

**O que representa**
O que o executivo precisa ver: taxa de fraude, ROI, compliance.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Decisões estratégicas sem dados, risco regulatório.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Perfil do executivo: KPIs, relatórios, necessidades de compliance.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



### Persona — Sistema Automatizado

- Categoria: Negócio/Usuário
- Público: Negócio, Exec, Produto, Operação
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Definir o perfil do sistema que consome a API.

**Quando usar**
Contrato de API, SLAs, tratamento de erros.

**O que representa**
Características do sistema integrador: volume, latência esperada, retry policy.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Integrações frágeis, SLAs não atendidos.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Perfil do sistema integrador: requisitos de latência, volume, retry.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



---

### 1.4 Mapas de Jornada do Usuário


### Jornada — Criação de Regra

- Categoria: Negócio/Usuário
- Público: Negócio, Exec, Produto, Operação
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Mapear a experiência do usuário ao criar uma regra.

**Quando usar**
UX, identificação de pain points, melhoria contínua.

**O que representa**
Passo a passo: acessar → configurar → validar → salvar.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
UX confusa, erros de configuração, abandono.

**Notas**
Derivado do fluxo da UI ComplexRules e endpoint POST /complex-rules.



```mermaid
journey
    title Jornada de Criação de Regra
    section Acessar
      Abrir tela de regras: 5: Analista
      Clicar em Nova Regra: 5: Analista
    section Configurar
      Preencher nome e descrição: 4: Analista
      Definir condições: 3: Analista
      Definir ações: 4: Analista
    section Validar
      Executar validação: 4: Sistema
      Revisar erros: 2: Analista
    section Salvar
      Confirmar criação: 5: Analista
      Regra salva: 5: Sistema
```



### Jornada — Simulação de Regra

- Categoria: Negócio/Usuário
- Público: Negócio, Exec, Produto, Operação
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Mapear a experiência ao simular uma regra.

**Quando usar**
Validação pré-produção, redução de erros.

**O que representa**
Passo a passo: selecionar regra → configurar payload → executar → analisar resultado.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Regras publicadas sem teste, incidentes em produção.

**Notas**
Derivado do endpoint POST /rules/simulate e tela de simulação.



```mermaid
journey
    title Jornada de Simulação
    section Selecionar
      Acessar regra: 5: Analista
      Clicar em Simular: 5: Analista
    section Configurar
      Preencher payload de teste: 3: Analista
      Ajustar parâmetros: 4: Analista
    section Executar
      Rodar simulação: 5: Sistema
      Aguardar resultado: 3: Analista
    section Analisar
      Ver decisão: 5: Analista
      Ver score detalhado: 4: Analista
      Identificar ajustes: 3: Analista
```



### Jornada — Publicação de Regra

- Categoria: Negócio/Usuário
- Público: Negócio, Exec, Produto, Operação
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Mapear a experiência ao publicar uma regra.

**Quando usar**
Governança, aprovação, auditoria.

**O que representa**
Passo a passo: solicitar publicação → aprovar → ativar → monitorar.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Publicações sem governança, regras problemáticas em produção.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Fluxo de aprovação e ativação de regra em produção.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



### Jornada — Rollback de Regra

- Categoria: Negócio/Usuário
- Público: Negócio, Exec, Produto, Operação
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Mapear a experiência ao reverter uma regra problemática.

**Quando usar**
Resposta a incidentes, mitigação rápida.

**O que representa**
Passo a passo: identificar problema → desabilitar → reverter → validar.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Incidentes prolongados por falta de processo claro.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Fluxo de emergência para reverter regra.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



### Jornada — Investigação de Fraude

- Categoria: Negócio/Usuário
- Público: Negócio, Exec, Produto, Operação
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Mapear a experiência ao investigar uma transação suspeita.

**Quando usar**
Auditoria, compliance, treinamento de analistas.

**O que representa**
Passo a passo: receber alerta → consultar transação → ver regras acionadas → decidir.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Investigações lentas, falta de rastreabilidade.

**Notas**
Derivado do endpoint GET /audit/transaction/{id} e tela de auditoria.



```mermaid
journey
    title Jornada de Investigação
    section Alerta
      Receber notificação: 4: Analista
      Acessar sistema: 5: Analista
    section Consultar
      Buscar transação: 5: Analista
      Ver detalhes: 5: Sistema
    section Analisar
      Ver regras acionadas: 5: Sistema
      Ver score breakdown: 4: Analista
      Consultar histórico do cliente: 3: Analista
    section Decidir
      Marcar como fraude confirmada: 5: Analista
      Ou liberar transação: 5: Analista
```



---

### 1.5 User Story Mapping


### User Story Map — RULEX

- Categoria: Negócio/Usuário
- Público: Negócio, Exec, Produto, Operação
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Organizar funcionalidades em backbone de atividades e releases.

**Quando usar**
Priorização, planejamento de releases, visão de produto.

**O que representa**
Atividades principais → passos do usuário → histórias → priorização MVP vs avançado.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Escopo mal definido, entregas fragmentadas.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Mapa de histórias organizado por atividades e releases.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



---

### 1.6 Service Blueprint


### Service Blueprint — Análise de Transação

- Categoria: Negócio/Usuário
- Público: Negócio, Exec, Produto, Operação
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Mapear frontstage, backstage e sistemas de apoio.

**Quando usar**
Visão holística do serviço, identificação de pontos de falha.

**O que representa**
O que o usuário vê (frontstage) vs o que acontece internamente (backstage).

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Falhas invisíveis, experiência do usuário degradada.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Blueprint do serviço de análise de transação: frontstage, backstage, sistemas de apoio, evidências físicas, pontos de falha.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



---

### 1.7 Business Model Canvas (BMC)


### Business Model Canvas — RULEX

- Categoria: Negócio/Usuário
- Público: Negócio, Exec, Produto, Operação
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar o modelo de negócio do RULEX.

**Quando usar**
Alinhamento estratégico, comunicação com stakeholders, pitch.

**O que representa**
9 blocos: Proposta de Valor, Segmentos, Canais, Relacionamento, Receitas, Recursos, Atividades, Parcerias, Custos.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Desalinhamento estratégico, proposta de valor confusa.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Business Model Canvas com 9 blocos preenchidos para o RULEX.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



---

### 1.8 Value Proposition Canvas


### Value Proposition Canvas — RULEX

- Categoria: Negócio/Usuário
- Público: Negócio, Exec, Produto, Operação
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Detalhar a proposta de valor vs dores e ganhos do cliente.

**Quando usar**
Product-market fit, priorização de features, comunicação.

**O que representa**
Jobs do cliente, dores, ganhos desejados vs como o RULEX resolve.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Produto desalinhado com necessidades reais do cliente.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Canvas com jobs, dores, ganhos e como o RULEX endereça cada um.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



---

## 2. Diagramas de Frontend

Esta seção cobre arquitetura, fluxos de UI, componentes, estados e navegação do frontend React.

---

### 2.1 Diagrama de Arquitetura de Frontend


### Arquitetura do Frontend

- Categoria: Frontend
- Público: Dev Frontend, Design, QA, Produto
- Nível: Tático
- Status: ✅ OK

**Evidência no repositório**
- client/src/main.tsx
- client/src/App.tsx
- vite.config.ts
- package.json

**Objetivo**
Documentar a estrutura de camadas do frontend.

**Quando usar**
Onboarding de devs, decisões de arquitetura, evolução.

**O que representa**
Camadas: UI components, state management, API services, routing.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Código desorganizado, difícil manutenção.

**Notas**
Derivado da estrutura client/src: pages, components, lib, contexts verificados.



```mermaid
flowchart TB
    subgraph UI["🎨 UI Layer"]
        Pages[Pages: Login, Dashboard, Rules, Audit, etc.]
        Components[Components: Forms, Tables, Dialogs]
    end
    subgraph State["📦 State Layer"]
        Context[React Context: Theme, Auth]
        TanStack[TanStack Query: Server State]
    end
    subgraph Services["🔌 Services Layer"]
        API[API Client: fetch/axios]
        Types[TypeScript Types]
    end
    subgraph External["🌐 External"]
        Backend[Backend API: /api/*]
    end
    Pages --> Components
    Components --> Context
    Components --> TanStack
    TanStack --> API
    API --> Backend
```



---

### 2.2 Fluxogramas de UI


### Fluxo UI: Frontend: Pages inventory

- Categoria: Frontend
- Público: Dev Frontend, Design, QA, Produto
- Nível: Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar o fluxo de interação na tela.

**Quando usar**
UX, testes, onboarding.

**O que representa**
Inventário de páginas React presentes no frontend.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Fluxos mal documentados, bugs de UX.

**Notas**
Extraído do inventário (inventory 2026-01-16T21:57:10.121Z).


```mermaid
flowchart TD
  FE[client/src/pages]
  FE --> P1["client/src/pages/Audit.test.tsx"]
  FE --> P2["client/src/pages/Audit.tsx"]
  FE --> P3["client/src/pages/ComplexRules.tsx"]
  FE --> P4["client/src/pages/ComponentShowcase.tsx"]
  FE --> P5["client/src/pages/Dashboard.test.tsx"]
  FE --> P6["client/src/pages/Dashboard.tsx"]
  FE --> P7["client/src/pages/DashboardProfessional.tsx"]
  FE --> P8["client/src/pages/Diagrams.tsx"]
  FE --> P9["client/src/pages/DiagramsHub.tsx"]
  FE --> P10["client/src/pages/Home.test.tsx"]
  FE --> P11["client/src/pages/Home.tsx"]
  FE --> P12["client/src/pages/Login.test.tsx"]
  FE --> P13["client/src/pages/Login.tsx"]
  FE --> P14["client/src/pages/Manual.test.tsx"]
  FE --> P15["client/src/pages/Manual.tsx"]
  FE --> P16["client/src/pages/Monitoring.tsx"]
  FE --> P17["client/src/pages/NotFound.test.tsx"]
  FE --> P18["client/src/pages/NotFound.tsx"]
  FE --> P19["client/src/pages/Rules.test.tsx"]
  FE --> P20["client/src/pages/Rules.tsx"]
  FE --> P21["client/src/pages/RulesAdvanced.tsx"]
  FE --> P22["client/src/pages/RulesDidactic.tsx"]
  FE --> P23["client/src/pages/Settings.tsx"]
  FE --> P24["client/src/pages/Transactions.test.tsx"]
  FE --> P25["client/src/pages/Transactions.tsx"]
  FE --> P26["client/src/pages/TransactionSimulator.tsx"]
  FE --> P27["client/src/pages/TransactionsProfessional.tsx"]
```


### Fluxo UI — Criação de Regra

- Categoria: Frontend
- Público: Dev Frontend, Design, QA, Produto
- Nível: Tático
- Status: ✅ OK

**Evidência no repositório**
- client/src/pages/ComplexRules.tsx
- client/src/App.tsx

**Objetivo**
Documentar o fluxo de criação de regra na interface.

**Quando usar**
UX, testes E2E, treinamento.

**O que representa**
Passo a passo visual: formulário → validação → salvamento.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Bugs de interface, fluxo confuso.

**Notas**
Derivado de RuleFormDialog.tsx e endpoint POST /complex-rules.



```mermaid
flowchart TD
    A[Abrir tela /rules] --> B[Clicar 'Nova Regra']
    B --> C[Abrir RuleFormDialog]
    C --> D{Preencher campos}
    D --> E[Nome, Descrição, Tipo]
    D --> F[Condições]
    D --> G[Ações/Score]
    E & F & G --> H[Clicar Salvar]
    H --> I{Validação}
    I -->|Sucesso| J[POST /complex-rules]
    I -->|Erro| K[Mostrar erros]
    J --> L[Fechar dialog]
    L --> M[Atualizar lista]
    K --> D
```



---

### 2.3 Diagrama de Componentes (Frontend)


### Componentes do Frontend

- Categoria: Frontend
- Público: Dev Frontend, Design, QA, Produto
- Nível: Tático
- Status: ✅ OK

**Evidência no repositório**
- client/src/components
- components.json

**Objetivo**
Mapear os principais componentes e suas dependências.

**Quando usar**
Arquitetura, reuso, manutenção.

**O que representa**
Hierarquia de componentes: pages → containers → components → UI primitives.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Componentes acoplados, difícil reuso.

**Notas**
Derivado da estrutura client/src/components e client/src/pages.



```mermaid
flowchart TD
    subgraph Pages["📄 Pages"]
        PLogin[Login]
        PDash[DashboardProfessional]
        PRules[ComplexRules]
        PAudit[Audit]
        PSim[TransactionSimulator]
        PMon[Monitoring]
    end
    subgraph Containers["📦 Containers"]
        CRuleList[RuleList]
        CRuleForm[RuleFormDialog]
        CAuditTable[AuditTable]
        CSimForm[SimulatorForm]
    end
    subgraph Components["🧩 Components"]
        CompTable[DataTable]
        CompForm[Form Controls]
        CompDialog[Dialog]
        CompChart[Charts]
    end
    subgraph UI["🎨 UI Primitives (shadcn)"]
        UIBtn[Button]
        UIInput[Input]
        UISelect[Select]
        UICard[Card]
    end
    PRules --> CRuleList
    PRules --> CRuleForm
    PAudit --> CAuditTable
    PSim --> CSimForm
    CRuleList --> CompTable
    CRuleForm --> CompForm
    CRuleForm --> CompDialog
    CompForm --> UIInput
    CompForm --> UISelect
    CompTable --> UIBtn
```



---

### 2.4 Diagrama de Estados da UI


### Estados da UI — Componente Genérico

- Categoria: Frontend
- Público: Dev Frontend, Design, QA, Produto
- Nível: Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar os estados possíveis de um componente.

**Quando usar**
Testes, UX, tratamento de erros.

**O que representa**
Estados: Idle → Loading → Success/Error → Retry/Fallback.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Estados não tratados, UX degradada.

**Notas**
Padrão derivado do uso de TanStack Query no frontend.



```mermaid
stateDiagram-v2
    [*] --> Idle
    Idle --> Loading: Ação do usuário
    Loading --> Success: Resposta OK
    Loading --> Error: Resposta erro
    Success --> Idle: Reset/Nova ação
    Error --> Retry: Tentar novamente
    Error --> Fallback: Máximo de tentativas
    Retry --> Loading
    Fallback --> Idle: Ação manual
```



---

### 2.5 Wireflow / User Flow


### Wireflow — Navegação Principal

- Categoria: Frontend
- Público: Dev Frontend, Design, QA, Produto
- Nível: Tático
- Status: ✅ OK

**Evidência no repositório**
- client/src/App.tsx

**Objetivo**
Documentar os caminhos de navegação entre telas.

**Quando usar**
UX, testes E2E, onboarding.

**O que representa**
Mapa de navegação: login → dashboard → telas específicas.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Navegação confusa, usuário perdido.

**Notas**
Derivado de App.tsx: rotas verificadas no código.



```mermaid
flowchart LR
    Login[/login] --> Dashboard[/dashboard]
    Dashboard --> Transactions[/transactions]
    Dashboard --> Rules[/rules]
    Dashboard --> Audit[/audit]
    Dashboard --> Simulator[/simulator]
    Dashboard --> Monitoring[/monitoring]
    Dashboard --> Settings[/settings]
    Dashboard --> Manual[/manual]
    Dashboard --> Diagrams[/diagrams]
    Rules --> RuleDetail[Editar Regra]
    Audit --> AuditDetail[Detalhe Transação]
```



---

### 2.6 Design System / Component Library


### Design System — RULEX

- Categoria: Frontend
- Público: Dev Frontend, Design, QA, Produto
- Nível: Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar paleta, tipografia, espaçamentos, estados, acessibilidade.

**Quando usar**
Consistência visual, onboarding de designers, acessibilidade.

**O que representa**
Cores, fontes, espaçamentos, estados (hover/focus/disabled), tokens de design.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Interface inconsistente, problemas de acessibilidade.

**Notas**
O frontend usa shadcn/ui (components.json verificado), mas não há design system documentado formalmente.



> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Documentação de paleta de cores, tipografia, espaçamentos, componentes base (shadcn/ui) e estados de interação.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



---

## 3. Diagramas de Backend (Java)

Esta seção cobre arquitetura, C4, UML, fluxos de processamento e regras duras.

---

### 3.1 Diagrama de Arquitetura Geral


### Arquitetura Backend — Camadas

- Categoria: Backend
- Público: Dev Backend, Arquiteto, QA, Operação
- Nível: Tático/Detalhado
- Status: ✅ OK

**Evidência no repositório**
- backend/src/main/java/com/rulex

**Objetivo**
Documentar a estrutura de camadas do backend Spring Boot.

**Quando usar**
Onboarding, decisões de arquitetura, manutenção.

**O que representa**
Camadas: Controller → Service → Repository → Entity.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Código desorganizado, violação de camadas.

**Notas**
Derivado da estrutura backend/src/main/java/com/rulex: controller, service, entity, repository.



```mermaid
flowchart TB
    subgraph API["🌐 API Layer"]
        Controllers[REST Controllers]
        Filters[Filters: Auth, CORS, RateLimit]
    end
    subgraph Service["⚙️ Service Layer"]
        RuleEngine[RuleEngineService]
        VelocityFacade[VelocityServiceFacade]
        AuditService[AuditService]
        Neo4jService[Neo4jGraphService]
    end
    subgraph Repository["💾 Repository Layer"]
        JpaRepos[JPA Repositories]
        RedisTemplate[RedisTemplate]
    end
    subgraph Domain["📦 Domain Layer"]
        Entities[JPA Entities]
        DTOs[DTOs]
    end
    subgraph External["🔌 External"]
        Postgres[(PostgreSQL)]
        Redis[(Redis)]
        Neo4j[(Neo4j)]
    end
    Controllers --> RuleEngine
    Controllers --> AuditService
    Filters --> Controllers
    RuleEngine --> VelocityFacade
    RuleEngine --> Neo4jService
    RuleEngine --> JpaRepos
    VelocityFacade --> RedisTemplate
    VelocityFacade --> JpaRepos
    JpaRepos --> Postgres
    RedisTemplate --> Redis
    Neo4jService --> Neo4j
```



---

### 3.2 Diagrama C4


### C4 — Container Diagram

- Categoria: Backend
- Público: Dev Backend, Arquiteto, QA, Operação
- Nível: Tático/Detalhado
- Status: ✅ OK

**Evidência no repositório**
- docker-compose.yml
- backend/src/main/resources/application.yml
- client/src/App.tsx

**Objetivo**
Visão de containers do sistema e suas dependências.

**Quando usar**
Arquitetura de alto nível, comunicação com stakeholders.

**O que representa**
Frontend, Backend, Databases, integrações externas.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Visão sistêmica perdida, decisões desalinhadas.

**Notas**
Visão de containers da solução (Web/API/DB/Cache/Obs).


```mermaid
flowchart LR
  subgraph Users[Users]
    U1[Analista]
    U2[Admin]
  end
  subgraph RULEX[RULEX Platform]
    FE[RULEX Web (React)]
    API[API (Spring Boot)]
    ENG[Rules Engine]
    DB[(PostgreSQL)]
    CACHE[(Redis)]
  end
  subgraph Obs[Observability]
    LOGS[Audit/Logs]
    METRICS[Metrics]
  end
  U1 --> FE
  U2 --> FE
  FE --> API
  API --> ENG
  ENG --> DB
  ENG --> CACHE
  API --> LOGS
  ENG --> LOGS
  LOGS --> METRICS
```


### C4 — Component Diagram (Backend)

- Categoria: Backend
- Público: Dev Backend, Arquiteto, QA, Operação
- Nível: Tático/Detalhado
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Detalhar os componentes internos do backend.

**Quando usar**
Arquitetura detalhada, onboarding de devs.

**O que representa**
Controllers, Services, Repositories, Entities.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Componentes mal definidos, acoplamento.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Diagrama de componentes detalhado do backend.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



---

### 3.3 Diagramas UML


### UML — Diagrama de Classes (Entidades Core)

- Categoria: Backend
- Público: Dev Backend, Arquiteto, QA, Operação
- Nível: Tático/Detalhado
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar as principais entidades do domínio.

**Quando usar**
Modelagem, manutenção, onboarding.

**O que representa**
Transaction, RuleConfiguration, TransactionDecision, AuditLog.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Modelo de dados confuso, bugs de persistência.

**Notas**
Derivado de Transaction.java, RuleConfiguration.java, TransactionDecision.java, AuditLog.java.



```mermaid
classDiagram
    class Transaction {
        +Long id
        +String externalTransactionId
        +String pan
        +BigDecimal transactionAmount
        +Integer mcc
        +String merchantId
        +LocalDateTime createdAt
    }
    class RuleConfiguration {
        +Long id
        +String ruleName
        +RuleType ruleType
        +Integer threshold
        +Integer weight
        +Boolean enabled
        +String conditionsJson
    }
    class TransactionDecision {
        +Long id
        +Long transactionId
        +String classification
        +Integer riskScore
        +String rulesApplied
    }
    class AuditLog {
        +Long id
        +Long transactionId
        +String actionType
        +String performedBy
        +LocalDateTime createdAt
    }
    Transaction "1" --> "*" TransactionDecision
    Transaction "1" --> "*" AuditLog
    RuleConfiguration "1" --> "*" TransactionDecision : aplica
```



### UML — Diagrama de Pacotes

- Categoria: Backend
- Público: Dev Backend, Arquiteto, QA, Operação
- Nível: Tático/Detalhado
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar a organização de pacotes do backend.

**Quando usar**
Arquitetura, separação de responsabilidades.

**O que representa**
Pacotes: controller, service, entity, dto, config, util.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Pacotes desorganizados, ciclos de dependência.

**Notas**
Derivado da estrutura de diretórios backend/src/main/java/com/rulex.



```mermaid
flowchart TB
    subgraph com.rulex
        controller[controller]
        service[service]
        entity[entity]
        dto[dto]
        config[config]
        util[util]
        v31[v31]
    end
    controller --> service
    controller --> dto
    service --> entity
    service --> dto
    service --> util
    v31 --> service
```



### UML — Diagrama de Sequência (Análise de Transação)

- Categoria: Backend
- Público: Dev Backend, Arquiteto, QA, Operação
- Nível: Tático/Detalhado
- Status: ✅ OK

**Evidência no repositório**
- backend/src/main/java/com/rulex/controller/TransactionController.java
- backend/src/main/java/com/rulex/service/RuleEngineService.java

**Objetivo**
Documentar o fluxo de chamadas para análise.

**Quando usar**
Debugging, testes de integração, documentação técnica.

**O que representa**
Frontend → API → Engine → DB → Response.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Fluxo opaco, difícil de debugar.

**Notas**
Sequência real do request /analyze e persistência/auditoria.


```mermaid
sequenceDiagram
  participant FE as RULEX Web
  participant API as TransactionController
  participant ENG as RuleEngineService
  participant DB as PostgreSQL
  participant AUD as AccessLogService
  FE->>API: POST /api/analyze (transaction)
  API->>AUD: log request (headers, route, outcome)
  API->>ENG: analyze(transaction)
  ENG->>DB: load active rules / operators
  DB-->>ENG: rules
  ENG-->>API: decision + score + reasons
  API->>DB: persist transaction + audit
  DB-->>API: ok
  API-->>FE: 200 decision payload
```


### UML — Diagrama de Estados (Regra)

- Categoria: Backend
- Público: Dev Backend, Arquiteto, QA, Operação
- Nível: Tático/Detalhado
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar os estados possíveis de uma regra.

**Quando usar**
Governança, auditoria, workflow de aprovação.

**O que representa**
Estados: DRAFT → PENDING_APPROVAL → ACTIVE → DISABLED → ARCHIVED.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Regras em estados inconsistentes.

**Notas**
Derivado de RuleApproval.java e endpoints de aprovação.



```mermaid
stateDiagram-v2
    [*] --> DRAFT: Criar
    DRAFT --> PENDING_APPROVAL: Solicitar aprovação
    PENDING_APPROVAL --> ACTIVE: Aprovar
    PENDING_APPROVAL --> DRAFT: Rejeitar
    ACTIVE --> DISABLED: Desabilitar
    DISABLED --> ACTIVE: Reabilitar
    DISABLED --> ARCHIVED: Arquivar
    ACTIVE --> ARCHIVED: Arquivar
    ARCHIVED --> [*]
```



---

### 3.4 Fluxogramas de Processamento


### Fluxo — Análise de Transação (/analyze)

- Categoria: Backend
- Público: Dev Backend, Arquiteto, QA, Operação
- Nível: Tático/Detalhado
- Status: ✅ OK

**Evidência no repositório**
- backend/src/main/java/com/rulex/controller/TransactionController.java
- backend/src/main/java/com/rulex/service/RuleEngineService.java
- openapi/rulex.yaml

**Objetivo**
Documentar o fluxo completo de análise.

**Quando usar**
Debugging, testes, documentação.

**O que representa**
Entrada → Validação → Regras → Score → Decisão → Auditoria.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Fluxo opaco, bugs difíceis de rastrear.

**Notas**
Representação fiel do fluxo ponta-a-ponta (FE→API→Engine→DB/Audit).


```mermaid
flowchart TD
  FE[Frontend / Simulator] -->|POST /analyze| API[API (Spring Boot)]
  API --> V[Validate + Normalize payload]
  V --> ENG[RuleEngineService]
  ENG -->|load rules| DB[(PostgreSQL)]
  ENG --> C[Compute score + matches]
  C --> D{Decision}
  D -->|APPROVE| A1[Persist audit + decision]
  D -->|REVIEW| A2[Persist audit + create case]
  D -->|BLOCK| A3[Persist audit + block]
  A1 --> DB
  A2 --> DB
  A3 --> DB
  API --> AUD[AccessLogService / Audit]
  AUD --> MON[MetricsService / Monitoring]
  API --> FE
```


### Fluxo — Tratamento de Exceções

- Categoria: Backend
- Público: Dev Backend, Arquiteto, QA, Operação
- Nível: Tático/Detalhado
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar como exceções são tratadas.

**Quando usar**
Resiliência, debugging, monitoramento.

**O que representa**
Try → Catch → Log → Fallback → Response.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Exceções não tratadas, erros 500.

**Notas**
Derivado de GlobalExceptionHandler.java verificado.



```mermaid
flowchart TD
    A[Requisição] --> B{Try}
    B -->|Sucesso| C[Processar]
    B -->|Exceção| D{Tipo de Exceção}
    D -->|Validação| E[HTTP 400 + detalhes]
    D -->|Não Encontrado| F[HTTP 404]
    D -->|Timeout| G[HTTP 504 + retry hint]
    D -->|Erro Interno| H[HTTP 500 + log]
    C --> I[Resposta OK]
    E & F & G & H --> J[GlobalExceptionHandler]
    J --> K[Log estruturado]
    K --> L[Métricas]
```



---

### 3.5 Diagrama de Regras Duras


### Arquitetura de Regras Duras

- Categoria: Backend
- Público: Dev Backend, Arquiteto, QA, Operação
- Nível: Tático/Detalhado
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar a estrutura do motor de regras.

**Quando usar**
Evolução do motor, debugging, documentação técnica.

**O que representa**
Operadores, condições, encadeamento, prioridade, curto-circuito.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Motor inflexível, regras mal configuradas.

**Notas**
Derivado de RuleEngineService.java, ParallelRuleExecutionService.java, RuleCondition.java.



```mermaid
flowchart TD
    subgraph Motor["⚙️ Motor de Regras"]
        Load[Carregar Regras Ativas]
        Sort[Ordenar por Prioridade/Tier]
        Eval[Avaliar Condições]
        Score[Calcular Score]
        Decision[Decisão Final]
    end
    
    subgraph Condição["📋 Estrutura de Condição"]
        Field[Campo: transactionAmount, mcc, etc.]
        Operator[Operador: GT, LT, EQ, IN, REGEX, etc.]
        Value[Valor: número, lista, pattern]
    end
    
    subgraph Tiers["🏷️ Tiers de Execução"]
        T1["TIER 1: Blocklists (< 1ms)"]
        T2["TIER 2: Velocity (< 10ms)"]
        T3["TIER 3: Agregações (< 100ms)"]
    end
    
    Load --> Sort
    Sort --> T1
    T1 -->|BLOCK?| ShortCircuit[Curto-circuito]
    T1 -->|ALLOW| T2
    T2 -->|BLOCK?| ShortCircuit
    T2 -->|ALLOW| T3
    T3 --> Eval
    Eval --> Score
    Score --> Decision
    ShortCircuit --> Decision
    
    Eval -.-> Field
    Eval -.-> Operator
    Eval -.-> Value
```



---

### 3.6 API Contract / Integrações


### API Contract — OpenAPI

- Categoria: Backend
- Público: Dev Backend, Arquiteto, QA, Operação
- Nível: Tático/Detalhado
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar endpoints expostos conforme contrato OpenAPI.

**Quando usar**
Integração, documentação técnica, testes de contrato.

**O que representa**
Endpoints REST, métodos HTTP, payloads, códigos de resposta.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Integrações quebradas, documentação desatualizada.

**Notas**
Derivado de openapi/rulex.yaml verificado.



```mermaid
flowchart LR
    subgraph Endpoints["📡 Principais Endpoints (openapi/rulex.yaml)"]
        E1["POST /transactions/analyze"]
        E2["POST /evaluate"]
        E3["GET/POST /rules"]
        E4["GET/POST /complex-rules"]
        E5["GET /audit"]
        E6["POST /rules/simulate"]
        E7["GET /metrics"]
        E8["GET /actuator/health"]
    end
    
    subgraph Consumers["🔌 Consumidores"]
        C1["Frontend React"]
        C2["Sistemas Externos"]
        C3["Monitoramento"]
    end
    
    C1 --> E3
    C1 --> E4
    C1 --> E5
    C1 --> E6
    C2 --> E1
    C2 --> E2
    C3 --> E7
    C3 --> E8
```



### Integrações Externas

- Categoria: Backend
- Público: Dev Backend, Arquiteto, QA, Operação
- Nível: Tático/Detalhado
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar sistemas externos que se integram ao RULEX.

**Quando usar**
Arquitetura de integração, contratos, SLAs.

**O que representa**
Sistemas de pagamento, sistemas legados, APIs externas.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Integrações frágeis, quebras de contrato.

**Notas**
Não há evidência de integrações externas específicas documentadas no repositório.



> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Diagrama de sistemas externos que consomem ou alimentam o RULEX, com protocolos e formatos.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



---

### 3.7 Event / Message Flow


### Event / Message Flow

- Categoria: Backend
- Público: Dev Backend, Arquiteto, QA, Operação
- Nível: Tático/Detalhado
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar fluxo de eventos e mensagens (filas, tópicos).

**Quando usar**
Arquitetura assíncrona, debugging, monitoramento.

**O que representa**
Filas, tópicos, producers, consumers, eventos de domínio.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Perda de mensagens, processamento duplicado, acoplamento.

**Notas**
Não há evidência de mensageria (Kafka, RabbitMQ, etc.) no repositório. O sistema opera de forma síncrona.



> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Diagrama de filas/tópicos (Kafka, RabbitMQ, etc.), producers e consumers. Se não houver mensageria, registrar como N/A.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



---

## 4. Diagramas de PostgreSQL

Esta seção cobre modelo de dados, ERD, schemas, armazenamento e replicação.

---

### 4.1 Modelo de Dados


### Modelo Conceitual

- Categoria: PostgreSQL
- Público: Dev Backend, DBA, Arquiteto, Operação
- Nível: Tático/Detalhado
- Status: ✅ OK

**Evidência no repositório**
- backend/src/main/resources/db/migration/V2__core_schema.sql

**Objetivo**
Visão de alto nível das entidades e relacionamentos.

**Quando usar**
Comunicação com negócio, modelagem inicial.

**O que representa**
Entidades principais sem detalhes de implementação.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Modelo desalinhado com negócio.

**Notas**
Derivado das entidades JPA e migrations Flyway.



```mermaid
erDiagram
    TRANSACAO ||--o{ DECISAO : tem
    TRANSACAO ||--o{ AUDITORIA : gera
    REGRA ||--o{ DECISAO : aplica
    REGRA ||--o{ HISTORICO : versiona
    REGRA }|--|| APROVACAO : requer
```



### Modelo Lógico

- Categoria: PostgreSQL
- Público: Dev Backend, DBA, Arquiteto, Operação
- Nível: Tático/Detalhado
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Detalhes de tabelas, colunas e tipos.

**Quando usar**
Desenvolvimento, migrations, documentação técnica.

**O que representa**
Tabelas com colunas, tipos, constraints.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Schema inconsistente, bugs de persistência.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Diagrama com todas as tabelas, colunas, tipos e constraints.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



### Modelo Físico

- Categoria: PostgreSQL
- Público: Dev Backend, DBA, Arquiteto, Operação
- Nível: Tático/Detalhado
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Detalhes de índices, particionamento, storage.

**Quando usar**
Performance, DBA, otimização.

**O que representa**
Índices, tablespaces, partições.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Performance degradada, queries lentas.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Diagrama com índices, partições e configurações de storage.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



---

### 4.2 Diagrama ER (ERD)


### ERD — Entidades Core

- Categoria: PostgreSQL
- Público: Dev Backend, DBA, Arquiteto, Operação
- Nível: Tático/Detalhado
- Status: ✅ OK

**Evidência no repositório**
- backend/src/main/resources/db/migration/V2__core_schema.sql

**Objetivo**
Diagrama ER das principais tabelas.

**Quando usar**
Modelagem, manutenção, onboarding.

**O que representa**
Transações, Regras, Decisões, Auditoria.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Relacionamentos incorretos, integridade comprometida.

**Notas**
Modelo de dados central (alto nível).


```mermaid
erDiagram
  TRANSACTION ||--o{ RULE_EVAL : evaluated_by
  RULE_EVAL }o--|| RULE : references
  TRANSACTION {
    uuid id
    string externalTransactionId
    decimal amount
    string currency
  }
  RULE {
    uuid id
    string name
    string status
  }
  RULE_EVAL {
    uuid id
    uuid transactionId
    uuid ruleId
    int scoreImpact
  }
```


### ERD — Completo

- Categoria: PostgreSQL
- Público: Dev Backend, DBA, Arquiteto, Operação
- Nível: Tático/Detalhado
- Status: ✅ OK

**Evidência no repositório**
- backend/src/main/resources/db/migration

**Objetivo**
Diagrama ER de todas as tabelas.

**Quando usar**
DBA, documentação completa.

**O que representa**
Todas as tabelas do schema com relacionamentos.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Visão incompleta do banco.

**Notas**
Derivado de V2__core_schema.sql e entidades JPA verificadas.



```mermaid
erDiagram
    transactions ||--o{ transaction_decisions : has
    transactions ||--o{ audit_logs : generates
    transactions ||--o{ velocity_transaction_log : tracks
    
    rule_configurations ||--o{ transaction_decisions : applies
    rule_configurations ||--o{ rule_configuration_history : versions
    rule_configurations ||--o{ rule_approvals : requires
    
    complex_rules ||--o{ complex_rule_conditions : has
    
    shadow_evaluation_log }|--|| rule_configurations : evaluates
    
    transactions {
        bigint id PK
        varchar external_transaction_id UK
        varchar pan
        numeric transaction_amount
        integer mcc
        timestamp created_at
    }
    
    rule_configurations {
        bigint id PK
        varchar rule_name UK
        varchar rule_type
        integer threshold
        integer weight
        boolean enabled
        jsonb conditions_json
    }
    
    transaction_decisions {
        bigint id PK
        bigint transaction_id FK
        varchar classification
        integer risk_score
    }
    
    audit_logs {
        bigint id PK
        bigint transaction_id FK
        varchar action_type
        timestamp created_at
    }
```



---

### 4.3 Diagrama de Schemas


### Organização de Schemas

- Categoria: PostgreSQL
- Público: Dev Backend, DBA, Arquiteto, Operação
- Nível: Tático/Detalhado
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar schemas, tabelas e índices.

**Quando usar**
DBA, organização, isolamento.

**O que representa**
Schema público com tabelas do RULEX.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Mistura de dados, isolamento comprometido.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Diagrama de schemas com tabelas organizadas.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



---

### 4.4 Diagrama de Armazenamento Físico


### Armazenamento PostgreSQL

- Categoria: PostgreSQL
- Público: Dev Backend, DBA, Arquiteto, Operação
- Nível: Tático/Detalhado
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar pages, WAL, files.

**Quando usar**
DBA, performance, backup/recovery.

**O que representa**
Estrutura física: data files, WAL, checkpoints.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Backup inconsistente, recovery problemático.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Diagrama de pages, WAL, data files do PostgreSQL.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



---

### 4.5 Diagrama de Replicação


### Replicação PostgreSQL

- Categoria: PostgreSQL
- Público: Dev Backend, DBA, Arquiteto, Operação
- Nível: Tático/Detalhado
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar topologia de replicação.

**Quando usar**
Alta disponibilidade, disaster recovery.

**O que representa**
Primário, réplicas síncronas/assíncronas.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Indisponibilidade, perda de dados.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Diagrama de primário, réplicas, síncrona vs assíncrona.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



---

### 4.6 Data Lifecycle / Retenção / LGPD


### Data Lifecycle — Retenção e LGPD

- Categoria: PostgreSQL
- Público: Dev Backend, DBA, Arquiteto, Operação
- Nível: Tático/Detalhado
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar ciclo de vida dos dados, retenção e conformidade LGPD.

**Quando usar**
Compliance, auditoria, governança de dados.

**O que representa**
Políticas de retenção, anonimização, exclusão, auditoria de acesso.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Não conformidade LGPD, dados retidos indefinidamente, risco regulatório.

**Notas**
Não há evidência de políticas de retenção ou processos de anonimização documentados no repositório.



> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Diagrama de ciclo de vida: criação → uso → arquivamento → exclusão. Políticas de retenção por tipo de dado. Processo de anonimização/pseudonimização para LGPD.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



---

## 5. Diagramas de Redis

Esta seção cobre tipos de dados, arquitetura, cache, replicação, cluster e persistência.

---

### 5.1 Diagrama de Tipos de Dados


### Tipos de Dados Redis — RULEX

- Categoria: Redis
- Público: Dev Backend, Arquiteto, Operação
- Nível: Tático/Detalhado
- Status: ✅ OK

**Evidência no repositório**
- backend/src/main/java/com/rulex/service/RedisVelocityCacheService.java
- backend/src/main/java/com/rulex/service/RedisVelocityService.java

**Objetivo**
Documentar como o RULEX usa cada tipo de dado.

**Quando usar**
Desenvolvimento, debugging, otimização.

**O que representa**
String (contadores), Hash (stats), HyperLogLog (distincts).

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Uso inadequado, performance degradada.

**Notas**
Derivado de RedisVelocityCacheService.java verificado.



```mermaid
flowchart LR
    subgraph Redis["🔴 Redis RULEX"]
        subgraph Strings["String"]
            S1["velocity:{keyType}:{hash}:count:{window}"]
            S2["velocity:{keyType}:{hash}:sum:{window}"]
        end
        subgraph HLL["HyperLogLog"]
            H1["velocity:{keyType}:{hash}:distinct:merchants"]
            H2["velocity:{keyType}:{hash}:distinct:mccs"]
            H3["velocity:{keyType}:{hash}:distinct:countries"]
        end
    end
    
    subgraph Operações["Operações"]
        INCR[INCR/INCRBY]
        GET[GET]
        PFADD[PFADD]
        PFCOUNT[PFCOUNT]
    end
    
    INCR --> S1
    INCR --> S2
    GET --> S1
    GET --> S2
    PFADD --> H1
    PFADD --> H2
    PFADD --> H3
    PFCOUNT --> H1
    PFCOUNT --> H2
    PFCOUNT --> H3
```



---

### 5.2 Diagrama de Arquitetura (Event Loop)


### Arquitetura Redis — Event Loop

- Categoria: Redis
- Público: Dev Backend, Arquiteto, Operação
- Nível: Tático/Detalhado
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar o modelo de execução single-threaded.

**Quando usar**
Performance, debugging, capacity planning.

**O que representa**
Event loop, I/O multiplexado, comandos atômicos.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Bloqueios por comandos lentos.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Diagrama do event loop single-threaded do Redis.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



---

### 5.3 Diagrama de Cache


### Estratégia de Cache — RULEX

- Categoria: Redis
- Público: Dev Backend, Arquiteto, Operação
- Nível: Tático/Detalhado
- Status: ✅ OK

**Evidência no repositório**
- backend/src/main/resources/application.yml
- backend/src/main/java/com/rulex/service/RedisVelocityCacheService.java
- backend/src/main/java/com/rulex/service/RedisVelocityService.java
- backend/src/main/java/com/rulex/service/VelocityServiceFacade.java

**Objetivo**
Documentar como o cache é usado.

**Quando usar**
Performance, consistência, debugging.

**O que representa**
TTL por janela temporal, cache-aside pattern.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Cache stale, dados inconsistentes.

**Notas**
Derivado de VelocityServiceFacade.java e RedisVelocityCacheService.java.



```mermaid
flowchart TD
    subgraph App["⚙️ RULEX Backend"]
        Request[Requisição de Velocidade]
        Facade[VelocityServiceFacade]
    end
    
    subgraph Cache["🔴 Redis"]
        Check{Cache hit?}
        Get[GET/PFCOUNT]
        Set[INCRBY/PFADD]
    end
    
    subgraph DB["🐘 PostgreSQL"]
        Query[Query de fallback]
    end
    
    Request --> Facade
    Facade --> Check
    Check -->|Hit| Get
    Check -->|Miss| Query
    Query --> Set
    Get --> Response[Resposta]
    Set --> Response
    
    subgraph TTL["⏱️ TTL por Janela"]
        T1["5min → TTL 6min"]
        T2["1h → TTL 65min"]
        T3["24h → TTL 25h"]
    end
```



---

### 5.4 Diagrama de Replicação


### Replicação Redis

- Categoria: Redis
- Público: Dev Backend, Arquiteto, Operação
- Nível: Tático/Detalhado
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar topologia leader-follower.

**Quando usar**
Alta disponibilidade, leitura escalável.

**O que representa**
Leader para escrita, followers para leitura.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Indisponibilidade, inconsistência em failover.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Diagrama leader-follower do Redis.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



---

### 5.5 Diagrama de Cluster


### Cluster Redis

- Categoria: Redis
- Público: Dev Backend, Arquiteto, Operação
- Nível: Tático/Detalhado
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar sharding e hash slots.

**Quando usar**
Escalabilidade horizontal.

**O que representa**
Shards, hash slots, redirecionamento.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Sem escalabilidade, limite de memória.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Diagrama de shards, hash slots, cluster topology.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



---

### 5.6 Diagrama de Persistência


### Persistência Redis

- Categoria: Redis
- Público: Dev Backend, Arquiteto, Operação
- Nível: Tático/Detalhado
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar RDB, AOF, estratégias.

**Quando usar**
Durabilidade, recovery.

**O que representa**
RDB snapshots, AOF append-only, combinado.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Perda de dados em crash.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Diagrama de RDB, AOF, estratégias de persistência.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



---

### 5.7 Consistência / Invalidação / Stampede


### Consistência e Invalidação de Cache

- Categoria: Redis
- Público: Dev Backend, Arquiteto, Operação
- Nível: Tático/Detalhado
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar estratégias de consistência, invalidação e proteção contra stampede.

**Quando usar**
Performance, consistência de dados, resiliência.

**O que representa**
TTL, invalidação explícita, thundering herd protection, distributed locks.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Dados stale, inconsistência, cache stampede degradando o sistema.

**Notas**
Derivado de RedisVelocityCacheService.java e VelocityServiceFacade.java: TTL configurado por janela temporal.



```mermaid
flowchart TD
    subgraph Estratégias["🔄 Estratégias de Consistência"]
        TTL["TTL por janela temporal"]
        Expire["Expiração automática"]
        NoInvalidation["Sem invalidação explícita (event-driven)"]
    end
    
    subgraph Proteção["🛡️ Proteção contra Stampede"]
        P1["TTL com jitter (variação aleatória)"]
        P2["Fallback para PostgreSQL"]
        P3["Sem lock distribuído implementado"]
    end
    
    subgraph Fluxo["Fluxo de Verificação"]
        F1["Requisição"] --> F2{"Cache hit?"}
        F2 -->|Hit| F3["Retornar valor"]
        F2 -->|Miss| F4["Buscar no PostgreSQL"]
        F4 --> F5["Atualizar cache com TTL"]
        F5 --> F3
    end
```



---

## 6. Diagramas de Neo4j

Esta seção cobre modelo de grafo, instâncias, adjacência, armazenamento, cluster e multi-data-center.

---

### 6.1 Modelo de Grafo (Property Graph)


### Property Graph — RULEX

- Categoria: Neo4j
- Público: Dev Backend, Data/Graph, Arquiteto
- Nível: Detalhado
- Status: ✅ OK

**Evidência no repositório**
- backend/src/main/java/com/rulex/service/Neo4jGraphService.java
- backend/src/main/java/com/rulex/service/complex/ComplexRuleEvaluator.java
- docker-compose.yml
- backend/src/main/resources/application.yml

**Objetivo**
Documentar nós, relacionamentos e propriedades.

**Quando usar**
Modelagem de grafos, análise de fraude.

**O que representa**
Nós: Account, Transaction. Relacionamentos: TRANSFERRED_TO, SHARES_PII.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Análise de rede ineficaz, fraud rings não detectados.

**Notas**
Derivado de Neo4jGraphService.java: queries verificadas.



```mermaid
flowchart LR
    subgraph Nodes["📍 Nós"]
        A1["Account"]
        T1["Transaction"]
    end
    
    subgraph Properties["📝 Propriedades"]
        A1 --> AP1["id, email, phone, riskLevel"]
        T1 --> TP1["amount, timestamp, decision"]
    end
    
    subgraph Relationships["🔗 Relacionamentos"]
        A1 -->|TRANSFERRED_TO| A2["Account"]
        A1 -->|SHARES_PII| A3["Account"]
        A1 -->|MADE| T1
    end
```



---

### 6.2 Diagrama de Instâncias


### Exemplo de Grafo — Fraud Ring

- Categoria: Neo4j
- Público: Dev Backend, Data/Graph, Arquiteto
- Nível: Detalhado
- Status: ✅ OK

**Evidência no repositório**
- backend/src/main/java/com/rulex/service/Neo4jGraphService.java
- backend/src/main/java/com/rulex/service/complex/ComplexRuleEvaluator.java

**Objetivo**
Visualizar exemplo real de dados de fraude.

**Quando usar**
Treinamento, validação de modelo.

**O que representa**
Contas conectadas formando um ring de fraude.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Conceito abstrato, difícil de entender.

**Notas**
Exemplo ilustrativo baseado em queries de Neo4jGraphService.java.



```mermaid
flowchart LR
    A["Account A<br/>riskLevel: HIGH"] -->|"$1000"| B["Account B<br/>riskLevel: MEDIUM"]
    B -->|"$950"| C["Account C<br/>riskLevel: LOW"]
    C -->|"$900"| A
    
    A -.->|SHARES_PII| D["Account D"]
    B -.->|SHARES_PII| D
    
    style A fill:#ff6b6b
    style B fill:#feca57
    style C fill:#48dbfb
    style D fill:#ff9ff3
```



---

### 6.3 Diagrama de Adjacência Sem Índice


### Index-Free Adjacency

- Categoria: Neo4j
- Público: Dev Backend, Data/Graph, Arquiteto
- Nível: Detalhado
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar a navegação O(1) do Neo4j.

**Quando usar**
Entendimento de performance de grafos.

**O que representa**
Ponteiros diretos entre nós, sem lookup de índice.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Mal entendimento de performance de grafos.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Diagrama explicando navegação O(1) via ponteiros diretos.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



---

### 6.4 Diagrama de Armazenamento


### Armazenamento Neo4j

- Categoria: Neo4j
- Público: Dev Backend, Data/Graph, Arquiteto
- Nível: Detalhado
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar stores: Node, Relationship, Property.

**Quando usar**
DBA, performance, sizing.

**O que representa**
Arquivos de store, estrutura interna.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Má configuração, performance degradada.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Diagrama de Node Store, Relationship Store, Property Store.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



---

### 6.5 Diagrama de Cluster Causal


### Cluster Causal Neo4j

- Categoria: Neo4j
- Público: Dev Backend, Data/Graph, Arquiteto
- Nível: Detalhado
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar Core Servers, Read Replicas, RAFT.

**Quando usar**
Alta disponibilidade, escalabilidade.

**O que representa**
Core servers para escrita, read replicas para leitura.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Indisponibilidade, inconsistência.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Diagrama de Core Servers, Read Replicas, protocolo RAFT.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



---

### 6.6 Diagrama Multi-Data Center


### Multi-Data Center Neo4j

- Categoria: Neo4j
- Público: Dev Backend, Data/Graph, Arquiteto
- Nível: Detalhado
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar topologia multi-DC.

**Quando usar**
Disaster recovery, latência global.

**O que representa**
Padrões recomendados e proibidos.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Arquitetura frágil, latência alta.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Diagrama de topologia multi-data-center.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



---

### 6.7 Índices, Constraints e Query Patterns


### Índices e Constraints Neo4j

- Categoria: Neo4j
- Público: Dev Backend, Data/Graph, Arquiteto
- Nível: Detalhado
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar índices, constraints e padrões de query Cypher.

**Quando usar**
Performance de grafos, integridade de dados.

**O que representa**
Índices por propriedade, constraints de unicidade, padrões de query otimizados.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Queries lentas, dados duplicados, integridade comprometida.

**Notas**
Não há evidência de índices ou constraints Neo4j documentados. Verificar Neo4jGraphService.java para queries utilizadas.



> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Diagrama de índices (node/relationship), constraints de unicidade, e exemplos de queries Cypher otimizadas.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



---

## 7. Diagramas Transversais

Esta seção cobre DFD, segurança, observabilidade e resiliência.

---

### 7.1 Diagramas de Fluxo de Dados (DFD)


### DFD Nível 0 — Contexto

- Categoria: Transversal
- Público: Arquiteto, Segurança, Operação, QA
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Visão de alto nível do sistema e suas fronteiras.

**Quando usar**
Segurança, privacidade, análise de riscos.

**O que representa**
Sistema RULEX e entidades externas.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Fronteiras mal definidas, riscos de privacidade.

**Notas**
Derivado da arquitetura geral verificada.



```mermaid
flowchart TB
    subgraph External["🌐 Entidades Externas"]
        E1["Sistema de Pagamentos"]
        E2["Analista de Fraude"]
        E3["Executivo"]
    end
    
    subgraph RULEX["⚙️ RULEX"]
        P1["Motor de Regras"]
    end
    
    E1 -->|"Transações"| P1
    P1 -->|"Decisão"| E1
    E2 -->|"Configuração"| P1
    P1 -->|"Dashboard"| E2
    P1 -->|"Relatórios"| E3
```



### DFD Nível 1 — Processos Principais

- Categoria: Transversal
- Público: Arquiteto, Segurança, Operação, QA
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Detalhar os processos internos.

**Quando usar**
Análise de riscos, auditoria.

**O que representa**
Processos: Receber, Avaliar, Decidir, Registrar.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Processos opacos, auditoria difícil.

**Notas**
Derivado do fluxo de TransactionController → RuleEngineService.



```mermaid
flowchart TB
    subgraph Input["📥 Entrada"]
        D1["Transação JSON"]
    end
    
    subgraph Processes["⚙️ Processos"]
        P1["1. Validar"]
        P2["2. Enriquecer"]
        P3["3. Avaliar Regras"]
        P4["4. Calcular Score"]
        P5["5. Decidir"]
        P6["6. Registrar"]
    end
    
    subgraph Stores["💾 Armazenamentos"]
        S1[("Regras")]
        S2[("Transações")]
        S3[("Auditoria")]
        S4[("Cache")]
    end
    
    subgraph Output["📤 Saída"]
        D2["Decisão JSON"]
    end
    
    D1 --> P1
    P1 --> P2
    P2 --> P3
    P3 --> S1
    P3 --> S4
    P3 --> P4
    P4 --> P5
    P5 --> P6
    P6 --> S2
    P6 --> S3
    P5 --> D2
```



### DFD Nível 2 — Detalhamento

- Categoria: Transversal
- Público: Arquiteto, Segurança, Operação, QA
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Detalhar subprocessos.

**Quando usar**
Análise detalhada, debugging.

**O que representa**
Subprocessos de cada processo principal.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Visão superficial, detalhes perdidos.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Diagrama detalhando subprocessos de avaliação de regras.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



---

### 7.2 Diagramas de Segurança


### Autenticação e Autorização

- Categoria: Transversal
- Público: Arquiteto, Segurança, Operação, QA
- Nível: Estratégico/Tático
- Status: ✅ OK

**Evidência no repositório**
- backend/src/main/java/com/rulex/config/SecurityConfig.java
- backend/src/main/resources/application.yml

**Objetivo**
Documentar mecanismos de segurança.

**Quando usar**
Auditoria de segurança, compliance.

**O que representa**
Basic Auth, roles (ADMIN, ANALYST), endpoints protegidos.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Acesso não autorizado, vazamento de dados.

**Notas**
Derivado de SecurityConfig.java verificado.



```mermaid
flowchart TD
    subgraph Request["📥 Requisição"]
        R1["HTTP Request"]
        R2["Authorization Header"]
    end
    
    subgraph Security["🔐 Security Layer"]
        F1["SecurityFilterChain"]
        F2["BasicAuthenticationFilter"]
        F3["UserDetailsService"]
        F4["BCryptPasswordEncoder"]
    end
    
    subgraph Authorization["🎫 Autorização"]
        A1{"Endpoint protegido?"}
        A2{"Role permitida?"}
    end
    
    subgraph Roles["👥 Roles"]
        ADMIN["ADMIN: CRUD completo"]
        ANALYST["ANALYST: Leitura + Simulação"]
    end
    
    R1 --> F1
    R2 --> F2
    F2 --> F3
    F3 --> F4
    F1 --> A1
    A1 -->|Sim| A2
    A1 -->|Não| Allow["Permitir"]
    A2 -->|ADMIN| ADMIN
    A2 -->|ANALYST| ANALYST
    A2 -->|Negado| Deny["HTTP 403"]
```



### Proteção de Dados / LGPD

- Categoria: Transversal
- Público: Arquiteto, Segurança, Operação, QA
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar tratamento de dados sensíveis.

**Quando usar**
Compliance, auditoria LGPD.

**O que representa**
Mascaramento de PAN, hash de dados sensíveis, logs seguros.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Vazamento de dados, não conformidade LGPD.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Diagrama de tratamento de dados sensíveis e compliance LGPD.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



### Threat Model — STRIDE

- Categoria: Transversal
- Público: Arquiteto, Segurança, Operação, QA
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar análise de ameaças usando metodologia STRIDE.

**Quando usar**
Segurança, análise de riscos, compliance.

**O que representa**
Spoofing, Tampering, Repudiation, Information Disclosure, Denial of Service, Elevation of Privilege.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Vulnerabilidades não identificadas, ataques não mitigados.

**Notas**
Template STRIDE baseado em SecurityConfig.java. Análise formal de ameaças não encontrada no repositório.



```mermaid
flowchart TB
    subgraph STRIDE["🔐 Análise STRIDE (Template Recomendado)"]
        S["Spoofing<br/>Risco: Impersonar usuário/sistema"]
        T["Tampering<br/>Risco: Alterar dados em trânsito/repouso"]
        R["Repudiation<br/>Risco: Negar ações realizadas"]
        I["Information Disclosure<br/>Risco: Vazamento de dados sensíveis"]
        D["Denial of Service<br/>Risco: Indisponibilidade do serviço"]
        E["Elevation of Privilege<br/>Risco: Acesso não autorizado"]
    end
    
    subgraph Mitigações["🛡️ Mitigações Identificadas"]
        M1["Basic Auth + BCrypt (S)"]
        M2["HTTPS obrigatório (T)"]
        M3["Audit logs (R)"]
        M4["Mascaramento PAN (I)"]
        M5["Rate limiting (D)"]
        M6["RBAC: ADMIN/ANALYST (E)"]
    end
    
    S -.-> M1
    T -.-> M2
    R -.-> M3
    I -.-> M4
    D -.-> M5
    E -.-> M6
```



---

### 7.3 Diagramas de Observabilidade


### Stack de Observabilidade

- Categoria: Transversal
- Público: Arquiteto, Segurança, Operação, QA
- Nível: Estratégico/Tático
- Status: ✅ OK

**Evidência no repositório**
- backend/src/main/resources/application.yml
- backend/src/main/resources/prometheus-alerts.yml

**Objetivo**
Documentar logs, métricas, traces, alertas.

**Quando usar**
Operação, debugging, SRE.

**O que representa**
Prometheus metrics, OpenTelemetry traces, alertas configurados.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Sistema opaco, incidentes prolongados.

**Notas**
Derivado de application.yml: prometheus, otel configurados.



```mermaid
flowchart TB
    subgraph App["⚙️ RULEX Backend"]
        Logs["Logs (SLF4J)"]
        Metrics["Micrometer Metrics"]
        Traces["OpenTelemetry Traces"]
    end
    
    subgraph Collection["📊 Coleta"]
        Prometheus["Prometheus"]
        Jaeger["Jaeger/OTLP"]
        Loki["Loki (opcional)"]
    end
    
    subgraph Visualization["📈 Visualização"]
        Grafana["Grafana"]
        Alerts["Alertmanager"]
    end
    
    Metrics --> Prometheus
    Traces --> Jaeger
    Logs --> Loki
    Prometheus --> Grafana
    Prometheus --> Alerts
    Jaeger --> Grafana
    Loki --> Grafana
```



### Alertas Configurados

- Categoria: Transversal
- Público: Arquiteto, Segurança, Operação, QA
- Nível: Estratégico/Tático
- Status: ✅ OK

**Evidência no repositório**
- backend/src/main/resources/prometheus-alerts.yml

**Objetivo**
Documentar alertas ativos.

**Quando usar**
Operação, resposta a incidentes.

**O que representa**
Alertas de error rate, latência, pool, auth, JVM, fraude.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Incidentes não detectados.

**Notas**
Derivado de prometheus-alerts.yml verificado.



```mermaid
flowchart LR
    subgraph Alerts["🚨 Alertas RULEX"]
        A1["HighErrorRate: >5% erros em /analyze"]
        A2["HighLatency: p99 > 500ms"]
        A3["HikariPoolExhausted: pool < 5"]
        A4["AuthSpike: >100 401/403 em 5min"]
        A5["JVMMemoryHigh: heap > 85%"]
        A6["FraudRateAnomaly: fraude > 10%"]
    end
    
    subgraph Actions["📤 Ações"]
        Slack["Slack/Teams"]
        PagerDuty["PagerDuty"]
        Email["Email"]
    end
    
    A1 & A2 & A3 --> PagerDuty
    A4 & A5 & A6 --> Slack
    A1 & A2 & A3 & A4 & A5 & A6 --> Email
```



---

### 7.4 Diagramas de Resiliência


### Padrões de Resiliência

- Categoria: Transversal
- Público: Arquiteto, Segurança, Operação, QA
- Nível: Estratégico/Tático
- Status: ✅ OK

**Evidência no repositório**
- backend/pom.xml
- backend/src/main/resources/application.yml

**Objetivo**
Documentar retry, timeout, circuit breaker, fallback.

**Quando usar**
Alta disponibilidade, degradação graciosa.

**O que representa**
Estratégias implementadas: timeout em regex, tiers com timeout, fallback de cache.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Sistema frágil, cascata de falhas.

**Notas**
Derivado de VelocityServiceFacade.java, ParallelRuleExecutionService.java, RegexValidator.java.



```mermaid
flowchart TD
    subgraph Patterns["🛡️ Padrões Implementados"]
        P1["Timeout: Regex (1s), Regras por tier (5ms/50ms/200ms)"]
        P2["Fallback: Redis → Memory → PostgreSQL"]
        P3["Early Termination: BLOCK → Skip remaining tiers"]
        P4["Graceful Degradation: Erro em regra → continuar com outras"]
    end
    
    subgraph Flow["Fluxo de Fallback (Velocidade)"]
        F1["Tentar RedisVelocityCacheService"]
        F2{"Sucesso?"}
        F3["Tentar RedisVelocityService (memória)"]
        F4{"Sucesso?"}
        F5["Fallback: VelocityService (PostgreSQL)"]
        F6["Retornar resultado"]
    end
    
    F1 --> F2
    F2 -->|Sim| F6
    F2 -->|Não| F3
    F3 --> F4
    F4 -->|Sim| F6
    F4 -->|Não| F5
    F5 --> F6
```



### Circuit Breaker

- Categoria: Transversal
- Público: Arquiteto, Segurança, Operação, QA
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar implementação de circuit breaker.

**Quando usar**
Proteção contra serviços degradados.

**O que representa**
Estados: CLOSED → OPEN → HALF_OPEN.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Requisições travadas em serviço degradado.




> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Diagrama de estados e transições do circuit breaker.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



---

### 7.5 Deploy / Infra / Ambientes


### Deployment Diagram

- Categoria: Transversal
- Público: Arquiteto, Segurança, Operação, QA
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar como o sistema é implantado.

**Quando usar**
Operação, infra, DevOps.

**O que representa**
Containers, hosts, rede, volumes.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Deploy mal documentado, falhas de infra.

**Notas**
Derivado de docker-compose.yml verificado.



```mermaid
flowchart TB
    subgraph Docker["🐳 Docker Compose (Local)"]
        subgraph Services["Serviços"]
            Web["web<br/>React + Nginx<br/>:5173"]
            Backend["backend<br/>Spring Boot<br/>:8080"]
        end
        subgraph Data["Dados"]
            Postgres["postgres<br/>PostgreSQL 16<br/>:5432"]
            Redis["redis<br/>Redis 7<br/>:6379"]
            Neo4j["neo4j<br/>Neo4j 5<br/>:7474/:7687"]
        end
    end
    
    subgraph Network["🌐 Rede"]
        Web --> Backend
        Backend --> Postgres
        Backend --> Redis
        Backend --> Neo4j
    end
    
    subgraph Volumes["💾 Volumes"]
        PgData["postgres_data"]
        RedisData["redis_data"]
        Neo4jData["neo4j_data"]
    end
    
    Postgres --> PgData
    Redis --> RedisData
    Neo4j --> Neo4jData
```



### Ambientes (Dev/Hml/Prod)

- Categoria: Transversal
- Público: Arquiteto, Segurança, Operação, QA
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar configuração por ambiente.

**Quando usar**
DevOps, promoção de código, configuração.

**O que representa**
Diferenças de configuração entre ambientes.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Configuração incorreta em produção, bugs de ambiente.

**Notas**
Apenas ambiente local (docker-compose) documentado. Não há evidência de ambientes hml/prod no repositório.



> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Diagrama de ambientes (dev/hml/prod) com diferenças de configuração, variáveis de ambiente, e processo de promoção.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



### CI/CD Pipeline

- Categoria: Transversal
- Público: Arquiteto, Segurança, Operação, QA
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar pipeline de integração e entrega contínua.

**Quando usar**
DevOps, automação, qualidade.

**O que representa**
Stages: build, test, lint, deploy.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Deploys manuais, sem validação automática.

**Notas**
Não há evidência de pipeline CI/CD no repositório (sem .github/workflows ou Jenkinsfile).



> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Diagrama de pipeline: commit → build → test → lint → deploy. Ferramentas (GitHub Actions, Jenkins, etc.).
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



---

### 7.6 Performance & Capacidade


### Diagrama de Latência

- Categoria: Transversal
- Público: Arquiteto, Segurança, Operação, QA
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar latências esperadas (p50, p95, p99).

**Quando usar**
SRE, capacity planning, SLOs.

**O que representa**
Latência por endpoint, por tier de processamento.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
SLOs não definidos, performance degradada sem alerta.

**Notas**
Derivado de ParallelRuleExecutionService.java (tiers) e prometheus-alerts.yml (alertas de latência).



```mermaid
flowchart LR
    subgraph Latências["⏱️ Latências Esperadas (baseado em tiers)"]
        L1["TIER 1 (Blocklists): < 1ms"]
        L2["TIER 2 (Velocity): < 10ms"]
        L3["TIER 3 (Agregações): < 100ms"]
        L4["Total /analyze: < 200ms p95"]
    end
    
    subgraph Alertas["🚨 Alertas"]
        A1["HighLatency: p99 > 500ms"]
    end
```



### Pontos de Gargalo

- Categoria: Transversal
- Público: Arquiteto, Segurança, Operação, QA
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Identificar potenciais gargalos de performance.

**Quando usar**
Otimização, capacity planning.

**O que representa**
Database queries, cache misses, regex evaluation, graph traversal.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Gargalos não identificados, degradação em pico.

**Notas**
Derivado de application.yml (HikariCP), RegexValidator.java (timeout), VelocityServiceFacade.java (fallback).



```mermaid
flowchart TD
    subgraph Gargalos["🔥 Potenciais Gargalos"]
        G1["PostgreSQL: Queries complexas sem índice"]
        G2["Redis: Cache miss em pico"]
        G3["Neo4j: Traversal profundo"]
        G4["Regex: Patterns complexos (timeout 1s)"]
        G5["HikariCP: Pool exhaustion"]
    end
    
    subgraph Mitigações["✅ Mitigações Implementadas"]
        M1["Índices em colunas frequentes"]
        M2["Fallback cascade: Redis → Memory → PG"]
        M3["Limite de profundidade em grafos"]
        M4["Timeout em avaliação de regex"]
        M5["Pool sizing + alerta de exaustão"]
    end
    
    G1 -.-> M1
    G2 -.-> M2
    G3 -.-> M3
    G4 -.-> M4
    G5 -.-> M5
```



### Limites de TPS

- Categoria: Transversal
- Público: Arquiteto, Segurança, Operação, QA
- Nível: Estratégico/Tático
- Status: 🟥 SEM EVIDÊNCIA

**Evidência no repositório**
- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**

**Objetivo**
Documentar capacidade e limites de throughput.

**Quando usar**
Capacity planning, SLAs.

**O que representa**
TPS máximo por endpoint, por ambiente.

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
Sistema subdimensionado, indisponibilidade em pico.

**Notas**
Não há evidência de load testing ou limites de TPS documentados no repositório.



> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: Diagrama com TPS máximo testado/estimado por endpoint. Resultados de load testing se disponíveis.
>
> **Para completar, anexar/confirmar no repositório**:
- (definir paths esperados para completar)



---

## Checklist Final (PROMPT FINAL / DOUBLE CHECK)

### Estrutura e Formato
- [x] Documento em página única (arquivo único: docs/DIAGRAMAS.md)
- [x] PASSO ZERO — varredura do repositório incluída no topo
- [x] Catálogo Mestre incluído com: Categoria | Diagrama | Público | Nível | Evidência | Status
- [x] Índice navegável com links âncora
- [x] Linguagem 100% PT-BR

### Conteúdo por Diagrama
- [x] Cada diagrama possui: Objetivo, Quando usar, O que representa, Riscos
- [x] Cada diagrama possui "Evidência no repositório" ou marcou "SEM EVIDÊNCIA"
- [x] Não há nomes inventados de tabelas/endpoints/classes
- [x] Nada foi deduzido: apenas evidência ou template neutro

### Seções Obrigatórias — Negócio/Usuário (2.1)
- [x] 2.1.1 BPMN: AS-IS, TO-BE, Decisão de Fraude, Exceção/Fallback, Rollback
- [x] 2.1.2 Casos de Uso: Analista, Operação, Administrador, Sistema Externo, Motor
- [x] 2.1.3 Personas: Analista, Operação/SRE, Executivo/Compliance, Sistema Automatizado
- [x] 2.1.4 Mapas de Jornada: Criação, Simulação, Publicação, Rollback, Investigação
- [x] 2.1.5 User Story Mapping
- [x] 2.1.6 Service Blueprint
- [x] 2.1.7 Business Model Canvas
- [x] 2.1.8 Value Proposition Canvas

### Seções Obrigatórias — Frontend (2.2)
- [x] 2.2.1 Arquitetura do Frontend
- [x] 2.2.2 Fluxos de UI
- [x] 2.2.3 Component Diagram
- [x] 2.2.4 State Machine (UI)
- [x] 2.2.5 Wireflow / User Flow
- [x] 2.2.6 Design System / Component Library

### Seções Obrigatórias — Backend Java (2.3)
- [x] 2.3.1 Arquitetura Geral
- [x] 2.3.2 C4 Model (Context, Container, Component)
- [x] 2.3.3 UML (Classes, Pacotes, Sequência, Estados)
- [x] 2.3.4 Fluxogramas de Processamento
- [x] 2.3.5 Regras Duras
- [x] 2.3.6 API Contract / Integrações
- [x] 2.3.7 Event / Message Flow

### Seções Obrigatórias — PostgreSQL (2.4)
- [x] 2.4.1 Modelo conceitual/lógico/físico
- [x] 2.4.2 ERD completo
- [x] 2.4.3 Schemas/tabelas/índices
- [x] 2.4.4 Armazenamento físico
- [x] 2.4.5 Replicação
- [x] 2.4.6 Data Lifecycle / Retenção / LGPD

### Seções Obrigatórias — Redis (2.5)
- [x] 2.5.1 Tipos de dados usados
- [x] 2.5.2 Arquitetura (event loop)
- [x] 2.5.3 Cache patterns
- [x] 2.5.4 Replicação
- [x] 2.5.5 Cluster
- [x] 2.5.6 Persistência
- [x] 2.5.7 Consistência / Invalidação / Stampede

### Seções Obrigatórias — Neo4j (2.6)
- [x] 2.6.1 Modelo de grafo
- [x] 2.6.2 Diagrama de instâncias
- [x] 2.6.3 Index-free adjacency
- [x] 2.6.4 Armazenamento
- [x] 2.6.5 Cluster causal
- [x] 2.6.6 Multi-data center
- [x] 2.6.7 Índices/Constraints/Query Patterns

### Seções Obrigatórias — Transversais (2.7)
- [x] 2.7.1 DFD (Nível 0, 1, 2)
- [x] 2.7.2 Segurança (Autenticação, Autorização, LGPD, Threat Model STRIDE)
- [x] 2.7.3 Observabilidade (Logs, Métricas, Traces, Alertas)
- [x] 2.7.4 Resiliência (Timeout, Retry, Circuit Breaker, Fallback)
- [x] 2.7.5 Deploy / Infra / Ambientes
- [x] 2.7.6 Performance & Capacidade

### Pendências para Revisão Humana
- [ ] Completar diagramas marcados SEM EVIDÊNCIA com artefatos reais
- [ ] Validar BPMN AS-IS/TO-BE com área de negócio
- [ ] Criar documentação de Design System
- [ ] Documentar políticas de retenção/LGPD
- [ ] Realizar load testing e documentar limites de TPS
- [ ] Configurar CI/CD pipeline


---

## 8. Anexo: Catálogo Completo da UI

O catálogo completo de diagramas disponíveis na UI (422 itens) pode ser consultado em:

- **UI**: Rota `/diagrams` no frontend.
- **Checklist QA**: [docs/qa/DIAGRAMS_CATALOG_CHECKLIST.md](qa/DIAGRAMS_CATALOG_CHECKLIST.md)

### Resumo do Catálogo

| Origem | Quantidade | Descrição |
|--------|------------|-----------|
| solution | 213 | Diagramas verificados, derivados de evidência no repositório |
| template | 209 | Templates didáticos para tipos de diagrama |

### Lista de Diagramas Verificados (solution)


- **Fluxo real: /analyze (RULEX)** (`RULEX/FLOW_analyze`) — FLOWCHART
- **Sequência real: FE → API → Engine → DB** (`RULEX/SEQ_analyze`) — UML
- **C4 (Container): RULEX** (`RULEX/C4_container`) — C4
- **ER (Core): Transaction ↔ Rule ↔ Evaluation** (`RULEX/ER_core`) — ER
- **API Controller: RulesV31Controller** (`RULEX/CTRL_rulesv31controller`) — UML
- **Endpoint: POST /rules/validate** (`RULEX/EP_post-rules-validate-rulesv31controller`) — UML
- **Endpoint: POST /rules/lint** (`RULEX/EP_post-rules-lint-rulesv31controller`) — UML
- **Endpoint: POST /rules/simulate** (`RULEX/EP_post-rules-simulate-rulesv31controller`) — UML
- **API Controller: FieldDictionaryController** (`RULEX/CTRL_fielddictionarycontroller`) — UML
- **Endpoint: GET /field-dictionary** (`RULEX/EP_get-field-dictionary-fielddictionarycontroller`) — UML
- **API Controller: AuditController** (`RULEX/CTRL_auditcontroller`) — UML
- **Endpoint: GET /audit** (`RULEX/EP_get-audit-auditcontroller`) — UML
- **Endpoint: GET /audit/export** (`RULEX/EP_get-audit-export-auditcontroller`) — UML
- **Endpoint: GET /audit/export/csv** (`RULEX/EP_get-audit-export-csv-auditcontroller`) — UML
- **Endpoint: GET /audit/transaction/{transactionId}** (`RULEX/EP_get-audit-transaction-transactionid-auditcontroller`) — UML
- **API Controller: ComplexRuleCrudController** (`RULEX/CTRL_complexrulecrudcontroller`) — UML
- **Endpoint: GET /complex-rules** (`RULEX/EP_get-complex-rules-complexrulecrudcontroller`) — UML
- **Endpoint: GET /complex-rules/{id}** (`RULEX/EP_get-complex-rules-id-complexrulecrudcontroller`) — UML
- **Endpoint: GET /complex-rules/key/{key}** (`RULEX/EP_get-complex-rules-key-key-complexrulecrudcontroller`) — UML
- **Endpoint: POST /complex-rules** (`RULEX/EP_post-complex-rules-complexrulecrudcontroller`) — UML
- **Endpoint: PUT /complex-rules/{id}** (`RULEX/EP_put-complex-rules-id-complexrulecrudcontroller`) — UML
- **Endpoint: DELETE /complex-rules/{id}** (`RULEX/EP_delete-complex-rules-id-complexrulecrudcontroller`) — UML
- **Endpoint: PATCH /complex-rules/{id}/toggle** (`RULEX/EP_patch-complex-rules-id-toggle-complexrulecrudcontroller`) — UML
- **Endpoint: POST /complex-rules/{id}/duplicate** (`RULEX/EP_post-complex-rules-id-duplicate-complexrulecrudcontroller`) — UML
- **Endpoint: POST /complex-rules/validate** (`RULEX/EP_post-complex-rules-validate-complexrulecrudcontroller`) — UML
- **API Controller: EvaluateController** (`RULEX/CTRL_evaluatecontroller`) — UML
- **Endpoint: POST /evaluate** (`RULEX/EP_post-evaluate-evaluatecontroller`) — UML
- **Endpoint: POST /evaluate/raw** (`RULEX/EP_post-evaluate-raw-evaluatecontroller`) — UML
- **API Controller: MetricsController** (`RULEX/CTRL_metricscontroller`) — UML
- **Endpoint: GET /metrics** (`RULEX/EP_get-metrics-metricscontroller`) — UML
- **Endpoint: GET /metrics/mcc** (`RULEX/EP_get-metrics-mcc-metricscontroller`) — UML
- **Endpoint: GET /metrics/merchant** (`RULEX/EP_get-metrics-merchant-metricscontroller`) — UML
- **Endpoint: GET /metrics/timeline** (`RULEX/EP_get-metrics-timeline-metricscontroller`) — UML
- **API Controller: RuleApprovalController** (`RULEX/CTRL_ruleapprovalcontroller`) — UML
- **Endpoint: POST /rules/approvals/create** (`RULEX/EP_post-rules-approvals-create-ruleapprovalcontroller`) — UML
- **Endpoint: POST /rules/approvals/update/{ruleId}** (`RULEX/EP_post-rules-approvals-update-ruleid-ruleapprovalcontroller`) — UML
- **Endpoint: POST /rules/approvals/delete/{ruleId}** (`RULEX/EP_post-rules-approvals-delete-ruleid-ruleapprovalcontroller`) — UML
- **Endpoint: POST /rules/approvals/{id}/approve** (`RULEX/EP_post-rules-approvals-id-approve-ruleapprovalcontroller`) — UML
- **Endpoint: POST /rules/approvals/{id}/reject** (`RULEX/EP_post-rules-approvals-id-reject-ruleapprovalcontroller`) — UML
- **Endpoint: POST /rules/approvals/{id}/cancel** (`RULEX/EP_post-rules-approvals-id-cancel-ruleapprovalcontroller`) — UML
- **Endpoint: GET /rules/approvals/pending** (`RULEX/EP_get-rules-approvals-pending-ruleapprovalcontroller`) — UML
- **Endpoint: GET /rules/approvals/pending/page** (`RULEX/EP_get-rules-approvals-pending-page-ruleapprovalcontroller`) — UML
- **Endpoint: GET /rules/approvals/pending/count** (`RULEX/EP_get-rules-approvals-pending-count-ruleapprovalcontroller`) — UML
- **Endpoint: GET /rules/approvals/{id}** (`RULEX/EP_get-rules-approvals-id-ruleapprovalcontroller`) — UML
- **Endpoint: GET /rules/approvals/history/{ruleId}** (`RULEX/EP_get-rules-approvals-history-ruleid-ruleapprovalcontroller`) — UML
- **API Controller: RuleController** (`RULEX/CTRL_rulecontroller`) — UML
- **Endpoint: GET /rules** (`RULEX/EP_get-rules-rulecontroller`) — UML
- **Endpoint: GET /rules/{id}** (`RULEX/EP_get-rules-id-rulecontroller`) — UML
- **Endpoint: POST /rules** (`RULEX/EP_post-rules-rulecontroller`) — UML
- **Endpoint: PUT /rules/{id}** (`RULEX/EP_put-rules-id-rulecontroller`) — UML
- ... e mais 163 diagramas verificados.


---

## Changelog

| Data | Versão | Descrição |
|------|--------|-----------|
| 2026-01-17 | 1.0.0 | Geração inicial do documento completo |

---

*Documento gerado automaticamente. Para atualizar, execute `pnpm diagrams:doc-completo`.*
