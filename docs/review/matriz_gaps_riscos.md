# MATRIZ DE GAPS E RISCOS — ANÁLISE DETALHADA
**Data**: 2025-12-19  
**Projeto**: RULEX Banking Rules Engine  
**Versão**: 1.0.0

---

## ÍNDICE
1. [Matriz de Gaps por Prioridade](#matriz-de-gaps-por-prioridade)
2. [Matriz de Riscos por Severidade](#matriz-de-riscos-por-severidade)
3. [Gaps por Especialista](#gaps-por-especialista)
4. [Riscos por Categoria](#riscos-por-categoria)
5. [Plano de Mitigação](#plano-de-mitigação)

---

## MATRIZ DE GAPS POR PRIORIDADE

### ❌ GAPS P0 (BLOQUEADORES ABSOLUTOS)

| # | GAP | EVIDÊNCIA | ESPECIALISTAS | IMPACTO | MITIGAÇÃO | ESFORÇO |
|---|-----|-----------|---------------|---------|-----------|---------|
| **P0-1** | **Senha Hardcoded** | `backend/src/main/resources/application.yml` (linha 12): `password: postgres` | AppSec (2.0), Backend (5.8), DBA (6.2) | **CRÍTICO**: Vulnerabilidade CWE-798, senha exposta em Git | Usar variável de ambiente `POSTGRES_PASSWORD` ou Kubernetes Secret | **1 dia** |
| **P0-2** | **Sem Autenticação/Autorização** | Nenhum controller implementa Spring Security ou OAuth2 | AppSec (2.0) | **CRÍTICO**: Qualquer pessoa pode acessar APIs críticas (deletar todas as regras) | Implementar Spring Security + JWT ou OAuth2 com roles (ADMIN, ANALYST, VIEWER) | **3-5 dias** |
| **P0-3** | **`fixtures/crtran.json` NÃO EXISTE** | `backend/src/test/java/com/rulex/controller/CrtranBaselineIT.java` (linhas 64-81) procura arquivo inexistente | Negócio (6.5), Backend (5.8), QA (4.5) | **ALTO**: Testes de baseline vão falhar, impossível validar regras com dados reais | Criar arquivo `fixtures/crtran.json` com payload realista (baseado em documentação) | **1 dia** |
| **P0-4** | **Sem CI/CD** | Não existe `.github/workflows/` ou `.gitlab-ci.yml` | QA (4.5), DevOps (4.0) | **CRÍTICO**: Testes não são executados automaticamente, código quebrado pode ir para produção | Criar GitHub Actions workflow com: testes, linting, build, deploy para staging | **2-3 dias** |
| **P0-5** | **Sem Healthcheck Endpoint** | Não existe `/actuator/health` ou similar | Backend (5.8), DevOps (4.0) | **ALTO**: Deploy em Kubernetes vai falhar (liveness/readiness probes) | Adicionar Spring Boot Actuator e expor `/actuator/health` | **0.5 dia** |
| **P0-6** | **Teste ArchUnit NÃO VALIDA MÓDULO CORE** | `CleanArchitectureRulesTest.java` apenas valida `com.rulex.homolog`, módulo core não é validado | Arquiteto (6.8) | **ALTO**: Degradação arquitetural no core é possível | Estender `CleanArchitectureRulesTest` para validar `com.rulex.service`, `com.rulex.controller` | **1 dia** |

**TOTAL DE GAPS P0**: **6 gaps**  
**ESFORÇO TOTAL**: **9-12 dias úteis**

---

### ⚠️ GAPS P1 (IMPORTANTES, MAS NÃO BLOQUEADORES)

| # | GAP | EVIDÊNCIA | ESPECIALISTAS | IMPACTO | MITIGAÇÃO | ESFORÇO |
|---|-----|-----------|---------------|---------|-----------|---------|
| **P1-1** | **Sem HTTPS Enforced** | `application.yml` não força HTTPS | AppSec (2.0) | **ALTO**: Dados sensíveis (PAN mascarado) trafegam em plaintext, vulnerável a MitM | Configurar TLS/SSL em Nginx ou K8s Ingress, forçar redirect HTTP→HTTPS | **1 dia** |
| **P1-2** | **Sem Matriz de Cobertura de Regras** | Documentação menciona 28 regras avançadas, mas não há matriz "Regra X | Teste X | Status" | Negócio (6.5), QA (4.5) | **MÉDIO**: Não sabemos quais regras foram testadas, rastreabilidade é impossível | Criar matriz em `docs/hml/rule-inventory.md` e validar com testes | **2 dias** |
| **P1-3** | **Sem Observabilidade (Prometheus/Grafana)** | Não há métricas exportadas no formato Prometheus | Backend (5.8), DevOps (4.0) | **ALTO**: Impossível monitorar performance em produção, MTTR alto | Adicionar Micrometer + Prometheus endpoint + Grafana dashboards | **3 dias** |
| **P1-4** | **Sem Backup Automatizado de Postgres** | Não há evidência de backup automatizado | DBA (6.2), DevOps (4.0) | **ALTO**: Perda de dados é risco real, RTO/RPO não são garantidos | Configurar backup diário com retenção de 30 dias (pg_dump ou Velero) | **1 dia** |
| **P1-5** | **Sem LGPD Compliance** | Não há data retention policy nem endpoint "direito ao esquecimento" | AppSec (2.0) | **ALTO**: Não conformidade com LGPD, multas da ANPD são possíveis | Implementar purga automática após 5 anos + endpoint DELETE /api/customers/{id}/data | **3 dias** |
| **P1-6** | **Falta de Read Replicas** | Arquitetura não prevê read replicas | DBA (6.2) | **MÉDIO**: Queries de auditoria vão impactar write performance | Configurar read replica do Postgres, direcionar queries de leitura para replica | **2 dias** |
| **P1-7** | **Sem API Gateway** | Frontend faz chamadas diretas ao backend Java | Arquiteto (6.8) | **MÉDIO**: Sem rate limiting, throttling, ou circuit breaker na fronteira | Adicionar Kong ou NGINX como API Gateway com rate limiting | **3 dias** |
| **P1-8** | **Sem User Stories / Acceptance Criteria** | Não há arquivo com acceptance criteria detalhado | Product Owner (7.0) | **MÉDIO**: Homologação pode ser subjetiva, dificulta validação de requisitos | Criar backlog com user stories e acceptance criteria (formato Gherkin) | **2 dias** |
| **P1-9** | **Sem Fluxo de Onboarding** | Usuário novo não tem tutorial ou guia inicial | UX (7.2) | **MÉDIO**: Curva de aprendizado alta, adoção pode ser baixa | Criar wizard de onboarding com tour guiado (React Joyride) | **3 dias** |
| **P1-10** | **Sem Undo/Redo em Ações Críticas** | Deletar regra é irreversível | UX (7.2) | **MÉDIO**: Erros humanos são custosos | Adicionar toast "Desfazer" após deleção (soft delete com TTL de 30s) | **1 dia** |
| **P1-11** | **Sem Design Tokens Documentados** | Não há arquivo `design-tokens.json` | UI (7.5) | **MÉDIO**: Difícil manter consistência visual, mudanças de branding são custosas | Criar design tokens (cores, tipografia, espaçamentos) e exportar como CSS variables | **1 dia** |
| **P1-12** | **Sem Testes de Regra Antes de Salvar** | Usuário não pode testar regra com payload exemplo antes de salvar | Product Designer (6.8) | **MÉDIO**: Regras inválidas vão para produção | Adicionar botão "Testar Regra" em RuleBuilder com modal de resultado | **2 dias** |
| **P1-13** | **Sem Histórico Visual de Mudanças** | Endpoint `/api/rules/{id}/history` existe, mas frontend não mostra | Product Designer (6.8) | **MÉDIO**: Rastreabilidade visual inexistente | Criar página "Histórico de Regra" com timeline de mudanças | **2 dias** |
| **P1-14** | **Sem Error Boundary em Componentes Críticos** | `ErrorBoundary.tsx` existe, mas não é usado em `Rules.tsx` | Frontend (7.0) | **MÉDIO**: Erros podem quebrar toda a UI | Adicionar ErrorBoundary em rotas principais (Dashboard, Rules, Transactions) | **0.5 dia** |
| **P1-15** | **Sem Loading States em Mutações** | `handleSave` em `Rules.tsx` não mostra loading | Frontend (7.0) | **MÉDIO**: Usuário não sabe se requisição está pendente | Adicionar loading state e desabilitar botão durante save | **0.5 dia** |

**TOTAL DE GAPS P1**: **15 gaps**  
**ESFORÇO TOTAL**: **27.5 dias úteis**

---

### 🔵 GAPS P2 (DESEJÁVEIS, MELHORIAS FUTURAS)

| # | GAP | EVIDÊNCIA | ESPECIALISTAS | IMPACTO | MITIGAÇÃO | ESFORÇO |
|---|-----|-----------|---------------|---------|-----------|---------|
| **P2-1** | **Sem Rate Limiting** | Nenhum controller implementa rate limiting | Backend (5.8), AppSec (2.0) | **MÉDIO**: Vulnerável a DDoS e brute force | Adicionar Bucket4j ou Redis-based rate limiter (10 req/s por IP) | **2 dias** |
| **P2-2** | **Sem Cache Distribuído** | `application.yml` menciona `cache-enabled: true`, mas cache está apenas em memória | Arquiteto (6.8), Backend (5.8) | **BAIXO**: Cache não é compartilhado entre instâncias, escalabilidade limitada | Adicionar Redis para cache de regras | **2 dias** |
| **P2-3** | **Sem Particionamento de `transactions`** | Tabela `transactions` vai crescer indefinidamente | DBA (6.2) | **MÉDIO**: Queries vão ficar lentas após milhões de registros | Particionar por `transaction_date` (monthly ou quarterly) | **3 dias** |
| **P2-4** | **Sem Índices Compostos** | Queries filtradas precisam de índices compostos | DBA (6.2) | **BAIXO**: Queries como "transações do cliente X no período Y" são lentas | Criar índice composto em `(customer_id_from_header, transaction_date)` | **0.5 dia** |
| **P2-5** | **Sem Tratamento de Deadlock** | `@Transactional` não trata deadlocks de Postgres | Backend (5.8) | **BAIXO**: Deadlocks vão estourar exception sem retry | Adicionar retry com backoff exponencial (@Retryable) | **1 dia** |
| **P2-6** | **Sem Testes E2E de Frontend** | Não há Playwright ou Cypress | Frontend (7.0), QA (4.5) | **MÉDIO**: Não valida fluxo completo frontend → backend | Adicionar Playwright com testes E2E (login, create rule, analyze transaction) | **3 dias** |
| **P2-7** | **Sem Testes de Performance** | Não há testes de carga (JMeter, Gatling) | QA (4.5) | **MÉDIO**: Não sabemos se suporta 1000 TPS conforme documentação | Criar testes de carga com Gatling (cenários: 100, 500, 1000 TPS) | **2 dias** |
| **P2-8** | **Sem Dependency Check (OWASP)** | Não há OWASP Dependency Check ou Snyk | AppSec (2.0) | **MÉDIO**: Vulnerabilidades em bibliotecas não são detectadas | Adicionar OWASP Dependency Check no CI/CD | **0.5 dia** |
| **P2-9** | **Sem Helm Charts** | Não há Helm charts para deploy em Kubernetes | DevOps (4.0) | **BAIXO**: Deploy manual é error-prone, rollback é difícil | Criar Helm chart com values.yaml para staging/production | **2 dias** |
| **P2-10** | **Sem Horizontal Pod Autoscaler (HPA)** | Não há configuração de HPA | DevOps (4.0) | **BAIXO**: Sistema não escala automaticamente | Configurar HPA baseado em CPU/memória (min: 2, max: 10 pods) | **0.5 dia** |
| **P2-11** | **Sem Versionamento de API** | Endpoints não têm `/v1/` na URL | Arquiteto (6.8) | **BAIXO**: Breaking changes vão quebrar clientes | Adicionar `/v1/` prefix em todos os endpoints | **1 dia** |
| **P2-12** | **Sem Circuit Breaker** | Não há Resilience4j ou similar | Arquiteto (6.8) | **BAIXO**: Sistema não é resiliente a falhas de Postgres | Adicionar Resilience4j circuit breaker para queries de Postgres | **1 dia** |
| **P2-13** | **Sem Busca/Filtro em Campos de RuleBuilder** | Dropdown com 35+ campos, sem busca | Product Designer (6.8) | **BAIXO**: Usuário precisa scrollar muito | Adicionar busca em select de campos (react-select ou Combobox) | **1 dia** |
| **P2-14** | **Sem Templates de Regras** | Não há templates pré-configurados | Product Designer (6.8) | **BAIXO**: Usuário precisa criar do zero sempre | Criar templates ("Regra de Alto Valor", "Regra de País Suspeito") | **1 dia** |
| **P2-15** | **Sem Animações de Transição** | Modal abre/fecha sem animação suave | UI (7.5) | **BAIXO**: UX parece "robótica", percepção de qualidade é baixa | Adicionar animações com Framer Motion (fade in/out, slide) | **1 dia** |
| **P2-16** | **Sem Lazy Loading de Componentes** | Todos os componentes são importados estaticamente | Frontend (7.0) | **BAIXO**: Bundle JavaScript é grande, performance inicial é ruim | Adicionar React.lazy() e Suspense para rotas | **0.5 dia** |
| **P2-17** | **Sem React Query ou SWR** | `fetch` manual sem cache | Frontend (7.0) | **BAIXO**: Sem cache, UX é lenta (recarrega sempre) | Adicionar React Query com cache de 5 minutos | **1 dia** |
| **P2-18** | **Sem Modo Dark** | `ThemeContext.tsx` existe, mas não é usado | UX (7.2) | **BAIXO**: UX inconsistente com tendências modernas | Implementar modo dark com next-themes | **1 dia** |

**TOTAL DE GAPS P2**: **18 gaps**  
**ESFORÇO TOTAL**: **24 dias úteis**

---

## MATRIZ DE RISCOS POR SEVERIDADE

### 🔴 RISCOS CRÍTICOS (P0)

| # | RISCO | PROBABILIDADE | IMPACTO | EXPOSIÇÃO | EVIDÊNCIA | MITIGAÇÃO | RESPONSÁVEL |
|---|-------|---------------|---------|-----------|-----------|-----------|-------------|
| **R-P0-1** | **Vazamento de Senha do Banco de Dados** | **ALTA** (senha em Git) | **CRÍTICO** | **Imediato** | `application.yml` (linha 12): `password: postgres` em plaintext | Remover senha do código, usar variável de ambiente ou Kubernetes Secret | Backend + DevOps |
| **R-P0-2** | **Acesso Não Autorizado a APIs Críticas** | **ALTA** (APIs abertas) | **CRÍTICO** | **Imediato** | Nenhum endpoint implementa autenticação | Implementar Spring Security + JWT/OAuth2 | Backend + AppSec |
| **R-P0-3** | **Falha de Testes de Baseline** | **CERTA** (arquivo não existe) | **ALTO** | **Imediato** | `CrtranBaselineIT.java` procura `fixtures/crtran.json` inexistente | Criar arquivo `fixtures/crtran.json` | Negócio + QA |
| **R-P0-4** | **Deploy de Código Quebrado em Produção** | **ALTA** (sem CI/CD) | **CRÍTICO** | **Futuro** | Testes não são executados automaticamente | Criar CI/CD pipeline com testes obrigatórios | DevOps + QA |
| **R-P0-5** | **Pods Kubernetes Marcados como "Unhealthy"** | **CERTA** (sem healthcheck) | **ALTO** | **Deploy** | Não existe `/actuator/health` | Adicionar Spring Boot Actuator | Backend + DevOps |

**TOTAL DE RISCOS P0**: **5 riscos**  
**EXPOSIÇÃO TOTAL**: **IMEDIATA**

---

### 🟠 RISCOS ALTOS (P1)

| # | RISCO | PROBABILIDADE | IMPACTO | EXPOSIÇÃO | EVIDÊNCIA | MITIGAÇÃO | RESPONSÁVEL |
|---|-------|---------------|---------|-----------|-----------|-----------|-------------|
| **R-P1-1** | **Interceptação de Dados em Trânsito (MitM)** | **MÉDIA** | **ALTO** | **Produção** | HTTPS não é enforced | Configurar TLS/SSL e forçar redirect HTTP→HTTPS | DevOps + AppSec |
| **R-P1-2** | **Multas da ANPD por Não Conformidade LGPD** | **MÉDIA** | **ALTO** | **Produção** | Sem data retention policy nem "direito ao esquecimento" | Implementar purga automática e endpoint de deleção | Backend + AppSec |
| **R-P1-3** | **Perda de Dados Críticos** | **BAIXA** | **CRÍTICO** | **Produção** | Sem backup automatizado de Postgres | Configurar backup diário com retenção de 30 dias | DBA + DevOps |
| **R-P1-4** | **Degradação de Performance em Produção** | **MÉDIA** | **ALTO** | **Produção** | Sem observabilidade (Prometheus/Grafana) | Adicionar métricas e dashboards | Backend + DevOps |
| **R-P1-5** | **Regras Inválidas em Produção** | **MÉDIA** | **ALTO** | **Operação** | Usuário não pode testar regra antes de salvar | Adicionar botão "Testar Regra" com validação | Backend + Product |
| **R-P1-6** | **Impossibilidade de Rastrear Mudanças** | **ALTA** | **MÉDIO** | **Auditoria** | Histórico de regras não é exibido no frontend | Criar página "Histórico de Regra" | Frontend + Product |
| **R-P1-7** | **Impacto de Queries de Leitura em Writes** | **MÉDIA** | **MÉDIO** | **Produção** | Sem read replicas | Configurar read replica do Postgres | DBA + DevOps |
| **R-P1-8** | **Falha Total da UI por Erro Não Tratado** | **BAIXA** | **ALTO** | **Operação** | ErrorBoundary não é usado em componentes críticos | Adicionar ErrorBoundary em rotas principais | Frontend |

**TOTAL DE RISCOS P1**: **8 riscos**  
**EXPOSIÇÃO TOTAL**: **PRODUÇÃO + OPERAÇÃO**

---

### 🟡 RISCOS MÉDIOS (P2)

| # | RISCO | PROBABILIDADE | IMPACTO | EXPOSIÇÃO | EVIDÊNCIA | MITIGAÇÃO | RESPONSÁVEL |
|---|-------|---------------|---------|-----------|-----------|-----------|-------------|
| **R-P2-1** | **Ataque DDoS Bem-Sucedido** | **MÉDIA** | **MÉDIO** | **Produção** | Sem rate limiting | Adicionar rate limiter (10 req/s por IP) | Backend + DevOps |
| **R-P2-2** | **Performance Insuficiente (<1000 TPS)** | **MÉDIA** | **MÉDIO** | **Produção** | Sem testes de performance | Criar testes de carga com Gatling | QA + Backend |
| **R-P2-3** | **Queries Lentas Após Milhões de Registros** | **ALTA** | **MÉDIO** | **Futuro** | Tabela `transactions` sem particionamento | Particionar por `transaction_date` | DBA |
| **R-P2-4** | **Vulnerabilidades em Dependências** | **MÉDIA** | **MÉDIO** | **Produção** | Sem OWASP Dependency Check | Adicionar Dependency Check no CI/CD | AppSec + DevOps |
| **R-P2-5** | **Deadlocks Causam Falhas** | **BAIXA** | **MÉDIO** | **Produção** | Sem tratamento de deadlock | Adicionar retry com backoff exponencial | Backend |
| **R-P2-6** | **Escalabilidade Limitada (Cache Local)** | **MÉDIA** | **BAIXO** | **Produção** | Cache não é distribuído (Redis) | Adicionar Redis para cache de regras | Backend + DevOps |
| **R-P2-7** | **Breaking Changes Quebram Clientes** | **BAIXA** | **MÉDIO** | **Futuro** | Sem versionamento de API (/v1/) | Adicionar prefix /v1/ em endpoints | Backend + Arquiteto |
| **R-P2-8** | **Falha em Cascade por Postgres Indisponível** | **BAIXA** | **MÉDIO** | **Produção** | Sem circuit breaker | Adicionar Resilience4j circuit breaker | Backend + Arquiteto |

**TOTAL DE RISCOS P2**: **8 riscos**  
**EXPOSIÇÃO TOTAL**: **PRODUÇÃO + FUTURO**

---

## GAPS POR ESPECIALISTA

### Especialista com Mais Gaps Identificados

| ESPECIALISTA | GAPS P0 | GAPS P1 | GAPS P2 | TOTAL |
|--------------|---------|---------|---------|-------|
| **AppSec / Segurança** | 2 | 2 | 2 | **6** |
| **QA Engineer** | 2 | 1 | 2 | **5** |
| **DevOps / SRE** | 2 | 2 | 2 | **6** |
| **Backend Engineer Java** | 3 | 1 | 2 | **6** |
| **DBA / PostgreSQL** | 1 | 2 | 2 | **5** |
| **Product Designer** | 0 | 3 | 2 | **5** |
| **Arquiteto de Software** | 1 | 1 | 3 | **5** |
| **Frontend Engineer React** | 0 | 2 | 3 | **5** |
| **UX Designer** | 0 | 2 | 1 | **3** |
| **UI Designer** | 0 | 1 | 1 | **2** |
| **Negócio (Crédito/Fraude)** | 1 | 1 | 0 | **2** |
| **Product Owner Técnico** | 0 | 1 | 0 | **1** |

**Especialistas com visão mais crítica**:
1. **AppSec / Segurança** (6 gaps, nota 2.0/10)
2. **DevOps / SRE** (6 gaps, nota 4.0/10)
3. **Backend Engineer Java** (6 gaps, nota 5.8/10)

---

## RISCOS POR CATEGORIA

### Categoria: Segurança (AppSec)
- **Riscos P0**: 2 (senha hardcoded, sem autenticação)
- **Riscos P1**: 2 (sem HTTPS, sem LGPD)
- **Riscos P2**: 2 (sem rate limiting, vulnerabilidades em dependências)
- **TOTAL**: **6 riscos** ← **ÁREA MAIS CRÍTICA**

### Categoria: Infraestrutura (DevOps/DBA)
- **Riscos P0**: 2 (sem CI/CD, sem healthcheck)
- **Riscos P1**: 4 (sem backup, sem observabilidade, sem read replicas, sem API Gateway)
- **Riscos P2**: 5 (sem HPA, sem Helm charts, sem particionamento, sem circuit breaker, sem versionamento)
- **TOTAL**: **11 riscos** ← **ÁREA COM MAIS GAPS**

### Categoria: Qualidade (QA)
- **Riscos P0**: 1 (fixtures/crtran.json)
- **Riscos P1**: 1 (sem matriz de cobertura)
- **Riscos P2**: 2 (sem testes E2E, sem testes de performance)
- **TOTAL**: **4 riscos**

### Categoria: Produto/UX (Product/Design)
- **Riscos P0**: 0
- **Riscos P1**: 4 (sem onboarding, sem undo/redo, sem teste de regra, sem histórico visual)
- **Riscos P2**: 5 (sem templates, sem busca em campos, sem animações, sem modo dark, sem lazy loading)
- **TOTAL**: **9 riscos**

---

## PLANO DE MITIGAÇÃO

### FASE 1: BLOQUEADORES (P0) — 9-12 dias úteis
**Objetivo**: Resolver gaps bloqueadores para possibilitar homologação

| SEMANA | ATIVIDADE | RESPONSÁVEL | STATUS |
|--------|-----------|-------------|--------|
| **Semana 1** | Remover senha hardcoded, usar variável de ambiente | Backend + DevOps | ❌ Pendente |
| **Semana 1** | Criar arquivo `fixtures/crtran.json` | Negócio + QA | ❌ Pendente |
| **Semana 1** | Adicionar healthcheck `/actuator/health` | Backend | ❌ Pendente |
| **Semana 1-2** | Implementar Spring Security + JWT | Backend + AppSec | ❌ Pendente |
| **Semana 1-2** | Criar CI/CD pipeline (GitHub Actions) | DevOps + QA | ❌ Pendente |
| **Semana 2** | Estender teste ArchUnit para módulo core | Arquiteto + Backend | ❌ Pendente |

**Critério de Sucesso**: Todos os gaps P0 resolvidos, sistema pronto para homologação.

---

### FASE 2: PRODUÇÃO (P1) — 27.5 dias úteis (~6 semanas)
**Objetivo**: Resolver gaps importantes para produção segura

| MÊS | ATIVIDADE | RESPONSÁVEL | STATUS |
|-----|-----------|-------------|--------|
| **Mês 1** | Configurar HTTPS enforced (TLS/SSL) | DevOps | ❌ Pendente |
| **Mês 1** | Criar matriz de cobertura de regras | QA + Negócio | ❌ Pendente |
| **Mês 1** | Adicionar Prometheus + Grafana | DevOps + Backend | ❌ Pendente |
| **Mês 1** | Configurar backup automatizado (diário) | DBA + DevOps | ❌ Pendente |
| **Mês 1** | Implementar LGPD compliance (purga + endpoint) | Backend + AppSec | ❌ Pendente |
| **Mês 1** | Configurar read replica do Postgres | DBA | ❌ Pendente |
| **Mês 1** | Adicionar API Gateway (Kong ou NGINX) | DevOps + Arquiteto | ❌ Pendente |
| **Mês 1-2** | Criar user stories com acceptance criteria | Product Owner | ❌ Pendente |
| **Mês 2** | Implementar onboarding wizard | Frontend + UX | ❌ Pendente |
| **Mês 2** | Adicionar undo/redo em deleção de regras | Frontend + UX | ❌ Pendente |
| **Mês 2** | Documentar design tokens | UI + Frontend | ❌ Pendente |
| **Mês 2** | Adicionar teste de regra (botão "Testar") | Backend + Product | ❌ Pendente |
| **Mês 2** | Criar página "Histórico de Regra" | Frontend + Product | ❌ Pendente |
| **Mês 2** | Adicionar ErrorBoundary em rotas | Frontend | ❌ Pendente |
| **Mês 2** | Adicionar loading states em mutações | Frontend | ❌ Pendente |

**Critério de Sucesso**: Sistema pronto para produção com observabilidade, backup, LGPD, e UX polido.

---

### FASE 3: OTIMIZAÇÕES (P2) — 24 dias úteis (~5 semanas)
**Objetivo**: Melhorias de performance, escalabilidade, e UX

| TRIMESTRE | ATIVIDADE | RESPONSÁVEL | STATUS |
|-----------|-----------|-------------|--------|
| **Q1** | Adicionar rate limiting (Bucket4j) | Backend | ❌ Pendente |
| **Q1** | Adicionar Redis para cache distribuído | Backend + DevOps | ❌ Pendente |
| **Q1** | Particionar tabela `transactions` por data | DBA | ❌ Pendente |
| **Q1** | Criar índices compostos | DBA | ❌ Pendente |
| **Q1** | Adicionar retry para deadlocks | Backend | ❌ Pendente |
| **Q1** | Criar testes E2E com Playwright | Frontend + QA | ❌ Pendente |
| **Q1** | Criar testes de performance (Gatling) | QA + Backend | ❌ Pendente |
| **Q1** | Adicionar OWASP Dependency Check | AppSec + DevOps | ❌ Pendente |
| **Q1** | Criar Helm charts | DevOps | ❌ Pendente |
| **Q1** | Configurar HPA (Kubernetes) | DevOps | ❌ Pendente |
| **Q1** | Adicionar versionamento de API (/v1/) | Backend + Arquiteto | ❌ Pendente |
| **Q1** | Adicionar circuit breaker (Resilience4j) | Backend + Arquiteto | ❌ Pendente |
| **Q1** | Adicionar busca em select de campos | Frontend + Product | ❌ Pendente |
| **Q1** | Criar templates de regras | Backend + Product | ❌ Pendente |
| **Q1** | Adicionar animações (Framer Motion) | Frontend + UI | ❌ Pendente |
| **Q1** | Adicionar lazy loading de componentes | Frontend | ❌ Pendente |
| **Q1** | Adicionar React Query | Frontend | ❌ Pendente |
| **Q1** | Implementar modo dark | Frontend + UX | ❌ Pendente |

**Critério de Sucesso**: Sistema escalável, performático, e com UX moderna.

---

## CONCLUSÃO

A matriz de gaps e riscos revela **39 gaps** (6 P0, 15 P1, 18 P2) e **21 riscos** (5 P0, 8 P1, 8 P2).

### Priorização de Esforço
- **FASE 1 (P0)**: **9-12 dias úteis** ← **BLOQUEADORES PARA HOMOLOGAÇÃO**
- **FASE 2 (P1)**: **27.5 dias úteis** ← **PREPARAÇÃO PARA PRODUÇÃO**
- **FASE 3 (P2)**: **24 dias úteis** ← **OTIMIZAÇÕES FUTURAS**

**ESFORÇO TOTAL ESTIMADO**: **60.5-63.5 dias úteis** (~12-13 semanas, ~3 meses)

### Recomendação
**FOCAR NA FASE 1 (P0) IMEDIATAMENTE** para viabilizar homologação. Apenas após resolver os 6 gaps P0, o sistema pode ser reavaliado para homologação.
