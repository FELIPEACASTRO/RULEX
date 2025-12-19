# 🔴 Matriz de Gaps e Riscos — Motor de Regras Duras

> **Data da Análise**: 19/12/2025
> **Critério**: GAP = funcionalidade esperada NÃO ENCONTRADA no código
> **Metodologia**: Análise baseada exclusivamente em evidências de código

---

## 📊 Sumário Executivo

| Categoria | P0 (Bloqueio) | P1 (Crítico) | P2 (Importante) | Total |
|-----------|---------------|--------------|-----------------|-------|
| Segurança | 2 | 5 | 2 | 9 |
| QA/Testes | 1 | 4 | 0 | 5 |
| DevOps | 1 | 4 | 2 | 7 |
| Negócio/Regras | 0 | 4 | 2 | 6 |
| Frontend | 0 | 3 | 2 | 5 |
| Arquitetura | 0 | 2 | 3 | 5 |
| Banco de Dados | 0 | 1 | 4 | 5 |
| **TOTAL** | **4** | **23** | **15** | **42** |

---

## 🔴 GAPS P0 — BLOQUEADORES DE HOMOLOGAÇÃO

### GAP-001: Autenticação/Autorização ausente na API Java
- **Área**: Segurança (AppSec)
- **Prioridade**: P0
- **Descrição**: Os endpoints REST do backend Java (`/api/transactions/*`, `/api/rules/*`, `/api/audit/*`, `/api/metrics/*`) não possuem nenhum mecanismo de autenticação ou autorização.
- **Evidência NEGATIVA**: Nenhum `@PreAuthorize`, `@Secured`, `SecurityFilterChain` ou JWT validation encontrado nos controllers.
- **Arquivos verificados**:
  - `backend/src/main/java/com/rulex/controller/TransactionController.java`
  - `backend/src/main/java/com/rulex/controller/RuleController.java`
  - `backend/src/main/java/com/rulex/controller/AuditController.java`
- **Impacto**: Qualquer agente malicioso pode enviar transações falsas, alterar regras ou exfiltrar dados.
- **Remediação**: Implementar Spring Security com JWT ou OAuth2.

### GAP-002: Rate Limiting ausente
- **Área**: Segurança (AppSec)
- **Prioridade**: P0
- **Descrição**: Não há proteção contra ataques de negação de serviço (DoS) ou brute force. Os endpoints podem ser chamados ilimitadamente.
- **Evidência NEGATIVA**: Nenhum `@RateLimiter`, `Bucket4j`, ou configuração de throttling encontrada.
- **Impacto**: Sistema vulnerável a ataques que podem indisponibilizá-lo ou gerar custos excessivos.
- **Remediação**: Implementar rate limiting por IP/API key (ex: Bucket4j, Redis).

### GAP-003: Testes E2E automatizados ausentes
- **Área**: QA
- **Prioridade**: P0
- **Descrição**: Não existem testes end-to-end que validem o fluxo completo (frontend → backend → banco → resposta).
- **Evidência NEGATIVA**: Nenhum arquivo `.spec.ts` (Playwright) ou `cypress/` encontrado.
- **Arquivos verificados**: Glob `**/*.spec.ts`, `**/cypress/**`, `**/e2e/**`
- **Impacto**: Regressões críticas podem passar despercebidas em deploys.
- **Remediação**: Implementar Playwright ou Cypress com cenários críticos.

### GAP-004: Pipeline CI/CD ausente
- **Área**: DevOps
- **Prioridade**: P0
- **Descrição**: Não existe automação de build, test e deploy.
- **Evidência NEGATIVA**: Nenhum `.github/workflows/`, `Jenkinsfile`, `.gitlab-ci.yml`, `azure-pipelines.yml` encontrado.
- **Impacto**: Deploys manuais propensos a erros, sem validação automática antes de produção.
- **Remediação**: Criar pipeline CI/CD com stages de build, test, security scan e deploy.

---

## 🟠 GAPS P1 — CRÍTICOS

### GAP-005: Regras de velocidade sem cache
- **Área**: Negócio/Regras
- **Prioridade**: P1
- **Descrição**: As regras de velocidade (VELOCITY_*) fazem queries no PostgreSQL a cada transação. Não há Redis ou cache para contagem em janela temporal.
- **Evidência**: `TransactionRepository.java:46-48` faz `COUNT` no banco.
- **Impacto**: Performance degradada em alto volume; regras de velocidade ineficientes.
- **Remediação**: Implementar Redis para contadores com TTL.

### GAP-006: Regras geográficas não implementadas
- **Área**: Negócio/Regras
- **Prioridade**: P1
- **Descrição**: Regras como GEO_IMPOSSIBLE_DISTANCE, GEO_MULTI_COUNTRY_1HOUR documentadas em `REGRAS_DURAS_60_IMPLEMENTACAO.md` não existem no código.
- **Evidência NEGATIVA**: Grep por "GEO_" em Java retorna 0 resultados.
- **Impacto**: Fraudes geográficas não detectadas (viagem impossível, múltiplos países).
- **Remediação**: Implementar cálculo de distância e validação de tempo.

### GAP-007: Blacklist de cartões ausente
- **Área**: Negócio/Regras
- **Prioridade**: P1
- **Descrição**: Não existe tabela ou consulta para cartões/clientes em lista negra.
- **Evidência NEGATIVA**: Nenhuma tabela `blacklist` ou `blocked_cards` no schema.
- **Impacto**: Cartões roubados/fraudulentos conhecidos continuam sendo aceitos.
- **Remediação**: Criar tabela de blacklist com consulta no fluxo de análise.

### GAP-008: Card Testing Detection ausente
- **Área**: Negócio/Regras
- **Prioridade**: P1
- **Descrição**: Padrão de sequência falha→sucesso (CARD_TESTING_FAIL_SUCCESS_SEQUENCE) não implementado.
- **Evidência NEGATIVA**: Nenhuma lógica de sequência de autenticação encontrada.
- **Impacto**: Card testing (validação de cartões roubados) não detectado.
- **Remediação**: Implementar detecção de sequência temporal de falhas.

### GAP-009: Workflow de aprovação 4-eyes
- **Área**: Product Owner
- **Prioridade**: P1
- **Descrição**: Regras podem ser criadas/ativadas sem aprovação de segundo usuário.
- **Evidência NEGATIVA**: Nenhum campo `approved_by`, `pending_approval` ou workflow state machine.
- **Impacto**: Alterações maliciosas ou erros podem ir para produção imediatamente.
- **Remediação**: Implementar workflow de aprovação com estados DRAFT→PENDING→APPROVED.

### GAP-010: Cache de regras ausente
- **Área**: Backend Java
- **Prioridade**: P1
- **Descrição**: `RuleConfigurationRepository.findByEnabled(true)` é chamado a cada transação.
- **Evidência**: `RuleEngineService.java:147`
- **Impacto**: N queries por transação, performance degradada.
- **Remediação**: Cache local com invalidação por evento (Caffeine + listener).

### GAP-011: Cobertura de código (JaCoCo) ausente
- **Área**: QA
- **Prioridade**: P1
- **Descrição**: Não há relatório de cobertura de testes.
- **Evidência NEGATIVA**: Nenhum plugin JaCoCo no `pom.xml`, nenhum `coverage/` no repo.
- **Impacto**: Áreas não testadas desconhecidas.
- **Remediação**: Configurar JaCoCo com mínimo de 70%.

### GAP-012: Testes de carga ausentes
- **Área**: QA
- **Prioridade**: P1
- **Descrição**: Não há evidência de testes de performance/stress.
- **Evidência NEGATIVA**: Nenhum `*.jmx`, `k6.js`, `loadtest.yml` encontrado.
- **Impacto**: Comportamento sob carga desconhecido; pode falhar em produção.
- **Remediação**: Criar baseline com k6 ou JMeter.

### GAP-013: Testes de outras páginas frontend
- **Área**: Frontend
- **Prioridade**: P1
- **Descrição**: Apenas `Rules.test.tsx` existe. Outras páginas (Dashboard, Transactions, Audit) sem testes.
- **Evidência**: Glob `**/*.test.tsx` retorna apenas 1 arquivo.
- **Impacto**: Regressões em páginas críticas não detectadas.
- **Remediação**: Criar testes para todas as páginas.

### GAP-014: Tratamento de erros visual no frontend
- **Área**: Frontend
- **Prioridade**: P1
- **Descrição**: Erros de API são logados no console, não exibidos ao usuário.
- **Evidência**: `Rules.tsx:57-58` - `console.error('Erro ao buscar regras:', error)`
- **Impacto**: Usuário não sabe que operação falhou.
- **Remediação**: Usar toast/alert para feedback de erro.

### GAP-015: Builder visual de condições
- **Área**: UX
- **Prioridade**: P1
- **Descrição**: Condições de regras são JSON bruto, editado manualmente.
- **Evidência**: `RuleConfiguration.conditionsJson` é TEXT, sem UI de construção.
- **Impacto**: Usuários não técnicos não conseguem criar regras complexas; erros de JSON.
- **Remediação**: Implementar RuleBuilder visual com drag-and-drop.

### GAP-016: LGPD - Direito ao esquecimento
- **Área**: Segurança (LGPD)
- **Prioridade**: P1
- **Descrição**: Não há endpoint para exclusão de dados pessoais.
- **Evidência NEGATIVA**: Nenhum endpoint DELETE para dados de cliente.
- **Impacto**: Não conformidade com LGPD Art. 18.
- **Remediação**: Implementar endpoint de anonimização/exclusão.

### GAP-017: LGPD - Controle de retenção
- **Área**: Segurança (LGPD)
- **Prioridade**: P1
- **Descrição**: Dados de transação armazenados indefinidamente.
- **Evidência NEGATIVA**: Nenhuma política de retenção ou job de expurgo.
- **Impacto**: Não conformidade com LGPD (minimização de dados).
- **Remediação**: Implementar política de retenção com expurgo automatizado.

### GAP-018: Headers de segurança HTTP
- **Área**: Segurança
- **Prioridade**: P1
- **Descrição**: Sem Content-Security-Policy, X-Frame-Options, Strict-Transport-Security.
- **Evidência NEGATIVA**: Nenhuma configuração de security headers.
- **Impacto**: Vulnerável a XSS, clickjacking, downgrade attacks.
- **Remediação**: Configurar security headers no Spring Security ou reverse proxy.

### GAP-019: Criptografia em trânsito (HSTS)
- **Área**: Segurança
- **Prioridade**: P1
- **Descrição**: Sem HTTP Strict Transport Security.
- **Evidência NEGATIVA**: Nenhum header HSTS configurado.
- **Impacto**: Conexões podem ser interceptadas via downgrade para HTTP.
- **Remediação**: Configurar HSTS com max-age mínimo de 1 ano.

### GAP-020: Backup automatizado de banco
- **Área**: DBA
- **Prioridade**: P1
- **Descrição**: Sem evidência de rotina de backup.
- **Evidência NEGATIVA**: Nenhum script de backup, pg_dump, ou configuração de RDS.
- **Impacto**: Perda de dados em caso de falha.
- **Remediação**: Configurar backup automatizado (pg_dump cron ou RDS automated backups).

### GAP-021: Kubernetes manifests
- **Área**: DevOps
- **Prioridade**: P1
- **Descrição**: Apenas docker-compose, sem manifests para orquestração em produção.
- **Evidência NEGATIVA**: Nenhum `k8s/`, `helm/`, `deployment.yaml`.
- **Impacto**: Deploy em produção não padronizado.
- **Remediação**: Criar Kubernetes manifests ou Helm charts.

### GAP-022: Observabilidade (logs estruturados)
- **Área**: DevOps
- **Prioridade**: P1
- **Descrição**: Logs em texto plano, não JSON estruturado.
- **Evidência**: Logs com `log.info()` padrão, sem JSON layout.
- **Impacto**: Difícil agregar e buscar logs em produção.
- **Remediação**: Configurar logback-json ou similar.

### GAP-023: APM (traces distribuídos)
- **Área**: DevOps
- **Prioridade**: P1
- **Descrição**: Sem OpenTelemetry, Jaeger ou similar.
- **Evidência NEGATIVA**: Nenhuma dependência de tracing.
- **Impacto**: Debugging de problemas em produção muito difícil.
- **Remediação**: Implementar OpenTelemetry com exportação para Jaeger/Zipkin.

### GAP-024: Alertas automatizados
- **Área**: DevOps
- **Prioridade**: P1
- **Descrição**: Sem integração com PagerDuty, OpsGenie ou similar.
- **Evidência NEGATIVA**: Nenhuma configuração de alerting.
- **Impacto**: Incidentes não detectados/comunicados automaticamente.
- **Remediação**: Configurar alertas via Prometheus Alertmanager ou CloudWatch Alarms.

### GAP-025: Cache distribuído (Redis)
- **Área**: Arquitetura
- **Prioridade**: P1
- **Descrição**: Sem Redis para cache e contadores de velocidade.
- **Evidência NEGATIVA**: Nenhuma dependência `spring-data-redis`.
- **Impacto**: Escalabilidade limitada; regras de velocidade ineficientes.
- **Remediação**: Adicionar Redis para cache de regras e contadores.

### GAP-026: SQL Injection potencial
- **Área**: Segurança
- **Prioridade**: P1
- **Descrição**: Queries nativas podem ser vulneráveis se parâmetros não forem sanitizados.
- **Evidência**: `TransactionRepository.java:78-95` usa nativeQuery.
- **Impacto**: Possível SQL injection em cenários específicos.
- **Remediação**: Usar apenas parameterized queries; evitar nativeQuery.

### GAP-027: Fixture crtran.json para testes
- **Área**: QA
- **Prioridade**: P1
- **Descrição**: Sem arquivo de payload real (crtran.json) para testes de regressão.
- **Evidência NEGATIVA**: Nenhum arquivo `crtran.json` ou fixture de produção.
- **Impacto**: Testes podem não refletir dados reais.
- **Remediação**: Adicionar fixtures anonimizados de transações reais.

---

## 🟡 GAPS P2 — IMPORTANTES

### GAP-028: Circuit breaker
- **Área**: Arquitetura
- **Prioridade**: P2
- **Descrição**: Sem Resilience4j ou similar para resiliência.
- **Impacto**: Falhas em cascata possíveis.

### GAP-029: Message queue para async
- **Área**: Arquitetura
- **Prioridade**: P2
- **Descrição**: Sem Kafka/RabbitMQ para processamento assíncrono.
- **Impacto**: Todas as operações síncronas.

### GAP-030: Dashboard de KPIs de fraude
- **Área**: Product Owner
- **Prioridade**: P2
- **Descrição**: Métricas básicas existem, mas sem visualização rica.

### GAP-031: Comparativo A/B de regras
- **Área**: Product Owner
- **Prioridade**: P2
- **Descrição**: Sem A/B testing de regras.

### GAP-032: Bureau de Crédito
- **Área**: Negócio
- **Prioridade**: P2
- **Descrição**: Sem integração com Serasa/SPC.

### GAP-033: Gráficos de métricas
- **Área**: UI
- **Prioridade**: P2
- **Descrição**: Chart.tsx existe mas não usado em Dashboard.

### GAP-034: Empty states customizados
- **Área**: UI
- **Prioridade**: P2
- **Descrição**: Mensagens genéricas de lista vazia.

### GAP-035: Onboarding/Tutorial
- **Área**: UX
- **Prioridade**: P2
- **Descrição**: Sem guia para novos usuários.

### GAP-036: Notificações in-app
- **Área**: Product Design
- **Prioridade**: P2
- **Descrição**: Sem sistema de alertas.

### GAP-037: Jornada do analista
- **Área**: Product Design
- **Prioridade**: P2 (deveria ser P1)
- **Descrição**: Sem workflow guiado.

### GAP-038: Particionamento de tabela
- **Área**: DBA
- **Prioridade**: P2
- **Descrição**: transactions pode crescer muito.

### GAP-039: Índice GIN para JSONB
- **Área**: DBA
- **Prioridade**: P2
- **Descrição**: conditions_json não indexável.

### GAP-040: Vacuum/Analyze automatizado
- **Área**: DBA
- **Prioridade**: P2
- **Descrição**: Sem pg_cron ou similar.

### GAP-041: Secrets management
- **Área**: Segurança
- **Prioridade**: P2
- **Descrição**: Sem Vault ou similar.

### GAP-042: Health check detalhado
- **Área**: Backend
- **Prioridade**: P2
- **Descrição**: Sem endpoint /actuator/health customizado.

---

## 📈 Riscos Consolidados

### RISCO-001: API totalmente aberta
- **Severidade**: CRÍTICA
- **Probabilidade**: CERTA (se exposta à internet)
- **Impacto**: Fraude, vazamento de dados, manipulação de regras
- **GAPs relacionados**: GAP-001, GAP-002

### RISCO-002: Performance desconhecida
- **Severidade**: ALTA
- **Probabilidade**: MÉDIA (depende do volume)
- **Impacto**: Sistema pode cair em pico de transações
- **GAPs relacionados**: GAP-005, GAP-010, GAP-012

### RISCO-003: Regressões não detectadas
- **Severidade**: ALTA
- **Probabilidade**: MÉDIA (a cada deploy)
- **Impacto**: Bugs em produção afetando detecção de fraude
- **GAPs relacionados**: GAP-003, GAP-011, GAP-013

### RISCO-004: Não conformidade LGPD
- **Severidade**: ALTA
- **Probabilidade**: CERTA (se receber requisição de titular)
- **Impacto**: Multas de até 2% do faturamento
- **GAPs relacionados**: GAP-016, GAP-017

### RISCO-005: Deploy sem rollback
- **Severidade**: MÉDIA
- **Probabilidade**: ALTA (em incidentes)
- **Impacto**: Downtime prolongado
- **GAPs relacionados**: GAP-004, GAP-021

---

## ✅ Recomendações de Priorização

### Sprint 0 (Bloqueadores - antes de HML)
1. GAP-001: Implementar autenticação JWT
2. GAP-002: Implementar rate limiting
3. GAP-003: Criar E2E básico com Playwright
4. GAP-004: Criar pipeline CI/CD mínimo

### Sprint 1 (Críticos - HML)
1. GAP-005: Redis para velocidade
2. GAP-007: Tabela de blacklist
3. GAP-009: Workflow de aprovação
4. GAP-016: Endpoint LGPD
5. GAP-011: JaCoCo coverage

### Sprint 2 (Produção)
1. GAP-018, GAP-019: Security headers
2. GAP-021: Kubernetes manifests
3. GAP-022, GAP-023: Observabilidade
4. GAP-020: Backup automatizado

### Backlog (Melhorias)
- Todos os P2
