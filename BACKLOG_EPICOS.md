# Backlog de Épicos (P0/P1/P2) — Refatoração Completa RULEX

Este backlog está organizado para execução por um time (engenharia + arquitetura + produto), com foco em reduzir duplicidade e estabilizar contratos.

## P0 — Bloqueadores (segurança, contrato, fonte de verdade)

### EPIC P0.1 — Escolha do Core e banco único
**Objetivo:** definir a fonte de verdade e eliminar duplicidade.
- Decisão: **Java Core + PostgreSQL único** (recomendado)
- Resultado: regras/auditoria/decisões ficam em um único lugar

**Critérios de aceite**
- Existe um “source of truth” documentado
- Banco oficial definido e usado pelo domínio de regras
- Plano de migração aprovado

---

### EPIC P0.2 — Contrato canônico (OpenAPI) + geração de client
**Objetivo:** acabar com divergência de payloads/DTOs.

**Entregas**
- OpenAPI com endpoints:
  - `POST /api/transactions/analyze`
  - `GET /api/transactions` / `GET /api/transactions/{id}`
  - `GET/POST/PUT/PATCH/DELETE /api/rules`
  - `GET /api/audit`
  - `GET /api/metrics`
- Client TS gerado e usado pelo frontend

**Critérios de aceite**
- Frontend não possui tipos “inventados” divergentes do backend
- Requests/responses batem no runtime (contrato executável)

---

### EPIC P0.3 — Endpoints de análise: unificar resposta e remover simulação em produção
**Objetivo:** uma resposta única e consistente.

**Entregas**
- `AnalyzeTransactionResponse` com:
  - `classification`, `riskScore`, `triggeredRules[]`, `processingTimeMs`, `timestamp`, `reason`
- `analyze-advanced` deixa de retornar `Map` inconsistente e passa a retornar o DTO oficial
- No frontend, fallback “simulado” só existe em modo demo/dev (feature-flag)

**Critérios de aceite**
- `TransactionSimulator` funciona com API real
- Não há “shape mismatch” entre UI e Java

---

### EPIC P0.4 — Segurança mínima de produção
**Objetivo:** impedir configuração insegura.

**Entregas**
- Java:
  - CORS restrito por ambiente (sem `*` em prod)
  - headers seguros básicos
- Node (se mantido):
  - falha startup se `JWT_SECRET` vazio
  - cookie config revisada
- Dados sensíveis:
  - PAN mascarado/tokenizado/criptografado antes de persistir

**Critérios de aceite**
- Deploy não aceita segredos vazios
- Sem CORS `*` em ambiente produtivo
- PAN não é armazenado em claro

---

## P1 — Consolidar motor e auditoria (manutenibilidade e rastreabilidade)

### EPIC P1.1 — Motor de regras genérico por condições (eliminar switch-case)
**Objetivo:** regras configuráveis de verdade.

**Entregas**
- Regra com `conditions[]` + `logicOperator`
- Avaliador genérico (operadores, coerção de tipo)
- Detalhamento de por que cada condição passou/falhou

**Critérios de aceite**
- Adicionar regra nova não exige mudança de código do motor
- Auditoria registra quais condições dispararam

---

### EPIC P1.2 — Versionamento de regras + histórico
**Objetivo:** rastrear mudanças e reproduzir decisões.

**Entregas**
- `rules.version` incrementa em alterações
- `rule_history` (diff/estado anterior/novo, autor, timestamp)
- decisão grava `rulesetVersion`/snapshot

**Critérios de aceite**
- É possível explicar “por que aprovou” com regras daquela versão

---

### EPIC P1.3 — Auditoria append-only e padronizada
**Objetivo:** compliance e troubleshooting.

**Entregas**
- `audit_events` append-only (tipo, ator, payload sanitizado, correlação)
- correlação por `externalTransactionId`

**Critérios de aceite**
- Consultas de auditoria retornam trilha completa

---

## P2 — Qualidade, performance e operação

### EPIC P2.1 — Métricas corretas e eficientes
**Objetivo:** métricas por período e agregações no DB.

**Entregas**
- queries agregadas (COUNT/GROUP BY) no banco
- denominadores por período (sem `findAll().stream().count()`)

**Critérios de aceite**
- métricas batem com auditoria/decisões
- performance ok em volume

---

### EPIC P2.2 — Testes ponta-a-ponta (contrato + integração)
**Objetivo:** confiança no core.

**Entregas**
- testes de integração do Java (WebMvcTest/Testcontainers)
- smoke tests do frontend contra API
- pipeline CI rodando (lint/typecheck/test)

**Critérios de aceite**
- build verde reproduzível em ambiente limpo

---

### EPIC P2.3 — Simplificação do monorepo e execução local
**Objetivo:** onboarding fácil.

**Entregas**
- scripts `make`/`pnpm`/`mvn` documentados
- `.env.example` para Node (se existir)
- instruções de DB local (docker compose opcional)

**Critérios de aceite**
- dev novo sobe a stack em <30 min
