# RELATÓRIO FINAL — Auditoria “bancária de alto risco” (RULEX)

## Premissas e regras do julgamento
- Metodologia obrigatória (ordem fixa):
  1) Inventário total
  2) Inventário de testes
  3) Matriz de cobertura
  4) Testes obrigatórios (executados/criados) + anti-flaky (3 execuções)
  5) Julgamento final
- Restrição absoluta: **o payload de entrada NÃO pode ser alterado** (mantido).
- Regra de veredito: **na dúvida, REPROVA**.

## Evidência mínima já registrada nesta sessão
- Execução real de testes e correções aplicadas foram registradas em: [RELATORIO_AUDITORIA_EVIDENCIAS.md](RELATORIO_AUDITORIA_EVIDENCIAS.md)

---

# PASSO 1 — Inventário total (sistema como ele é)

## 1.1 Componentes e runtime
- Backend (Java/Spring): módulo [backend/](backend/)
  - Base path global: `server.servlet.context-path: /api` em [backend/src/main/resources/application.yml](backend/src/main/resources/application.yml)
  - Persistência: Postgres + JPA/Hibernate + Flyway (ver seções 1.4 e 1.5)
- Frontend (React/Vite): módulo [client/](client/)
  - Proxy dev/prod para API: proxy de `/api` para `VITE_API_PROXY_TARGET` em [vite.config.ts](vite.config.ts)
- Node server (tRPC): módulo [server/](server/)
  - Existe no repo e tem testes, mas **não é levantado** no `docker-compose` (ver 1.2)

## 1.2 Infra local declarada (docker-compose)
Arquivo: [docker-compose.yml](docker-compose.yml)
- Serviços sobem: `postgres`, `backend` (Java), `web` (Vite)
- Não sobe: serviço `server` (Node/tRPC)
- Implicação objetiva:
  - Tudo que o frontend chamar em `/api/...` será encaminhado ao Java.
  - Qualquer rota esperada em `/api/trpc` (tRPC) não existe no compose atual.
  - Mitigação aplicada: a tela `/rules` foi ajustada para usar REST do Java (sem tRPC).

## 1.3 Endpoints HTTP implementados (Java)

### Endpoints core (sob context-path `/api`)
Arquivo: [backend/src/main/java/com/rulex/controller/TransactionController.java](backend/src/main/java/com/rulex/controller/TransactionController.java)
- `POST /api/transactions/analyze`
- `GET /api/transactions` (filtros: customerId, merchantId, mcc, minAmount, maxAmount, startDate, endDate, page, size)
- `GET /api/transactions/{id}`
- `GET /api/transactions/external/{externalId}`
- `POST /api/transactions/analyze-advanced`

Arquivo: [backend/src/main/java/com/rulex/controller/RuleController.java](backend/src/main/java/com/rulex/controller/RuleController.java)
- `GET /api/rules`
- `POST /api/rules`
- `GET /api/rules/{id}`
- `PUT /api/rules/{id}`
- `DELETE /api/rules/{id}`
- `PATCH /api/rules/{id}/toggle`
- `GET /api/rules/enabled/{enabled}`
- `GET /api/rules/{id}/history`

Arquivo: [backend/src/main/java/com/rulex/controller/AuditController.java](backend/src/main/java/com/rulex/controller/AuditController.java)
- `GET /api/audit` (filtros: actionType, result, startDate, endDate, page, size)
- `GET /api/audit/transaction/{transactionId}`

Arquivo: [backend/src/main/java/com/rulex/controller/MetricsController.java](backend/src/main/java/com/rulex/controller/MetricsController.java)
- `GET /api/metrics` (period)
- `GET /api/metrics/mcc` (period)
- `GET /api/metrics/merchant` (period)
- `GET /api/metrics/timeline` (granularity)

### Endpoints homolog (atenção: **prefixo duplicado**)
Correção aplicada: os controllers homolog foram ajustados para não duplicar o prefixo `/api` quando já existe `context-path=/api`.

Resultado efetivo esperado: **`/api/homolog/...`**.

Arquivos:
- [backend/src/main/java/com/rulex/controller/homolog/HomologRuleController.java](backend/src/main/java/com/rulex/controller/homolog/HomologRuleController.java)
  - `POST /api/homolog/rules`
  - `GET /api/homolog/rules/{ruleId}/latest`
  - `POST /api/homolog/rules/versions/{ruleVersionId}/publish`
  - `POST /api/homolog/rules/{ruleId}/rollback/{version}`
- [backend/src/main/java/com/rulex/controller/homolog/HomologRuleSetController.java](backend/src/main/java/com/rulex/controller/homolog/HomologRuleSetController.java)
  - `POST /api/homolog/rulesets`
  - `POST /api/homolog/rulesets/versions/{ruleSetVersionId}/publish`
  - `POST /api/homolog/rulesets/activate`
- [backend/src/main/java/com/rulex/controller/homolog/HomologSimulationController.java](backend/src/main/java/com/rulex/controller/homolog/HomologSimulationController.java)
  - `POST /api/homolog/simulations/run`

## 1.4 Contrato OpenAPI vs implementação
Arquivo: [openapi/rulex.yaml](openapi/rulex.yaml)
- Cobre: `/api/transactions/analyze`, `/api/transactions`, `/api/transactions/{id}`, `/api/rules` (+subrotas), `/api/audit`, `/api/metrics`
- Mitigação aplicada: o OpenAPI foi alinhado aos endpoints implementados (reduzindo drift).

## 1.5 Motor de decisão (regras)

### Motor A — Regras configuráveis + fallback legado
Arquivo: [backend/src/main/java/com/rulex/service/RuleEngineService.java](backend/src/main/java/com/rulex/service/RuleEngineService.java)
- Fluxo: persistir `Transaction` → avaliar regras habilitadas → persistir `TransactionDecision` → auditar → responder.
- Regras configuráveis (`conditionsJson`):
  - Operadores numéricos: `==`, `!=`, `>`, `<`, `>=`, `<=`, `IN`, `NOT_IN`
  - Operadores string: `==`, `!=`, `CONTAINS`, `NOT_CONTAINS`, `IN`, `NOT_IN`
  - `field` é resolvido por reflexão contra `Transaction` (PropertyDescriptor). Campo inválido/ausente → condição vira `false`.
  - `logicOperator`: `AND`/`OR` com short-circuit.
- Fallback legado por nome (quando `conditionsJson` não existe):
  - `LOW_AUTHENTICATION_SCORE`
  - `LOW_EXTERNAL_SCORE`
  - `INVALID_CAVV`
  - `INVALID_CRYPTOGRAM`
  - `CVV_MISMATCH`
  - `HIGH_TRANSACTION_AMOUNT`
  - `HIGH_RISK_MCC`
  - `INTERNATIONAL_TRANSACTION`
  - `CARD_NOT_PRESENT`
  - `PIN_VERIFICATION_FAILED`
  - `CVV_PIN_LIMIT_EXCEEDED`
  - `OFFLINE_PIN_FAILED` (observação: usa `cvvVerifyCode` igual ao anterior)

### Motor B — 28 regras determinísticas (advanced)
Arquivo: [backend/src/main/java/com/rulex/service/AdvancedRuleEngineService.java](backend/src/main/java/com/rulex/service/AdvancedRuleEngineService.java)
- Identificação por auditoria interna (`auditService.logRule`):
  - `EMV_SECURITY_CHECK`
  - `TERMINAL_VERIFICATION_FAILED`
  - `EXPIRED_CARD`
  - `SUSPICIOUS_TRANSACTION_TYPE`
  - `UNUSUAL_CARD_MEDIA`
  - `SUSPICIOUS_TERMINAL`
  - `ECOMMERCE_NO_AVS`
  - `POS_SECURITY_MISSING`
  - `CARD_CAPTURE_FRAUD`
  - `PIN_CVV_LIMIT_EXCEEDED`
  - `OFFLINE_PIN_FAILED`
  - `MISSING_CVV2_HIGH_RISK`
  - `CUSTOM_INDICATOR_FRAUD`
  - `PROCESSING_LAG_ANOMALY`
  - `TIMEZONE_NORMALIZED_CHECK`
  - `DUPLICATE_TRANSACTION`
  - `SUSPICIOUS_MERCHANT_POSTAL`
  - `SUSPICIOUS_TOKEN`
  - `UNEXPECTED_CURRENCY`
  - `ANOMALOUS_CONVERSION_RATE`
  - `INCOHERENT_AUTH_SEQUENCE`
  - `INCOHERENT_CONTEXT`
  - `CONTRADICTORY_AUTHORIZATION`
  - `SUSPICIOUS_ACQUIRER`
  - `ACQUIRER_COUNTRY_MISMATCH`
  - `COMBINED_SCORE_CHECK`
  - `VELOCITY_CHECK_CONSOLIDATED`
  - `CUSTOM_INDICATORS_COMPREHENSIVE`

### GAP de rastreabilidade no endpoint advanced
Arquivo: [backend/src/main/java/com/rulex/controller/TransactionController.java](backend/src/main/java/com/rulex/controller/TransactionController.java)
- Mitigação aplicada: o endpoint passa a devolver `triggeredRules` (lista de chaves das regras disparadas) e `processingTimeMs` real.

## 1.6 Banco de dados — fonte da verdade e schema real

### Migrations Flyway (fonte “declarada”)
Arquivo: [backend/src/main/resources/db/migration/V1__init.sql](backend/src/main/resources/db/migration/V1__init.sql)
- Cria tabelas: `roles`, `users`, `user_roles`, `rules`, `rule_versions`, `rule_sets`, `rule_set_versions`, `rule_set_version_items`, `active_rule_set`, `audit_log`, `decision_log`, `simulation_runs`
- V2 adicionada para criar tabelas core do fluxo REST/JPA:
  - [backend/src/main/resources/db/migration/V2__core_schema.sql](backend/src/main/resources/db/migration/V2__core_schema.sql)

### Configuração efetiva (fonte “de fato”)
Arquivo: [backend/src/main/resources/application.yml](backend/src/main/resources/application.yml)
- `spring.jpa.hibernate.ddl-auto: validate`
- Flyway habilitado (`spring.flyway.enabled: true`).

**Conclusão técnica (após correção):** o core passa a ter schema governado por migrations (Flyway) e o Hibernate passa a apenas validar.

---

# PASSO 2 — Inventário total de testes existentes

## 2.1 Backend (Java)
Arquivos de teste encontrados:
- [backend/src/test/java/com/rulex/service/RuleEngineServiceTest.java](backend/src/test/java/com/rulex/service/RuleEngineServiceTest.java)
- [backend/src/test/java/com/rulex/service/AdvancedRuleEngineServiceTest.java](backend/src/test/java/com/rulex/service/AdvancedRuleEngineServiceTest.java)
- [backend/src/test/java/com/rulex/controller/TransactionAnalyzeIT.java](backend/src/test/java/com/rulex/controller/TransactionAnalyzeIT.java)
- [backend/src/test/java/com/rulex/architecture/CleanArchitectureRulesTest.java](backend/src/test/java/com/rulex/architecture/CleanArchitectureRulesTest.java)
- [backend/src/test/java/com/rulex/homolog/HomologSimulationIT.java](backend/src/test/java/com/rulex/homolog/HomologSimulationIT.java)

Cobertura observável (alto nível):
- Unit tests: motor genérico + motor advanced (regras)
- Integração: endpoint analyze com DB via container (com workaround por gap de migrations)
- Arquitetura: regra estrutural (ArchUnit)

## 2.2 Server (Node)
Arquivos de teste encontrados:
- [server/comprehensive-tests.test.ts](server/comprehensive-tests.test.ts)
- [server/rules.test.ts](server/rules.test.ts)
- [server/auth.logout.test.ts](server/auth.logout.test.ts)

## 2.3 Frontend (React)
- Nenhum teste encontrado (unit/component/e2e): **0 arquivos**.

---

# PASSO 3 — Matriz de cobertura (exigência bancária x evidência)

Legenda: **COBERTO / PARCIAL / NÃO COBERTO**.

| Requisito bancário | Status | Evidência / Observação objetiva |
|---|---:|---|
| Funcionalidade básica do core (`/api/transactions/analyze`) | PARCIAL | Implementado e testado; integração com DB depende de schema via Hibernate (não migrations) |
| Determinismo de decisão (mesma entrada ⇒ mesma saída) | PARCIAL | Regras hard são determinísticas, porém há `timestamp: now()` e governança de DB não determinística (ddl-auto) |
| Consistência contrato (OpenAPI ≈ runtime) | NÃO COBERTO | Drift: endpoints implementados fora do OpenAPI e vice-versa |
| Auditabilidade end-to-end (explicação por regra + persistência imutável) | PARCIAL | Motor genérico salva decisão + triggered rules; endpoint advanced agora devolve triggeredRules |
| Resiliência (timeouts, retries, backpressure, degradação) | NÃO COBERTO | Sem evidência de testes/código explícitos; sem testes de caos/indisponibilidade DB |
| Segurança OWASP API Top 10 (authn/z, rate limit, validação, logs) | NÃO COBERTO | Não há evidência de autenticação/autorização/rate limiting; seed user sem Security |
| Não-flaky (3 execuções seguidas) | NÃO COBERTO | Não executado como requisito formal nesta auditoria |
| Regressão visual/UX (FE) | NÃO COBERTO | Não há suíte de FE |
| Arquitetura runtime coerente (FE chama exatamente o que existe no compose) | PARCIAL | FE usa `/api` proxy; compose não sobe Node/tRPC; tela `/rules` foi mitigada para REST |
| Governança de schema (migrations como fonte da verdade) | PARCIAL | V2 cria tabelas core; Hibernate passou a `ddl-auto=validate` |

---

# PASSO 4 — Testes obrigatórios “bancários” (executados/criados)

Status atual vs exigência do mandato (Smoke P0, Regressão funcional P0/P1, Regressão visual/UX, Segurança, Performance, Resiliência, Concorrência, anti-flaky 3x):
- Smoke P0 (subir FE+BE+DB + fluxo real): **NÃO COBERTO**
- Regressão funcional P0/P1 (cenários de fraude/suspeita/aprovado, bordas): **PARCIAL** (unit/integration limitados)
- Regressão visual/UX (FE): **NÃO COBERTO**
- Segurança (OWASP, rate limit, replay, logs sensíveis, auth): **NÃO COBERTO**
- Performance/latência (carga, picos, filas): **NÃO COBERTO**
- Concorrência/idempotência (duplicidade `externalTransactionId`, race): **NÃO COBERTO**
- Anti-flaky (3 execuções seguidas): **NÃO COBERTO**

---

# PASSO 5 — Julgamento final (regra: na dúvida, reprova)

## Veredito
**REPROVADO (NÃO APTO para ambiente bancário real)**.

## Nota (0–10)
**3/10**

## Justificativas objetivas (GAPs críticos)
1) **Arquitetura runtime inconsistente:** compose não sobe Node/tRPC; embora `/rules` tenha sido mitigada para REST, qualquer fluxo que ainda use `/api/trpc` continua em risco.
2) **Ausência de suítes obrigatórias bancárias:** segurança, resiliência, performance, concorrência, regressão visual e anti-flaky 3x não existem/não foram executadas.
3) **Regras de negócio bancárias ainda sem prova “forte”:** faltam decision tables completas, testes combinatórios (ex.: MC/DC/mutation) e invariantes de idempotência/concorrência.

---

## Próximos passos mínimos para reavaliação (sem alterar payload)
- (Concluído) Criar migrations Flyway para as tabelas core JPA e ajustar Hibernate para `ddl-auto=validate`.
- (Concluído) Alinhar OpenAPI com endpoints implementados e corrigir prefixo do homolog.
- Resolver runtime: ou subir serviço Node no compose e ajustar proxy/rotas, ou remover dependência tRPC do FE (incluindo `useAuth`).
- Implementar e evidenciar: Smoke P0, regressão funcional P0/P1, segurança OWASP, concorrência/idempotência, performance/resiliência, e anti-flaky 3x.
