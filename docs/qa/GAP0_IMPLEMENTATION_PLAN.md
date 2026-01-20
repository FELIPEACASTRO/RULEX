# Plano de Implementação GAP 0 (Consolidado)

Este plano consolida os gaps e ações necessárias para atingir **GAP 0** com base nos relatórios de auditoria e registros atuais.
Ele **não altera contratos** e foca em garantir estabilidade e rastreabilidade.

## 1) Bloqueadores de produção (críticos)

1. **Operadores STUB em regras ativas (86 operadores)**
   - **Ação:** desabilitar regras ativas que usam operadores STUB ou implementar os operadores.
   - **Resultado esperado:** nenhuma regra ativa pode lançar exceção por operador não implementado.
   - **Fonte:** `AUDIT_REPORT.md` (seção “86 operadores críticos”).

2. **Implementação efetiva dos operadores críticos**
   - **Ação:** criar/atualizar evaluators para cobrir todos os operadores listados como STUB.
   - **Critério de aceite:** suite de testes cobre operadores novos + validação com dados de regras.

## 2) Cobertura e regressão (qualidade)

1. **Cobertura mínima para operadores e regras críticas**
   - **Ação:** adicionar testes unitários por avaliador e testes de integração para o fluxo de avaliação.
   - **Critério de aceite:** cobertura de operadores críticos >= 80%.

2. **Golden Master / Shadow Mode (quando aplicável)**
   - **Ação:** capturar outputs de transações reais/simuladas e comparar resultados antes/depois.
   - **Critério de aceite:** 100% equivalência funcional (com tolerância definida).

## 3) Infra e integrações (Postgres, Redis, Neo4j)

1. **Postgres**
   - **Ação:** validar migrations e schema com `flyway:info`.
   - **Critério de aceite:** healthcheck OK + migrations em estado “applied”.

2. **Redis**
   - **Ação:** confirmar uso de cache/velocity (config e healthcheck).
   - **Critério de aceite:** healthcheck OK + endpoints dependentes operando sem erro.

3. **Neo4j**
   - **Ação:** validar conexão e healthcheck; confirmar habilitação via `rulex.neo4j.enabled`.
   - **Critério de aceite:** healthcheck OK + queries mínimas executando.

## 4) Frontend (UX, navegação e sincronização com backend)

1. **Integração front-back**
   - **Ação:** validar que a UI consome os endpoints atuais via proxy e autenticação básica.
   - **Critério de aceite:** operações principais (listar/criar/editar regras) funcionam sem erro.

2. **UX / Navegação**
   - **Ação:** revisão sistemática das telas com critérios: consistência, feedback de erros, validações.
   - **Critério de aceite:** nenhum fluxo principal sem feedback claro e validações obrigatórias.

## 5) Testes obrigatórios para GAP 0

- **Frontend:** `pnpm test --run`
- **Backend:** `mvn -f backend/pom.xml test`
- **E2E:** `pnpm exec playwright test`

## 6) Rastreabilidade e documentação

- Atualizar `DEVIN_PROGRESS.md` com o status das ações.
- Atualizar `DEVIN_EVIDENCE_LOG.md` com resultados de testes.
- Atualizar `GAPS_REGISTER.md` apenas quando um gap for **realmente fechado**.

---

## Checklist resumido (execução)

- [ ] Desabilitar ou implementar os 86 operadores críticos.
- [ ] Adicionar testes unitários/integrados para operadores críticos.
- [ ] Validar Postgres/Redis/Neo4j com healthchecks e smoke tests.
- [ ] Validar fluxo de UI com backend (listagem/criação/edição/toggle).
- [ ] Executar suites de testes e registrar evidências.
