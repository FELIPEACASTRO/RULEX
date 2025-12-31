# GAPS REGISTER - RULEX

## Classificação
- **P0**: Crítico - Bloqueia funcionalidade core
- **P1**: Importante - Afeta qualidade/segurança
- **P2**: Desejável - Melhoria de UX/DX

---

## P0 - Crítico

### GAP-P0-01: RuleFormDialog Incompleto
**Descrição:** O componente RuleFormDialog está marcado como TODO e não implementado.
**Impacto:** Popup de criação/edição de regras não funciona como componente reutilizável.
**Evidência:**
```typescript
// client/src/components/RuleFormDialog/index.tsx
// TODO: Implementar RuleFormDialog componente
```
**Solução:** Implementar o componente ou integrar com ComplexRuleBuilder.
**Status:** ❌ Aberto

---

### GAP-P0-02: Popup Simples Não Suporta Operadores Avançados
**Descrição:** O popup de regras simples (Rules.tsx) só suporta 20 operadores, enquanto o backend suporta 50.
**Impacto:** Usuários não conseguem criar regras com GEO, VELOCITY, FIELD_*, etc. via popup.
**Evidência:**
```typescript
// client/src/components/RuleFormDialog/types.ts
export const OPERATORS: OperatorOption[] = [
  // Apenas 20 operadores básicos
  // Faltam: GEO_*, VELOCITY_*, FIELD_*, DATE_*, TIME_*, ARRAY_*, MOD_*
];
```
**Solução:** Evoluir popup para suportar todos os operadores OU integrar ComplexRuleBuilder.
**Status:** ❌ Aberto

---

### GAP-P0-03: Constraint CHECK Comentada em V12
**Descrição:** A constraint que garante que rule_condition_groups tenha pelo menos uma FK está comentada.
**Impacto:** Possível inconsistência de dados (grupos órfãos).
**Evidência:**
```sql
-- V12__complex_rules_crud.sql
-- Comentado por enquanto para não quebrar dados existentes
-- ALTER TABLE rule_condition_groups
-- ADD CONSTRAINT chk_condition_groups_has_parent
-- CHECK (rule_version_id IS NOT NULL OR complex_rule_id IS NOT NULL);
```
**Solução:** Criar migration de backfill + ativar constraint.
**Status:** ❌ Aberto

---

### GAP-P0-04: Falta Optimistic Locking ✅ FECHADO
**Descrição:** RuleConfiguration não tem @Version para evitar lost updates.
**Impacto:** Edições concorrentes podem sobrescrever dados silenciosamente.
**Solução:** Adicionar @Version e tratar 409 no frontend.
**Status:** ✅ Fechado (commit 2fcef9b)
**Implementação:**
- Adicionado @Version ao campo version em RuleConfiguration.java
- Adicionado handler para ObjectOptimisticLockingFailureException em GlobalExceptionHandler.java

---

## P1 - Importante

### GAP-P1-01: Falta Limites Anti-Abuso ✅ FECHADO
**Descrição:** Não há limites para nesting, condições por grupo, tamanho de listas, etc.
**Impacto:** Regras monstruosas podem causar DoS ou performance degradada.
**Status:** ✅ Fechado (commit 88753c6)
**Implementação em RuleValidationService.java:**
- MAX_NESTING_DEPTH: 10 níveis
- MAX_CONDITIONS_PER_GROUP: 50 condições
- MAX_TOTAL_CONDITIONS: 200 condições
- MAX_LIST_SIZE: 1000 itens para IN/NOT_IN
- MAX_REGEX_LENGTH: 500 caracteres

---

### GAP-P1-02: Falta E2E Playwright Completo
**Descrição:** Testes E2E existem mas não cobrem todos os fluxos.
**Impacto:** Regressões podem passar despercebidas.
**Evidência:**
```bash
ls e2e/
# Apenas arquivos básicos
```
**Solução:** Criar suite E2E cobrindo:
- CRUD regras simples
- CRUD regras complexas
- Simulação
- RBAC (401/403/200)
**Status:** ❌ Aberto

---

### GAP-P1-03: Falta Testes Unitários por Operador
**Descrição:** Não há testes unitários para cada um dos 50 operadores.
**Impacto:** Bugs em operadores específicos podem passar despercebidos.
**Evidência:** Não encontrado arquivo de teste específico para operadores.
**Solução:** Criar ComplexRuleEvaluatorOperatorsTest.java com teste para cada operador.
**Status:** ❌ Aberto

---

### GAP-P1-04: Falta Rate Limiting
**Descrição:** Não há rate limiting nos endpoints.
**Impacto:** Vulnerável a ataques de força bruta e DoS.
**Evidência:** Nenhuma configuração de rate limiting encontrada.
**Solução:** Implementar rate limiting via Spring Cloud Gateway ou Bucket4j.
**Status:** ❌ Aberto

---

### GAP-P1-05: Falta Audit Log de Acessos
**Descrição:** Não há log de quem acessou o quê.
**Impacto:** Dificuldade em investigar incidentes de segurança.
**Evidência:** AuditService só loga operações de regras, não acessos.
**Solução:** Adicionar filtro de auditoria para todos os endpoints.
**Status:** ❌ Aberto

---

### GAP-P1-06: Frontend Não Trata 401/403 Adequadamente
**Descrição:** Frontend não redireciona para login em 401 nem mostra mensagem em 403.
**Impacto:** UX ruim quando usuário não tem permissão.
**Evidência:** Verificar tratamento de erros em javaApi.ts.
**Solução:** Implementar interceptor global para 401/403.
**Status:** ⏳ Verificando

---

### GAP-P1-07: Falta Preview JSON Antes de Salvar
**Descrição:** Usuário não consegue ver o JSON final da regra antes de salvar.
**Impacto:** Dificuldade em debugar regras complexas.
**Evidência:** Não encontrado componente de preview no popup.
**Solução:** Adicionar aba/modal de preview JSON.
**Status:** ❌ Aberto

---

## P2 - Desejável

### GAP-P2-01: Basic Auth Não Ideal para Produção
**Descrição:** Autenticação via HTTP Basic não é segura para produção.
**Impacto:** Credenciais trafegam em cada request (mesmo com HTTPS).
**Solução:** Migrar para JWT ou OAuth2.
**Status:** ❌ Aberto

---

### GAP-P2-02: Falta OpenTelemetry
**Descrição:** Não há tracing distribuído.
**Impacto:** Dificuldade em debugar problemas em produção.
**Solução:** Adicionar OpenTelemetry com exportador para Jaeger/Zipkin.
**Status:** ❌ Aberto

---

### GAP-P2-03: Falta Dashboards Grafana
**Descrição:** Não há dashboards de monitoramento.
**Impacto:** Dificuldade em visualizar métricas.
**Solução:** Criar dashboards para métricas de regras, transações, performance.
**Status:** ❌ Aberto

---

### GAP-P2-04: Falta Documentação de Tipologias
**Descrição:** Não há documentação de tipologias de fraude reais.
**Impacto:** Usuários não sabem quais regras criar.
**Solução:** Criar guia de tipologias com exemplos de regras.
**Status:** ❌ Aberto

---

### GAP-P2-05: Falta Contract Tests
**Descrição:** Não há testes de contrato entre frontend e backend.
**Impacto:** Drift entre API e client pode passar despercebido.
**Solução:** Implementar contract tests com OpenAPI ou Pact.
**Status:** ❌ Aberto

---

## Resumo

| Prioridade | Total | Abertos | Fechados |
|------------|-------|---------|----------|
| P0 | 4 | 3 | 1 |
| P1 | 7 | 6 | 1 |
| P2 | 5 | 5 | 0 |
| **Total** | **16** | **14** | **2** |

---

## Próximos Passos

1. **Passada 2**: Subir stack e validar integração
2. **Passada 3**: Implementar correções P0
3. **Passada 4**: Criar test suite extrema

---

## Última Atualização
2024-12-31T21:20:00Z
