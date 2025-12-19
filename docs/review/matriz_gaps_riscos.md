# Matriz de Gaps e Riscos

**Data**: 2025-12-19  
**Projeto**: RULEX - Motor de Regras Duras

---

## Classificação de Severidade

| Nível | Descrição | Impacto em Homologação |
|-------|-----------|------------------------|
| P0 | Bloqueador crítico | ❌ Impede homologação |
| P1 | Alto risco | ⚠️ Requer mitigação antes de produção |
| P2 | Médio risco | ⚠️ Pode ir para produção com plano de ação |
| P3 | Baixo risco | ✅ Melhorias futuras |

---

## Gaps Identificados

### Gaps P0 (Bloqueadores)

| ID | Gap | Área | Responsável | Evidência |
|----|-----|------|-------------|-----------|
| G-001 | **Spring Security não configurado** - Sistema bancário sem autenticação/autorização | Segurança | AppSec | Não encontrado `@EnableWebSecurity`, `SecurityFilterChain` em todo o backend |

### Gaps P1 (Alto Risco)

| ID | Gap | Área | Responsável | Evidência |
|----|-----|------|-------------|-----------|
| G-002 | Testes E2E/Navegação SPA ausentes | QA | QA Lead | Não encontrado Cypress/Playwright |
| G-003 | 20+ regras documentadas não implementadas | Negócio | Backend | `REGRAS_DURAS_60_IMPLEMENTACAO.md` lista 60+, apenas 40 implementadas |
| G-004 | Pipeline CI/CD não documentado | DevOps | SRE | Não encontrado `.github/workflows` |
| G-005 | Pen-test não documentado | Segurança | AppSec | Não encontrado relatório OWASP |
| G-006 | SAST/DAST não integrado | Segurança | AppSec | Não encontrado SonarQube/Snyk |
| G-007 | Apenas 1 teste frontend | QA | QA Lead | Apenas `Rules.test.tsx` encontrado |
| G-008 | Cobertura de código não medida | QA | QA Lead | Não encontrado jacoco/lcov |

### Gaps P2 (Médio Risco)

| ID | Gap | Área | Responsável | Evidência |
|----|-----|------|-------------|-----------|
| G-009 | Métricas Prometheus/Grafana | DevOps | SRE | Health check existe, falta exportação |
| G-010 | Cache de regras ausente | Backend | Java Engineer | Regras recarregadas a cada request (`RuleEngineService.java:147`) |
| G-011 | Política de retenção de dados | DBA | DBA | Não encontrado |
| G-012 | ADRs (Architecture Decision Records) | Arquitetura | Arquiteto | Não encontrado |
| G-013 | Storybook para componentes | Frontend | React Engineer | Não encontrado |
| G-014 | Particionamento da tabela transactions | DBA | DBA | `V2__core_schema.sql` sem particionamento |

### Gaps P3 (Baixo Risco)

| ID | Gap | Área | Responsável | Evidência |
|----|-----|------|-------------|-----------|
| G-015 | Onboarding/tour guiado | UX | UX Designer | Não encontrado |
| G-016 | Lazy loading em rotas | Frontend | React Engineer | `App.tsx` sem React.lazy |
| G-017 | Tokens de design formalizados | UI | UI Designer | Parcialmente em `DESIGN_SYSTEM.md` |

---

## Riscos Identificados

### Riscos P0 (Bloqueadores)

| ID | Risco | Área | Probabilidade | Impacto | Mitigação |
|----|-------|------|---------------|---------|-----------|
| R-001 | **Sistema bancário sem autenticação/autorização** - Qualquer requisição pode acessar endpoints críticos | Segurança | **Alta** | **Crítico** | **Configurar Spring Security IMEDIATAMENTE** |

### Riscos P1 (Alto Risco)

| ID | Risco | Área | Probabilidade | Impacto | Mitigação |
|----|-------|------|---------------|---------|-----------|
| R-002 | Bugs de integração passam sem E2E | QA | Alta | Alto | Implementar Playwright/Cypress |
| R-003 | Vulnerabilidades sem pen-test | Segurança | Média | Crítico | Contratar pen-test OWASP |
| R-004 | Deploy manual propenso a erros | DevOps | Alta | Alto | Implementar CI/CD |
| R-005 | Expectativa de 60+ regras vs 40 implementadas | Negócio | Alta | Médio | Documentar gap ou implementar regras faltantes |
| R-006 | Regras legadas divergem de genéricas | Backend | Média | Médio | Migrar para condições JSON |

### Riscos P2 (Médio Risco)

| ID | Risco | Área | Probabilidade | Impacto | Mitigação |
|----|-------|------|---------------|---------|-----------|
| R-007 | Performance degrada com muitas regras | Backend | Média | Médio | Implementar cache |
| R-008 | Tabela transactions cresce sem controle | DBA | Alta | Médio | Implementar particionamento |
| R-009 | Duas stacks de banco (PG + MySQL/Drizzle) | Arquitetura | Baixa | Médio | Consolidar em uma stack |
| R-010 | CORS permissivo pode permitir CSRF | Segurança | Média | Médio | Restringir CORS |

### Riscos P3 (Baixo Risco)

| ID | Risco | Área | Probabilidade | Impacto | Mitigação |
|----|-------|------|---------------|---------|-----------|
| R-011 | Curva de aprendizado alta | UX | Média | Baixo | Criar onboarding |
| R-012 | Bundle size frontend cresce | Frontend | Baixa | Baixo | Lazy loading |
| R-013 | Divergência visual | UI | Baixa | Baixo | Storybook |

---

## Matriz de Priorização

```
         IMPACTO
           ↑
     Alto  │  R-001 (P0)
           │  R-002   R-003   R-004   R-005
    Médio  │  R-006   R-007   R-008   R-009   R-010
           │
    Baixo  │  R-011   R-012   R-013
           └─────────────────────────→ PROBABILIDADE
              Baixa   Média   Alta
```

---

## Análise de Regras Implementadas vs Documentadas

### Regras Implementadas: 40

**Legadas (12)**: `RuleEngineService.java:239-275`
- LOW_AUTHENTICATION_SCORE
- LOW_EXTERNAL_SCORE
- INVALID_CAVV
- INVALID_CRYPTOGRAM
- CVV_MISMATCH
- HIGH_TRANSACTION_AMOUNT
- HIGH_RISK_MCC
- INTERNATIONAL_TRANSACTION
- CARD_NOT_PRESENT
- PIN_VERIFICATION_FAILED
- CVV_PIN_LIMIT_EXCEEDED
- OFFLINE_PIN_FAILED

**Avançadas (28)**: `AdvancedRuleEngineService.java`
- EMV_SECURITY_CHECK até CUSTOM_INDICATORS_COMPREHENSIVE

### Regras Documentadas mas NÃO Implementadas: ~20+

**Evidência**: `REGRAS_DURAS_60_IMPLEMENTACAO.md` lista 60+ regras

**Grupos não implementados**:
- GRUPO 1: VELOCITY CHECKS (8 regras) - apenas VELOCITY_CHECK_CONSOLIDATED implementada
- GRUPO 2: CARD TESTING FRAUD (6 regras) - não implementadas
- GRUPO 3: GEOGRAPHIC ANOMALIES (7 regras) - não implementadas
- GRUPO 5: TRANSACTION AMOUNT ANOMALIES (6 regras) - parcialmente implementadas
- GRUPO 6: TIME-BASED ANOMALIES (7 regras) - parcialmente implementadas
- GRUPO 7: MCC-BASED FRAUD (9 regras) - parcialmente implementadas
- GRUPO 11: DUPLICATE & REPEAT PATTERNS (5 regras) - parcialmente implementadas
- GRUPO 12: SPECIAL PATTERNS (5 regras) - não implementadas

---

## Plano de Ação Sugerido

### Sprint 0 (BLOQUEADOR - Antes de Qualquer Homologação)

1. [ ] **URGENTE**: Configurar Spring Security com autenticação/autorização - G-001, R-001

### Sprint 1 (Pré-Homologação)

2. [ ] Implementar testes E2E básicos (Playwright) - G-002, R-002
3. [ ] Documentar pipeline CI/CD - G-004, R-004
4. [ ] Solicitar pen-test OWASP - G-005, R-003
5. [ ] Adicionar mais testes frontend - G-007

### Sprint 2 (Pós-Homologação)

6. [ ] Integrar SAST/DAST - G-006
7. [ ] Implementar métricas Prometheus - G-009
8. [ ] Adicionar cobertura de código - G-008
9. [ ] Implementar cache de regras - G-010, R-007
10. [ ] Documentar gap de regras ou implementar regras faltantes - G-003, R-005

### Backlog

11. [ ] Particionamento de transactions - G-014, R-008
12. [ ] ADRs formais - G-012
13. [ ] Storybook - G-013
14. [ ] Consolidar stacks de banco - R-009

---

## Resumo Executivo

- **Gaps P0**: 1 (Spring Security não configurado)
- **Gaps P1**: 7 (E2E, regras faltantes, CI/CD, pen-test, SAST/DAST, testes frontend, cobertura)
- **Riscos P0**: 1 (Sistema sem autenticação)
- **Riscos P1**: 5 (E2E, pen-test, CI/CD, regras faltantes, regras legadas)

**Conclusão**: Sistema tem fundamento técnico sólido, mas gaps críticos de segurança impedem homologação imediata.
