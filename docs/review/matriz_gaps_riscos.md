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
| - | Nenhum gap P0 identificado | - | - | - |

### Gaps P1 (Alto Risco)

| ID | Gap | Área | Responsável | Evidência |
|----|-----|------|-------------|-----------|
| G-001 | Testes E2E/Navegação SPA ausentes | QA | QA Lead | Não encontrado Cypress/Playwright |
| G-002 | Pipeline CI/CD não documentado | DevOps | SRE | Não encontrado .github/workflows |
| G-003 | Pen-test não documentado | Segurança | AppSec | Não encontrado relatório OWASP |
| G-004 | SAST/DAST não integrado | Segurança | AppSec | Não encontrado SonarQube/Snyk |

### Gaps P2 (Médio Risco)

| ID | Gap | Área | Responsável | Evidência |
|----|-----|------|-------------|-----------|
| G-005 | Métricas Prometheus/Grafana | DevOps | SRE | Health check existe, falta exportação |
| G-006 | Cobertura de código não medida | QA | QA Lead | Não encontrado jacoco/lcov |
| G-007 | Cache de regras ausente | Backend | Java Engineer | Regras recarregadas a cada request |
| G-008 | Política de retenção de dados | DBA | DBA | Não encontrado |
| G-009 | ADRs (Architecture Decision Records) | Arquitetura | Arquiteto | Não encontrado |
| G-010 | Storybook para componentes | Frontend | React Engineer | Não encontrado |

### Gaps P3 (Baixo Risco)

| ID | Gap | Área | Responsável | Evidência |
|----|-----|------|-------------|-----------|
| G-011 | Onboarding/tour guiado | UX | UX Designer | Não encontrado |
| G-012 | Lazy loading em rotas | Frontend | React Engineer | App.tsx sem React.lazy |
| G-013 | Tokens de design formalizados | UI | UI Designer | Parcialmente em DESIGN_SYSTEM.md |

---

## Riscos Identificados

### Riscos P0 (Bloqueadores)

| ID | Risco | Área | Probabilidade | Impacto | Mitigação |
|----|-------|------|---------------|---------|-----------|
| - | Nenhum risco P0 identificado | - | - | - | - |

### Riscos P1 (Alto Risco)

| ID | Risco | Área | Probabilidade | Impacto | Mitigação |
|----|-------|------|---------------|---------|-----------|
| R-001 | Bugs de integração passam sem E2E | QA | Alta | Alto | Implementar Playwright |
| R-002 | Vulnerabilidades sem pen-test | Segurança | Média | Crítico | Contratar pen-test |
| R-003 | Deploy manual propenso a erros | DevOps | Alta | Alto | Implementar CI/CD |
| R-004 | Regras legadas divergem de genéricas | Backend | Média | Médio | Migrar para condições JSON |

### Riscos P2 (Médio Risco)

| ID | Risco | Área | Probabilidade | Impacto | Mitigação |
|----|-------|------|---------------|---------|-----------|
| R-005 | Performance degrada com muitas regras | Backend | Média | Médio | Implementar cache |
| R-006 | Tabela transactions cresce sem controle | DBA | Alta | Médio | Implementar particionamento |
| R-007 | Duas stacks de banco (PG + MySQL) | Arquitetura | Baixa | Médio | Consolidar em uma stack |

### Riscos P3 (Baixo Risco)

| ID | Risco | Área | Probabilidade | Impacto | Mitigação |
|----|-------|------|---------------|---------|-----------|
| R-008 | Curva de aprendizado alta | UX | Média | Baixo | Criar onboarding |
| R-009 | Bundle size frontend cresce | Frontend | Baixa | Baixo | Lazy loading |
| R-010 | Divergência visual | UI | Baixa | Baixo | Storybook |

---

## Matriz de Priorização

```
         IMPACTO
           ↑
     Alto  │  R-002   R-001
           │  R-003   R-004
    Médio  │  R-005   R-006   R-007
           │
    Baixo  │  R-008   R-009   R-010
           └─────────────────────────→ PROBABILIDADE
              Baixa   Média   Alta
```

---

## Plano de Ação Sugerido

### Sprint 1 (Pré-Homologação)
1. [ ] Implementar testes E2E básicos (Playwright) - G-001
2. [ ] Documentar pipeline CI/CD - G-002
3. [ ] Solicitar pen-test OWASP - G-003

### Sprint 2 (Pós-Homologação)
4. [ ] Integrar SAST/DAST - G-004
5. [ ] Implementar métricas Prometheus - G-005
6. [ ] Adicionar cobertura de código - G-006
7. [ ] Implementar cache de regras - G-007

### Backlog
8. [ ] Particionamento de transactions - R-006
9. [ ] ADRs formais - G-009
10. [ ] Storybook - G-010
