# Matriz de Gaps e Riscos v2.0

**Data**: 2025-12-19  
**Projeto**: RULEX - Motor de Regras Duras
**Versao**: Arquitetura Simplificada (React + Java)

---

## Classificacao de Severidade

| Nivel | Descricao | Impacto em Homologacao |
|-------|-----------|------------------------|
| P0 | Bloqueador critico | Impede homologacao |
| P1 | Alto risco | Requer mitigacao antes de producao |
| P2 | Medio risco | Pode ir para producao com plano de acao |
| P3 | Baixo risco | Melhorias futuras |

---

## Gaps Identificados

### Gaps P0 (Bloqueadores)

| ID | Gap | Area | Responsavel | Evidencia |
|----|-----|------|-------------|-----------|
| - | Nenhum gap P0 identificado | - | - | - |

### Gaps P1 (Alto Risco)

| ID | Gap | Area | Responsavel | Evidencia |
|----|-----|------|-------------|-----------|
| G-001 | Testes E2E/Navegacao SPA ausentes | QA | QA Lead | Nao encontrado Cypress/Playwright |
| G-002 | Pipeline CI/CD nao documentado | DevOps | SRE | Nao encontrado .github/workflows |
| G-003 | Pen-test nao documentado | Seguranca | AppSec | Nao encontrado relatorio OWASP |

### Gaps P2 (Medio Risco)

| ID | Gap | Area | Responsavel | Evidencia |
|----|-----|------|-------------|-----------|
| G-004 | Metricas Prometheus/Grafana | DevOps | SRE | Health check existe, falta exportacao |
| G-005 | Cobertura de codigo nao medida | QA | QA Lead | Nao encontrado jacoco |
| G-006 | Cache de regras ausente | Backend | Java Engineer | Regras recarregadas a cada request |
| G-007 | Autenticacao frontend mock | Seguranca | AppSec | useAuth.ts retorna usuario fixo |
| G-008 | ADRs ausentes | Arquitetura | Arquiteto | Nao encontrado |

### Gaps P3 (Baixo Risco)

| ID | Gap | Area | Responsavel | Evidencia |
|----|-----|------|-------------|-----------|
| G-009 | Onboarding/tour guiado | UX | UX Designer | Nao encontrado |
| G-010 | Lazy loading em rotas | Frontend | React Engineer | App.tsx sem React.lazy |
| G-011 | Storybook para componentes | Frontend | React Engineer | Nao encontrado |
| G-012 | Componentes grandes | Frontend | React Engineer | RulesDidactic.tsx 1200+ linhas |

---

## Riscos Identificados

### Riscos P0 (Bloqueadores)

| ID | Risco | Area | Probabilidade | Impacto | Mitigacao |
|----|-------|------|---------------|---------|-----------|
| - | Nenhum risco P0 identificado | - | - | - | - |

### Riscos P1 (Alto Risco)

| ID | Risco | Area | Probabilidade | Impacto | Mitigacao |
|----|-------|------|---------------|---------|-----------|
| R-001 | Bugs de integracao passam sem E2E | QA | Alta | Alto | Implementar Playwright |
| R-002 | Vulnerabilidades sem pen-test | Seguranca | Media | Critico | Contratar pen-test |
| R-003 | Deploy manual propenso a erros | DevOps | Alta | Alto | Implementar CI/CD |

### Riscos P2 (Medio Risco)

| ID | Risco | Area | Probabilidade | Impacto | Mitigacao |
|----|-------|------|---------------|---------|-----------|
| R-004 | Performance degrada com muitas regras | Backend | Media | Medio | Implementar cache |
| R-005 | Tabela transactions cresce sem controle | DBA | Alta | Medio | Implementar particionamento |
| R-006 | Autenticacao mock em producao | Seguranca | Baixa | Alto | Integrar auth real |

### Riscos P3 (Baixo Risco)

| ID | Risco | Area | Probabilidade | Impacto | Mitigacao |
|----|-------|------|---------------|---------|-----------|
| R-007 | Curva de aprendizado alta | UX | Media | Baixo | Criar onboarding |
| R-008 | Bundle size frontend grande | Frontend | Baixa | Baixo | Code splitting |

---

## Matriz de Priorizacao

```
         IMPACTO
           ↑
     Alto  │  R-002   R-001
           │  R-003   R-006
    Medio  │  R-004   R-005
           │
    Baixo  │  R-007   R-008
           └─────────────────────────→ PROBABILIDADE
              Baixa   Media   Alta
```

---

## Comparacao v1.0 vs v2.0

| Metrica | v1.0 | v2.0 | Variacao |
|---------|------|------|----------|
| Gaps P0 | 0 | 0 | = |
| Gaps P1 | 4 | 3 | -1 |
| Riscos P1 | 4 | 3 | -1 |
| Complexidade | Alta (2 backends) | Media (1 backend) | Melhor |

### Gap Removido
- **SAST/DAST**: Reclassificado para recomendacao pos-producao (nao bloqueador)

---

## Plano de Acao Sugerido

### Sprint 1 (Pre-Homologacao)
1. [ ] Implementar testes E2E basicos (Playwright) - G-001
2. [ ] Documentar pipeline CI/CD - G-002
3. [ ] Solicitar pen-test OWASP - G-003

### Sprint 2 (Pos-Homologacao)
4. [ ] Implementar metricas Prometheus - G-004
5. [ ] Adicionar cobertura de codigo (jacoco) - G-005
6. [ ] Implementar cache de regras - G-006
7. [ ] Integrar autenticacao real - G-007

### Backlog
8. [ ] ADRs formais - G-008
9. [ ] Onboarding - G-009
10. [ ] Storybook - G-011
