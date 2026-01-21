# RELATÓRIO MULTIDISCIPLINAR — GAPS DE USABILIDADE E NEGÓCIO (FRAUDE BANCÁRIA)

**Data:** 2026-01-19  
**Escopo:** RULEX (motor determinístico de regras)  
**Objetivo:** identificar lacunas de usabilidade e negócio para prevenção de fraudes bancárias, alinhadas a governança, auditoria, simulação, operação e compliance.

---

## 1) Equipe Multidisciplinar (papéis e foco)

1. **Fraude Bancária (SME)**: padrões de fraude, TTPs, priorização por impacto.
2. **Analista de Fraude (Operação)**: fluxo diário de criação/ajuste/teste/publicação de regras.
3. **Produto/Negócio**: objetivos, KPIs, governança e risco.
4. **Compliance/Regulatório (LGPD/PCI/AML)**: auditoria, retenção, consentimento e trilhas.
5. **UX/Research**: jornada do analista, fricções e legibilidade das decisões.
6. **Engenharia Backend**: compatibilidade com endpoints e modelos de dados.
7. **Engenharia Frontend**: usabilidade da criação/edição/simulação de regras.
8. **Ops/SRE**: observabilidade, alertas, rollback e incidentes.

---

## 2) Evidências analisadas (principais)

- Ausência de evidências de jornadas e casos de uso (Analista/Admin/Operação):  
  [docs/DIAGRAMAS.md](docs/DIAGRAMAS.md#L560-L700)
- Jornada de simulação descrita como SEM EVIDÊNCIA:  
  [docs/DIAGRAMAS.md](docs/DIAGRAMAS.md#L1110-L1185)
- Fluxo de regras e UI unificada (simples + complexas):  
  [client/src/pages/Rules.tsx](client/src/pages/Rules.tsx#L1-L220)
- Criador de regras com UX avançada (validação, tabs, preview):  
  [client/src/components/RuleFormDialog/RuleFormDialog.tsx](client/src/components/RuleFormDialog/RuleFormDialog.tsx#L1-L220)
- Builder de regras complexas + simulador embutido chamando /evaluate:  
  [client/src/components/ComplexRuleBuilder/RuleSimulator.tsx](client/src/components/ComplexRuleBuilder/RuleSimulator.tsx#L1-L220)
- Endpoint dedicado de simulação/backtest no backend:  
  [backend/src/main/java/com/rulex/controller/RuleSimulationController.java](backend/src/main/java/com/rulex/controller/RuleSimulationController.java#L1-L120)
- Gaps de dados no payload (campos comuns de fraude ausentes):  
  [docs/PAYLOAD_DICTIONARY.md](docs/PAYLOAD_DICTIONARY.md#L227-L242)  
  [docs/02_CAPABILITIES_EXTRACTION.md](docs/02_CAPABILITIES_EXTRACTION.md#L239-L260)

---

## 3) Gaps de USABILIDADE (impacto direto em analistas)

### U-01 — Descoberta/entrada inconsistente para regras complexas
**Evidência:** UI unifica listagem, mas o builder avançado está separado por fluxo e exige troca de contexto.  
[client/src/pages/Rules.tsx](client/src/pages/Rules.tsx#L1-L220)  
[client/src/components/ComplexRuleBuilder/index.tsx](client/src/components/ComplexRuleBuilder/index.tsx)

**Impacto:** analistas encontram dificuldade para migrar de regras simples para complexas, aumentando o tempo para iteração e risco de erros por falta de visibilidade de capacidades avançadas.

### U-02 — Simulação de regra não usa endpoint de simulação/backtest
**Evidência:** simulador do builder chama /evaluate com payload, sem contexto da regra, enquanto o backend possui endpoints de simulação/backtest.  
[client/src/components/ComplexRuleBuilder/RuleSimulator.tsx](client/src/components/ComplexRuleBuilder/RuleSimulator.tsx#L1-L220)  
[backend/src/main/java/com/rulex/controller/RuleSimulationController.java](backend/src/main/java/com/rulex/controller/RuleSimulationController.java#L1-L120)

**Impacto:** teste de regra incompleto e possível falsa confiança; não há backtest histórico ou comparação de cenários.

### U-03 — Ausência de jornada formal do analista (treinamento e consistência)
**Evidência:** casos de uso do analista estão marcados como SEM EVIDÊNCIA.  
[docs/DIAGRAMAS.md](docs/DIAGRAMAS.md#L560-L700)

**Impacto:** curva de aprendizado alta, ações inconsistentes e dificuldade de auditoria do processo humano.

### U-04 — Falta de fluxo guiado para publicação/rollback
**Evidência:** jornadas de publicação e rollback estão SEM EVIDÊNCIA.  
[docs/DIAGRAMAS.md](docs/DIAGRAMAS.md#L1110-L1185)

**Impacto:** risco operacional (publicação sem governança e rollback lento em incidentes).

---

## 4) Gaps de NEGÓCIO (fraude bancária e governança)

### N-01 — Gaps críticos de dados antifraude no payload
**Evidência:** lista de campos comuns de fraude ausentes, com impacto direto em device/IP/session/email/phone.  
[docs/PAYLOAD_DICTIONARY.md](docs/PAYLOAD_DICTIONARY.md#L227-L242)  
[docs/02_CAPABILITIES_EXTRACTION.md](docs/02_CAPABILITIES_EXTRACTION.md#L239-L260)

**Impacto:** impossibilidade de implementar regras essenciais para fraude bancária moderna (device fingerprint, IP intel, sessões, validação de email/telefone).

### N-02 — Backtest/impacto financeiro não expostos ao usuário
**Evidência:** backend provê backtest, mas a UX não oferece análise de impacto antes de publicar.  
[backend/src/main/java/com/rulex/controller/RuleSimulationController.java](backend/src/main/java/com/rulex/controller/RuleSimulationController.java#L1-L120)  
[client/src/components/ComplexRuleBuilder/RuleSimulator.tsx](client/src/components/ComplexRuleBuilder/RuleSimulator.tsx#L1-L220)

**Impacto:** decisões de publicação sem estimativa de falsos positivos, perdas ou carga operacional.

### N-03 — Governança e compliance sem trilha clara no produto
**Evidência:** jornadas e casos de uso (admin/analista/operação) sem evidência formal.  
[docs/DIAGRAMAS.md](docs/DIAGRAMAS.md#L560-L700)

**Impacto:** risco regulatório; ausência de trilha de aprovação, responsabilização e auditoria do processo humano.

---

## 5) Relatório de Ajustes (priorizado)

### Prioridade P0 (crítico)
1. **UX de simulação unificada com backend**  
   - Integrar UI com endpoints de simulação/backtest e permitir comparar cenários antes da publicação.  
   **Referências:** [backend/src/main/java/com/rulex/controller/RuleSimulationController.java](backend/src/main/java/com/rulex/controller/RuleSimulationController.java#L1-L120), [client/src/components/ComplexRuleBuilder/RuleSimulator.tsx](client/src/components/ComplexRuleBuilder/RuleSimulator.tsx#L1-L220)

2. **Gaps de dados antifraude**  
   - Roadmap de enriquecimento para deviceId, ipAddress, sessionId, userAgent, email/phone (integrações externas).  
   **Referências:** [docs/PAYLOAD_DICTIONARY.md](docs/PAYLOAD_DICTIONARY.md#L227-L242), [docs/02_CAPABILITIES_EXTRACTION.md](docs/02_CAPABILITIES_EXTRACTION.md#L239-L260)

### Prioridade P1 (alto)
3. **Jornadas do analista/admin/operação**  
   - Documentar fluxos e criar telas/assistentes para publicação, rollback e auditoria.  
   **Referências:** [docs/DIAGRAMAS.md](docs/DIAGRAMAS.md#L560-L700)

4. **Governança de publicação**  
   - Workflow de aprovação com evidências de teste, simulador e impacto antes de ativar regras.  
   **Referências:** [docs/DIAGRAMAS.md](docs/DIAGRAMAS.md#L1110-L1185)

### Prioridade P2 (médio)
5. **Descoberta do builder de regras complexas**  
   - Destacar “modo avançado” na mesma jornada e reduzir troca de contexto.  
   **Referências:** [client/src/pages/Rules.tsx](client/src/pages/Rules.tsx#L1-L220)

---

## 6) Resultado esperado após ajustes

- Redução de erros de publicação e incidentes em produção.
- Aumento da confiança em decisões de regra com backtest e impacto financeiro visível.
- Menor tempo de treinamento e operação para analistas.
- Base pronta para evolução de fraude bancária com enriquecimento de dados.

---

## 7) Próximos passos recomendados

1. Validar escopo com produto/compliance.
2. Priorizar integração de simulação/backtest na UI.
3. Executar plano de enriquecimento de payload antifraude.
4. Construir jornadas guiadas de publicação e rollback.

---

*Relatório gerado automaticamente com base nos artefatos do repositório RULEX.*
