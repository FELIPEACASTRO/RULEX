# Inventário de Regras (código como verdade)

## 1) Regras configuráveis (DB) — RuleEngineService
A avaliação principal (`POST /transactions/analyze`) carrega regras habilitadas via `RuleConfigurationRepository.findByEnabled(true)` e avalia:

1) **Condições genéricas**: se `conditionsJson` estiver preenchido (lista de `RuleConditionDTO`), aplica AND/OR e operadores.
2) **Fallback legado por nome** (compatibilidade): quando não há `conditionsJson`, algumas regras por `ruleName` possuem mapeamento hard-coded.

### Operadores suportados (condições)
O motor avalia `field`, `operator`, `value` contra os campos da entidade `Transaction` (via reflexão). Os operadores dependem da implementação de `evaluateCondition(...)`.

### Regras legadas por nome (fallback)
- `LOW_AUTHENTICATION_SCORE`
- `LOW_EXTERNAL_SCORE`
- `INVALID_CAVV`
- `INVALID_CRYPTOGRAM`
- `CVV_MISMATCH`
- `HIGH_TRANSACTION_AMOUNT`
- `HIGH_RISK_MCC`
- `INTERNATIONAL_TRANSACTION`
- `CARD_NOT_PRESENT`
- `CVV_PIN_LIMIT_EXCEEDED`
- `OFFLINE_PIN_FAILED`

Resultado:
- `riskScore` = soma de pesos (capado em 100)
- `classification` = severidade máxima entre score e classificação das regras disparadas

---

## 2) Regras avançadas (28) — AdvancedRuleEngineService
Endpoint: `POST /transactions/analyze-advanced`

Regras (por chave):
1. `EMV_SECURITY_CHECK`
2. `TERMINAL_VERIFICATION_FAILED`
3. `EXPIRED_CARD`
4. `SUSPICIOUS_TRANSACTION_TYPE`
5. `UNUSUAL_CARD_MEDIA`
6. `SUSPICIOUS_TERMINAL`
7. `ECOMMERCE_NO_AVS`
8. `POS_SECURITY_MISSING`
9. `CARD_CAPTURE_FRAUD`
10. `PIN_CVV_LIMIT_EXCEEDED`
11. `OFFLINE_PIN_FAILED`
12. `MISSING_CVV2_HIGH_RISK`
13. `CUSTOM_INDICATOR_FRAUD`
14. `PROCESSING_LAG_ANOMALY`
15. `TIMEZONE_NORMALIZED_CHECK`
16. `DUPLICATE_TRANSACTION`
17. `SUSPICIOUS_MERCHANT_POSTAL`
18. `SUSPICIOUS_TOKEN`
19. `UNEXPECTED_CURRENCY`
20. `ANOMALOUS_CONVERSION_RATE`
21. `INCOHERENT_AUTH_SEQUENCE`
22. `INCOHERENT_CONTEXT`
23. `CONTRADICTORY_AUTHORIZATION`
24. `SUSPICIOUS_ACQUIRER`
25. `ACQUIRER_COUNTRY_MISMATCH`
26. `COMBINED_SCORE_CHECK`
27. `VELOCITY_CHECK_CONSOLIDATED`
28. `CUSTOM_INDICATORS_COMPREHENSIVE`

Observações:
- A execução retorna o resultado **mais severo** entre as 28.
- Quando uma regra dispara, ela é registrada como `TriggeredRuleDTO(name=<ruleKey>, detail="advanced")`.

---

## 3) Regras e RuleSets de Homolog (DSL)
Endpoints sob `/homolog/*` permitem:
- Criar versões de regra (`/homolog/rules`)
- Publicar e fazer rollback de versões
- Criar versões de ruleset e ativar ruleset
- Simular execução (`/homolog/simulations/run`)

Os detalhes da DSL e validações estão encapsulados nos serviços de aplicação `HomologRuleApplicationService` e `HomologRuleSetApplicationService`.
