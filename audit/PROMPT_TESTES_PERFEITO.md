# PROMPT DE TESTES PERFEITO — RULEX (AUDITORIA BANCÁRIA EXTREMA)

Você está operando como **banca de certificação**. **Ausência de prova = falha**.

## Entradas obrigatórias
- Repositório RULEX (workspace completo)
- Payload base imutável: `fixtures/crtran.json`

### Restrição do payload
- **Proibido** alterar estrutura (chaves, tipos, remoção/adição de campos).
- Permitido apenas **PATCH/MUTATION CONTROLADA** via JSON Patch (RFC 6902) com operações `replace` (e, somente se o campo já existir no JSON, `add` no mesmo caminho é equivalente a replace; prefira `replace`).

---

# FORMATO DE SAÍDA OBRIGATÓRIO (você deve produzir exatamente nesta ordem)
1) PASSO 0 — prova de leitura completa do zip
2) PASSO 1 — inventário real extraído do código
3) PASSO 2 — catálogo completo de regras duras
4) PASSO 3 — matriz regra×campo×teste×evidência
5) PASSO 4 — construção da suíte (com `fixtures/crtran.json`)
6) PASSO 5 — tipos de teste obrigatórios
7) PASSO 6 — contratos e observabilidade
8) PASSO 7 — veredito + nota + gaps

Regra final: se não provar com arquivo/trecho/teste/asserção → **GAP** → **REPROVA**.

---

# PASSO 0 — PROVA DE LEITURA COMPLETA DO ZIP
## 0.1 Evidência já existente no repo (obrigatório citar)
- `audit/inventory_all_files.txt`
- `audit/inventory_git_ls_files.txt`
- `audit/filetype_counts_all.json` e `audit/filetype_counts_all.txt`
- `audit/filetype_counts_git.json` e `audit/filetype_counts_git.txt`

## 0.2 Regerar e comparar (obrigatório)
Execute e **salve a saída** (anexe como artefato de auditoria):

### Windows PowerShell
```powershell
Set-Location "$PWD"  # deve ser a raiz do repo
New-Item -ItemType Directory -Force audit | Out-Null

# inventário full (filesystem)
Get-ChildItem -Recurse -Force | Select-Object FullName | Out-File -Encoding utf8 audit\inventory_all_files.REGEN.txt

# inventário tracked (git)
git ls-files | Out-File -Encoding utf8 audit\inventory_git_ls_files.REGEN.txt

# contagem por extensão (full)
Get-Content audit\inventory_all_files.REGEN.txt |
  ForEach-Object {
    $p = $_
    $ext = [IO.Path]::GetExtension($p)
    if ([string]::IsNullOrWhiteSpace($ext)) { "<noext>" } else { $ext.ToLower() }
  } |
  Group-Object | Sort-Object Count -Descending |
  Select-Object Name,Count | ConvertTo-Json | Out-File -Encoding utf8 audit\filetype_counts_all.REGEN.json

# contagem por extensão (git)
git ls-files |
  ForEach-Object {
    $ext = [IO.Path]::GetExtension($_)
    if ([string]::IsNullOrWhiteSpace($ext)) { "<noext>" } else { $ext.ToLower() }
  } |
  Group-Object | Sort-Object Count -Descending |
  Select-Object Name,Count | ConvertTo-Json | Out-File -Encoding utf8 audit\filetype_counts_git.REGEN.json
```

## 0.3 Critério objetivo
- Se `REGEN` divergir dos arquivos `audit/*` existentes sem justificativa → **GAP**.
- Se qualquer subpasta não aparecer no inventário full → **REPROVA**.

---

# PASSO 1 — INVENTÁRIO REAL EXTRAÍDO DO CÓDIGO
Você deve produzir inventários **com paths e trechos** para:

## 1.1 Backend (Java)
Obrigatório mapear:
- Endpoints REST: controllers e mappings
- Serviços decisórios
- Motores de regra (configurável/legado e 28 regras avançadas)
- Auditoria/mascaramento

Pontos mínimos que você deve citar por arquivo:
- Motor configurável/legado: `backend/src/main/java/com/rulex/service/RuleEngineService.java`
- Motor avançado 28 regras: `backend/src/main/java/com/rulex/service/AdvancedRuleEngineService.java`
- Entrada REST: `backend/src/main/java/com/rulex/controller/TransactionController.java`
- Homolog (versionamento/simulação): `backend/src/main/java/com/rulex/controller/homolog/*`

## 1.2 DB
Obrigatório inventariar objetos:
- Migrações: `backend/src/main/resources/db/migration/V1__init.sql`, `backend/src/main/resources/db/migration/V2__core_schema.sql`
- Tabelas, índices, constraints, enums/checks

## 1.3 Frontend (React SPA)
Obrigatório inventariar:
- Rotas SPA e páginas P0/P1: `client/src/App.tsx`
- Fluxos principais (Rules, Audit, Simulator, Transactions)

Atenção (critério bancário): qualquer fallback de “simulação local” no frontend deve ser tratado como **não evidência** para certificação; só vale evidência FE→BE→DB.

---

# PASSO 2 — EXTRAÇÃO DAS REGRAS DURAS “SEM PERDÃO”

Você deve construir um catálogo com:
- ID único
- Nome
- Arquivo/classe/método
- Condição exata
- Campos do `fixtures/crtran.json`
- Severidade (deny/review/allow)
- Efeitos colaterais (DB/audit/log)

## 2.1 Catálogo mínimo já identificável (não substitui sua extração completa)

### 2.1.1 Motor avançado (28 regras determinísticas)
Fonte: `backend/src/main/java/com/rulex/service/AdvancedRuleEngineService.java`
Ordem fixa (deve ser citada): `executeAllAdvancedRulesDetailed`.

Regras 1–28 (nome lógico == `TriggeredRuleDTO.name`):
1. EMV_SECURITY_CHECK
2. TERMINAL_VERIFICATION_FAILED
3. EXPIRED_CARD
4. SUSPICIOUS_TRANSACTION_TYPE
5. UNUSUAL_CARD_MEDIA
6. SUSPICIOUS_TERMINAL
7. ECOMMERCE_NO_AVS
8. POS_SECURITY_MISSING
9. CARD_CAPTURE_FRAUD
10. PIN_CVV_LIMIT_EXCEEDED
11. OFFLINE_PIN_FAILED
12. MISSING_CVV2_HIGH_RISK
13. CUSTOM_INDICATOR_FRAUD
14. PROCESSING_LAG_ANOMALY
15. TIMEZONE_NORMALIZED_CHECK
16. DUPLICATE_TRANSACTION
17. SUSPICIOUS_MERCHANT_POSTAL
18. SUSPICIOUS_TOKEN
19. UNEXPECTED_CURRENCY
20. ANOMALOUS_CONVERSION_RATE
21. INCOHERENT_AUTH_SEQUENCE
22. INCOHERENT_CONTEXT
23. CONTRADICTORY_AUTHORIZATION
24. SUSPICIOUS_ACQUIRER
25. ACQUIRER_COUNTRY_MISMATCH
26. COMBINED_SCORE_CHECK
27. VELOCITY_CHECK_CONSOLIDATED
28. CUSTOM_INDICATORS_COMPREHENSIVE

### 2.1.2 Motor configurável (DB) + fallback legado por nome
Fonte: `backend/src/main/java/com/rulex/service/RuleEngineService.java`
- Regras configuráveis: `RuleConfiguration.conditionsJson` + `logicOperator` (AND/OR)
- Operadores suportados (motor configurável): `== != > < >= <= IN NOT_IN CONTAINS NOT_CONTAINS`
- Fallback legado por nome (mínimo de 12 nomes):
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

**Obrigatório:** validar se há duplicidade/erro de mapeamento (ex.: duas regras usando mesmo campo) e tratar como **GAP** se incoerente.

### 2.1.3 Homolog (motor versionado)
- Avaliador DSL: `backend/src/main/java/com/rulex/homolog/adapter/RuleDslEvaluatorAdapter.java`
- Operadores permitidos: `EQ, NEQ, GT, GTE, LT, LTE, IN, NOT_IN, CONTAINS, STARTS_WITH, ENDS_WITH, IS_NULL, NOT_NULL`
- Normalização rígida de campo (somente `[A-Za-z0-9_]+`)

---

# PASSO 3 — MATRIZ REGRA × CAMPO × TESTE × EVIDÊNCIA

## 3.1 Regras avançadas (28) — matriz preenchida (mínimo)

### Nota crítica sobre baseline
O payload base `fixtures/crtran.json` **pode** acionar múltiplas regras simultaneamente.
- Obrigatório: criar um teste baseline (sem patch) que registra **todas** as regras acionadas.
- Para testes “1 regra por teste”, é permitido usar um **PATCH de normalização** fixo (aplicado sempre antes) para neutralizar ruídos, desde que:
  - não mude schema/tipos
  - o patch fique versionado e auditável
  - a suíte também inclua o baseline puro

Abaixo, por regra, o **patch mínimo** assume que você está partindo de um baseline “normalizado” (definido por você e auditado).

| Regra | Campos do crtran.json | Tipo de teste | Caso de teste | Patch aplicado (RFC6902) | Asserções mínimas | Evidência |
|---|---|---|---|---|---|---|
| EMV_SECURITY_CHECK | cardAipStatic/cardAipDynamic/cardAipVerify, transactionAmount | unit (Java) | adv_rule01_emv_invalid_high_amount | replace cardAipStatic->"N"; replace transactionAmount->1001 | resultado=SUSPICIOUS; audit logRule("EMV_SECURITY_CHECK") | print/assert JUnit |
| TERMINAL_VERIFICATION_FAILED | terminalVerificationResults/cardVerificationResults | unit | adv_rule02_terminal_fail | replace terminalVerificationResults->"FAIL" | resultado=FRAUD | JUnit |
| EXPIRED_CARD | cardExpireDate, transactionDate | unit | adv_rule03_expired | replace cardExpireDate->20240101; replace transactionDate->20250101 | resultado=FRAUD | JUnit |
| SUSPICIOUS_TRANSACTION_TYPE | transactionType, transactionAmount | unit+stub DB | adv_rule04_reversal_above_2x_avg | replace transactionType->"R"; replace transactionAmount->25; stub avg=10 | resultado=SUSPICIOUS | JUnit |
| UNUSUAL_CARD_MEDIA | cardMediaType,posEntryMode | unit | adv_rule05_unusual_media | replace cardMediaType->"X"; replace posEntryMode->"E" | resultado=SUSPICIOUS | JUnit |
| SUSPICIOUS_TERMINAL | terminalType,posOffPremises,transactionAmount | unit | adv_rule06_atm_offprem_high | replace terminalType->"A"; replace posOffPremises->1; replace transactionAmount->5000.01 | SUSPICIOUS | JUnit |
| ECOMMERCE_NO_AVS | eciIndicator, avsRequest, transactionAmount | unit | adv_rule07_no_avs | replace eciIndicator->5; replace avsRequest->"N"; replace transactionAmount->1000.01 | SUSPICIOUS | JUnit |
| POS_SECURITY_MISSING | posSecurity,posEntryMode,transactionAmount | unit | adv_rule08_pos_sec_missing | replace posSecurity->0; replace posEntryMode->"C"; replace transactionAmount->2000.01 | SUSPICIOUS | JUnit |
| CARD_CAPTURE_FRAUD | posCardCapture, pan | unit+stub DB | adv_rule09_capture_count | replace posCardCapture->1; stub countCardCapturesSince=3 | FRAUD | JUnit |
| PIN_CVV_LIMIT_EXCEEDED | cvvPinTryLimitExceeded | unit | adv_rule10_limit_exceeded | replace cvvPinTryLimitExceeded->1 | FRAUD | JUnit |
| OFFLINE_PIN_FAILED | cvrofflinePinVerificationPerformed/cvrofflinePinVerificationFailed | unit | adv_rule11_offline_pin_failed | replace cvrofflinePinVerificationPerformed->1; replace cvrofflinePinVerificationFailed->1 | FRAUD | JUnit |
| MISSING_CVV2_HIGH_RISK | cvv2Present,mcc,transactionAmount | unit | adv_rule12_missing_cvv2 | replace cvv2Present->"0"; replace mcc->7995; replace transactionAmount->1000.01 | SUSPICIOUS | JUnit |
| CUSTOM_INDICATOR_FRAUD | userIndicator01/userIndicator03/userData04 | unit | adv_rule13_custom_flag | replace userIndicator01->"F" | FRAUD | JUnit |
| PROCESSING_LAG_ANOMALY | transactionTime, recordCreationTime, transactionAmount | unit | adv_rule14_lag | replace transactionTime->100000; replace recordCreationTime->200000; replace transactionAmount->5000.01 | SUSPICIOUS | JUnit |
| TIMEZONE_NORMALIZED_CHECK | transactionTime,gmtOffset,transactionAmount | unit | adv_rule15_night_tz | replace transactionTime->10000; replace gmtOffset->"+00.00"; replace transactionAmount->2000.01 | SUSPICIOUS | JUnit |
| DUPLICATE_TRANSACTION | externalTransactionId, transactionDate | unit+stub DB | adv_rule16_duplicate | stub countDuplicateTransactions=1 | FRAUD | JUnit |
| SUSPICIOUS_MERCHANT_POSTAL | merchantPostalCode | unit | adv_rule17_postal_invalid | replace merchantPostalCode->"00000012" | SUSPICIOUS | JUnit |
| SUSPICIOUS_TOKEN | tokenId, transactionAmount | unit | adv_rule18_test_token | replace tokenId->"MY_TEST_TOKEN"; replace transactionAmount->1000.01 | SUSPICIOUS | JUnit |
| UNEXPECTED_CURRENCY | transactionCurrencyCode, merchantCountryCode, transactionAmount | unit | adv_rule19_non_brl | replace merchantCountryCode->"076"; replace transactionCurrencyCode->840; replace transactionAmount->1000.01 | SUSPICIOUS | JUnit |
| ANOMALOUS_CONVERSION_RATE | transactionCurrencyCode, transactionCurrencyConversionRate | unit+stub DB | adv_rule20_rate_deviation | replace transactionCurrencyCode->840; replace transactionCurrencyConversionRate->2.0; stub avgRate=1.0 | SUSPICIOUS | JUnit |
| INCOHERENT_AUTH_SEQUENCE | cryptogramValid, cvv2Response, cavvResult, pinVerifyCode, tokenAssuranceLevel, consumerAuthenticationScore | unit | adv_rule21_incoherent_auth | replace cryptogramValid->"V"; replace cvv2Response->"N" | SUSPICIOUS | JUnit |
| INCOHERENT_CONTEXT | posEntryMode, customerPresent, terminalType, cardMediaType, cryptogramValid | unit | adv_rule22_incoherent_context | replace posEntryMode->"E"; replace customerPresent->"Y" | SUSPICIOUS | JUnit |
| CONTRADICTORY_AUTHORIZATION | authDecisionCode, authResponseCode, authPostFlag, transactionAmount, authId | unit | adv_rule23_contradiction | replace authDecisionCode->"A"; replace authResponseCode->"D" | SUSPICIOUS | JUnit |
| SUSPICIOUS_ACQUIRER | acquirerCountry, transactionAmount | unit | adv_rule24_acquirer | replace acquirerCountry->"999"; replace transactionAmount->10000.01 | SUSPICIOUS | JUnit |
| ACQUIRER_COUNTRY_MISMATCH | acquirerCountry, merchantCountryCode, transactionAmount | unit | adv_rule25_country_mismatch | replace acquirerCountry->"840"; replace merchantCountryCode->"076"; replace transactionAmount->5000.01 | SUSPICIOUS | JUnit |
| COMBINED_SCORE_CHECK | consumerAuthenticationScore, externalScore3 | unit | adv_rule26_combined_low | replace consumerAuthenticationScore->10; replace externalScore3->10 | FRAUD | JUnit |
| VELOCITY_CHECK_CONSOLIDATED | customerIdFromHeader, transactionDate | unit+stub DB | adv_rule27_velocity | stub countTransactionsByCustomerSince(5min)=3 | FRAUD | JUnit |
| CUSTOM_INDICATORS_COMPREHENSIVE | userIndicator01/03/04/05/08 | unit | adv_rule28_custom_comprehensive | replace userIndicator05->"ALERT" | FRAUD | JUnit |

**Obrigatório:** além de unit tests, criar ao menos 1 teste integrado chamando `/api/transactions/analyze-advanced` e validando `triggeredRules`.

## 3.2 Regras legadas/configuráveis (mínimo)
Você deve produzir matriz análoga para:
- cada regra legada por nome
- cada operador suportado no JSON de condições (==, !=, >, <, >=, <=, IN, NOT_IN, CONTAINS, NOT_CONTAINS)
- AND/OR + short-circuit

**Se não existir teste para um operador/branch → GAP.**

## 3.3 Homolog
Você deve produzir matriz para:
- cada operador do DSL
- validação de campo (regex) e erros
- AND/OR

---

# PASSO 4 — CONSTRUÇÃO DA SUÍTE DE TESTES (COM crtran.json)

## 4.1 Baseline test (imutável)
- Ler `fixtures/crtran.json` e enviar para:
  - `/api/transactions/analyze`
  - `/api/transactions/analyze-advanced`
- Registrar: classification, riskScore, triggeredRules, processingTimeMs, auditoria persistida.

## 4.2 1 teste por regra
- Para regras avançadas: aplicar patch mínimo e assertar disparo.
- Para regras configuráveis: criar RuleConfiguration no DB e disparar via payload.

## 4.3 1 teste por condição (branch/MC/DC)
- Para regras críticas (P0): produzir MC/DC explícito (tabelas) e implementar asserts.

## 4.4 Conflitos
- Criar payloads onde duas regras conflitam e provar regra de severidade (FRAUD > SUSPICIOUS > APPROVED).

## 4.5 Bordas e BigDecimal
- Montar casos com valores extremos (0, negativo, max) para campos monetários e provar validação/erro.

## 4.6 Idempotência
- Enviar o mesmo payload 3x e provar mesma decisão (e verificar persistência/audit não quebra invariantes).

---

# PASSO 5 — TIPOS DE TESTE OBRIGATÓRIOS

A) Unit (Java): obrigatório já existe base mínima
- `backend/src/test/java/com/rulex/service/AdvancedRuleEngineServiceTest.java`
- `backend/src/test/java/com/rulex/service/RuleEngineServiceTest.java`

B) Integration (API + PostgreSQL real): obrigatório
- `backend/src/test/java/com/rulex/controller/TransactionAnalyzeIT.java`
- `backend/src/test/java/com/rulex/homolog/HomologSimulationIT.java`

C) Regressão: obrigatório
- smoke P0 (curto)
- suíte deve passar 3 vezes seguidas (anti-flaky)

D) Navegação SPA (obrigatório)
- Rotas: `/`, `/dashboard`, `/transactions`, `/rules`, `/audit`, `/simulator`, `/404`.
- A jornada P0 deve incluir: listar regras → criar regra → validar persistência → simular transação → ver decisão/motivos → ver auditoria.

**Se não houver ferramenta E2E (Playwright/Cypress) no repo: você deve marcar como GAP e reprovar, ou adicionar e demonstrar execução reprodutível.**

E) E2E FE→BE→DB
- Rodar com backend ativo, sem fallback local.

F) Segurança (OWASP API / LGPD)
- Procurar autenticação/autorização, rate limit, validação e mascaramento.
- Se inexistente ou não testado: GAP.

G) Performance/Resiliência
- Testes de carga e latência; se não executáveis e versionados: GAP.

---

# PASSO 6 — CONTRATOS E OBSERVABILIDADE
Obrigatório:
- Validar contrato (OpenAPI) vs implementações e vs frontend.
- Validar que logs/audit não vazam PAN.

Atenção: o frontend possui simulação local quando a API falha; isso não é evidência bancária.

---

# PASSO 7 — VEREDITO
Você deve produzir:
- Critérios objetivos de aprovação/reprovação
- Lista de GAPS (cada item com arquivo/trecho e impacto)
- Nota 0–10
- Veredito: ❌ NÃO APTO / ⚠️ APTO COM RESSALVAS / ✅ APTO
