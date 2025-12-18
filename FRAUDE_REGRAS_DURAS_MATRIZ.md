# Matriz priorizada — Regras Duras (payload imutável)

## Fontes efetivamente utilizadas (auditáveis no repo)
- [PESQUISA_FRAUDES_BRASIL.md](PESQUISA_FRAUDES_BRASIL.md) — cabeçalho indica 2024-2025 (menciona FEBRABAN/BCB)
- [DOCUMENTACAO_TECNICA.md](DOCUMENTACAO_TECNICA.md) — exemplos de regra/score e API
- [IMPLEMENTACAO_28_REGRAS.md](IMPLEMENTACAO_28_REGRAS.md) — catálogo de 28 regras “avançadas” (payload-only)
- [DOUBLE_CHECK_RIGOROSO_60_REGRAS.md](DOUBLE_CHECK_RIGOROSO_60_REGRAS.md) — validação de viabilidade vs payload
- [TRIPLE_CHECK_ANALISE_RIGOROSA.md](TRIPLE_CHECK_ANALISE_RIGOROSA.md) — inventário/uso dos campos
- Baseline de payload fornecido (CRTRAN25) — ver [FRAUDE_PAYLOAD_INVENTARIO.md](FRAUDE_PAYLOAD_INVENTARIO.md)

## Escopo
- Canais cobertos por este payload: **cartão** (crédito/débito) e sinais de autorização/EMV/3DS.
- Pix: **não coberto** (campos ausentes).
- Regras abaixo são **determinísticas** e usam **apenas campos do payload** (sem histórico/velocity).

## Thresholds (conservadores)
- Valores (amount): pisos altos (ex.: >= 500, >= 1000) para reduzir FP em regras de “ausência de sinal”.
- Conversão: fora de faixa (<=0 ou >=10) como anomalia grosseira.
- ATC divergente: diferença >=5 como inconsistência grosseira.

> Nota: thresholds devem ser calibrados por portfólio/MCC após operação, mas aqui começam “superconservadores” para evitar bloqueios indevidos.

---

## Regras P0 (alto impacto / baixo FP)

### CARD-P0-001 — Cartão expirado
- Canal: CREDIT_DEBIT
- Severidade: HIGH
- Condição:
  - `cardExpireDate < transactionDate`
- Campos usados: `cardExpireDate`, `transactionDate`
- Justificativa: inconsistência objetiva do instrumento.
- Evidência: [DOUBLE_CHECK_RIGOROSO_60_REGRAS.md](DOUBLE_CHECK_RIGOROSO_60_REGRAS.md) (discute `cardExpireDate` e coerência)
- Impacto FP: baixo (dados corretos) / médio (dados ruins upstream)
- Mitigação: se FP por qualidade, tratar como `SUSPICIOUS` e exigir verificação adicional.
- Exemplos:
  - Fraudulenta: `cardExpireDate=20211029`, `transactionDate=20250210`
  - Suspeita: `cardExpireDate=20250101`, `transactionDate=20250102`
  - Legítima: `cardExpireDate=20261231`, `transactionDate=20250210`

### CARD-P0-002 — CVV2 mismatch com valor não-trivial
- Canal: CREDIT_DEBIT
- Severidade: HIGH
- Condição:
  - `cvv2Response != 'M' AND transactionAmount >= 100`
- Campos usados: `cvv2Response`, `transactionAmount`
- Evidência: [PESQUISA_FRAUDES_BRASIL.md](PESQUISA_FRAUDES_BRASIL.md) (2.7)
- Impacto FP: médio (erros de integração) — por isso piso de valor
- Mitigação: reduzir severidade em MCC de baixo risco; exigir 3DS quando aplicável.
- Exemplos:
  - Fraudulenta: `cvv2Response='N'`, `transactionAmount=900`
  - Suspeita: `cvv2Response='U'`, `transactionAmount=150`
  - Legítima: `cvv2Response='M'`, `transactionAmount=150`

### CARD-P0-003 — PIN offline falhou
- Canal: CREDIT_DEBIT
- Severidade: HIGH
- Condição:
  - `cvrofflinePinVerificationPerformed == 1 AND cvrofflinePinVerificationFailed == 1`
- Campos usados: `cvrofflinePinVerificationPerformed`, `cvrofflinePinVerificationFailed`
- Evidência: [IMPLEMENTACAO_28_REGRAS.md](IMPLEMENTACAO_28_REGRAS.md) (OFFLINE_PIN_FAILED)
- Impacto FP: baixo-médio (sinal de falha real) 
- Mitigação: permitir retry controlado; exigir canal seguro.
- Exemplos:
  - Fraudulenta: `performed=1`, `failed=1`
  - Suspeita: `performed=1`, `failed=1`, `transactionAmount=5000`
  - Legítima: `performed=1`, `failed=0`

### CARD-P0-004 — Criptograma inválido
- Canal: CREDIT_DEBIT
- Severidade: HIGH
- Condição:
  - `cryptogramValid != 'V'`
- Campos usados: `cryptogramValid`
- Evidência: [DOCUMENTACAO_TECNICA.md](DOCUMENTACAO_TECNICA.md) (INVALID_CRYPTOGRAM)
- Impacto FP: baixo (sinal objetivo)
- Mitigação: se ocorrer em ondas, investigar integridade do emissor/adquirente.
- Exemplos:
  - Fraudulenta: `cryptogramValid='N'`
  - Suspeita: `cryptogramValid='0'`
  - Legítima: `cryptogramValid='V'`

### CARD-P0-005 — Conversão de moeda anômala (grossa)
- Canal: CREDIT_DEBIT
- Severidade: MEDIUM
- Condição:
  - `transactionCurrencyConversionRate <= 0 OR transactionCurrencyConversionRate >= 10`
- Campos usados: `transactionCurrencyConversionRate`
- Evidência: [IMPLEMENTACAO_28_REGRAS.md](IMPLEMENTACAO_28_REGRAS.md) (ANOMALOUS_CONVERSION_RATE)
- Impacto FP: médio (qualidade de dado) 
- Mitigação: tratar como sinal auxiliar (não bloqueante sozinho) ou validar com adquirência.
- Exemplos:
  - Fraudulenta: `rate=0`
  - Suspeita: `rate=12`
  - Legítima: `rate=0.19`

---

## Regras P1 (alto valor, FP controlável por piso)

### CARD-P1-001 — E-commerce/Não presencial sem 3DS (ECI=0) com valor alto
- Canal: CREDIT_DEBIT
- Severidade: MEDIUM
- Condição:
  - `customerPresent == 'N' AND eciIndicator == 0 AND transactionAmount >= 500`
- Campos: `customerPresent`, `eciIndicator`, `transactionAmount`
- Evidência: [PESQUISA_FRAUDES_BRASIL.md](PESQUISA_FRAUDES_BRASIL.md) (2.3)
- FP: médio (fluxos legítimos sem 3DS)
- Mitigação: elevar para SUSPICIOUS (não FRAUD) e exigir step-up quando existir.
- Exemplos:
  - Fraudulenta: `customerPresent='N'`, `eciIndicator=0`, `amount=1500`
  - Suspeita: `customerPresent='N'`, `eciIndicator=0`, `amount=600`
  - Legítima: `customerPresent='Y'`, `eciIndicator=0`, `amount=600`

### CARD-P1-002 — MCC alto risco + valor não-trivial
- Canal: CREDIT_DEBIT
- Severidade: HIGH
- Condição:
  - `mcc IN {7995,7994,5967,6051,4829} AND transactionAmount >= 50`
- Campos: `mcc`, `transactionAmount`
- Evidência: [PESQUISA_FRAUDES_BRASIL.md](PESQUISA_FRAUDES_BRASIL.md) (MCCs alto risco)
- FP: médio (alguns MCCs têm uso legítimo)
- Mitigação: combinar com sinais de autenticação (CVV/3DS/EMV) antes de bloquear.
- Exemplos:
  - Fraudulenta: `mcc=6051`, `amount=500`
  - Suspeita: `mcc=7995`, `amount=70`
  - Legítima: `mcc=5411`, `amount=70`

### CARD-P1-003 — POS off-premises + posSecurity=0 + valor alto
- Canal: CREDIT_DEBIT
- Severidade: MEDIUM
- Condição:
  - `posOffPremises == 1 AND posSecurity == 0 AND transactionAmount >= 500`
- Campos: `posOffPremises`, `posSecurity`, `transactionAmount`
- Evidência: [PESQUISA_FRAUDES_BRASIL.md](PESQUISA_FRAUDES_BRASIL.md) (2.5 “maquininha”, sinais de POS)
- FP: médio (configuração de POS)
- Mitigação: usar como sinal auxiliar ou exigir confirmação.
- Exemplos:
  - Fraudulenta: `posOffPremises=1`, `posSecurity=0`, `amount=1200`
  - Suspeita: `posOffPremises=1`, `posSecurity=0`, `amount=700`
  - Legítima: `posOffPremises=0`, `posSecurity=1`, `amount=700`

### CARD-P1-004 — CVV/PIN try limit excedido
- Canal: CREDIT_DEBIT
- Severidade: HIGH
- Condição:
  - `cvvPinTryLimitExceeded == 1`
- Campos: `cvvPinTryLimitExceeded`
- Evidência: [IMPLEMENTACAO_28_REGRAS.md](IMPLEMENTACAO_28_REGRAS.md) (PIN_CVV_LIMIT_EXCEEDED)
- FP: baixo-médio
- Mitigação: step-up; bloqueio temporário por canal.
- Exemplos:
  - Fraudulenta: `cvvPinTryLimitExceeded=1`
  - Suspeita: `cvvPinTryLimitExceeded=1`, `amount=900`
  - Legítima: `cvvPinTryLimitExceeded=0`

### CARD-P1-005 — ATC incoerente
- Canal: CREDIT_DEBIT
- Severidade: MEDIUM
- Condição:
  - `ABS(atcCard - atcHost) >= 5`
- Campos: `atcCard`, `atcHost`
- Evidência: [IMPLEMENTACAO_28_REGRAS.md](IMPLEMENTACAO_28_REGRAS.md) (INCOHERENT_AUTH_SEQUENCE)
- FP: baixo-médio
- Mitigação: usar como sinal auxiliar com outros.
- Exemplos:
  - Fraudulenta: `atcCard=9999`, `atcHost=100`
  - Suspeita: `atcCard=205`, `atcHost=198`
  - Legítima: `atcCard=9999`, `atcHost=9999`

---

## Regras P2 (sinais auxiliares / qualidade de dados)

### CARD-P2-001 — Lag de processamento anômalo (registro muito depois)
- Canal: CREDIT_DEBIT
- Severidade: LOW
- Condição:
  - `recordCreationDate > transactionDate` (ou mesma data com `recordCreationTime - transactionTime` muito alto)
- Campos: `recordCreationDate`, `recordCreationTime`, `transactionDate`, `transactionTime`
- Evidência: [IMPLEMENTACAO_28_REGRAS.md](IMPLEMENTACAO_28_REGRAS.md) (PROCESSING_LAG_ANOMALY)
- FP: médio (batching/backoffice)
- Mitigação: tratar como sinal de integridade, não como fraude direta.
- Exemplos:
  - Fraudulenta: `transactionDate=20250210`, `recordCreationDate=20250212`
  - Suspeita: `transactionDate=20250210`, `recordCreationTime=235959`, `transactionTime=001000`
  - Legítima: `transactionDate=20250210`, `recordCreationDate=20250210`

---

## Observações de cobertura
- Velocity/first-time patterns/ATO/SIM-swap/mulas exigem **histórico** ou campos ausentes (device/contato/beneficiário). Não entram em payload-only.
- Pix exige payload distinto (chave/PSP/ISPB/QR/iniciação etc.).
