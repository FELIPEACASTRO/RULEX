# Catálogo Completo de Regras de Fraude (V31-V35)

**Data:** 2026-01-06
**Autor:** Manus AI
**Total de Regras:** 310

Este documento detalha as 310 regras de fraude implementadas nas migrations V31, V32, V33, V34 e V35, cobrindo um espectro completo de vetores de ataque e padrões de fraude.

## Sumário Executivo

| Migration | Título | Quantidade | Foco |
|---|---|---|---|
| **V31** | Regras SIMPLES | 100 | Condições únicas de alto risco |
| **V32** | Regras COMPLEXAS | 100 | Padrões multi-condição sofisticados |
| **V33** | Velocidade e Agregação | 50 | Análise de frequência e valores temporais |
| **V34** | Device e Geolocalização | 30 | Fingerprinting e análise de localização |
| **V35** | Comportamento e Padrão | 30 | Análise comportamental e de padrões de uso |

## V31: 100 Regras SIMPLES de Fraude

Esta migration implementa 100 regras de condição única para detecção rápida de sinais de fraude óbvios. As regras cobrem todos os 102 campos do payload do RULEX.

### Exemplos de Regras Simples:

- **S001:** `transactionAmount > 10000`
- **S002:** `mcc IN_LIST [6051, 7995, 5944]`
- **S003:** `merchantCountry IN_LIST [NG, RU, UA]`
- **S004:** `cvv2Response EQ N`
- **S005:** `isFirstTransaction EQ Y`
- **S006:** `deviceFraudHistory EQ Y`
- **S007:** `ipBlacklist EQ Y`
- **S008:** `emulatorDetected EQ Y`
- **S009:** `rootedDevice EQ Y`
- **S010:** `torDetected EQ Y`

## V32: 100 Regras COMPLEXAS de Fraude

Esta migration implementa 100 regras multi-condição para detecção de padrões de fraude sofisticados.

### Categorias Cobertas:

- **Card Testing (10 regras):** Detecção de validação de cartões roubados.
- **Account Takeover (10 regras):** Detecção de sequestro de contas.
- **Bust-Out Fraud (10 regras):** Detecção de fraude de crédito.
- **Friendly Fraud (10 regras):** Detecção de abuso de chargeback.
- **Synthetic Identity (10 regras):** Detecção de identidade sintética.
- **Money Laundering (10 regras):** Detecção de lavagem de dinheiro.
- **Triangulation Fraud (10 regras):** Detecção de fraude de triangulação.
- **Refund Fraud (10 regras):** Detecção de abuso de reembolso.
- **Promo/Coupon Abuse (10 regras):** Detecção de abuso de promoções.
- **Miscellaneous (10 regras):** Outros padrões complexos.

### Exemplo de Regra Complexa (C001: Card Testing Clássico):

- **Condição 1:** `transactionAmount < 5`
- **Condição 2:** `terminalType EQ ECOM`
- **Condição 3:** `cvv2Response EQ N`
- **Lógica:** `AND`
- **Decisão:** `BLOCK`

## V33: 50 Regras de VELOCIDADE e AGREGAÇÃO

Esta migration implementa 50 regras baseadas em frequência e agregações temporais, utilizando os novos operadores da DSL.

### Categorias Cobertas:

- **Velocidade de Transações (15 regras):** Análise de frequência em janelas de tempo.
- **Agregação de Valores (15 regras):** Análise de soma, média e valores máximos.
- **Contagem Distinta (10 regras):** Análise de contagem de entidades únicas.
- **Padrões Temporais (10 regras):** Análise de comportamento em horários específicos.

### Exemplo de Regra de Velocidade (V001: Alta Frequência 1h):

- **Condição:** `COUNT_LAST_N_HOURS > 10:1`
- **Lógica:** `AND`
- **Decisão:** `REVIEW`

## V34: 30 Regras de DEVICE FINGERPRINT e GEOLOCALIZAÇÃO

Esta migration implementa 30 regras baseadas em análise de dispositivo e geolocalização.

### Categorias Cobertas:

- **Device Fingerprint (15 regras):** Análise de dispositivo, emulador, proxy, etc.
- **Geolocalização (15 regras):** Análise de país, cidade, IP, viagem impossível, etc.

### Exemplo de Regra de Device (D001: Novo Device Alto Valor):

- **Condição 1:** `deviceId IS_NEW`
- **Condição 2:** `transactionAmount > 2000`
- **Lógica:** `AND`
- **Decisão:** `REVIEW`

## V35: 30 Regras de COMPORTAMENTO e PADRÃO

Esta migration implementa 30 regras baseadas em análise comportamental e de padrões de uso.

### Categorias Cobertas:

- **Mudança de Comportamento (15 regras):** Análise de desvios do comportamento normal.
- **Padrões de Uso (15 regras):** Análise de padrões de uso suspeitos (escada, round numbers, etc.).

### Exemplo de Regra de Comportamento (B001: Mudança de Valor Médio):

- **Condição:** `transactionAmount > customerAvgAmount * 5`
- **Lógica:** `AND`
- **Decisão:** `REVIEW`

## Conclusão

Com a implementação dessas 310 regras, o RULEX agora possui um dos mais completos e sofisticados arsenais de regras de fraude do mercado, totalmente parametrizado e pronto para produção. O sistema está equipado para combater desde fraudes simples até esquemas complexos de lavagem de dinheiro e roubo de identidade.
