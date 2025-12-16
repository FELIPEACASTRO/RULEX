# 🇧🇷 PESQUISA DE PADRÕES DE FRAUDE NO BRASIL

## Fonte: FEBRABAN, DataRudder, Banco Central (2024-2025)

---

## 1. ESTATÍSTICAS GERAIS (Brasil 2024)

| Métrica | Valor | Fonte |
|---------|-------|-------|
| Prejuízo total com golpes | R$ 10,1 bilhões | FEBRABAN |
| Crescimento vs 2023 | +17% | FEBRABAN |
| Brasileiros vítimas de fraude | 36% | FEBRABAN |
| Vítimas de fraudes digitais | 40 milhões | DataSenado |
| Crescimento fraudes Pix | +70% | Banco Central |
| Crescimento fraudes e-commerce | +66% | CommerceGate |

---

## 2. TIPOS DE FRAUDE MAIS COMUNS NO BRASIL

### 2.1 Clonagem de Cartão (44% dos casos)
**Descrição**: Uso de skimmers para copiar dados da tarja magnética.
**Padrão Detectável**:
- Transações em locais diferentes em curto espaço de tempo
- Uso de tarja magnética quando chip está disponível
- Transações online após uso presencial

**Regra RULEX**:
```
SE posEntryMode = "M" (tarja magnética)
E transactionAmount > 100
ENTÃO classificação = SUSPICIOUS (peso 60)
```

---

### 2.2 Falsa Central de Atendimento (32% dos casos)
**Descrição**: Criminoso se passa por atendente do banco.
**Padrão Detectável**:
- Transações após ligação telefônica
- Alteração de dados cadastrais seguida de transação
- Transações em horários atípicos

**Regra RULEX**:
```
SE transactionTime BETWEEN 22:00 AND 06:00
E transactionAmount > 1000
E customerPresent = "N"
ENTÃO classificação = SUSPICIOUS (peso 70)
```

---

### 2.3 Fraudes Online / E-commerce (24% dos brasileiros)
**Descrição**: Phishing, páginas falsas, apps maliciosos.
**Padrão Detectável**:
- Múltiplas transações pequenas seguidas de grande
- Transações em merchants novos
- Ausência de autenticação 3DS

**Regra RULEX**:
```
SE eciIndicator = 7 (sem autenticação)
E transactionAmount > 500
E customerPresent = "N"
ENTÃO classificação = SUSPICIOUS (peso 75)
```

---

### 2.4 Card Testing (Teste de Cartão)
**Descrição**: Criminosos testam cartões roubados com transações pequenas.
**Padrão Detectável**:
- Múltiplas transações < R$ 10 em curto período
- Mesmo cartão em merchants diferentes
- Transações em MCCs de alto risco

**Regra RULEX**:
```
SE transactionAmount < 10
E mcc IN (7995, 7994, 5967, 6051)
ENTÃO classificação = FRAUD (peso 85)
```

---

### 2.5 Golpe da Maquininha
**Descrição**: Alteração do valor na maquininha.
**Padrão Detectável**:
- Valores "redondos" incomuns
- Transações em terminais off-premises
- Múltiplas transações no mesmo terminal

**Regra RULEX**:
```
SE posOffPremises = 1
E transactionAmount > 500
E posSecurity = 0
ENTÃO classificação = SUSPICIOUS (peso 65)
```

---

### 2.6 Chargeback Fraudulento
**Descrição**: Compra legítima seguida de contestação falsa.
**Padrão Detectável**:
- Histórico de chargebacks do cliente
- Transações em MCCs de alto risco de chargeback
- Transações próximas ao limite de crédito

**Regra RULEX**:
```
SE transactionAmount > (availableCredit * 0.8)
E mcc IN (5967, 5968, 5969)
ENTÃO classificação = SUSPICIOUS (peso 55)
```

---

### 2.7 Uso de Dados Vazados
**Descrição**: Cartões de vazamentos testados em pequenas compras.
**Padrão Detectável**:
- Transações em merchants de teste (doações, assinaturas)
- Valores pequenos seguidos de grandes
- Ausência de CVV ou CVV incorreto

**Regra RULEX**:
```
SE cvv2Response != "M"
E transactionAmount > 100
ENTÃO classificação = SUSPICIOUS (peso 70)
```

---

## 3. IRREGULARIDADES ESPECÍFICAS DO BRASIL

### 3.1 Saque Simulado
**Descrição**: Compra fictícia para transformar limite em dinheiro.
**Padrão Detectável**:
- Valores redondos (R$ 500, R$ 1000, R$ 2000)
- MCCs de serviços (4829 - Wire Transfer)
- Transações em horários comerciais

**Regra RULEX**:
```
SE mcc = 4829
E transactionAmount IN (500, 1000, 2000, 5000)
ENTÃO classificação = SUSPICIOUS (peso 60)
```

---

### 3.2 Lavagem de Dinheiro via Cartão
**Descrição**: Uso de cartões para movimentar dinheiro ilícito.
**Padrão Detectável**:
- Transações internacionais em países de alto risco
- Valores próximos ao limite de reporte (R$ 10.000)
- Múltiplas transações em curto período

**Regra RULEX**:
```
SE transactionAmount BETWEEN 9000 AND 10000
E merchantCountryCode IN ("RU", "CN", "NG", "PK")
ENTÃO classificação = SUSPICIOUS (peso 80)
```

---

### 3.3 Fraude de Identidade Sintética
**Descrição**: Criação de identidades falsas para obter cartões.
**Padrão Detectável**:
- Cartões novos com uso intenso imediato
- Transações em MCCs de alto risco logo após emissão
- Ausência de histórico de transações

**Regra RULEX**:
```
SE cardExpireDate - transactionDate > 1080 (cartão novo, 3 anos de validade)
E transactionAmount > 2000
E mcc IN (alto_risco)
ENTÃO classificação = SUSPICIOUS (peso 70)
```

---

## 4. MCCs DE ALTO RISCO NO BRASIL

| MCC | Descrição | Risco |
|-----|-----------|-------|
| 7995 | Apostas/Gambling | Altíssimo |
| 7994 | Jogos de vídeo | Altíssimo |
| 5967 | Direct Marketing | Altíssimo |
| 6051 | Cryptocurrency | Altíssimo |
| 4829 | Wire Transfer | Alto |
| 5912 | Farmácias | Alto |
| 5122 | Drogas/Medicamentos | Alto |
| 5968 | Subscription Services | Alto |
| 5969 | Direct Marketing - Other | Alto |

---

## 5. HORÁRIOS DE ALTO RISCO NO BRASIL

| Horário | Risco | Justificativa |
|---------|-------|---------------|
| 00:00 - 06:00 | Altíssimo | Madrugada, baixa atividade legítima |
| 06:00 - 08:00 | Alto | Início do dia, pouca atividade |
| 22:00 - 00:00 | Alto | Noite, aumento de fraudes online |
| 12:00 - 14:00 | Baixo | Horário comercial normal |
| 18:00 - 20:00 | Baixo | Horário de pico de compras |

---

## 6. PAÍSES DE ALTO RISCO PARA TRANSAÇÕES BRASILEIRAS

| País | Código | Risco | Justificativa |
|------|--------|-------|---------------|
| Rússia | RU | Altíssimo | Origem de ataques cibernéticos |
| China | CN | Alto | Volume de fraudes |
| Nigéria | NG | Altíssimo | Golpes financeiros |
| Paquistão | PK | Alto | Fraudes online |
| Vietnã | VN | Alto | Card testing |
| Indonésia | ID | Alto | Fraudes em massa |
| Ucrânia | UA | Alto | Ataques cibernéticos |
| Romênia | RO | Alto | Skimming |
| Bulgária | BG | Alto | Fraudes organizadas |

---

## 7. REGRAS ESPECÍFICAS PARA O BRASIL

### Regra BR-001: Transação Madrugada Alto Valor
```
SE HOUR(transactionTime) BETWEEN 0 AND 5
E transactionAmount > 1000
E customerPresent = "N"
ENTÃO classificação = SUSPICIOUS (peso 75)
```

### Regra BR-002: MCC Gambling Brasil
```
SE mcc = 7995
E merchantCountryCode = "076" (Brasil)
E transactionAmount > 500
ENTÃO classificação = SUSPICIOUS (peso 70)
```

### Regra BR-003: Wire Transfer Alto Valor
```
SE mcc = 4829
E transactionAmount > 5000
ENTÃO classificação = SUSPICIOUS (peso 65)
```

### Regra BR-004: Crypto Sem Autenticação
```
SE mcc = 6051
E eciIndicator = 7
ENTÃO classificação = FRAUD (peso 85)
```

### Regra BR-005: E-commerce Sem 3DS Alto Valor
```
SE customerPresent = "N"
E eciIndicator = 7
E transactionAmount > 2000
ENTÃO classificação = SUSPICIOUS (peso 70)
```

### Regra BR-006: Transação Internacional País Risco
```
SE merchantCountryCode IN ("RU", "CN", "NG", "PK", "VN", "ID", "UA", "RO", "BG")
E transactionAmount > 500
ENTÃO classificação = SUSPICIOUS (peso 65)
```

### Regra BR-007: Card Testing Pattern
```
SE transactionAmount < 10
E mcc IN (7995, 7994, 5967, 6051, 5968)
ENTÃO classificação = FRAUD (peso 85)
```

### Regra BR-008: Valor Próximo Limite Reporte
```
SE transactionAmount BETWEEN 9500 AND 10500
ENTÃO classificação = SUSPICIOUS (peso 50)
```

### Regra BR-009: CVV Falhou Alto Valor
```
SE cvv2Response != "M"
E transactionAmount > 500
ENTÃO classificação = SUSPICIOUS (peso 70)
```

### Regra BR-010: Terminal Sem Segurança
```
SE posSecurity = 0
E transactionAmount > 1000
ENTÃO classificação = SUSPICIOUS (peso 60)
```

---

## 8. PRÓXIMOS PASSOS

1. ✅ Pesquisar padrões de fraude GLOBAIS
2. ✅ Mapear para os 103 campos do payload
3. ✅ Implementar TODAS as regras no backend Java
4. ✅ Criar testes QA rigorosos
5. ✅ Corrigir erros até tudo funcionar

---

**Autor**: Manus AI
**Data**: 16 de Dezembro de 2025
**Versão**: 1.0


---

# 🌍 PESQUISA DE PADRÕES DE FRAUDE GLOBAIS

## Fonte: Vespia, Mastercard, Visa, Stripe, IEEE (2024-2025)

---

## 9. TIPOS DE REGRAS DE DETECÇÃO DE FRAUDE (GLOBAL)

### 9.1 Regras Estáticas (Static Rules)
**Descrição**: Condições fixas e simples.
**Exemplos**:
- Flag qualquer transação > $5,000
- Flag transações de países blacklist
- Flag transações em horários incomuns

### 9.2 Regras Dinâmicas (Dynamic Rules)
**Descrição**: Ajustam-se com base no contexto do usuário.
**Exemplos**:
- $500 flagged para cliente novo, OK para cliente recorrente
- Permite valores maiores em períodos promocionais
- Ajusta threshold baseado em histórico

### 9.3 Regras Baseadas em Comportamento
**Descrição**: Detectam anomalias comparadas ao comportamento típico.
**Exemplos**:
- Desvio do padrão de gastos
- Mudança de localização geográfica
- Alteração de dispositivo

---

## 10. EXEMPLOS DE REGRAS COMUNS (GLOBAL)

### 10.1 Velocity Rules (Regras de Velocidade)
```
SE COUNT(transações) > 5 EM 1 HORA
ENTÃO classificação = SUSPICIOUS (peso 70)
```

### 10.2 Geographic Rules (Regras Geográficas)
```
SE merchantCountryCode != país_habitual
E transactionAmount > 1000
ENTÃO classificação = SUSPICIOUS (peso 60)
```

### 10.3 Amount Threshold Rules (Regras de Valor)
```
SE transactionAmount > 5000
ENTÃO classificação = SUSPICIOUS (peso 50)
```

### 10.4 Time-Based Rules (Regras Temporais)
```
SE HOUR(transactionTime) BETWEEN 2 AND 5
E transactionAmount > 500
ENTÃO classificação = SUSPICIOUS (peso 65)
```

### 10.5 Device/Location Mismatch Rules
```
SE device_location != billing_address_country
ENTÃO classificação = SUSPICIOUS (peso 55)
```

### 10.6 Card Testing Detection Rules
```
SE transactionAmount < 5
E COUNT(transações_mesmo_cartão) > 3 EM 10 MIN
ENTÃO classificação = FRAUD (peso 90)
```

### 10.7 Account Takeover (ATO) Rules
```
SE failed_logins > 5 EM 1 HORA
E transação_logo_após_login
ENTÃO classificação = SUSPICIOUS (peso 75)
```

### 10.8 Synthetic Identity Rules
```
SE conta_nova (< 30 dias)
E transactionAmount > 2000
E mcc IN (alto_risco)
ENTÃO classificação = SUSPICIOUS (peso 70)
```

---

## 11. THRESHOLDS RECOMENDADOS (GLOBAL)

| Regra | Threshold | Peso | Fonte |
|-------|-----------|------|-------|
| High Amount | > $5,000 | 50 | Vespia |
| Very High Amount | > $10,000 | 70 | Mastercard |
| Card Testing | < $5 + múltiplas | 90 | Visa |
| Velocity 1h | > 5 transações | 70 | Stripe |
| Velocity 24h | > 20 transações | 80 | IEEE |
| Night Transaction | 02:00-05:00 | 65 | FEBRABAN |
| New Account | < 30 dias | 60 | Feedzai |
| High Risk MCC | 7995, 6051, 5967 | 75 | Mastercard |
| Failed Auth | > 3 tentativas | 70 | Visa |
| Country Mismatch | != país habitual | 60 | Stripe |

---

## 12. SISTEMA DE SCORING CUMULATIVO

**Modelo de Score Cumulativo**:
- Cada regra acionada contribui pontos para o score total
- Se múltiplas regras de médio risco são acionadas, o score combinado pode exceder o threshold

**Exemplo**:
```
Regra 1: High Amount (+50 pontos)
Regra 2: Night Transaction (+65 pontos)
Regra 3: New Account (+60 pontos)
-----------------------------------
TOTAL: 175 pontos

SE TOTAL >= 100 ENTÃO SUSPICIOUS
SE TOTAL >= 150 ENTÃO FRAUD
```

---

## 13. DESAFIOS EM SISTEMAS BASEADOS EM REGRAS

1. **Fraudadores evoluem rápido**: Regras estáticas podem ser contornadas
2. **Recursos limitados**: Pequenas empresas não conseguem monitorar constantemente
3. **Regras sobrepostas**: Muitas regras podem causar confusão
4. **Conflitos com GDPR**: Regras complexas podem violar privacidade
5. **Falsos positivos**: Regras muito rígidas bloqueiam transações legítimas

---

## 14. BEST PRACTICES PARA REGRAS DE FRAUDE

1. **Coleta e rotulagem de dados**: Manter histórico de fraudes confirmadas
2. **Identificar tendências**: Analisar padrões de fraude recentes
3. **Transparência**: Regras devem ser explicáveis para auditoria
4. **Teste e otimização**: Validar regras com dados históricos
5. **Gerenciar falsos positivos**: Balancear segurança e experiência do usuário
6. **Colaboração entre departamentos**: Fraude, TI, Compliance, Negócios
7. **Revisão contínua**: Atualizar regras conforme fraudes evoluem
8. **Alinhamento com negócios**: Regras devem refletir objetivos da empresa

---

## 15. REGRAS GLOBAIS MAPEADAS PARA O PAYLOAD RULEX

### Regra GL-001: High Amount Threshold
```
SE transactionAmount > 5000
ENTÃO classificação = SUSPICIOUS (peso 50)
```
**Campo do Payload**: `transactionAmount`

### Regra GL-002: Very High Amount
```
SE transactionAmount > 10000
ENTÃO classificação = SUSPICIOUS (peso 70)
```
**Campo do Payload**: `transactionAmount`

### Regra GL-003: Card Testing Pattern
```
SE transactionAmount < 5
E mcc IN (7995, 6051, 5967, 5968)
ENTÃO classificação = FRAUD (peso 90)
```
**Campos do Payload**: `transactionAmount`, `mcc`

### Regra GL-004: Night Transaction
```
SE HOUR(transactionTime) BETWEEN 2 AND 5
E transactionAmount > 500
ENTÃO classificação = SUSPICIOUS (peso 65)
```
**Campos do Payload**: `transactionTime`, `transactionAmount`

### Regra GL-005: High Risk MCC
```
SE mcc IN (7995, 7994, 6051, 5967, 5968, 5969)
ENTÃO classificação = SUSPICIOUS (peso 60)
```
**Campo do Payload**: `mcc`

### Regra GL-006: Country Mismatch
```
SE merchantCountryCode NOT IN (países_habituais)
E transactionAmount > 1000
ENTÃO classificação = SUSPICIOUS (peso 60)
```
**Campos do Payload**: `merchantCountryCode`, `transactionAmount`

### Regra GL-007: E-commerce No Authentication
```
SE customerPresent = "N"
E eciIndicator = 7
E transactionAmount > 500
ENTÃO classificação = SUSPICIOUS (peso 70)
```
**Campos do Payload**: `customerPresent`, `eciIndicator`, `transactionAmount`

### Regra GL-008: CVV Mismatch
```
SE cvv2Response != "M"
E transactionAmount > 200
ENTÃO classificação = SUSPICIOUS (peso 65)
```
**Campos do Payload**: `cvv2Response`, `transactionAmount`

### Regra GL-009: Low Authentication Score
```
SE consumerAuthenticationScore < 100
E transactionAmount > 1000
ENTÃO classificação = SUSPICIOUS (peso 70)
```
**Campos do Payload**: `consumerAuthenticationScore`, `transactionAmount`

### Regra GL-010: External Score Alert
```
SE externalScore3 < 50
ENTÃO classificação = SUSPICIOUS (peso 75)
```
**Campo do Payload**: `externalScore3`

### Regra GL-011: Cryptogram Invalid
```
SE cryptogramValid = false
E transactionAmount > 500
ENTÃO classificação = FRAUD (peso 85)
```
**Campos do Payload**: `cryptogramValid`, `transactionAmount`

### Regra GL-012: Terminal Security Failure
```
SE posSecurity = 0
E transactionAmount > 1000
ENTÃO classificação = SUSPICIOUS (peso 60)
```
**Campos do Payload**: `posSecurity`, `transactionAmount`

### Regra GL-013: Off-Premises Terminal
```
SE posOffPremises = 1
E transactionAmount > 2000
ENTÃO classificação = SUSPICIOUS (peso 55)
```
**Campos do Payload**: `posOffPremises`, `transactionAmount`

### Regra GL-014: PIN Entry Limit Exceeded
```
SE pinEntryLimitExceeded = true
ENTÃO classificação = FRAUD (peso 90)
```
**Campo do Payload**: `pinEntryLimitExceeded`

### Regra GL-015: CVV Limit Exceeded
```
SE cvv2EntryLimitExceeded = true
ENTÃO classificação = FRAUD (peso 90)
```
**Campo do Payload**: `cvv2EntryLimitExceeded`

---

**Autor**: Manus AI
**Data**: 16 de Dezembro de 2025
**Versão**: 2.0 (Brasil + Global)
