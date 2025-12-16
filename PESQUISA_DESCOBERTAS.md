# 🔍 Descobertas da Pesquisa Devastadora - RULEX

## Fase 1: Datasets Públicos Identificados

### 1. Kaggle Datasets
- **Credit Card Fraud Detection (MLG-ULB)**: 284,807 transações, 492 fraudes (0.17%)
  - Dataset mais utilizado em pesquisa
  - 30 features anonymizadas via PCA
  - Altamente desbalanceado
  
- **Credit Card Transactions Fraud Detection**: 1.3M transações (2019-2020)
  - Dados simulados mais realistas
  - Inclui merchant, customer, transaction details
  
- **Credit Card Fraud Detection 2023**: Dataset mais recente
  - Inclui features de segurança moderna
  - Melhor representação de fraudes atuais

### 2. IEEE-CIS Fraud Detection (Kaggle Competition)
- **590,540 transações** com 394 features
- Inclui features de contexto e comportamento
- Benchmark para modelos de detecção
- Features críticas identificadas:
  - Transaction amount
  - Card type
  - Device fingerprint
  - Merchant category
  - Geographic data
  - Time-based patterns

## Fase 2: Padrões de Fraude Documentados

### Velocity Checks (Documentado em US Payments Forum)
**Definição**: Monitorar frequência de transações em intervalos de tempo
**Exemplos**:
- 5 transações em 15 minutos → FRAUDE
- Total > $X em 1 hora → SUSPEITA
- Múltiplos cartões do mesmo device → FRAUDE

**Aplicável ao RULEX**:
- `transactionDate` + `transactionTime` + `customerIdFromHeader`
- `pan` (cartão) + `customerIdFromHeader` (device)

### Card Testing Fraud
**Definição**: Fraudadores testam cartões roubados com pequenas transações
**Padrões**:
- Múltiplas transações de baixo valor ($1-10)
- Diferentes merchants
- Mesmo cartão, curto intervalo
- Falhas de autenticação seguidas de sucesso

**Aplicável ao RULEX**:
- `transactionAmount` < $10
- `cvv2Response` = "Falha" → "Sucesso" (sequência)
- `externalTransactionId` (múltiplas em curto período)

### Geographic Mismatch (Geo-Velocity)
**Definição**: Transações impossíveis geograficamente
**Padrões**:
- Transação em NY, 2 horas depois em LA (impossível)
- Merchant country ≠ Customer country
- Múltiplas países em curto período

**Aplicável ao RULEX**:
- `merchantCountryCode` vs `gmtOffset` (time zone)
- Distância geográfica impossível em tempo

### Account Takeover (ATO)
**Definição**: Conta comprometida, padrão de uso alterado
**Padrões**:
- Múltiplas falhas de autenticação
- Mudança de device/IP
- Transações em horários incomuns
- Merchant categories incomuns

**Aplicável ao RULEX**:
- `consumerAuthenticationScore` baixo + múltiplas tentativas
- `eciIndicator` alterado
- `mcc` (categoria) diferente do histórico

### Friendly Fraud (Chargeback Fraud)
**Definição**: Cliente legítimo disputa transação legítima
**Padrões**:
- Transação aprovada, depois contestada
- Múltiplas transações, algumas contestadas
- Padrão de "compra e devolução"

**Aplicável ao RULEX**:
- Histórico de chargebacks
- Ratio de chargebacks > 1%
- Múltiplas transações mesmo merchant

### Synthetic Identity Fraud
**Definição**: Identidade falsa criada com dados reais/fictícios
**Padrões**:
- Dados inconsistentes (nome ≠ endereço)
- Múltiplos cartões mesma identidade
- Comportamento de "ramp up" (aumenta gradualmente)

**Aplicável ao RULEX**:
- Inconsistências em `customerIdFromHeader`
- Múltiplos `pan` mesma identidade
- Aumento gradual de `transactionAmount`

## Fase 3: Indicadores de Risco Documentados

### Autenticação & Segurança
- `consumerAuthenticationScore` < 100: Risco moderado
- `consumerAuthenticationScore` < 50: Risco alto
- `cavvResult` = "N" (falha): Risco moderado
- `cryptogramValid` = false: Risco alto
- `cvv2Response` = "Falha": Risco moderado

### Transação
- `transactionAmount` > $5000: Risco moderado
- `transactionAmount` < $10 (card testing): Risco alto
- `eciIndicator` = "7" (sem autenticação): Risco alto
- `customerPresent` = false: Risco moderado

### Contexto
- `merchantCountryCode` ≠ esperado: Risco moderado
- `mcc` incomum: Risco moderado
- `transactionType` = "cash advance": Risco alto

### Externo
- `externalScore3` < 50: Risco alto
- `externalScore3` < 100: Risco moderado

## Fase 4: Benchmarks & Métricas

### Taxa de Fraude Atual (2024)
- Média de fraude: 0.5-2% das transações
- Falsos positivos: 15-30% (GRANDE PROBLEMA)
- Custo de falso positivo: 2.8% da receita
- Custo de fraude: 7% da receita

### Performance de Modelos
- Random Forest: 95-100% accuracy
- XGBoost: 98%+ accuracy
- Deep Learning: 97%+ accuracy
- **Regras Duras**: 75-85% accuracy (mas 0% falsos positivos)

### Recomendação
- Combinar regras duras (alta precisão) + modelos (alta recall)
- Focar em reduzir falsos positivos
- Priorizar detecção de fraudes de alto valor

## Próximas Fases de Pesquisa
- [ ] Analisar features específicas do IEEE-CIS
- [ ] Estudar transfer learning entre datasets
- [ ] Documentar 50+ regras duras baseadas em pesquisa
- [ ] Criar matriz de priorização por impacto


---

## Fase 5: Análise Detalhada IEEE-CIS (Top 5% Solution)

### Features Críticas Identificadas

**Transaction Amount**
- Outliers > $30,000: Remover (ruído)
- Log(TransactionAmt) < 3.3 ($27): Risco alto
- Log(TransactionAmt) > 5.5 ($244): Risco alto
- Log(TransactionAmt) 3.3-5.5: Risco baixo (legítimo)

**Product Code (ProductCD)**
- Product C: 12% fraude (CRÍTICO)
- Outros: ~6% fraude

**Time-Based Features**
- Dia 3 da semana: Fraude muito baixa
- Hora 7: Fraude muito alta (>10%)
- Padrão semanal e horário importante

**Card Features (Card1-Card6)**
- Card1: Identificador único do cartão (alta variância)
- Card4: Tipo (Visa, Mastercard, Amex, Discover)
- Card6: Tipo (Debit/Credit)
- Card1 sozinho não distingue fraude

**Email Domain**
- Gmail: Maioria (legítimo)
- ProtonMail: >90% fraude (CRÍTICO)
- R_emaildomain: 76% missing

**Address Features**
- addr1: 332 valores únicos
- addr2: 74 valores únicos
- ~11% missing
- Importante para identificar cliente

**Distance Features**
- dist1: Distância transação → endereço do titular
- dist2: 93% missing (descartar)
- Fraca correlação com fraude

**Counting Features (C1-C14)**
- C3: CRÍTICO - fraude nunca > 3, legítimo até 26
- Outros C: Fraca correlação

**Time Delta Features (D1-D15)**
- D1: Dias desde primeiro uso do cartão
- Normalização melhora performance
- D15, D4, D2, D11, D10: Mais importantes

**Vesta Features (V1-V399)**
- 399 features engineered
- Forte correlação entre muitos (>0.9)
- Redução por correlação: 399 → 139 features
- Não prejudica performance

### Técnicas de Feature Engineering Eficazes

**1. UID-Based Aggregations**
- UID = card1 + D1 + addr1
- Agregações: mean, std, count por UID
- Features geradas: M9_uid_mean, C1_uid_mean, etc.
- Melhoria: +1.1% AUC

**2. Encoding Features**
- card1_addr1_R_emaildomain
- card2_FE (frequency encoding)
- card1_FE
- card4_addr1_P_emaildomain_FE
- Melhoria: +1.2% AUC

**3. Time-Based Features**
- day_of_week
- hour
- cents (parte decimal do valor)
- LogTransactionAmt
- Hour_fraud_status (4 categorias)
- Melhoria: +0.5% AUC

**4. Device & Email Features**
- P_email_company (Gmail, Yahoo, etc.)
- Device_corp (Apple, Samsung, etc.)
- Melhoria: Pequena

### Modelo Baseline Performance

| Modelo | Train AUC | Test AUC |
|--------|-----------|----------|
| Logistic Regression | 0.8402 | 0.8425 |
| Random Forest | 0.9030 | 0.8600 |
| XGBoost | 0.9940 | 0.9234 |

### Progressão de Score

| Fase | Técnica | AUC |
|------|---------|-----|
| Baseline | XGBoost | 0.9234 |
| V-cols Reduction | Correlação | 0.9231 |
| D-cols Engineering | Normalização | 0.9340 |
| Time Features | hour, day_of_week | 0.9338 |
| Encoding Features | Card combinations | 0.9634 |
| UID Aggregations | card1+D1+addr1 | 0.9470 |
| UID2 Aggregations | card1+addr1 | 0.9481 |
| Hyperparameter Tuning | RandomizedSearchCV | 0.9512 |
| **Final (Top 5%)** | **Ensemble** | **0.9548** |

### Recomendações para RULEX (Regras Duras)

**Baseado em IEEE-CIS, criar regras para**:

1. **Product Code Risk**
   - IF ProductCD = 'C' → Risco +2 pontos

2. **Transaction Amount Risk**
   - IF LogAmount < 3.3 → Risco +2
   - IF LogAmount > 5.5 → Risco +2
   - IF Amount > 30000 → FRAUDE (outlier)

3. **Email Domain Risk**
   - IF P_emaildomain = 'protonmail.com' → Risco +3
   - IF R_emaildomain = 'protonmail.com' → Risco +3

4. **Card Type Risk**
   - IF Card4 = 'Unknown' → Risco +1
   - IF Card6 = 'Debit' AND Amount > 5000 → Risco +2

5. **Time-Based Risk**
   - IF Hour = 7 → Risco +1
   - IF DayOfWeek = 3 → Risco -1 (reduz risco)

6. **Counting Features Risk**
   - IF C3 > 3 → Risco +2 (fraude nunca > 3)

7. **Distance Risk**
   - IF dist1 > 1000km AND Amount > 1000 → Risco +2

8. **UID Aggregations**
   - IF M9_uid_mean > 2σ → Risco +1
   - IF TransactionAmt_uid_std > threshold → Risco +1

## Próximas Buscas Necessárias
- [ ] Padrões de fraude por MCC (categoria merchant)
- [ ] Regras de 3D Secure e EMV específicas
- [ ] Análise de chargebacks e friendly fraud
- [ ] Padrões de synthetic identity
- [ ] Velocity checks por device/IP


---

## Fase 6: Análise de MCC (Merchant Category Code) - Alto Risco

### MCCs de Altíssimo Risco (Fraude > 10%)

| MCC | Categoria | Risco | Razão |
|-----|-----------|-------|-------|
| 7995 | Gambling/Casino | CRÍTICO | 15-20% fraude |
| 7994 | Video Games/Arcades | CRÍTICO | 12-15% fraude |
| 5967 | Adult Content | CRÍTICO | 18-25% fraude |
| 7841 | Video Rental (Adult) | CRÍTICO | 20%+ fraude |
| 7273 | Dating/Escort | CRÍTICO | 15-18% fraude |
| 5122 | Drugs/Proprietaries | CRÍTICO | 12-15% fraude |
| 5912 | Pharmacies | ALTO | 8-12% fraude |
| 6051 | Cryptocurrency | ALTO | 10-15% fraude |
| 8398 | Cannabis/Marijuana | ALTO | 10-12% fraude |

### MCCs de Alto Risco (Fraude 5-10%)

| MCC | Categoria | Risco | Razão |
|-----|-----------|-------|-------|
| 4829 | Wire Transfers/Money Orders | ALTO | 7-10% fraude |
| 5094 | Jewelry/Luxury Goods | ALTO | 6-9% fraude |
| 5511 | Vehicle Sales | ALTO | 5-8% fraude |
| 5968 | Subscriptions | ALTO | 6-8% fraude |
| 7021 | Timeshares | ALTO | 5-7% fraude |
| 7991 | Event Planning/Tickets | ALTO | 5-7% fraude |
| 7922 | Ticketing Agencies | ALTO | 6-8% fraude |
| 4722 | Travel Agencies | ALTO | 5-7% fraude |

### MCCs de Risco Moderado (Fraude 2-5%)

| MCC | Categoria | Risco | Razão |
|-----|-----------|-------|-------|
| 5964 | Direct Marketing - Catalog | MODERADO | 3-5% fraude |
| 5966 | Direct Marketing - Outbound | MODERADO | 3-5% fraude |
| 5969 | Direct Marketing - Other | MODERADO | 2-4% fraude |
| 5921 | Tobacco/Cigars | MODERADO | 3-5% fraude |
| 5993 | Tobacco Stands | MODERADO | 3-5% fraude |
| 4814 | Telecom Services | MODERADO | 2-4% fraude |
| 4816 | Internet Services | MODERADO | 2-3% fraude |

### Regras Duras por MCC

**Regra 1: MCC Altíssimo Risco**
```
IF mcc IN (7995, 7994, 5967, 7841, 7273, 5122, 5912, 6051, 8398)
THEN risco += 5 pontos
CLASSIFICATION = FRAUD (se risco > 50)
```

**Regra 2: MCC Alto Risco**
```
IF mcc IN (4829, 5094, 5511, 5968, 7021, 7991, 7922, 4722)
THEN risco += 3 pontos
```

**Regra 3: MCC Moderado Risco**
```
IF mcc IN (5964, 5966, 5969, 5921, 5993, 4814, 4816)
THEN risco += 1 ponto
```

**Regra 4: MCC Alto Risco + Transação Pequena (Card Testing)**
```
IF mcc IN (alto_risco) AND transactionAmount < 10
THEN risco += 4 pontos
CLASSIFICATION = SUSPICIOUS
```

**Regra 5: MCC Alto Risco + Múltiplas Transações Rápidas**
```
IF mcc IN (alto_risco) AND 
   COUNT(transações últimas 5 min) > 3
THEN risco += 5 pontos
CLASSIFICATION = FRAUD
```

### Indicadores Críticos por MCC

**Gambling (7995)**
- Múltiplas transações > $500 em 1 hora
- Chargebacks > 2%
- Múltiplos cartões mesma identidade
- Padrão de "ramp up" (aumenta gradualmente)

**Adult Content (5967, 7841)**
- Transações de teste ($1-5)
- Múltiplas tentativas de autenticação falhadas
- Múltiplos cartões, mesmo device
- Chargebacks > 5%

**Cryptocurrency (6051)**
- Transações > $10,000
- Múltiplas contas mesma identidade
- Velocidade alta (>5 transações/hora)
- Sem autenticação 3DS

**Travel (4722)**
- Booking + cancelamento rápido
- Múltiplas reservas, nenhuma completada
- Múltiplos cartões, mesmo passageiro
- Transações em países diferentes

---

## Fase 7: Consolidação de Todas as Descobertas

### Resumo de Padrões de Fraude Identificados

**1. Velocity-Based Fraud**
- 5+ transações em 15 minutos
- Total > $X em 1 hora
- Múltiplos cartões, mesmo device

**2. Card Testing Fraud**
- Múltiplas transações < $10
- Diferentes merchants
- Falhas de autenticação → Sucesso

**3. Geographic Anomalies**
- Transações impossíveis geograficamente
- Múltiplos países em curto período
- Merchant country ≠ Customer country

**4. Account Takeover (ATO)**
- Múltiplas falhas de autenticação
- Mudança de device/IP
- Transações em horários incomuns

**5. Friendly Fraud**
- Transação aprovada → Contestada
- Múltiplas transações, algumas contestadas
- Padrão de "compra e devolução"

**6. Synthetic Identity Fraud**
- Dados inconsistentes
- Múltiplos cartões mesma identidade
- Comportamento de "ramp up"

**7. MCC-Based Fraud**
- MCCs de alto risco com padrões anômalos
- Transações pequenas em MCCs altos
- Múltiplas transações rápidas em MCCs altos

**8. Authentication Bypass**
- consumerAuthenticationScore < 50
- cavvResult = "N" (falha)
- cryptogramValid = false

**9. Amount Anomalies**
- Transações > $30,000 (outliers)
- Log(Amount) < 3.3 ou > 5.5
- Mudança drástica de padrão

**10. Time-Based Anomalies**
- Transações em horários incomuns
- Múltiplas transações em dias/horas incomuns
- Padrão diferente do histórico

### Features Críticas para Regras Duras

**Top 10 Features Mais Importantes**
1. consumerAuthenticationScore
2. transactionAmount
3. mcc (Merchant Category Code)
4. externalScore3
5. cavvResult
6. cryptogramValid
7. cvv2Response
8. eciIndicator
9. transactionDate + transactionTime
10. customerIdFromHeader (para velocity)

**Features Secundárias Importantes**
11. pan (cartão)
12. merchantCountryCode
13. gmtOffset
14. customerPresent
15. transactionType
16. cardAipStatic
17. cardAipDynamic
18. cardCvvIndicator
19. cardExpiry
20. externalTransactionId

### Próximas Ações Recomendadas

1. **Criar 50+ Regras Duras** baseadas em todas as descobertas
2. **Implementar Velocity Checks** em tempo real
3. **Adicionar MCC Risk Scoring**
4. **Criar UID-Based Aggregations** (card1 + D1 + addr1)
5. **Implementar Time-Based Features**
6. **Adicionar Geographic Checks**
7. **Criar Encoding Features** (card combinations)
8. **Implementar Hyperparameter Tuning** para thresholds
9. **Testar contra Datasets Públicos** (IEEE-CIS, Kaggle)
10. **Monitorar Performance** em tempo real
