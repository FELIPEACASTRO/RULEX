# 🎯 Como Usar os Operadores - Guia Prático Definitivo

## 📌 Sobre Este Guia

Este documento mostra **exemplos reais** de como os operadores funcionam no motor RULEX, baseados no código do backend. Use como referência quando criar regras.

---

## 🏃‍♂️ VELOCIDADE (Velocity)

### O que faz
Conta, soma ou calcula a média de eventos em uma janela de tempo (últimas 24h, 7 dias, etc).

### Como funciona no backend
O motor consulta o `VelocityService` que:
1. Usa um **hash do campo** (ex: PAN, customerId) como chave
2. Consulta o histórico de eventos na janela de tempo
3. Retorna estatísticas (count, sum, avg, distinct merchants, etc)

### Sintaxe Real

```yaml
# Exemplo 1: Contar transações do cliente nas últimas 24h
operator: VELOCITY_COUNT_GT
fieldName: customerId
window: HOUR_24
threshold: 5

# Como o motor interpreta:
# - Pega o customerId do payload
# - Consulta VelocityService.getStats(customerId, TimeWindow.HOUR_24)
# - Retorna stats.transactionCount > 5
```

```yaml
# Exemplo 2: Soma de valores por cartão na última hora
operator: VELOCITY_SUM_GT
fieldName: pan
window: HOUR_1
threshold: 1000.00

# Como o motor interpreta:
# - Usa hash(PAN) por privacidade
# - Consulta VelocityService.getAggregation(pan, HOUR_1, SUM)
# - Retorna totalAmount > 1000
```

```yaml
# Exemplo 3: Merchants distintos por cliente em 30 dias
operator: COUNT_DISTINCT_MERCHANTS_LAST_N_DAYS
fieldName: customerId
valueS Single: "30"

# Como o motor interpreta:
# - Conta quantos merchants únicos o cliente usou em 30 dias
# - Útil para detectar "teste de cartões" (muitos merchants em pouco tempo)
```

### Quando usar
- ✅ Detectar **alta frequência** (automação/bots)
- ✅ Detectar **fragmentação** (smurfing: dividir valor em várias transações pequenas)
- ✅ Detectar **mudança de padrão** (cliente normalmente faz 2 TXs/dia, hoje fez 20)

### Campos disponíveis no backend
- `customerId` / `pan` / `merchantId`
- Janelas: `5MIN`, `15MIN`, `30MIN`, `HOUR_1`, `HOUR_6`, `HOUR_12`, `HOUR_24`, `DAY_7`, `DAY_30`

---

## 🌍 GEO (Geolocalização)

### O que faz
Compara distâncias ou verifica se um ponto está dentro de um polígono.

### Como funciona no backend
O `GeoService` recebe lat/lon do payload e:
1. **GEO_DISTANCE_LT**: calcula distância entre ponto atual e referência (Haversine)
2. **GEO_IN_POLYGON**: usa algoritmo Ray Casting para verificar se está dentro

### Sintaxe Real

```yaml
# Exemplo 1: Transação a mais de 500km do endereço cadastrado
operator: GEO_DISTANCE_GT
valueMin: "-23.5505"  # lat de referência (endereço cadastrado)
valueMax: "-46.6333"  # lon de referência
valueSingle: "500"    # distância em km

# Como o motor interpreta:
# - Pega lat/lon do payload (transaction.latitude, transaction.longitude)
# - Calcula distância usando Haversine
# - Retorna distance > 500km
```

```yaml
# Exemplo 2: Transação dentro de "área de risco" (polígono pré-cadastrado)
operator: GEO_IN_POLYGON
valueSingle: "ZONA_FRONTEIRIÇA"  # nome do polígono salvo no DB

# Como o motor interpreta:
# - Busca o polígono pelo nome
# - Usa Ray Casting para verificar se (lat, lon) está dentro
# - Retorna true se estiver
```

### Quando usar
- ✅ Detectar **transações impossíveis** (cliente em SP às 10h e em NY às 10h05)
- ✅ Bloquear transações em **zonas de risco**
- ✅ Validar **consistência de localização** (IP de RU + GPS no Brasil = suspeito)

---

## 📱 DEVICE (Dispositivo)

### O que faz
Verifica flags de segurança do dispositivo (jailbreak, emulador, VPN, etc).

### Como funciona no backend
O `DeviceOperatorEvaluator` lê flags do payload:

```yaml
# Exemplo 1: Dispositivo com jailbreak/root
operator: DEVICE_JAILBREAK_ROOTED

# Como o motor interpreta:
# - Verifica payload.isJailbroken OR payload.isRooted OR payload.deviceCompromised
# - Retorna true se qualquer um for true
```

```yaml
# Exemplo 2: Emulador/VM
operator: EMULATOR_DETECTION

# Como o motor interpreta:
# - Verifica payload.isEmulator OR payload.isVirtualMachine
# - Útil para detectar bots (fraudadores testam em emuladores antes de atacar)
```

```yaml
# Exemplo 3: VPN/Proxy/Datacenter
operator: VPN_PROXY_DETECTION

# Como o motor interpreta:
# - Verifica payload.isVpn OR payload.isProxy OR payload.isDatacenter
# - Muitos fraudadores usam VPN para esconder localização real
```

### Quando usar
- ✅ Bloquear dispositivos **comprometidos** (jailbreak aumenta risco)
- ✅ Detectar **automação** (emuladores indicam bot)
- ✅ Detectar **ocultação de localização** (VPN/Tor = comportamento suspeito)

### Payload esperado
O frontend/SDK deve enviar essas flags no payload:
```json
{
  "isJailbroken": false,
  "isRooted": false,
  "isEmulator": false,
  "isVpn": false,
  "isProxy": false,
  "isTor": false
}
```

---

## 🕸️ GRAPH (Neo4j - Análise de Rede)

### O que faz
Identifica conexões ocultas entre entidades (contas/pessoas/dispositivos) usando Neo4j.

### Como funciona no backend
O `GraphOperatorEvaluator` chama algoritmos do Neo4j:

```yaml
# Exemplo 1: Centralidade de grau (quantas conexões uma conta tem)
operator: NEO4J_DEGREE_CENTRALITY
valueSingle: "10"  # threshold

# Como o motor interpreta:
# - Chama neo4jService.getDegreeCentrality(accountId)
# - Retorna degree > 10
# - Útil para detectar "hub accounts" (muitas conexões = rede de fraude)
```

```yaml
# Exemplo 2: Detecção de anel de fraude
operator: NEO4J_FRAUD_RING_DETECTION

# Como o motor interpreta:
# - Usa algoritmo Louvain para detectar comunidades
# - Verifica se a conta está em uma comunidade com histórico de fraude
# - Retorna true se pertence a um "fraud ring"
```

```yaml
# Exemplo 3: Transações circulares (lavagem de dinheiro)
operator: NEO4J_CIRCULAR_TRANSACTION_DETECTION

# Como o motor interpreta:
# - Busca ciclos no grafo (A → B → C → A)
# - Útil para detectar "round tripping" e outras técnicas de AML
```

### Quando usar
- ✅ Detectar **redes de fraude** (múltiplas contas controladas por uma pessoa)
- ✅ Detectar **mules** (contas intermediárias para lavagem)
- ✅ Detectar **conluio** (múltiplos atacantes trabalhando juntos)

### Pré-requisito
Neo4j deve estar populado com relações como:
- `SAME_DEVICE` (contas que usam o mesmo dispositivo)
- `SAME_ADDRESS` (contas com o mesmo endereço)
- `SAME_BENEFICIARY` (transferem para os mesmos destinos)

---

## ⚖️ COMPARAÇÃO (Básicos)

### Sintaxe Real

```yaml
# Exemplo 1: Valor maior que limite
operator: GT
fieldName: transactionAmount
valueSingle: "1000"

# Como o motor interpreta:
# - Pega payload.transactionAmount
# - Retorna transactionAmount > 1000
```

```yaml
# Exemplo 2: IN (valor em lista)
operator: IN
fieldName: mcc
valueArray: ["7995", "7994", "5967"]  # MCCs de risco

# Como o motor interpreta:
# - Verifica se payload.mcc está na lista [7995, 7994, 5967]
# - Suporta tanto números quanto strings
```

```yaml
# Exemplo 3: BETWEEN (faixa)
operator: BETWEEN
fieldName: transactionAmount
valueMin: "100"
valueMax: "500"

# Como o motor interpreta:
# - Retorna 100 <= transactionAmount <= 500
```

---

## 🔤 STRING (Texto)

```yaml
# Exemplo 1: Contém palavra suspeita
operator: CONTAINS
fieldName: merchantName
valueSingle: "CRYPTO"

# Como o motor interpreta:
# - Retorna merchantName.toLowerCase().contains("crypto")
```

```yaml
# Exemplo 2: Regex (padrão complexo)
operator: REGEX
fieldName: email
valueSingle: "^[a-z0-9]+@(temp|guerrilla|10minute)\\."

# Como o motor interpreta:
# - Verifica se email corresponde ao padrão (detectar emails temporários)
```

---

## ❓ NULL / BOOLEAN

```yaml
# Exemplo 1: Campo vazio (não fornecido)
operator: IS_NULL
fieldName: cardExpireDate

# Como o motor interpreta:
# - Retorna payload.cardExpireDate == null
```

```yaml
# Exemplo 2: Flag verdadeira
operator: IS_TRUE
fieldName: cardPresent

# Como o motor interpreta:
# - Retorna payload.cardPresent == true
```

---

## 🧪 Como Testar Suas Regras

### Passo 1: Use o Simulador
No frontend, vá em **Regras → Simular** e cole um payload de teste:

```json
{
  "customerId": "C123",
  "transactionAmount": 1500,
  "mcc": 7995,
  "latitude": -23.5505,
  "longitude": -46.6333,
  "isVpn": true
}
```

### Passo 2: Teste Edge Cases
- **Valor exatamente no limite** (se threshold = 1000, teste com 1000)
- **Campos vazios** (null, undefined, "")
- **Tipos errados** (string onde espera número)

### Passo 3: Verifique o Log
O backend loga cada avaliação:
```
VelocityOperatorEvaluator: op=VELOCITY_COUNT_GT, count=7, threshold=5
→ Resultado: true (dispara a regra)
```

---

## 🎓 Exemplos de Regras Completas

### Regra 1: Teste de Cartões
```yaml
ruleName: CARD_TESTING_DETECTION
conditions:
  - operator: VELOCITY_COUNT_GT
    fieldName: pan
    window: HOUR_1
    threshold: 10  # Mais de 10 TXs em 1h com o mesmo cartão
  AND
  - operator: LT
    fieldName: transactionAmount
    valueSingle: "10"  # Todas abaixo de R$ 10
classification: FRAUD
action: BLOCK
```

### Regra 2: Mudança de País Suspeita
```yaml
ruleName: COUNTRY_SWITCH_FRAUD
conditions:
  - operator: VELOCITY_DISTINCT_COUNTRIES_GT
    fieldName: customerId
    window: HOUR_6
    threshold: 2  # Mais de 2 países em 6h
classification: SUSPICIOUS
action: CHALLENGE  # Pedir 2FA
```

### Regra 3: Rede de Fraude
```yaml
ruleName: FRAUD_RING_MEMBER
conditions:
  - operator: NEO4J_DEGREE_CENTRALITY
    threshold: 15  # Conta com mais de 15 conexões
  AND
  - operator: NEO4J_FRAUD_RING_DETECTION
classification: FRAUD
action: BLOCK
```

---

## 🚨 Dicas Importantes

### ⚠️ Armadilhas Comuns

1. **Não confunda operador com campo**
   - ❌ Errado: `operator: customerId`
   - ✅ Certo: `operator: VELOCITY_COUNT_GT, fieldName: customerId`

2. **Respeite os tipos**
   - `valueSingle` espera **string** (mesmo para números: `"1000"`)
   - `valueArray` espera **lista de strings**: `["7995", "7994"]`

3. **Operadores de velocidade precisam de `window`**
   - ❌ Errado: `VELOCITY_COUNT_GT` sem especificar janela
   - ✅ Certo: adicionar `window: HOUR_24` no DTO

### 💡 Boas Práticas

1. **Comece simples, aumente complexidade depois**
   - Primeira versão: 1 condição (ex: `amount > 1000`)
   - Segunda versão: 2 condições com AND
   - Terceira versão: operadores avançados (velocity, geo, graph)

2. **Teste com dados reais do dia a dia**
   - Não teste só com "transações suspeitas"
   - Teste com **transações normais** para ver se não bloqueia clientes legítimos

3. **Use weights para balancear risco**
   - Regra simples (amount > 1000): weight = 30
   - Regra complexa (fraud ring detection): weight = 80
   - Soma = riskScore final

---

## 📚 Próximos Passos

1. **Leia o código**: `backend/src/main/java/com/rulex/service/complex/evaluator/*Evaluator.java`
2. **Veja os testes**: `backend/src/test/java/com/rulex/service/RuleEngineServiceTest.java`
3. **Experimente no Manual**: http://localhost:5173/manual → aba Operadores

---

**Última atualização**: 2025-01-26
