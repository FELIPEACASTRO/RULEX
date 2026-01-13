# Tipologias de Fraude - RULEX

> **Sistema 100% Regras Duras - SEM Machine Learning**
>
> Este documento descreve as tipologias de fraude implementadas no RULEX usando exclusivamente regras determinísticas e estatísticas.

## Índice

1. [Visão Geral](#visão-geral)
2. [Tipologias por Categoria](#tipologias-por-categoria)
3. [Regras por Tipologia](#regras-por-tipologia)
4. [Configuração de Thresholds](#configuração-de-thresholds)
5. [Exemplos de Implementação](#exemplos-de-implementação)

---

## Visão Geral

O RULEX implementa detecção de fraude através de **regras duras** (hard rules) que avaliam transações em tempo real. As tipologias são categorizadas em:

| Categoria | Descrição | Complexidade |
|-----------|-----------|--------------|
| **VELOCITY** | Padrões de frequência e volume | Média |
| **SECURITY** | Validações de segurança e autenticação | Alta |
| **ANOMALY** | Desvios de padrões comportamentais | Alta |
| **BLOCKLIST** | Listas negras de entidades | Baixa |
| **GEO** | Análise geográfica e localização | Média |
| **DEVICE** | Fingerprinting e análise de dispositivo | Alta |

---

## Tipologias por Categoria

### 1. VELOCITY (Velocidade/Frequência)

#### 1.1 Transações Excessivas por Período
**Descrição**: Detecta volume anormal de transações em janelas de tempo específicas.

| Tipologia | Janela | Threshold | Ação |
|-----------|--------|-----------|------|
| `VEL_TX_1H` | 1 hora | > 10 transações | REVIEW |
| `VEL_TX_24H` | 24 horas | > 50 transações | REVIEW |
| `VEL_TX_1H_CRITICAL` | 1 hora | > 20 transações | BLOCK |

**Campos avaliados**:
- `customer_id`
- `card_number_hash`
- `transaction_timestamp`

#### 1.2 Valor Acumulado por Período
**Descrição**: Monitora soma de valores em períodos específicos.

| Tipologia | Janela | Threshold | Ação |
|-----------|--------|-----------|------|
| `VEL_AMOUNT_1H` | 1 hora | > R$ 10.000 | REVIEW |
| `VEL_AMOUNT_24H` | 24 horas | > R$ 50.000 | REVIEW |
| `VEL_AMOUNT_7D` | 7 dias | > R$ 200.000 | BLOCK |

#### 1.3 Merchants Distintos
**Descrição**: Número excessivo de estabelecimentos diferentes.

| Tipologia | Janela | Threshold | Ação |
|-----------|--------|-----------|------|
| `VEL_MERCHANTS_1H` | 1 hora | > 5 merchants | REVIEW |
| `VEL_MERCHANTS_24H` | 24 horas | > 15 merchants | REVIEW |

---

### 2. SECURITY (Segurança)

#### 2.1 Tentativas de Autenticação
**Descrição**: Monitora falhas consecutivas de autenticação.

| Tipologia | Condição | Threshold | Ação |
|-----------|----------|-----------|------|
| `SEC_AUTH_FAIL_3X` | Falhas consecutivas | 3 tentativas | REVIEW |
| `SEC_AUTH_FAIL_5X` | Falhas consecutivas | 5 tentativas | BLOCK |
| `SEC_AUTH_BRUTE_FORCE` | Falhas em 5 min | > 10 tentativas | BLOCK + ALERT |

#### 2.2 CVV/PIN Incorreto
**Descrição**: Validação de códigos de segurança.

| Tipologia | Condição | Threshold | Ação |
|-----------|----------|-----------|------|
| `SEC_CVV_INVALID` | CVV incorreto | 3x consecutivas | BLOCK |
| `SEC_PIN_INVALID` | PIN incorreto | 3x consecutivas | BLOCK_CARD |

#### 2.3 Cartão Comprometido
**Descrição**: Verificação em bases de cartões comprometidos.

| Tipologia | Fonte | Ação |
|-----------|-------|------|
| `SEC_CARD_COMPROMISED` | Lista interna | BLOCK |
| `SEC_BIN_BLACKLIST` | BINs suspeitos | REVIEW |

---

### 3. ANOMALY (Anomalias Comportamentais)

#### 3.1 Desvio de Valor Médio
**Descrição**: Transação significativamente acima da média histórica.

| Tipologia | Condição | Threshold | Ação |
|-----------|----------|-----------|------|
| `ANO_HIGH_VALUE_3X` | Valor > 3x média | Desvio 300% | REVIEW |
| `ANO_HIGH_VALUE_5X` | Valor > 5x média | Desvio 500% | BLOCK |
| `ANO_FIRST_HIGH_VALUE` | 1ª tx > R$ 5.000 | Novo cliente | REVIEW |

**Cálculo do desvio**:
```
desvio = (valor_atual - média_histórica) / média_histórica * 100
```

#### 3.2 Horário Atípico
**Descrição**: Transações em horários incomuns para o perfil.

| Tipologia | Condição | Ação |
|-----------|----------|------|
| `ANO_UNUSUAL_HOUR` | Fora do padrão 95% | REVIEW |
| `ANO_LATE_NIGHT` | Entre 02:00-05:00 | REVIEW |
| `ANO_LATE_NIGHT_HIGH` | 02:00-05:00 + > R$ 1.000 | BLOCK |

#### 3.3 Categoria de Merchant Atípica
**Descrição**: Compra em MCC nunca utilizado pelo cliente.

| Tipologia | Condição | Ação |
|-----------|----------|------|
| `ANO_NEW_MCC` | 1º uso do MCC | REVIEW |
| `ANO_HIGH_RISK_MCC` | MCCs 7995, 5967, 5966 | REVIEW |

---

### 4. BLOCKLIST (Listas Negras)

#### 4.1 Entidades Bloqueadas
**Descrição**: Verificação contra listas de bloqueio.

| Tipologia | Entidade | Ação |
|-----------|----------|------|
| `BLK_CPF` | CPF bloqueado | BLOCK |
| `BLK_CNPJ` | CNPJ bloqueado | BLOCK |
| `BLK_EMAIL` | E-mail bloqueado | BLOCK |
| `BLK_PHONE` | Telefone bloqueado | BLOCK |
| `BLK_DEVICE_ID` | Device fingerprint | BLOCK |
| `BLK_IP` | Endereço IP | BLOCK |

#### 4.2 Listas de Observação
**Descrição**: Entidades em análise.

| Tipologia | Entidade | Ação |
|-----------|----------|------|
| `WATCH_CPF` | CPF em observação | REVIEW |
| `WATCH_MERCHANT` | Merchant suspeito | REVIEW |

---

### 5. GEO (Geográfica)

#### 5.1 Impossibilidade Geográfica
**Descrição**: Transações em locais geograficamente impossíveis.

| Tipologia | Condição | Threshold | Ação |
|-----------|----------|-----------|------|
| `GEO_IMPOSSIBLE_TRAVEL` | Distância/tempo impossível | > 500km/h | BLOCK |
| `GEO_COUNTRY_CHANGE` | País diferente em 24h | 2+ países | REVIEW |

**Cálculo de velocidade**:
```
velocidade = distância_km / tempo_horas
se velocidade > 500 então IMPOSSÍVEL
```

#### 5.2 País de Alto Risco
**Descrição**: Transações originadas de países de risco.

| Tipologia | Condição | Ação |
|-----------|----------|------|
| `GEO_HIGH_RISK_COUNTRY` | País na lista de risco | REVIEW |
| `GEO_SANCTIONED_COUNTRY` | País sancionado | BLOCK |

#### 5.3 Geolocalização vs IP
**Descrição**: Discrepância entre localização declarada e IP.

| Tipologia | Condição | Threshold | Ação |
|-----------|----------|-----------|------|
| `GEO_IP_MISMATCH` | País IP ≠ País declarado | - | REVIEW |
| `GEO_IP_PROXY` | IP de proxy/VPN detectado | - | REVIEW |

---

### 6. DEVICE (Dispositivo)

#### 6.1 Dispositivo Novo
**Descrição**: Transação de dispositivo não reconhecido.

| Tipologia | Condição | Threshold | Ação |
|-----------|----------|-----------|------|
| `DEV_NEW_DEVICE` | 1º uso do device | - | REVIEW |
| `DEV_NEW_DEVICE_HIGH` | 1º device + > R$ 1.000 | - | BLOCK |

#### 6.2 Múltiplos Dispositivos
**Descrição**: Uso excessivo de dispositivos diferentes.

| Tipologia | Janela | Threshold | Ação |
|-----------|--------|-----------|------|
| `DEV_MULTI_24H` | 24 horas | > 3 devices | REVIEW |
| `DEV_MULTI_7D` | 7 dias | > 5 devices | REVIEW |

#### 6.3 Dispositivo Compartilhado
**Descrição**: Device usado por múltiplos clientes.

| Tipologia | Janela | Threshold | Ação |
|-----------|--------|-----------|------|
| `DEV_SHARED_24H` | 24 horas | > 2 clientes | REVIEW |
| `DEV_SHARED_7D` | 7 dias | > 5 clientes | BLOCK |

---

## Configuração de Thresholds

### Arquivo de Configuração
Os thresholds são configuráveis via `application.yml`:

```yaml
rulex:
  typologies:
    velocity:
      tx-per-hour: 10
      tx-per-day: 50
      amount-per-hour: 10000
      amount-per-day: 50000
      merchants-per-hour: 5
    security:
      auth-fail-review: 3
      auth-fail-block: 5
      brute-force-window-minutes: 5
      brute-force-threshold: 10
    anomaly:
      high-value-multiplier-review: 3.0
      high-value-multiplier-block: 5.0
      first-tx-high-value: 5000
      late-night-start: "02:00"
      late-night-end: "05:00"
    geo:
      impossible-travel-speed-kmh: 500
      high-risk-countries:
        - NG
        - RU
        - CN
      sanctioned-countries:
        - KP
        - IR
    device:
      new-device-high-value: 1000
      multi-device-24h: 3
      multi-device-7d: 5
      shared-device-24h: 2
      shared-device-7d: 5
```

### Ajuste Dinâmico
Os thresholds podem ser ajustados via API:

```http
PUT /api/v1/config/typologies/velocity
Content-Type: application/json

{
  "txPerHour": 15,
  "txPerDay": 75,
  "amountPerHour": 15000
}
```

---

## Exemplos de Implementação

### Exemplo 1: Regra de Velocidade

```json
{
  "name": "VEL_TX_1H_CRITICAL",
  "description": "Bloqueia mais de 20 transações em 1 hora",
  "type": "VELOCITY",
  "status": "ACTIVE",
  "conditions": [
    {
      "field": "velocity.tx_count_1h",
      "operator": "GREATER_THAN",
      "value": "20"
    }
  ],
  "action": "BLOCK",
  "weight": 100,
  "classification": "FRAUDE_VELOCITY"
}
```

### Exemplo 2: Regra de Anomalia Composta

```json
{
  "name": "ANO_LATE_NIGHT_HIGH_VALUE",
  "description": "Transação alta em horário atípico",
  "type": "ANOMALY",
  "status": "ACTIVE",
  "conditionLogic": "AND",
  "conditions": [
    {
      "field": "transaction.hour",
      "operator": "BETWEEN",
      "value": "[2, 5]"
    },
    {
      "field": "transaction.amount",
      "operator": "GREATER_THAN",
      "value": "1000"
    }
  ],
  "action": "BLOCK",
  "weight": 85,
  "classification": "FRAUDE_HORARIO_ATIPICO"
}
```

### Exemplo 3: Regra de Impossibilidade Geográfica

```json
{
  "name": "GEO_IMPOSSIBLE_TRAVEL",
  "description": "Velocidade de deslocamento impossível",
  "type": "GEO",
  "status": "ACTIVE",
  "conditions": [
    {
      "field": "geo.travel_speed_kmh",
      "operator": "GREATER_THAN",
      "value": "500"
    }
  ],
  "action": "BLOCK",
  "weight": 95,
  "classification": "FRAUDE_GEO_IMPOSSIVEL"
}
```

### Exemplo 4: Regra de Blocklist

```json
{
  "name": "BLK_CPF_FRAUD",
  "description": "CPF na lista de bloqueio por fraude",
  "type": "BLOCKLIST",
  "status": "ACTIVE",
  "conditions": [
    {
      "field": "customer.cpf",
      "operator": "IN_LIST",
      "value": "blocklist_cpf_fraud"
    }
  ],
  "action": "BLOCK",
  "weight": 100,
  "classification": "FRAUDE_BLOCKLIST"
}
```

---

## Matriz de Decisão

| Score Total | Ação | Descrição |
|-------------|------|-----------|
| 0-30 | APPROVE | Transação aprovada |
| 31-60 | REVIEW | Enviar para análise manual |
| 61-80 | CHALLENGE | Solicitar autenticação adicional |
| 81-100 | BLOCK | Bloquear transação |

### Cálculo do Score
```
score_total = Σ (peso_regra × match_regra)
```

Onde `match_regra` é 1 se a regra foi acionada, 0 caso contrário.

---

## Métricas de Eficácia

### KPIs Recomendados

| Métrica | Fórmula | Meta |
|---------|---------|------|
| Taxa de Detecção | Fraudes detectadas / Total fraudes | > 95% |
| Taxa de Falso Positivo | Bloqueios incorretos / Total bloqueios | < 5% |
| Tempo de Avaliação | P95 latência de avaliação | < 100ms |
| Coverage | Transações avaliadas / Total transações | 100% |

### Dashboard de Monitoramento
Consulte os [Dashboards Grafana](../grafana/README.md) para visualização em tempo real.

---

## Referências

- [FRAUDE_REGRAS_DURAS_EXPORT.yaml](../../../../FRAUDE_REGRAS_DURAS_EXPORT.yaml) - Regras exportadas
- [OpenAPI Specification](../../../../openapi/rulex.yaml) - Documentação da API
- [Grafana Dashboards](../grafana/README.md) - Dashboards de monitoramento

---

*Última atualização: 2025-01-15*
*Versão: 1.0.0*
