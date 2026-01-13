# Grafana Dashboards para RULEX

Este diretório contém os dashboards Grafana pré-configurados para monitoramento do sistema RULEX.

## Dashboards Disponíveis

### 1. RULEX - Overview Dashboard (`rulex-overview-dashboard.json`)
Dashboard principal com visão geral do sistema:
- **HTTP Request Rate**: Taxa de requisições por endpoint
- **HTTP Response Latency (P95/P99)**: Latência de respostas
- **Error Rate (5xx)**: Taxa de erros do servidor
- **Total Active Rules**: Quantidade de regras ativas
- **Transactions Evaluated (24h)**: Volume de transações avaliadas
- **Fraud Detected (24h)**: Fraudes detectadas
- **Rules by Type**: Distribuição de regras por tipo
- **Rule Triggers (per hour)**: Acionamentos de regras
- **JVM Heap Memory Usage**: Uso de memória da JVM
- **Database Connection Pool**: Conexões HikariCP

### 2. RULEX - Rules Performance Dashboard (`rulex-rules-dashboard.json`)
Dashboard focado em performance das regras:
- **Active/Inactive Rules**: Contagem por status
- **Rules by Type**: VELOCITY, SECURITY, ANOMALY, BLOCKLIST
- **Rule Triggers by Rule (per hour)**: Acionamentos detalhados
- **Rule Evaluation Latency (P50/P95)**: Tempo de avaliação por regra
- **Rule Trigger Distribution (24h)**: Distribuição de acionamentos
- **Rule Configuration Changes**: Alterações em regras
- **Top Triggering Rules (24h)**: Tabela com regras mais acionadas

### 3. RULEX - Fraud Detection Dashboard (`rulex-fraud-dashboard.json`)
Dashboard focado em detecção de fraude:
- **Transactions (24h)**: Volume total de transações
- **Fraud Detected (24h)**: Fraudes detectadas
- **Fraud Rate (24h)**: Taxa de fraude
- **Avg Evaluation Time (P95)**: Tempo médio de avaliação
- **Transaction Volume vs Fraud**: Comparativo volume x fraude
- **Fraud Blocked Amount (per hour)**: Valor bloqueado em R$
- **Decision Distribution (24h)**: APPROVED/BLOCKED/REVIEW
- **Fraud by Typology (24h)**: Distribuição por tipologia
- **Fraud by Channel (per hour)**: Fraudes por canal
- **Fraud Typologies Over Time**: Evolução de tipologias

## Como Importar

### Via UI do Grafana
1. Acesse o Grafana (padrão: http://localhost:3000)
2. Vá em **Dashboards** → **Import**
3. Clique em **Upload JSON file**
4. Selecione o arquivo `.json` desejado
5. Configure a fonte de dados Prometheus
6. Clique em **Import**

### Via Provisioning (Recomendado para Produção)

1. Crie o diretório de provisioning:
```bash
mkdir -p /etc/grafana/provisioning/dashboards
```

2. Crie o arquivo de configuração `dashboards.yaml`:
```yaml
apiVersion: 1

providers:
  - name: 'RULEX Dashboards'
    orgId: 1
    folder: 'RULEX'
    folderUid: 'rulex'
    type: file
    disableDeletion: false
    updateIntervalSeconds: 30
    options:
      path: /etc/grafana/provisioning/dashboards/rulex
```

3. Copie os dashboards:
```bash
cp *.json /etc/grafana/provisioning/dashboards/rulex/
```

4. Reinicie o Grafana:
```bash
systemctl restart grafana-server
```

## Configuração do Prometheus

### Data Source
Certifique-se de ter uma fonte de dados Prometheus configurada com o UID `${DS_PROMETHEUS}`.

### Métricas Requeridas
As seguintes métricas devem ser expostas pela aplicação RULEX:

#### Métricas HTTP (Spring Boot Actuator)
- `http_server_requests_seconds_count`
- `http_server_requests_seconds_bucket`

#### Métricas JVM (Micrometer)
- `jvm_memory_used_bytes`

#### Métricas HikariCP
- `hikaricp_connections_active`
- `hikaricp_connections_idle`
- `hikaricp_connections_pending`

#### Métricas Customizadas RULEX
- `rulex_rules_total{status}` - Total de regras por status
- `rulex_rules_by_type_total{type}` - Regras por tipo
- `rulex_transactions_evaluated_total` - Transações avaliadas
- `rulex_fraud_detected_total` - Fraudes detectadas
- `rulex_fraud_blocked_amount_total` - Valor bloqueado
- `rulex_transaction_decisions_total{decision}` - Decisões por tipo
- `rulex_fraud_by_typology_total{typology}` - Fraudes por tipologia
- `rulex_fraud_by_channel_total{channel}` - Fraudes por canal
- `rulex_rule_triggers_total{rule_name}` - Acionamentos por regra
- `rulex_rule_evaluation_duration_seconds_bucket` - Histograma de latência
- `rulex_transaction_evaluation_duration_seconds_bucket` - Histograma de avaliação
- `rulex_rule_updates_total{operation}` - Operações CRUD em regras

## Alertas Recomendados

### Alertas Críticos
```yaml
- alert: HighFraudRate
  expr: increase(rulex_fraud_detected_total[1h]) / increase(rulex_transactions_evaluated_total[1h]) > 0.05
  for: 5m
  labels:
    severity: critical
  annotations:
    summary: "Taxa de fraude acima de 5%"

- alert: RuleEvaluationSlow
  expr: histogram_quantile(0.95, sum(rate(rulex_transaction_evaluation_duration_seconds_bucket[5m])) by (le)) > 0.1
  for: 5m
  labels:
    severity: warning
  annotations:
    summary: "Avaliação de transações lenta (P95 > 100ms)"

- alert: HighErrorRate
  expr: sum(rate(http_server_requests_seconds_count{status=~"5.."}[5m])) / sum(rate(http_server_requests_seconds_count[5m])) > 0.01
  for: 5m
  labels:
    severity: critical
  annotations:
    summary: "Taxa de erros 5xx acima de 1%"
```

## Personalização

### Período de Retenção
Por padrão, os dashboards mostram dados das últimas 1-24 horas. Ajuste o seletor de tempo conforme necessário.

### Intervalos de Refresh
Todos os dashboards atualizam a cada 30 segundos. Ajuste em **Dashboard Settings** → **General** → **Auto refresh**.

### Thresholds
Os thresholds de cores (verde/amarelo/vermelho) podem ser ajustados editando cada painel conforme os SLAs da sua organização.
