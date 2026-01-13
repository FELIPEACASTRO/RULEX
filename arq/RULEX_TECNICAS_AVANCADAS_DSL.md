# TÉCNICAS AVANÇADAS E OPERADORES DSL PARA RULEX
## Sintaxe Completa de Queries e Implementações Práticas

---

## PARTE 1: OPERADORES AVANÇADOS

### 1. Operadores de Janela Temporal Deslizante

```sql
-- Window Functions para análise deslizante
SELECT 
    transaction_id,
    pan_hash,
    amount,
    timestamp,
    COUNT(*) OVER (
        PARTITION BY pan_hash 
        ORDER BY timestamp 
        RANGE BETWEEN INTERVAL 5 MINUTE PRECEDING AND CURRENT ROW
    ) AS count_5m,
    
    SUM(amount) OVER (
        PARTITION BY pan_hash 
        ORDER BY timestamp 
        RANGE BETWEEN INTERVAL 1 HOUR PRECEDING AND CURRENT ROW
    ) AS sum_1h,
    
    AVG(amount) OVER (
        PARTITION BY pan_hash 
        ORDER BY timestamp 
        RANGE BETWEEN INTERVAL 24 HOUR PRECEDING AND CURRENT ROW
    ) AS avg_24h,
    
    STDDEV(amount) OVER (
        PARTITION BY pan_hash 
        ORDER BY timestamp 
        RANGE BETWEEN INTERVAL 7 DAY PRECEDING AND CURRENT ROW
    ) AS stddev_7d
FROM transactions
WHERE timestamp >= NOW() - INTERVAL 30 DAY;
```

### 2. Detecção de Mudança de Padrão (Change Point Detection)

```sql
-- Identificar mudanças abruptas em padrão
WITH daily_stats AS (
    SELECT 
        DATE(timestamp) as date,
        account_id,
        COUNT(*) as daily_count,
        SUM(amount) as daily_sum,
        AVG(amount) as daily_avg
    FROM transactions
    GROUP BY DATE(timestamp), account_id
),
stats_with_baseline AS (
    SELECT 
        date,
        account_id,
        daily_count,
        daily_sum,
        daily_avg,
        LAG(daily_avg) OVER (
            PARTITION BY account_id 
            ORDER BY date
        ) as prev_avg,
        LAG(daily_count) OVER (
            PARTITION BY account_id 
            ORDER BY date
        ) as prev_count,
        AVG(daily_avg) OVER (
            PARTITION BY account_id 
            ORDER BY date 
            ROWS BETWEEN 30 PRECEDING AND 1 PRECEDING
        ) as baseline_avg_30d,
        
        STDDEV(daily_avg) OVER (
            PARTITION BY account_id 
            ORDER BY date 
            ROWS BETWEEN 30 PRECEDING AND 1 PRECEDING
        ) as baseline_stddev_30d
    FROM daily_stats
)
SELECT 
    date,
    account_id,
    daily_count,
    daily_avg,
    baseline_avg_30d,
    baseline_stddev_30d,
    ROUND(ABS(daily_avg - baseline_avg_30d) / NULLIF(baseline_stddev_30d, 0), 2) as z_score,
    CASE 
        WHEN ABS(daily_avg - baseline_avg_30d) > (3 * baseline_stddev_30d) THEN 'ANOMALY_DETECTED'
        WHEN daily_count > (prev_count * 3) THEN 'VELOCITY_SPIKE'
        ELSE 'NORMAL'
    END as status
FROM stats_with_baseline
WHERE date >= NOW() - INTERVAL 30 DAY
ORDER BY z_score DESC;
```

### 3. Clustering de Entidades (Entity Graph Analysis)

```sql
-- Encontrar grupos de contas potencialmente conectadas
WITH entity_connections AS (
    SELECT 
        t1.account_id as account_a,
        t2.account_id as account_b,
        COUNT(*) as common_device_count,
        COUNT(DISTINCT t1.device_id) as t1_devices,
        COUNT(DISTINCT t2.device_id) as t2_devices
    FROM transactions t1
    JOIN transactions t2 
        ON t1.device_id = t2.device_id 
        AND t1.account_id < t2.account_id
        AND ABS(EXTRACT(EPOCH FROM (t1.timestamp - t2.timestamp))) < 300  -- 5 min
    WHERE t1.timestamp >= NOW() - INTERVAL 30 DAY
    GROUP BY t1.account_id, t2.account_id
    HAVING COUNT(*) >= 3
)
SELECT 
    account_a,
    account_b,
    common_device_count,
    CASE 
        WHEN common_device_count >= 10 THEN 'LIKELY_RING'
        WHEN common_device_count >= 5 THEN 'SUSPICIOUS'
        WHEN common_device_count >= 3 THEN 'MONITOR'
        ELSE 'NORMAL'
    END as classification
FROM entity_connections
ORDER BY common_device_count DESC;
```

### 4. Análise de Sequência de Eventos

```sql
-- Detectar padrões sequenciais específicos (ex: reset senha + transfer)
WITH event_sequence AS (
    SELECT 
        account_id,
        event_type,
        timestamp,
        ROW_NUMBER() OVER (
            PARTITION BY account_id 
            ORDER BY timestamp
        ) as event_seq,
        
        LAG(event_type) OVER (
            PARTITION BY account_id 
            ORDER BY timestamp
        ) as prev_event_1,
        
        LAG(event_type, 2) OVER (
            PARTITION BY account_id 
            ORDER BY timestamp
        ) as prev_event_2,
        
        LAG(timestamp) OVER (
            PARTITION BY account_id 
            ORDER BY timestamp
        ) as prev_timestamp
    FROM events
    WHERE timestamp >= NOW() - INTERVAL 24 HOUR
)
SELECT 
    account_id,
    prev_event_2,
    prev_event_1,
    event_type,
    EXTRACT(HOUR FROM (timestamp - prev_timestamp)) as hours_between,
    CASE 
        WHEN prev_event_2 = 'password_reset' 
            AND prev_event_1 = 'email_change' 
            AND event_type = 'wire_transfer' 
            AND EXTRACT(HOUR FROM (timestamp - prev_timestamp)) <= 24
        THEN 'ACCOUNT_TAKEOVER_PATTERN'
        ELSE 'OTHER'
    END as pattern
FROM event_sequence
WHERE prev_event_2 IS NOT NULL
    AND pattern = 'ACCOUNT_TAKEOVER_PATTERN';
```

### 5. Análise de Dispersão/Concentração (Gini Coefficient)

```sql
-- Detectar dispersão (muitos beneficiários) vs concentração (poucos)
WITH beneficiary_distribution AS (
    SELECT 
        account_id,
        beneficiary_id,
        SUM(amount) as total_to_beneficiary,
        COUNT(*) as txn_count,
        SUM(amount) OVER (PARTITION BY account_id) as total_account_amount
    FROM transactions
    WHERE timestamp >= NOW() - INTERVAL 30 DAY
    GROUP BY account_id, beneficiary_id
),
gini_calc AS (
    SELECT 
        account_id,
        COUNT(DISTINCT beneficiary_id) as num_beneficiaries,
        SUM(total_to_beneficiary) as total_amount,
        SUM(total_to_beneficiary * total_to_beneficiary) as sum_squared,
        SUM(total_to_beneficiary) OVER (PARTITION BY account_id) as cumsum_amount
    FROM beneficiary_distribution
    GROUP BY account_id
)
SELECT 
    account_id,
    num_beneficiaries,
    total_amount,
    ROUND(
        2.0 * SUM(ROW_NUMBER() OVER (PARTITION BY account_id ORDER BY total_to_beneficiary) * total_to_beneficiary) 
        / (COUNT(*) * SUM(total_to_beneficiary)), 
        3
    ) as gini_coefficient,
    CASE 
        WHEN ROUND(
            2.0 * SUM(ROW_NUMBER() OVER (PARTITION BY account_id ORDER BY total_to_beneficiary) * total_to_beneficiary) 
            / (COUNT(*) * SUM(total_to_beneficiary)), 
            3
        ) < 0.3 THEN 'CONCENTRATED_DISPERSION_RISK'
        WHEN ROUND(...) > 0.8 THEN 'HIGH_DISPERSION_RISK'
        ELSE 'NORMAL'
    END as risk_level
FROM gini_calc;
```

### 6. Detecção de Layering (Money Laundering)

```sql
-- Identificar movimentação rápida entre contas (layering)
WITH rapid_movements AS (
    SELECT 
        sender_account_id,
        receiver_account_id,
        amount,
        send_timestamp,
        LAG(receive_timestamp) OVER (
            PARTITION BY receiver_account_id 
            ORDER BY receive_timestamp
        ) as prev_receive_time,
        LEAD(send_timestamp) OVER (
            PARTITION BY sender_account_id 
            ORDER BY send_timestamp
        ) as next_send_time
    FROM transfer_ledger
    WHERE send_timestamp >= NOW() - INTERVAL 7 DAY
),
layering_pattern AS (
    SELECT 
        sender_account_id,
        receiver_account_id,
        amount,
        send_timestamp,
        EXTRACT(MINUTE FROM (send_timestamp - prev_receive_time)) as minutes_held,
        CASE 
            WHEN EXTRACT(MINUTE FROM (send_timestamp - prev_receive_time)) <= 30
                AND ABS(amount - LAG(amount) OVER (ORDER BY send_timestamp)) < 100
            THEN 'LAYERING_DETECTED'
            ELSE 'NORMAL'
        END as risk
    FROM rapid_movements
)
SELECT *
FROM layering_pattern
WHERE risk = 'LAYERING_DETECTED'
ORDER BY send_timestamp DESC;
```

---

## PARTE 2: DSL (Domain Specific Language) PARA RULEX

### Sintaxe Completa de Especificação de Regra

```
RULE rule_id {
    METADATA {
        name: "Descrição da Regra"
        version: "1.0"
        author: "fraud-team"
        created_date: "2026-01-12"
        last_modified: "2026-01-12"
        description: "Descrição detalhada da regra"
        tags: ["tag1", "tag2"]
        enabled: true
    }
    
    SEVERITY {
        fraud_risk_score: 85          # 0-100
        priority: 95                  # 0-100
        impact_level: "HIGH"          # LOW, MEDIUM, HIGH, CRITICAL
        false_positive_rate: 0.05     # 5% máximo aceitável
    }
    
    PARAMETERS {
        window_1m_threshold: 20
        window_5m_threshold: 50
        window_1h_threshold: 500
        amount_threshold_low: 15
        amount_threshold_high: 5000
        decline_rate_threshold: 0.75
        customer_age_min_days: 180
    }
    
    WHEN {
        count(auth_attempt, pan_hash, @window_1m_threshold) >= @window_1m_threshold AND
        count(decline, pan_hash, @window_1m_threshold) >= (@window_1m_threshold * 0.75) AND
        avg(amount, pan_hash, @window_1m_threshold) <= @amount_threshold_low
    }
    
    EXCEPT {
        merchant_id IN @merchant_allowlist OR
        account_age_days > @customer_age_min_days OR
        account_status == 'TRUSTED' OR
        kyc_level == 3
    }
    
    THEN {
        final_score += 85
        decision = HOLD_FOR_REVIEW
        reason = "Possible card testing detected"
        
        ACTIONS {
            alert_level = "MEDIUM"
            notify_recipient = "fraud_team"
            block_merchant = false
            require_mfa = true
            add_to_watchlist = false
            increase_monitoring = true
            custom_action = "CALL_CUSTOMER"
        }
    }
    
    MONITORING {
        track_true_positives: true
        track_false_positives: true
        alert_on_exception_rate_exceeds: 0.10
        alert_on_performance_degrades: true
    }
}
```

### 2. Operadores de Composição de Regras

```
# AND Lógico
WHEN {
    condition_1 AND
    condition_2 AND
    (condition_3 OR condition_4)
}

# Priorização de Condições (fail-fast)
WHEN {
    HIGH_SEVERITY_CHECK: condition_1  # Verifica primeiro
    MEDIUM_SEVERITY_CHECK: condition_2
    LOW_SEVERITY_CHECK: condition_3
    AGGREGATE: (condition_2 OR condition_3)
}

# Negação
WHEN {
    condition_1 AND
    NOT condition_2
}

# Condicional aninhado
WHEN {
    IF account_type == 'business' THEN
        condition_business
    ELSE IF account_type == 'personal' THEN
        condition_personal
    END
}
```

### 3. Funções de Agregação Customizadas

```sql
-- Agregação com peso
WEIGHTED_AVG(amount * risk_weight, amount, 24h)

-- Percentil
PERCENTILE(amount, 0.95, 7d)  -- 95º percentil dos últimos 7 dias

-- Moda (valor mais frequente)
MODE(merchant_category, 30d)

-- Entropia (medida de diversidade)
ENTROPY(distinct(merchant_id), 24h)  # Baixo = poucos merchants, Alto = muitos

-- Coefficient of Variation (CV = stddev/mean)
CV(amount, 7d) > 1.5  # Variação alta

-- Skewness (assimetria)
SKEWNESS(amount, 30d)  # >0 = cauda direita (muitos pagamentos pequenos)

-- Autocorrelation (correlação com valores passados)
AUTOCORR(amount, lag=1, window=7d)  # Padrão repetitivo
```

### 4. Operadores de Matching

```sql
-- Exact Match
field EXACT_MATCH "value"

-- Fuzzy Match (Levenshtein)
name FUZZY_MATCH "expected_name" WITH threshold=0.85

-- Regex Match
email REGEX_MATCH "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.com$"

-- List Membership
country IN ("US", "CA", "MX")

-- Range
amount BETWEEN 100 AND 1000

-- NULL/Empty
field IS NULL
field IS NOT EMPTY

-- Type Checking
amount IS NUMERIC
date IS DATE FORMAT "YYYY-MM-DD"
```

### 5. Operadores de Correlação

```sql
-- Correlação de Pearson
PEARSON_CORR(variable_1, variable_2, window=30d)

-- Distância Euclidiana
EUCLIDEAN_DISTANCE([amount, frequency, device_count], [baseline_amount, baseline_freq, baseline_device], account_id)

-- Cosine Similarity (para fingerprints)
COSINE_SIM(device_fingerprint_1, device_fingerprint_2) > 0.95

-- Jaccard Similarity (para conjuntos)
JACCARD_SIM(merchant_set_1, merchant_set_2) > 0.7

-- Edit Distance (para strings)
EDIT_DIST(name_1, name_2) <= 2  # Máximo 2 caracteres diferentes
```

---

## PARTE 3: EXEMPLOS PRÁTICOS DE IMPLEMENTAÇÃO

### Exemplo 1: Detecção de Fraude Sintética (Synthetic Identity)

```yaml
RULE synthetic_identity_detection {
    PARAMETERS {
        kyc_inconsistency_threshold: 3
        bureau_not_found_weight: 40
        thin_file_weight: 35
        address_history_weight: 25
    }
    
    WHEN {
        # Múltiplas inconsistências de KYC
        kyc_field_mismatch_count >= @kyc_inconsistency_threshold AND
        
        # Não encontrado em credit bureaus
        credit_bureau_match_score < 0.5 AND
        
        # Thin file (pouco histórico)
        credit_history_depth < 12 AND
        
        # Endereço com inconsistências
        address_verification_failed == true
    }
    
    THEN {
        synthetic_identity_score = (
            (bureau_not_found_weight * 1.0) +
            (thin_file_weight * 1.0) +
            (address_history_weight * kyc_field_mismatch_count / 3)
        ) / 100
        
        final_score = MIN(synthetic_identity_score * 100, 95)
        decision = HOLD_FOR_REVIEW
        reason = "Possible synthetic identity detected"
    }
}
```

### Exemplo 2: Detecção de BEC (Business Email Compromise)

```yaml
RULE bec_detection {
    WHEN {
        account_type == 'business' AND
        (
            # Novo beneficiário com ticket alto
            (new_beneficiary_in_24h == true AND amount >= p95(historical_transfers)) OR
            
            # Mudança de dados bancários + transferência
            (bank_account_change_in_7d == true AND is_transfer == true) OR
            
            # Email/telefone alterado + ação financeira sensível
            (email_change_in_24h == true OR phone_change_in_24h == true) AND
            (action IN ('wire', 'transfer', 'cash_out')) OR
            
            # Padrão de reconhecimento
            email_sender_from_free_domain == true AND
            matches(email_subject_line, /(urgent|verify|confirm|action required)/i)
        )
    }
    
    THEN {
        score += 90
        decision = BLOCK_WITH_CALLBACK
        reason = "Business Email Compromise pattern detected"
        
        ACTIONS {
            contact_method = "PHONE_CALLBACK"
            provide_caller_id = true
            require_ceo_verbal_verification = true
        }
    }
}
```

### Exemplo 3: Detecção de Estruturação (Smurfing)

```yaml
RULE smurfing_detection {
    PARAMETERS {
        structuring_threshold: 10000
        look_back_days: 7
        max_transactions_below_threshold: 3
        ctc_threshold: 9990  # Just below $10k reporting requirement
    }
    
    WHEN {
        # Múltiplas transações abaixo do limiar
        count(
            transaction WHERE amount >= (@ctc_threshold * 0.99) AND amount < @ctc_threshold,
            account_id,
            @look_back_days
        ) >= @max_transactions_below_threshold AND
        
        # Total acumulado ultrapassa limiar
        sum(
            amount WHERE amount >= (@ctc_threshold * 0.99) AND amount < @ctc_threshold,
            account_id,
            @look_back_days
        ) >= @structuring_threshold AND
        
        # Mesmo beneficiário ou padrão circular
        (same_beneficiary_pattern == true OR circular_pattern == true)
    }
    
    THEN {
        score = 92
        decision = HOLD_FOR_REVIEW
        reason = "Possible structuring/smurfing detected"
        
        ACTIONS {
            file_ctc = true  # File Currency Transaction Report
            file_sar = true  # File Suspicious Activity Report
            monitoring_level = 'ENHANCED'
        }
    }
}
```

### Exemplo 4: Detecção de Account Takeover

```yaml
RULE account_takeover_multi_signal {
    WHEN {
        (
            # Novo device + novo país + nova hora
            is_new_device(account_id) == true AND
            geo_country(ip_current) != customer_country AND
            hour_of_day NOT IN customer_active_hours AND
            DAYS_SINCE_last_login > 30
        ) OR
        (
            # Múltiplas falhas seguidas de sucesso
            count(login_fail, account_id, 5m) >= 50 AND
            login_success_after_fails == true
        ) OR
        (
            # MFA fatigue
            count(mfa_prompt_denied, account_id, 10m) >= 3 AND
            mfa_prompt_accepted_after == true
        ) OR
        (
            # Viagem impossível
            geo_distance_km(prev_transaction, current_transaction) > 900 AND
            time_between_txn_hours < 2
        )
    }
    
    THEN {
        # Scoring múltiplo
        ato_score = 0
        
        IF is_new_device AND geo_country != customer_country THEN ato_score += 35
        IF login_velocity_spike THEN ato_score += 25
        IF mfa_fatigue_detected THEN ato_score += 30
        IF impossible_travel THEN ato_score += 40
        
        final_score = MIN(ato_score, 95)
        
        decision = STEP_UP_AUTHENTICATION
        
        ACTIONS {
            require_mfa_type = 'STRONG'  # Not SMS only
            send_verification_email = true
            require_csa_callback = true
            block_sensitive_actions = true
        }
    }
}
```

---

## PARTE 4: OTIMIZAÇÕES DE PERFORMANCE

### 1. Indexação Estratégica

```sql
-- Índices para contadores temporais
CREATE INDEX idx_transactions_pan_timestamp 
ON transactions(pan_hash, timestamp);

CREATE INDEX idx_transactions_account_timestamp 
ON transactions(account_id, timestamp);

CREATE INDEX idx_transactions_device_timestamp 
ON transactions(device_id, timestamp);

-- Índices para lookups
CREATE INDEX idx_transactions_merchant_id 
ON transactions(merchant_id);

CREATE INDEX idx_transactions_beneficiary_id 
ON transactions(beneficiary_id);

-- Índices de texto para busca fuzzy
CREATE INDEX idx_names_fulltext 
ON customers USING GIN(name_tsv);
```

### 2. Particionamento de Tabelas

```sql
-- Particionamento por data (melhora performance de janelas temporais)
CREATE TABLE transactions_2026_01 
PARTITION OF transactions
FOR VALUES FROM ('2026-01-01') TO ('2026-02-01');

-- Particionamento por hash para paralelização
CREATE TABLE transactions_shard_0
PARTITION OF transactions
FOR VALUES WITH (MODULUS 8, REMAINDER 0);
```

### 3. Materialização de Vistas

```sql
-- Pre-computar contadores
CREATE MATERIALIZED VIEW daily_account_stats AS
SELECT 
    account_id,
    DATE(timestamp) as date,
    COUNT(*) as daily_txn_count,
    SUM(amount) as daily_sum,
    AVG(amount) as daily_avg,
    MAX(amount) as daily_max,
    MIN(amount) as daily_min
FROM transactions
GROUP BY account_id, DATE(timestamp);

-- Refresh automático
CREATE OR REPLACE FUNCTION refresh_daily_stats()
RETURNS void AS $$
BEGIN
    REFRESH MATERIALIZED VIEW CONCURRENTLY daily_account_stats;
END;
$$ LANGUAGE plpgsql;

-- Trigger para refresh automático
CREATE TRIGGER trigger_refresh_daily_stats
AFTER INSERT ON transactions
FOR EACH STATEMENT
EXECUTE FUNCTION refresh_daily_stats();
```

### 4. Cache em Redis

```python
# Pseudo-código para cache distribuído
class RulesEngine:
    def __init__(self, redis_client):
        self.redis = redis_client
        
    def get_counters(self, entity_id, windows=[1,5,15,60,1440]):
        """Buscar contadores com cache"""
        cache_key = f"counters:{entity_id}"
        
        # Verificar cache
        cached = self.redis.get(cache_key)
        if cached:
            return json.loads(cached)
        
        # Buscar do banco
        counters = self.db.fetch_counters(entity_id, windows)
        
        # Cachear com TTL = maior janela
        self.redis.setex(
            cache_key, 
            max(windows) * 60, 
            json.dumps(counters)
        )
        
        return counters
```

---

## CONCLUSÃO: ROADMAP DE IMPLEMENTAÇÃO

**Fase 1 (Semanas 1-4)**
- [ ] Implementar parser DSL RULEX
- [ ] Configurar infraestrutura de BD (particionamento, índices)
- [ ] Integrar 20 regras básicas (card testing, ATO, etc)

**Fase 2 (Semanas 5-8)**
- [ ] Implementar window functions avançadas
- [ ] Adicionar 50 regras intermediárias
- [ ] Calibração de thresholds por segmento

**Fase 3 (Semanas 9-12)**
- [ ] Machine Learning scoring hybrid
- [ ] Monitoramento de performance em tempo real
- [ ] 100+ regras em produção

**KPIs de Sucesso**
- Detecção de fraude: ≥ 85%
- Taxa de falsos positivos: ≤ 5%
- Latência P99: < 100ms
- Disponibilidade: ≥ 99.9%

