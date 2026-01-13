# Plano de Implementação - Campos Derivados Faltantes

**Data:** 2026-01-06  
**Autor:** Setup Agent  
**Status:** ✅ COMPLETO

## 🎉 Implementação Concluída!

Todos os 8 serviços de enriquecimento foram implementados com sucesso:

| Serviço | Arquivo | Campos | Status |
|---------|---------|--------|--------|
| VelocityEnrichment | `VelocityEnrichment.java` | 25 | ✅ |
| DeviceEnrichment | `DeviceEnrichment.java` | 20 | ✅ |
| GeoEnrichment | `GeoEnrichment.java` | 18 | ✅ |
| CustomerEnrichment | `CustomerEnrichment.java` | 15 | ✅ |
| CardEnrichment | `CardEnrichment.java` | 12 | ✅ |
| AuthEnrichment | `AuthEnrichment.java` | 8 | ✅ |
| AnomalyEnrichment | `AnomalyEnrichment.java` | 15 | ✅ |
| TransactionEnrichmentFacade | `TransactionEnrichmentFacade.java` | - | ✅ |

**Total de campos implementados: 113+**

## 📊 Resumo Executivo

Este documento detalha o plano de implementação dos 103 campos derivados identificados como faltantes no relatório de gaps do RULEX.

## 🔍 Análise do Estado Atual

### Services Existentes

| Service | Campos Implementados | Status |
|---------|---------------------|--------|
| **EnrichmentService** | BIN (10 campos), MCC (10 campos) | ✅ Completo |
| **DerivedContext** | timestamp, bin, maskedPan, last4, normalizedPostalCode, normalizedState, normalizedCountryCode | ✅ Básico |
| **VelocityService** | count, sum, avg por PAN/Customer/Merchant em janelas temporais | ✅ Parcial |
| **GeoService** | coordenadas, distância, polígono | ✅ Parcial |
| **DeviceFingerprintService** | fingerprint, isNew, distinctDevices, riskScore | ✅ Parcial |
| **ImpossibleTravelService** | travelSpeed, impossibleTravel | ✅ Parcial |

### Campos Faltantes por Categoria

## 📋 CATEGORIA 1: Velocity (25 campos) - PRIORIDADE ALTA

### Campos Necessários
```
transactionsLast5min          → VelocityService.getStats(PAN, MINUTE_5).count
transactionsLast1h            → VelocityService.getStats(PAN, HOUR_1).count
transactionsLast24h           → VelocityService.getStats(PAN, HOUR_24).count
amountLast1h                  → VelocityService.getStats(PAN, HOUR_1).sum
amountLast24h                 → VelocityService.getStats(PAN, HOUR_24).sum
avgAmountLast24h              → VelocityService.getStats(PAN, HOUR_24).avg
avgAmountRatio24h             → currentAmount / avgAmountLast24h
distinctMerchantsLast1h       → VelocityService.getDistinctCount(PAN, MERCHANT, HOUR_1)
distinctMerchantsLast24h      → VelocityService.getDistinctCount(PAN, MERCHANT, HOUR_24)
maxAmountLast24h              → VelocityService.getStats(PAN, HOUR_24).max
minAmountLast24h              → VelocityService.getStats(PAN, HOUR_24).min
sumAmountLast7d               → VelocityService.getStats(PAN, DAY_7).sum
countLast7d                   → VelocityService.getStats(PAN, DAY_7).count
countLast30d                  → VelocityService.getStats(PAN, DAY_30).count
avgAmountLast30d              → VelocityService.getStats(PAN, DAY_30).avg
countDeclinedLast24h          → VelocityService.getDeclinedCount(PAN, HOUR_24)
countApprovedLast24h          → VelocityService.getApprovedCount(PAN, HOUR_24)
approvalRateLast24h           → approved / (approved + declined)
timeSinceLastTransaction      → now - lastTransactionTimestamp (minutos)
timeSinceLastApproved         → now - lastApprovedTimestamp (minutos)
consecutiveDeclines           → contagem de declines consecutivos
consecutiveApprovals          → contagem de approvals consecutivos
velocityScore                 → score calculado baseado em múltiplas métricas
isVelocityAnomaly             → velocityScore > threshold
merchantVelocityLast1h        → transações no mesmo merchant na última hora
```

### Implementação Proposta
- Expandir `VelocityService.VelocityStats` com campos `max`, `min`
- Criar método `getDistinctCount(keyType, distinctField, window)`
- Criar método `getDeclinedCount()` e `getApprovedCount()`
- Criar `VelocityEnrichment` class para consolidar todos os campos

---

## 📋 CATEGORIA 2: Device (20 campos) - PRIORIDADE ALTA

### Campos Necessários
```
device.fingerprint            → DeviceFingerprintService.generateFingerprint()
device.is_new                 → DeviceFingerprintService.isNewDevice()
device.risk_score             → DeviceFingerprintService.calculateRiskScore()
device.distinct_devices_24h   → DeviceFingerprintService.getDistinctDevicesForPan(24h)
device.distinct_devices_7d    → DeviceFingerprintService.getDistinctDevicesForPan(7d)
device.distinct_pans_24h      → DeviceFingerprintService.getDistinctPansForDevice(24h)
device.age_days               → dias desde primeira vez visto
device.last_seen_hours        → horas desde última vez visto
device.is_emulator            → flag de emulador detectado
device.is_rooted              → flag de dispositivo rooteado
device.is_vpn                 → flag de VPN detectado
device.is_proxy               → flag de proxy detectado
device.is_tor                 → flag de Tor detectado
device.is_datacenter_ip       → flag de IP de datacenter
device.browser_anomaly        → anomalia no browser fingerprint
device.timezone_mismatch      → timezone não bate com localização
device.language_mismatch      → idioma não bate com país
device.screen_anomaly         → resolução de tela suspeita
device.fingerprint_blocked    → fingerprint em lista de bloqueio
device.trust_score            → score de confiança do device (0-100)
```

### Implementação Proposta
- Expandir `DeviceFingerprintService` com métodos faltantes
- Criar `DeviceEnrichment` class para consolidar campos
- Integrar com tabela `device_fingerprints` existente

---

## 📋 CATEGORIA 3: Geo (18 campos) - PRIORIDADE MÉDIA

### Campos Necessários
```
geo.latitude                  → GeoService.getCoordinates().latitude
geo.longitude                 → GeoService.getCoordinates().longitude
geo.country                   → país normalizado
geo.state                     → estado normalizado
geo.city                      → cidade normalizada
geo.postal_code               → CEP normalizado
geo.ip_country                → país do IP
geo.ip_country_mismatch       → ip_country != merchant_country
geo.travel_speed_kmh          → ImpossibleTravelService.calculateSpeed()
geo.travel_distance_km        → distância desde última transação
geo.is_impossible_travel      → ImpossibleTravelService.isImpossibleTravel()
geo.is_high_risk_country      → país em lista de alto risco
geo.is_sanctioned_country     → país sancionado (OFAC/FATF)
geo.distance_from_home        → distância do endereço cadastrado
geo.is_domestic               → transação doméstica (mesmo país)
geo.is_cross_border           → transação internacional
geo.region_risk_score         → score de risco da região
geo.timezone                  → timezone derivado da localização
```

### Implementação Proposta
- Expandir `GeoService` com métodos faltantes
- Criar `GeoEnrichment` class
- Integrar com `ImpossibleTravelService`

---

## 📋 CATEGORIA 4: Customer (15 campos) - PRIORIDADE MÉDIA

### Campos Necessários
```
customer.is_first_transaction → primeira transação do cliente
customer.account_age_days     → dias desde criação da conta
customer.total_transactions   → total de transações históricas
customer.total_amount         → soma de todas transações
customer.avg_amount           → média histórica
customer.max_amount           → maior transação histórica
customer.last_transaction_days→ dias desde última transação
customer.chargeback_count     → número de chargebacks
customer.chargeback_rate      → taxa de chargeback
customer.fraud_flag           → flag de fraude anterior
customer.risk_score           → score de risco do cliente
customer.kyc_verified         → KYC verificado
customer.cpf_blocked          → CPF em lista de bloqueio
customer.email_domain_risk    → risco do domínio do email
customer.phone_verified       → telefone verificado
```

### Implementação Proposta
- Criar novo `CustomerEnrichmentService`
- Integrar com tabelas de histórico de cliente
- Criar `CustomerEnrichment` class

---

## 📋 CATEGORIA 5: Card (12 campos) - PRIORIDADE MÉDIA

### Campos Necessários
```
card.is_expired               → data de expiração < hoje
card.days_to_expire           → dias até expiração
card.has_chip                 → cartão tem chip
card.is_contactless           → cartão é contactless
card.is_virtual               → cartão virtual
card.is_prepaid               → cartão pré-pago (do BIN)
card.is_commercial            → cartão comercial (do BIN)
card.bin_blacklisted          → BIN em lista negra
card.testing_pattern          → padrão de teste de cartão
card.brand                    → bandeira (do BIN)
card.type                     → tipo (crédito/débito)
card.level                    → nível (classic/gold/platinum)
```

### Implementação Proposta
- Expandir `EnrichmentService.BinEnrichment`
- Adicionar lógica de expiração
- Criar `CardEnrichment` class

---

## 📋 CATEGORIA 6: Auth (8 campos) - PRIORIDADE BAIXA

### Campos Necessários
```
auth.consecutive_failures     → falhas consecutivas de auth
auth.attempts_5min            → tentativas nos últimos 5min
auth.attempts_1h              → tentativas na última hora
auth.last_failure_minutes     → minutos desde última falha
cvv.consecutive_failures      → falhas consecutivas de CVV
cvv.attempts_24h              → tentativas de CVV em 24h
pin.consecutive_failures      → falhas consecutivas de PIN
mfa.completed                 → MFA completado com sucesso
```

### Implementação Proposta
- Criar `AuthEnrichmentService`
- Rastrear tentativas de autenticação
- Criar `AuthEnrichment` class

---

## 📋 CATEGORIA 7: Anomaly (5 campos) - PRIORIDADE BAIXA

### Campos Necessários
```
anomaly.unusual_hour          → hora fora do padrão do cliente
anomaly.unusual_day           → dia da semana fora do padrão
anomaly.value_deviation_ratio → desvio do valor vs média
anomaly.behavior_change_score → score de mudança de comportamento
anomaly.is_anomalous          → flag geral de anomalia
```

### Implementação Proposta
- Criar `AnomalyDetectionService`
- Implementar análise de padrões temporais
- Criar `AnomalyEnrichment` class

---

## 🏗️ Arquitetura Proposta

### Novo Service: `TransactionEnrichmentFacade`

```java
@Service
public class TransactionEnrichmentFacade {
    
    private final EnrichmentService enrichmentService;
    private final VelocityService velocityService;
    private final DeviceFingerprintService deviceService;
    private final GeoService geoService;
    private final ImpossibleTravelService travelService;
    private final CustomerEnrichmentService customerService;
    private final AuthEnrichmentService authService;
    private final AnomalyDetectionService anomalyService;
    
    public EnrichedContext enrich(TransactionRequest request) {
        return EnrichedContext.builder()
            .basic(enrichmentService.enrich(request))
            .velocity(velocityService.enrich(request))
            .device(deviceService.enrich(request))
            .geo(geoService.enrich(request))
            .customer(customerService.enrich(request))
            .auth(authService.enrich(request))
            .anomaly(anomalyService.enrich(request))
            .build();
    }
    
    public Map<String, Object> toFlatMap(EnrichedContext ctx) {
        // Retorna todos os campos em um Map plano para o evaluator
    }
}
```

---

## 📅 Cronograma de Implementação

### Fase 1: Velocity (Semana 1) - CRÍTICO
- [ ] Expandir VelocityStats com max/min
- [ ] Implementar getDistinctCount
- [ ] Implementar contadores de declined/approved
- [ ] Criar VelocityEnrichment
- [ ] Testes unitários

### Fase 2: Device (Semana 1-2) - CRÍTICO
- [ ] Expandir DeviceFingerprintService
- [ ] Implementar campos de detecção (emulator, vpn, etc)
- [ ] Criar DeviceEnrichment
- [ ] Testes unitários

### Fase 3: Geo (Semana 2) - IMPORTANTE
- [ ] Expandir GeoService
- [ ] Integrar ImpossibleTravelService
- [ ] Criar GeoEnrichment
- [ ] Testes unitários

### Fase 4: Customer (Semana 3) - IMPORTANTE
- [ ] Criar CustomerEnrichmentService
- [ ] Implementar histórico de cliente
- [ ] Criar CustomerEnrichment
- [ ] Testes unitários

### Fase 5: Card/Auth/Anomaly (Semana 3-4) - DESEJÁVEL
- [ ] Expandir CardEnrichment
- [ ] Criar AuthEnrichmentService
- [ ] Criar AnomalyDetectionService
- [ ] Testes unitários

### Fase 6: Integração (Semana 4)
- [ ] Criar TransactionEnrichmentFacade
- [ ] Integrar com ComplexRuleEvaluator
- [ ] Testes de integração
- [ ] Documentação

---

## 🎯 Métricas de Sucesso

1. **Cobertura de Campos**: 100% dos 103 campos implementados
2. **Cobertura de Testes**: > 80% para novos services
3. **Performance**: Enriquecimento < 50ms por transação
4. **Regras Funcionais**: Todas as 310 regras executando corretamente

---

## ⚠️ Riscos e Mitigações

| Risco | Impacto | Mitigação |
|-------|---------|-----------|
| Performance degradada | Alto | Cache agressivo, queries otimizadas |
| Dados históricos insuficientes | Médio | Fallback para valores default |
| Complexidade de integração | Médio | Facade pattern, testes extensivos |
| Breaking changes | Alto | Versionamento, backward compatibility |

