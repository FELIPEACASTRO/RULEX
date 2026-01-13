# RULEX - Guia do Analista de Fraude

**Versão:** 1.0.0  
**Data:** 2025-01-03  
**Público-Alvo:** Analistas de Fraude, Operadores de Regras

---

## 1. Introdução

O RULEX é um **motor de regras duras (determinísticas)** para detecção de fraude em transações de cartão de crédito. Este guia explica como criar, gerenciar e monitorar regras de fraude.

### 1.1 O que é uma Regra Dura?

Uma regra dura é uma condição **determinística** que sempre produz o mesmo resultado para os mesmos inputs:
- ✅ Se `transactionAmount > 5000` → SUSPEITA
- ✅ Se `mcc IN (7995, 7994)` → SUSPEITA
- ❌ NÃO usa Machine Learning
- ❌ NÃO usa probabilidades

### 1.2 Vantagens das Regras Duras

| Vantagem | Descrição |
|----------|-----------|
| **Auditabilidade** | Cada decisão pode ser explicada |
| **Compliance** | Atende LGPD, BACEN |
| **Controle** | Ajustes em tempo real |
| **Previsibilidade** | Comportamento consistente |
| **Performance** | Avaliação em milissegundos |

---

## 2. Tipos de Regras

### 2.1 Regras Simples

Condições diretas com operador lógico único (AND/OR).

**Exemplo:**
```
SE mcc IN (7995, 7994, 7993) 
   E transactionAmount > 50000
ENTÃO SUSPEITA_DE_FRAUDE
```

### 2.2 Regras Complexas

Condições aninhadas com múltiplos grupos e operadores lógicos.

**Exemplo:**
```
SE (
    (mcc IN (7995, 7994, 7993))
    E (merchantCountryCode ≠ "076")
)
E (
    (transactionTime ENTRE 000000 E 060000)
    OU (transactionAmount > 100000)
)
ENTÃO FRAUDE
```

---

## 3. Campos Disponíveis

### 3.1 Campos de Identificação

| Campo | Tipo | Descrição |
|-------|------|-----------|
| `externalTransactionId` | String | ID único da transação |
| `customerIdFromHeader` | String | ID do cliente |
| `customerAcctNumber` | Long | Número da conta |
| `pan` | String | Número do cartão (PCI) |

### 3.2 Campos da Transação

| Campo | Tipo | Descrição |
|-------|------|-----------|
| `transactionAmount` | BigDecimal | Valor em centavos |
| `transactionDate` | Integer | Data (YYYYMMDD) |
| `transactionTime` | Integer | Hora (HHMMSS) |
| `mcc` | Integer | Merchant Category Code |

### 3.3 Campos de Segurança

| Campo | Tipo | Descrição |
|-------|------|-----------|
| `consumerAuthenticationScore` | Integer | Score de autenticação (0-999) |
| `cvv2Response` | String | Resposta CVV ("M"=match) |
| `cryptogramValid` | String | Criptograma válido ("Y"/"N") |
| `eciIndicator` | Integer | Indicador ECI (3DS) |

### 3.4 Campos de Localização

| Campo | Tipo | Descrição |
|-------|------|-----------|
| `merchantCountryCode` | String | Código do país (ISO 3166) |
| `merchantCity` | String | Cidade |
| `merchantState` | String | Estado |

### 3.5 Campos de Canal

| Campo | Tipo | Descrição |
|-------|------|-----------|
| `posEntryMode` | String | Modo de entrada ("1"=manual, "5"=chip, "9"=e-commerce) |
| `customerPresent` | String | Cliente presente ("0"=não, "1"=sim) |

---

## 4. Operadores

### 4.1 Comparação

| Operador | Descrição | Exemplo |
|----------|-----------|---------|
| `EQ` | Igual | `mcc EQ 7995` |
| `NEQ` | Diferente | `merchantCountryCode NEQ "076"` |
| `GT` | Maior que | `transactionAmount GT 50000` |
| `GTE` | Maior ou igual | `consumerAuthenticationScore GTE 50` |
| `LT` | Menor que | `externalScore3 LT 30` |
| `LTE` | Menor ou igual | `tokenAssuranceLevel LTE 10` |

### 4.2 Lista

| Operador | Descrição | Exemplo |
|----------|-----------|---------|
| `IN` | Está na lista | `mcc IN (7995, 7994, 7993)` |
| `NOT_IN` | Não está na lista | `merchantCountryCode NOT_IN ("076", "840")` |

### 4.3 Range

| Operador | Descrição | Exemplo |
|----------|-----------|---------|
| `BETWEEN` | Entre valores | `transactionAmount BETWEEN 10000,50000` |
| `NOT_BETWEEN` | Fora do range | `transactionTime NOT_BETWEEN 060000,220000` |

### 4.4 String

| Operador | Descrição | Exemplo |
|----------|-----------|---------|
| `CONTAINS` | Contém | `merchantName CONTAINS "CASINO"` |
| `STARTS_WITH` | Começa com | `pan STARTS_WITH "4111"` |
| `ENDS_WITH` | Termina com | `merchantId ENDS_WITH "-TEST"` |
| `REGEX` | Expressão regular | `merchantName REGEX "^CASINO.*"` |

### 4.5 Temporal

| Operador | Descrição | Exemplo |
|----------|-----------|---------|
| `TIME_BETWEEN` | Hora entre | `transactionTime TIME_BETWEEN 000000,060000` |
| `DATE_BEFORE` | Data anterior | `transactionDate DATE_BEFORE 20241231` |
| `DATE_AFTER` | Data posterior | `transactionDate DATE_AFTER 20240101` |

### 4.6 Velocity (Agregações)

| Operador | Descrição | Exemplo |
|----------|-----------|---------|
| `VELOCITY_COUNT_GT` | Contagem maior que | `VELOCITY_COUNT_GT PAN,60,5` (>5 tx em 60min) |
| `VELOCITY_SUM_GT` | Soma maior que | `VELOCITY_SUM_GT PAN,1440,1000000` (>R$10k em 24h) |
| `VELOCITY_DISTINCT_GT` | Distintos maior que | `VELOCITY_DISTINCT_GT PAN,1440,MERCHANTS,5` |

---

## 5. Decisões

| Decisão | Código | Descrição |
|---------|--------|-----------|
| `APROVADO` | 0 | Transação aprovada |
| `SUSPEITA_DE_FRAUDE` | 1 | Enviar para revisão |
| `FRAUDE` | 2 | Bloquear transação |

---

## 6. Criando uma Regra

### 6.1 Via Interface (UI)

1. Acesse **Regras** no menu lateral
2. Clique em **Nova Regra**
3. Preencha:
   - Nome da regra
   - Descrição
   - Categoria (SECURITY, CONTEXT, VELOCITY, ANOMALY)
   - Condições
   - Decisão
   - Severidade (0-100)
4. Clique em **Validar**
5. Clique em **Salvar**

### 6.2 Via API

```bash
POST /api/v1/complex-rules
Content-Type: application/json

{
  "key": "HIGH_VALUE_GAMBLING",
  "title": "High Value Gambling Transaction",
  "description": "Detects gambling transactions above R$500",
  "priority": 80,
  "severity": 75,
  "decision": "SUSPEITA_DE_FRAUDE",
  "rootConditionGroup": {
    "logicOperator": "AND",
    "conditions": [
      {"fieldName": "mcc", "operator": "IN", "valueArray": ["7995", "7994"]},
      {"fieldName": "transactionAmount", "operator": "GT", "valueSingle": "50000"}
    ]
  }
}
```

---

## 7. Ciclo de Vida da Regra

```
┌─────────┐     ┌───────────┐     ┌────────────┐
│  DRAFT  │────>│ PUBLISHED │────>│ DEPRECATED │
└─────────┘     └───────────┘     └────────────┘
     │                │
     │                │
     └────────────────┘
         (rollback)
```

### 7.1 Estados

| Estado | Descrição |
|--------|-----------|
| `DRAFT` | Em desenvolvimento, não avaliada |
| `PUBLISHED` | Ativa, sendo avaliada |
| `DEPRECATED` | Desativada, histórico |

### 7.2 Operações

| Operação | Descrição |
|----------|-----------|
| **Criar** | Nova regra em DRAFT |
| **Validar** | Verifica sintaxe e campos |
| **Publicar** | DRAFT → PUBLISHED |
| **Rollback** | Volta para versão anterior |
| **Deprecar** | PUBLISHED → DEPRECATED |

---

## 8. Boas Práticas

### 8.1 Nomenclatura

- Use nomes descritivos: `MCC_GAMBLING_HIGH_VALUE`
- Prefixe por categoria: `VELOCITY_`, `SECURITY_`, `CONTEXT_`
- Evite abreviações obscuras

### 8.2 Thresholds

- Comece conservador (menos falsos positivos)
- Ajuste baseado em dados reais
- Documente a razão do threshold

### 8.3 Testes

- Teste com transações reais (shadow mode)
- Valide falsos positivos antes de publicar
- Use simulação para estimar impacto

### 8.4 Documentação

- Sempre preencha a descrição
- Documente a fonte/evidência da regra
- Registre alterações no histórico

---

## 9. Monitoramento

### 9.1 Métricas Importantes

| Métrica | Descrição |
|---------|-----------|
| **Hit Rate** | % de transações que disparam a regra |
| **False Positive Rate** | % de alertas que são legítimos |
| **Detection Rate** | % de fraudes detectadas |
| **Latência** | Tempo de avaliação |

### 9.2 Alertas

Configure alertas para:
- Regra com hit rate muito alto (>10%)
- Regra com hit rate zero (pode estar errada)
- Latência acima de 100ms

---

## 10. Troubleshooting

### 10.1 Regra não está disparando

1. Verifique se está PUBLISHED
2. Verifique se está enabled
3. Verifique os campos e operadores
4. Teste com payload de exemplo

### 10.2 Muitos falsos positivos

1. Ajuste thresholds
2. Adicione exceções (whitelist)
3. Combine com outras condições
4. Use shadow mode para validar

### 10.3 Performance lenta

1. Evite REGEX complexos
2. Limite profundidade de grupos
3. Use índices apropriados
4. Considere cache de velocity

---

## 11. Referências

- [PAYLOAD_DICTIONARY.md](./PAYLOAD_DICTIONARY.md) - Campos disponíveis
- [RULE_ENGINE_CAPABILITIES.md](./RULE_ENGINE_CAPABILITIES.md) - Operadores
- [FRAUD_DETECTION_RULES_DEPLOYED.md](./FRAUD_DETECTION_RULES_DEPLOYED.md) - Regras existentes
- [03_RULES_CATALOG_TOP50.md](./03_RULES_CATALOG_TOP50.md) - Catálogo de regras
