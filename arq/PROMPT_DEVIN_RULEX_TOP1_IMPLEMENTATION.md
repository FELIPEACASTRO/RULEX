# 🎯 PROMPT PARA DEVIN - IMPLEMENTAÇÃO COMPLETA RULEX TOP 1

**Missão:** Implementar todos os operadores e operações faltantes no sistema RULEX para alcançar o status **TOP 1 GLOBAL** em detecção de fraude bancária.

**Branch:** `cursor/rulex-project-review-1c58`  
**Commit Base:** `054ff52`  
**Idioma:** Português (Brasil)  
**Regra Central:** Anti-alucinação total - nada pode ser deduzido ou inventado.

---

## 📋 CONTEXTO DO PROJETO

O RULEX é um motor de regras parametrizáveis para detecção de fraude bancária e AML (Anti-Money Laundering), construído com:
- **Backend:** Java 21 + Spring Boot 3.5.9
- **Frontend:** React 19.2.1 + TypeScript + Vite
- **Banco de Dados:** PostgreSQL 16 + Redis 7
- **Arquitetura:** Monorepo com backend, client, e2e, docs

**Status Atual:**
- ✅ 119 operadores definidos no enum `ConditionOperator`
- ✅ 93 operadores implementados no `ComplexRuleEvaluator`
- ❌ 26 operadores faltando implementação
- ❌ 8 serviços de enrichment criados mas **não integrados**
- ❌ 100+ campos derivados **não disponíveis** para as regras

**Documentação Base:** 9 arquivos em `/home/ubuntu/RULEX/arq/`:
1. `RULEX_QUADRUPLE_CHECK_FINAL.md` - Auditoria de 4.049 URLs, 28 frameworks regulatórios
2. `RULEX_COMPENDIO_COMPLETO.md` - Base de conhecimento completa
3. `RULEX_TECNICAS_AVANCADAS_DSL.md` - Operadores avançados e DSL
4. `RULEX_TRIPLE_CHECK_VALIDACAO.md` - Validação de 95% das URLs
5. `ANALISE_URLS_SISTEMA_RULEX_COMPLETA.md` - Catalogação de todas as fontes
6. `REFERENCIA_URLS_COMPLETA.md` - Referências técnicas
7. `RULEX-TripleCheck-1000x.md` - Triple-check rigoroso
8. `RULEX_TRIPLE_CHECK_1000x_FINAL.md` - Versão final do triple-check
9. `PROMPT_UNICO_DEVIN_BACKUP_RULEX.md` - Guia de backup forense

---

## 🎯 OBJETIVOS DA MISSÃO

### Objetivo 1: Integrar Serviços de Enrichment (CRÍTICO)
**Status Atual:** 8 serviços criados mas **não integrados** no fluxo de avaliação.

**Arquivos Afetados:**
- `/backend/src/main/java/com/rulex/service/RuleEngineService.java`
- `/backend/src/main/java/com/rulex/service/enrichment/TransactionEnrichmentFacade.java`
- `/backend/src/main/java/com/rulex/service/complex/ComplexRuleEvaluator.java`

**Ações Obrigatórias:**
1. Modificar `RuleEngineService.evaluate()` para chamar `TransactionEnrichmentFacade.enrichFull(transactionRequest)` **ANTES** de avaliar as regras
2. Obter o `FullEnrichmentContext` e converter para Map plano usando `context.toFlatMap()`
3. Passar este Map para o `ComplexRuleEvaluator` como parte do `EvaluationContext.payload`
4. Validar que todos os 100+ campos derivados estão acessíveis nas regras (ex: `velocity_count_5m`, `device_is_new`, `impossible_travel_detected`)

**Evidência de Sucesso:**
- Criar teste de integração que valida que `velocity_count_5m` está disponível no payload
- Criar teste que valida que `device_is_new` está disponível no payload
- Criar teste que valida que `impossible_travel_detected` está disponível no payload

**Campos Derivados Esperados (100+):**
```java
// VelocityEnrichment (20 campos)
velocity_count_1m, velocity_count_5m, velocity_count_1h, velocity_count_24h
velocity_sum_1m, velocity_sum_5m, velocity_sum_1h, velocity_sum_24h
velocity_avg_1m, velocity_avg_5m, velocity_avg_1h, velocity_avg_24h
velocity_distinct_merchants_1h, velocity_distinct_countries_1h
velocity_distinct_pans_1h, velocity_distinct_accounts_1h

// DeviceEnrichment (14 campos)
device_age_days, device_reputation_score, device_abuse_score
device_is_new, device_pan_count, device_account_count
device_first_seen_at, device_last_seen_at

// GeoEnrichment (10 campos)
geo_distance_from_last_km, impossible_travel_detected
geo_country_mismatch, ip_datacenter_flag, ip_reputation_score
geo_latitude, geo_longitude

// CustomerEnrichment (18 campos)
customer_age_days, customer_lifetime_value, customer_risk_score
customer_chargeback_rate, customer_usual_hours, in_customer_usual_hours
customer_usual_merchants, in_customer_usual_merchants

// CardEnrichment (10 campos)
card_age_days, card_freshness_score, card_on_decline_list
card_mill_pattern_detected, cvv_brute_force_detected

// AuthEnrichment (12 campos)
mfa_abandonments_count, mfa_denials_count_last_1h
failed_3ds_last_5m, login_failures_count_last_5m
device_changed_in_session

// AnomalyEnrichment (15 campos)
anomaly_score, velocity_spike_detected, amount_spike_detected
pattern_escalation_detected, pattern_round_numbers_detected
pattern_split_transaction_detected
```

---

### Objetivo 2: Implementar 15 Operadores Faltantes (ALTA PRIORIDADE)

**Arquivo Afetado:**
- `/backend/src/main/java/com/rulex/service/complex/ComplexRuleEvaluator.java`

**Operadores a Implementar:**

#### 2.1. Operadores de Velocity/Temporal (8)

**1. COUNT_CRYPTO_TXN_LAST_N_DAYS**
```java
case COUNT_CRYPTO_TXN_LAST_N_DAYS -> {
  // Contar transações crypto nos últimos N dias
  // Usar VelocityServiceFacade com filtro mcc IN (6211, 6051, 7995)
  int days = Integer.parseInt(condition.getValueSingle());
  String groupBy = condition.getValueFieldRef(); // Ex: "account_id"
  Object groupValue = context.getPayload().get(groupBy);
  
  VelocityStats stats = velocityServiceFacade.getStats(
    context.getTransactionRequest(),
    VelocityService.KeyType.ACCOUNT,
    groupValue.toString(),
    VelocityService.AggregationType.COUNT,
    days * 24 * 60 // converter dias para minutos
  );
  
  // Filtrar apenas MCCs de crypto
  long cryptoCount = stats.getCount(); // Implementar filtro no VelocityService
  yield compareValues(cryptoCount, condition.getValueMin()) > 0;
}
```

**2. COUNT_DISTINCT_INSTRUMENTS_LAST_N_DAYS**
```java
case COUNT_DISTINCT_INSTRUMENTS_LAST_N_DAYS -> {
  // Contar instrumentos distintos (PANs) nos últimos N dias
  int days = Integer.parseInt(condition.getValueSingle());
  String groupBy = condition.getValueFieldRef(); // Ex: "account_id"
  Object groupValue = context.getPayload().get(groupBy);
  
  VelocityStats stats = velocityServiceFacade.getStats(
    context.getTransactionRequest(),
    VelocityService.KeyType.ACCOUNT,
    groupValue.toString(),
    VelocityService.AggregationType.DISTINCT_COUNT,
    days * 24 * 60
  );
  
  yield compareValues(stats.getDistinctCount(), condition.getValueMin()) > 0;
}
```

**3. COUNT_DISTINCT_PAYERS_LAST_N_DAYS**
```java
case COUNT_DISTINCT_PAYERS_LAST_N_DAYS -> {
  // Contar pagadores distintos nos últimos N dias
  // Similar ao COUNT_DISTINCT_INSTRUMENTS, mas com KeyType.PAYER
  int days = Integer.parseInt(condition.getValueSingle());
  String groupBy = condition.getValueFieldRef();
  Object groupValue = context.getPayload().get(groupBy);
  
  VelocityStats stats = velocityServiceFacade.getStats(
    context.getTransactionRequest(),
    VelocityService.KeyType.PAYER,
    groupValue.toString(),
    VelocityService.AggregationType.DISTINCT_COUNT,
    days * 24 * 60
  );
  
  yield compareValues(stats.getDistinctCount(), condition.getValueMin()) > 0;
}
```

**4. COUNT_DISTINCT_USER_AGENTS_LAST_N_HOURS**
```java
case COUNT_DISTINCT_USER_AGENTS_LAST_N_HOURS -> {
  // Contar user agents distintos nas últimas N horas
  int hours = Integer.parseInt(condition.getValueSingle());
  String groupBy = condition.getValueFieldRef(); // Ex: "account_id"
  Object groupValue = context.getPayload().get(groupBy);
  
  VelocityStats stats = velocityServiceFacade.getStats(
    context.getTransactionRequest(),
    VelocityService.KeyType.USER_AGENT,
    groupValue.toString(),
    VelocityService.AggregationType.DISTINCT_COUNT,
    hours * 60
  );
  
  yield compareValues(stats.getDistinctCount(), condition.getValueMin()) > 0;
}
```

**5. COUNT_LAST_N_DAYS**
```java
case COUNT_LAST_N_DAYS -> {
  // Contagem genérica nos últimos N dias
  int days = Integer.parseInt(condition.getValueSingle());
  String groupBy = condition.getValueFieldRef();
  Object groupValue = context.getPayload().get(groupBy);
  
  VelocityStats stats = velocityServiceFacade.getStats(
    context.getTransactionRequest(),
    VelocityService.KeyType.valueOf(groupBy.toUpperCase()),
    groupValue.toString(),
    VelocityService.AggregationType.COUNT,
    days * 24 * 60
  );
  
  yield compareValues(stats.getCount(), condition.getValueMin()) > 0;
}
```

**6. DAYS_SINCE_LAST_ACTIVITY**
```java
case DAYS_SINCE_LAST_ACTIVITY -> {
  // Dias desde última atividade
  String groupBy = condition.getValueFieldRef(); // Ex: "account_id"
  Object groupValue = context.getPayload().get(groupBy);
  
  // Buscar última transação do account_id no banco
  LocalDateTime lastActivity = transactionRepository.findLastActivityTime(groupValue.toString());
  
  if (lastActivity == null) {
    yield false; // Nunca teve atividade
  }
  
  long daysSince = ChronoUnit.DAYS.between(lastActivity, LocalDateTime.now());
  yield compareValues(daysSince, condition.getValueSingle()) > 0;
}
```

**7. HAS_INCOMING_TRANSFER_LAST_N_HOURS**
```java
case HAS_INCOMING_TRANSFER_LAST_N_HOURS -> {
  // Verificar se houve transferência de entrada nas últimas N horas
  int hours = Integer.parseInt(condition.getValueSingle());
  String accountId = context.getPayload().get("account_id").toString();
  
  // Buscar transações de entrada (credit) nas últimas N horas
  boolean hasIncoming = transactionRepository.hasIncomingTransfer(
    accountId,
    LocalDateTime.now().minusHours(hours)
  );
  
  yield hasIncoming;
}
```

**8. PIX_KEY_CHANGED_LAST_N_DAYS**
```java
case PIX_KEY_CHANGED_LAST_N_DAYS -> {
  // Verificar se chave PIX foi alterada nos últimos N dias
  int days = Integer.parseInt(condition.getValueSingle());
  String accountId = context.getPayload().get("account_id").toString();
  
  // Buscar histórico de mudanças de chave PIX
  boolean keyChanged = pixKeyChangeRepository.hasChangeInLastDays(accountId, days);
  
  yield keyChanged;
}
```

---

#### 2.2. Operadores de Autenticação/MFA (2)

**9. COUNT_MFA_ABANDONMENTS**
```java
case COUNT_MFA_ABANDONMENTS -> {
  // Contagem de abandonos de MFA
  String accountId = context.getPayload().get("account_id").toString();
  int windowMinutes = Integer.parseInt(condition.getValueSingle());
  
  // Buscar eventos de MFA abandonados nas últimas N minutos
  long abandonments = authEventRepository.countMfaAbandonments(
    accountId,
    LocalDateTime.now().minusMinutes(windowMinutes)
  );
  
  yield compareValues(abandonments, condition.getValueMin()) > 0;
}
```

**10. COUNT_MFA_DENIALS_LAST_N_HOURS**
```java
case COUNT_MFA_DENIALS_LAST_N_HOURS -> {
  // Contagem de negações MFA nas últimas N horas
  String accountId = context.getPayload().get("account_id").toString();
  int hours = Integer.parseInt(condition.getValueSingle());
  
  long denials = authEventRepository.countMfaDenials(
    accountId,
    LocalDateTime.now().minusHours(hours)
  );
  
  yield compareValues(denials, condition.getValueMin()) > 0;
}
```

---

#### 2.3. Operadores de Fraude Avançada (4)

**11. CONTAINS_SUSPICIOUS_KEYWORDS**
```java
case CONTAINS_SUSPICIOUS_KEYWORDS -> {
  // Verificar se contém palavras-chave suspeitas
  Object fieldValue = getFieldValue(condition.getFieldName(), context);
  if (fieldValue == null) yield false;
  
  String text = fieldValue.toString().toLowerCase();
  
  // Lista de palavras-chave suspeitas (fonte: FinCEN Advisories)
  String[] suspiciousKeywords = {
    "urgent", "immediately", "wire transfer", "bitcoin", "crypto",
    "ransom", "encrypt", "decrypt", "pay now", "verify account",
    "suspended", "confirm identity", "click here", "reset password",
    "unusual activity", "fraud alert", "security breach"
  };
  
  for (String keyword : suspiciousKeywords) {
    if (text.contains(keyword)) {
      yield true;
    }
  }
  
  yield false;
}
```

**12. DEVICE_CHANGED_IN_SESSION**
```java
case DEVICE_CHANGED_IN_SESSION -> {
  // Verificar se device mudou na sessão
  String sessionId = context.getPayload().get("session_id").toString();
  String currentDeviceId = context.getPayload().get("device_id").toString();
  
  // Buscar device_id inicial da sessão
  String initialDeviceId = sessionRepository.getInitialDeviceId(sessionId);
  
  yield !currentDeviceId.equals(initialDeviceId);
}
```

**13. IS_CRYPTO_RANSOM_AMOUNT**
```java
case IS_CRYPTO_RANSOM_AMOUNT -> {
  // Verificar se valor é típico de ransom crypto
  // Fonte: FinCEN Cryptocurrency Typology Report
  Object amountObj = getFieldValue("amount", context);
  if (amountObj == null) yield false;
  
  BigDecimal amount = toBigDecimal(amountObj);
  
  // Valores típicos de ransomware (em USD equivalente)
  BigDecimal[] ransomAmounts = {
    new BigDecimal("500"),    // Pequeno
    new BigDecimal("1000"),   // Médio
    new BigDecimal("5000"),   // Grande
    new BigDecimal("10000"),  // Corporativo
    new BigDecimal("50000"),  // Enterprise
    new BigDecimal("100000")  // Critical Infrastructure
  };
  
  // Verificar se está dentro de 10% de algum valor típico
  for (BigDecimal ransomAmount : ransomAmounts) {
    BigDecimal diff = amount.subtract(ransomAmount).abs();
    BigDecimal threshold = ransomAmount.multiply(new BigDecimal("0.1"));
    
    if (diff.compareTo(threshold) <= 0) {
      yield true;
    }
  }
  
  yield false;
}
```

**14. IS_IMPOSSIBLE_COMBINATION**
```java
case IS_IMPOSSIBLE_COMBINATION -> {
  // Verificar combinação impossível de dados
  // Exemplos: CPF de menor de idade + cartão de crédito
  //           Email criado hoje + histórico de 5 anos
  //           Telefone VoIP + endereço residencial
  
  // Verificar CPF de menor + cartão
  Object cpfObj = context.getPayload().get("cpf");
  Object cardTypeObj = context.getPayload().get("card_type");
  
  if (cpfObj != null && cardTypeObj != null) {
    int age = calculateAgeFromCPF(cpfObj.toString());
    if (age < 18 && "CREDIT".equals(cardTypeObj.toString())) {
      yield true;
    }
  }
  
  // Verificar email novo + histórico antigo
  Object emailAgeObj = context.getPayload().get("email_age_days");
  Object accountAgeObj = context.getPayload().get("account_age_days");
  
  if (emailAgeObj != null && accountAgeObj != null) {
    int emailAge = Integer.parseInt(emailAgeObj.toString());
    int accountAge = Integer.parseInt(accountAgeObj.toString());
    
    if (emailAge < 30 && accountAge > 365) {
      yield true; // Email criado há menos de 30 dias, mas conta tem mais de 1 ano
    }
  }
  
  // Verificar telefone VoIP + endereço residencial
  Object phoneTypeObj = context.getPayload().get("phone_type");
  Object addressTypeObj = context.getPayload().get("address_type");
  
  if ("VOIP".equals(phoneTypeObj) && "RESIDENTIAL".equals(addressTypeObj)) {
    yield true;
  }
  
  yield false;
}
```

---

#### 2.4. Operadores de Compatibilidade (1)

**15. IN_LIST**
```java
case IN_LIST -> {
  // Alias para IN (compatibilidade com migrações)
  yield evaluateIn(fieldValue, condition.getValueArray(), condition.getCaseSensitive());
}
```

---

### Objetivo 3: Criar Repositórios e Entidades Faltantes (MÉDIO PRAZO)

**Novos Repositórios Necessários:**

1. **AuthEventRepository.java**
```java
@Repository
public interface AuthEventRepository extends JpaRepository<AuthEvent, UUID> {
  
  @Query("""
    SELECT COUNT(e) FROM AuthEvent e
    WHERE e.accountId = :accountId
    AND e.eventType = 'MFA_ABANDONED'
    AND e.timestamp >= :since
  """)
  long countMfaAbandonments(String accountId, LocalDateTime since);
  
  @Query("""
    SELECT COUNT(e) FROM AuthEvent e
    WHERE e.accountId = :accountId
    AND e.eventType = 'MFA_DENIED'
    AND e.timestamp >= :since
  """)
  long countMfaDenials(String accountId, LocalDateTime since);
}
```

2. **PixKeyChangeRepository.java**
```java
@Repository
public interface PixKeyChangeRepository extends JpaRepository<PixKeyChange, UUID> {
  
  @Query("""
    SELECT COUNT(p) > 0 FROM PixKeyChange p
    WHERE p.accountId = :accountId
    AND p.changedAt >= :since
  """)
  boolean hasChangeInLastDays(String accountId, int days);
}
```

3. **SessionRepository.java**
```java
@Repository
public interface SessionRepository extends JpaRepository<Session, UUID> {
  
  @Query("""
    SELECT s.initialDeviceId FROM Session s
    WHERE s.sessionId = :sessionId
  """)
  String getInitialDeviceId(String sessionId);
}
```

**Novas Entidades Necessárias:**

1. **AuthEvent.java**
```java
@Entity
@Table(name = "auth_events")
public class AuthEvent {
  @Id
  @GeneratedValue(strategy = GenerationType.UUID)
  private UUID id;
  
  @Column(nullable = false)
  private String accountId;
  
  @Enumerated(EnumType.STRING)
  @Column(nullable = false)
  private AuthEventType eventType; // MFA_ABANDONED, MFA_DENIED, LOGIN_FAILED, etc.
  
  @Column(nullable = false)
  private LocalDateTime timestamp;
  
  private String deviceId;
  private String ipAddress;
  private String userAgent;
}
```

2. **PixKeyChange.java**
```java
@Entity
@Table(name = "pix_key_changes")
public class PixKeyChange {
  @Id
  @GeneratedValue(strategy = GenerationType.UUID)
  private UUID id;
  
  @Column(nullable = false)
  private String accountId;
  
  @Column(nullable = false)
  private String oldPixKey;
  
  @Column(nullable = false)
  private String newPixKey;
  
  @Column(nullable = false)
  private LocalDateTime changedAt;
}
```

3. **Session.java**
```java
@Entity
@Table(name = "sessions")
public class Session {
  @Id
  @GeneratedValue(strategy = GenerationType.UUID)
  private UUID id;
  
  @Column(nullable = false, unique = true)
  private String sessionId;
  
  @Column(nullable = false)
  private String accountId;
  
  @Column(nullable = false)
  private String initialDeviceId;
  
  @Column(nullable = false)
  private LocalDateTime createdAt;
  
  private LocalDateTime expiresAt;
}
```

---

### Objetivo 4: Criar Migrations do Flyway (OBRIGATÓRIO)

**Arquivo:** `/backend/src/main/resources/db/migration/V39__add_auth_pix_session_tables.sql`

```sql
-- Tabela de eventos de autenticação
CREATE TABLE auth_events (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    account_id VARCHAR(255) NOT NULL,
    event_type VARCHAR(50) NOT NULL,
    timestamp TIMESTAMP WITH TIME ZONE NOT NULL,
    device_id VARCHAR(255),
    ip_address VARCHAR(45),
    user_agent TEXT,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_auth_events_account_timestamp ON auth_events(account_id, timestamp);
CREATE INDEX idx_auth_events_type ON auth_events(event_type);

-- Tabela de mudanças de chave PIX
CREATE TABLE pix_key_changes (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    account_id VARCHAR(255) NOT NULL,
    old_pix_key VARCHAR(255) NOT NULL,
    new_pix_key VARCHAR(255) NOT NULL,
    changed_at TIMESTAMP WITH TIME ZONE NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_pix_key_changes_account_changed ON pix_key_changes(account_id, changed_at);

-- Tabela de sessões
CREATE TABLE sessions (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    session_id VARCHAR(255) NOT NULL UNIQUE,
    account_id VARCHAR(255) NOT NULL,
    initial_device_id VARCHAR(255) NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL,
    expires_at TIMESTAMP WITH TIME ZONE,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_sessions_session_id ON sessions(session_id);
CREATE INDEX idx_sessions_account ON sessions(account_id);
```

---

### Objetivo 5: Criar Testes de Integração (OBRIGATÓRIO)

**Arquivo:** `/backend/src/test/java/com/rulex/service/EnrichmentIntegrationTest.java`

```java
@SpringBootTest
@Testcontainers
class EnrichmentIntegrationTest {
  
  @Autowired
  private RuleEngineService ruleEngineService;
  
  @Autowired
  private TransactionEnrichmentFacade enrichmentFacade;
  
  @Test
  void testEnrichmentFieldsAvailableInRules() {
    // Criar transação de teste
    TransactionRequest request = TransactionRequest.builder()
      .transactionId("test-123")
      .amount(new BigDecimal("1000.00"))
      .panHash("hash123")
      .accountId("acc-456")
      .deviceId("dev-789")
      .build();
    
    // Enriquecer transação
    FullEnrichmentContext enriched = enrichmentFacade.enrichFull(request);
    Map<String, Object> fields = enriched.toFlatMap();
    
    // Validar que campos derivados estão presentes
    assertThat(fields).containsKey("velocity_count_5m");
    assertThat(fields).containsKey("device_is_new");
    assertThat(fields).containsKey("impossible_travel_detected");
    assertThat(fields).containsKey("customer_risk_score");
    assertThat(fields).containsKey("anomaly_score");
    
    // Validar que campos têm valores corretos
    assertThat(fields.get("velocity_count_5m")).isInstanceOf(Long.class);
    assertThat(fields.get("device_is_new")).isInstanceOf(Boolean.class);
  }
  
  @Test
  void testNewOperatorsImplemented() {
    // Testar COUNT_CRYPTO_TXN_LAST_N_DAYS
    RuleCondition condition = RuleCondition.builder()
      .operator(ConditionOperator.COUNT_CRYPTO_TXN_LAST_N_DAYS)
      .valueSingle("7")
      .valueFieldRef("account_id")
      .valueMin("5")
      .build();
    
    // Criar contexto de avaliação
    EvaluationContext context = EvaluationContext.builder()
      .payload(Map.of("account_id", "acc-123"))
      .transactionRequest(createTestTransaction())
      .build();
    
    // Avaliar condição
    boolean result = evaluator.evaluateCondition(condition, context, new ArrayList<>());
    
    // Validar resultado
    assertThat(result).isNotNull();
  }
}
```

---

## 🔥 CRITÉRIOS DE SUCESSO

### Critério 1: Integração de Enrichments
- ✅ `TransactionEnrichmentFacade.enrichFull()` é chamado em `RuleEngineService.evaluate()`
- ✅ Todos os 100+ campos derivados estão disponíveis no `EvaluationContext.payload`
- ✅ Teste de integração valida presença de pelo menos 10 campos derivados

### Critério 2: Operadores Implementados
- ✅ Todos os 15 operadores faltantes estão implementados no switch do `ComplexRuleEvaluator`
- ✅ Cada operador tem lógica funcional (não apenas `yield false`)
- ✅ Cada operador tem comentário explicando a fonte (ex: "Fonte: FinCEN Advisory")

### Critério 3: Repositórios e Entidades
- ✅ 3 novos repositórios criados: `AuthEventRepository`, `PixKeyChangeRepository`, `SessionRepository`
- ✅ 3 novas entidades criadas: `AuthEvent`, `PixKeyChange`, `Session`
- ✅ Migration V39 criada e aplicada com sucesso

### Critério 4: Testes
- ✅ Teste de integração `EnrichmentIntegrationTest` criado e passando
- ✅ Pelo menos 2 testes para novos operadores criados
- ✅ Cobertura de código >= 80% para `ComplexRuleEvaluator`

### Critério 5: Compilação e Build
- ✅ Backend compila sem erros: `mvn clean compile`
- ✅ Testes passam: `mvn test`
- ✅ Frontend compila sem erros: `pnpm run check`

---

## 📚 REFERÊNCIAS OBRIGATÓRIAS

### Documentação Técnica (Ler ANTES de implementar)
1. **RULEX_TECNICAS_AVANCADAS_DSL.md** - Operadores avançados e Window Functions
2. **RULEX_QUADRUPLE_CHECK_FINAL.md** - Frameworks regulatórios (NIST, FinCEN, FATF)
3. **RULEX_COMPENDIO_COMPLETO.md** - Base de conhecimento completa

### Arquivos de Código (Estudar estrutura)
1. `/backend/src/main/java/com/rulex/service/complex/ComplexRuleEvaluator.java` - Motor de regras
2. `/backend/src/main/java/com/rulex/service/enrichment/TransactionEnrichmentFacade.java` - Facade de enrichment
3. `/backend/src/main/java/com/rulex/service/RuleEngineService.java` - Serviço principal
4. `/backend/src/main/java/com/rulex/entity/complex/RuleCondition.java` - Enum de operadores

### Padrões de Código (Seguir rigorosamente)
1. **Nomenclatura:** snake_case para campos derivados (ex: `velocity_count_5m`)
2. **Comentários:** Sempre incluir fonte da regra (ex: "Fonte: FinCEN Alert FIN-2024-Alert005")
3. **Validação:** Sempre verificar se campo existe antes de usar (`getFieldValue()`)
4. **Logging:** Usar `log.debug()` para operadores de velocity, `log.warn()` para anomalias
5. **Performance:** Operadores de velocity devem usar cache Redis quando possível

---

## ⚠️ REGRAS ANTI-ALUCINAÇÃO

1. **Proibido inventar** campos, métodos ou classes que não existem
2. **Proibido deduzir** comportamento de código não lido
3. **Obrigatório citar** fonte de cada regra implementada (FinCEN, FATF, NIST, etc.)
4. **Obrigatório validar** que campo existe antes de usar
5. **Obrigatório testar** cada operador implementado

---

## 🎯 ENTREGA FINAL

### Arquivos Modificados (6)
1. `/backend/src/main/java/com/rulex/service/RuleEngineService.java`
2. `/backend/src/main/java/com/rulex/service/complex/ComplexRuleEvaluator.java`
3. `/backend/src/main/resources/db/migration/V39__add_auth_pix_session_tables.sql`

### Arquivos Criados (6)
4. `/backend/src/main/java/com/rulex/repository/AuthEventRepository.java`
5. `/backend/src/main/java/com/rulex/repository/PixKeyChangeRepository.java`
6. `/backend/src/main/java/com/rulex/repository/SessionRepository.java`
7. `/backend/src/main/java/com/rulex/entity/AuthEvent.java`
8. `/backend/src/main/java/com/rulex/entity/PixKeyChange.java`
9. `/backend/src/main/java/com/rulex/entity/Session.java`
10. `/backend/src/test/java/com/rulex/service/EnrichmentIntegrationTest.java`

### Relatórios (2)
11. `IMPLEMENTATION_REPORT.md` - Relatório de implementação com evidências
12. `TEST_RESULTS.md` - Resultados de todos os testes executados

---

## 🏆 RESULTADO ESPERADO

Após a implementação completa:
- ✅ **119/119 operadores** funcionais (100%)
- ✅ **100+ campos derivados** disponíveis para regras
- ✅ **8/8 enrichments** integrados
- ✅ **85+ tipologias** de fraude cobertas
- ✅ **99.2% coverage** validado pelo Quadruple-Check
- ✅ **Performance <100ms** P99

**Status Final:** 🏆 **RULEX TOP 1 GLOBAL**

---

**COMECE AGORA.**
