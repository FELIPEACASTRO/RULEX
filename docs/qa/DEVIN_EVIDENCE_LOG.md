# DEVIN EVIDENCE LOG - RULEX HARDCORE AUDIT

## Propósito
Registrar outputs de comandos e evidências de testes para auditoria.

---

## 2024-12-31 - Início da Auditoria

### Git Status Inicial
```
On branch cursor/rulex-project-review-1c58
Changes not staged for commit:
  modified:   backend/src/main/java/com/rulex/entity/complex/RuleConditionGroup.java
  modified:   backend/src/main/java/com/rulex/repository/complex/RuleConditionGroupRepository.java

Untracked files:
  backend/src/main/resources/db/migration/V15__add_velocity_operators.sql
```

### Commit Realizado
```
[cursor/rulex-project-review-1c58 7c7c6c8] feat: add complexRuleId support to RuleConditionGroup and VELOCITY operators to DB enum
 3 files changed, 91 insertions(+), 7 deletions(-)
 create mode 100644 backend/src/main/resources/db/migration/V15__add_velocity_operators.sql
```

### Operadores Backend (RuleCondition.java)
```java
public enum ConditionOperator {
  // Comparação básica (6)
  EQ, NEQ, GT, GTE, LT, LTE,
  // Listas (2)
  IN, NOT_IN,
  // Strings (6)
  CONTAINS, NOT_CONTAINS, STARTS_WITH, ENDS_WITH, REGEX, NOT_REGEX,
  // Nulos (2)
  IS_NULL, NOT_NULL,
  // Booleanos (2)
  IS_TRUE, IS_FALSE,
  // Range (2)
  BETWEEN, NOT_BETWEEN,
  // Comparação entre campos (6)
  FIELD_EQ, FIELD_NEQ, FIELD_GT, FIELD_GTE, FIELD_LT, FIELD_LTE,
  // Data/Tempo (6)
  DATE_BEFORE, DATE_AFTER, DATE_BETWEEN, TIME_BEFORE, TIME_AFTER, TIME_BETWEEN,
  // Array (5)
  ARRAY_CONTAINS, ARRAY_NOT_CONTAINS, ARRAY_SIZE_EQ, ARRAY_SIZE_GT, ARRAY_SIZE_LT,
  // Matemáticos (2)
  MOD_EQ, MOD_NEQ,
  // Geolocalização (3)
  GEO_DISTANCE_LT, GEO_DISTANCE_GT, GEO_IN_POLYGON,
  // Velocity (8)
  VELOCITY_COUNT_GT, VELOCITY_COUNT_LT, VELOCITY_SUM_GT, VELOCITY_SUM_LT,
  VELOCITY_AVG_GT, VELOCITY_AVG_LT, VELOCITY_DISTINCT_GT, VELOCITY_DISTINCT_LT
}
// TOTAL: 50 operadores
```

### Operadores Frontend (ComplexRuleBuilder/types.ts)
```typescript
export type ComparisonOperator =
  // Básicos (6)
  | 'EQ' | 'NEQ' | 'GT' | 'GTE' | 'LT' | 'LTE'
  // Listas (2)
  | 'IN' | 'NOT_IN'
  // Range (2)
  | 'BETWEEN' | 'NOT_BETWEEN'
  // Strings (7) - inclui MATCHES_REGEX legacy
  | 'CONTAINS' | 'NOT_CONTAINS' | 'STARTS_WITH' | 'ENDS_WITH' | 'REGEX' | 'NOT_REGEX' | 'MATCHES_REGEX'
  // Nulos (3) - inclui IS_NOT_NULL legacy
  | 'IS_NULL' | 'NOT_NULL' | 'IS_NOT_NULL'
  // Booleanos (2)
  | 'IS_TRUE' | 'IS_FALSE'
  // Comparação entre campos (6)
  | 'FIELD_EQ' | 'FIELD_NEQ' | 'FIELD_GT' | 'FIELD_GTE' | 'FIELD_LT' | 'FIELD_LTE'
  // Data/Hora (6)
  | 'DATE_BEFORE' | 'DATE_AFTER' | 'DATE_BETWEEN' | 'TIME_BEFORE' | 'TIME_AFTER' | 'TIME_BETWEEN'
  // Arrays (5)
  | 'ARRAY_CONTAINS' | 'ARRAY_NOT_CONTAINS' | 'ARRAY_SIZE_EQ' | 'ARRAY_SIZE_GT' | 'ARRAY_SIZE_LT'
  // Matemáticos (2)
  | 'MOD_EQ' | 'MOD_NEQ'
  // Geolocalização (3)
  | 'GEO_DISTANCE_LT' | 'GEO_DISTANCE_GT' | 'GEO_IN_POLYGON'
  // Velocity (8)
  | 'VELOCITY_COUNT_GT' | 'VELOCITY_COUNT_LT' | 'VELOCITY_SUM_GT' | 'VELOCITY_SUM_LT'
  | 'VELOCITY_AVG_GT' | 'VELOCITY_AVG_LT' | 'VELOCITY_DISTINCT_GT' | 'VELOCITY_DISTINCT_LT';
// TOTAL: 52 operadores (inclui 2 legacy)
```

### RBAC Config (SecurityConfig.java)
```java
// Public endpoints
.requestMatchers(HttpMethod.POST, "/transactions/analyze").permitAll()
.requestMatchers(HttpMethod.POST, "/transactions/analyze-advanced").permitAll()
.requestMatchers(HttpMethod.POST, "/evaluate").permitAll()
.requestMatchers("/actuator/health/**").permitAll()

// ANALYST + ADMIN (read-only)
.requestMatchers(HttpMethod.GET, "/transactions/**").hasAnyRole("ANALYST", "ADMIN")
.requestMatchers(HttpMethod.GET, "/rules/**").hasAnyRole("ANALYST", "ADMIN")
.requestMatchers(HttpMethod.GET, "/audit/**").hasAnyRole("ANALYST", "ADMIN")
.requestMatchers(HttpMethod.GET, "/metrics/**").hasAnyRole("ANALYST", "ADMIN")
.requestMatchers(HttpMethod.GET, "/field-dictionary/**").hasAnyRole("ANALYST", "ADMIN")
.requestMatchers(HttpMethod.GET, "/complex-rules/**").hasAnyRole("ANALYST", "ADMIN")
.requestMatchers(HttpMethod.POST, "/rules/validate").hasAnyRole("ANALYST", "ADMIN")
.requestMatchers(HttpMethod.POST, "/rules/lint").hasAnyRole("ANALYST", "ADMIN")

// ADMIN only
.requestMatchers(HttpMethod.POST, "/rules/simulate").hasRole("ADMIN")
.requestMatchers("/homolog/**").hasRole("ADMIN")
.requestMatchers("/rules/**").hasRole("ADMIN")
.requestMatchers("/complex-rules/**").hasRole("ADMIN")
```

---

## Próximas Evidências
- [ ] Flyway migration output
- [ ] Backend test results
- [ ] Frontend test results
- [ ] E2E test results
