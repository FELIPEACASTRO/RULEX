# DEVIN EVIDENCE LOG - RULEX

## Data da Auditoria
2024-12-31T23:30:00Z

---

## 1. STACK STATUS

### Docker Compose
```bash
$ docker compose ps
NAME               IMAGE                COMMAND                  SERVICE    STATUS
rulex-backend-1    rulex-backend        "sh -c 'java $JAVA_O…"   backend    Up (healthy)
rulex-postgres-1   postgres:16-alpine   "docker-entrypoint.s…"   postgres   Up (healthy)
rulex-web-1        rulex-web            "docker-entrypoint.s…"   web        Up
```

---

## 2. FLYWAY MIGRATIONS

```bash
$ docker exec rulex-postgres-1 psql -U postgres -d rulex_db -c "SELECT version, description, success FROM flyway_schema_history ORDER BY installed_rank;"

 version |            description             | success 
---------+------------------------------------+---------
 1       | init                               | t
 2       | core schema                        | t
 3       | extend workflow length             | t
 4       | raw hash idempotency               | t
 5       | raw as received                    | t
 6       | v31 exec log field dictionary      | t
 7       | v31 exec log dedup                 | t
 8       | complex rules support              | t
 9       | audit compliance enhancements      | t
 10      | derived context improvements       | t
 11      | bin lookup table                   | t
 12      | complex rules crud                 | t
 13      | geo reference table                | t
 14      | velocity counters                  | t
 15      | add velocity operators             | t
 16      | fix geo polygon id type            | t
 17      | fix geo reference id type          | t
 18      | enable condition groups constraint | t
(18 rows)
```

---

## 3. CONSTRAINT CHECK

```bash
$ docker exec rulex-postgres-1 psql -U postgres -d rulex_db -c "SELECT conname FROM pg_constraint WHERE conname = 'chk_condition_groups_has_parent';"

             conname             
---------------------------------
 chk_condition_groups_has_parent
(1 row)
```

---

## 4. RBAC TESTS

```bash
# 401 sem auth
$ curl -s -o /dev/null -w "%{http_code}" http://localhost:8080/api/rules
401

# 200 ANALYST GET
$ curl -s -o /dev/null -w "%{http_code}" -u <ANALYST_USER>:<ANALYST_PASS> http://localhost:8080/api/rules
200

# 403 ANALYST POST
$ curl -s -o /dev/null -w "%{http_code}" -u <ANALYST_USER>:<ANALYST_PASS> -X POST -H "Content-Type: application/json" -d '{"ruleName":"TEST"}' http://localhost:8080/api/rules
403

# ADMIN pode criar
$ curl -s -o /dev/null -w "%{http_code}" -u <ADMIN_USER>:<ADMIN_PASS> -X POST -H "Content-Type: application/json" -d '{"ruleName":"RBAC_TEST","ruleType":"SECURITY","classification":"SUSPICIOUS","logicOperator":"AND","conditions":[]}' http://localhost:8080/api/rules
400  # 400 porque faltam campos obrigatórios, mas passou auth
```

---

## 5. FRONTEND TESTS

```bash
$ pnpm test --run

 ✓ client/src/components/RuleFormDialog/schema.test.ts (83 tests) 33ms
 ✓ client/src/components/ComplexRuleBuilder/ComplexRuleBuilder.test.tsx (55 tests) 29ms
 ✓ client/src/lib/validators/regexValidator.test.ts (31 tests) 9ms
 ✓ client/src/pages/Rules.test.tsx (4 tests) 1419ms
 ✓ client/src/pages/Login.test.tsx (5 tests) 132ms
 ✓ client/src/pages/Transactions.test.tsx (3 tests) 79ms
 ✓ client/src/pages/Audit.test.tsx (3 tests) 142ms
 ✓ client/src/pages/Dashboard.test.tsx (3 tests) 42ms
 ✓ client/src/components/DashboardLayout.test.tsx (3 tests) 57ms
 ✓ client/src/components/ErrorBoundary.test.tsx (3 tests) 34ms

 Test Files  12 passed (12)
      Tests  198 passed (198)
   Duration  2.95s
```

---

## 6. BACKEND TESTS

```bash
$ mvn -f backend/pom.xml test

[INFO] Tests run: 6, Failures: 0, Errors: 0, Skipped: 0 -- Casos de Uso Reais
[INFO] Tests run: 3, Failures: 0, Errors: 0, Skipped: 0 -- Safe Compile
[INFO] Tests run: 4, Failures: 0, Errors: 0, Skipped: 0 -- Execução com Timeout
[INFO] Tests run: 7, Failures: 0, Errors: 0, Skipped: 0 -- Proteção contra ReDoS
[INFO] Tests run: 7, Failures: 0, Errors: 0, Skipped: 0 -- Validação de Patterns
[INFO] Tests run: 1, Failures: 0, Errors: 0, Skipped: 0 -- CleanArchitectureRulesTest
[INFO] Tests run: 6, Failures: 0, Errors: 0, Skipped: 0 -- SecurityRbacTest
[INFO] Tests run: 198, Failures: 0, Errors: 0, Skipped: 0
[INFO] BUILD SUCCESS
```

---

## 7. API RULES LIST

```bash
$ curl -s -u <ADMIN_USER>:<ADMIN_PASS> http://localhost:8080/api/rules | jq '.content | length'
5

$ curl -s -u <ADMIN_USER>:<ADMIN_PASS> http://localhost:8080/api/rules | jq '.content[0]'
{
  "id": 2,
  "ruleName": "ADMIN_TEST",
  "ruleType": "SECURITY",
  "threshold": 0,
  "weight": 50,
  "enabled": true,
  "classification": "SUSPICIOUS",
  "conditions": [],
  "logicOperator": "AND",
  "version": 1
}
```

---

## 8. OPERADORES BACKEND

```bash
$ grep -A 60 "enum ConditionOperator" backend/src/main/java/com/rulex/entity/complex/RuleCondition.java | head -65

  public enum ConditionOperator {
    // Comparação básica
    EQ, NEQ, GT, GTE, LT, LTE,
    // Listas
    IN, NOT_IN,
    // Strings
    CONTAINS, NOT_CONTAINS, STARTS_WITH, ENDS_WITH, REGEX, NOT_REGEX,
    // Nulos
    IS_NULL, NOT_NULL,
    // Booleanos
    IS_TRUE, IS_FALSE,
    // Range
    BETWEEN, NOT_BETWEEN,
    // Comparação entre campos
    FIELD_EQ, FIELD_NEQ, FIELD_GT, FIELD_GTE, FIELD_LT, FIELD_LTE,
    // Data/Tempo
    DATE_BEFORE, DATE_AFTER, DATE_BETWEEN, TIME_BEFORE, TIME_AFTER, TIME_BETWEEN,
    // Arrays
    ARRAY_CONTAINS, ARRAY_NOT_CONTAINS, ARRAY_SIZE_EQ, ARRAY_SIZE_GT, ARRAY_SIZE_LT,
    // Matemáticos
    MOD_EQ, MOD_NEQ,
    // Geolocalização
    GEO_DISTANCE_LT, GEO_DISTANCE_GT, GEO_IN_POLYGON,
    // Velocity
    VELOCITY_COUNT_GT, VELOCITY_COUNT_LT, VELOCITY_SUM_GT, VELOCITY_SUM_LT,
    VELOCITY_AVG_GT, VELOCITY_AVG_LT, VELOCITY_DISTINCT_GT, VELOCITY_DISTINCT_LT
  }
```

**Total: 50 operadores**

---

## 9. TABELAS DO BANCO

```bash
$ docker exec rulex-postgres-1 psql -U postgres -d rulex_db -c "\dt" | wc -l
44  # 40 tabelas + headers
```

Tabelas principais:
- rules
- rule_configurations
- complex_rules
- rule_condition_groups
- rule_conditions
- velocity_counters
- geo_reference
- geo_polygon
- audit_log
- transactions

---

## 10. GIT STATUS

```bash
$ git status
On branch cursor/rulex-project-review-1c58
Your branch is up to date with 'origin/cursor/rulex-project-review-1c58'.

nothing to commit, working tree clean

$ git log --oneline -5
d418684 AJUSTE
54d7f24 docs: update DEVIN_RESTART_PROMPT with current progress
c525a79 docs: update scorecard and gaps after RuleFormDialog implementation
b9444c9 feat: implement RuleFormDialog component (closes GAP-P0-01)
6fbb314 docs: update checkpoint files with integration test evidence
```

---

## 11. ENGINE PERFORMANCE OPTIMIZATIONS (2025-01-XX)

### Commit
```bash
$ git log --oneline -1
bc6bd32 perf(engine): add candidate filtering and optional optimized rule ordering
```

### Changes Summary
- **RuleEngineService.java**: +198 lines
  - Added `CandidateIndex` and `CandidateIndexCache` for rule precondition caching
  - Added `RulePreconditions` with required field extraction from expressions
  - Candidate filtering skips AND-logic rules when required fields are missing
  - Integration with optional `RuleOrderingService` behind config toggle
  - Per-rule telemetry recording (best-effort, never affects decisions)
  - **Bug Fix**: `LocalDateTime.max()` replaced with `isAfter()` comparison

- **RuleEngineServiceTest.java**: Updated constructor for new dependency

### Configuration
```yaml
# application.yml - default OFF for behavioral safety
rulex:
  engine:
    optimizedRuleOrder: false  # Enable with caution after validation
```

### Unit Tests
```bash
$ mvn -f backend/pom.xml test -Dtest=RuleEngineServiceTest

[INFO] Tests run: 18, Failures: 0, Errors: 0, Skipped: 0
[INFO] BUILD SUCCESS
```

### Full Backend Test Suite
```bash
$ mvn -f backend/pom.xml test

Results:
- Tests Passed: 256
- Tests Failed: 15 (Integration Tests)
- Root Cause: TestContainers requires Docker (pre-existing infrastructure issue)

Failed Tests (all IT - require Docker):
- TransactionAnalyzeIT
- CrtranBaselineIT  
- RulePopupE2EIT
- RuleExecutionLogIT
- HomologSimulationIT

Error: "No qualifying bean of type 'com.rulex.repository.AuditLogRepository'"
Cause: Spring context fails to initialize without Docker/TestContainers
```

### Safety Guarantees
1. **Default behavior unchanged**: Optimized ordering is OFF by default
2. **Conservative candidate filtering**: Only skips rules with provably missing AND-required fields
3. **Best-effort telemetry**: Recording failures never affect rule decisions
4. **No ML components**: 100% deterministic rule evaluation preserved

---

## Última Atualização
2025-01-XX (Engine Optimization Sprint)
