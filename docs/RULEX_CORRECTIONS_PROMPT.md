# 🎯 RULEX - Prompt de Correções para Devin

## 📋 Contexto do Projeto

**RULEX** é um sistema de detecção de fraudes em transações financeiras com:
- **Backend:** Java 21 + Spring Boot 3.x + PostgreSQL + Neo4j
- **Frontend:** React 19 + TypeScript + Vite
- **Arquivos Java:** 329 classes
- **Branch:** `cursor/rulex-project-review-1c58`

---

## ⚠️ REGRAS IMPORTANTES

1. **SEMPRE** executar `mvn compile -q` após mudanças no backend
2. **SEMPRE** executar `pnpm check` após mudanças no frontend
3. **SEMPRE** manter git limpo (`git status` deve mostrar working tree clean)
4. **NUNCA** remover migrations Flyway (apenas código Java)
5. **COMMITAR** cada correção separadamente com mensagem descritiva

---

## 🚀 FASE 1: LIMPEZA DE CÓDIGO MORTO (Prioridade: ALTA)

### Objetivo
Remover 19 arquivos Java órfãos que não são utilizados por nenhum serviço.

### Arquivos para Remover

```bash
cd ~/repos/RULEX/backend

# 1. Entities órfãos (6 arquivos)
rm -f src/main/java/com/rulex/entity/PanLocationHistory.java
rm -f src/main/java/com/rulex/entity/VelocityMetrics.java
rm -f src/main/java/com/rulex/entity/BloomFilterMetadata.java
rm -f src/main/java/com/rulex/entity/ShadowEvaluationLog.java
rm -f src/main/java/com/rulex/entity/RuleAbTestAssignment.java
rm -f src/main/java/com/rulex/entity/RuleAbTest.java

# 2. Repositories órfãos (6 arquivos)
rm -f src/main/java/com/rulex/repository/PanLocationHistoryRepository.java
rm -f src/main/java/com/rulex/repository/VelocityMetricsRepository.java
rm -f src/main/java/com/rulex/repository/BloomFilterMetadataRepository.java
rm -f src/main/java/com/rulex/repository/ShadowEvaluationLogRepository.java
rm -f src/main/java/com/rulex/repository/RuleAbTestAssignmentRepository.java
rm -f src/main/java/com/rulex/repository/RuleAbTestRepository.java

# 3. Sistema Roles órfão - homolog (4 arquivos)
rm -f src/main/java/com/rulex/entity/homolog/RoleEntity.java
rm -f src/main/java/com/rulex/entity/homolog/UserRoleEntity.java
rm -f src/main/java/com/rulex/repository/homolog/RoleRepository.java
rm -f src/main/java/com/rulex/repository/homolog/UserRoleRepository.java

# 4. Utilitário órfão (1 arquivo)
rm -f src/main/java/com/rulex/util/SensitiveDataMasker.java
```

### Validação
```bash
cd ~/repos/RULEX/backend
mvn compile -q  # Deve compilar sem erros
mvn test -q     # Testes devem passar
```

### Commit
```bash
git add -A
git commit -m "chore(cleanup): remover 19 arquivos Java órfãos

CLEAN-001: Remoção de código morto identificado na auditoria.

Removidos:
- 6 entities não utilizadas (PanLocationHistory, VelocityMetrics, etc.)
- 6 repositories correspondentes
- 4 arquivos do sistema de roles homolog não integrado
- 1 utilitário SensitiveDataMasker não utilizado

As tabelas no banco permanecem (migrations preservadas)."
```

---

## 🔒 FASE 2: CORREÇÕES DE SEGURANÇA (Prioridade: CRÍTICA)

### SEC-001: Remover Credenciais Default

**Arquivo:** `docker-compose.yml`

**Localizar e substituir:**
```yaml
# DE:
RULEX_ADMIN_PASSWORD: ${RULEX_ADMIN_PASSWORD:-rulex}
RULEX_ANALYST_PASSWORD: ${RULEX_ANALYST_PASSWORD:-rulex}
NEO4J_PASSWORD: ${NEO4J_PASSWORD:-rulex123}
POSTGRES_PASSWORD: ${POSTGRES_PASSWORD:-postgres}

# PARA:
RULEX_ADMIN_PASSWORD: ${RULEX_ADMIN_PASSWORD:?RULEX_ADMIN_PASSWORD é obrigatório}
RULEX_ANALYST_PASSWORD: ${RULEX_ANALYST_PASSWORD:?RULEX_ANALYST_PASSWORD é obrigatório}
NEO4J_PASSWORD: ${NEO4J_PASSWORD:?NEO4J_PASSWORD é obrigatório}
POSTGRES_PASSWORD: ${POSTGRES_PASSWORD:?POSTGRES_PASSWORD é obrigatório}
```

**Criar arquivo `.env.example`:**
```bash
cat > .env.example << 'EOF'
# Copie este arquivo para .env e preencha os valores
# cp .env.example .env

# Credenciais do RULEX (OBRIGATÓRIAS)
RULEX_ADMIN_PASSWORD=SuaSenhaForteAqui123!
RULEX_ANALYST_PASSWORD=SuaSenhaForteAqui456!

# Banco de Dados
POSTGRES_PASSWORD=SuaSenhaPostgres789!
NEO4J_PASSWORD=SuaSenhaNeo4j012!

# Opcional
HIKARI_MAX_POOL_SIZE=20
HIKARI_MIN_IDLE=5
EOF
```

**Commit:**
```bash
git add docker-compose.yml .env.example
git commit -m "fix(security): SEC-001 remover credenciais default

- Substituir fallbacks por variáveis obrigatórias
- Criar .env.example com template de configuração
- Docker Compose agora falha se variáveis não estiverem definidas"
```

---

### SEC-004: Validação de Senha em Todos os Profiles

**Arquivo:** `backend/src/main/java/com/rulex/config/SecurityConfig.java`

**Localizar método `validatePasswordStrength` e substituir a lógica:**

```java
// DE:
private void validatePasswordStrength(String password, String userType, String profile) {
    boolean isProduction = "prod".equalsIgnoreCase(profile) || "production".equalsIgnoreCase(profile);
    if (WEAK_PASSWORDS.contains(password.toLowerCase())) {
        if (isProduction) {
            throw new IllegalStateException(...);
        } else {
            log.warn(...);
        }
    }
}

// PARA:
private void validatePasswordStrength(String password, String userType, String profile) {
    // Profiles onde senhas fracas são PERMITIDAS (apenas desenvolvimento local)
    Set<String> allowWeakPasswordProfiles = Set.of("dev", "local", "test");
    boolean allowWeakPasswords = allowWeakPasswordProfiles.contains(profile.toLowerCase());
    
    if (WEAK_PASSWORDS.contains(password.toLowerCase())) {
        if (allowWeakPasswords) {
            log.warn("⚠️ Senha fraca detectada para {} em profile {}. Permitido apenas em dev.", userType, profile);
        } else {
            throw new IllegalStateException(
                String.format("Senha fraca não permitida para %s em profile %s. Use senha forte.", userType, profile)
            );
        }
    }
}
```

**Validação e Commit:**
```bash
cd ~/repos/RULEX/backend
mvn compile -q
git add src/main/java/com/rulex/config/SecurityConfig.java
git commit -m "fix(security): SEC-004 validar senha em todos os profiles exceto dev/local/test"
```

---

### SEC-007: Restringir CORS Headers

**Arquivo:** `backend/src/main/java/com/rulex/config/CorsConfig.java`

**Localizar e substituir:**
```java
// DE:
.allowedHeaders("*")

// PARA:
.allowedHeaders(
    "Content-Type",
    "Authorization",
    "X-Requested-With",
    "X-XSRF-TOKEN",
    "Accept",
    "Origin",
    "Cache-Control"
)
```

**Commit:**
```bash
git add src/main/java/com/rulex/config/CorsConfig.java
git commit -m "fix(security): SEC-007 restringir CORS headers permitidos"
```

---

## ⚡ FASE 3: CORREÇÕES DE PERFORMANCE (Prioridade: ALTA)

### PERF-001: Ajustar Pool HikariCP

**Arquivo:** `backend/src/main/resources/application.yml`

**Localizar seção `hikari` e ajustar:**
```yaml
# DE:
hikari:
  maximum-pool-size: ${HIKARI_MAX_POOL_SIZE:100}
  minimum-idle: ${HIKARI_MIN_IDLE:20}

# PARA:
hikari:
  # Valores otimizados: (core_count * 2) + effective_spindle_count
  maximum-pool-size: ${HIKARI_MAX_POOL_SIZE:20}
  minimum-idle: ${HIKARI_MIN_IDLE:5}
  connection-timeout: ${HIKARI_CONNECTION_TIMEOUT:30000}
  idle-timeout: ${HIKARI_IDLE_TIMEOUT:600000}
  max-lifetime: ${HIKARI_MAX_LIFETIME:1800000}
  leak-detection-threshold: ${HIKARI_LEAK_DETECTION:60000}
```

**Commit:**
```bash
git add src/main/resources/application.yml
git commit -m "perf(db): PERF-001 otimizar pool HikariCP (100->20 conexões)"
```

---

## 🏗️ FASE 4: MELHORIAS DE INFRAESTRUTURA (Prioridade: MÉDIA)

### INFRA-001: Habilitar Neo4j Health Check

**Arquivo:** `backend/src/main/resources/application.yml`

**Localizar e modificar:**
```yaml
# DE:
management:
  health:
    neo4j:
      enabled: false

# PARA:
management:
  health:
    neo4j:
      enabled: true
  endpoint:
    health:
      show-details: when_authorized
      group:
        readiness:
          include: db, neo4j
        liveness:
          include: ping
```

**Commit:**
```bash
git add src/main/resources/application.yml
git commit -m "fix(infra): INFRA-001 habilitar Neo4j health check"
```

---

## 📝 FASE 5: QUALIDADE DE CÓDIGO (Prioridade: BAIXA)

### QUAL-005: Documentar Endpoints Deprecated

**Arquivo:** `backend/src/main/java/com/rulex/controller/EvaluateController.java`

**Localizar `@Deprecated` e adicionar documentação:**
```java
// DE:
@Deprecated
@PostMapping("/raw")
public ResponseEntity<EvaluateResponse> evaluateRaw(...)

// PARA:
/**
 * @deprecated Desde v2.0. Use {@link #evaluate(TransactionRequest)} em vez disso.
 *             Este endpoint será removido na versão 3.0 (estimado Q2 2026).
 *             Migração: Altere o path de /evaluate/raw para /evaluate
 *             e ajuste o payload para TransactionRequest.
 */
@Deprecated(since = "2.0", forRemoval = true)
@PostMapping("/raw")
public ResponseEntity<EvaluateResponse> evaluateRaw(...)
```

**Commit:**
```bash
git add src/main/java/com/rulex/controller/EvaluateController.java
git commit -m "docs(api): QUAL-005 documentar migration path para endpoints deprecated"
```

---

## ✅ CHECKLIST FINAL

Após completar todas as fases, execute:

```bash
cd ~/repos/RULEX

# 1. Verificar backend
cd backend
mvn clean compile -q
mvn test -q
echo "✅ Backend OK"

# 2. Verificar frontend
cd ..
pnpm check
pnpm test --run
echo "✅ Frontend OK"

# 3. Verificar git está limpo
git status
echo "✅ Git limpo"

# 4. Push das mudanças
git push origin cursor/rulex-project-review-1c58
echo "✅ Push realizado"
```

---

## 📊 RESUMO DAS CORREÇÕES

| Fase | Issue | Descrição | Tempo Est. |
|------|-------|-----------|------------|
| 1 | CLEAN-001 | Remover 19 arquivos órfãos | 30 min |
| 2 | SEC-001 | Remover credenciais default | 30 min |
| 2 | SEC-004 | Validar senha em mais profiles | 20 min |
| 2 | SEC-007 | Restringir CORS headers | 15 min |
| 3 | PERF-001 | Otimizar pool HikariCP | 15 min |
| 4 | INFRA-001 | Habilitar Neo4j health check | 15 min |
| 5 | QUAL-005 | Documentar deprecated | 15 min |
| **TOTAL** | **7 issues** | | **~2.5 horas** |

---

## 🚫 NÃO FAZER NESTA SESSÃO

As seguintes correções são **complexas demais** e devem ser feitas em sessões separadas:

1. **SEC-002** (Basic Auth → httpOnly cookies) - Requer mudança de arquitetura
2. **SEC-003** (Habilitar CSRF) - Requer mudanças em frontend e backend
3. **ARCH-001** (Refatorar ComplexRuleEvaluator) - 9214 linhas, muito arriscado
4. **ARCH-003** (Consolidar 5 engines) - Refatoração massiva

---

## 📞 EM CASO DE ERRO

Se algum comando falhar:

1. **Erro de compilação:** Verificar se removeu arquivo que era usado
   ```bash
   mvn compile 2>&1 | grep "cannot find symbol"
   ```

2. **Teste falhando:** Verificar qual teste falhou
   ```bash
   mvn test 2>&1 | grep -A5 "FAILURE"
   ```

3. **Reverter última mudança:**
   ```bash
   git checkout -- .
   ```

---

*Prompt otimizado para Devin - Gerado em: 2026-01-19*
*Baseado na auditoria RULEX com 103 issues identificadas*
