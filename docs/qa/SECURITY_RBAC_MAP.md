# SECURITY RBAC MAP - RULEX

## Data da Auditoria
2024-12-31T23:10:00Z

---

## 1. CONFIGURAÇÃO DE SEGURANÇA

### Arquivo: `config/SecurityConfig.java`

```java
@Configuration
@EnableWebSecurity
@EnableConfigurationProperties(RulexSecurityProperties.class)
public class SecurityConfig {
  // Basic Auth com BCrypt
  // CSRF desabilitado (API stateless)
}
```

### Propriedades: `RulexSecurityProperties.java`

```java
@ConfigurationProperties(prefix = "rulex.security")
public record RulexSecurityProperties(
    boolean enabled,
    User admin,
    User analyst
) {}
```

---

## 2. ROLES DEFINIDAS

| Role | Descrição | Permissões |
|------|-----------|------------|
| ADMIN | Administrador | CRUD completo, simulação, homologação |
| ANALYST | Analista | Leitura, validação, lint |

---

## 3. CREDENCIAIS (Ambiente Dev)

| Usuário | Senha | Role |
|---------|-------|------|
| admin | rulex | ADMIN |
| analyst | rulex | ANALYST |

**Fonte:** `docker-compose.yml`
```yaml
RULEX_ADMIN_USERNAME: admin
RULEX_ADMIN_PASSWORD: rulex
RULEX_ANALYST_USERNAME: analyst
RULEX_ANALYST_PASSWORD: rulex
```

---

## 4. MATRIZ DE PERMISSÕES

### Endpoints Públicos (Sem Autenticação)
```java
.requestMatchers(HttpMethod.POST, "/transactions/analyze").permitAll()
.requestMatchers(HttpMethod.POST, "/transactions/analyze-advanced").permitAll()
.requestMatchers(HttpMethod.POST, "/evaluate").permitAll()
.requestMatchers("/actuator/health/**").permitAll()
```

### Endpoints ANALYST (Leitura)
```java
.requestMatchers(HttpMethod.GET, "/transactions/**").hasAnyRole("ANALYST", "ADMIN")
.requestMatchers(HttpMethod.GET, "/rules/**").hasAnyRole("ANALYST", "ADMIN")
.requestMatchers(HttpMethod.GET, "/audit/**").hasAnyRole("ANALYST", "ADMIN")
.requestMatchers(HttpMethod.GET, "/metrics/**").hasAnyRole("ANALYST", "ADMIN")
.requestMatchers(HttpMethod.GET, "/field-dictionary/**").hasAnyRole("ANALYST", "ADMIN")
.requestMatchers(HttpMethod.GET, "/complex-rules/**").hasAnyRole("ANALYST", "ADMIN")
```

### Endpoints ANALYST (Validação)
```java
.requestMatchers(HttpMethod.POST, "/rules/validate").hasAnyRole("ANALYST", "ADMIN")
.requestMatchers(HttpMethod.POST, "/rules/lint").hasAnyRole("ANALYST", "ADMIN")
```

### Endpoints ADMIN Only
```java
.requestMatchers(HttpMethod.POST, "/rules/simulate").hasRole("ADMIN")
.requestMatchers("/homolog/**").hasRole("ADMIN")
.requestMatchers("/rules/**").hasRole("ADMIN")  // POST, PUT, DELETE
.requestMatchers("/complex-rules/**").hasRole("ADMIN")  // POST, PUT, DELETE
```

---

## 5. TESTES DE RBAC REALIZADOS

### Teste 1: 401 sem autenticação
```bash
$ curl -s -o /dev/null -w "%{http_code}" http://localhost:8080/api/rules
401
```
✅ **PASSOU**

### Teste 2: 200 ANALYST GET
```bash
$ curl -s -o /dev/null -w "%{http_code}" -u analyst:rulex http://localhost:8080/api/rules
200
```
✅ **PASSOU**

### Teste 3: 403 ANALYST POST
```bash
$ curl -s -o /dev/null -w "%{http_code}" -u analyst:rulex -X POST \
  -H "Content-Type: application/json" \
  -d '{"ruleName":"TEST"}' \
  http://localhost:8080/api/rules
403
```
✅ **PASSOU**

### Teste 4: 201 ADMIN POST
```bash
$ curl -s -o /dev/null -w "%{http_code}" -u admin:rulex -X POST \
  -H "Content-Type: application/json" \
  -d '{"ruleName":"RBAC_TEST","ruleType":"SECURITY","classification":"SUSPICIOUS","logicOperator":"AND","conditions":[]}' \
  http://localhost:8080/api/rules
201
```
✅ **PASSOU** (ou 400 se campos obrigatórios faltando)

---

## 6. TRATAMENTO DE ERROS NO FRONTEND

### Arquivo: `pages/Rules.tsx`

```typescript
// P1-07: Mensagens de erro amigáveis
if (error.message.includes('400')) {
  toast.error('Dados inválidos. Verifique os campos e tente novamente.');
} else if (error.message.includes('401') || error.message.includes('403')) {
  toast.error('Você não tem permissão para realizar esta ação.');
} else if (error.message.includes('500')) {
  toast.error('Erro interno do servidor. Tente novamente mais tarde.');
}
```

✅ **Implementado**

---

## 7. VULNERABILIDADES CONHECIDAS

| Vulnerabilidade | Status | Mitigação |
|-----------------|--------|-----------|
| Basic Auth em produção | ⚠️ | Usar JWT/OAuth2 |
| Sem Rate Limiting | ⚠️ | Implementar throttling |
| CSRF desabilitado | ✅ | API stateless, OK |
| ReDoS | ✅ | RegexValidator implementado |

---

## 8. RECOMENDAÇÕES

### P1 - Importante
1. Implementar rate limiting (429 Too Many Requests)
2. Adicionar audit log de acessos (quem acessou o quê)

### P2 - Desejável
1. Migrar para JWT/OAuth2
2. Implementar MFA para ADMIN
3. Adicionar IP whitelist para endpoints sensíveis

---

## Última Atualização
2024-12-31T23:10:00Z
