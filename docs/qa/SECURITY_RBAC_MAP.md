# SECURITY RBAC MAP - RULEX

## Fonte
Extraído de: `backend/src/main/java/com/rulex/config/SecurityConfig.java`

---

## 1. Roles Definidos

| Role | Descrição |
|------|-----------|
| `ROLE_ADMIN` | Acesso total (CRUD, simulação, aprovação) |
| `ROLE_ANALYST` | Acesso somente leitura + validação/lint |

---

## 2. Configuração de Usuários

**Arquivo:** `RulexSecurityProperties.java`

```java
@ConfigurationProperties(prefix = "rulex.security")
public record RulexSecurityProperties(
    boolean enabled,
    User admin,
    User analyst
) {
    public record User(String username, String password) {}
}
```

**Exemplo de configuração (.env ou application.properties):**
```properties
rulex.security.enabled=true
rulex.security.admin.username=admin
rulex.security.admin.password=admin123
rulex.security.analyst.username=analyst
rulex.security.analyst.password=analyst123
```

---

## 3. Matriz de Permissões

### 3.1 Endpoints Públicos (sem autenticação)
| Endpoint | Método | Justificativa |
|----------|--------|---------------|
| `/transactions/analyze` | POST | API de avaliação em tempo real |
| `/transactions/analyze-advanced` | POST | API de avaliação avançada |
| `/evaluate` | POST | API de avaliação simplificada |
| `/actuator/health/**` | GET | Health checks (K8s/containers) |

### 3.2 Endpoints ANALYST + ADMIN (leitura)
| Endpoint | Método | Descrição |
|----------|--------|-----------|
| `/transactions/**` | GET | Listar/buscar transações |
| `/rules/**` | GET | Listar/buscar regras |
| `/audit/**` | GET | Logs de auditoria |
| `/metrics/**` | GET | Métricas |
| `/field-dictionary/**` | GET | Catálogo de campos |
| `/complex-rules/**` | GET | Regras complexas (leitura) |
| `/rules/validate` | POST | Validar regra |
| `/rules/lint` | POST | Lint de regra |

### 3.3 Endpoints ADMIN Only (escrita)
| Endpoint | Método | Descrição |
|----------|--------|-----------|
| `/rules/simulate` | POST | Simular regra |
| `/homolog/**` | ALL | Homologação completa |
| `/rules/**` | POST/PUT/DELETE | CRUD de regras |
| `/complex-rules/**` | POST/PUT/DELETE | CRUD de regras complexas |
| `/**` (default) | ALL | Qualquer outro endpoint |

---

## 4. Código de Configuração

```java
@Bean
SecurityFilterChain securityFilterChain(HttpSecurity http, RulexSecurityProperties props)
    throws Exception {

  http.csrf(csrf -> csrf.disable());

  if (!props.enabled()) {
    http.authorizeHttpRequests(auth -> auth.anyRequest().permitAll());
    return http.build();
  }

  http.authorizeHttpRequests(
      auth ->
          auth
              // Public endpoints
              .requestMatchers(HttpMethod.POST, "/transactions/analyze").permitAll()
              .requestMatchers(HttpMethod.POST, "/transactions/analyze-advanced").permitAll()
              .requestMatchers(HttpMethod.POST, "/evaluate").permitAll()
              .requestMatchers("/actuator/health/**").permitAll()

              // Read-only access for analysts
              .requestMatchers(HttpMethod.GET, "/transactions/**").hasAnyRole("ANALYST", "ADMIN")
              .requestMatchers(HttpMethod.GET, "/rules/**").hasAnyRole("ANALYST", "ADMIN")
              .requestMatchers(HttpMethod.GET, "/audit/**").hasAnyRole("ANALYST", "ADMIN")
              .requestMatchers(HttpMethod.GET, "/metrics/**").hasAnyRole("ANALYST", "ADMIN")
              .requestMatchers(HttpMethod.GET, "/field-dictionary/**").hasAnyRole("ANALYST", "ADMIN")
              .requestMatchers(HttpMethod.GET, "/complex-rules/**").hasAnyRole("ANALYST", "ADMIN")
              .requestMatchers(HttpMethod.POST, "/rules/validate").hasAnyRole("ANALYST", "ADMIN")
              .requestMatchers(HttpMethod.POST, "/rules/lint").hasAnyRole("ANALYST", "ADMIN")

              // Admin-only
              .requestMatchers(HttpMethod.POST, "/rules/simulate").hasRole("ADMIN")
              .requestMatchers("/homolog/**").hasRole("ADMIN")
              .requestMatchers("/rules/**").hasRole("ADMIN")
              .requestMatchers("/complex-rules/**").hasRole("ADMIN")
              .anyRequest().authenticated())
      .httpBasic(withDefaults());

  return http.build();
}
```

---

## 5. Testes de RBAC

### 5.1 Teste 401 (não autenticado)
```bash
# Deve retornar 401
curl -X GET http://localhost:8080/api/rules
```

### 5.2 Teste 403 (ANALYST tentando escrita)
```bash
# Deve retornar 403
curl -X POST http://localhost:8080/api/rules \
  -u analyst:analyst123 \
  -H "Content-Type: application/json" \
  -d '{"ruleName": "TEST"}'
```

### 5.3 Teste 200 (ADMIN com escrita)
```bash
# Deve retornar 200/201
curl -X POST http://localhost:8080/api/rules \
  -u admin:admin123 \
  -H "Content-Type: application/json" \
  -d '{"ruleName": "TEST_RULE", "ruleType": "SECURITY", ...}'
```

### 5.4 Teste público
```bash
# Deve retornar 200 sem autenticação
curl -X POST http://localhost:8080/api/evaluate \
  -H "Content-Type: application/json" \
  -d '{"externalTransactionId": "TXN-001", ...}'
```

---

## 6. Gaps Identificados

### P1: Falta de granularidade
- Não há role intermediário (ex: APPROVER para aprovar regras)
- Não há separação entre regras simples e complexas

### P2: Auditoria de acesso
- Não há log de quem acessou o quê
- Não há rate limiting por role

### P3: Frontend
- Frontend deve tratar 401/403 com UX adequada
- Redirect para login em 401
- Mensagem de permissão negada em 403

---

## 7. Recomendações

1. **Adicionar role APPROVER** para workflow de aprovação
2. **Implementar audit log** de acessos
3. **Rate limiting** por role/endpoint
4. **Frontend**: Ocultar botões de ação para ANALYST
5. **Tokens JWT** em vez de Basic Auth para produção
