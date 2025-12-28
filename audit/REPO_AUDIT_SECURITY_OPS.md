# RULEX — Auditoria (Segurança + Operação)

Data: 2025-12-26

Escopo
- Backend Java (Spring Boot) + frontend Vite/React + Docker Compose + arquivos de ambiente.
- Objetivo: identificar riscos operacionais e de segurança **com evidência em arquivos**.

## 1) Achados de alto impacto (prioridade)

### A1 — Credenciais default em exemplos (risco de “copiar e esquecer”)
Evidência
- `.env.example` define `POSTGRES_PASSWORD=postgres` e Basic Auth `admin:rulex` / `analyst:rulex`.
- `backend/src/main/resources/application-dev.yml` também possui defaults `rulex` para admin/analyst.

Risco
- Se `.env.example` ou o profile `dev` for usado fora de dev (HML/PROD), o ambiente pode ficar com credenciais triviais.

Mitigação recomendada
- Manter `.env.example` com valores “placeholder” (ex.: `change-me`) e reforçar isso no README.
- Para `application-dev.yml`, considerar defaults mais seguros ou exigir override explícito em runtime (se o objetivo é reduzir risco em ambientes compartilhados).

### A2 — Endpoints de análise estão públicos mesmo com security habilitada
Evidência
- `backend/src/main/java/com/rulex/config/SecurityConfig.java`: quando `rulex.security.enabled=true`, os endpoints abaixo são `permitAll()`:
  - `POST /transactions/analyze`
  - `POST /transactions/analyze-advanced`
  - `POST /evaluate`

Risco
- Superfície de ataque maior (abuso/DoS), já que endpoints de processamento podem ser chamados sem autenticação.

Mitigação recomendada
- Se a intenção é “demo only”, documentar explicitamente.
- Se a intenção é produção, exigir autenticação ou aplicar rate limiting/quotas/gateway.

### A3 — Frontend sugere JWT/OAuth, mas backend expõe Basic Auth (possível desalinhamento)
Evidência
- `client/src/_core/hooks/useAuth.ts` chama endpoints `GET /api/auth/me`, `POST /api/auth/refresh`, `POST /api/auth/logout` e usa Bearer token.
- Não foram encontrados controllers/rotas correspondentes em `backend/src/main/java/**` para `/auth`.
- `backend/src/main/java/com/rulex/config/SecurityConfig.java` configura **HTTP Basic** com usuários em memória.
- `replit.md` menciona “JWT_SECRET required in production”.

Risco
- Fluxo de login/token pode estar “meio implementado” ou divergente por versão.

Mitigação recomendada
- Definir claramente o modo suportado:
  - (A) Apenas Basic Auth (simples/HML) → remover/ocultar fluxo JWT no frontend.
  - (B) JWT/OAuth real → implementar endpoints `/api/auth/*` no backend e alinhar CORS, roles e refresh.

## 2) Controles existentes (pontos positivos)

### P1 — Segurança falha-fechada no profile base
Evidência
- `backend/src/main/resources/application.yml` deixa `rulex.security.admin.password` e `rulex.security.analyst.password` vazios por default.
- `SecurityConfig.userDetailsService(...)` lança `IllegalStateException` se `rulex.security.enabled=true` e credenciais não estiverem preenchidas.

Efeito
- Ajuda a evitar “subir em PROD sem senha” (desde que não use o profile `dev`).

### P2 — Actuator com exposição mínima
Evidência
- `backend/src/main/resources/application.yml` expõe apenas `management.endpoints.web.exposure.include: health`.

## 3) CORS

Evidência
- `backend/src/main/java/com/rulex/config/CorsConfig.java` permite origens via `rulex.cors.allowed-origins` (CSV), com default `http://localhost:5173,http://localhost:3000`.

Notas
- Permite `allowedHeaders("*")` e métodos comuns.
- Não habilita `allowCredentials(true)`, o que é ok para `Authorization` header e sem cookies.

Recomendação
- Em HML/PROD, setar `rulex.cors.allowed-origins` para domínios reais e evitar curingas.

## 4) Docker / Operação

### O1 — Portas expostas no docker-compose
Evidência
- `docker-compose.yml` mapeia:
  - Postgres `5432:5432`
  - Backend `8080:8080`
  - Web `5173:5173`

Risco
- Em host compartilhado, expõe serviços diretamente.

Mitigação recomendada
- Para HML/PROD, preferir rede interna e expor apenas o necessário (ex.: via reverse proxy/gateway).

### O2 — Dockerfiles são “dev-friendly”, não “prod-hardening”
Evidência
- `Dockerfile.web` roda `pnpm vite --host 0.0.0.0` (dev server) e não faz build estático.
- `backend/Dockerfile` roda como usuário default (root), sem hardening extra.

Mitigação recomendada
- Separar imagens de dev vs prod (build estático do frontend + user não-root no backend).

## 5) Itens para decisão (não-bloqueantes, mas importantes)

- Confirmar política: `POST /transactions/analyze` deve ser público ou autenticado?
- Escolher oficialmente Basic Auth vs JWT/OAuth (ou ambos, com roteamento claro e documentação).
- Remover defaults “rulex/postgres” dos exemplos para reduzir risco de rollout inseguro.

---

Arquivos-chave auditados
- `.env.example`, `.env.hml.example`
- `docker-compose.yml`
- `backend/src/main/resources/application.yml`, `application-dev.yml`, `application-prod.yml`
- `backend/src/main/java/com/rulex/config/SecurityConfig.java`, `CorsConfig.java`
- `client/src/_core/hooks/useAuth.ts`, `client/src/lib/javaApi.ts`, `client/src/components/Map.tsx`
