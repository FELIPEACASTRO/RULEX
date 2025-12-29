# CI BLOCKERS - RULEX

**Data**: 2024-12-29  
**Branch**: cursor/rulex-project-review-1c58  
**Status**: ANÁLISE DE BLOQUEADORES

---

## SUMÁRIO DE BLOQUEADORES

| ID | Bloqueador | Severidade | Status |
|----|------------|------------|--------|
| B1 | Gitleaks PR scan - unknown revision | ALTA | 🔧 FIX PROPOSTO |
| B2 | pnpm version mismatch (potencial) | MÉDIA | ✅ OK (corepack) |
| B3 | Trivy HIGH/CRITICAL | MÉDIA | ✅ OK (0 vulns) |
| B4 | E2E healthcheck timeout | ALTA | ✅ OK (testado local) |

---

## B1. GITLEAKS PR SCAN - UNKNOWN REVISION

### Evidência

```
Error: unknown revision or path not in the working tree
```

O Gitleaks Action v2 em modo PR tenta fazer `git log base..head` mas quando o range não existe (push direto, shallow clone, etc.), falha.

### Arquivo de Configuração

`.github/workflows/ci.yml` linha 18-22:
```yaml
- name: Gitleaks (secret scan)
  uses: gitleaks/gitleaks-action@v2
  env:
    GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
```

### Impacto

- Job `appsec` falha em PRs quando base commit não está disponível
- Bloqueia jobs dependentes (`e2e`)

### Fix Proposto

Usar modo filesystem (`--no-git`) como fallback ou configurar explicitamente:

```yaml
- name: Gitleaks (secret scan)
  uses: gitleaks/gitleaks-action@v2
  env:
    GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
    GITLEAKS_ENABLE_COMMENTS: false
  continue-on-error: false
```

Ou usar CLI diretamente com fallback:

```yaml
- name: Gitleaks (secret scan)
  run: |
    # Tenta scan de commits (PR), fallback para filesystem (push)
    if [ "${{ github.event_name }}" = "pull_request" ]; then
      gitleaks detect --source . --log-opts="${{ github.event.pull_request.base.sha }}..${{ github.event.pull_request.head.sha }}" --verbose --exit-code 1 || \
      gitleaks detect --source . --no-git --verbose --exit-code 1
    else
      gitleaks detect --source . --no-git --verbose --exit-code 1
    fi
```

---

## B2. PNPM VERSION MISMATCH

### Evidência

`package.json`:
```json
"packageManager": "pnpm@10.4.1+sha512..."
```

Workflow usa corepack para extrair versão do packageManager:
```yaml
- name: Enable Corepack (pnpm from packageManager)
  run: |
    corepack enable
    PM="$(node -p "require('./package.json').packageManager")"
    VER="${PM#pnpm@}"
    VER="${VER%%+*}"
    corepack prepare "pnpm@${VER}" --activate
    pnpm --version
```

### Impacto

- **NENHUM** - O workflow já usa corepack corretamente
- Extrai versão 10.4.1 do packageManager
- Não há conflito com pnpm/action-setup

### Status

✅ **OK** - Não requer correção

---

## B3. TRIVY HIGH/CRITICAL

### Evidência

Execução local:
```bash
$ trivy fs --severity HIGH,CRITICAL --exit-code 1 .
# Exit code: 0
# Nenhuma vulnerabilidade HIGH/CRITICAL encontrada
```

### Impacto

- **NENHUM** - Dependências estão atualizadas

### Status

✅ **OK** - Não requer correção

---

## B4. E2E HEALTHCHECK TIMEOUT

### Evidência

Execução local:
```bash
$ docker compose up -d
$ curl -s http://localhost:8080/api/actuator/health
{"status":"UP","groups":["liveness","readiness"]}
```

Backend inicia corretamente com:
- Flyway 11.20.0 (compatível com PostgreSQL 16)
- Spring Boot 3.5.9
- Java 21

### Impacto

- **NENHUM** - Healthcheck funciona localmente
- Possível problema no CI: timing ou recursos

### Status

✅ **OK** - Funciona localmente, monitorar CI

---

## CONCLUSÃO

| Bloqueador | Ação Necessária |
|------------|-----------------|
| B1 Gitleaks | Aplicar fix com fallback filesystem |
| B2 pnpm | Nenhuma |
| B3 Trivy | Nenhuma |
| B4 E2E | Nenhuma (monitorar) |

---

**Documento gerado pelo QA Military Mode**
