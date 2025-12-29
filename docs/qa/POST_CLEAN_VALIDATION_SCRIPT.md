# POST CLEAN VALIDATION SCRIPT - RULEX

**Data**: 2024-12-29  
**Branch**: cursor/rulex-project-review-1c58

---

## OBJETIVO

Script automatizado para validar o estado do repositório após qualquer alteração.
Deve ser executado antes de qualquer merge/deploy.

---

## LOCALIZAÇÃO

```
scripts/validate.sh
```

---

## PRÉ-REQUISITOS

| Ferramenta | Versão Mínima | Verificação |
|------------|---------------|-------------|
| Java | 21 | `java -version` |
| Maven | 3.6+ | `mvn -v` |
| Node.js | 22+ | `node -v` |
| pnpm | 10+ | `pnpm -v` |
| Docker | 24+ | `docker -v` |
| Gitleaks | 8.18+ | `gitleaks version` |
| Trivy | 0.50+ | `trivy -v` |
| Playwright | (via pnpm) | `pnpm exec playwright -v` |

---

## USO

### Linux/macOS

```bash
cd ~/repos/RULEX
./scripts/validate.sh
```

### Windows (PowerShell)

```powershell
cd C:\repos\RULEX
# Usar WSL ou Git Bash
bash scripts/validate.sh
```

---

## ETAPAS EXECUTADAS

| # | Etapa | Comando | Exit Code |
|---|-------|---------|-----------|
| 1 | Backend Tests | `mvn -q test -Pcoverage` | 0 = pass |
| 2 | Frontend Tests | `pnpm test` | 0 = pass |
| 3 | TypeCheck | `pnpm check` | 0 = pass |
| 4 | Build | `pnpm build` | 0 = pass |
| 5 | Gitleaks | `gitleaks detect --source . --no-git` | 0 = pass |
| 6 | Trivy | `trivy fs --severity HIGH,CRITICAL` | 0 = pass |
| 7 | E2E | `docker compose up + playwright test` | 0 = pass |

---

## INTERPRETAÇÃO DOS RESULTADOS

### Exit Codes

| Code | Significado |
|------|-------------|
| 0 | ✅ Todos os checks passaram |
| 1 | ❌ Algum check P0/P1 falhou |

### Output

```
=== 1. BACKEND TESTS ===
✓ PASS: Backend Tests (JUnit)

=== 2. FRONTEND TESTS ===
✓ PASS: Frontend Tests (Vitest)

...

==============================================
  SUMMARY
==============================================
✓ ALL CHECKS PASSED
```

---

## LOGS

Os logs de cada etapa são salvos em `/tmp/`:

| Arquivo | Conteúdo |
|---------|----------|
| `/tmp/backend-test.log` | Output do Maven |
| `/tmp/frontend-test.log` | Output do Vitest |
| `/tmp/typecheck.log` | Output do tsc |
| `/tmp/build.log` | Output do Vite |
| `/tmp/gitleaks.log` | Output do Gitleaks |
| `/tmp/trivy.log` | Output do Trivy |
| `/tmp/e2e.log` | Output do Playwright |
| `/tmp/docker-up.log` | Output do Docker Compose |

---

## TROUBLESHOOTING

### Erro: Java não encontrado

```bash
export JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64
export PATH=$JAVA_HOME/bin:$PATH
```

### Erro: pnpm não encontrado

```bash
corepack enable
corepack prepare pnpm@10.4.1 --activate
```

### Erro: Docker não disponível

O script pula os testes E2E se Docker não estiver disponível.
Para executar E2E manualmente:

```bash
docker compose up -d --build
# Aguardar backend ficar healthy
curl http://localhost:8080/api/actuator/health
# Executar testes
PLAYWRIGHT_BASE_URL=http://localhost:5173 pnpm exec playwright test
# Limpar
docker compose down -v
```

---

## INTEGRAÇÃO COM CI

O script pode ser usado no CI como fallback ou validação adicional:

```yaml
- name: Run validation script
  run: ./scripts/validate.sh
```

---

## MANUTENÇÃO

Ao adicionar novos checks:

1. Adicionar etapa no script
2. Atualizar este documento
3. Garantir exit code correto (0 = pass, 1 = fail)

---

**Documento gerado pelo QA Military Mode**
