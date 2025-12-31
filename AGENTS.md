# AGENTS.md - Guia para Agentes de IA no RULEX

## 1. Comandos Reais do Repositório

### 1.1 Frontend (React/Vite)
```bash
# Instalar dependências
cd ~/repos/RULEX && pnpm install

# Type check (lint)
cd ~/repos/RULEX && pnpm check

# Testes unitários
cd ~/repos/RULEX && pnpm test

# Testes com coverage
cd ~/repos/RULEX && pnpm test:coverage

# Build
cd ~/repos/RULEX && pnpm build

# Dev server
cd ~/repos/RULEX && pnpm dev

# E2E (requer Docker)
cd ~/repos/RULEX && pnpm e2e
```

### 1.2 Backend (Java/Spring Boot)
```bash
# Instalar dependências
cd ~/repos/RULEX/backend && mvn dependency:resolve

# Compilar
cd ~/repos/RULEX/backend && mvn compile

# Testes
cd ~/repos/RULEX/backend && mvn test

# Build
cd ~/repos/RULEX/backend && mvn package -DskipTests

# Lint (Spotless)
cd ~/repos/RULEX/backend && mvn spotless:check

# Formatar código
cd ~/repos/RULEX/backend && mvn spotless:apply
```

### 1.3 Docker/Ambiente Completo
```bash
# Subir ambiente
cd ~/repos/RULEX && cp .env.example .env && docker compose up -d --build

# Derrubar ambiente
cd ~/repos/RULEX && docker compose down -v

# Logs
cd ~/repos/RULEX && docker compose logs -f backend
```

### 1.4 Validação Completa
```bash
# Script de validação (se existir)
cd ~/repos/RULEX && ./scripts/validate.sh

# Validação manual
cd ~/repos/RULEX && pnpm check && pnpm test && cd backend && mvn test
```

---

## 2. Convenções

### 2.1 Commits
- Formato: `type(scope): description`
- Types: `feat`, `fix`, `docs`, `test`, `refactor`, `chore`
- Exemplos:
  - `feat(geo): implement GEO_DISTANCE_LT operator`
  - `fix(validation): handle null values in BETWEEN operator`
  - `docs: update EXTREME_CAPABILITIES_MAP`

### 2.2 Branches
- Feature: `feat/description`
- Fix: `fix/description`
- Docs: `docs/description`

### 2.3 Code Style
- **Frontend**: Prettier + ESLint (via `pnpm format`)
- **Backend**: Spotless (via `mvn spotless:apply`)

---

## 3. Política de Testes

### 3.1 Regra de Ouro
**NÃO fazer merge/commit sem todos os testes passando.**

### 3.2 Cobertura Mínima
- Backend: 80%+ em services críticos
- Frontend: 70%+ em componentes de formulário

### 3.3 Tipos de Teste
- **Unit**: Lógica isolada (operadores, validadores)
- **Integration**: API + DB
- **E2E**: Fluxos completos via Playwright

---

## 4. Política de Documentação

### 4.1 Arquivos Obrigatórios
- `/docs/EXTREME_CAPABILITIES_MAP.md` - Capacidades reais
- `/docs/RULES_SCHEMA_AND_FIELDS.md` - Schema de regras
- `/DEVIN_RUNBOOK.md` - Plano de execução
- `/DEVIN_GAPS.md` - Lista de gaps
- `/DEVIN_COMMAND_LOG.md` - Log de comandos

### 4.2 Regra
Toda mudança significativa deve atualizar a documentação correspondente.

---

## 5. Estrutura do Projeto

```
RULEX/
├── backend/                    # Java/Spring Boot
│   ├── src/main/java/com/rulex/
│   │   ├── controller/         # REST Controllers
│   │   ├── service/            # Business Logic
│   │   ├── entity/             # JPA Entities
│   │   ├── repository/         # Data Access
│   │   └── dto/                # Data Transfer Objects
│   └── src/main/resources/
│       └── db/migration/       # Flyway migrations
├── client/                     # React/Vite
│   └── src/
│       ├── pages/              # Page components
│       ├── components/         # Reusable components
│       └── lib/                # Utilities
├── docs/                       # Documentation
├── e2e/                        # Playwright tests
└── scripts/                    # Utility scripts
```

---

## 6. Endpoints Principais

### 6.1 Regras Simples
- `GET /api/rules` - Listar
- `POST /api/rules` - Criar
- `PUT /api/rules/{id}` - Atualizar
- `DELETE /api/rules/{id}` - Deletar

### 6.2 Regras Complexas
- `GET /api/v1/complex-rules` - Listar
- `POST /api/v1/complex-rules` - Criar
- `PUT /api/v1/complex-rules/{id}` - Atualizar
- `DELETE /api/v1/complex-rules/{id}` - Deletar
- `POST /api/v1/complex-rules/validate` - Validar

### 6.3 Avaliação
- `POST /api/evaluate` - Avaliar transação
- `POST /api/transactions/analyze` - Analisar transação

---

## 7. Restrições Críticas

### 7.1 Payload Imutável
**NUNCA alterar TransactionRequest.** Features derivadas devem ser calculadas internamente.

### 7.2 Git Limpo
**SEMPRE manter git status clean.** Fazer commit de todas as mudanças.

### 7.3 Testes Passando
**NUNCA deixar testes falhando.** Corrigir antes de prosseguir.
