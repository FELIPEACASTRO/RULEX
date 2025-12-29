# RULEX — Antifraude (Java + UI)

Este repositório contém:

- **Backend**: Spring Boot (Java 21) em `backend/`
- **Web UI**: Vite/React em `client/` (proxy para o backend via `/api`)
- **Banco**: Postgres (via Docker Compose)

## Subir local (Docker)

Pré-requisito: Docker Desktop.

- `docker compose up --build`

Serviços:

- Web (Vite): http://localhost:5173
- Backend (Spring Boot): http://localhost:8080
- Postgres: `localhost:5432` (db `rulex_db`, user/pass `postgres`)

## Backend (dev/local sem Docker)

Pré-requisitos: Java 21 + Maven + Postgres.

- Testes: `mvn -q clean test` (executar em `backend/`)

## Web (dev/local)

Pré-requisitos: Node + pnpm.

- Instalar deps: `pnpm install`
- Rodar UI: `pnpm dev`

A UI usa proxy de `/api` para o backend (configurado em `vite.config.ts`).

### Autenticação (local/homolog)

- O backend usa **HTTP Basic**.
- A UI suporta duas formas:
  - **Via env**: setar `VITE_API_BASIC_AUTH=usuario:senha` (ver `.env.example`)
  - **Via tela**: acessar `http://localhost:5173/login` e informar usuário/senha (salva no `localStorage`)

- **Build estrito**: compilação com `-Werror` e lint (`-Xlint:all,-processing`) no backend.
- **Logs/erros**: mensagens em PT‑BR e sem vazamento de dados sensíveis no retorno ao cliente.

## SOLID

- **S**: classes de use case focam em fluxo/regra; adapters focam em integração.
- **O**: portas permitem extensões sem alterar o core.
- **L**: adapters são substituíveis via interface.
- **I**: portas são pequenas e específicas.
- **D**: use cases dependem de abstrações (`port`), não de implementações.

## Testes de unidade e integração

- **Integração (backend Java)**: `HomologSimulationIT` usa **Testcontainers (Postgres)** para validar o fluxo real (persistência + endpoints).
- **Unidade**: quando necessário, use cases podem ser testados com *stubs/mocks* das portas (sem subir Spring).

## E2E (UI)

Pré-requisito: Docker (para subir o stack via `docker compose`).

- `pnpm e2e`

Comandos úteis (backend):

- `mvn -q clean test`
- `mvn -q clean verify`

## Cobertura de testes (Code Coverage)

O backend Java suporta geração de relatório de cobertura via **JaCoCo** (profile opcional):

- `mvn -q -Pcoverage test`

O relatório HTML fica em `backend/target/site/jacoco/index.html`.

## Documentação

- Este README: visão geral + critérios de homologação + tópicos de arquitetura.
- Backend: detalhes técnicos em `backend/README.md`.

## Subir local (Docker)

Pré-requisitos:
- Docker Desktop

Comando:
- `docker compose up --build`

Serviços:
- Web (Vite): http://localhost:5173
- Backend (Spring Boot): http://localhost:8080
- Postgres: `localhost:5432` (user/pass/db definidos no `docker-compose.yml`)

## Backend (dev/local sem Docker)

Pré-requisitos:
- Java 21
- Maven
- Postgres (ou usar o container do compose)

Rodar testes (recomendado):
- `mvn -q clean test`

## Endpoints de homologação (MVP)

Headers opcionais:
- `X-Actor-Email: admin@rulex.local` (resolve para o usuário seed da migration)

### Regras (criação/versões)
- `POST /api/homolog/rules` cria regra + versão 1 (DRAFT)
- `GET /api/homolog/rules/{ruleId}/latest` busca última versão
- `POST /api/homolog/rules/versions/{ruleVersionId}/publish` publica versão
- `POST /api/homolog/rules/{ruleId}/rollback/{version}` cria novo DRAFT copiando a versão alvo

### RuleSets (publicar/ativar)
- `POST /api/homolog/rulesets` cria ruleset + versão 1 (DRAFT) com itens
- `POST /api/homolog/rulesets/versions/{ruleSetVersionId}/publish` publica versão
- `POST /api/homolog/rulesets/activate` ativa versão publicada

### Simulação
- `POST /api/homolog/simulations/run` executa avaliação determinística e persiste:
  - `simulation_runs`
  - `decision_log`
  - `audit_log`

Obs: o payload é mascarado (`pan`) antes de persistir/logar.

## Banco / Migrations

As tabelas de homologação são criadas via Flyway:
- `backend/src/main/resources/db/migration/V1__init.sql`

O seed mínimo inclui:
- roles: `ADMIN`, `ANALYST`
- user: `admin@rulex.local`
