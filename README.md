# RULEX — Antifraude (homologável)

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
