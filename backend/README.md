# Backend (Spring Boot / Java 21)

## Requisitos

- Java 21
- Maven
- Postgres (local) **ou** Docker (recomendado)

## Subir via Docker Compose (recomendado)

Na raiz do repositório:

- `cp .env.example .env`
- `docker compose up --build`

Serviços:

- Backend: `http://localhost:8080/api`
- Healthcheck: `http://localhost:8080/api/actuator/health`

## Rodar local (sem Docker)

1) Suba um Postgres e configure:

- `SPRING_DATASOURCE_URL`
- `SPRING_DATASOURCE_USERNAME`
- `SPRING_DATASOURCE_PASSWORD`

2) (Opcional) Neo4j:

- Para habilitar integração com grafos, defina `RULEX_NEO4J_PASSWORD` (e opcionalmente `RULEX_NEO4J_URI` e `RULEX_NEO4J_USERNAME`).
- Se `RULEX_NEO4J_PASSWORD` não estiver definido, a integração Neo4j é desabilitada automaticamente.

3) Configure autenticação (HTTP Basic) se `RULEX_SECURITY_ENABLED=true`:

- `RULEX_ADMIN_USERNAME` / `RULEX_ADMIN_PASSWORD`
- `RULEX_ANALYST_USERNAME` / `RULEX_ANALYST_PASSWORD`

4) Execute:

- `mvn -q clean spring-boot:run`

## Testes

- **Unit + IT (Testcontainers)**: `mvn -q test`
  - Se não houver Docker disponível, os testes de integração com Testcontainers ficam **skipped** automaticamente.

### Windows: erro AccessDenied no `ComplexRuleEvaluator.class`

Se ocorrer erro de bloqueio de arquivo durante `mvn test`, use o script:

- `scripts/run-backend-tests.ps1` (recompila e executa testes)
- `scripts/run-backend-tests-clean.ps1` (cópia para diretório temporário)

O primeiro força recompilação do `ComplexRuleEvaluator.class`. O segundo copia o backend para um diretório temporário e executa os testes fora do workspace para evitar locks.

## Migrations (Flyway)

As migrations ficam em `src/main/resources/db/migration/` e rodam no startup (ou em testes) via Flyway.

