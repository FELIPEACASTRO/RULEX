# RULEX — Antifraude (homologável)

## Arquitetura utilizada

Este repositório é um **multi-serviço** (via `docker compose`) com:

- **Web**: Vite/React (UI)
- **Server Node**: API/integrações (tRPC/Express)
- **Backend Java**: Spring Boot (domínio de antifraude/homologação) + Postgres

No backend Java, o recorte de “homologação” foi estruturado seguindo **Clean Architecture** (com *ports/adapters*), para manter a lógica de aplicação testável e desacoplada de frameworks.

## Abstração, acoplamento, extensibilidade e coesão

- **Abstração**: contratos em `com.rulex.homolog.port.*` definem o que o core precisa (persistência, auditoria, JSON, sanitização, avaliação de DSL).
- **Acoplamento**: *use cases* em `com.rulex.homolog.usecase.*` não dependem de Spring/Jackson/repos; dependem apenas das portas.
- **Extensibilidade**: para trocar detalhes (ex.: auditoria, armazenamento, serialização), cria-se um novo adapter implementando a porta, sem reescrever use cases.
- **Coesão**: cada classe tem uma responsabilidade clara (use case = regra do fluxo; adapter = integração; application service = transação).

## Análise assintótica (Big O)

O caminho crítico típico é a **simulação** de um *RuleSet*:

- Seja $n$ o número de itens (regras) no *RuleSet*.
- Seja $m$ o número de nós/condições na expressão (árvore) de uma regra.

Complexidade aproximada:

- **Avaliar regras**: $O(n \cdot m)$ (cada regra avalia uma lógica/árvore de condições).
- **Persistência**: leituras/gravações principais são $O(n)$ em número de itens (ex.: carregar versões + itens + registrar decisões), desconsiderando custo interno do banco.

Observação: o custo real depende também do tamanho do JSON de condições e do payload sanitizado; a implementação atual privilegia determinismo e segurança.

## Design Patterns

- **Ports & Adapters (Hexagonal)**: `com.rulex.homolog.port.*` + `com.rulex.homolog.adapter.*`.
- **Adapter**: adapters Spring implementam as portas e delegam para repositórios/serviços/framework.
- **Facade / Application Service**: `com.rulex.homolog.application.*` define a fronteira transacional e expõe um API coesa para os controllers.

## Microservices Patterns

O projeto **não está estruturado como microserviços independentes** (é um repo com múltiplos serviços executáveis). Por isso, padrões como **CQRS** e **SAGA** não são aplicados formalmente aqui.

Ainda assim, há um conceito semelhante a **ACL (Anti‑Corruption Layer)** no backend Java: as portas/adapters evitam que detalhes de Spring/JPA/Jackson “contaminem” o core da aplicação.

## Clean Architecture

Camadas (backend Java, recorte de homologação):

- **Interface/Delivery**: controllers REST (`com.rulex.controller.*`).
- **Application**: serviços transacionais (`com.rulex.homolog.application.*`).
- **Use Cases**: casos de uso puros (`com.rulex.homolog.usecase.*`).
- **Ports**: contratos (`com.rulex.homolog.port.*`).
- **Adapters/Infra**: integrações e persistência (`com.rulex.homolog.adapter.*`, `com.rulex.repository.*`).

Validação automática: existe teste ArchUnit para impedir dependências proibidas no core.

## Clean Code

- **Formatação automática**: Spotless (google-java-format) no backend Java.
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
