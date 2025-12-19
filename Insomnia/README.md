# Insomnia — Projeto de Homologação (RULEX)

Este diretório contém um export do Insomnia com requests organizados para evidência de homologação.

## Como importar
1) Insomnia → `Application` → `Preferences` → `Data` → `Import Data`
2) Selecione o arquivo: `rulex-hml.insomnia.json`

## Environments
Ajuste as variáveis do environment `Base`:
- `java_api_url` (ex.: `http://localhost:8080/api`)
- `node_base_url` (ex.: `http://localhost:3000`)
- `session_cookie` (opcional, cookie `COOKIE_NAME` do Node para endpoints protected/admin)

Variáveis usadas no fluxo de Homolog (UUIDs):
- `homolog_rule_id`: **ruleId** retornado em `Create RuleVersion`
- `homolog_rule_version_id`: **ruleVersionId** retornado em `Create RuleVersion`
- `ruleset_version_id`: **ruleSetVersionId** retornado em `Create RuleSet draft`

## Dados base
- Payload base de transação: `fixtures/crtran.json`
  - Use-o como referência para montar o body de `/transactions/analyze`.

## Execução (ordem sugerida)
1) `Java API / Transactions / Analyze (idempotent #1)`
2) `Java API / Transactions / Analyze (idempotent #2)`
3) `Java API / Transactions / List + filtros` e `GET by id/external`
4) `Java API / Rules` CRUD + toggle + history
5) `Java API / Audit` list + filtros (inclui datas inválidas)
6) `Java API / Metrics` endpoints
7) `Java API / Homolog` create/publish/activate/simulate
8) `Node tRPC` procedures públicas
9) `Node tRPC` protected/admin (se houver cookie + role)
