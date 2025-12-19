# Homologação Bancária (HML) — RULEX

Este diretório contém os artefatos formais para evidência e condução de homologação (HML).

## Arquivos
- [Inventário de API](api-inventory.md)
- [Inventário de Regras](rule-inventory.md)
- [Matriz de Cobertura](coverage-matrix.md)
- [Relatório de Prontidão HML](hml-readiness-report.md)
- [Checklist de Homologação](hml-checklist.md)

## Escopo
O projeto possui duas superfícies de API:
- **API Java (Spring Boot)** sob `http://<host>:<port>/api/*`.
- **API Node (Express + tRPC)** sob `http://<host>:<port>/api/trpc/*` e callback OAuth em `GET /api/oauth/callback`.

A homologação deve considerar ambas, salvo decisão explícita em contrário.
