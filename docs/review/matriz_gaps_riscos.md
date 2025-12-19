# Matriz de Gaps e Riscos

## Gaps (Funcionalidades Ausentes ou Incompletas)

| ID | Severidade | Descrição | Área Afetada | Evidência |
|:---|:---:|:---|:---|:---|
| **G01** | **P0 (Crítico)** | **Ausência do Dataset `crtran.json`** <br> O arquivo base para testes de fraude e homologação não foi encontrado no repositório. Sem ele, não é possível validar a eficácia das regras contra cenários reais conhecidos. | QA / Negócio | `glob: crtran.json` -> 0 results |
| **G02** | **P1 (Alto)** | **Falta de Unificação dos Motores de Regras** <br> Existem dois endpoints separados (`/analyze` e `/analyze-advanced`). O frontend e clientes da API precisam "adivinhar" qual chamar ou chamar ambos. Não há um orquestrador central. | Arquitetura | `TransactionController.java` |
| **G03** | **P1 (Alto)** | **Invisibilidade das Regras Avançadas no Frontend** <br> As 28 regras avançadas implementadas no backend não são visíveis, listáveis ou configuráveis na interface administrativa `Rules.tsx`. | Produto / UX | `client/src/pages/Rules.tsx` |
| **G04** | **P2 (Médio)** | **Scripts de Migração de Banco** <br> Ausência de ferramentas como Flyway/Liquibase. Dependência de DDL Auto pode causar problemas em produção. | DBA / DevOps | Ausência de pasta `db/migration` ou similar. |

---

## Riscos (Problemas Potenciais)

| ID | Probabilidade | Impacto | Descrição | Mitigação |
|:---|:---:|:---:|:---|:---|
| **R01** | Alta | Alto | **Performance de Queries de Agregação** <br> Regras como `checkCardCaptureFraud` e `checkVelocityConsolidated` fazem `count` direto na tabela de transações (`transactionRepository.count...`). Com milhões de linhas, isso travará o banco na validação síncrona. | Implementar tabelas de pré-agregação, Redis ou janelas de tempo deslizantes assíncronas. |
| **R02** | Média | Alto | **Divergência de Decisão** <br> Uma transação pode ser Aprovada no `/analyze` mas seria Fraude no `/analyze-advanced`. Como não rodam juntos, o sistema pode aprovar fraude indevidamente. | Unificar a chamada dos serviços dentro de um único Facade. |
| **R03** | Alta | Médio | **Hardcoding de Parâmetros de Negócio** <br> Thresholds (ex: valor > 5000, 3 tentativas) estão "chumbados" no código Java (`AdvancedRuleEngineService`). Mudanças exigem recompilação e deploy. | Extrair parâmetros para banco de dados (`RuleConfiguration`). |

