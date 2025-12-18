# Busca e Evidências (rastreamento)

## Limitação operacional nesta execução
Este repo/ambiente fornece leitura de arquivos locais e fetch de URLs específicas, mas **não fornece um mecanismo de busca geral na web** (search engine) para executar “busca devastadora” multi-camada do jeito solicitado.

Portanto, este registro separa:
1) **Fontes efetivamente analisadas dentro do repositório** (auditável), e
2) **Plano de queries e categorias** para executar assim que você fornecer URLs-alvo ou habilitar um mecanismo de busca.

## 1) Fontes efetivamente analisadas (no repo)
- [PESQUISA_FRAUDES_BRASIL.md](PESQUISA_FRAUDES_BRASIL.md) — notas 2024-2025 (menciona FEBRABAN/BCB/DataSenado/CommerceGate)
- [DOUBLE_CHECK_RIGOROSO_60_REGRAS.md](DOUBLE_CHECK_RIGOROSO_60_REGRAS.md) — validação de regras vs payload (com gaps e impossibilidades)
- [TRIPLE_CHECK_ANALISE_RIGOROSA.md](TRIPLE_CHECK_ANALISE_RIGOROSA.md) — inventário de 103 campos e oportunidades
- [DOCUMENTACAO_TECNICA.md](DOCUMENTACAO_TECNICA.md) — exemplos de request/response e regras-base
- [openapi/rulex.yaml](openapi/rulex.yaml) — contrato canônico atual

## 2) Queries planejadas (para busca web aberta)
> Estas queries são **apenas o log do plano**; não foram executadas aqui por falta de mecanismo de busca.

### Reguladores / oficiais (Brasil e global)
- "Banco Central do Brasil fraude Pix relatório 2024" 
- "BCB segurança Pix golpes engenharia social 2025" 
- "FEBRABAN relatório golpes bancários 2024" 
- "BIS fraud prevention card payments guidance" 
- "FATF guidance mule accounts 2024" 
- "ENISA payment fraud threat landscape 2024 2025" 

### Academia
- "SIM swap account takeover payment fraud detection 2024" 
- "3DS ECI CAVV fraud signals study" 
- "merchant category code fraud risk empirical" 

### Threat intelligence (defensivo)
- "payment fraud report 2024 vendor" 
- "account takeover report banking 2025" 

## 3) Como cada evidência vira regra dura
- Converter apenas sinais observáveis do payload em condições determinísticas.
- Declarar explicitamente quando um padrão exige campos ausentes (ex.: device fingerprint) ou histórico (velocity).
- Parametrizar thresholds de forma conservadora, começando por pisos baseados em domínio e ajustando por validação operacional.
