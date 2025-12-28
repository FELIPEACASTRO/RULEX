# RULEX — Deliverables Audit (Hard Rules Pack)

Data: 2025-12-26

Escopo
- Este relatório audita **apenas** o pacote de entregáveis (DSL + catálogos YAML + fixtures + especificação de testes), conforme decisão: **“entregáveis externos”** (não assumir que o backend Java atual executa estes YAMLs).

## 1) Resumo executivo

O pack está bem estruturado e consistente em termos de **formato** (campos essenciais por regra, IDs únicos, prioridade, `reason/evidence/tags`).

Principais achados:
- **Divergência de contagem**: os cabeçalhos/README declaram **55 regras DQ** e **32 regras Fraud**, porém os arquivos atuais contêm **49 DQ** e **19 Fraud** (total **68** regras).
- O pack depende de **campos sintéticos internos** (`__contract.*`, `__rawstore.*`, `__history.*`) que **precisam ser produzidos pelo consumidor** (engine/harness) para as regras funcionarem.
- Há **placeholders de validação** (ex.: `__YYYYMMDD__`, `__LENGTH_13_19_DIGITS__`) que exigem implementação do validador real.
- A pasta `tests/java/*.java` contém **testes de integração do backend** (SpringBootTest + TestRestTemplate), o que conflita com a interpretação “entregáveis externos” se não for explicitamente documentado.

## 2) Contagem real de regras (vs. declarado)

Fonte: contagem por documento YAML (`---`) e por ocorrência de `id:`.

| Arquivo | Declarado (comentário/README) | Contagem real (docs/ids) |
|---|---:|---:|
| `rules/dq_rules.yaml` | 55 | 49 |
| `rules/fraud_rules.yaml` | 32 | 19 |

Artefato de contagem: `RULEX/audit/rule_counts.json`.

Impacto
- Se “55/32” for uma exigência contratual da entrega, **faltam regras**.
- Se os YAMLs forem a fonte de verdade, então os cabeçalhos/README estão **desatualizados**.

## 3) Dependências e campos sintéticos (obrigatórios)

As regras dependem de campos que **não existem** no payload CRTRAN25 “puro”. O consumidor deve fornecer:

### 3.1 `__contract.*` (contrato/parse/strict)
- `__contract.unknownFields`: lista/campo sinalizando propriedades desconhecidas.
- `__contract.jsonParseError`: sinalizador/objeto quando o JSON falha no parse.

Uso típico: regras DQ de contrato (`DQ_UNKNOWN_FIELD_FATAL`, `DQ_JSON_PARSE_FATAL`).

### 3.2 `__rawstore.*` (anti-tamper/idempotência)
- `__rawstore.hashMismatch`
- `__rawstore.previousHash`
- `__rawstore.newHash`

Uso típico: `FR_ANTI_TAMPER_PAYLOAD_HASH_MISMATCH`.

### 3.3 `__history.*` (velocidade)
- `__history.txCount_5min`
- `__history.txCount_1h`
- `__history.amountSum_1h`
- `__history.mccCount_24h`

Uso típico: regras `FR_VEL_*`.

Recomendação
- Documentar explicitamente a **fonte** e a **semântica** desses campos (janela temporal, unidade monetária, chave de agregação — PAN/customerId etc.).

## 4) Placeholders de validação

Encontrados no catálogo DQ:
- `__YYYYMMDD__` (formato de data)
- `__LENGTH_13_19_DIGITS__` (PAN)

Recomendação
- Explicitar no README a “assinatura” esperada do validador (ex.: regex ou função) e como o consumidor deve substituir/aplicar esses placeholders.

## 5) Regras: consistência estrutural

Checagens executadas (heurísticas):
- IDs únicos (`id:` não duplicado).
- Presença de campos essenciais por regra (`id`, `version`, `priority`, `when`, `then`, `reason`, `evidence`).
- Uso de operadores conforme DSL (`EQ/NE/GT/GE/LT/LE/IN_SET/NOT_IN_SET/MATCHES_REGEX/EXISTS/NOT_EXISTS`).

Resultado
- Não foram detectados erros estruturais no conjunto atual de regras.

## 6) Testes e boundary do pacote

O pack inclui:
- `tests/fixtures/*.json` e `tests/test_cases.yaml`: úteis como **spec externa**.
- `tests/java/*.java`: são **testes de integração do backend** (dependem de um app Spring rodando e endpoint específico).

Risco
- Misturar testes de integração do backend dentro do pack pode induzir o consumidor a achar que:
  - o backend atual “oficialmente” executa estes YAMLs; e/ou
  - o contrato de tipos/DTO do backend é o mesmo dos fixtures.

Recomendação (sem alterar código)
- No README do pack, declarar que `tests/java` é **opcional** e só aplicável para um **harness compatível**.
- Alternativamente, mover `tests/java` para um diretório claramente “backend-only” (ex.: `RULEX/backend/src/test/...`) — se isso estiver alinhado com a estratégia do repositório.

## 7) Checklist do consumidor (para produção)

- Implementar parser/loader da DSL v1 e a avaliação em `priority_desc`.
- Implementar política de conflito `BLOCK_WINS` e exceção por tag `ALLOWLIST_OVERRIDE`.
- Produzir os campos sintéticos `__contract.*`, `__rawstore.*`, `__history.*` com semântica documentada.
- Substituir/aplicar placeholders de validação.
- Definir a unidade e escala de `transactionAmount` e demais numéricos (ex.: centavos vs unidade monetária).
- Validar que `reason` e `evidence` sejam persistidos para auditoria.

---

Apêndice
- DSL: `dsl_schema.yaml`
- Catálogos: `rules/dq_rules.yaml`, `rules/fraud_rules.yaml`
- Contagens: `RULEX/audit/rule_counts.json`
