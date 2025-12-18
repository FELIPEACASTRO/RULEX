# Arquitetura Alvo (Refatoração Completa) — RULEX

Este documento descreve uma arquitetura alvo **coesa** para o RULEX, reduzindo duplicidade (dois backends / dois bancos / dois modelos de regras) e tornando o sistema apto a operar com consistência, auditoria e manutenção previsível.

## 1) Objetivo

- **Uma fonte de verdade** para regras, decisões e auditoria.
- **Contratos estáveis** entre UI e API (sem “shapes” divergentes).
- **Motor de regras determinístico** baseado em regras persistidas, com rastreabilidade (quem mudou o quê, qual versão rodou, quais regras dispararam).
- **Segurança mínima de produção** (CORS restrito, segredos obrigatórios, cookies consistentes, proteção de dados sensíveis).

## 2) Decisão Arquitetural (alvo recomendado)

**Java/Spring Boot como Core + PostgreSQL como banco único**.

- O backend Java passa a ser o **serviço principal** (core) para:
  - análise de transações
  - execução do motor de regras
  - auditoria
  - métricas
  - CRUD/versionamento de regras
- O backend Node/Express+tRPC vira:
  - **(Opção 1)** BFF (backend-for-frontend) apenas para autenticação/OAuth e agregação (se realmente necessário);
  - **(Opção 2)** removido após migração (preferível para reduzir superfície operacional).

> Observação: Esta arquitetura reduz drasticamente risco de divergência e simplifica governança.

## 3) Componentes (alvo)

### 3.1 Frontend (React)
Responsável por:
- simular transações
- listar decisões/auditoria/métricas
- criar/editar regras

Comunicação:
- chamadas REST para o Core Java
- (opcional) chamadas para BFF de auth

### 3.2 Core API (Java/Spring Boot)
Responsável por:
- validar payload
- normalizar dados
- buscar regras ativas
- executar motor
- persistir transação + decisão + auditoria
- responder com detalhes do resultado

### 3.3 Banco de dados (PostgreSQL)
Responsável por:
- persistência única (transações, decisões, regras, histórico, auditoria)

### 3.4 BFF/Auth (Node) — opcional
Responsável por:
- OAuth callback e emissão de cookie/sessão
- (se mantido) *proxy* de requests autenticados para o Java

## 4) Fluxos (alvo)

### 4.1 Analisar transação
1. UI envia `AnalyzeTransactionRequest` para Java
2. Java valida e normaliza
3. Java carrega regras ativas
4. Java executa motor (determinístico)
5. Java grava `transaction`, `decision`, `audit_event`
6. Java retorna `AnalyzeTransactionResponse`

### 4.2 CRUD de regras (com versionamento)
1. UI cria/edita regra
2. Java valida schema de regra (condições, operadores, campos)
3. Java salva regra e cria evento de histórico (diff + autor)
4. Java incrementa `version` e registra `effective_from` (opcional)

### 4.3 Auditoria
- Toda ação relevante gera um evento append-only em `audit_events`.

## 5) Contratos (contract-first)

### 5.1 Definição canônica
- **OpenAPI** como contrato canônico.
- Geração automática de:
  - tipos/clients no frontend (TS)
  - DTOs/validações no backend Java (quando aplicável)

### 5.2 Contratos mínimos

#### AnalyzeTransactionRequest (exemplo conceitual)
- `externalTransactionId: string`
- `customerId: string`
- `merchantId?: string`
- `amount: number` (em centavos)
- `currencyCode: string|number`
- `...` (demais campos)

#### AnalyzeTransactionResponse (exemplo conceitual)
- `transactionId: string`
- `classification: APPROVED | SUSPICIOUS | FRAUD`
- `riskScore: number` (0–100)
- `triggeredRules: Array<{ name: string; weight: number; contribution: number; detail?: string }>`
- `reason: string`
- `rulesetVersion: string` (ou `rulesVersion`)
- `processingTimeMs: number`
- `timestamp: string`

> Importante: o contrato deve bater com o que o frontend realmente usa.

## 6) Motor de regras (alvo)

### 6.1 Modelo de regra (persistido)
Uma regra “genérica” (persistida) deve conter:
- `name` (único)
- `category`
- `classification` (quando disparar)
- `weight` (0–100)
- `conditions[]` (JSON)
- `logicOperator` (AND/OR)
- `isActive`
- `version`
- `source` (opcional)

### 6.2 Avaliador
O motor deve interpretar condições do tipo:
- operadores: `==`, `!=`, `>`, `<`, `>=`, `<=`, `IN`, `NOT_IN`, `CONTAINS`, `NOT_CONTAINS`
- coerção de tipo: string/number/boolean/date-time (definir regras)

### 6.3 Cálculo de score
- Somar contribuições de cada regra disparada
- Normalizar para 0–100
- Classificar por faixas (ex.: <30 aprovado, 30–69 suspeito, >=70 fraude)

### 6.4 Resultado detalhado
Sempre retornar:
- quais regras dispararam
- por que dispararam (campo/operador/valor)
- contribuição e score total

## 7) Segurança/Compliance (requisitos mínimos)

- **Segredos obrigatórios**: API não sobe com `JWT_SECRET` vazio.
- **CORS**: sem `*` em produção; permitir apenas origens conhecidas.
- **Dados sensíveis**:
  - não armazenar PAN em claro
  - armazenar apenas token/mascara (ex.: `411111******1111`) ou criptografar
- **Cookies**:
  - `SameSite=None` apenas com `Secure=true`
  - considerar `SameSite=Lax` quando viável

## 8) Migração (estratégia)

### 8.1 “Strangler” por endpoints
- Definir endpoints oficiais no Java e migrar tela por tela.
- Descontinuar gradualmente endpoints/rotas do Node para domínio de regras.

### 8.2 Banco único
- Migrar regras do MySQL/Drizzle (seeds) para Postgres (tabela `rules`/`rule_configurations` alvo).
- Congelar escrita no banco antigo (read-only) antes do corte.

## 9) Critérios de sucesso (DoD global)

- Frontend chama **um único contrato** para análise (sem simulação silenciosa em produção).
- Um único banco contém as entidades de regra/decisão/auditoria.
- Testes de integração validam: request → API → DB → response.
- Auditoria permite reconstruir decisão (regras, versões, score).
