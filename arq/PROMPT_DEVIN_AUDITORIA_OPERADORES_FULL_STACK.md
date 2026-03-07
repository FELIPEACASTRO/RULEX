# 🎯 PROMPT DEVIN — AUDITORIA COMPLETA DE OPERADORES E OPERAÇÕES RULEX

**Versão:** 1.0 — Baseado em análise direta do repositório (branch `cursor/rulex-project-review-1c58`)  
**Data:** Março 2026  
**Objetivo:** Verificar e garantir que todos os 469 operadores e operações do sistema RULEX existam de forma **consistente e funcional** em **todas as camadas** da solução: Frontend, Backend, PostgreSQL, Redis e Neo4j.

---

## 🚨 REGRAS ABSOLUTAS ANTI-ALUCINAÇÃO

> **ANTES DE QUALQUER AÇÃO, LEIA ESTAS REGRAS:**
>
> 1. **NUNCA invente métodos, classes, campos ou tabelas.** Tudo deve ser validado no código real.
> 2. **NUNCA assuma que algo existe.** Use `grep`, `find` e leitura de arquivos para confirmar.
> 3. **NUNCA modifique código sem antes entender o padrão existente.** Leia os 68+ métodos `evaluate*` antes de criar novos.
> 4. **NUNCA adicione dependências** ao `pom.xml` ou `package.json` sem verificar se já existem.
> 5. **SEMPRE valide compilação** após cada modificação: `mvn compile -DskipTests` e `pnpm run check`.
> 6. **SEMPRE siga os padrões existentes.** O projeto tem arquitetura modular com `OperatorEvaluatorRegistry`.
> 7. **NUNCA crie repositórios** dentro do `ComplexRuleEvaluator`. Ele usa apenas `GeoService`, `VelocityService` e `VelocityServiceFacade`.

---

## 📋 CONTEXTO DO PROJETO

### Stack Tecnológica
```
Frontend:  React 19 + TypeScript + Vite + TailwindCSS
Backend:   Java 21 + Spring Boot 3.x + Maven
Database:  PostgreSQL 16 (via Flyway migrations)
Cache:     Redis 7 (via RedisVelocityService + BloomFilterService)
Graph:     Neo4j 5 Community + GDS Plugin (via Neo4jGraphService)
```

### Estrutura do Repositório
```
RULEX/
├── backend/
│   ├── src/main/java/com/rulex/
│   │   ├── entity/complex/ConditionOperator.java          ← ENUM com 469 operadores
│   │   ├── service/complex/evaluator/                     ← 30+ avaliadores modulares
│   │   │   ├── OperatorEvaluatorRegistry.java             ← Registro central
│   │   │   ├── StubOperatorEvaluator.java                 ← Operadores não implementados
│   │   │   ├── VelocityOperatorEvaluator.java             ← Operadores de velocity
│   │   │   ├── GraphOperatorEvaluator.java                ← Operadores NEO4J_*
│   │   │   ├── FATFOperatorEvaluator.java                 ← Operadores FATF
│   │   │   ├── SCAOperatorEvaluator.java                  ← Operadores SCA/PSD2
│   │   │   ├── BaselOperatorEvaluator.java                ← Operadores Basel III
│   │   │   └── PlatformOperatorEvaluator.java             ← Operadores PLT_*
│   │   ├── service/Neo4jGraphService.java                 ← Queries Cypher para Neo4j
│   │   ├── service/RedisVelocityService.java              ← Operações Redis
│   │   ├── service/VelocityServiceFacade.java             ← Facade Redis/DB
│   │   └── controller/OperatorStatusController.java       ← API GET /api/operators/status
│   └── src/main/resources/
│       ├── db/migration/V51__sync_condition_operator_enum.sql  ← Sync PostgreSQL
│       └── neo4j/init.cypher                              ← Schema Neo4j
├── client/src/
│   ├── lib/operators.ts                                   ← 470 operadores no frontend
│   ├── lib/operatorTypes.ts                               ← Tipos TypeScript
│   ├── manual/OperatorCatalog.tsx                         ← Catálogo visual
│   └── manual/generated/backendOperators.generated.ts    ← Gerado automaticamente
└── docker-compose.yml                                     ← Stack completa
```

### Estado Atual (Validado por Análise Direta)
| Camada | Operadores | Status |
|--------|-----------|--------|
| Backend `ConditionOperator.java` | **469** | ✅ Fonte da verdade |
| Frontend `operators.ts` | **470** | ⚠️ +1 (definição de interface) |
| PostgreSQL `condition_operator` enum | **469** | ✅ Sincronizado via V51 |
| Redis (estruturas de dados) | N/A | ✅ Usado por operadores VELOCITY_* |
| Neo4j (queries Cypher) | **18** operadores NEO4J_* | ✅ Implementados |
| StubOperatorEvaluator (não implementados) | **0** | ✅ Todos implementados |

---

## 🎯 MISSÃO PRINCIPAL

Realizar uma **auditoria forense completa** de consistência de operadores em toda a stack, respondendo às seguintes perguntas com evidências de código:

### Pergunta 1: Consistência Frontend ↔ Backend
> Cada operador no `ConditionOperator.java` existe exatamente igual no `operators.ts` do frontend?  
> Cada operador no `operators.ts` existe no `ConditionOperator.java`?

### Pergunta 2: Consistência Backend ↔ PostgreSQL
> Cada operador no `ConditionOperator.java` existe no enum `condition_operator` do PostgreSQL?  
> A migration `V51__sync_condition_operator_enum.sql` está completa e atualizada?

### Pergunta 3: Consistência Backend ↔ Redis
> Cada operador que usa Redis (VELOCITY_*, BLOOM_FILTER_*, etc.) tem a estrutura de dados correspondente no `RedisVelocityService`?  
> Os prefixos de chave Redis estão documentados e consistentes?

### Pergunta 4: Consistência Backend ↔ Neo4j
> Cada operador `NEO4J_*` no enum tem implementação correspondente no `GraphOperatorEvaluator` e `Neo4jGraphService`?  
> As queries Cypher usam os labels e relacionamentos definidos no `init.cypher`?

### Pergunta 5: Consistência Implementação
> Cada operador no enum está registrado em algum `OperatorEvaluator` (não está "solto")?  
> O `StubOperatorEvaluator` está realmente vazio (todos implementados)?

---

## 📌 TAREFAS OBRIGATÓRIAS

### TAREFA 1: Gerar Mapa de Cobertura Completo

Execute os seguintes comandos e documente os resultados:

```bash
# 1.1 Contar operadores no enum Java
grep -c "^  [A-Z]" backend/src/main/java/com/rulex/entity/complex/ConditionOperator.java

# 1.2 Contar operadores no frontend
grep -c "value:" client/src/lib/operators.ts

# 1.3 Extrair operadores do enum Java
grep "^  [A-Z]" backend/src/main/java/com/rulex/entity/complex/ConditionOperator.java \
  | sed 's/[, ]//g' | sort > /tmp/backend_ops.txt

# 1.4 Extrair operadores do frontend
grep "value:" client/src/lib/operators.ts \
  | sed "s/.*value: '//;s/'.*//" | sort > /tmp/frontend_ops.txt

# 1.5 Encontrar diferenças
echo "=== No Backend mas NÃO no Frontend ===" 
comm -23 /tmp/backend_ops.txt /tmp/frontend_ops.txt

echo "=== No Frontend mas NÃO no Backend ===" 
comm -13 /tmp/backend_ops.txt /tmp/frontend_ops.txt

# 1.6 Verificar operadores sem avaliador registrado
grep -r "getSupportedOperators" backend/src/main/java/com/rulex/service/complex/evaluator/ \
  | grep -v "test\|Test" | wc -l

# 1.7 Verificar StubOperatorEvaluator
cat backend/src/main/java/com/rulex/service/complex/evaluator/StubOperatorEvaluator.java

# 1.8 Contar operadores NEO4J no enum vs implementados
grep "^  NEO4J_" backend/src/main/java/com/rulex/entity/complex/ConditionOperator.java | wc -l
grep "ConditionOperator.NEO4J_" backend/src/main/java/com/rulex/service/complex/evaluator/GraphOperatorEvaluator.java | wc -l
```

---

### TAREFA 2: Auditoria PostgreSQL

#### 2.1 Verificar sincronização do enum PostgreSQL

```bash
# Extrair operadores da migration V51
grep "ADD VALUE" backend/src/main/resources/db/migration/V51__sync_condition_operator_enum.sql \
  | sed "s/.*EXISTS '//;s/'.*//" | sort > /tmp/postgres_ops.txt

# Comparar com enum Java
comm -23 /tmp/backend_ops.txt /tmp/postgres_ops.txt
# → Resultado esperado: VAZIO (todos sincronizados)

comm -13 /tmp/backend_ops.txt /tmp/postgres_ops.txt
# → Resultado esperado: VAZIO
```

#### 2.2 Verificar se há operadores nas regras do banco que não existem no enum

```bash
# Verificar migrations de inserção de regras
grep -r "operator.*NEO4J_\|operator.*VELOCITY_\|operator.*SCA_" \
  backend/src/main/resources/db/migration/ | head -20
```

#### 2.3 Ação Corretiva (SE necessário)
Se houver operadores no enum Java que **não estão** na migration V51, criar migration nova:

```sql
-- Exemplo: V64__sync_new_operators.sql
ALTER TYPE condition_operator ADD VALUE IF NOT EXISTS 'NOVO_OPERADOR';
```

> ⚠️ **ATENÇÃO:** Não remover valores existentes do enum PostgreSQL. PostgreSQL não suporta `DROP VALUE` em enums sem recriar o tipo.

---

### TAREFA 3: Auditoria Redis

#### 3.1 Mapear estruturas de dados Redis usadas

Leia o arquivo `backend/src/main/java/com/rulex/service/RedisVelocityService.java` e documente:

| Estrutura | Prefixo de Chave | Tipo Redis | Operadores que Usam |
|-----------|-----------------|------------|---------------------|
| Contagem de transações | `{keyType}:{value}:count:{bucket}` | Hash | VELOCITY_COUNT_GT/LT |
| Soma de valores | `{keyType}:{value}:sum:{bucket}` | Hash | VELOCITY_SUM_GT/LT |
| Contagem distinta | `{keyType}:{value}:hll:{type}:{window}` | HyperLogLog | VELOCITY_DISTINCT_GT/LT |
| Bloom Filter | `bloom:{type}` | Bloom Filter | BLOOM_FILTER_* |

#### 3.2 Verificar operadores VELOCITY que usam Redis

```bash
# Listar todos os operadores VELOCITY no enum
grep "^  VELOCITY_" backend/src/main/java/com/rulex/entity/complex/ConditionOperator.java

# Verificar quais estão implementados no VelocityOperatorEvaluator
grep "ConditionOperator.VELOCITY_" backend/src/main/java/com/rulex/service/complex/evaluator/VelocityOperatorEvaluator.java
```

#### 3.3 Verificar KeyType enum

```bash
grep -A 20 "enum KeyType" backend/src/main/java/com/rulex/service/VelocityService.java
```

> Os valores válidos de `KeyType` são: `PAN`, `CUSTOMER_ID`, `MERCHANT_ID`, `DEVICE_ID`, `IP_ADDRESS`, `ACCOUNT_ID`.  
> **NUNCA use `KeyType.ACCOUNT` — não existe!**

#### 3.4 Verificar TimeWindow enum

```bash
grep -A 20 "enum TimeWindow" backend/src/main/java/com/rulex/service/VelocityService.java
```

> Os valores de `TimeWindow` são: `MINUTE_5`, `MINUTE_15`, `MINUTE_30`, `HOUR_1`, `HOUR_6`, `HOUR_12`, `HOUR_24`, `DAY_7`, `DAY_30`.

---

### TAREFA 4: Auditoria Neo4j

#### 4.1 Verificar todos os operadores NEO4J_*

```bash
# Listar todos os operadores NEO4J no enum
grep "^  NEO4J_" backend/src/main/java/com/rulex/entity/complex/ConditionOperator.java

# Verificar implementações no GraphOperatorEvaluator
cat backend/src/main/java/com/rulex/service/complex/evaluator/GraphOperatorEvaluator.java

# Verificar queries Cypher no Neo4jGraphService
cat backend/src/main/java/com/rulex/service/Neo4jGraphService.java
```

#### 4.2 Verificar consistência de labels e relacionamentos

Os labels e relacionamentos usados nas queries Cypher devem corresponder ao schema definido em `init.cypher`:

| Label/Relacionamento | Definido em init.cypher | Usado em Neo4jGraphService |
|---------------------|------------------------|---------------------------|
| `:Account` | ✅ | ✅ |
| `:Customer` | ✅ | Verificar |
| `:Device` | ✅ | Verificar |
| `:IPAddress` | ✅ | Verificar |
| `[:TRANSFERRED_TO]` | ✅ | ✅ |
| `[:SHARES_PII]` | ❓ | ✅ (verificar init.cypher) |
| `[:RECEIVED_FROM]` | ❓ | ✅ (verificar init.cypher) |

#### 4.3 Ação Corretiva (SE necessário)

Se houver relacionamentos usados nas queries mas não definidos em `init.cypher`:

```cypher
-- Adicionar ao init.cypher
CREATE CONSTRAINT shares_pii_unique IF NOT EXISTS
FOR ()-[r:SHARES_PII]-()
REQUIRE r.type IS NOT NULL;
```

#### 4.4 Verificar GDS (Graph Data Science)

```bash
cat backend/src/main/resources/neo4j/gds-config.cypher
```

Os algoritmos GDS usados (`gds.wcc.stream`, `gds.pageRank`, `gds.louvain`, etc.) devem ter projeções de grafo configuradas.

---

### TAREFA 5: Auditoria Frontend

#### 5.1 Verificar consistência operators.ts ↔ ConditionOperator.java

```bash
# Verificar se há operadores no frontend com labels genéricos (gerados automaticamente)
grep "description: 'Operador" client/src/lib/operators.ts | wc -l
```

> Se muitos operadores têm descrição genérica `'Operador X'`, significa que foram gerados automaticamente sem descrição real.

#### 5.2 Verificar operatorTypes.ts

```bash
cat client/src/lib/operatorTypes.ts
```

Verificar se os tipos TypeScript para operadores estão alinhados com o backend.

#### 5.3 Verificar operatorNullBehavior.ts

```bash
cat client/src/lib/operatorNullBehavior.ts
```

Verificar se o comportamento de null para cada operador está documentado no frontend.

#### 5.4 Verificar OperatorCatalog.tsx

```bash
head -20 client/src/manual/OperatorCatalog.tsx
```

O catálogo deve mostrar todos os 469 operadores com suas categorias corretas.

#### 5.5 Verificar geração automática

```bash
ls client/src/manual/generated/
cat client/src/manual/generated/backendOperators.generated.ts | head -30
```

Verificar se os arquivos gerados automaticamente estão atualizados com os operadores mais recentes.

---

### TAREFA 6: Verificar Endpoint de Status de Operadores

O backend expõe um endpoint que permite verificar o status de todos os operadores em tempo de execução:

```
GET /api/operators/status
GET /api/operators/status/{operatorName}
GET /api/operators/list?status=STABLE&implementedOnly=true
GET /api/operators/categories
```

#### 6.1 Verificar se o frontend consome esse endpoint

```bash
grep -r "operators/status\|operators/list" client/src/ | head -10
```

#### 6.2 Verificar se o OperatorCatalog usa dados dinâmicos ou estáticos

```bash
grep -n "fetch\|useQuery\|api\." client/src/manual/OperatorCatalog.tsx | head -10
```

> **IDEAL:** O catálogo deveria buscar dados do endpoint `/api/operators/list` para garantir consistência automática.  
> **ATUAL:** Pode estar usando dados estáticos do `operators.ts`.

---

### TAREFA 7: Verificar Testes de Sincronização

O projeto já possui testes de sincronização. Verifique se estão passando:

```bash
# Executar apenas os testes de sincronização
cd backend && mvn test -pl . -Dtest="OperatorSyncTest,AllOperatorsSmokeTest,AllOperatorsIntegrationTest" -DskipTests=false 2>&1 | tail -30
```

Verificar também:
```bash
cat backend/src/test/java/com/rulex/service/complex/OperatorSyncTest.java
cat backend/src/test/java/com/rulex/service/complex/evaluator/AllOperatorsSmokeTest.java
```

---

## 📊 RELATÓRIO DE AUDITORIA ESPERADO

Ao final da auditoria, gere um relatório no formato:

```markdown
# RELATÓRIO DE AUDITORIA — OPERADORES RULEX

## Resumo Executivo
| Camada | Total | Sincronizado | Gaps |
|--------|-------|--------------|------|
| Backend (ConditionOperator.java) | 469 | 469 | 0 |
| Frontend (operators.ts) | ? | ? | ? |
| PostgreSQL (condition_operator enum) | ? | ? | ? |
| Redis (estruturas de dados) | N/A | ? | ? |
| Neo4j (GraphOperatorEvaluator) | 18 | ? | ? |

## Gaps Identificados

### Gap 1: [Nome do Gap]
- **Camada:** Frontend / Backend / PostgreSQL / Redis / Neo4j
- **Operador(es):** NOME_DO_OPERADOR
- **Evidência:** [arquivo:linha]
- **Impacto:** [Alto/Médio/Baixo]
- **Correção Proposta:** [descrição]

## Ações Corretivas

### Ação 1: [Título]
**Arquivo:** `caminho/para/arquivo`
**Mudança:**
```diff
- código antigo
+ código novo
```
**Validação:** `comando para verificar`
```

---

## 🔧 AÇÕES CORRETIVAS PADRÃO

### Se operador está no Backend mas NÃO no Frontend:

Adicionar em `client/src/lib/operators.ts`:
```typescript
{ 
  value: 'NOME_OPERADOR', 
  label: 'Nome Legível', 
  description: 'Descrição clara do que o operador faz', 
  requiresValue: true,  // false para operadores unários (IS_NULL, IS_TRUE, etc.)
  category: 'Categoria Correta'
}
```

### Se operador está no Frontend mas NÃO no Backend:

**Opção A:** Adicionar ao `ConditionOperator.java` e criar implementação.  
**Opção B:** Remover do `operators.ts` (se for erro).

### Se operador está no Backend mas NÃO no PostgreSQL:

Criar nova migration (ex: `V64__sync_new_operators.sql`):
```sql
ALTER TYPE condition_operator ADD VALUE IF NOT EXISTS 'NOME_OPERADOR';
```

### Se operador NEO4J_* não tem query Cypher:

Adicionar método no `Neo4jGraphService.java`:
```java
public boolean evaluateNomeOperador(String accountId, RuleCondition condition) {
    String query = """
        MATCH (a:Account {id: $accountId})
        // Cypher query aqui
        RETURN resultado
        """;
    // Implementação usando neo4jTemplate
}
```

E registrar no `GraphOperatorEvaluator.java`:
```java
// Em getSupportedOperators():
ConditionOperator.NEO4J_NOME_OPERADOR,

// No switch:
case NEO4J_NOME_OPERADOR -> neo4jService.evaluateNomeOperador(accountId, condition);
```

### Se operador VELOCITY_* não tem estrutura Redis:

O `VelocityServiceFacade.getStats()` aceita 3 parâmetros:
```java
VelocityService.VelocityStats stats = velocityServiceFacade.getStats(
    context.getTransactionRequest(),  // TransactionRequest
    VelocityService.KeyType.PAN,      // KeyType (PAN, CUSTOMER_ID, MERCHANT_ID, DEVICE_ID, IP_ADDRESS, ACCOUNT_ID)
    VelocityService.TimeWindow.HOUR_1 // TimeWindow (MINUTE_5, MINUTE_15, MINUTE_30, HOUR_1, HOUR_6, HOUR_12, HOUR_24, DAY_7, DAY_30)
);
```

---

## ✅ CRITÉRIOS DE SUCESSO

A auditoria está **CONCLUÍDA COM SUCESSO** quando:

- [ ] **S1:** `comm -23 /tmp/backend_ops.txt /tmp/frontend_ops.txt` retorna **VAZIO**
- [ ] **S2:** `comm -13 /tmp/backend_ops.txt /tmp/frontend_ops.txt` retorna **VAZIO**
- [ ] **S3:** Todos os 469 operadores têm avaliador registrado no `OperatorEvaluatorRegistry`
- [ ] **S4:** `StubOperatorEvaluator.PLANNED_OPERATORS` está **VAZIO** (ou documentado)
- [ ] **S5:** Todos os 18 operadores `NEO4J_*` têm query Cypher correspondente
- [ ] **S6:** A migration V51 (ou nova V64+) contém todos os 469 operadores
- [ ] **S7:** `mvn compile -DskipTests` retorna **BUILD SUCCESS**
- [ ] **S8:** `pnpm run check` retorna **0 erros TypeScript**
- [ ] **S9:** `mvn test -Dtest="OperatorSyncTest"` retorna **BUILD SUCCESS**
- [ ] **S10:** Relatório de auditoria gerado com evidências de código

---

## 📁 ARQUIVOS-CHAVE PARA ANÁLISE

Leia estes arquivos **na ordem indicada** antes de fazer qualquer modificação:

1. `backend/src/main/java/com/rulex/entity/complex/ConditionOperator.java` — Fonte da verdade
2. `backend/src/main/java/com/rulex/service/complex/evaluator/OperatorEvaluatorRegistry.java` — Registro central
3. `backend/src/main/java/com/rulex/service/complex/evaluator/StubOperatorEvaluator.java` — Operadores não implementados
4. `client/src/lib/operators.ts` — Definições do frontend
5. `backend/src/main/resources/db/migration/V51__sync_condition_operator_enum.sql` — Sync PostgreSQL
6. `backend/src/main/java/com/rulex/service/complex/evaluator/GraphOperatorEvaluator.java` — Operadores Neo4j
7. `backend/src/main/java/com/rulex/service/Neo4jGraphService.java` — Queries Cypher
8. `backend/src/main/java/com/rulex/service/RedisVelocityService.java` — Estruturas Redis
9. `backend/src/main/java/com/rulex/service/VelocityService.java` — Enums KeyType e TimeWindow
10. `backend/src/main/resources/neo4j/init.cypher` — Schema Neo4j
11. `backend/src/test/java/com/rulex/service/complex/OperatorSyncTest.java` — Testes de sincronização

---

## 🔗 ENDPOINTS ÚTEIS PARA VALIDAÇÃO EM RUNTIME

Após subir a stack com `docker-compose up -d`:

```bash
# Ver todos os operadores e seus status
curl -u admin:senha http://localhost:8080/api/operators/status | jq .

# Ver operadores STABLE (implementados)
curl -u admin:senha "http://localhost:8080/api/operators/list?status=STABLE" | jq '.count'

# Ver operadores PLANNED (não implementados)
curl -u admin:senha "http://localhost:8080/api/operators/list?status=PLANNED" | jq '.operators[].name'

# Ver categorias
curl -u admin:senha http://localhost:8080/api/operators/categories | jq .

# Verificar operador específico
curl -u admin:senha http://localhost:8080/api/operators/status/NEO4J_FRAUD_RING_DETECTION | jq .
```

---

## 📌 NOTAS FINAIS

### Sobre o Padrão de Implementação de Operadores

O projeto usa arquitetura **Strategy Pattern** com registro automático:

```
ConditionOperator (enum) 
    → OperatorEvaluatorRegistry (registro via @PostConstruct)
        → OperatorEvaluator (interface)
            → VelocityOperatorEvaluator (implementação)
            → GraphOperatorEvaluator (implementação)
            → FATFOperatorEvaluator (implementação)
            → ... (30+ implementações)
```

### Sobre o Padrão de Valores em Operadores

Operadores complexos usam formato **pipe-separated** no campo `valueSingle`:
```
"fieldName|nDays|threshold|operator"
Exemplo: "amount|7|5000|GT"
```

### Sobre Neo4j e GDS

O Neo4j usa o plugin **Graph Data Science (GDS)** para algoritmos de grafos. As projeções de grafo devem ser criadas antes de executar algoritmos como `gds.wcc.stream`, `gds.pageRank`, etc. Verifique o arquivo `gds-config.cypher`.

### Sobre Redis e Velocity

O Redis usa **Hash** para contagens/somas e **HyperLogLog** para contagens distintas. Os dados são particionados por `KeyType` e `TimeWindow`. O TTL padrão é configurado em `RedisVelocityService.REDIS_TTL`.

---

**FIM DO PROMPT — Boa auditoria, Devin! 🚀**
