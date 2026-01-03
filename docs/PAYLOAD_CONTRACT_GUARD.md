# RULEX - Guarda do Contrato de Payload

**Versão:** 1.0.0  
**Data:** 2025-01-03  
**Status:** ATIVO - PAYLOAD IMUTÁVEL

---

## 1. Declaração de Imutabilidade

> **⚠️ ATENÇÃO MÁXIMA: O payload de entrada do RULEX é IMUTÁVEL.**

O contrato de entrada (TransactionRequest) **NÃO PODE** ser alterado em hipótese alguma:
- ❌ NÃO criar novos parâmetros/campos de entrada
- ❌ NÃO remover parâmetros existentes
- ❌ NÃO renomear parâmetros
- ❌ NÃO alterar tipos (String→Integer, etc.)
- ❌ NÃO alterar estrutura (nesting/arrays/objetos)
- ❌ NÃO alterar DTO/schema/request contract

---

## 2. Localização do Contrato

### 2.1 Arquivo Principal (Fonte da Verdade)
```
backend/src/main/java/com/rulex/dto/TransactionRequest.java
```

### 2.2 Arquivos Relacionados
| Arquivo | Propósito |
|---------|-----------|
| `TransactionRequest.java` | DTO principal - FONTE DA VERDADE |
| `FieldDictionarySeeder.java` | Popula `field_dictionary` via reflexão do DTO |
| `field_dictionary` (tabela) | Catálogo de campos disponíveis para regras |
| `PAYLOAD_DICTIONARY.md` | Documentação dos campos |

---

## 3. Campos Obrigatórios (Validados por @NotNull/@NotBlank)

| Campo | Tipo | Validação |
|-------|------|-----------|
| `externalTransactionId` | String | @NotBlank |
| `customerIdFromHeader` | String | @NotBlank |
| `customerAcctNumber` | Long | @NotNull |
| `pan` | String | @NotBlank |
| `transactionCurrencyCode` | Integer | @NotNull |
| `transactionAmount` | BigDecimal | @NotNull, @DecimalMin("0.0") |
| `transactionDate` | Integer | @NotNull |
| `transactionTime` | Integer | @NotNull |
| `mcc` | Integer | @NotNull |
| `consumerAuthenticationScore` | Integer | @NotNull, @Min(0), @Max(999) |
| `externalScore3` | Integer | @NotNull, @Min(0), @Max(999) |
| `cavvResult` | Integer | @NotNull |
| `eciIndicator` | Integer | @NotNull |
| `atcCard` | Integer | @NotNull |
| `atcHost` | Integer | @NotNull |
| `tokenAssuranceLevel` | Integer | @NotNull |
| `availableCredit` | BigDecimal | @NotNull |
| `cardCashBalance` | BigDecimal | @NotNull |
| `cardDelinquentAmount` | BigDecimal | @NotNull |

---

## 4. Testes de Proteção do Contrato

### 4.1 Contract Tests Existentes
```
backend/src/test/java/com/rulex/contract/ContractTestBase.java
```

### 4.2 Testes Recomendados (a implementar)
```java
// PayloadContractTest.java
@Test
void transactionRequest_shouldHaveExactly80Fields() {
    // Verifica que o número de campos não mudou
}

@Test
void transactionRequest_shouldHaveRequiredFieldsWithCorrectTypes() {
    // Verifica tipos dos campos obrigatórios
}

@Test
void transactionRequest_shouldNotHaveNewFields() {
    // Compara com snapshot de campos conhecidos
}
```

---

## 5. Regras para Novas Funcionalidades

### 5.1 O que PODE ser feito (SEM alterar payload):
- ✅ Criar campos DERIVADOS a partir de campos existentes
- ✅ Fazer lookup em tabelas internas (geo_reference, bin_lookup, mcc_categories)
- ✅ Calcular agregações (velocity) usando campos existentes
- ✅ Normalizar/parsear campos existentes (ex: extrair hora de transactionTime)
- ✅ Concatenar campos existentes para criar chaves compostas

### 5.2 Exemplos de Campos Derivados Permitidos:
| Campo Derivado | Fonte | Método |
|----------------|-------|--------|
| `merchantLocation` | merchantCity + merchantState + merchantCountryCode | Lookup geo_reference |
| `panHash` | pan | SHA-256 hash |
| `binInfo` | pan (6 primeiros dígitos) | Lookup bin_lookup |
| `transactionHour` | transactionTime | Parsing (HHMMSS → HH) |
| `isInternational` | merchantCountryCode | Comparação (≠ "076") |
| `mccCategory` | mcc | Lookup mcc_categories |

### 5.3 O que NÃO PODE ser feito:
- ❌ Exigir novos campos do cliente (deviceId, ipAddress, etc.)
- ❌ Alterar estrutura do JSON de entrada
- ❌ Adicionar campos ao DTO TransactionRequest

---

## 6. Procedimento para Gaps de Dados

Se uma regra de fraude exigir um campo que NÃO existe no payload:

1. **Classificar como GAP_DE_DADO (PAYLOAD_IMUTAVEL)**
2. **NÃO implementar a regra (DO_NOT_IMPLEMENT)**
3. **Documentar em GAPS_DA_SOLUCAO.md**
4. **Propor alternativa** que use apenas campos existentes, se possível
5. **Registrar em PLAN_ENRICHMENT.md** para futura evolução (fora do escopo atual)

---

## 7. Checklist de Validação

Antes de qualquer implementação, verificar:

- [ ] A regra usa APENAS campos do TransactionRequest?
- [ ] Campos derivados são calculados a partir de campos existentes?
- [ ] Lookups usam chaves já presentes no payload?
- [ ] Nenhum novo campo foi adicionado ao DTO?
- [ ] Contract tests continuam passando?
- [ ] git status está limpo (sem alterações no DTO)?

---

## 8. Evidências

### 8.1 DTO TransactionRequest
- **Path**: `backend/src/main/java/com/rulex/dto/TransactionRequest.java`
- **Linhas**: 1-320
- **Total de campos**: ~80

### 8.2 Field Dictionary Seeder
- **Path**: `backend/src/main/java/com/rulex/v31/field/FieldDictionarySeeder.java`
- **Método**: Reflexão do DTO TransactionRequest

### 8.3 Contract Test Base
- **Path**: `backend/src/test/java/com/rulex/contract/ContractTestBase.java`
- **Framework**: Spring Cloud Contract + RestAssured

---

## 9. Referências

- [PAYLOAD_DICTIONARY.md](./PAYLOAD_DICTIONARY.md) - Dicionário completo de campos
- [GAPS_DA_SOLUCAO.md](./GAPS_DA_SOLUCAO.md) - Gaps identificados
- [TransactionRequest.java](../backend/src/main/java/com/rulex/dto/TransactionRequest.java) - Fonte da verdade
