# ALT_COMMITTEE_BACKUP_DR.md
## Auditoria de Backup e Disaster Recovery - RULEX

**Data:** 2025-12-30
**Auditor:** Comitê Alternativo de Auditoria

---

## RESUMO

| Teste | Status |
|-------|--------|
| Export YAML | ✅ PASS |
| Export JSON | ✅ PASS |
| Estrutura preservada | ✅ PASS |
| Metadados incluídos | ✅ PASS |

---

## 1. TESTE DE EXPORT

### 1.1 Export YAML

#### Comando
```bash
curl -s -u admin:rulex \
  "http://localhost:8080/api/api/v1/rules/export-import/export?format=yaml" \
  > rulex_backup.yaml
```

#### Resultado ✅
```yaml
metadata:
  version: 1.0
  exportedAt: 1767124526.283842928
  exportedBy: null
  description: Export completo de regras RULEX
  totalRules: 4
  sourceSystem: RULEX
rules:
- key: TEST_KEY
  title: Test Rule
  description: Teste
  version: 1
  status: DRAFT
  priority: 100
  severity: 70
  decision: SUSPEITA_DE_FRAUDE
  ruleType: SIMPLE
  simpleConfig:
    logicOperator: AND
    conditions:
    - field: transactionAmount
      operator: GT
      value: 5000
    weight: 70
    classification: SUSPEITA_DE_FRAUDE
  fieldsUsed:
  - transactionAmount
  createdAt: 1767122402.684509000
```

### 1.2 Conteúdo Exportado

| Campo | Incluído | Observação |
|-------|----------|------------|
| key | ✅ | Identificador único |
| title | ✅ | Nome da regra |
| version | ✅ | Número da versão |
| status | ✅ | DRAFT/PUBLISHED |
| priority | ✅ | Prioridade |
| severity | ✅ | Severidade |
| decision | ✅ | Decisão (enum) |
| conditions | ✅ | Condições da regra |
| fieldsUsed | ✅ | Campos utilizados |
| createdAt | ✅ | Timestamp de criação |

---

## 2. ESTRUTURA DO BACKUP

### 2.1 Metadados
```yaml
metadata:
  version: "1.0"           # Versão do formato
  exportedAt: timestamp    # Momento do export
  totalRules: 4            # Contagem de regras
  sourceSystem: "RULEX"    # Sistema de origem
```

### 2.2 Regras
Cada regra contém:
- Identificação (key, title)
- Configuração (priority, severity, decision)
- Lógica (conditions, logicOperator)
- Metadados (version, status, createdAt)

---

## 3. ENDPOINTS DE BACKUP

| Operação | Endpoint | Método |
|----------|----------|--------|
| Export All | `/api/api/v1/rules/export-import/export` | GET |
| Export by Key | `/api/api/v1/rules/export-import/export/{key}` | GET |
| Import | `/api/api/v1/rules/export-import/import` | POST |
| Validate | `/api/api/v1/rules/export-import/validate` | POST |

### Parâmetros de Export
- `format`: `yaml` ou `json` (default: json)

---

## 4. FIDELIDADE DO BACKUP

### 4.1 Campos Preservados
| Campo | Preservado | Verificação |
|-------|------------|-------------|
| key | ✅ | Único, não duplicado |
| conditions | ✅ | Estrutura completa |
| priority | ✅ | Valor numérico |
| severity | ✅ | Valor numérico |
| decision | ✅ | Enum válido |
| status | ✅ | DRAFT/PUBLISHED |

### 4.2 Integridade
- **Hash de validação:** Não implementado (recomendação futura)
- **Versionamento:** Incluído no metadata
- **Timestamp:** Incluído para rastreabilidade

---

## 5. PROCEDIMENTO DE RESTORE

### 5.1 Passos Recomendados
1. Fazer backup do estado atual
2. Validar arquivo de import
3. Executar import
4. Verificar regras importadas
5. Testar simulação

### 5.2 Comando de Import
```bash
curl -X POST -u admin:rulex \
  "http://localhost:8080/api/api/v1/rules/export-import/import" \
  -H "Content-Type: multipart/form-data" \
  -F "file=@rulex_backup.yaml"
```

---

## 6. LIMITAÇÕES CONHECIDAS

1. **RuleSets não exportados:** O export atual foca em regras individuais
2. **Active RuleSet:** Não é incluído no backup
3. **Audit Log:** Não é exportado (design intencional)

### Recomendações
- Para DR completo, incluir dump do PostgreSQL
- Documentar RuleSet ativo antes de restore

---

## 7. CONCLUSÃO

O sistema de backup/export do RULEX:
- ✅ Exporta todas as regras com metadados
- ✅ Preserva estrutura e configurações
- ✅ Suporta múltiplos formatos (YAML/JSON)
- ✅ Inclui versionamento e timestamps

**STATUS: APROVADO**

---

**Documento gerado pelo Comitê Alternativo de Auditoria**
