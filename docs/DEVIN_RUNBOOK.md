# DEVIN RUNBOOK - Como Retomar em 2 Minutos

**Projeto:** RULEX Credit Rules Research & Implementation  
**Última Atualização:** 2025-01-03

---

## 🚀 RETOMADA RÁPIDA

### 1. Verificar Estado Atual
```bash
cd ~/repos/RULEX
git status
git branch
cat docs/DEVIN_STATE.json | jq '.last_known_good'
cat docs/DEVIN_STATE.json | jq '.next_actions'
```

### 2. Verificar Progresso
```bash
cat docs/DEVIN_PROGRESS.md | head -80
```

### 3. Continuar do Último Ponto
Consulte `docs/DEVIN_STATE.json`:
- `last_known_good.step_id` → último passo concluído
- `next_actions` → próximos 3 passos

---

## 📋 CHECKLIST DE RETOMADA

- [ ] Ler `docs/DEVIN_STATE.json`
- [ ] Ler `docs/DEVIN_PROGRESS.md`
- [ ] Verificar branch atual (`git branch`)
- [ ] Verificar se há commits pendentes (`git status`)
- [ ] Continuar do `next_step_id`
- [ ] NÃO refazer trabalho marcado como DONE

---

## 🔧 COMANDOS ÚTEIS

### Auditoria do Repo
```bash
# Ver campos do payload
cat backend/src/main/java/com/rulex/dto/TransactionRequest.java

# Ver operadores do motor
cat backend/src/main/java/com/rulex/v31/ast/AstEvaluator.java

# Ver field dictionary
cat backend/src/main/java/com/rulex/v31/field/FieldDictionarySeeder.java

# Ver regras existentes
find . -name "*.json" -path "*/rules/*" | head -20
```

### Commits por Marco
```bash
git add docs/
git commit -m "checkpoint: <descrição do marco>"
```

### Testes
```bash
cd ~/repos/RULEX && pnpm test
cd ~/repos/RULEX && mvn -f backend/pom.xml test
```

---

## 📊 FASES DO PROJETO

| Fase | Descrição | Arquivos Relacionados |
|------|-----------|----------------------|
| 1 | Auditoria do Repo | TransactionRequest.java, AstEvaluator.java |
| 2 | Análise de URLs | docs/EXTERNAL_CREDIT_DATASET_RESEARCH.md |
| 3 | Catálogo de Regras | docs/RULE_IDEAS_FROM_PUBLIC_DATASETS.md |
| 4 | Implementação | backend/src/main/resources/rules/ |
| 5 | Documentação | docs/PAYLOAD_DICTIONARY.md |

---

## ⚠️ REGRAS CRÍTICAS

1. **NÃO É ML** - Sistema de regra dura apenas
2. **Anti-alucinação** - Toda afirmação precisa de evidência (URL + trecho ou path + linha)
3. **Git limpo** - Sempre manter git status limpo
4. **Commits pequenos** - Por marco, com mensagens padronizadas
5. **Atualizar estado** - Sempre atualizar DEVIN_STATE.json e DEVIN_PROGRESS.md

---

## 📁 ARQUIVOS OBRIGATÓRIOS

| Arquivo | Propósito |
|---------|-----------|
| docs/DEVIN_STATE.json | Estado estruturado (JSON) |
| docs/DEVIN_PROGRESS.md | Diário humano-legível |
| docs/DEVIN_RUNBOOK.md | Este arquivo |
| docs/EXTERNAL_CREDIT_DATASET_RESEARCH.md | Pesquisa de URLs |
| docs/FEATURE_CATALOG_CREDIT_RULES.md | Catálogo de features |
| docs/RULE_IDEAS_FROM_PUBLIC_DATASETS.md | 80+ regras candidatas |
| docs/IMPLEMENTED_RULES_CHANGELOG.md | Regras implementadas |
| docs/GAPS_AND_RECOMMENDATIONS.md | Gaps e propostas |
| docs/PAYLOAD_DICTIONARY.md | Dicionário atualizado |

---

## 🔗 URLs A ANALISAR (22 total)

### Datasets (18)
1. Home Credit Default Risk (Kaggle)
2. Taiwan Credit Card (UCI)
3. German Credit (UCI)
4. Give Me Some Credit (Kaggle)
5. Lending Club (Kaggle)
6. Lending Club (Figshare)
7. OpenIntro Loans
8. Bondora P2P (Kaggle)
9. Bondora P2P Alt (Kaggle)
10. Freddie Mac
11. Freddie Mac Guide (PDF)
12. Fannie Mae
13. Fannie Mae Docs
14. FICO Blog
15. FICO Challenge
16. OpenML FICO
17. Interpretable AI
18. FHFA PUDB

### Transfer Learning Papers (4)
19. Frontiers AI
20. ACM Domain Adaptation
21. MDPI Mathematics
22. AAAI KDF

---

## 📞 CONTATO

Se houver problemas, consulte:
- Documentação existente em `docs/`
- Código fonte em `backend/src/main/java/com/rulex/`
- Testes em `backend/src/test/java/`
