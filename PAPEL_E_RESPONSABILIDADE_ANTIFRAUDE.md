# Papel e Responsabilidade (Antifraude: regras duras)

Este documento define responsabilidades **defensivas** para operação de regras duras em ambiente bancário, com trilha de auditoria e segregação de funções.

## Princípios
- **Segregação de funções**: quem cria regra não aprova sozinho.
- **Change management**: toda alteração de regra gera histórico append-only e justificativa.
- **Evidência mínima**: toda regra deve ter fonte, hipótese e impacto esperado.
- **Segurança por padrão**: logs e payload com dados sensíveis devem ser mascarados.

## RACI (alto nível)

### 1) Definição / Curadoria de Regras
- **Responsible**: Antifraude Tier-1 / Fraud Strategy
- **Accountable**: Head de Risco (Fraud Risk Owner)
- **Consulted**: Compliance/Regulatório; Segurança (AppSec); Operações; Jurídico
- **Informed**: Atendimento/Chargeback; Produto

### 2) Implementação Técnica (Engine + API)
- **Responsible**: Engenharia (Rule Engine / Backend)
- **Accountable**: Engineering Manager / Tech Lead
- **Consulted**: Antifraude; Data Platform (se houver); SRE
- **Informed**: Compliance; Auditoria interna

### 3) Aprovação para Produção (Go/No-Go)
- **Responsible**: Change Advisory Board (CAB) / Operações de TI
- **Accountable**: Risco Operacional
- **Consulted**: Antifraude; Engenharia; Compliance
- **Informed**: Stakeholders de negócio

### 4) Monitoramento e Ajuste
- **Responsible**: Antifraude (monitorar FPR/TPR); SRE (disponibilidade/latência)
- **Accountable**: Head de Risco
- **Consulted**: Engenharia; Operações
- **Informed**: Compliance

### 5) Auditoria e Evidências
- **Responsible**: Auditoria interna / Risco Operacional
- **Accountable**: Diretorias de Risco/Compliance
- **Consulted**: Engenharia (extração de logs); Antifraude (interpretação)
- **Informed**: Comitês de governança

## Controles mínimos (checklist)
- Versionamento de regras (snapshot anterior/atual + autor + data + justificativa)
- Testes determinísticos (unit/integration) por regra crítica
- “Kill switch” por regra (toggle) com trilha de auditoria
- Política de false positive (limites e rollback)
- Observabilidade: métricas por regra (disparos, impacto em decisão)

## Artefatos recomendados no RULEX
- Histórico append-only de regras (já existe no backend)
- Export canônico de regras (ver [FRAUDE_REGRAS_DURAS_EXPORT.yaml](FRAUDE_REGRAS_DURAS_EXPORT.yaml))
- Inventário do payload (ver [FRAUDE_PAYLOAD_INVENTARIO.md](FRAUDE_PAYLOAD_INVENTARIO.md))
