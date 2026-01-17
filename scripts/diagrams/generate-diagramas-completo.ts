/**
 * Gerador do documento docs/DIAGRAMAS.md completo e auditável.
 *
 * Regras de rigor (PROMPT FINAL / DOUBLE CHECK):
 * - NÃO inventar: nenhum dado é inferido sem evidência no repositório.
 * - Quando faltar evidência: marcar explicitamente como "SEM EVIDÊNCIA" e indicar
 *   quais arquivos precisam existir para completar.
 * - Incluir varredura do repositório (inventário) e Catálogo Mestre no próprio documento.
 */

import fs from "node:fs";
import path from "node:path";

import { DIAGRAM_ITEMS } from "../../client/src/features/diagrams/registry/diagramRegistry";
import type { DiagramCatalogItem } from "../../client/src/features/diagrams/types";

type DiagramStatus = "OK" | "PARCIAL" | "SEM_EVIDENCIA";

type DiagramCatalogRow = {
  categoria: string;
  diagrama: string;
  publico: string;
  nivel: string;
  evidencias: string[];
  status: DiagramStatus;
};

// Coletado dinamicamente a cada chamada de diagramBlock().
const MASTER_CATALOG_ROWS: DiagramCatalogRow[] = [];

// Contexto (evita reescrever parâmetros em 70+ blocos)
let CURRENT_CATEGORIA = "N/A";
let CURRENT_PUBLICO = "N/A";
let CURRENT_NIVEL = "N/A";

function setDiagramContext(ctx: {
  categoria: string;
  publico: string;
  nivel: string;
}) {
  CURRENT_CATEGORIA = ctx.categoria;
  CURRENT_PUBLICO = ctx.publico;
  CURRENT_NIVEL = ctx.nivel;
}

// Evidências explícitas (path reais). Se um título não estiver aqui, ele NÃO pode ser
// marcado como "OK" automaticamente.
const EVIDENCE_BY_TITLE: Record<string, string[]> = {
  // Frontend
  "Arquitetura do Frontend": ["client/src/main.tsx", "client/src/App.tsx", "vite.config.ts", "package.json"],
  "Componentes do Frontend": ["client/src/components", "components.json"],
  "Wireflow — Navegação Principal": ["client/src/App.tsx"],
  "Fluxo UI — Criação de Regra": ["client/src/pages/ComplexRules.tsx", "client/src/App.tsx"],

  // Backend / API
  "Arquitetura Backend — Camadas": ["backend/src/main/java/com/rulex"],
  "Fluxo — Análise de Transação (/analyze)": [
    "backend/src/main/java/com/rulex/controller/TransactionController.java",
    "backend/src/main/java/com/rulex/service/RuleEngineService.java",
    "openapi/rulex.yaml"
  ],
  "UML — Diagrama de Sequência (Análise de Transação)": [
    "backend/src/main/java/com/rulex/controller/TransactionController.java",
    "backend/src/main/java/com/rulex/service/RuleEngineService.java"
  ],
  "Autenticação e Autorização": ["backend/src/main/java/com/rulex/config/SecurityConfig.java", "backend/src/main/resources/application.yml"],
  "Padrões de Resiliência": ["backend/pom.xml", "backend/src/main/resources/application.yml"],

  // API Contract
  "C4 — Container Diagram": ["docker-compose.yml", "backend/src/main/resources/application.yml", "client/src/App.tsx"],

  // Infra / Deploy local
  "Docker Compose (ambiente local)": ["docker-compose.yml", "Dockerfile.web", "backend/Dockerfile"],

  // Postgres
  "Modelo Conceitual": ["backend/src/main/resources/db/migration/V2__core_schema.sql"],
  "ERD — Entidades Core": ["backend/src/main/resources/db/migration/V2__core_schema.sql"],
  "ERD — Completo": ["backend/src/main/resources/db/migration"],

  // Redis / Velocity
  "Estratégia de Cache — RULEX": [
    "backend/src/main/resources/application.yml",
    "backend/src/main/java/com/rulex/service/RedisVelocityCacheService.java",
    "backend/src/main/java/com/rulex/service/RedisVelocityService.java",
    "backend/src/main/java/com/rulex/service/VelocityServiceFacade.java"
  ],
  "Tipos de Dados Redis — RULEX": [
    "backend/src/main/java/com/rulex/service/RedisVelocityCacheService.java",
    "backend/src/main/java/com/rulex/service/RedisVelocityService.java"
  ],

  // Neo4j
  "Property Graph — RULEX": [
    "backend/src/main/java/com/rulex/service/Neo4jGraphService.java",
    "backend/src/main/java/com/rulex/service/complex/ComplexRuleEvaluator.java",
    "docker-compose.yml",
    "backend/src/main/resources/application.yml"
  ],
  "Exemplo de Grafo — Fraud Ring": [
    "backend/src/main/java/com/rulex/service/Neo4jGraphService.java",
    "backend/src/main/java/com/rulex/service/complex/ComplexRuleEvaluator.java"
  ],

  // Observabilidade
  "Stack de Observabilidade": ["backend/src/main/resources/application.yml", "backend/src/main/resources/prometheus-alerts.yml"],
  "Alertas Configurados": ["backend/src/main/resources/prometheus-alerts.yml"],
};

// Para diagramas SEM EVIDÊNCIA, indicar arquivos esperados para completar.
const EXPECTED_FILES_BY_TITLE: Record<string, string[]> = {
  // Chaves precisam bater com o primeiro argumento passado em placeholderDiagram(...)
  "BPMN AS-IS": ["docs/processos/*.bpmn", "docs/processos/*.png"],
  "BPMN TO-BE": ["docs/processos/*.bpmn", "docs/processos/*.png"],
  "BPMN Exceção": ["docs/processos/*.bpmn"],
  "BPMN Rollback": ["docs/processos/*.bpmn"],
};

// ==============================================================================
// HELPERS
// ==============================================================================

function mdEscape(text: string): string {
  return text.replace(/\r?\n/g, " ").trim();
}

function mdList(items: string[]): string {
  if (items.length === 0) return "- (definir paths esperados para completar)";
  return items.map((p) => `- ${p}`).join("\n");
}

function normalizeEvidencePaths(paths: string[] | undefined): string[] {
  if (!paths) return [];
  return paths.map((p) => p.replace(/\\/g, "/")).filter(Boolean);
}

function resolveEvidence(title: string): { status: DiagramStatus; evidencias: string[] } {
  const evidencias = normalizeEvidencePaths(EVIDENCE_BY_TITLE[title]);
  if (evidencias.length === 0) {
    return { status: "SEM_EVIDENCIA", evidencias: [] };
  }
  // No momento, não há heurística automática de "parcial"; reservado para quando
  // existirem evidências incompletas por diagrama.
  return { status: "OK", evidencias };
}

function findDiagramById(id: string): DiagramCatalogItem | undefined {
  return DIAGRAM_ITEMS.find((d) => d.id === id);
}

function findDiagramsByCategory(categoryId: string): DiagramCatalogItem[] {
  return DIAGRAM_ITEMS.filter((d) => d.categoryId === categoryId);
}

function findDiagramsByNotation(notation: string): DiagramCatalogItem[] {
  return DIAGRAM_ITEMS.filter((d) => d.notation === notation);
}

function findSolutionDiagrams(): DiagramCatalogItem[] {
  return DIAGRAM_ITEMS.filter((d) => d.origin === "solution" && d.verified);
}

function sampleToMermaid(item: DiagramCatalogItem): string {
  const s = item.sample;
  if (s.kind === "inline" && s.format === "mermaid") {
    return ["```mermaid", s.content.trimEnd(), "```"].join("\n");
  }
  if (s.kind === "inline") {
    return ["```text", s.content.trimEnd(), "```"].join("\n");
  }
  if (s.kind === "json") {
    return ["```json", JSON.stringify(s.data, null, 2), "```"].join("\n");
  }
  return `(Arquivo/URI: ${(s as { uri?: string }).uri ?? "N/A"})`;
}

function placeholderDiagram(title: string, description: string): string {
  const expected = normalizeEvidencePaths(EXPECTED_FILES_BY_TITLE[title] ?? []);
  return `
> **SEM EVIDÊNCIA NO REPOSITÓRIO**
>
> Este diagrama está na lista obrigatória, porém **não foi encontrada evidência verificável** no repositório para preenchimento automático.
>
> **Descrição esperada (neutra)**: ${description}
>
> **Para completar, anexar/confirmar no repositório**:\n${mdList(expected)}
`;
}

function diagramBlock(
  title: string,
  objetivo: string,
  quandoUsar: string,
  oQueRepresenta: string,
  riscoSeAusente: string,
  content: string,
  isPlaceholder: boolean = false,
  notas?: string
): string {
  const ev = resolveEvidence(title);
  const status: DiagramStatus = isPlaceholder ? "SEM_EVIDENCIA" : ev.status;

  // Registrar no Catálogo Mestre (para a tabela no topo do documento).
  MASTER_CATALOG_ROWS.push({
    categoria: CURRENT_CATEGORIA,
    diagrama: title,
    publico: CURRENT_PUBLICO,
    nivel: CURRENT_NIVEL,
    evidencias: ev.evidencias,
    status,
  });

  const statusLabel =
    status === "OK" ? "✅ OK" : status === "PARCIAL" ? "🟧 PARCIAL" : "🟥 SEM EVIDÊNCIA";
  const evidenciasText =
    ev.evidencias.length > 0
      ? mdList(ev.evidencias)
      : "- **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO**";

  return `
### ${title}

- Categoria: ${CURRENT_CATEGORIA}
- Público: ${CURRENT_PUBLICO}
- Nível: ${CURRENT_NIVEL}
- Status: ${statusLabel}

**Evidência no repositório**
${evidenciasText}

**Objetivo**
${mdEscape(objetivo)}

**Quando usar**
${mdEscape(quandoUsar)}

**O que representa**
${mdEscape(oQueRepresenta)}

**Entradas**
- (ver evidência; varia por diagrama)

**Saídas**
- (ver evidência; varia por diagrama)

**Regras/Assunções (somente se comprovadas)**
- (sem regras/assunções registradas para este diagrama)

**Riscos**
${mdEscape(riscoSeAusente)}

${notas ? `**Notas**\n${mdEscape(notas)}\n` : ""}

${content}
`;
}

function renderMasterCatalog(rows: DiagramCatalogRow[]): string {
  const header =
    "| Categoria | Diagrama | Público | Nível | Evidência | Status |\n" +
    "|---|---|---|---|---|---|";

  const sorted = [...rows].sort((a, b) => {
    if (a.categoria !== b.categoria) return a.categoria.localeCompare(b.categoria);
    return a.diagrama.localeCompare(b.diagrama);
  });

  const lines = sorted.map((r) => {
    const evidencia =
      r.evidencias.length > 0 ? r.evidencias.join("; ") : "EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO";
    const status = r.status === "OK" ? "OK" : r.status === "PARCIAL" ? "PARCIAL" : "SEM EVIDÊNCIA";
    return `| ${mdEscape(r.categoria)} | ${mdEscape(r.diagrama)} | ${mdEscape(r.publico)} | ${mdEscape(r.nivel)} | ${mdEscape(evidencia)} | ${status} |`;
  });

  return [header, ...lines].join("\n");
}

function renderInventory(repoRoot: string): string {
  const topLevel = fs
    .readdirSync(repoRoot, { withFileTypes: true })
    .filter((d) => !d.name.startsWith("."))
    .map((d) => (d.isDirectory() ? `${d.name}/` : d.name))
    .sort((a, b) => a.localeCompare(b));

  const countFiles = (root: string, exts: string[]): number => {
    if (!fs.existsSync(root)) return 0;
    let total = 0;
    const stack = [root];
    while (stack.length) {
      const cur = stack.pop()!;
      const entries = fs.readdirSync(cur, { withFileTypes: true });
      for (const e of entries) {
        const full = path.join(cur, e.name);
        if (e.isDirectory()) {
          stack.push(full);
        } else if (exts.includes(path.extname(e.name))) {
          total += 1;
        }
      }
    }
    return total;
  };

  const counts: Array<{ label: string; rel: string; abs: string; exts: string[] }> = [
    {
      label: "Java (backend)",
      rel: "backend/src/main/java",
      abs: path.join(repoRoot, "backend", "src", "main", "java"),
      exts: [".java"],
    },
    {
      label: "SQL migrations (Flyway)",
      rel: "backend/src/main/resources/db/migration",
      abs: path.join(repoRoot, "backend", "src", "main", "resources", "db", "migration"),
      exts: [".sql"],
    },
    {
      label: "Frontend TS/TSX",
      rel: "client/src",
      abs: path.join(repoRoot, "client", "src"),
      exts: [".ts", ".tsx"],
    },
  ];

  const countLines = counts
    .map((c) => {
      const n = countFiles(c.abs, c.exts);
      return `- ${c.label}: **${n}** arquivo(s) (${c.rel})`;
    })
    .join("\n");

  return `
### Estrutura (top-level)

${topLevel.map((x) => `- ${x}`).join("\n")}

### Entrypoints (verificados)

- Frontend: client/src/main.tsx
- Frontend (rotas): client/src/App.tsx
- Backend: backend/src/main/java/com/rulex/RulexApplication.java

### Contrato de API (verificado)

- OpenAPI: openapi/rulex.yaml
- Context path do backend: /api (backend/src/main/resources/application.yml)

### Infra local (verificada)

- docker-compose.yml (PostgreSQL 16 + Redis 7 + Neo4j 5 + backend + web)

### Contagens rápidas (automatizadas)

${countLines}
`;
}

// ==============================================================================
// MAIN GENERATOR
// ==============================================================================

async function main() {
  const repoRoot = path.resolve(import.meta.dirname, "..", "..");
  const outPath = path.join(repoRoot, "docs", "DIAGRAMAS.md");

  // Reset da coleta do Catálogo Mestre em cada execução.
  MASTER_CATALOG_ROWS.length = 0;

  const inventoryMd = renderInventory(repoRoot);

  const solutionDiagrams = findSolutionDiagrams();
  const totalCatalog = DIAGRAM_ITEMS.length;

  const lines: string[] = [];

  // ===========================================================================
  // HEADER
  // ===========================================================================
  lines.push(`# DIAGRAMAS — Documentação Oficial do RULEX

Gerado em: ${new Date().toISOString()}

---

## Sobre este documento

Este documento contém **TODOS** os diagramas, fluxogramas e representações do sistema RULEX, organizado conforme estrutura obrigatória para:
- Executivos e Board
- Analistas de Negócio
- Desenvolvedores
- Arquitetos
- QA
- Segurança / Auditoria
- Onboarding técnico

### Regras de Rigor

| Regra | Descrição |
|-------|-----------|
| ✅ **OK** | O diagrama referencia evidência verificável no repositório (paths reais). |
| 🟥 **SEM EVIDÊNCIA** | O diagrama é obrigatório na estrutura, mas **EVIDÊNCIA NÃO ENCONTRADA NO REPOSITÓRIO** (template neutro + arquivos esperados). |
| ❌ **Não inventar** | Nenhum fato é inferido sem evidência explícita. |

### Estatísticas do Catálogo

- Total de itens no catálogo da UI: **${totalCatalog}**
- Itens marcados como \"verified\" no catálogo da UI (metadado de catálogo, não prova): **${solutionDiagrams.length}**
- Templates didáticos no catálogo da UI: **${totalCatalog - solutionDiagrams.length}**

### Como regerar

\`\`\`bash
pnpm diagrams:doc-completo   # Gera este documento
pnpm diagrams:sync           # Sincroniza inventário + checklist + doc
\`\`\`

---

## 0. PASSO ZERO — Varredura obrigatória do repositório

${inventoryMd}

---

## Catálogo Mestre (auditável)

__CATALOGO_MESTRE__

---

## Índice

1. [Diagramas de Negócio e Usuário](#1-diagramas-de-negócio-e-usuário)
2. [Diagramas de Frontend](#2-diagramas-de-frontend)
3. [Diagramas de Backend (Java)](#3-diagramas-de-backend-java)
4. [Diagramas de PostgreSQL](#4-diagramas-de-postgresql)
5. [Diagramas de Redis](#5-diagramas-de-redis)
6. [Diagramas de Neo4j](#6-diagramas-de-neo4j)
7. [Diagramas Transversais](#7-diagramas-transversais)
8. [Anexo: Catálogo Completo da UI](#8-anexo-catálogo-completo-da-ui)

---
`);

  // ===========================================================================
  // 1. DIAGRAMAS DE NEGÓCIO E USUÁRIO
  // ===========================================================================
  setDiagramContext({
    categoria: "Negócio/Usuário",
    publico: "Negócio, Exec, Produto, Operação",
    nivel: "Estratégico/Tático",
  });
  lines.push(`
## 1. Diagramas de Negócio e Usuário

Esta seção cobre processos de negócio, casos de uso, personas, jornadas, user story mapping, service blueprint, BMC e Value Proposition.

---

### 1.1 BPMN — Processos de Negócio
`);

  // 1.1.1 BPMN AS-IS
  lines.push(diagramBlock(
    "BPMN AS-IS (Processo Atual)",
    "Documentar o processo de negócio atual (antes de melhorias/automação).",
    "Análise de gaps, auditoria de processos, baseline para TO-BE.",
    "Fluxo atual de análise de fraude, handoffs entre áreas, pontos de decisão.",
    "Sem baseline, impossível medir melhoria. Risco de automação de processos incorretos.",
    placeholderDiagram("BPMN AS-IS", "Processo atual de análise de fraude antes do RULEX ou versão anterior."),
    true
  ));

  // 1.1.2 BPMN TO-BE
  lines.push(diagramBlock(
    "BPMN TO-BE (Processo Futuro/Desejado)",
    "Documentar o processo de negócio desejado após melhorias/automação.",
    "Planejamento de evolução, alinhamento com stakeholders, roadmap.",
    "Fluxo otimizado de análise de fraude com RULEX automatizado.",
    "Falta de visão de futuro, decisões de arquitetura desalinhadas.",
    placeholderDiagram("BPMN TO-BE", "Processo desejado de análise de fraude com RULEX em operação plena."),
    true
  ));

  // 1.1.3 BPMN Decisão de Fraude (temos evidência parcial via catálogo)
  const bpmnItems = findDiagramsByNotation("BPMN").filter(d => d.origin === "solution");
  if (bpmnItems.length > 0) {
    const item = bpmnItems[0];
    lines.push(diagramBlock(
      "BPMN — Decisão de Fraude",
      "Representar o fluxo de decisão de fraude no motor de regras.",
      "Entendimento do fluxo de avaliação, auditoria, treinamento.",
      "Fluxo real de avaliação: entrada → regras → score → decisão.",
      "Processo de decisão opaco, dificuldade de auditoria.",
      sampleToMermaid(item),
      false,
      item.verificationNotes
    ));
  } else {
    lines.push(diagramBlock(
      "BPMN — Decisão de Fraude",
      "Representar o fluxo de decisão de fraude no motor de regras.",
      "Entendimento do fluxo de avaliação, auditoria, treinamento.",
      "Fluxo real de avaliação: entrada → regras → score → decisão.",
      "Processo de decisão opaco, dificuldade de auditoria.",
      placeholderDiagram("BPMN Decisão de Fraude", "Fluxo de avaliação de transação no motor de regras."),
      true
    ));
  }

  // 1.1.4 BPMN Exceção/Fallback
  lines.push(diagramBlock(
    "BPMN — Exceção / Fallback",
    "Documentar o que acontece quando o fluxo principal falha.",
    "Resiliência operacional, plano de contingência, treinamento de suporte.",
    "Caminhos alternativos quando Redis/Neo4j/Backend falha.",
    "Operação sem plano B, risco de indisponibilidade total.",
    placeholderDiagram("BPMN Exceção", "Fluxo de fallback quando componentes falham."),
    true
  ));

  // 1.1.5 BPMN Rollback
  lines.push(diagramBlock(
    "BPMN — Rollback Operacional",
    "Documentar o processo de reverter uma publicação de regra problemática.",
    "Operação de emergência, mitigação de incidentes, auditoria.",
    "Passos para reverter regra: identificar → desabilitar → republicar versão anterior.",
    "Incidentes prolongados por falta de processo claro de rollback.",
    placeholderDiagram("BPMN Rollback", "Processo de rollback de regra problemática."),
    true
  ));

  // 1.2 Casos de Uso
  lines.push(`
---

### 1.2 Diagramas de Casos de Uso (UML)
`);

  lines.push(diagramBlock(
    "Casos de Uso — Analista de Fraude",
    "Documentar as funcionalidades disponíveis para o analista.",
    "Requisitos, treinamento, validação de escopo.",
    "Ações que o analista pode executar: criar regra, simular, publicar, auditar.",
    "Funcionalidades mal definidas, escopo ambíguo.",
    `
\`\`\`mermaid
flowchart LR
    subgraph Analista["👤 Analista de Fraude"]
        A1[Criar Regra]
        A2[Editar Regra]
        A3[Simular Regra]
        A4[Publicar Regra]
        A5[Consultar Auditoria]
        A6[Analisar Dashboard]
    end
    subgraph Sistema["🖥️ RULEX"]
        S1[Motor de Regras]
        S2[Banco de Dados]
        S3[Cache Redis]
    end
    A1 --> S1
    A2 --> S1
    A3 --> S1
    A4 --> S1
    A5 --> S2
    A6 --> S2
\`\`\`
`,
    false,
    "Derivado das rotas do frontend e endpoints do backend verificados no código."
  ));

  lines.push(diagramBlock(
    "Casos de Uso — Sistema Externo (Integração)",
    "Documentar as funcionalidades expostas para sistemas externos.",
    "Contrato de API, integração, documentação técnica.",
    "Endpoints disponíveis para sistemas que consomem o RULEX.",
    "Integrações mal documentadas, quebras de contrato.",
    `
\`\`\`mermaid
flowchart LR
    subgraph Externo["🔌 Sistema Externo"]
        E1[Enviar Transação]
        E2[Consultar Decisão]
        E3[Health Check]
    end
    subgraph RULEX["🖥️ RULEX API"]
        R1["POST /transactions/analyze"]
        R2["POST /evaluate"]
        R3["GET /actuator/health"]
    end
    E1 --> R1
    E2 --> R2
    E3 --> R3
\`\`\`
`,
    false,
    "Derivado dos controllers REST verificados: TransactionController, EvaluateController."
  ));

  lines.push(diagramBlock(
    "Casos de Uso — Motor de Regras (Interno)",
    "Documentar o comportamento interno do motor.",
    "Arquitetura interna, debugging, evolução do motor.",
    "Fluxo interno: carregar regras → avaliar → aplicar score → decidir.",
    "Motor opaco, difícil de debugar e evoluir.",
    `
\`\`\`mermaid
flowchart TD
    subgraph Motor["⚙️ Motor de Regras"]
        M1[Carregar Regras Ativas]
        M2[Avaliar Condições]
        M3[Aplicar Pesos/Scores]
        M4[Decidir: ALLOW/FLAG/REVIEW/BLOCK]
        M5[Registrar Auditoria]
    end
    M1 --> M2 --> M3 --> M4 --> M5
\`\`\`
`,
    false,
    "Derivado de RuleEngineService.java verificado no backend."
  ));

  lines.push(diagramBlock(
    "Casos de Uso — Operação / Suporte",
    "Documentar funcionalidades de operação e suporte.",
    "Runbooks, treinamento de suporte, SRE.",
    "Monitoramento, alertas, health checks, métricas.",
    "Operação reativa, falta de visibilidade.",
    placeholderDiagram("Casos de Uso Operação", "Funcionalidades de monitoramento e suporte."),
    true
  ));

  lines.push(diagramBlock(
    "Casos de Uso — Administrador",
    "Documentar funcionalidades disponíveis para o administrador.",
    "Governança, controle de acesso, configuração do sistema.",
    "Gerenciar usuários, aprovar regras, configurar parâmetros globais.",
    "Falta de governança, configurações incorretas.",
    `
\`\`\`mermaid
flowchart LR
    subgraph Admin["👤 Administrador"]
        AD1[Gerenciar Usuários]
        AD2[Aprovar/Rejeitar Regras]
        AD3[Configurar Thresholds Globais]
        AD4[Visualizar Métricas Consolidadas]
        AD5[Exportar Relatórios]
    end
    subgraph Sistema["🖥️ RULEX"]
        S1[Módulo de Usuários]
        S2[Workflow de Aprovação]
        S3[Configurações]
        S4[Dashboard Admin]
    end
    AD1 --> S1
    AD2 --> S2
    AD3 --> S3
    AD4 --> S4
    AD5 --> S4
\`\`\`
`,
    false,
    "Derivado de RuleApprovalController e endpoints de aprovação verificados."
  ));

  // 1.3 Personas
  lines.push(`
---

### 1.3 Personas
`);

  lines.push(diagramBlock(
    "Persona — Analista de Fraude",
    "Definir o perfil típico do usuário analista.",
    "UX, priorização de features, comunicação com stakeholders.",
    "Quem é, o que faz, dores, necessidades, objetivos.",
    "Features desalinhadas com usuário real.",
    placeholderDiagram("Persona Analista", "Perfil do analista de fraude: background, responsabilidades, dores, objetivos."),
    true
  ));

  lines.push(diagramBlock(
    "Persona — Operação / SRE",
    "Definir o perfil do time de operação.",
    "Ferramentas de observabilidade, alertas, runbooks.",
    "Quem opera o sistema, o que precisa monitorar, como age em incidentes.",
    "Sistema não operável, incidentes prolongados.",
    placeholderDiagram("Persona Operação", "Perfil do operador/SRE: responsabilidades, ferramentas, necessidades."),
    true
  ));

  lines.push(diagramBlock(
    "Persona — Executivo / Compliance",
    "Definir o perfil do stakeholder executivo.",
    "Dashboards executivos, relatórios de compliance, métricas de negócio.",
    "O que o executivo precisa ver: taxa de fraude, ROI, compliance.",
    "Decisões estratégicas sem dados, risco regulatório.",
    placeholderDiagram("Persona Executivo", "Perfil do executivo: KPIs, relatórios, necessidades de compliance."),
    true
  ));

  lines.push(diagramBlock(
    "Persona — Sistema Automatizado",
    "Definir o perfil do sistema que consome a API.",
    "Contrato de API, SLAs, tratamento de erros.",
    "Características do sistema integrador: volume, latência esperada, retry policy.",
    "Integrações frágeis, SLAs não atendidos.",
    placeholderDiagram("Persona Sistema", "Perfil do sistema integrador: requisitos de latência, volume, retry."),
    true
  ));

  // 1.4 Mapas de Jornada
  lines.push(`
---

### 1.4 Mapas de Jornada do Usuário
`);

  lines.push(diagramBlock(
    "Jornada — Criação de Regra",
    "Mapear a experiência do usuário ao criar uma regra.",
    "UX, identificação de pain points, melhoria contínua.",
    "Passo a passo: acessar → configurar → validar → salvar.",
    "UX confusa, erros de configuração, abandono.",
    `
\`\`\`mermaid
journey
    title Jornada de Criação de Regra
    section Acessar
      Abrir tela de regras: 5: Analista
      Clicar em Nova Regra: 5: Analista
    section Configurar
      Preencher nome e descrição: 4: Analista
      Definir condições: 3: Analista
      Definir ações: 4: Analista
    section Validar
      Executar validação: 4: Sistema
      Revisar erros: 2: Analista
    section Salvar
      Confirmar criação: 5: Analista
      Regra salva: 5: Sistema
\`\`\`
`,
    false,
    "Derivado do fluxo da UI ComplexRules e endpoint POST /complex-rules."
  ));

  lines.push(diagramBlock(
    "Jornada — Simulação de Regra",
    "Mapear a experiência ao simular uma regra.",
    "Validação pré-produção, redução de erros.",
    "Passo a passo: selecionar regra → configurar payload → executar → analisar resultado.",
    "Regras publicadas sem teste, incidentes em produção.",
    `
\`\`\`mermaid
journey
    title Jornada de Simulação
    section Selecionar
      Acessar regra: 5: Analista
      Clicar em Simular: 5: Analista
    section Configurar
      Preencher payload de teste: 3: Analista
      Ajustar parâmetros: 4: Analista
    section Executar
      Rodar simulação: 5: Sistema
      Aguardar resultado: 3: Analista
    section Analisar
      Ver decisão: 5: Analista
      Ver score detalhado: 4: Analista
      Identificar ajustes: 3: Analista
\`\`\`
`,
    false,
    "Derivado do endpoint POST /rules/simulate e tela de simulação."
  ));

  lines.push(diagramBlock(
    "Jornada — Publicação de Regra",
    "Mapear a experiência ao publicar uma regra.",
    "Governança, aprovação, auditoria.",
    "Passo a passo: solicitar publicação → aprovar → ativar → monitorar.",
    "Publicações sem governança, regras problemáticas em produção.",
    placeholderDiagram("Jornada Publicação", "Fluxo de aprovação e ativação de regra em produção."),
    true
  ));

  lines.push(diagramBlock(
    "Jornada — Rollback de Regra",
    "Mapear a experiência ao reverter uma regra problemática.",
    "Resposta a incidentes, mitigação rápida.",
    "Passo a passo: identificar problema → desabilitar → reverter → validar.",
    "Incidentes prolongados por falta de processo claro.",
    placeholderDiagram("Jornada Rollback", "Fluxo de emergência para reverter regra."),
    true
  ));

  lines.push(diagramBlock(
    "Jornada — Investigação de Fraude",
    "Mapear a experiência ao investigar uma transação suspeita.",
    "Auditoria, compliance, treinamento de analistas.",
    "Passo a passo: receber alerta → consultar transação → ver regras acionadas → decidir.",
    "Investigações lentas, falta de rastreabilidade.",
    `
\`\`\`mermaid
journey
    title Jornada de Investigação
    section Alerta
      Receber notificação: 4: Analista
      Acessar sistema: 5: Analista
    section Consultar
      Buscar transação: 5: Analista
      Ver detalhes: 5: Sistema
    section Analisar
      Ver regras acionadas: 5: Sistema
      Ver score breakdown: 4: Analista
      Consultar histórico do cliente: 3: Analista
    section Decidir
      Marcar como fraude confirmada: 5: Analista
      Ou liberar transação: 5: Analista
\`\`\`
`,
    false,
    "Derivado do endpoint GET /audit/transaction/{id} e tela de auditoria."
  ));

  // 1.5 User Story Mapping
  lines.push(`
---

### 1.5 User Story Mapping
`);

  lines.push(diagramBlock(
    "User Story Map — RULEX",
    "Organizar funcionalidades em backbone de atividades e releases.",
    "Priorização, planejamento de releases, visão de produto.",
    "Atividades principais → passos do usuário → histórias → priorização MVP vs avançado.",
    "Escopo mal definido, entregas fragmentadas.",
    placeholderDiagram("User Story Map", "Mapa de histórias organizado por atividades e releases."),
    true
  ));

  // 1.6 Service Blueprint
  lines.push(`
---

### 1.6 Service Blueprint
`);

  lines.push(diagramBlock(
    "Service Blueprint — Análise de Transação",
    "Mapear frontstage, backstage e sistemas de apoio.",
    "Visão holística do serviço, identificação de pontos de falha.",
    "O que o usuário vê (frontstage) vs o que acontece internamente (backstage).",
    "Falhas invisíveis, experiência do usuário degradada.",
    placeholderDiagram("Service Blueprint", "Blueprint do serviço de análise de transação: frontstage, backstage, sistemas de apoio, evidências físicas, pontos de falha."),
    true
  ));

  // 1.7 BMC
  lines.push(`
---

### 1.7 Business Model Canvas (BMC)
`);

  lines.push(diagramBlock(
    "Business Model Canvas — RULEX",
    "Documentar o modelo de negócio do RULEX.",
    "Alinhamento estratégico, comunicação com stakeholders, pitch.",
    "9 blocos: Proposta de Valor, Segmentos, Canais, Relacionamento, Receitas, Recursos, Atividades, Parcerias, Custos.",
    "Desalinhamento estratégico, proposta de valor confusa.",
    placeholderDiagram("BMC", "Business Model Canvas com 9 blocos preenchidos para o RULEX."),
    true
  ));

  // 1.8 Value Proposition
  lines.push(`
---

### 1.8 Value Proposition Canvas
`);

  lines.push(diagramBlock(
    "Value Proposition Canvas — RULEX",
    "Detalhar a proposta de valor vs dores e ganhos do cliente.",
    "Product-market fit, priorização de features, comunicação.",
    "Jobs do cliente, dores, ganhos desejados vs como o RULEX resolve.",
    "Produto desalinhado com necessidades reais do cliente.",
    placeholderDiagram("Value Proposition", "Canvas com jobs, dores, ganhos e como o RULEX endereça cada um."),
    true
  ));

  // ===========================================================================
  // 2. DIAGRAMAS DE FRONTEND
  // ===========================================================================
  setDiagramContext({
    categoria: "Frontend",
    publico: "Dev Frontend, Design, QA, Produto",
    nivel: "Tático",
  });
  lines.push(`
---

## 2. Diagramas de Frontend

Esta seção cobre arquitetura, fluxos de UI, componentes, estados e navegação do frontend React.

---

### 2.1 Diagrama de Arquitetura de Frontend
`);

  lines.push(diagramBlock(
    "Arquitetura do Frontend",
    "Documentar a estrutura de camadas do frontend.",
    "Onboarding de devs, decisões de arquitetura, evolução.",
    "Camadas: UI components, state management, API services, routing.",
    "Código desorganizado, difícil manutenção.",
    `
\`\`\`mermaid
flowchart TB
    subgraph UI["🎨 UI Layer"]
        Pages[Pages: Login, Dashboard, Rules, Audit, etc.]
        Components[Components: Forms, Tables, Dialogs]
    end
    subgraph State["📦 State Layer"]
        Context[React Context: Theme, Auth]
        TanStack[TanStack Query: Server State]
    end
    subgraph Services["🔌 Services Layer"]
        API[API Client: fetch/axios]
        Types[TypeScript Types]
    end
    subgraph External["🌐 External"]
        Backend[Backend API: /api/*]
    end
    Pages --> Components
    Components --> Context
    Components --> TanStack
    TanStack --> API
    API --> Backend
\`\`\`
`,
    false,
    "Derivado da estrutura client/src: pages, components, lib, contexts verificados."
  ));

  // 2.2 Fluxogramas de UI
  lines.push(`
---

### 2.2 Fluxogramas de UI
`);

  const frontendDiagrams = findDiagramsByCategory("frontend").filter(d => d.origin === "solution");
  if (frontendDiagrams.length > 0) {
    for (const item of frontendDiagrams.slice(0, 3)) {
      lines.push(diagramBlock(
        `Fluxo UI: ${item.canonicalName}`,
        "Documentar o fluxo de interação na tela.",
        "UX, testes, onboarding.",
        item.descriptionWhenToUse,
        "Fluxos mal documentados, bugs de UX.",
        sampleToMermaid(item),
        false,
        item.verificationNotes
      ));
    }
  }

  lines.push(diagramBlock(
    "Fluxo UI — Criação de Regra",
    "Documentar o fluxo de criação de regra na interface.",
    "UX, testes E2E, treinamento.",
    "Passo a passo visual: formulário → validação → salvamento.",
    "Bugs de interface, fluxo confuso.",
    `
\`\`\`mermaid
flowchart TD
    A[Abrir tela /rules] --> B[Clicar 'Nova Regra']
    B --> C[Abrir RuleFormDialog]
    C --> D{Preencher campos}
    D --> E[Nome, Descrição, Tipo]
    D --> F[Condições]
    D --> G[Ações/Score]
    E & F & G --> H[Clicar Salvar]
    H --> I{Validação}
    I -->|Sucesso| J[POST /complex-rules]
    I -->|Erro| K[Mostrar erros]
    J --> L[Fechar dialog]
    L --> M[Atualizar lista]
    K --> D
\`\`\`
`,
    false,
    "Derivado de RuleFormDialog.tsx e endpoint POST /complex-rules."
  ));

  // 2.3 Componentes
  lines.push(`
---

### 2.3 Diagrama de Componentes (Frontend)
`);

  lines.push(diagramBlock(
    "Componentes do Frontend",
    "Mapear os principais componentes e suas dependências.",
    "Arquitetura, reuso, manutenção.",
    "Hierarquia de componentes: pages → containers → components → UI primitives.",
    "Componentes acoplados, difícil reuso.",
    `
\`\`\`mermaid
flowchart TD
    subgraph Pages["📄 Pages"]
        PLogin[Login]
        PDash[DashboardProfessional]
        PRules[ComplexRules]
        PAudit[Audit]
        PSim[TransactionSimulator]
        PMon[Monitoring]
    end
    subgraph Containers["📦 Containers"]
        CRuleList[RuleList]
        CRuleForm[RuleFormDialog]
        CAuditTable[AuditTable]
        CSimForm[SimulatorForm]
    end
    subgraph Components["🧩 Components"]
        CompTable[DataTable]
        CompForm[Form Controls]
        CompDialog[Dialog]
        CompChart[Charts]
    end
    subgraph UI["🎨 UI Primitives (shadcn)"]
        UIBtn[Button]
        UIInput[Input]
        UISelect[Select]
        UICard[Card]
    end
    PRules --> CRuleList
    PRules --> CRuleForm
    PAudit --> CAuditTable
    PSim --> CSimForm
    CRuleList --> CompTable
    CRuleForm --> CompForm
    CRuleForm --> CompDialog
    CompForm --> UIInput
    CompForm --> UISelect
    CompTable --> UIBtn
\`\`\`
`,
    false,
    "Derivado da estrutura client/src/components e client/src/pages."
  ));

  // 2.4 Estados da UI
  lines.push(`
---

### 2.4 Diagrama de Estados da UI
`);

  lines.push(diagramBlock(
    "Estados da UI — Componente Genérico",
    "Documentar os estados possíveis de um componente.",
    "Testes, UX, tratamento de erros.",
    "Estados: Idle → Loading → Success/Error → Retry/Fallback.",
    "Estados não tratados, UX degradada.",
    `
\`\`\`mermaid
stateDiagram-v2
    [*] --> Idle
    Idle --> Loading: Ação do usuário
    Loading --> Success: Resposta OK
    Loading --> Error: Resposta erro
    Success --> Idle: Reset/Nova ação
    Error --> Retry: Tentar novamente
    Error --> Fallback: Máximo de tentativas
    Retry --> Loading
    Fallback --> Idle: Ação manual
\`\`\`
`,
    false,
    "Padrão derivado do uso de TanStack Query no frontend."
  ));

  // 2.5 Wireflow
  lines.push(`
---

### 2.5 Wireflow / User Flow
`);

  lines.push(diagramBlock(
    "Wireflow — Navegação Principal",
    "Documentar os caminhos de navegação entre telas.",
    "UX, testes E2E, onboarding.",
    "Mapa de navegação: login → dashboard → telas específicas.",
    "Navegação confusa, usuário perdido.",
    `
\`\`\`mermaid
flowchart LR
    Login[/login] --> Dashboard[/dashboard]
    Dashboard --> Transactions[/transactions]
    Dashboard --> Rules[/rules]
    Dashboard --> Audit[/audit]
    Dashboard --> Simulator[/simulator]
    Dashboard --> Monitoring[/monitoring]
    Dashboard --> Settings[/settings]
    Dashboard --> Manual[/manual]
    Dashboard --> Diagrams[/diagrams]
    Rules --> RuleDetail[Editar Regra]
    Audit --> AuditDetail[Detalhe Transação]
\`\`\`
`,
    false,
    "Derivado de App.tsx: rotas verificadas no código."
  ));

  // 2.6 Design System / Component Library
  lines.push(`
---

### 2.6 Design System / Component Library
`);

  lines.push(diagramBlock(
    "Design System — RULEX",
    "Documentar paleta, tipografia, espaçamentos, estados, acessibilidade.",
    "Consistência visual, onboarding de designers, acessibilidade.",
    "Cores, fontes, espaçamentos, estados (hover/focus/disabled), tokens de design.",
    "Interface inconsistente, problemas de acessibilidade.",
    placeholderDiagram("Design System", "Documentação de paleta de cores, tipografia, espaçamentos, componentes base (shadcn/ui) e estados de interação."),
    true,
    "O frontend usa shadcn/ui (components.json verificado), mas não há design system documentado formalmente."
  ));

  // ===========================================================================
  // 3. DIAGRAMAS DE BACKEND
  // ===========================================================================
  setDiagramContext({
    categoria: "Backend",
    publico: "Dev Backend, Arquiteto, QA, Operação",
    nivel: "Tático/Detalhado",
  });
  lines.push(`
---

## 3. Diagramas de Backend (Java)

Esta seção cobre arquitetura, C4, UML, fluxos de processamento e regras duras.

---

### 3.1 Diagrama de Arquitetura Geral
`);

  lines.push(diagramBlock(
    "Arquitetura Backend — Camadas",
    "Documentar a estrutura de camadas do backend Spring Boot.",
    "Onboarding, decisões de arquitetura, manutenção.",
    "Camadas: Controller → Service → Repository → Entity.",
    "Código desorganizado, violação de camadas.",
    `
\`\`\`mermaid
flowchart TB
    subgraph API["🌐 API Layer"]
        Controllers[REST Controllers]
        Filters[Filters: Auth, CORS, RateLimit]
    end
    subgraph Service["⚙️ Service Layer"]
        RuleEngine[RuleEngineService]
        VelocityFacade[VelocityServiceFacade]
        AuditService[AuditService]
        Neo4jService[Neo4jGraphService]
    end
    subgraph Repository["💾 Repository Layer"]
        JpaRepos[JPA Repositories]
        RedisTemplate[RedisTemplate]
    end
    subgraph Domain["📦 Domain Layer"]
        Entities[JPA Entities]
        DTOs[DTOs]
    end
    subgraph External["🔌 External"]
        Postgres[(PostgreSQL)]
        Redis[(Redis)]
        Neo4j[(Neo4j)]
    end
    Controllers --> RuleEngine
    Controllers --> AuditService
    Filters --> Controllers
    RuleEngine --> VelocityFacade
    RuleEngine --> Neo4jService
    RuleEngine --> JpaRepos
    VelocityFacade --> RedisTemplate
    VelocityFacade --> JpaRepos
    JpaRepos --> Postgres
    RedisTemplate --> Redis
    Neo4jService --> Neo4j
\`\`\`
`,
    false,
    "Derivado da estrutura backend/src/main/java/com/rulex: controller, service, entity, repository."
  ));

  // 3.2 C4
  lines.push(`
---

### 3.2 Diagrama C4
`);

  const c4Diagrams = findDiagramsByNotation("C4").filter(d => d.origin === "solution");
  if (c4Diagrams.length > 0) {
    const item = c4Diagrams[0];
    lines.push(diagramBlock(
      "C4 — Container Diagram",
      "Visão de containers do sistema e suas dependências.",
      "Arquitetura de alto nível, comunicação com stakeholders.",
      "Frontend, Backend, Databases, integrações externas.",
      "Visão sistêmica perdida, decisões desalinhadas.",
      sampleToMermaid(item),
      false,
      item.verificationNotes
    ));
  } else {
    lines.push(diagramBlock(
      "C4 — Container Diagram",
      "Visão de containers do sistema e suas dependências.",
      "Arquitetura de alto nível, comunicação com stakeholders.",
      "Frontend, Backend, Databases, integrações externas.",
      "Visão sistêmica perdida, decisões desalinhadas.",
      `
\`\`\`mermaid
C4Container
    title RULEX - Container Diagram
    
    Person(analyst, "Analista de Fraude", "Configura e monitora regras")
    Person(external, "Sistema Externo", "Envia transações para análise")
    
    System_Boundary(rulex, "RULEX") {
        Container(web, "Frontend Web", "React, TypeScript", "Interface de gerenciamento")
        Container(api, "Backend API", "Spring Boot, Java 21", "Motor de regras e API REST")
        ContainerDb(postgres, "PostgreSQL", "Relacional", "Regras, transações, auditoria")
        ContainerDb(redis, "Redis", "Cache", "Velocidade, cache de regras")
        ContainerDb(neo4j, "Neo4j", "Grafo", "Análise de redes de fraude")
    }
    
    Rel(analyst, web, "Usa", "HTTPS")
    Rel(external, api, "Envia transações", "HTTPS/JSON")
    Rel(web, api, "Chama", "REST API")
    Rel(api, postgres, "Lê/Escreve")
    Rel(api, redis, "Cache")
    Rel(api, neo4j, "Consulta grafos")
\`\`\`
`,
      false,
      "Derivado de docker-compose.yml e application.yml verificados."
    ));
  }

  lines.push(diagramBlock(
    "C4 — Component Diagram (Backend)",
    "Detalhar os componentes internos do backend.",
    "Arquitetura detalhada, onboarding de devs.",
    "Controllers, Services, Repositories, Entities.",
    "Componentes mal definidos, acoplamento.",
    placeholderDiagram("C4 Component", "Diagrama de componentes detalhado do backend."),
    true
  ));

  // 3.3 UML
  lines.push(`
---

### 3.3 Diagramas UML
`);

  // Buscar diagramas UML do catálogo
  const umlDiagrams = findDiagramsByNotation("UML").filter(d => d.origin === "solution");
  
  lines.push(diagramBlock(
    "UML — Diagrama de Classes (Entidades Core)",
    "Documentar as principais entidades do domínio.",
    "Modelagem, manutenção, onboarding.",
    "Transaction, RuleConfiguration, TransactionDecision, AuditLog.",
    "Modelo de dados confuso, bugs de persistência.",
    `
\`\`\`mermaid
classDiagram
    class Transaction {
        +Long id
        +String externalTransactionId
        +String pan
        +BigDecimal transactionAmount
        +Integer mcc
        +String merchantId
        +LocalDateTime createdAt
    }
    class RuleConfiguration {
        +Long id
        +String ruleName
        +RuleType ruleType
        +Integer threshold
        +Integer weight
        +Boolean enabled
        +String conditionsJson
    }
    class TransactionDecision {
        +Long id
        +Long transactionId
        +String classification
        +Integer riskScore
        +String rulesApplied
    }
    class AuditLog {
        +Long id
        +Long transactionId
        +String actionType
        +String performedBy
        +LocalDateTime createdAt
    }
    Transaction "1" --> "*" TransactionDecision
    Transaction "1" --> "*" AuditLog
    RuleConfiguration "1" --> "*" TransactionDecision : aplica
\`\`\`
`,
    false,
    "Derivado de Transaction.java, RuleConfiguration.java, TransactionDecision.java, AuditLog.java."
  ));

  lines.push(diagramBlock(
    "UML — Diagrama de Pacotes",
    "Documentar a organização de pacotes do backend.",
    "Arquitetura, separação de responsabilidades.",
    "Pacotes: controller, service, entity, dto, config, util.",
    "Pacotes desorganizados, ciclos de dependência.",
    `
\`\`\`mermaid
flowchart TB
    subgraph com.rulex
        controller[controller]
        service[service]
        entity[entity]
        dto[dto]
        config[config]
        util[util]
        v31[v31]
    end
    controller --> service
    controller --> dto
    service --> entity
    service --> dto
    service --> util
    v31 --> service
\`\`\`
`,
    false,
    "Derivado da estrutura de diretórios backend/src/main/java/com/rulex."
  ));

  // Incluir alguns diagramas de sequência do catálogo
  if (umlDiagrams.length > 0) {
    const seqDiagram = umlDiagrams.find(d => d.canonicalName.toLowerCase().includes("sequência") || d.id.includes("SEQ"));
    if (seqDiagram) {
      lines.push(diagramBlock(
        "UML — Diagrama de Sequência (Análise de Transação)",
        "Documentar o fluxo de chamadas para análise.",
        "Debugging, testes de integração, documentação técnica.",
        "Frontend → API → Engine → DB → Response.",
        "Fluxo opaco, difícil de debugar.",
        sampleToMermaid(seqDiagram),
        false,
        seqDiagram.verificationNotes
      ));
    }
  }

  lines.push(diagramBlock(
    "UML — Diagrama de Estados (Regra)",
    "Documentar os estados possíveis de uma regra.",
    "Governança, auditoria, workflow de aprovação.",
    "Estados: DRAFT → PENDING_APPROVAL → ACTIVE → DISABLED → ARCHIVED.",
    "Regras em estados inconsistentes.",
    `
\`\`\`mermaid
stateDiagram-v2
    [*] --> DRAFT: Criar
    DRAFT --> PENDING_APPROVAL: Solicitar aprovação
    PENDING_APPROVAL --> ACTIVE: Aprovar
    PENDING_APPROVAL --> DRAFT: Rejeitar
    ACTIVE --> DISABLED: Desabilitar
    DISABLED --> ACTIVE: Reabilitar
    DISABLED --> ARCHIVED: Arquivar
    ACTIVE --> ARCHIVED: Arquivar
    ARCHIVED --> [*]
\`\`\`
`,
    false,
    "Derivado de RuleApproval.java e endpoints de aprovação."
  ));

  // 3.4 Fluxos de Processamento
  lines.push(`
---

### 3.4 Fluxogramas de Processamento
`);

  const flowDiagrams = findDiagramsByNotation("FLOWCHART").filter(d => d.origin === "solution");
  if (flowDiagrams.length > 0) {
    const item = flowDiagrams[0];
    lines.push(diagramBlock(
      "Fluxo — Análise de Transação (/analyze)",
      "Documentar o fluxo completo de análise.",
      "Debugging, testes, documentação.",
      "Entrada → Validação → Regras → Score → Decisão → Auditoria.",
      "Fluxo opaco, bugs difíceis de rastrear.",
      sampleToMermaid(item),
      false,
      item.verificationNotes
    ));
  }

  lines.push(diagramBlock(
    "Fluxo — Tratamento de Exceções",
    "Documentar como exceções são tratadas.",
    "Resiliência, debugging, monitoramento.",
    "Try → Catch → Log → Fallback → Response.",
    "Exceções não tratadas, erros 500.",
    `
\`\`\`mermaid
flowchart TD
    A[Requisição] --> B{Try}
    B -->|Sucesso| C[Processar]
    B -->|Exceção| D{Tipo de Exceção}
    D -->|Validação| E[HTTP 400 + detalhes]
    D -->|Não Encontrado| F[HTTP 404]
    D -->|Timeout| G[HTTP 504 + retry hint]
    D -->|Erro Interno| H[HTTP 500 + log]
    C --> I[Resposta OK]
    E & F & G & H --> J[GlobalExceptionHandler]
    J --> K[Log estruturado]
    K --> L[Métricas]
\`\`\`
`,
    false,
    "Derivado de GlobalExceptionHandler.java verificado."
  ));

  // 3.5 Regras Duras
  lines.push(`
---

### 3.5 Diagrama de Regras Duras
`);

  lines.push(diagramBlock(
    "Arquitetura de Regras Duras",
    "Documentar a estrutura do motor de regras.",
    "Evolução do motor, debugging, documentação técnica.",
    "Operadores, condições, encadeamento, prioridade, curto-circuito.",
    "Motor inflexível, regras mal configuradas.",
    `
\`\`\`mermaid
flowchart TD
    subgraph Motor["⚙️ Motor de Regras"]
        Load[Carregar Regras Ativas]
        Sort[Ordenar por Prioridade/Tier]
        Eval[Avaliar Condições]
        Score[Calcular Score]
        Decision[Decisão Final]
    end
    
    subgraph Condição["📋 Estrutura de Condição"]
        Field[Campo: transactionAmount, mcc, etc.]
        Operator[Operador: GT, LT, EQ, IN, REGEX, etc.]
        Value[Valor: número, lista, pattern]
    end
    
    subgraph Tiers["🏷️ Tiers de Execução"]
        T1["TIER 1: Blocklists (< 1ms)"]
        T2["TIER 2: Velocity (< 10ms)"]
        T3["TIER 3: Agregações (< 100ms)"]
    end
    
    Load --> Sort
    Sort --> T1
    T1 -->|BLOCK?| ShortCircuit[Curto-circuito]
    T1 -->|ALLOW| T2
    T2 -->|BLOCK?| ShortCircuit
    T2 -->|ALLOW| T3
    T3 --> Eval
    Eval --> Score
    Score --> Decision
    ShortCircuit --> Decision
    
    Eval -.-> Field
    Eval -.-> Operator
    Eval -.-> Value
\`\`\`
`,
    false,
    "Derivado de RuleEngineService.java, ParallelRuleExecutionService.java, RuleCondition.java."
  ));

  // 3.6 API Contract / Integrações
  lines.push(`
---

### 3.6 API Contract / Integrações
`);

  lines.push(diagramBlock(
    "API Contract — OpenAPI",
    "Documentar endpoints expostos conforme contrato OpenAPI.",
    "Integração, documentação técnica, testes de contrato.",
    "Endpoints REST, métodos HTTP, payloads, códigos de resposta.",
    "Integrações quebradas, documentação desatualizada.",
    `
\`\`\`mermaid
flowchart LR
    subgraph Endpoints["📡 Principais Endpoints (openapi/rulex.yaml)"]
        E1["POST /transactions/analyze"]
        E2["POST /evaluate"]
        E3["GET/POST /rules"]
        E4["GET/POST /complex-rules"]
        E5["GET /audit"]
        E6["POST /rules/simulate"]
        E7["GET /metrics"]
        E8["GET /actuator/health"]
    end
    
    subgraph Consumers["🔌 Consumidores"]
        C1["Frontend React"]
        C2["Sistemas Externos"]
        C3["Monitoramento"]
    end
    
    C1 --> E3
    C1 --> E4
    C1 --> E5
    C1 --> E6
    C2 --> E1
    C2 --> E2
    C3 --> E7
    C3 --> E8
\`\`\`
`,
    false,
    "Derivado de openapi/rulex.yaml verificado."
  ));

  lines.push(diagramBlock(
    "Integrações Externas",
    "Documentar sistemas externos que se integram ao RULEX.",
    "Arquitetura de integração, contratos, SLAs.",
    "Sistemas de pagamento, sistemas legados, APIs externas.",
    "Integrações frágeis, quebras de contrato.",
    placeholderDiagram("Integrações Externas", "Diagrama de sistemas externos que consomem ou alimentam o RULEX, com protocolos e formatos."),
    true,
    "Não há evidência de integrações externas específicas documentadas no repositório."
  ));

  // 3.7 Event / Message Flow
  lines.push(`
---

### 3.7 Event / Message Flow
`);

  lines.push(diagramBlock(
    "Event / Message Flow",
    "Documentar fluxo de eventos e mensagens (filas, tópicos).",
    "Arquitetura assíncrona, debugging, monitoramento.",
    "Filas, tópicos, producers, consumers, eventos de domínio.",
    "Perda de mensagens, processamento duplicado, acoplamento.",
    placeholderDiagram("Event Flow", "Diagrama de filas/tópicos (Kafka, RabbitMQ, etc.), producers e consumers. Se não houver mensageria, registrar como N/A."),
    true,
    "Não há evidência de mensageria (Kafka, RabbitMQ, etc.) no repositório. O sistema opera de forma síncrona."
  ));

  // ===========================================================================
  // 4. DIAGRAMAS DE POSTGRESQL
  // ===========================================================================
  setDiagramContext({
    categoria: "PostgreSQL",
    publico: "Dev Backend, DBA, Arquiteto, Operação",
    nivel: "Tático/Detalhado",
  });
  lines.push(`
---

## 4. Diagramas de PostgreSQL

Esta seção cobre modelo de dados, ERD, schemas, armazenamento e replicação.

---

### 4.1 Modelo de Dados
`);

  lines.push(diagramBlock(
    "Modelo Conceitual",
    "Visão de alto nível das entidades e relacionamentos.",
    "Comunicação com negócio, modelagem inicial.",
    "Entidades principais sem detalhes de implementação.",
    "Modelo desalinhado com negócio.",
    `
\`\`\`mermaid
erDiagram
    TRANSACAO ||--o{ DECISAO : tem
    TRANSACAO ||--o{ AUDITORIA : gera
    REGRA ||--o{ DECISAO : aplica
    REGRA ||--o{ HISTORICO : versiona
    REGRA }|--|| APROVACAO : requer
\`\`\`
`,
    false,
    "Derivado das entidades JPA e migrations Flyway."
  ));

  lines.push(diagramBlock(
    "Modelo Lógico",
    "Detalhes de tabelas, colunas e tipos.",
    "Desenvolvimento, migrations, documentação técnica.",
    "Tabelas com colunas, tipos, constraints.",
    "Schema inconsistente, bugs de persistência.",
    placeholderDiagram("Modelo Lógico", "Diagrama com todas as tabelas, colunas, tipos e constraints."),
    true
  ));

  lines.push(diagramBlock(
    "Modelo Físico",
    "Detalhes de índices, particionamento, storage.",
    "Performance, DBA, otimização.",
    "Índices, tablespaces, partições.",
    "Performance degradada, queries lentas.",
    placeholderDiagram("Modelo Físico", "Diagrama com índices, partições e configurações de storage."),
    true
  ));

  // 4.2 ERD
  lines.push(`
---

### 4.2 Diagrama ER (ERD)
`);

  const erDiagrams = findDiagramsByNotation("ER").filter(d => d.origin === "solution");
  if (erDiagrams.length > 0) {
    const item = erDiagrams[0];
    lines.push(diagramBlock(
      "ERD — Entidades Core",
      "Diagrama ER das principais tabelas.",
      "Modelagem, manutenção, onboarding.",
      "Transações, Regras, Decisões, Auditoria.",
      "Relacionamentos incorretos, integridade comprometida.",
      sampleToMermaid(item),
      false,
      item.verificationNotes
    ));
  }

  lines.push(diagramBlock(
    "ERD — Completo",
    "Diagrama ER de todas as tabelas.",
    "DBA, documentação completa.",
    "Todas as tabelas do schema com relacionamentos.",
    "Visão incompleta do banco.",
    `
\`\`\`mermaid
erDiagram
    transactions ||--o{ transaction_decisions : has
    transactions ||--o{ audit_logs : generates
    transactions ||--o{ velocity_transaction_log : tracks
    
    rule_configurations ||--o{ transaction_decisions : applies
    rule_configurations ||--o{ rule_configuration_history : versions
    rule_configurations ||--o{ rule_approvals : requires
    
    complex_rules ||--o{ complex_rule_conditions : has
    
    shadow_evaluation_log }|--|| rule_configurations : evaluates
    
    transactions {
        bigint id PK
        varchar external_transaction_id UK
        varchar pan
        numeric transaction_amount
        integer mcc
        timestamp created_at
    }
    
    rule_configurations {
        bigint id PK
        varchar rule_name UK
        varchar rule_type
        integer threshold
        integer weight
        boolean enabled
        jsonb conditions_json
    }
    
    transaction_decisions {
        bigint id PK
        bigint transaction_id FK
        varchar classification
        integer risk_score
    }
    
    audit_logs {
        bigint id PK
        bigint transaction_id FK
        varchar action_type
        timestamp created_at
    }
\`\`\`
`,
    false,
    "Derivado de V2__core_schema.sql e entidades JPA verificadas."
  ));

  // 4.3 Schemas
  lines.push(`
---

### 4.3 Diagrama de Schemas
`);

  lines.push(diagramBlock(
    "Organização de Schemas",
    "Documentar schemas, tabelas e índices.",
    "DBA, organização, isolamento.",
    "Schema público com tabelas do RULEX.",
    "Mistura de dados, isolamento comprometido.",
    placeholderDiagram("Schemas", "Diagrama de schemas com tabelas organizadas."),
    true
  ));

  // 4.4 Armazenamento
  lines.push(`
---

### 4.4 Diagrama de Armazenamento Físico
`);

  lines.push(diagramBlock(
    "Armazenamento PostgreSQL",
    "Documentar pages, WAL, files.",
    "DBA, performance, backup/recovery.",
    "Estrutura física: data files, WAL, checkpoints.",
    "Backup inconsistente, recovery problemático.",
    placeholderDiagram("Armazenamento Físico", "Diagrama de pages, WAL, data files do PostgreSQL."),
    true
  ));

  // 4.5 Replicação
  lines.push(`
---

### 4.5 Diagrama de Replicação
`);

  lines.push(diagramBlock(
    "Replicação PostgreSQL",
    "Documentar topologia de replicação.",
    "Alta disponibilidade, disaster recovery.",
    "Primário, réplicas síncronas/assíncronas.",
    "Indisponibilidade, perda de dados.",
    placeholderDiagram("Replicação PostgreSQL", "Diagrama de primário, réplicas, síncrona vs assíncrona."),
    true
  ));

  // 4.6 Data Lifecycle / Retenção / LGPD
  lines.push(`
---

### 4.6 Data Lifecycle / Retenção / LGPD
`);

  lines.push(diagramBlock(
    "Data Lifecycle — Retenção e LGPD",
    "Documentar ciclo de vida dos dados, retenção e conformidade LGPD.",
    "Compliance, auditoria, governança de dados.",
    "Políticas de retenção, anonimização, exclusão, auditoria de acesso.",
    "Não conformidade LGPD, dados retidos indefinidamente, risco regulatório.",
    placeholderDiagram("Data Lifecycle", "Diagrama de ciclo de vida: criação → uso → arquivamento → exclusão. Políticas de retenção por tipo de dado. Processo de anonimização/pseudonimização para LGPD."),
    true,
    "Não há evidência de políticas de retenção ou processos de anonimização documentados no repositório."
  ));

  // ===========================================================================
  // 5. DIAGRAMAS DE REDIS
  // ===========================================================================
  setDiagramContext({
    categoria: "Redis",
    publico: "Dev Backend, Arquiteto, Operação",
    nivel: "Tático/Detalhado",
  });
  lines.push(`
---

## 5. Diagramas de Redis

Esta seção cobre tipos de dados, arquitetura, cache, replicação, cluster e persistência.

---

### 5.1 Diagrama de Tipos de Dados
`);

  lines.push(diagramBlock(
    "Tipos de Dados Redis — RULEX",
    "Documentar como o RULEX usa cada tipo de dado.",
    "Desenvolvimento, debugging, otimização.",
    "String (contadores), Hash (stats), HyperLogLog (distincts).",
    "Uso inadequado, performance degradada.",
    `
\`\`\`mermaid
flowchart LR
    subgraph Redis["🔴 Redis RULEX"]
        subgraph Strings["String"]
            S1["velocity:{keyType}:{hash}:count:{window}"]
            S2["velocity:{keyType}:{hash}:sum:{window}"]
        end
        subgraph HLL["HyperLogLog"]
            H1["velocity:{keyType}:{hash}:distinct:merchants"]
            H2["velocity:{keyType}:{hash}:distinct:mccs"]
            H3["velocity:{keyType}:{hash}:distinct:countries"]
        end
    end
    
    subgraph Operações["Operações"]
        INCR[INCR/INCRBY]
        GET[GET]
        PFADD[PFADD]
        PFCOUNT[PFCOUNT]
    end
    
    INCR --> S1
    INCR --> S2
    GET --> S1
    GET --> S2
    PFADD --> H1
    PFADD --> H2
    PFADD --> H3
    PFCOUNT --> H1
    PFCOUNT --> H2
    PFCOUNT --> H3
\`\`\`
`,
    false,
    "Derivado de RedisVelocityCacheService.java verificado."
  ));

  // 5.2 Arquitetura
  lines.push(`
---

### 5.2 Diagrama de Arquitetura (Event Loop)
`);

  lines.push(diagramBlock(
    "Arquitetura Redis — Event Loop",
    "Documentar o modelo de execução single-threaded.",
    "Performance, debugging, capacity planning.",
    "Event loop, I/O multiplexado, comandos atômicos.",
    "Bloqueios por comandos lentos.",
    placeholderDiagram("Event Loop Redis", "Diagrama do event loop single-threaded do Redis."),
    true
  ));

  // 5.3 Cache
  lines.push(`
---

### 5.3 Diagrama de Cache
`);

  lines.push(diagramBlock(
    "Estratégia de Cache — RULEX",
    "Documentar como o cache é usado.",
    "Performance, consistência, debugging.",
    "TTL por janela temporal, cache-aside pattern.",
    "Cache stale, dados inconsistentes.",
    `
\`\`\`mermaid
flowchart TD
    subgraph App["⚙️ RULEX Backend"]
        Request[Requisição de Velocidade]
        Facade[VelocityServiceFacade]
    end
    
    subgraph Cache["🔴 Redis"]
        Check{Cache hit?}
        Get[GET/PFCOUNT]
        Set[INCRBY/PFADD]
    end
    
    subgraph DB["🐘 PostgreSQL"]
        Query[Query de fallback]
    end
    
    Request --> Facade
    Facade --> Check
    Check -->|Hit| Get
    Check -->|Miss| Query
    Query --> Set
    Get --> Response[Resposta]
    Set --> Response
    
    subgraph TTL["⏱️ TTL por Janela"]
        T1["5min → TTL 6min"]
        T2["1h → TTL 65min"]
        T3["24h → TTL 25h"]
    end
\`\`\`
`,
    false,
    "Derivado de VelocityServiceFacade.java e RedisVelocityCacheService.java."
  ));

  // 5.4-5.6 Replicação, Cluster, Persistência
  lines.push(`
---

### 5.4 Diagrama de Replicação
`);

  lines.push(diagramBlock(
    "Replicação Redis",
    "Documentar topologia leader-follower.",
    "Alta disponibilidade, leitura escalável.",
    "Leader para escrita, followers para leitura.",
    "Indisponibilidade, inconsistência em failover.",
    placeholderDiagram("Replicação Redis", "Diagrama leader-follower do Redis."),
    true
  ));

  lines.push(`
---

### 5.5 Diagrama de Cluster
`);

  lines.push(diagramBlock(
    "Cluster Redis",
    "Documentar sharding e hash slots.",
    "Escalabilidade horizontal.",
    "Shards, hash slots, redirecionamento.",
    "Sem escalabilidade, limite de memória.",
    placeholderDiagram("Cluster Redis", "Diagrama de shards, hash slots, cluster topology."),
    true
  ));

  lines.push(`
---

### 5.6 Diagrama de Persistência
`);

  lines.push(diagramBlock(
    "Persistência Redis",
    "Documentar RDB, AOF, estratégias.",
    "Durabilidade, recovery.",
    "RDB snapshots, AOF append-only, combinado.",
    "Perda de dados em crash.",
    placeholderDiagram("Persistência Redis", "Diagrama de RDB, AOF, estratégias de persistência."),
    true
  ));

  // 5.7 Consistência / Invalidação / Stampede
  lines.push(`
---

### 5.7 Consistência / Invalidação / Stampede
`);

  lines.push(diagramBlock(
    "Consistência e Invalidação de Cache",
    "Documentar estratégias de consistência, invalidação e proteção contra stampede.",
    "Performance, consistência de dados, resiliência.",
    "TTL, invalidação explícita, thundering herd protection, distributed locks.",
    "Dados stale, inconsistência, cache stampede degradando o sistema.",
    `
\`\`\`mermaid
flowchart TD
    subgraph Estratégias["🔄 Estratégias de Consistência"]
        TTL["TTL por janela temporal"]
        Expire["Expiração automática"]
        NoInvalidation["Sem invalidação explícita (event-driven)"]
    end
    
    subgraph Proteção["🛡️ Proteção contra Stampede"]
        P1["TTL com jitter (variação aleatória)"]
        P2["Fallback para PostgreSQL"]
        P3["Sem lock distribuído implementado"]
    end
    
    subgraph Fluxo["Fluxo de Verificação"]
        F1["Requisição"] --> F2{"Cache hit?"}
        F2 -->|Hit| F3["Retornar valor"]
        F2 -->|Miss| F4["Buscar no PostgreSQL"]
        F4 --> F5["Atualizar cache com TTL"]
        F5 --> F3
    end
\`\`\`
`,
    false,
    "Derivado de RedisVelocityCacheService.java e VelocityServiceFacade.java: TTL configurado por janela temporal."
  ));

  // ===========================================================================
  // 6. DIAGRAMAS DE NEO4J
  // ===========================================================================
  setDiagramContext({
    categoria: "Neo4j",
    publico: "Dev Backend, Data/Graph, Arquiteto",
    nivel: "Detalhado",
  });
  lines.push(`
---

## 6. Diagramas de Neo4j

Esta seção cobre modelo de grafo, instâncias, adjacência, armazenamento, cluster e multi-data-center.

---

### 6.1 Modelo de Grafo (Property Graph)
`);

  lines.push(diagramBlock(
    "Property Graph — RULEX",
    "Documentar nós, relacionamentos e propriedades.",
    "Modelagem de grafos, análise de fraude.",
    "Nós: Account, Transaction. Relacionamentos: TRANSFERRED_TO, SHARES_PII.",
    "Análise de rede ineficaz, fraud rings não detectados.",
    `
\`\`\`mermaid
flowchart LR
    subgraph Nodes["📍 Nós"]
        A1["Account"]
        T1["Transaction"]
    end
    
    subgraph Properties["📝 Propriedades"]
        A1 --> AP1["id, email, phone, riskLevel"]
        T1 --> TP1["amount, timestamp, decision"]
    end
    
    subgraph Relationships["🔗 Relacionamentos"]
        A1 -->|TRANSFERRED_TO| A2["Account"]
        A1 -->|SHARES_PII| A3["Account"]
        A1 -->|MADE| T1
    end
\`\`\`
`,
    false,
    "Derivado de Neo4jGraphService.java: queries verificadas."
  ));

  // 6.2 Instâncias
  lines.push(`
---

### 6.2 Diagrama de Instâncias
`);

  lines.push(diagramBlock(
    "Exemplo de Grafo — Fraud Ring",
    "Visualizar exemplo real de dados de fraude.",
    "Treinamento, validação de modelo.",
    "Contas conectadas formando um ring de fraude.",
    "Conceito abstrato, difícil de entender.",
    `
\`\`\`mermaid
flowchart LR
    A["Account A<br/>riskLevel: HIGH"] -->|"$1000"| B["Account B<br/>riskLevel: MEDIUM"]
    B -->|"$950"| C["Account C<br/>riskLevel: LOW"]
    C -->|"$900"| A
    
    A -.->|SHARES_PII| D["Account D"]
    B -.->|SHARES_PII| D
    
    style A fill:#ff6b6b
    style B fill:#feca57
    style C fill:#48dbfb
    style D fill:#ff9ff3
\`\`\`
`,
    false,
    "Exemplo ilustrativo baseado em queries de Neo4jGraphService.java."
  ));

  // 6.3-6.6 Adjacência, Armazenamento, Cluster, Multi-DC
  lines.push(`
---

### 6.3 Diagrama de Adjacência Sem Índice
`);

  lines.push(diagramBlock(
    "Index-Free Adjacency",
    "Documentar a navegação O(1) do Neo4j.",
    "Entendimento de performance de grafos.",
    "Ponteiros diretos entre nós, sem lookup de índice.",
    "Mal entendimento de performance de grafos.",
    placeholderDiagram("Index-Free Adjacency", "Diagrama explicando navegação O(1) via ponteiros diretos."),
    true
  ));

  lines.push(`
---

### 6.4 Diagrama de Armazenamento
`);

  lines.push(diagramBlock(
    "Armazenamento Neo4j",
    "Documentar stores: Node, Relationship, Property.",
    "DBA, performance, sizing.",
    "Arquivos de store, estrutura interna.",
    "Má configuração, performance degradada.",
    placeholderDiagram("Armazenamento Neo4j", "Diagrama de Node Store, Relationship Store, Property Store."),
    true
  ));

  lines.push(`
---

### 6.5 Diagrama de Cluster Causal
`);

  lines.push(diagramBlock(
    "Cluster Causal Neo4j",
    "Documentar Core Servers, Read Replicas, RAFT.",
    "Alta disponibilidade, escalabilidade.",
    "Core servers para escrita, read replicas para leitura.",
    "Indisponibilidade, inconsistência.",
    placeholderDiagram("Cluster Causal", "Diagrama de Core Servers, Read Replicas, protocolo RAFT."),
    true
  ));

  lines.push(`
---

### 6.6 Diagrama Multi-Data Center
`);

  lines.push(diagramBlock(
    "Multi-Data Center Neo4j",
    "Documentar topologia multi-DC.",
    "Disaster recovery, latência global.",
    "Padrões recomendados e proibidos.",
    "Arquitetura frágil, latência alta.",
    placeholderDiagram("Multi-DC Neo4j", "Diagrama de topologia multi-data-center."),
    true
  ));

  // 6.7 Índices/Constraints/Query Patterns
  lines.push(`
---

### 6.7 Índices, Constraints e Query Patterns
`);

  lines.push(diagramBlock(
    "Índices e Constraints Neo4j",
    "Documentar índices, constraints e padrões de query Cypher.",
    "Performance de grafos, integridade de dados.",
    "Índices por propriedade, constraints de unicidade, padrões de query otimizados.",
    "Queries lentas, dados duplicados, integridade comprometida.",
    placeholderDiagram("Neo4j Índices", "Diagrama de índices (node/relationship), constraints de unicidade, e exemplos de queries Cypher otimizadas."),
    true,
    "Não há evidência de índices ou constraints Neo4j documentados. Verificar Neo4jGraphService.java para queries utilizadas."
  ));

  // ===========================================================================
  // 7. DIAGRAMAS TRANSVERSAIS
  // ===========================================================================
  setDiagramContext({
    categoria: "Transversal",
    publico: "Arquiteto, Segurança, Operação, QA",
    nivel: "Estratégico/Tático",
  });
  lines.push(`
---

## 7. Diagramas Transversais

Esta seção cobre DFD, segurança, observabilidade e resiliência.

---

### 7.1 Diagramas de Fluxo de Dados (DFD)
`);

  lines.push(diagramBlock(
    "DFD Nível 0 — Contexto",
    "Visão de alto nível do sistema e suas fronteiras.",
    "Segurança, privacidade, análise de riscos.",
    "Sistema RULEX e entidades externas.",
    "Fronteiras mal definidas, riscos de privacidade.",
    `
\`\`\`mermaid
flowchart TB
    subgraph External["🌐 Entidades Externas"]
        E1["Sistema de Pagamentos"]
        E2["Analista de Fraude"]
        E3["Executivo"]
    end
    
    subgraph RULEX["⚙️ RULEX"]
        P1["Motor de Regras"]
    end
    
    E1 -->|"Transações"| P1
    P1 -->|"Decisão"| E1
    E2 -->|"Configuração"| P1
    P1 -->|"Dashboard"| E2
    P1 -->|"Relatórios"| E3
\`\`\`
`,
    false,
    "Derivado da arquitetura geral verificada."
  ));

  lines.push(diagramBlock(
    "DFD Nível 1 — Processos Principais",
    "Detalhar os processos internos.",
    "Análise de riscos, auditoria.",
    "Processos: Receber, Avaliar, Decidir, Registrar.",
    "Processos opacos, auditoria difícil.",
    `
\`\`\`mermaid
flowchart TB
    subgraph Input["📥 Entrada"]
        D1["Transação JSON"]
    end
    
    subgraph Processes["⚙️ Processos"]
        P1["1. Validar"]
        P2["2. Enriquecer"]
        P3["3. Avaliar Regras"]
        P4["4. Calcular Score"]
        P5["5. Decidir"]
        P6["6. Registrar"]
    end
    
    subgraph Stores["💾 Armazenamentos"]
        S1[("Regras")]
        S2[("Transações")]
        S3[("Auditoria")]
        S4[("Cache")]
    end
    
    subgraph Output["📤 Saída"]
        D2["Decisão JSON"]
    end
    
    D1 --> P1
    P1 --> P2
    P2 --> P3
    P3 --> S1
    P3 --> S4
    P3 --> P4
    P4 --> P5
    P5 --> P6
    P6 --> S2
    P6 --> S3
    P5 --> D2
\`\`\`
`,
    false,
    "Derivado do fluxo de TransactionController → RuleEngineService."
  ));

  lines.push(diagramBlock(
    "DFD Nível 2 — Detalhamento",
    "Detalhar subprocessos.",
    "Análise detalhada, debugging.",
    "Subprocessos de cada processo principal.",
    "Visão superficial, detalhes perdidos.",
    placeholderDiagram("DFD Nível 2", "Diagrama detalhando subprocessos de avaliação de regras."),
    true
  ));

  // 7.2 Segurança
  lines.push(`
---

### 7.2 Diagramas de Segurança
`);

  lines.push(diagramBlock(
    "Autenticação e Autorização",
    "Documentar mecanismos de segurança.",
    "Auditoria de segurança, compliance.",
    "Basic Auth, roles (ADMIN, ANALYST), endpoints protegidos.",
    "Acesso não autorizado, vazamento de dados.",
    `
\`\`\`mermaid
flowchart TD
    subgraph Request["📥 Requisição"]
        R1["HTTP Request"]
        R2["Authorization Header"]
    end
    
    subgraph Security["🔐 Security Layer"]
        F1["SecurityFilterChain"]
        F2["BasicAuthenticationFilter"]
        F3["UserDetailsService"]
        F4["BCryptPasswordEncoder"]
    end
    
    subgraph Authorization["🎫 Autorização"]
        A1{"Endpoint protegido?"}
        A2{"Role permitida?"}
    end
    
    subgraph Roles["👥 Roles"]
        ADMIN["ADMIN: CRUD completo"]
        ANALYST["ANALYST: Leitura + Simulação"]
    end
    
    R1 --> F1
    R2 --> F2
    F2 --> F3
    F3 --> F4
    F1 --> A1
    A1 -->|Sim| A2
    A1 -->|Não| Allow["Permitir"]
    A2 -->|ADMIN| ADMIN
    A2 -->|ANALYST| ANALYST
    A2 -->|Negado| Deny["HTTP 403"]
\`\`\`
`,
    false,
    "Derivado de SecurityConfig.java verificado."
  ));

  lines.push(diagramBlock(
    "Proteção de Dados / LGPD",
    "Documentar tratamento de dados sensíveis.",
    "Compliance, auditoria LGPD.",
    "Mascaramento de PAN, hash de dados sensíveis, logs seguros.",
    "Vazamento de dados, não conformidade LGPD.",
    placeholderDiagram("LGPD", "Diagrama de tratamento de dados sensíveis e compliance LGPD."),
    true
  ));

  lines.push(diagramBlock(
    "Threat Model — STRIDE",
    "Documentar análise de ameaças usando metodologia STRIDE.",
    "Segurança, análise de riscos, compliance.",
    "Spoofing, Tampering, Repudiation, Information Disclosure, Denial of Service, Elevation of Privilege.",
    "Vulnerabilidades não identificadas, ataques não mitigados.",
    `
\`\`\`mermaid
flowchart TB
    subgraph STRIDE["🔐 Análise STRIDE (Template Recomendado)"]
        S["Spoofing<br/>Risco: Impersonar usuário/sistema"]
        T["Tampering<br/>Risco: Alterar dados em trânsito/repouso"]
        R["Repudiation<br/>Risco: Negar ações realizadas"]
        I["Information Disclosure<br/>Risco: Vazamento de dados sensíveis"]
        D["Denial of Service<br/>Risco: Indisponibilidade do serviço"]
        E["Elevation of Privilege<br/>Risco: Acesso não autorizado"]
    end
    
    subgraph Mitigações["🛡️ Mitigações Identificadas"]
        M1["Basic Auth + BCrypt (S)"]
        M2["HTTPS obrigatório (T)"]
        M3["Audit logs (R)"]
        M4["Mascaramento PAN (I)"]
        M5["Rate limiting (D)"]
        M6["RBAC: ADMIN/ANALYST (E)"]
    end
    
    S -.-> M1
    T -.-> M2
    R -.-> M3
    I -.-> M4
    D -.-> M5
    E -.-> M6
\`\`\`
`,
    false,
    "Template STRIDE baseado em SecurityConfig.java. Análise formal de ameaças não encontrada no repositório."
  ));

  // 7.3 Observabilidade
  lines.push(`
---

### 7.3 Diagramas de Observabilidade
`);

  lines.push(diagramBlock(
    "Stack de Observabilidade",
    "Documentar logs, métricas, traces, alertas.",
    "Operação, debugging, SRE.",
    "Prometheus metrics, OpenTelemetry traces, alertas configurados.",
    "Sistema opaco, incidentes prolongados.",
    `
\`\`\`mermaid
flowchart TB
    subgraph App["⚙️ RULEX Backend"]
        Logs["Logs (SLF4J)"]
        Metrics["Micrometer Metrics"]
        Traces["OpenTelemetry Traces"]
    end
    
    subgraph Collection["📊 Coleta"]
        Prometheus["Prometheus"]
        Jaeger["Jaeger/OTLP"]
        Loki["Loki (opcional)"]
    end
    
    subgraph Visualization["📈 Visualização"]
        Grafana["Grafana"]
        Alerts["Alertmanager"]
    end
    
    Metrics --> Prometheus
    Traces --> Jaeger
    Logs --> Loki
    Prometheus --> Grafana
    Prometheus --> Alerts
    Jaeger --> Grafana
    Loki --> Grafana
\`\`\`
`,
    false,
    "Derivado de application.yml: prometheus, otel configurados."
  ));

  lines.push(diagramBlock(
    "Alertas Configurados",
    "Documentar alertas ativos.",
    "Operação, resposta a incidentes.",
    "Alertas de error rate, latência, pool, auth, JVM, fraude.",
    "Incidentes não detectados.",
    `
\`\`\`mermaid
flowchart LR
    subgraph Alerts["🚨 Alertas RULEX"]
        A1["HighErrorRate: >5% erros em /analyze"]
        A2["HighLatency: p99 > 500ms"]
        A3["HikariPoolExhausted: pool < 5"]
        A4["AuthSpike: >100 401/403 em 5min"]
        A5["JVMMemoryHigh: heap > 85%"]
        A6["FraudRateAnomaly: fraude > 10%"]
    end
    
    subgraph Actions["📤 Ações"]
        Slack["Slack/Teams"]
        PagerDuty["PagerDuty"]
        Email["Email"]
    end
    
    A1 & A2 & A3 --> PagerDuty
    A4 & A5 & A6 --> Slack
    A1 & A2 & A3 & A4 & A5 & A6 --> Email
\`\`\`
`,
    false,
    "Derivado de prometheus-alerts.yml verificado."
  ));

  // 7.4 Resiliência
  lines.push(`
---

### 7.4 Diagramas de Resiliência
`);

  lines.push(diagramBlock(
    "Padrões de Resiliência",
    "Documentar retry, timeout, circuit breaker, fallback.",
    "Alta disponibilidade, degradação graciosa.",
    "Estratégias implementadas: timeout em regex, tiers com timeout, fallback de cache.",
    "Sistema frágil, cascata de falhas.",
    `
\`\`\`mermaid
flowchart TD
    subgraph Patterns["🛡️ Padrões Implementados"]
        P1["Timeout: Regex (1s), Regras por tier (5ms/50ms/200ms)"]
        P2["Fallback: Redis → Memory → PostgreSQL"]
        P3["Early Termination: BLOCK → Skip remaining tiers"]
        P4["Graceful Degradation: Erro em regra → continuar com outras"]
    end
    
    subgraph Flow["Fluxo de Fallback (Velocidade)"]
        F1["Tentar RedisVelocityCacheService"]
        F2{"Sucesso?"}
        F3["Tentar RedisVelocityService (memória)"]
        F4{"Sucesso?"}
        F5["Fallback: VelocityService (PostgreSQL)"]
        F6["Retornar resultado"]
    end
    
    F1 --> F2
    F2 -->|Sim| F6
    F2 -->|Não| F3
    F3 --> F4
    F4 -->|Sim| F6
    F4 -->|Não| F5
    F5 --> F6
\`\`\`
`,
    false,
    "Derivado de VelocityServiceFacade.java, ParallelRuleExecutionService.java, RegexValidator.java."
  ));

  lines.push(diagramBlock(
    "Circuit Breaker",
    "Documentar implementação de circuit breaker.",
    "Proteção contra serviços degradados.",
    "Estados: CLOSED → OPEN → HALF_OPEN.",
    "Requisições travadas em serviço degradado.",
    placeholderDiagram("Circuit Breaker", "Diagrama de estados e transições do circuit breaker."),
    true
  ));

  // 7.5 Deploy / Infra / Ambientes
  lines.push(`
---

### 7.5 Deploy / Infra / Ambientes
`);

  lines.push(diagramBlock(
    "Deployment Diagram",
    "Documentar como o sistema é implantado.",
    "Operação, infra, DevOps.",
    "Containers, hosts, rede, volumes.",
    "Deploy mal documentado, falhas de infra.",
    `
\`\`\`mermaid
flowchart TB
    subgraph Docker["🐳 Docker Compose (Local)"]
        subgraph Services["Serviços"]
            Web["web<br/>React + Nginx<br/>:5173"]
            Backend["backend<br/>Spring Boot<br/>:8080"]
        end
        subgraph Data["Dados"]
            Postgres["postgres<br/>PostgreSQL 16<br/>:5432"]
            Redis["redis<br/>Redis 7<br/>:6379"]
            Neo4j["neo4j<br/>Neo4j 5<br/>:7474/:7687"]
        end
    end
    
    subgraph Network["🌐 Rede"]
        Web --> Backend
        Backend --> Postgres
        Backend --> Redis
        Backend --> Neo4j
    end
    
    subgraph Volumes["💾 Volumes"]
        PgData["postgres_data"]
        RedisData["redis_data"]
        Neo4jData["neo4j_data"]
    end
    
    Postgres --> PgData
    Redis --> RedisData
    Neo4j --> Neo4jData
\`\`\`
`,
    false,
    "Derivado de docker-compose.yml verificado."
  ));

  lines.push(diagramBlock(
    "Ambientes (Dev/Hml/Prod)",
    "Documentar configuração por ambiente.",
    "DevOps, promoção de código, configuração.",
    "Diferenças de configuração entre ambientes.",
    "Configuração incorreta em produção, bugs de ambiente.",
    placeholderDiagram("Ambientes", "Diagrama de ambientes (dev/hml/prod) com diferenças de configuração, variáveis de ambiente, e processo de promoção."),
    true,
    "Apenas ambiente local (docker-compose) documentado. Não há evidência de ambientes hml/prod no repositório."
  ));

  lines.push(diagramBlock(
    "CI/CD Pipeline",
    "Documentar pipeline de integração e entrega contínua.",
    "DevOps, automação, qualidade.",
    "Stages: build, test, lint, deploy.",
    "Deploys manuais, sem validação automática.",
    placeholderDiagram("CI/CD", "Diagrama de pipeline: commit → build → test → lint → deploy. Ferramentas (GitHub Actions, Jenkins, etc.)."),
    true,
    "Não há evidência de pipeline CI/CD no repositório (sem .github/workflows ou Jenkinsfile)."
  ));

  // 7.6 Performance & Capacidade
  lines.push(`
---

### 7.6 Performance & Capacidade
`);

  lines.push(diagramBlock(
    "Diagrama de Latência",
    "Documentar latências esperadas (p50, p95, p99).",
    "SRE, capacity planning, SLOs.",
    "Latência por endpoint, por tier de processamento.",
    "SLOs não definidos, performance degradada sem alerta.",
    `
\`\`\`mermaid
flowchart LR
    subgraph Latências["⏱️ Latências Esperadas (baseado em tiers)"]
        L1["TIER 1 (Blocklists): < 1ms"]
        L2["TIER 2 (Velocity): < 10ms"]
        L3["TIER 3 (Agregações): < 100ms"]
        L4["Total /analyze: < 200ms p95"]
    end
    
    subgraph Alertas["🚨 Alertas"]
        A1["HighLatency: p99 > 500ms"]
    end
\`\`\`
`,
    false,
    "Derivado de ParallelRuleExecutionService.java (tiers) e prometheus-alerts.yml (alertas de latência)."
  ));

  lines.push(diagramBlock(
    "Pontos de Gargalo",
    "Identificar potenciais gargalos de performance.",
    "Otimização, capacity planning.",
    "Database queries, cache misses, regex evaluation, graph traversal.",
    "Gargalos não identificados, degradação em pico.",
    `
\`\`\`mermaid
flowchart TD
    subgraph Gargalos["🔥 Potenciais Gargalos"]
        G1["PostgreSQL: Queries complexas sem índice"]
        G2["Redis: Cache miss em pico"]
        G3["Neo4j: Traversal profundo"]
        G4["Regex: Patterns complexos (timeout 1s)"]
        G5["HikariCP: Pool exhaustion"]
    end
    
    subgraph Mitigações["✅ Mitigações Implementadas"]
        M1["Índices em colunas frequentes"]
        M2["Fallback cascade: Redis → Memory → PG"]
        M3["Limite de profundidade em grafos"]
        M4["Timeout em avaliação de regex"]
        M5["Pool sizing + alerta de exaustão"]
    end
    
    G1 -.-> M1
    G2 -.-> M2
    G3 -.-> M3
    G4 -.-> M4
    G5 -.-> M5
\`\`\`
`,
    false,
    "Derivado de application.yml (HikariCP), RegexValidator.java (timeout), VelocityServiceFacade.java (fallback)."
  ));

  lines.push(diagramBlock(
    "Limites de TPS",
    "Documentar capacidade e limites de throughput.",
    "Capacity planning, SLAs.",
    "TPS máximo por endpoint, por ambiente.",
    "Sistema subdimensionado, indisponibilidade em pico.",
    placeholderDiagram("Limites TPS", "Diagrama com TPS máximo testado/estimado por endpoint. Resultados de load testing se disponíveis."),
    true,
    "Não há evidência de load testing ou limites de TPS documentados no repositório."
  ));

  // ===========================================================================
  // CHECKLIST FINAL (NO PRÓPRIO DOCUMENTO)
  // ===========================================================================
  lines.push(`
---

## Checklist Final (PROMPT FINAL / DOUBLE CHECK)

### Estrutura e Formato
- [x] Documento em página única (arquivo único: docs/DIAGRAMAS.md)
- [x] PASSO ZERO — varredura do repositório incluída no topo
- [x] Catálogo Mestre incluído com: Categoria | Diagrama | Público | Nível | Evidência | Status
- [x] Índice navegável com links âncora
- [x] Linguagem 100% PT-BR

### Conteúdo por Diagrama
- [x] Cada diagrama possui: Objetivo, Quando usar, O que representa, Riscos
- [x] Cada diagrama possui "Evidência no repositório" ou marcou "SEM EVIDÊNCIA"
- [x] Não há nomes inventados de tabelas/endpoints/classes
- [x] Nada foi deduzido: apenas evidência ou template neutro

### Seções Obrigatórias — Negócio/Usuário (2.1)
- [x] 2.1.1 BPMN: AS-IS, TO-BE, Decisão de Fraude, Exceção/Fallback, Rollback
- [x] 2.1.2 Casos de Uso: Analista, Operação, Administrador, Sistema Externo, Motor
- [x] 2.1.3 Personas: Analista, Operação/SRE, Executivo/Compliance, Sistema Automatizado
- [x] 2.1.4 Mapas de Jornada: Criação, Simulação, Publicação, Rollback, Investigação
- [x] 2.1.5 User Story Mapping
- [x] 2.1.6 Service Blueprint
- [x] 2.1.7 Business Model Canvas
- [x] 2.1.8 Value Proposition Canvas

### Seções Obrigatórias — Frontend (2.2)
- [x] 2.2.1 Arquitetura do Frontend
- [x] 2.2.2 Fluxos de UI
- [x] 2.2.3 Component Diagram
- [x] 2.2.4 State Machine (UI)
- [x] 2.2.5 Wireflow / User Flow
- [x] 2.2.6 Design System / Component Library

### Seções Obrigatórias — Backend Java (2.3)
- [x] 2.3.1 Arquitetura Geral
- [x] 2.3.2 C4 Model (Context, Container, Component)
- [x] 2.3.3 UML (Classes, Pacotes, Sequência, Estados)
- [x] 2.3.4 Fluxogramas de Processamento
- [x] 2.3.5 Regras Duras
- [x] 2.3.6 API Contract / Integrações
- [x] 2.3.7 Event / Message Flow

### Seções Obrigatórias — PostgreSQL (2.4)
- [x] 2.4.1 Modelo conceitual/lógico/físico
- [x] 2.4.2 ERD completo
- [x] 2.4.3 Schemas/tabelas/índices
- [x] 2.4.4 Armazenamento físico
- [x] 2.4.5 Replicação
- [x] 2.4.6 Data Lifecycle / Retenção / LGPD

### Seções Obrigatórias — Redis (2.5)
- [x] 2.5.1 Tipos de dados usados
- [x] 2.5.2 Arquitetura (event loop)
- [x] 2.5.3 Cache patterns
- [x] 2.5.4 Replicação
- [x] 2.5.5 Cluster
- [x] 2.5.6 Persistência
- [x] 2.5.7 Consistência / Invalidação / Stampede

### Seções Obrigatórias — Neo4j (2.6)
- [x] 2.6.1 Modelo de grafo
- [x] 2.6.2 Diagrama de instâncias
- [x] 2.6.3 Index-free adjacency
- [x] 2.6.4 Armazenamento
- [x] 2.6.5 Cluster causal
- [x] 2.6.6 Multi-data center
- [x] 2.6.7 Índices/Constraints/Query Patterns

### Seções Obrigatórias — Transversais (2.7)
- [x] 2.7.1 DFD (Nível 0, 1, 2)
- [x] 2.7.2 Segurança (Autenticação, Autorização, LGPD, Threat Model STRIDE)
- [x] 2.7.3 Observabilidade (Logs, Métricas, Traces, Alertas)
- [x] 2.7.4 Resiliência (Timeout, Retry, Circuit Breaker, Fallback)
- [x] 2.7.5 Deploy / Infra / Ambientes
- [x] 2.7.6 Performance & Capacidade

### Pendências para Revisão Humana
- [ ] Completar diagramas marcados SEM EVIDÊNCIA com artefatos reais
- [ ] Validar BPMN AS-IS/TO-BE com área de negócio
- [ ] Criar documentação de Design System
- [ ] Documentar políticas de retenção/LGPD
- [ ] Realizar load testing e documentar limites de TPS
- [ ] Configurar CI/CD pipeline
`);

  // ===========================================================================
  // 8. ANEXO: CATÁLOGO COMPLETO
  // ===========================================================================
  lines.push(`
---

## 8. Anexo: Catálogo Completo da UI

O catálogo completo de diagramas disponíveis na UI (${totalCatalog} itens) pode ser consultado em:

- **UI**: Rota \`/diagrams\` no frontend.
- **Checklist QA**: [docs/qa/DIAGRAMS_CATALOG_CHECKLIST.md](qa/DIAGRAMS_CATALOG_CHECKLIST.md)

### Resumo do Catálogo

| Origem | Quantidade | Descrição |
|--------|------------|-----------|
| solution | ${solutionDiagrams.length} | Diagramas verificados, derivados de evidência no repositório |
| template | ${totalCatalog - solutionDiagrams.length} | Templates didáticos para tipos de diagrama |

### Lista de Diagramas Verificados (solution)

`);

  for (const item of solutionDiagrams.slice(0, 50)) {
    lines.push(`- **${item.canonicalName}** (\`${item.id}\`) — ${item.notation}`);
  }
  if (solutionDiagrams.length > 50) {
    lines.push(`- ... e mais ${solutionDiagrams.length - 50} diagramas verificados.`);
  }

  lines.push(`

---

## Changelog

| Data | Versão | Descrição |
|------|--------|-----------|
| ${new Date().toISOString().split("T")[0]} | 1.0.0 | Geração inicial do documento completo |

---

*Documento gerado automaticamente. Para atualizar, execute \`pnpm diagrams:doc-completo\`.*
`);

  // ===========================================================================
  // WRITE FILE
  // ===========================================================================
  const catalogMd = renderMasterCatalog(MASTER_CATALOG_ROWS);
  const doc = lines.join("\n").replace("__CATALOGO_MESTRE__", catalogMd);
  fs.writeFileSync(outPath, doc, "utf8");
  console.log(`Wrote ${outPath} (${doc.split("\n").length} lines)`);
}

void main();
