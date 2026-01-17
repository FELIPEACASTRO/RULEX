/**
 * diagramRegistry.ts
 *
 * Central registry for the MASSIVE diagram catalog (200+ types).
 * Now uses NOTACAO/NOME ID format and tracks renderer status.
 */

import { MASSIVE_DIAGRAM_CATALOG, type MassiveDiagramFamily, getMassiveDiagramCount } from "./massiveCatalog";
import { getDiagramCategory } from "./categories";
import { buildInventorySolutionDiagrams } from "./inventorySolutionCatalog";
import type { 
  DiagramCatalogItem, 
  DiagramCategoryId, 
  DiagramFormat, 
  DiagramNotation,
  RendererId,
  RendererStatus,
} from "../types";

const RULEX_SYSTEM_LABELS = {
  frontend: "RULEX Web (React)",
  api: "API (Spring Boot)",
  engine: "Rules Engine", 
  db: "PostgreSQL", 
  cache: "Redis (cache)",
  audit: "Audit/Logs",
  monitoring: "Monitoring",
} as const;

/**
 * Slugify a name for use in IDs
 */
function slugify(text: string): string {
  return text
    .toLowerCase()
    .normalize("NFD")
    .replace(/[\u0300-\u036f]/g, "") // Remove diacritics
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-+|-+$/g, "");
}

/**
 * Map notation to primary category ID
 * NOTE: Some family IDs need special mapping to category IDs
 */
function notationToCategory(notation: DiagramNotation, familyId: string): DiagramCategoryId {
  // Map family IDs to category IDs
  const mapping: Record<string, DiagramCategoryId> = {
    processos: "processos",
    uml: "uml_estrutural", // Most UML diagrams are structural
    c4: "arquitetura", // C4 is an architectural notation
    arquitetura: "arquitetura",
    dados_postgres: "dados_postgres",
    dados_redis: "dados_redis",
    dados_neo4j: "dados_neo4j",
    frontend: "frontend",
    devops: "infra", // DevOps maps to infra category
    seguranca: "seguranca",
    qualidade: "qualidade",
    cs_classicos: "cs_classicos",
  };
  
  return mapping[familyId] || ("arquitetura" as DiagramCategoryId); // Default fallback
}

/**
 * Infer format from notation and name
 */
function inferFormat(notation: DiagramNotation, name: string): DiagramFormat {
  if (notation === "BPMN") return "bpmn";
  if (notation === "DMN") return "dmn";
  if (notation === "DFD") return "dfd";
  if (notation === "MATRIX") return "matrix";
  if (notation === "GRAPH") return "json";
  if (notation === "ER") return "mermaid";
  if (notation === "EPC") return "epc";
  return "mermaid"; // Default for flowcharts, UML, trees, etc.
}

/**
 * Determine renderer status
 */
function determineRendererStatus(format: DiagramFormat): RendererStatus {
  // Renderers marked as "OK" (functional)
  const okRenderers: DiagramFormat[] = ["mermaid", "bpmn", "pdf", "dfd", "matrix", "json"];
  return okRenderers.includes(format) ? "OK" : "PENDENTE";
}

/**
 * Infer renderer ID from format
 */
function inferRenderer(format: DiagramFormat): RendererId {
  switch (format) {
    case "bpmn":
      return "bpmn";
    case "dmn":
      return "dmn";
    case "dfd":
      return "dfd";
    case "matrix":
      return "matrix";
    case "pdf":
      return "pdf";
    case "plantuml":
      return "plantuml";
    case "json":
      return "graph";
    case "epc":
      return "fallback"; // EPC not yet implemented
    default:
      return "mermaid";
  }
}

function mermaidSampleForDiagram(diagramType: MassiveDiagramFamily["diagrams"][0], familyId: string): string {
  // Prefer explicit examples if they look like Mermaid syntax (heuristic)
  if (diagramType.example) {
    const ex = diagramType.example.trim();
    if (/(^flowchart\s)|(^sequenceDiagram\s)|(^erDiagram\s)|(^stateDiagram\b)|(^classDiagram\b)/m.test(ex)) {
      return ex;
    }
  }

  // Category-focused templates that represent RULEX
  if (familyId === "processos") {
    return [
      "flowchart TD",
      `  A[${RULEX_SYSTEM_LABELS.frontend}] -->|POST /analyze| B[${RULEX_SYSTEM_LABELS.api}]`,
      `  B --> C[Normalize + Validate payload]`,
      `  C --> D[${RULEX_SYSTEM_LABELS.engine}]`,
      "  D --> E{Score/Decision}",
      "  E -->|APPROVE| F[Persist decision]",
      "  E -->|REVIEW| G[Create case]",
      "  E -->|BLOCK| H[Block transaction]",
      `  F --> I[${RULEX_SYSTEM_LABELS.db}]`,
      `  G --> I`,
      `  H --> I`,
      `  I --> J[${RULEX_SYSTEM_LABELS.audit}]`,
      `  J --> K[${RULEX_SYSTEM_LABELS.monitoring}]`,
    ].join("\n");
  }

  if (familyId === "arquitetura" || familyId === "c4") {
    return [
      "flowchart LR",
      "  subgraph Client[Client Layer]",
      `    FE[${RULEX_SYSTEM_LABELS.frontend}]`,
      "  end",
      "  subgraph Backend[Backend Layer]",
      `    API[${RULEX_SYSTEM_LABELS.api}]`,
      `    ENG[${RULEX_SYSTEM_LABELS.engine}]`,
      "  end",
      "  subgraph Data[Data Layer]",
      `    DB[${RULEX_SYSTEM_LABELS.db}]`,
      `    CACHE[${RULEX_SYSTEM_LABELS.cache}]`,
      "  end",
      "  subgraph Observability[Observability]",
      `    AUD[${RULEX_SYSTEM_LABELS.audit}]`,
      `    MON[${RULEX_SYSTEM_LABELS.monitoring}]`,
      "  end",
      "  FE --> API",
      "  API --> ENG",
      "  ENG --> DB",
      "  ENG --> CACHE",
      "  API --> AUD",
      "  ENG --> AUD",
      "  AUD --> MON",
    ].join("\n");
  }

  if (familyId === "api") {
    return [
      "sequenceDiagram",
      "  participant FE as RULEX Web",
      "  participant API as API",
      "  participant ENG as Rules Engine",
      "  participant DB as Postgres",
      "  FE->>API: POST /analyze (transaction)",
      "  API->>ENG: evaluate(transaction)",
      "  ENG->>DB: load active rules",
      "  DB-->>ENG: rules",
      "  ENG-->>API: decision + score + matches",
      "  API->>DB: persist audit trail",
      "  DB-->>API: ok",
      "  API-->>FE: 200 {decision, score, reasons}",
    ].join("\n");
  }

  if (familyId.startsWith("dados_")) {
    return [
      "erDiagram",
      "  TRANSACTION ||--o{ RULE_EVAL : evaluated_by",
      "  RULE_EVAL }o--|| RULE : references",
      "  TRANSACTION {",
      "    uuid id",
      "    string externalTransactionId",
      "    decimal amount",
      "    string currency",
      "  }",
      "  RULE {",
      "    uuid id",
      "    string name",
      "    string status",
      "  }",
      "  RULE_EVAL {",
      "    uuid id",
      "    uuid transactionId",
      "    uuid ruleId",
      "    int scoreImpact",
      "  }",
    ].join("\n");
  }

  if (familyId === "seguranca") {
    return [
      "flowchart TD",
      "  A[User] --> B[Login]",
      "  B --> C{Auth ok?}",
      "  C -->|no| D[403]",
      "  C -->|yes| E[Access RULEX UI]",
      "  E --> F[Call API]",
      "  F --> G[Audit log]",
      "  G --> H[Alerting/Monitoring]",
    ].join("\n");
  }

  // Generic, but still RULEX-relevant and non-trivial
  return [
    "flowchart TD",
    `  A[${diagramType.name}] --> B[Aplicado no RULEX]`,
    `  B --> C[${RULEX_SYSTEM_LABELS.engine} executa avaliação]`,
    "  C --> D{Decisão}",
    "  D -->|Approve| E[OK]",
    "  D -->|Review| F[Case]",
    "  D -->|Block| G[Block]",
  ].join("\n");
}

function bpmnSampleXml(): string {
  // Minimal valid BPMN 2.0 XML for bpmn-js
  return `<?xml version="1.0" encoding="UTF-8"?>
<bpmn:definitions xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xmlns:bpmn="http://www.omg.org/spec/BPMN/20100524/MODEL"
  xmlns:bpmndi="http://www.omg.org/spec/BPMN/20100524/DI"
  xmlns:dc="http://www.omg.org/spec/DD/20100524/DC"
  id="Definitions_RULEX" targetNamespace="http://rulex.local/bpmn">
  <bpmn:process id="Process_RULEX_Analyze" isExecutable="false">
    <bpmn:startEvent id="StartEvent_1" name="Recebe transação" />
    <bpmn:task id="Task_Validate" name="Validar payload" />
    <bpmn:task id="Task_Evaluate" name="Avaliar regras" />
    <bpmn:exclusiveGateway id="Gateway_Decision" name="Decisão" />
    <bpmn:task id="Task_Persist" name="Persistir auditoria" />
    <bpmn:endEvent id="EndEvent_Approved" name="Approve" />
    <bpmn:endEvent id="EndEvent_Review" name="Review" />
    <bpmn:endEvent id="EndEvent_Blocked" name="Block" />

    <bpmn:sequenceFlow id="Flow_1" sourceRef="StartEvent_1" targetRef="Task_Validate" />
    <bpmn:sequenceFlow id="Flow_2" sourceRef="Task_Validate" targetRef="Task_Evaluate" />
    <bpmn:sequenceFlow id="Flow_3" sourceRef="Task_Evaluate" targetRef="Gateway_Decision" />
    <bpmn:sequenceFlow id="Flow_4" sourceRef="Gateway_Decision" targetRef="Task_Persist" />
    <bpmn:sequenceFlow id="Flow_5" sourceRef="Task_Persist" targetRef="EndEvent_Approved" />
  </bpmn:process>

  <bpmndi:BPMNDiagram id="BPMNDiagram_1">
    <bpmndi:BPMNPlane id="BPMNPlane_1" bpmnElement="Process_RULEX_Analyze" />
  </bpmndi:BPMNDiagram>
</bpmn:definitions>`;
}

function dfdSampleModel() {
  return {
    processes: [
      { id: "p_api", label: "API /analyze", x: 180, y: 170 },
      { id: "p_engine", label: "Rules Engine", x: 460, y: 170 },
    ],
    datastores: [
      { id: "d_pg", label: "PostgreSQL", x: 460, y: 340 },
      { id: "d_cache", label: "Redis", x: 700, y: 170 },
    ],
    entities: [
      { id: "e_client", label: "Frontend", x: 80, y: 70 },
      { id: "e_ops", label: "Analista", x: 80, y: 340 },
    ],
    flows: [
      { id: "f1", from: "e_client", to: "p_api", label: "transaction" },
      { id: "f2", from: "p_api", to: "p_engine", label: "validate + evaluate" },
      { id: "f3", from: "p_engine", to: "d_pg", label: "rules/audit" },
      { id: "f4", from: "p_engine", to: "d_cache", label: "cache" },
      { id: "f5", from: "p_api", to: "e_client", label: "decision" },
      { id: "f6", from: "e_ops", to: "p_api", label: "review" },
    ],
  };
}

function graphSampleModel() {
  return {
    nodes: [
      { id: "frontend", label: RULEX_SYSTEM_LABELS.frontend, x: 120, y: 140 },
      { id: "api", label: RULEX_SYSTEM_LABELS.api, x: 360, y: 140 },
      { id: "engine", label: RULEX_SYSTEM_LABELS.engine, x: 580, y: 140 },
      { id: "db", label: RULEX_SYSTEM_LABELS.db, x: 580, y: 320 },
      { id: "audit", label: RULEX_SYSTEM_LABELS.audit, x: 820, y: 140 },
    ],
    edges: [
      { id: "e1", from: "frontend", to: "api", label: "HTTP" },
      { id: "e2", from: "api", to: "engine", label: "evaluate" },
      { id: "e3", from: "engine", to: "db", label: "read/write" },
      { id: "e4", from: "api", to: "audit", label: "log" },
      { id: "e5", from: "engine", to: "audit", label: "events" },
    ],
  };
}

function matrixSampleModel() {
  return {
    rows: [
      { id: "frontend", label: "Frontend" },
      { id: "backend", label: "Backend" },
      { id: "rules", label: "Rules Engine" },
      { id: "ops", label: "Operações" },
    ],
    cols: [
      { id: "analyze", label: "Analisar transação" },
      { id: "author", label: "Criar regra" },
      { id: "audit", label: "Auditar" },
    ],
    cells: [
      { rowId: "frontend", colId: "analyze", value: "R" },
      { rowId: "backend", colId: "analyze", value: "A" },
      { rowId: "rules", colId: "analyze", value: "C" },
      { rowId: "ops", colId: "analyze", value: "I" },
      { rowId: "ops", colId: "author", value: "R" },
      { rowId: "rules", colId: "author", value: "C" },
      { rowId: "backend", colId: "audit", value: "A" },
      { rowId: "ops", colId: "audit", value: "R" },
    ],
  };
}

function sampleFor(format: DiagramFormat, diagramType: MassiveDiagramFamily["diagrams"][0], familyId: string) {
  switch (format) {
    case "bpmn":
      return { kind: "inline" as const, format: "bpmn" as const, content: bpmnSampleXml() };
    case "dfd":
      return { kind: "json" as const, format: "json" as const, data: dfdSampleModel() };
    case "matrix":
      return { kind: "json" as const, format: "json" as const, data: matrixSampleModel() };
    case "json":
      return { kind: "json" as const, format: "json" as const, data: graphSampleModel() };
    case "dmn":
      return {
        kind: "inline" as const,
        format: "dmn" as const,
        content:
          diagramType.example?.trim() ||
          "<dmn:definitions xmlns:dmn=\"https://www.omg.org/spec/DMN/20191111/MODEL/\" id=\"DMN_RULEX\">\n  <!-- TODO: definir decision table do RULEX -->\n</dmn:definitions>",
      };
    default:
      return { kind: "inline" as const, format: "mermaid" as const, content: mermaidSampleForDiagram(diagramType, familyId) };
  }
}

/**
 * Normalize a massive diagram type into registry format
 */
function normalizeMassiveDiagram(
  diagramType: MassiveDiagramFamily["diagrams"][0],
  familyId: string
): DiagramCatalogItem {
  const format = inferFormat(diagramType.notation, diagramType.name);
  const renderer = inferRenderer(format);
  const rendererStatus = determineRendererStatus(format);
  const categoryId = notationToCategory(diagramType.notation, familyId);
  const category = getDiagramCategory(categoryId);

  // ID format: "NOTACAO/nome-slugificado"
  const id = `${diagramType.notation}/${slugify(diagramType.name)}`;

  return {
    id,
    notation: diagramType.notation,
    canonicalName: diagramType.name,
    aliases: [diagramType.nameEn, ...(diagramType.tools || [])].filter(Boolean) as string[],

    origin: "template",
    verified: false,
    verificationNotes: "Template didático (não representa necessariamente o RULEX real).",
    categoryId,
    categoryLabel: category.label,
    descriptionWhenToUse: `${diagramType.description}. ${diagramType.useCase}`.trim(),
    formatsSupported: [format],
    rendererId: renderer,
    rendererStatus,
    sample: sampleFor(format, diagramType, familyId),
    source: diagramType.example,
  };
}

function solutionMermaidFlowAnalyze(): string {
  return [
    "flowchart TD",
    "  FE[Frontend / Simulator] -->|POST /analyze| API[API (Spring Boot)]",
    "  API --> V[Validate + Normalize payload]",
    "  V --> ENG[RuleEngineService]",
    "  ENG -->|load rules| DB[(PostgreSQL)]",
    "  ENG --> C[Compute score + matches]",
    "  C --> D{Decision}",
    "  D -->|APPROVE| A1[Persist audit + decision]",
    "  D -->|REVIEW| A2[Persist audit + create case]",
    "  D -->|BLOCK| A3[Persist audit + block]",
    "  A1 --> DB",
    "  A2 --> DB",
    "  A3 --> DB",
    "  API --> AUD[AccessLogService / Audit]",
    "  AUD --> MON[MetricsService / Monitoring]",
    "  API --> FE",
  ].join("\n");
}

function solutionMermaidSequenceAnalyze(): string {
  return [
    "sequenceDiagram",
    "  participant FE as RULEX Web",
    "  participant API as TransactionController",
    "  participant ENG as RuleEngineService",
    "  participant DB as PostgreSQL",
    "  participant AUD as AccessLogService",
    "  FE->>API: POST /api/analyze (transaction)",
    "  API->>AUD: log request (headers, route, outcome)",
    "  API->>ENG: analyze(transaction)",
    "  ENG->>DB: load active rules / operators",
    "  DB-->>ENG: rules",
    "  ENG-->>API: decision + score + reasons",
    "  API->>DB: persist transaction + audit",
    "  DB-->>API: ok",
    "  API-->>FE: 200 decision payload",
  ].join("\n");
}

function solutionMermaidC4Container(): string {
  return [
    "flowchart LR",
    "  subgraph Users[Users]",
    "    U1[Analista]",
    "    U2[Admin]",
    "  end",
    "  subgraph RULEX[RULEX Platform]",
    "    FE[RULEX Web (React)]",
    "    API[API (Spring Boot)]",
    "    ENG[Rules Engine]",
    "    DB[(PostgreSQL)]",
    "    CACHE[(Redis)]",
    "  end",
    "  subgraph Obs[Observability]",
    "    LOGS[Audit/Logs]",
    "    METRICS[Metrics]",
    "  end",
    "  U1 --> FE",
    "  U2 --> FE",
    "  FE --> API",
    "  API --> ENG",
    "  ENG --> DB",
    "  ENG --> CACHE",
    "  API --> LOGS",
    "  ENG --> LOGS",
    "  LOGS --> METRICS",
  ].join("\n");
}

function solutionMermaidErCore(): string {
  return [
    "erDiagram",
    "  TRANSACTION ||--o{ RULE_EVAL : evaluated_by",
    "  RULE_EVAL }o--|| RULE : references",
    "  TRANSACTION {",
    "    uuid id",
    "    string externalTransactionId",
    "    decimal amount",
    "    string currency",
    "  }",
    "  RULE {",
    "    uuid id",
    "    string name",
    "    string status",
    "  }",
    "  RULE_EVAL {",
    "    uuid id",
    "    uuid transactionId",
    "    uuid ruleId",
    "    int scoreImpact",
    "  }",
  ].join("\n");
}

export const SOLUTION_DIAGRAMS: DiagramCatalogItem[] = [
  {
    id: "RULEX/FLOW_analyze",
    notation: "FLOWCHART",
    canonicalName: "Fluxo real: /analyze (RULEX)",
    aliases: ["/analyze", "transaction", "score", "decision"],
    origin: "solution",
    verified: true,
    verificationNotes: "Representação fiel do fluxo ponta-a-ponta (FE→API→Engine→DB/Audit).",
    categoryId: "processos",
    categoryLabel: getDiagramCategory("processos").label,
    descriptionWhenToUse: "Fluxo operacional real da análise de transação no RULEX.",
    formatsSupported: ["mermaid"],
    rendererId: "mermaid",
    rendererStatus: "OK",
    sample: { kind: "inline", format: "mermaid", content: solutionMermaidFlowAnalyze() },
  },
  {
    id: "RULEX/SEQ_analyze",
    notation: "UML",
    canonicalName: "Sequência real: FE → API → Engine → DB",
    aliases: ["sequence", "TransactionController", "RuleEngineService"],
    origin: "solution",
    verified: true,
    verificationNotes: "Sequência real do request /analyze e persistência/auditoria.",
    categoryId: "api",
    categoryLabel: getDiagramCategory("api").label,
    descriptionWhenToUse: "Entender a ordem de chamadas e persistência do /analyze.",
    formatsSupported: ["mermaid"],
    rendererId: "mermaid",
    rendererStatus: "OK",
    sample: { kind: "inline", format: "mermaid", content: solutionMermaidSequenceAnalyze() },
  },
  {
    id: "RULEX/C4_container",
    notation: "C4",
    canonicalName: "C4 (Container): RULEX",
    aliases: ["C4", "containers", "architecture"],
    origin: "solution",
    verified: true,
    verificationNotes: "Visão de containers da solução (Web/API/DB/Cache/Obs).",
    categoryId: "arquitetura",
    categoryLabel: getDiagramCategory("arquitetura").label,
    descriptionWhenToUse: "Visão macro dos componentes implantáveis do RULEX.",
    formatsSupported: ["mermaid"],
    rendererId: "mermaid",
    rendererStatus: "OK",
    sample: { kind: "inline", format: "mermaid", content: solutionMermaidC4Container() },
  },
  {
    id: "RULEX/ER_core",
    notation: "ER",
    canonicalName: "ER (Core): Transaction ↔ Rule ↔ Evaluation",
    aliases: ["ER", "schema", "postgres"],
    origin: "solution",
    verified: true,
    verificationNotes: "Modelo de dados central (alto nível).",
    categoryId: "dados_postgres",
    categoryLabel: getDiagramCategory("dados_postgres").label,
    descriptionWhenToUse: "Entender entidades/tabelas core e relações principais.",
    formatsSupported: ["mermaid"],
    rendererId: "mermaid",
    rendererStatus: "OK",
    sample: { kind: "inline", format: "mermaid", content: solutionMermaidErCore() },
  },
];

export const INVENTORY_SOLUTION_DIAGRAMS: DiagramCatalogItem[] = buildInventorySolutionDiagrams();

/**
 * Build the massive diagram registry
 */
export const DIAGRAM_REGISTRY: DiagramCatalogItem[] = MASSIVE_DIAGRAM_CATALOG.flatMap((family) =>
  family.diagrams.map((diagram) => normalizeMassiveDiagram(diagram, family.id))
);

export const TEMPLATE_DIAGRAMS: DiagramCatalogItem[] = DIAGRAM_REGISTRY;

export const ALL_DIAGRAMS: DiagramCatalogItem[] = [...SOLUTION_DIAGRAMS, ...INVENTORY_SOLUTION_DIAGRAMS, ...TEMPLATE_DIAGRAMS];

/**
 * Get total count of diagrams in registry
 */
export function getDiagramCount(): number {
  return DIAGRAM_REGISTRY.length;
}

/**
 * Get count from massive catalog (should match registry count)
 */
export function getMassiveCatalogCount(): number {
  return getMassiveDiagramCount();
}

/**
 * Get all diagrams
 */
export function getAllDiagrams(): DiagramCatalogItem[] {
  return ALL_DIAGRAMS;
}

/**
 * Get a diagram by ID
 */
export function getDiagramById(id: string): DiagramCatalogItem | undefined {
  return ALL_DIAGRAMS.find((d) => d.id === id);
}

/**
 * Get diagrams by category
 */
export function getDiagramsByCategory(category: DiagramCategoryId): DiagramCatalogItem[] {
  return ALL_DIAGRAMS.filter((d) => d.categoryId === category);
}

/**
 * Get diagrams by notation
 */
export function getDiagramsByNotation(notation: DiagramNotation): DiagramCatalogItem[] {
  return ALL_DIAGRAMS.filter((d) => d.notation === notation);
}

/**
 * Get diagrams by renderer
 */
export function getDiagramsByRenderer(renderer: RendererId): DiagramCatalogItem[] {
  return ALL_DIAGRAMS.filter((d) => d.rendererId === renderer);
}

/**
 * Get diagrams by renderer status
 */
export function getDiagramsByStatus(status: RendererStatus): DiagramCatalogItem[] {
  return ALL_DIAGRAMS.filter((d) => d.rendererStatus === status);
}

/**
 * Search diagrams by name, description, or tags
 */
export function searchDiagrams(query: string): DiagramCatalogItem[] {
  const lowerQuery = query.toLowerCase();
  return ALL_DIAGRAMS.filter(
    (d) =>
      d.canonicalName.toLowerCase().includes(lowerQuery) ||
      d.aliases.some((alias) => alias.toLowerCase().includes(lowerQuery)) ||
      d.descriptionWhenToUse.toLowerCase().includes(lowerQuery) ||
      d.categoryLabel.toLowerCase().includes(lowerQuery)
  );
}

/**
 * Legacy aliases for compatibility
 */
export const DIAGRAM_ITEMS = ALL_DIAGRAMS;
export const findDiagramById = getDiagramById;
export function getLegacyDiagramCount(): number {
  return getDiagramCount();
}
