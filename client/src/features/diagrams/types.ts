export type DiagramNotation =
  | "FLOWCHART"
  | "BPMN"
  | "DMN"
  | "UML"
  | "C4"
  | "ARCHIMATE"
  | "DFD"
  | "EPC"
  | "GRAPH"
  | "ER"
  | "PROCESS"
  | "MATRIX"
  | "TREE"
  | "OTHER";

export type RendererStatus = "OK" | "PENDENTE";

export type DiagramCategoryId =
  | "processos"
  | "uml_estrutural"
  | "uml_comportamental"
  | "arquitetura"
  | "ddd"
  | "api"
  | "dados_postgres"
  | "dados_redis"
  | "dados_neo4j"
  | "frontend"
  | "infra"
  | "seguranca"
  | "qualidade"
  | "cs_classicos";

export type DiagramSourceKind = "inline" | "file" | "json";

export type DiagramFormat =
  | "mermaid"
  | "plantuml"
  | "bpmn"
  | "dmn"
  | "dfd"
  | "epc"
  | "matrix"
  | "svg"
  | "png"
  | "pdf"
  | "json";

export type RendererId =
  | "mermaid"
  | "bpmn"
  | "dmn"
  | "dfd"
  | "matrix"
  | "image"
  | "pdf"
  | "graph"
  | "plantuml"
  | "fallback";

export interface DiagramSourceBase {
  kind: DiagramSourceKind;
  format: DiagramFormat;
}

export interface InlineDiagramSource extends DiagramSourceBase {
  kind: "inline";
  content: string;
}

export interface FileDiagramSource extends DiagramSourceBase {
  kind: "file";
  /**
   * Conteúdo do arquivo: pode ser data URL (preferido para mock) ou URL relativa.
   * Exemplos:
   * - data:image/svg+xml;utf8,<svg ...>
   * - /assets/diagrams/xyz.svg
   */
  uri: string;
  mimeType?: string;
  fileName?: string;
}

export interface JsonDiagramSource extends DiagramSourceBase {
  kind: "json";
  data: unknown;
}

export type DiagramSource = InlineDiagramSource | FileDiagramSource | JsonDiagramSource;

export interface DiagramCatalogItem {
  /**
   * ID canônico no formato NOTACAO/NOME (ex.: UML/Sequence, C4/Container, BPMN/Process)
   */
  id: string;

  /**
   * Notação principal (UML, BPMN, C4, etc.)
   */
  notation: DiagramNotation;

  canonicalName: string;
  aliases: string[];

  categoryId: DiagramCategoryId;
  categoryLabel: string;

  descriptionWhenToUse: string;

  formatsSupported: DiagramFormat[];
  rendererId: RendererId;

  /**
   * Status do renderer: OK se funcional, PENDENTE se placeholder
   */
  rendererStatus: RendererStatus;

  /** Exemplo mínimo para demonstrar renderização */
  sample: DiagramSource;

  // Preparado para futura carga por backend
  version?: string;
  updatedAt?: string;
  author?: string;
  source?: string;
}

export interface DiagramCategory {
  id: DiagramCategoryId;
  label: string;
  description: string;
}

export interface GraphDiagramModel {
  nodes: Array<{ id: string; label?: string; x: number; y: number }>;
  edges: Array<{ id: string; from: string; to: string; label?: string }>;
}

export interface MatrixDiagramModel {
  rows: Array<{ id: string; label: string }>;
  cols: Array<{ id: string; label: string }>;
  cells: Array<{ rowId: string; colId: string; value: string; color?: string }>;
}

export interface DfdDiagramModel {
  processes: Array<{ id: string; label: string; x: number; y: number }>;
  datastores: Array<{ id: string; label: string; x: number; y: number }>;
  entities: Array<{ id: string; label: string; x: number; y: number }>;
  flows: Array<{ id: string; from: string; to: string; label?: string }>;
}
