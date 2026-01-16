export type DiagramCategoryId =
  | "processos"
  | "uml_estrutural"
  | "uml_comportamental"
  | "arquitetura"
  | "ddd"
  | "api"
  | "dados"
  | "frontend"
  | "infra"
  | "seguranca";

export type DiagramSourceKind = "inline" | "file" | "json";

export type DiagramFormat =
  | "mermaid"
  | "plantuml"
  | "bpmn"
  | "svg"
  | "png"
  | "pdf"
  | "json";

export type RendererId =
  | "mermaid"
  | "bpmn"
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
  id: string;

  canonicalName: string;
  aliases: string[];

  categoryId: DiagramCategoryId;
  categoryLabel: string;

  descriptionWhenToUse: string;

  formatsSupported: DiagramFormat[];
  rendererId: RendererId;

  /** Exemplo mínimo para demonstrar renderização */
  sample: DiagramSource;

  // Preparado para futura carga por backend
  version?: string;
  updatedAt?: string;
  author?: string;
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
