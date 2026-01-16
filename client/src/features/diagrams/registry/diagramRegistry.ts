/**
 * diagramRegistry.ts
 *
 * Central registry for the MASSIVE diagram catalog (200+ types).
 * Now uses NOTACAO/NOME ID format and tracks renderer status.
 */

import { MASSIVE_DIAGRAM_CATALOG, type MassiveDiagramFamily, getMassiveDiagramCount } from "./massiveCatalog";
import { getDiagramCategory } from "./categories";
import type { 
  DiagramCatalogItem, 
  DiagramCategoryId, 
  DiagramFormat, 
  DiagramNotation,
  RendererId,
  RendererStatus,
} from "../types";

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
  if (notation === "ER" || notation === "GRAPH") return "mermaid"; // Use Mermaid for ER/GRAPH
  if (notation === "EPC") return "epc";
  return "mermaid"; // Default for flowcharts, UML, trees, etc.
}

/**
 * Determine renderer status
 */
function determineRendererStatus(format: DiagramFormat): RendererStatus {
  // Renderers marked as "OK" (functional)
  const okRenderers: DiagramFormat[] = ["mermaid", "bpmn", "pdf", "dfd", "matrix"];
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
    case "epc":
      return "fallback"; // EPC not yet implemented
    default:
      return "mermaid";
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
    categoryId,
    categoryLabel: category.label,
    descriptionWhenToUse: `${diagramType.description}. ${diagramType.useCase}`.trim(),
    formatsSupported: [format],
    rendererId: renderer,
    rendererStatus,
    sample: {
      kind: "inline",
      format: "mermaid",
      content: `flowchart TD\n  A[${diagramType.name}] --> B[Exemplo]`,
    },
    source: diagramType.example,
  };
}

/**
 * Build the massive diagram registry
 */
export const DIAGRAM_REGISTRY: DiagramCatalogItem[] = MASSIVE_DIAGRAM_CATALOG.flatMap((family) =>
  family.diagrams.map((diagram) => normalizeMassiveDiagram(diagram, family.id))
);

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
  return DIAGRAM_REGISTRY;
}

/**
 * Get a diagram by ID
 */
export function getDiagramById(id: string): DiagramCatalogItem | undefined {
  return DIAGRAM_REGISTRY.find((d) => d.id === id);
}

/**
 * Get diagrams by category
 */
export function getDiagramsByCategory(category: DiagramCategoryId): DiagramCatalogItem[] {
  return DIAGRAM_REGISTRY.filter((d) => d.categoryId === category);
}

/**
 * Get diagrams by notation
 */
export function getDiagramsByNotation(notation: DiagramNotation): DiagramCatalogItem[] {
  return DIAGRAM_REGISTRY.filter((d) => d.notation === notation);
}

/**
 * Get diagrams by renderer
 */
export function getDiagramsByRenderer(renderer: RendererId): DiagramCatalogItem[] {
  return DIAGRAM_REGISTRY.filter((d) => d.rendererId === renderer);
}

/**
 * Get diagrams by renderer status
 */
export function getDiagramsByStatus(status: RendererStatus): DiagramCatalogItem[] {
  return DIAGRAM_REGISTRY.filter((d) => d.rendererStatus === status);
}

/**
 * Search diagrams by name, description, or tags
 */
export function searchDiagrams(query: string): DiagramCatalogItem[] {
  const lowerQuery = query.toLowerCase();
  return DIAGRAM_REGISTRY.filter(
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
export const DIAGRAM_ITEMS = DIAGRAM_REGISTRY;
export const findDiagramById = getDiagramById;
export function getLegacyDiagramCount(): number {
  return getDiagramCount();
}
