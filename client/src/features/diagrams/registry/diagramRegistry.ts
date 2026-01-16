import { LEGACY_DIAGRAM_CATEGORIES } from "./legacyCatalog";
import { getDiagramCategory } from "./categories";
import type {
  DiagramCatalogItem,
  DiagramCategoryId,
  DiagramFormat,
  DiagramSource,
  RendererId,
} from "../types";

function slugify(value: string): string {
  return value
    .toLowerCase()
    .normalize("NFKD")
    .replace(/[\u0300-\u036f]/g, "")
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/(^-|-$)/g, "")
    .slice(0, 80);
}

function svgDataUrl(title: string, subtitle?: string): string {
  const safeTitle = title.replace(/</g, "&lt;").replace(/>/g, "&gt;");
  const safeSubtitle = (subtitle ?? "placeholder")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;");

  const svg = `<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" width="960" height="540" viewBox="0 0 960 540">
  <defs>
    <linearGradient id="bg" x1="0" y1="0" x2="1" y2="1">
      <stop offset="0" stop-color="#0b1220"/>
      <stop offset="1" stop-color="#111827"/>
    </linearGradient>
  </defs>
  <rect width="960" height="540" rx="20" fill="url(#bg)"/>
  <rect x="36" y="36" width="888" height="468" rx="16" fill="#0f172a" stroke="#334155"/>
  <text x="60" y="120" fill="#e5e7eb" font-family="ui-sans-serif, system-ui" font-size="36" font-weight="700">${safeTitle}</text>
  <text x="60" y="170" fill="#94a3b8" font-family="ui-sans-serif, system-ui" font-size="20">${safeSubtitle}</text>
  <text x="60" y="470" fill="#64748b" font-family="ui-sans-serif, system-ui" font-size="16">RULEX • Central de Diagramas</text>
</svg>`;

  return `data:image/svg+xml;utf8,${encodeURIComponent(svg)}`;
}

function pickFormats(tools?: string[], name?: string): DiagramFormat[] {
  const formats = new Set<DiagramFormat>();

  const toolsStr = (tools ?? []).join("|").toLowerCase();
  const nameStr = (name ?? "").toLowerCase();

  if (toolsStr.includes("mermaid")) formats.add("mermaid");
  if (toolsStr.includes("plantuml")) formats.add("plantuml");
  if (
    nameStr.includes("bpmn") ||
    toolsStr.includes("camunda") ||
    toolsStr.includes("bizagi")
  ) {
    formats.add("bpmn");
  }

  // Sempre aceitamos um fallback em arquivo (export/placeholder)
  formats.add("svg");
  formats.add("png");
  formats.add("pdf");

  return Array.from(formats);
}

function pickRenderer(formatsSupported: DiagramFormat[]): RendererId {
  if (formatsSupported.includes("bpmn")) return "bpmn";
  if (formatsSupported.includes("mermaid")) return "mermaid";
  if (formatsSupported.includes("plantuml")) return "plantuml";
  if (formatsSupported.includes("pdf")) return "pdf";
  return "image";
}

function sampleFor(
  canonicalName: string,
  formatsSupported: DiagramFormat[],
  categoryLabel: string
): DiagramSource {
  if (formatsSupported.includes("mermaid")) {
    const label = canonicalName.replace(/"/g, "'");
    return {
      kind: "inline",
      format: "mermaid",
      content: `flowchart TD\n  A[${label}] --> B[Exemplo]\n  B --> C[Fim]`,
    };
  }

  if (formatsSupported.includes("bpmn")) {
    return {
      kind: "inline",
      format: "bpmn",
      content: `<?xml version="1.0" encoding="UTF-8"?>
<bpmn:definitions xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:bpmn="http://www.omg.org/spec/BPMN/20100524/MODEL" id="Definitions_1" targetNamespace="http://bpmn.io/schema/bpmn">
  <bpmn:process id="Process_1" isExecutable="false">
    <bpmn:startEvent id="StartEvent_1" name="Início" />
  </bpmn:process>
</bpmn:definitions>`,
    };
  }

  if (formatsSupported.includes("plantuml")) {
    return {
      kind: "inline",
      format: "plantuml",
      content: `@startuml\n' ${canonicalName}\nAlice -> Bob: exemplo\n@enduml`,
    };
  }

  return {
    kind: "file",
    format: "svg",
    uri: svgDataUrl(canonicalName, categoryLabel),
    mimeType: "image/svg+xml",
    fileName: `${slugify(canonicalName)}.svg`,
  };
}

function mapLegacyCategoryToId(legacyId: string): DiagramCategoryId {
  switch (legacyId) {
    case "process":
      return "processos";
    case "architecture":
      return "arquitetura";
    case "ddd":
      return "ddd";
    case "api":
      return "api";
    case "data":
      return "dados";
    case "frontend":
      return "frontend";
    case "infra":
      return "infra";
    case "security":
      return "seguranca";
    case "uml":
      // depende de subcategoria; fallback
      return "uml_estrutural";
    default:
      return "arquitetura";
  }
}

function mapUmlSubcategoryToId(subcategoryName: string): DiagramCategoryId {
  const n = subcategoryName.toLowerCase();
  if (n.includes("estrut")) return "uml_estrutural";
  return "uml_comportamental";
}

export function getLegacyDiagramCount(): number {
  let count = 0;
  for (const category of LEGACY_DIAGRAM_CATEGORIES) {
    if (category.diagrams) count += category.diagrams.length;
    if (category.subcategories) {
      for (const sc of category.subcategories) count += sc.diagrams.length;
    }
  }
  return count;
}

export const DIAGRAM_ITEMS: DiagramCatalogItem[] = (() => {
  const items: DiagramCatalogItem[] = [];

  for (const legacyCategory of LEGACY_DIAGRAM_CATEGORIES) {
    const baseCategoryId = mapLegacyCategoryToId(legacyCategory.id);

    const pushItem = (categoryId: DiagramCategoryId, d: any) => {
      const category = getDiagramCategory(categoryId);
      const formatsSupported = pickFormats(d.tools, d.name);
      const rendererId = pickRenderer(formatsSupported);
      const id = `${categoryId}:${slugify(d.name)}`;

      items.push({
        id,
        canonicalName: d.name,
        aliases: [d.nameEn, ...(d.tools ?? [])].filter(Boolean),
        categoryId,
        categoryLabel: category.label,
        descriptionWhenToUse: `${d.description} ${d.useCase}`.trim(),
        formatsSupported,
        rendererId,
        sample: sampleFor(d.name, formatsSupported, category.label),
      });
    };

    // diagrams at category root
    for (const d of legacyCategory.diagrams ?? []) {
      pushItem(baseCategoryId, d);
    }

    // subcategories (UML routes to structural/behavioral; others keep base category)
    for (const subcategory of legacyCategory.subcategories ?? []) {
      const categoryId =
        legacyCategory.id === "uml"
          ? mapUmlSubcategoryToId(subcategory.name)
          : baseCategoryId;

      for (const d of subcategory.diagrams) {
        pushItem(categoryId, d);
      }
    }
  }

  // Ensure IDs are unique and stable
  const seen = new Set<string>();
  for (const item of items) {
    if (seen.has(item.id)) {
      // If collision happens, append a short hash.
      item.id = `${item.id}-${Math.abs(hashString(item.canonicalName))}`;
    }
    seen.add(item.id);
  }

  return items;
})();

function hashString(value: string): number {
  let hash = 0;
  for (let i = 0; i < value.length; i++) {
    hash = (hash << 5) - hash + value.charCodeAt(i);
    hash |= 0;
  }
  return hash;
}

export function findDiagramById(id: string): DiagramCatalogItem | undefined {
  return DIAGRAM_ITEMS.find((d) => d.id === id);
}
