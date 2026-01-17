import { getDiagramCategory } from "./categories";
import type { DiagramCatalogItem, DiagramCategoryId, DiagramNotation } from "../types";

type InventoryEvidence = { file: string; line?: number; snippet?: string };

type InventoryController = {
  className: string;
  basePath?: string;
  evidence?: InventoryEvidence;
  endpoints?: Array<{ httpMethod: string; path: string; evidence?: InventoryEvidence }>;
};

type InventoryService = { className: string; evidence?: InventoryEvidence };

type InventoryEntity = { className: string; evidence?: InventoryEvidence };

type InventoryMigration = { version: number; name: string; file: string };

type InventoryDockerService = { name: string; evidence?: InventoryEvidence };

type RulexInventory = {
  generatedAt?: string;
  backend?: {
    controllers?: InventoryController[];
    services?: InventoryService[];
    entities?: InventoryEntity[];
    migrations?: InventoryMigration[];
    configFiles?: string[];
  };
  frontend?: {
    pages?: Array<{ file: string }>;
  };
  infra?: {
    dockerCompose?: { file: string; services?: InventoryDockerService[] };
    dockerfiles?: Array<{ file: string }>;
  };
  openapi?: {
    specs?: Array<{ file: string }>;
  };
};

function slugify(text: string): string {
  return text
    .toLowerCase()
    .normalize("NFD")
    .replace(/[\u0300-\u036f]/g, "")
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-+|-+$/g, "");
}

function loadInventory(): RulexInventory | null {
  try {
    const modules = import.meta.glob("../../../../../docs/DIAGRAMS_RULEX_INVENTORY.json", {
      query: "?raw",
      import: "default",
      eager: true,
    }) as Record<string, string>;

    const raw = Object.values(modules)[0];
    if (!raw) return null;

    return JSON.parse(raw) as RulexInventory;
  } catch {
    return null;
  }
}

function evidenceLine(e?: InventoryEvidence): string {
  if (!e?.file) return "";
  return e.line ? `${e.file}:${e.line}` : e.file;
}

function controllerDiagramMermaid(ctrl: InventoryController): string {
  const header = `  C["${ctrl.className}${ctrl.basePath ? `\\n(${ctrl.basePath})` : ""}"]`;
  const endpoints = (ctrl.endpoints ?? []).slice(0, 50).map((ep, i) => {
    const node = `E${i + 1}["${ep.httpMethod} ${ep.path}"]`;
    return `  ${header}\n  C --> ${node}`;
  });

  if (endpoints.length === 0) {
    return ["flowchart TD", header, "  C --> E1[\"(sem endpoints detectados)\"]"].join("\n");
  }

  // De-duplicate the header by taking it from the first entry.
  const first = endpoints[0];
  return ["flowchart TD", first.split("\n")[0], ...endpoints.map((e) => e.split("\n")[1])].join("\n");
}

function endpointSequenceMermaid(ctrl: InventoryController, httpMethod: string, path: string): string {
  return [
    "sequenceDiagram",
    "  participant FE as RULEX Web",
    `  participant API as ${ctrl.className}`,
    `  FE->>API: ${httpMethod} ${path}`,
    "  API-->>FE: 2xx/4xx (response)",
  ].join("\n");
}

function simpleClassDiagram(title: string, members?: string[]): string {
  const lines = ["classDiagram", `  class ${title} {`];
  if (members && members.length > 0) {
    for (const m of members.slice(0, 20)) lines.push(`    ${m}`);
  }
  lines.push("  }");
  return lines.join("\n");
}

function asSolutionItem(params: {
  id: string;
  notation: DiagramNotation;
  canonicalName: string;
  aliases: string[];
  categoryId: DiagramCategoryId;
  descriptionWhenToUse: string;
  verificationNotes: string;
  sampleMermaid: string;
}): DiagramCatalogItem {
  const category = getDiagramCategory(params.categoryId);
  return {
    id: params.id,
    notation: params.notation,
    canonicalName: params.canonicalName,
    aliases: params.aliases,
    origin: "solution",
    verified: true,
    verificationNotes: params.verificationNotes,
    categoryId: params.categoryId,
    categoryLabel: category.label,
    descriptionWhenToUse: params.descriptionWhenToUse,
    formatsSupported: ["mermaid"],
    rendererId: "mermaid",
    rendererStatus: "OK",
    sample: { kind: "inline", format: "mermaid", content: params.sampleMermaid },
  };
}

export function buildInventorySolutionDiagrams(): DiagramCatalogItem[] {
  const inv = loadInventory();
  if (!inv) return [];

  const generatedAt = inv.generatedAt ? ` (inventory ${inv.generatedAt})` : "";

  const out: DiagramCatalogItem[] = [];

  // Controllers: one diagram per controller showing endpoints.
  for (const ctrl of inv.backend?.controllers ?? []) {
    const id = `RULEX/CTRL_${slugify(ctrl.className)}`;
    const note = `Extraído do inventário${generatedAt}. Evidência: ${evidenceLine(ctrl.evidence)}`.trim();
    out.push(
      asSolutionItem({
        id,
        notation: "UML",
        canonicalName: `API Controller: ${ctrl.className}`,
        aliases: [ctrl.basePath ?? "", "controller"].filter(Boolean),
        categoryId: "api",
        descriptionWhenToUse: "Mapa dos endpoints expostos por um controller no backend.",
        verificationNotes: note,
        sampleMermaid: controllerDiagramMermaid(ctrl),
      })
    );

    // Endpoints: one diagram per endpoint for true 1-by-1 coverage.
    for (const ep of ctrl.endpoints ?? []) {
      const epId = `RULEX/EP_${slugify(`${ep.httpMethod}_${ep.path}_${ctrl.className}`)}`;
      const epNote = `Extraído do inventário${generatedAt}. Evidência: ${evidenceLine(ep.evidence)}`.trim();
      out.push(
        asSolutionItem({
          id: epId,
          notation: "UML",
          canonicalName: `Endpoint: ${ep.httpMethod} ${ep.path}`,
          aliases: [ctrl.className, ep.httpMethod, ep.path],
          categoryId: "api",
          descriptionWhenToUse: "Endpoint real detectado no backend (inventário).",
          verificationNotes: epNote,
          sampleMermaid: endpointSequenceMermaid(ctrl, ep.httpMethod, ep.path),
        })
      );
    }
  }

  // Services: one diagram per service.
  for (const svc of inv.backend?.services ?? []) {
    const id = `RULEX/SVC_${slugify(svc.className)}`;
    const note = `Extraído do inventário${generatedAt}. Evidência: ${evidenceLine(svc.evidence)}`.trim();
    out.push(
      asSolutionItem({
        id,
        notation: "ARCHIMATE",
        canonicalName: `Service: ${svc.className}`,
        aliases: ["service"],
        categoryId: "arquitetura",
        descriptionWhenToUse: "Mapa de serviços Spring detectados no backend.",
        verificationNotes: note,
        sampleMermaid: simpleClassDiagram(svc.className, ["<<Service>>"]),
      })
    );
  }

  // Entities: one diagram per entity class.
  for (const ent of inv.backend?.entities ?? []) {
    const id = `RULEX/ENT_${slugify(ent.className)}`;
    const note = `Extraído do inventário${generatedAt}. Evidência: ${evidenceLine(ent.evidence)}`.trim();
    out.push(
      asSolutionItem({
        id,
        notation: "ER",
        canonicalName: `Entity: ${ent.className}`,
        aliases: ["entity", "@Entity"],
        categoryId: "dados_postgres",
        descriptionWhenToUse: "Inventário das entidades JPA presentes no backend.",
        verificationNotes: note,
        sampleMermaid: simpleClassDiagram(ent.className, ["<<Entity>>"]),
      })
    );
  }

  // Migrations: timeline overview.
  const migrations = inv.backend?.migrations ?? [];
  if (migrations.length > 0) {
    const items = migrations
      .slice()
      .sort((a, b) => a.version - b.version)
      .slice(0, 60)
      .map((m) => `  V${m.version}["V${m.version} ${m.name}"]`);

    const links = migrations
      .slice()
      .sort((a, b) => a.version - b.version)
      .slice(0, 60)
      .map((m, i, arr) => {
        if (i === 0) return null;
        return `  V${arr[i - 1].version} --> V${m.version}`;
      })
      .filter(Boolean) as string[];

    out.push(
      asSolutionItem({
        id: "RULEX/DB_migrations",
        notation: "PROCESS",
        canonicalName: "DB: Flyway migrations (timeline)",
        aliases: ["flyway", "migration", "sql"],
        categoryId: "dados_postgres",
        descriptionWhenToUse: "Linha do tempo das migrações Flyway detectadas no repositório.",
        verificationNotes: `Extraído do inventário${generatedAt}.`,
        sampleMermaid: ["flowchart LR", ...items, ...links].join("\n"),
      })
    );
  }

  // Frontend pages: navigation inventory.
  const pages = inv.frontend?.pages ?? [];
  if (pages.length > 0) {
    const nodes = pages.slice(0, 80).map((p, i) => `  P${i + 1}["${p.file}"]`);
    out.push(
      asSolutionItem({
        id: "RULEX/FE_pages",
        notation: "TREE",
        canonicalName: "Frontend: Pages inventory",
        aliases: ["pages", "routes", "react"],
        categoryId: "frontend",
        descriptionWhenToUse: "Inventário de páginas React presentes no frontend.",
        verificationNotes: `Extraído do inventário${generatedAt}.`,
        sampleMermaid: ["flowchart TD", "  FE[client/src/pages]", ...nodes.map((n) => `  FE --> ${n.trim()}`)].join("\n"),
      })
    );
  }

  // Infra: docker compose services.
  const compose = inv.infra?.dockerCompose;
  if (compose?.services && compose.services.length > 0) {
    const nodes = compose.services.map((s) => `  ${slugify(s.name).toUpperCase()}["${s.name}"]`);
    const chain = compose.services
      .map((s) => slugify(s.name).toUpperCase())
      .map((id) => `  STACK --> ${id}`);

    out.push(
      asSolutionItem({
        id: "RULEX/INFRA_docker_compose",
        notation: "C4",
        canonicalName: "Infra: docker-compose services",
        aliases: ["docker", "compose", "infra"],
        categoryId: "infra",
        descriptionWhenToUse: "Visão do stack de desenvolvimento (docker-compose).",
        verificationNotes: `Extraído do inventário${generatedAt}. Evidência: ${compose.file}`,
        sampleMermaid: ["flowchart LR", "  STACK[\"docker-compose\"]", ...nodes, ...chain].join("\n"),
      })
    );
  }

  // OpenAPI specs.
  const specs = inv.openapi?.specs ?? [];
  if (specs.length > 0) {
    out.push(
      asSolutionItem({
        id: "RULEX/API_openapi_specs",
        notation: "UML",
        canonicalName: "API: OpenAPI specs",
        aliases: ["openapi", "swagger", "springdoc"],
        categoryId: "api",
        descriptionWhenToUse: "Localização das specs OpenAPI no repositório.",
        verificationNotes: `Extraído do inventário${generatedAt}.`,
        sampleMermaid: [
          "flowchart TD",
          "  API[\"API contract\"]",
          ...specs.map((s, i) => `  API --> S${i + 1}[\"${s.file}\"]`),
        ].join("\n"),
      })
    );
  }

  return out;
}
