import { useEffect, useMemo, useRef, useState } from "react";
import { useVirtualizer } from "@tanstack/react-virtual";
import { Search } from "lucide-react";

import { Badge } from "@/components/ui/badge";
import { Input } from "@/components/ui/input";
import { Tabs, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { ResizableHandle, ResizablePanel, ResizablePanelGroup } from "@/components/ui/resizable";
import { cn } from "@/lib/utils";

import { DIAGRAM_ITEMS } from "../registry/diagramRegistry";
import { DIAGRAM_CATEGORIES } from "../registry/categories";
import type { DiagramCategoryId, DiagramCatalogItem, RendererId, RendererStatus } from "../types";
import { DiagramViewer } from "./DiagramViewer";

type CategoryFilter = "all" | DiagramCategoryId;

type RendererFilter = "all" | RendererId;

type RendererStatusFilter = "all" | RendererStatus;

type CatalogMode = "all" | "solution" | "template";

function normalize(text: string): string {
  return text
    .toLowerCase()
    .normalize("NFKD")
    .replace(/[\u0300-\u036f]/g, "");
}

export default function DiagramsHub() {
  const [query, setQuery] = useState("");
  const [mode, setMode] = useState<CatalogMode>("solution");
  const [category, setCategory] = useState<CategoryFilter>("all");
  const [renderer, setRenderer] = useState<RendererFilter>("all");
  const [rendererStatus, setRendererStatus] = useState<RendererStatusFilter>("all");

  const [selectedId, setSelectedId] = useState<string>(() => DIAGRAM_ITEMS[0]?.id ?? "");

  const filtered = useMemo(() => {
    const q = normalize(query.trim());

    return DIAGRAM_ITEMS.filter((d) => {
      if (mode === "solution" && d.origin !== "solution") return false;
      if (mode === "template" && d.origin !== "template") return false;
      if (category !== "all" && d.categoryId !== category) return false;
      if (renderer !== "all" && d.rendererId !== renderer) return false;
      if (rendererStatus !== "all" && d.rendererStatus !== rendererStatus) return false;

      if (!q) return true;

      const hay = normalize(
        [d.canonicalName, d.categoryLabel, d.descriptionWhenToUse, ...d.aliases].join(" ")
      );
      return hay.includes(q);
    });
  }, [query, mode, category, renderer, rendererStatus]);

  const statusCounts = useMemo(() => {
    let ok = 0;
    let pending = 0;
    for (const d of filtered) {
      if (d.rendererStatus === "OK") ok += 1;
      else pending += 1;
    }
    return { ok, pending };
  }, [filtered]);

  const availableRenderers = useMemo(() => {
    const ids = new Set<RendererId>();
    for (const item of DIAGRAM_ITEMS) {
      if (mode === "solution" && item.origin !== "solution") continue;
      if (mode === "template" && item.origin !== "template") continue;
      ids.add(item.rendererId);
    }
    return Array.from(ids).sort();
  }, [mode]);

  const selected: DiagramCatalogItem | undefined = useMemo(
    () => filtered.find((d) => d.id === selectedId) ?? DIAGRAM_ITEMS.find((d) => d.id === selectedId) ?? filtered[0],
    [filtered, selectedId]
  );

  // Keep selection coherent with current filters to avoid showing an item that's not in the list.
  useEffect(() => {
    if (filtered.length === 0) return;
    const stillVisible = filtered.some((d) => d.id === selectedId);
    if (!stillVisible) setSelectedId(filtered[0].id);
  }, [filtered, selectedId]);

  const parentRef = useRef<HTMLDivElement | null>(null);
  const rowVirtualizer = useVirtualizer({
    count: filtered.length,
    getScrollElement: () => parentRef.current,
    estimateSize: () => 64,
    overscan: 12,
  });

  return (
    <div className="h-[calc(100vh-64px)] w-full p-4">
      <div className="mb-4 flex flex-wrap items-center justify-between gap-3">
        <div>
          <h1 className="text-xl font-semibold">Central de Diagramas do RULEX</h1>
          <p className="text-sm text-muted-foreground">
            Diagramas da solução (verificados) + templates (referência) com filtros e preview.
          </p>
        </div>

        <div className="flex items-center gap-2">
          <Badge variant="secondary">{filtered.length} itens</Badge>
          <Badge variant="secondary">OK: {statusCounts.ok}</Badge>
          <Badge variant={statusCounts.pending > 0 ? "destructive" : "secondary"}>
            PENDENTE: {statusCounts.pending}
          </Badge>
          <Badge variant={mode === "solution" ? "default" : "secondary"}>
            {mode === "all" ? "Todos (solução + templates)" : mode === "solution" ? "Solução (verificado)" : "Templates"}
          </Badge>
        </div>
      </div>

      <ResizablePanelGroup direction="horizontal" className="h-[calc(100%-72px)] rounded-lg border">
        <ResizablePanel defaultSize={38} minSize={26} className="bg-background">
          <div className="flex h-full flex-col">
            <div className="border-b p-3">
              <div className="mb-3 flex flex-wrap items-center justify-between gap-2">
                <Tabs value={mode} onValueChange={(v) => setMode(v as CatalogMode)}>
                  <TabsList className="flex flex-wrap">
                    <TabsTrigger value="all">Todos</TabsTrigger>
                    <TabsTrigger value="solution">Solução</TabsTrigger>
                    <TabsTrigger value="template">Templates</TabsTrigger>
                  </TabsList>
                </Tabs>

                {mode === "template" ? (
                  <div className="text-xs text-muted-foreground">
                    Templates são referência didática e podem não refletir o RULEX real.
                  </div>
                ) : mode === "all" ? (
                  <div className="text-xs text-muted-foreground">
                    Exibindo solução + templates (use os badges para diferenciar).
                  </div>
                ) : null}
              </div>

              <div className="relative">
                <Search className="pointer-events-none absolute left-2 top-2.5 size-4 text-muted-foreground" />
                <Input
                  value={query}
                  onChange={(e) => setQuery(e.target.value)}
                  placeholder="Buscar diagramas (nome, alias, descrição, ferramenta…)"
                  className="pl-8"
                  aria-label="Buscar diagramas"
                />
              </div>

              <div className="mt-3 flex flex-wrap items-center justify-between gap-2">
                <Tabs value={category} onValueChange={(v) => setCategory(v as CategoryFilter)}>
                  <TabsList className="flex flex-wrap">
                    <TabsTrigger value="all">Todos</TabsTrigger>
                    {Object.values(DIAGRAM_CATEGORIES).map((c) => (
                      <TabsTrigger key={c.id} value={c.id}>
                        {c.label}
                      </TabsTrigger>
                    ))}
                  </TabsList>
                </Tabs>

                <select
                  value={renderer}
                  onChange={(e) => setRenderer(e.target.value as RendererFilter)}
                  className={cn(
                    "h-9 rounded-md border bg-background px-2 text-sm",
                    "focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring"
                  )}
                  aria-label="Filtro de renderer"
                >
                  <option value="all">Todos renderers</option>
                  {availableRenderers.map((r) => (
                    <option key={r} value={r}>
                      {r}
                    </option>
                  ))}
                </select>

                <select
                  value={rendererStatus}
                  onChange={(e) => setRendererStatus(e.target.value as RendererStatusFilter)}
                  className={cn(
                    "h-9 rounded-md border bg-background px-2 text-sm",
                    "focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring"
                  )}
                  aria-label="Filtro de status do renderer"
                >
                  <option value="all">Todos status</option>
                  <option value="OK">OK</option>
                  <option value="PENDENTE">PENDENTE</option>
                </select>
              </div>
            </div>

            <div ref={parentRef} className="min-h-0 flex-1 overflow-auto">
              <div
                style={{ height: rowVirtualizer.getTotalSize(), position: "relative" }}
                className="w-full"
              >
                {rowVirtualizer.getVirtualItems().map((row) => {
                  const item = filtered[row.index];
                  const isSelected = item?.id === selected?.id;

                  return (
                    <button
                      key={item.id}
                      type="button"
                      onClick={() => setSelectedId(item.id)}
                      className={cn(
                        "absolute left-0 right-0 flex w-full items-start justify-between gap-3 border-b p-3 text-left",
                        "hover:bg-accent/50",
                        isSelected && "bg-accent"
                      )}
                      style={{ transform: `translateY(${row.start}px)` }}
                      aria-selected={isSelected}
                    >
                      <div className="min-w-0">
                        <div className="truncate font-medium">{item.canonicalName}</div>
                        <div className="mt-1 truncate text-xs text-muted-foreground">
                          {item.categoryLabel} • {item.rendererId}
                        </div>
                      </div>
                      <div className="flex shrink-0 items-center gap-2">
                        {item.verified ? <Badge>verificado</Badge> : <Badge variant="secondary">template</Badge>}
                        {item.rendererStatus === "OK" ? (
                          <Badge variant="secondary">OK</Badge>
                        ) : (
                          <Badge variant="destructive">PENDENTE</Badge>
                        )}
                        {item.formatsSupported.includes("mermaid") ? (
                          <Badge variant="secondary">mmd</Badge>
                        ) : null}
                        {item.formatsSupported.includes("bpmn") ? (
                          <Badge variant="secondary">bpmn</Badge>
                        ) : null}
                        {item.formatsSupported.includes("plantuml") ? (
                          <Badge variant="secondary">puml</Badge>
                        ) : null}
                      </div>
                    </button>
                  );
                })}
              </div>

              {filtered.length === 0 ? (
                <div className="p-4 text-sm text-muted-foreground">
                  Nenhum diagrama encontrado.
                </div>
              ) : null}
            </div>
          </div>
        </ResizablePanel>

        <ResizableHandle withHandle />

        <ResizablePanel defaultSize={62} minSize={38} className="bg-background">
          <div className="h-full p-4">
            {selected ? (
              <DiagramViewer item={selected} />
            ) : (
              <div className="text-sm text-muted-foreground">Selecione um diagrama.</div>
            )}
          </div>
        </ResizablePanel>
      </ResizablePanelGroup>
    </div>
  );
}
