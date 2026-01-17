import { useCallback, useEffect, useMemo, useRef, useState, Suspense } from "react";
import { Download, Maximize2, Minus, Plus, RotateCcw } from "lucide-react";

import { Button } from "@/components/ui/button";
import { cn } from "@/lib/utils";

import type { DiagramCatalogItem, DiagramSource } from "../types";
import { getRenderer } from "../registry/rendererRegistry";

type Props = {
  item: DiagramCatalogItem;
};

function getDownloadFilename(item: DiagramCatalogItem, source: DiagramSource): string {
  const base = item.id.replace(/[^a-z0-9:_-]/gi, "-");
  switch (source.format) {
    case "mermaid":
      return `${base}.mmd`;
    case "plantuml":
      return `${base}.puml`;
    case "bpmn":
      return `${base}.bpmn`;
    case "json":
      return `${base}.json`;
    case "svg":
      return `${base}.svg`;
    case "png":
      return `${base}.png`;
    case "pdf":
      return `${base}.pdf`;
    default:
      return `${base}.txt`;
  }
}

export function DiagramViewer({ item }: Props) {
  const renderer = getRenderer(item.rendererId);
  const containerRef = useRef<HTMLDivElement | null>(null);
  const [zoom, setZoom] = useState(1);

  const source = item.sample;

  const canZoom = renderer.supportsZoom;

  const onZoomIn = useCallback(() => setZoom((z) => Math.min(3, +(z + 0.1).toFixed(2))), []);
  const onZoomOut = useCallback(() => setZoom((z) => Math.max(0.3, +(z - 0.1).toFixed(2))), []);
  const onZoomReset = useCallback(() => setZoom(1), []);

  const onFullscreen = useCallback(async () => {
    const el = containerRef.current;
    if (!el) return;

    try {
      if (document.fullscreenElement) {
        await document.exitFullscreen();
      } else {
        await el.requestFullscreen();
      }
    } catch {
      // ignore
    }
  }, []);

  const download = useMemo(() => {
    if (source.kind === "file") {
      return { href: source.uri, filename: source.fileName ?? getDownloadFilename(item, source) };
    }

    if (source.kind === "inline") {
      const blob = new Blob([source.content], { type: "text/plain;charset=utf-8" });
      return { href: URL.createObjectURL(blob), filename: getDownloadFilename(item, source), revoke: true };
    }

    const blob = new Blob([JSON.stringify(source.data, null, 2)], {
      type: "application/json;charset=utf-8",
    });
    return { href: URL.createObjectURL(blob), filename: getDownloadFilename(item, source), revoke: true };
  }, [item, source]);

  // Cleanup object URLs when switching between sources/items.
  useEffect(() => {
    if (!download.revoke) return;
    return () => {
      try {
        URL.revokeObjectURL(download.href);
      } catch {
        // ignore
      }
    };
  }, [download.href, download.revoke]);

  const RendererComponent = renderer.Component;

  return (
    <div className="flex h-full w-full flex-col gap-3">
      <div className="flex items-start justify-between gap-3">
        <div className="min-w-0">
          <div className="truncate text-lg font-semibold">{item.canonicalName}</div>
          <div className="text-sm text-muted-foreground">{item.categoryLabel}</div>
          {item.verificationNotes ? (
            <div className="mt-1 text-xs text-muted-foreground">
              {item.verificationNotes}
            </div>
          ) : null}
        </div>

        <div className="flex items-center gap-2">
          {canZoom ? (
            <>
              <Button variant="outline" size="icon" aria-label="Zoom out" onClick={onZoomOut}>
                <Minus />
              </Button>
              <div className="w-[72px] text-center text-sm tabular-nums">{Math.round(zoom * 100)}%</div>
              <Button variant="outline" size="icon" aria-label="Zoom in" onClick={onZoomIn}>
                <Plus />
              </Button>
              <Button variant="outline" size="icon" aria-label="Reset zoom" onClick={onZoomReset}>
                <RotateCcw />
              </Button>
            </>
          ) : null}

          <Button variant="outline" size="icon" aria-label="Fullscreen" onClick={onFullscreen}>
            <Maximize2 />
          </Button>

          <Button
            asChild
            variant="outline"
            className="gap-2"
            aria-label="Download"
            onClick={() => {
              if (download.revoke) setTimeout(() => URL.revokeObjectURL(download.href), 5000);
            }}
          >
            <a href={download.href} download={download.filename}>
              <Download className="size-4" />
              <span className="hidden sm:inline">Download</span>
            </a>
          </Button>
        </div>
      </div>

      <div
        ref={containerRef}
        className={cn(
          "min-h-0 flex-1",
          "rounded-lg border bg-background p-3"
        )}
      >
        <Suspense fallback={<div className="text-sm text-muted-foreground">Carregando renderer…</div>}>
          <RendererComponent source={source} zoom={zoom} />
        </Suspense>
      </div>

      <div className="text-sm text-muted-foreground">{item.descriptionWhenToUse}</div>
    </div>
  );
}
