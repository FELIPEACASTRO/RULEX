import { useEffect, useRef, useState } from "react";
import type { DiagramSource } from "../types";

type Props = {
  source: DiagramSource;
};

export default function BpmnRenderer({ source }: Props) {
  const containerRef = useRef<HTMLDivElement | null>(null);
  const viewerRef = useRef<any>(null);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    let cancelled = false;

    async function mount() {
      setError(null);

      if (!containerRef.current) return;
      if (source.kind !== "inline") {
        setError("Fonte não suportada para BPMN (esperado inline XML). ");
        return;
      }

      try {
        const mod = await import("bpmn-js/lib/NavigatedViewer");
        const NavigatedViewer = (mod as any).default;

        const viewer = new NavigatedViewer({
          container: containerRef.current,
        });
        viewerRef.current = viewer;

        await viewer.importXML(source.content);
        if (cancelled) return;

        const canvas = viewer.get("canvas");
        canvas.zoom("fit-viewport");
      } catch (e) {
        if (cancelled) return;
        setError(e instanceof Error ? e.message : "Erro ao renderizar BPMN");
      }
    }

    void mount();

    return () => {
      cancelled = true;
      try {
        viewerRef.current?.destroy?.();
      } catch {
        // noop
      }
      viewerRef.current = null;
    };
  }, [source]);

  return (
    <div className="h-full w-full overflow-hidden rounded-md border bg-background">
      {error ? (
        <div className="p-3 text-sm text-red-600">{error}</div>
      ) : (
        <div ref={containerRef} className="h-full w-full" />
      )}
    </div>
  );
}
