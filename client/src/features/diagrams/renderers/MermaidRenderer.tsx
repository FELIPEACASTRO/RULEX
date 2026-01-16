import { useEffect, useMemo, useRef, useState } from "react";
import type { DiagramSource } from "../types";

type Props = {
  source: DiagramSource;
  zoom: number;
};

export default function MermaidRenderer({ source, zoom }: Props) {
  const containerRef = useRef<HTMLDivElement | null>(null);
  const [error, setError] = useState<string | null>(null);

  const code = source.kind === "inline" ? source.content : "";
  const renderId = useMemo(
    () => `mmd-${Math.random().toString(36).slice(2)}`,
    []
  );

  useEffect(() => {
    let cancelled = false;

    async function run() {
      setError(null);

      if (!containerRef.current) return;
      if (!code.trim()) {
        containerRef.current.innerHTML = "";
        return;
      }

      try {
        const mermaidMod = await import("mermaid");
        const mermaid = mermaidMod.default;

        mermaid.initialize({
          startOnLoad: false,
          securityLevel: "strict",
          theme: "dark",
          fontFamily: "ui-sans-serif, system-ui",
        });

        const { svg } = await mermaid.render(renderId, code);
        if (cancelled) return;

        containerRef.current.innerHTML = svg;
      } catch (e) {
        if (cancelled) return;
        setError(e instanceof Error ? e.message : "Erro ao renderizar Mermaid");
      }
    }

    void run();
    return () => {
      cancelled = true;
    };
  }, [code, renderId]);

  if (source.kind !== "inline") {
    return (
      <div className="text-sm text-muted-foreground">
        Fonte não suportada para Mermaid.
      </div>
    );
  }

  return (
    <div className="h-full w-full overflow-auto rounded-md bg-slate-950 p-4">
      {error ? (
        <div className="text-sm text-red-300">{error}</div>
      ) : (
        <div
          style={{ transform: `scale(${zoom})`, transformOrigin: "top left" }}
          ref={containerRef}
        />
      )}
    </div>
  );
}
