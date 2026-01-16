import type { DiagramSource } from "../types";

type Props = {
  source: DiagramSource;
  zoom: number;
};

export default function ImageRenderer({ source, zoom }: Props) {
  if (source.kind !== "file") {
    return (
      <div className="text-sm text-muted-foreground">
        Fonte não suportada para imagem.
      </div>
    );
  }

  return (
    <div className="h-full w-full overflow-auto rounded-md border bg-muted/20 p-4">
      <div style={{ transform: `scale(${zoom})`, transformOrigin: "top left" }}>
        <img
          src={source.uri}
          alt={source.fileName ?? "diagram"}
          className="max-w-none"
        />
      </div>
    </div>
  );
}
