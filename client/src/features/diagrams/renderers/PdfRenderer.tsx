import type { DiagramSource } from "../types";

type Props = {
  source: DiagramSource;
};

export default function PdfRenderer({ source }: Props) {
  if (source.kind !== "file") {
    return (
      <div className="text-sm text-muted-foreground">
        Fonte não suportada para PDF.
      </div>
    );
  }

  return (
    <div className="h-full w-full overflow-hidden rounded-md border bg-background">
      <iframe
        title={source.fileName ?? "pdf"}
        src={source.uri}
        className="h-full w-full"
      />
    </div>
  );
}
