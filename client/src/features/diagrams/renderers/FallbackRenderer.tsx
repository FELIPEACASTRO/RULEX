import type { DiagramSource } from "../types";

type Props = {
  source: DiagramSource;
};

export default function FallbackRenderer({ source }: Props) {
  return (
    <div className="h-full w-full overflow-auto rounded-md border bg-muted/20 p-4">
      <div className="mb-3 text-sm text-muted-foreground">
        Não há renderer disponível para este formato.
      </div>
      <pre className="whitespace-pre-wrap rounded-md bg-background p-3 text-xs">
        {JSON.stringify(source, null, 2)}
      </pre>
    </div>
  );
}
