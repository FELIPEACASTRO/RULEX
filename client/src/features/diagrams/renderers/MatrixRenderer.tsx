import type { DiagramSource, MatrixDiagramModel } from "../types";

type Props = {
  source: DiagramSource;
};

function isMatrixModel(data: unknown): data is MatrixDiagramModel {
  if (!data || typeof data !== "object") return false;
  const d = data as any;
  return Array.isArray(d.rows) && Array.isArray(d.cols) && Array.isArray(d.cells);
}

export default function MatrixRenderer({ source }: Props) {
  const model: MatrixDiagramModel | null =
    source.kind === "json" && isMatrixModel(source.data) ? source.data : null;

  if (!model) {
    return (
      <div className="text-sm text-muted-foreground">
        Modelo de matriz ausente (esperado JSON com rows/cols/cells).
      </div>
    );
  }

  const cellMap = new Map<string, (typeof model.cells)[number]>();
  for (const cell of model.cells) {
    cellMap.set(`${cell.rowId}:${cell.colId}`, cell);
  }

  return (
    <div className="h-full w-full overflow-auto rounded-md border bg-background p-4">
      <table className="w-full border-collapse">
        <thead>
          <tr>
            <th className="border border-border bg-muted p-2 text-left text-sm font-semibold">
              {/* Empty corner */}
            </th>
            {model.cols.map((col) => (
              <th
                key={col.id}
                className="border border-border bg-muted p-2 text-left text-sm font-semibold"
              >
                {col.label}
              </th>
            ))}
          </tr>
        </thead>
        <tbody>
          {model.rows.map((row) => (
            <tr key={row.id}>
              <th className="border border-border bg-muted p-2 text-left text-sm font-semibold">
                {row.label}
              </th>
              {model.cols.map((col) => {
                const cell = cellMap.get(`${row.id}:${col.id}`);
                const bgColor = cell?.color;
                return (
                  <td
                    key={col.id}
                    className="border border-border p-2 text-center text-sm"
                    style={bgColor ? { backgroundColor: bgColor } : undefined}
                  >
                    {cell?.value ?? "—"}
                  </td>
                );
              })}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}
