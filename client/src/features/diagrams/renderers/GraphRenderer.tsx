import type { DiagramSource, GraphDiagramModel } from "../types";

type Props = {
  source: DiagramSource;
  zoom: number;
};

function isGraphModel(data: unknown): data is GraphDiagramModel {
  if (!data || typeof data !== "object") return false;
  const d = data as any;
  return Array.isArray(d.nodes) && Array.isArray(d.edges);
}

export default function GraphRenderer({ source, zoom }: Props) {
  const model: GraphDiagramModel | null =
    source.kind === "json" && isGraphModel(source.data) ? source.data : null;

  if (!model) {
    return (
      <div className="text-sm text-muted-foreground">
        Modelo de grafo ausente (esperado JSON com nodes/edges).
      </div>
    );
  }

  const padding = 40;
  const width = 960;
  const height = 540;

  const nodeById = new Map(model.nodes.map((n) => [n.id, n]));

  return (
    <div className="h-full w-full overflow-auto rounded-md border bg-muted/20 p-4">
      <svg
        width={width}
        height={height}
        viewBox={`0 0 ${width} ${height}`}
        style={{ transform: `scale(${zoom})`, transformOrigin: "top left" }}
      >
        <rect x={0} y={0} width={width} height={height} fill="#0f172a" />

        {model.edges.map((e) => {
          const from = nodeById.get(e.from);
          const to = nodeById.get(e.to);
          if (!from || !to) return null;

          const x1 = padding + from.x;
          const y1 = padding + from.y;
          const x2 = padding + to.x;
          const y2 = padding + to.y;

          return (
            <g key={e.id}>
              <line
                x1={x1}
                y1={y1}
                x2={x2}
                y2={y2}
                stroke="#94a3b8"
                strokeWidth={2}
              />
              {e.label ? (
                <text
                  x={(x1 + x2) / 2}
                  y={(y1 + y2) / 2 - 6}
                  fill="#e2e8f0"
                  fontSize={12}
                  textAnchor="middle"
                >
                  {e.label}
                </text>
              ) : null}
            </g>
          );
        })}

        {model.nodes.map((n) => {
          const x = padding + n.x;
          const y = padding + n.y;
          return (
            <g key={n.id}>
              <rect
                x={x - 70}
                y={y - 22}
                width={140}
                height={44}
                rx={10}
                fill="#1e293b"
                stroke="#334155"
              />
              <text
                x={x}
                y={y + 5}
                fill="#e5e7eb"
                fontSize={13}
                textAnchor="middle"
              >
                {n.label ?? n.id}
              </text>
            </g>
          );
        })}
      </svg>
    </div>
  );
}
