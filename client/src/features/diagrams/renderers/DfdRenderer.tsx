import type { DiagramSource, DfdDiagramModel } from "../types";

type Props = {
  source: DiagramSource;
  zoom: number;
};

function isDfdModel(data: unknown): data is DfdDiagramModel {
  if (!data || typeof data !== "object") return false;
  const d = data as any;
  return (
    Array.isArray(d.processes) &&
    Array.isArray(d.datastores) &&
    Array.isArray(d.entities) &&
    Array.isArray(d.flows)
  );
}

export default function DfdRenderer({ source, zoom }: Props) {
  const model: DfdDiagramModel | null =
    source.kind === "json" && isDfdModel(source.data) ? source.data : null;

  if (!model) {
    return (
      <div className="text-sm text-muted-foreground">
        Modelo DFD ausente (esperado JSON com processes/datastores/entities/flows).
      </div>
    );
  }

  const padding = 40;
  const width = 960;
  const height = 540;

  return (
    <div className="h-full w-full overflow-auto rounded-md border bg-muted/20 p-4">
      <svg
        width={width}
        height={height}
        viewBox={`0 0 ${width} ${height}`}
        style={{ transform: `scale(${zoom})`, transformOrigin: "top left" }}
      >
        <rect x={0} y={0} width={width} height={height} fill="#0f172a" />

        {/* Flows */}
        {model.flows.map((f) => {
          const allNodes = [
            ...model.processes,
            ...model.datastores,
            ...model.entities,
          ];
          const from = allNodes.find((n) => n.id === f.from);
          const to = allNodes.find((n) => n.id === f.to);
          if (!from || !to) return null;

          const x1 = padding + from.x;
          const y1 = padding + from.y;
          const x2 = padding + to.x;
          const y2 = padding + to.y;

          return (
            <g key={f.id}>
              <line
                x1={x1}
                y1={y1}
                x2={x2}
                y2={y2}
                stroke="#94a3b8"
                strokeWidth={2}
                markerEnd="url(#arrow)"
              />
              {f.label ? (
                <text
                  x={(x1 + x2) / 2}
                  y={(y1 + y2) / 2 - 6}
                  fill="#e2e8f0"
                  fontSize={11}
                  textAnchor="middle"
                >
                  {f.label}
                </text>
              ) : null}
            </g>
          );
        })}

        {/* Processes (circles) */}
        {model.processes.map((p) => {
          const x = padding + p.x;
          const y = padding + p.y;
          return (
            <g key={p.id}>
              <circle cx={x} cy={y} r={40} fill="#1e293b" stroke="#334155" />
              <text
                x={x}
                y={y + 5}
                fill="#e5e7eb"
                fontSize={12}
                textAnchor="middle"
              >
                {p.label}
              </text>
            </g>
          );
        })}

        {/* Datastores (double rectangles) */}
        {model.datastores.map((d) => {
          const x = padding + d.x;
          const y = padding + d.y;
          return (
            <g key={d.id}>
              <rect
                x={x - 70}
                y={y - 20}
                width={140}
                height={40}
                fill="#1e293b"
                stroke="#334155"
              />
              <rect
                x={x - 70}
                y={y - 16}
                width={140}
                height={32}
                fill="none"
                stroke="#334155"
              />
              <text
                x={x}
                y={y + 5}
                fill="#e5e7eb"
                fontSize={12}
                textAnchor="middle"
              >
                {d.label}
              </text>
            </g>
          );
        })}

        {/* External entities (squares) */}
        {model.entities.map((e) => {
          const x = padding + e.x;
          const y = padding + e.y;
          return (
            <g key={e.id}>
              <rect
                x={x - 60}
                y={y - 30}
                width={120}
                height={60}
                fill="#1e293b"
                stroke="#334155"
              />
              <text
                x={x}
                y={y + 5}
                fill="#e5e7eb"
                fontSize={12}
                textAnchor="middle"
              >
                {e.label}
              </text>
            </g>
          );
        })}

        <defs>
          <marker
            id="arrow"
            markerWidth="10"
            markerHeight="10"
            refX="9"
            refY="3"
            orient="auto"
            markerUnits="strokeWidth"
          >
            <path d="M0,0 L0,6 L9,3 z" fill="#94a3b8" />
          </marker>
        </defs>
      </svg>
    </div>
  );
}
