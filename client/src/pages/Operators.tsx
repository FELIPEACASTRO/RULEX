import { BACKEND_OPERATORS } from "@/manual/generated/backendOperators.generated";

export default function Operators() {
  return (
    <div className="space-y-4">
      <div className="rounded-lg border bg-card p-4">
        <h1 className="text-xl font-semibold text-foreground">Operadores</h1>
        <p className="text-sm text-muted-foreground">
          Lista estática de operadores suportados pelo RULEX ({BACKEND_OPERATORS.length}).
        </p>
      </div>

      <div className="rounded-lg border bg-card">
        <div className="border-b px-4 py-2 text-sm font-medium text-muted-foreground">
          Nomes dos operadores
        </div>
        <div className="grid gap-2 px-4 py-3 sm:grid-cols-2 lg:grid-cols-3">
          {BACKEND_OPERATORS.map((operator) => (
            <div
              key={operator.name}
              className="rounded-md border border-border/60 bg-background px-3 py-2 text-sm"
            >
              {operator.name}
            </div>
          ))}
        </div>
      </div>
    </div>
  );
}
