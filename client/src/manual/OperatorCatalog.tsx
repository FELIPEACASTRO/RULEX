/**
 * OperatorCatalog.tsx - Catálogo de TODOS os 448 operadores do RULEX
 *
 * Features:
 * - Accordion por categoria (performance)
 * - Filtros: categoria, requiresValue, Neo4j, Velocity, NULL behavior
 * - Busca por value/label/description
 * - Badges informativos
 * - Explicações didáticas por categoria
 */
import { useMemo, useState } from "react";

import {
  Accordion,
  AccordionContent,
  AccordionItem,
  AccordionTrigger,
} from "@/components/ui/accordion";
import { Badge } from "@/components/ui/badge";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Checkbox } from "@/components/ui/checkbox";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "@/components/ui/table";
import {
  Tooltip,
  TooltipContent,
  TooltipProvider,
  TooltipTrigger,
} from "@/components/ui/tooltip";

import {
  OPERATORS,
  getOperatorsByCategory,
  getOperatorCategories,
  getNullBehavior,
  OPERATOR_CATEGORY_EXPLANATIONS,
  NULL_BEHAVIOR_LABELS,
  type OperatorDefinition,
  type NullBehavior,
} from "./manualData";

// Tipos de filtro
type NullBehaviorFilter = "all" | NullBehavior;

interface Filters {
  search: string;
  category: string;
  requiresValue: "all" | "yes" | "no";
  isNeo4j: boolean;
  isVelocity: boolean;
  nullBehavior: NullBehaviorFilter;
}

const INITIAL_FILTERS: Filters = {
  search: "",
  category: "all",
  requiresValue: "all",
  isNeo4j: false,
  isVelocity: false,
  nullBehavior: "all",
};

// Funções auxiliares para identificar tipos de operadores
function isNeo4jOperator(op: OperatorDefinition): boolean {
  return op.value.startsWith("NEO4J_") || op.category === "Neo4j Graph";
}

function isVelocityOperator(op: OperatorDefinition): boolean {
  return (
    op.value.includes("VELOCITY") ||
    op.category?.toLowerCase().includes("velocity") ||
    op.value.includes("_COUNT_") ||
    op.value.includes("_SUM_") ||
    op.value.includes("_AVG_")
  );
}

// Cores por tipo de NULL behavior
const NULL_BEHAVIOR_COLORS: Record<NullBehavior, string> = {
  returns_false: "bg-red-100 text-red-800 dark:bg-red-900 dark:text-red-200",
  returns_true: "bg-green-100 text-green-800 dark:bg-green-900 dark:text-green-200",
  checks_null: "bg-blue-100 text-blue-800 dark:bg-blue-900 dark:text-blue-200",
  context_dependent: "bg-yellow-100 text-yellow-800 dark:bg-yellow-900 dark:text-yellow-200",
  not_applicable: "bg-gray-100 text-gray-800 dark:bg-gray-700 dark:text-gray-200",
};

export function OperatorCatalog() {
  const [filters, setFilters] = useState<Filters>(INITIAL_FILTERS);
  const [openCategories, setOpenCategories] = useState<string[]>([]);

  const categories = useMemo(() => getOperatorCategories(), []);
  const operatorsByCategory = useMemo(() => getOperatorsByCategory(), []);

  // Aplicar filtros em todos os operadores
  const filteredOperatorsByCategory = useMemo(() => {
    const result: Record<string, OperatorDefinition[]> = {};

    Object.entries(operatorsByCategory).forEach(([category, ops]) => {
      const filtered = ops.filter((op) => {
        // Filtro de busca
        if (filters.search) {
          const searchLower = filters.search.toLowerCase();
          const haystack = `${op.value} ${op.label} ${op.description || ""}`.toLowerCase();
          if (!haystack.includes(searchLower)) return false;
        }

        // Filtro de categoria
        if (filters.category !== "all" && op.category !== filters.category) {
          return false;
        }

        // Filtro requiresValue
        if (filters.requiresValue === "yes" && !op.requiresValue) return false;
        if (filters.requiresValue === "no" && op.requiresValue) return false;

        // Filtro Neo4j
        if (filters.isNeo4j && !isNeo4jOperator(op)) return false;

        // Filtro Velocity
        if (filters.isVelocity && !isVelocityOperator(op)) return false;

        // Filtro NULL behavior
        if (filters.nullBehavior !== "all") {
          const behavior = getNullBehavior(op.value);
          if (behavior !== filters.nullBehavior) return false;
        }

        return true;
      });

      if (filtered.length > 0) {
        result[category] = filtered;
      }
    });

    return result;
  }, [operatorsByCategory, filters]);

  // Contagem total de operadores filtrados
  const filteredCount = useMemo(() => {
    return Object.values(filteredOperatorsByCategory).reduce(
      (sum, ops) => sum + ops.length,
      0
    );
  }, [filteredOperatorsByCategory]);

  const handleFilterChange = <K extends keyof Filters>(key: K, value: Filters[K]) => {
    setFilters((prev) => ({ ...prev, [key]: value }));
  };

  const clearFilters = () => {
    setFilters(INITIAL_FILTERS);
  };

  const hasActiveFilters =
    filters.search ||
    filters.category !== "all" ||
    filters.requiresValue !== "all" ||
    filters.isNeo4j ||
    filters.isVelocity ||
    filters.nullBehavior !== "all";

  return (
    <div className="space-y-6">
      {/* Header com estatísticas */}
      <Card>
        <CardHeader className="pb-3">
          <div className="flex items-center justify-between">
            <div>
              <CardTitle className="flex items-center gap-2">
                📚 Catálogo de Operadores
                <Badge variant="secondary" className="text-lg">
                  {OPERATORS.length}
                </Badge>
              </CardTitle>
              <CardDescription>
                Todos os operadores disponíveis para construção de regras de fraude
              </CardDescription>
            </div>
            {hasActiveFilters && (
              <button
                onClick={clearFilters}
                className="text-sm text-muted-foreground hover:text-foreground underline"
              >
                Limpar filtros
              </button>
            )}
          </div>
        </CardHeader>
        <CardContent>
          {/* Filtros */}
          <div className="grid gap-4 md:grid-cols-2 lg:grid-cols-4">
            {/* Busca */}
            <div className="space-y-2">
              <Label htmlFor="search">Buscar</Label>
              <Input
                id="search"
                placeholder="Ex: CONTAINS, velocity, neo4j..."
                value={filters.search}
                onChange={(e) => handleFilterChange("search", e.target.value)}
              />
            </div>

            {/* Categoria */}
            <div className="space-y-2">
              <Label>Categoria</Label>
              <Select
                value={filters.category}
                onValueChange={(v) => handleFilterChange("category", v)}
              >
                <SelectTrigger>
                  <SelectValue placeholder="Todas" />
                </SelectTrigger>
                <SelectContent>
                  <SelectItem value="all">Todas ({categories.length})</SelectItem>
                  {categories.map((cat) => (
                    <SelectItem key={cat} value={cat}>
                      {cat} ({operatorsByCategory[cat]?.length || 0})
                    </SelectItem>
                  ))}
                </SelectContent>
              </Select>
            </div>

            {/* Requer Valor */}
            <div className="space-y-2">
              <Label>Requer Valor</Label>
              <Select
                value={filters.requiresValue}
                onValueChange={(v) =>
                  handleFilterChange("requiresValue", v as Filters["requiresValue"])
                }
              >
                <SelectTrigger>
                  <SelectValue placeholder="Todos" />
                </SelectTrigger>
                <SelectContent>
                  <SelectItem value="all">Todos</SelectItem>
                  <SelectItem value="yes">Sim (requer valor)</SelectItem>
                  <SelectItem value="no">Não (unário)</SelectItem>
                </SelectContent>
              </Select>
            </div>

            {/* NULL Behavior */}
            <div className="space-y-2">
              <Label>Comportamento NULL</Label>
              <Select
                value={filters.nullBehavior}
                onValueChange={(v) =>
                  handleFilterChange("nullBehavior", v as NullBehaviorFilter)
                }
              >
                <SelectTrigger>
                  <SelectValue placeholder="Todos" />
                </SelectTrigger>
                <SelectContent>
                  <SelectItem value="all">Todos</SelectItem>
                  <SelectItem value="returns_false">Retorna FALSE</SelectItem>
                  <SelectItem value="returns_true">Retorna TRUE</SelectItem>
                  <SelectItem value="checks_null">Verifica NULL</SelectItem>
                  <SelectItem value="context_dependent">Depende do contexto</SelectItem>
                  <SelectItem value="not_applicable">N/A</SelectItem>
                </SelectContent>
              </Select>
            </div>
          </div>

          {/* Checkboxes para Neo4j e Velocity */}
          <div className="flex gap-6 mt-4">
            <div className="flex items-center space-x-2">
              <Checkbox
                id="neo4j"
                checked={filters.isNeo4j}
                onCheckedChange={(checked) =>
                  handleFilterChange("isNeo4j", checked === true)
                }
              />
              <Label htmlFor="neo4j" className="cursor-pointer">
                Apenas Neo4j Graph
              </Label>
            </div>
            <div className="flex items-center space-x-2">
              <Checkbox
                id="velocity"
                checked={filters.isVelocity}
                onCheckedChange={(checked) =>
                  handleFilterChange("isVelocity", checked === true)
                }
              />
              <Label htmlFor="velocity" className="cursor-pointer">
                Apenas Velocity
              </Label>
            </div>
          </div>

          {/* Resultado dos filtros */}
          <div className="mt-4 text-sm text-muted-foreground">
            Exibindo{" "}
            <span className="font-semibold text-foreground">{filteredCount}</span> de{" "}
            <span className="font-semibold text-foreground">{OPERATORS.length}</span>{" "}
            operadores em{" "}
            <span className="font-semibold text-foreground">
              {Object.keys(filteredOperatorsByCategory).length}
            </span>{" "}
            categorias
          </div>
        </CardContent>
      </Card>

      {/* Accordion por categoria */}
      <TooltipProvider>
        <Accordion
          type="multiple"
          value={openCategories}
          onValueChange={setOpenCategories}
          className="space-y-2"
        >
          {Object.entries(filteredOperatorsByCategory).map(([category, operators]) => {
            const explanation = OPERATOR_CATEGORY_EXPLANATIONS[category];

            return (
              <AccordionItem
                key={category}
                value={category}
                className="border rounded-lg px-4"
              >
                <AccordionTrigger className="hover:no-underline">
                  <div className="flex items-center gap-3">
                    <span className="text-2xl">{explanation?.icone || "📁"}</span>
                    <span className="font-semibold">{category}</span>
                    <Badge variant="outline">{operators.length}</Badge>
                  </div>
                </AccordionTrigger>
                <AccordionContent>
                  {/* Explicação didática */}
                  {explanation && (
                    <Card className="mb-4 bg-muted/50">
                      <CardContent className="pt-4">
                        <div className="grid gap-3 md:grid-cols-2">
                          <div>
                            <p className="font-medium text-sm text-muted-foreground">
                              O que faz?
                            </p>
                            <p className="text-sm">{explanation.oQueFaz}</p>
                          </div>
                          <div>
                            <p className="font-medium text-sm text-muted-foreground">
                              Por que é importante?
                            </p>
                            <p className="text-sm">{explanation.porQueImportante}</p>
                          </div>
                          <div>
                            <p className="font-medium text-sm text-muted-foreground">
                              Exemplo real
                            </p>
                            <p className="text-sm">{explanation.exemploReal}</p>
                          </div>
                          <div>
                            <p className="font-medium text-sm text-muted-foreground">
                              💡 Analogia
                            </p>
                            <p className="text-sm italic">{explanation.analogia}</p>
                          </div>
                        </div>
                      </CardContent>
                    </Card>
                  )}

                  {/* Tabela de operadores */}
                  <div className="rounded-md border overflow-auto">
                    <Table>
                      <TableHeader>
                        <TableRow>
                          <TableHead className="w-[200px]">Código</TableHead>
                          <TableHead>Label</TableHead>
                          <TableHead>Descrição</TableHead>
                          <TableHead className="w-[180px] text-right">Flags</TableHead>
                        </TableRow>
                      </TableHeader>
                      <TableBody>
                        {operators.map((op) => {
                          const nullBehavior = getNullBehavior(op.value);
                          const isNeo4j = isNeo4jOperator(op);
                          const isVelocity = isVelocityOperator(op);

                          return (
                            <TableRow key={op.value}>
                              <TableCell className="font-mono text-sm">
                                {op.value}
                              </TableCell>
                              <TableCell>{op.label}</TableCell>
                              <TableCell className="text-sm text-muted-foreground">
                                {op.description || "-"}
                              </TableCell>
                              <TableCell className="text-right">
                                <div className="flex flex-wrap gap-1 justify-end">
                                  {op.requiresValue && (
                                    <Tooltip>
                                      <TooltipTrigger>
                                        <Badge variant="outline" className="text-xs">
                                          valor
                                        </Badge>
                                      </TooltipTrigger>
                                      <TooltipContent>
                                        Requer um valor para comparação
                                      </TooltipContent>
                                    </Tooltip>
                                  )}
                                  {!op.requiresValue && (
                                    <Tooltip>
                                      <TooltipTrigger>
                                        <Badge variant="secondary" className="text-xs">
                                          unário
                                        </Badge>
                                      </TooltipTrigger>
                                      <TooltipContent>
                                        Não requer valor (operador unário)
                                      </TooltipContent>
                                    </Tooltip>
                                  )}
                                  {isNeo4j && (
                                    <Tooltip>
                                      <TooltipTrigger>
                                        <Badge className="text-xs bg-purple-600">
                                          Neo4j
                                        </Badge>
                                      </TooltipTrigger>
                                      <TooltipContent>
                                        Operador de banco de dados de grafos Neo4j
                                      </TooltipContent>
                                    </Tooltip>
                                  )}
                                  {isVelocity && !isNeo4j && (
                                    <Tooltip>
                                      <TooltipTrigger>
                                        <Badge className="text-xs bg-orange-600">
                                          Velocity
                                        </Badge>
                                      </TooltipTrigger>
                                      <TooltipContent>
                                        Operador de contagem/agregação temporal
                                      </TooltipContent>
                                    </Tooltip>
                                  )}
                                  <Tooltip>
                                    <TooltipTrigger>
                                      <Badge
                                        className={`text-xs ${NULL_BEHAVIOR_COLORS[nullBehavior]}`}
                                      >
                                        {nullBehavior === "returns_false" && "NULL→F"}
                                        {nullBehavior === "returns_true" && "NULL→T"}
                                        {nullBehavior === "checks_null" && "chk NULL"}
                                        {nullBehavior === "context_dependent" && "ctx"}
                                        {nullBehavior === "not_applicable" && "n/a"}
                                      </Badge>
                                    </TooltipTrigger>
                                    <TooltipContent className="max-w-xs">
                                      {NULL_BEHAVIOR_LABELS[nullBehavior]}
                                    </TooltipContent>
                                  </Tooltip>
                                </div>
                              </TableCell>
                            </TableRow>
                          );
                        })}
                      </TableBody>
                    </Table>
                  </div>
                </AccordionContent>
              </AccordionItem>
            );
          })}
        </Accordion>
      </TooltipProvider>

      {/* Mensagem quando nenhum resultado */}
      {filteredCount === 0 && (
        <Card className="py-8">
          <CardContent className="flex flex-col items-center justify-center text-center">
            <span className="text-4xl mb-4">🔍</span>
            <p className="text-lg font-medium">Nenhum operador encontrado</p>
            <p className="text-muted-foreground">
              Tente ajustar os filtros ou termos de busca
            </p>
            <button
              onClick={clearFilters}
              className="mt-4 text-sm text-primary hover:underline"
            >
              Limpar todos os filtros
            </button>
          </CardContent>
        </Card>
      )}
    </div>
  );
}

export default OperatorCatalog;
