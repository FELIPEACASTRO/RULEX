/**
 * FunctionsCatalog.tsx - Catálogo de Funções de Expressão do RULEX
 *
 * Exibe todas as funções disponíveis para uso em expressões:
 * - Funções matemáticas (ABS, ROUND, etc.)
 * - Funções de string (TRIM, UPPER, etc.)
 * - Funções de data (NOW, DAYS_BETWEEN, etc.)
 * - Funções condicionais (IF, COALESCE, etc.)
 *
 * Dados gerados de: ExpressionEvaluator.java e AstValidator.java
 */
import { useState, useMemo } from "react";
import { Search, FunctionSquare, Calculator, Type, Calendar, GitBranch } from "lucide-react";
import { Badge } from "@/components/ui/badge";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Input } from "@/components/ui/input";
import {
  Accordion,
  AccordionContent,
  AccordionItem,
  AccordionTrigger,
} from "@/components/ui/accordion";
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "@/components/ui/table";

import { EXPRESSION_FUNCTIONS, AST_FUNC_ALLOWLIST } from "./generated";

const CATEGORY_ICONS: Record<string, React.ReactNode> = {
  "Matemáticas": <Calculator className="h-4 w-4" />,
  "Strings": <Type className="h-4 w-4" />,
  "Data/Hora": <Calendar className="h-4 w-4" />,
  "Condicionais": <GitBranch className="h-4 w-4" />,
  "Agregação": <FunctionSquare className="h-4 w-4" />,
  "Geral": <FunctionSquare className="h-4 w-4" />
};

const CATEGORY_COLORS: Record<string, string> = {
  "Matemáticas": "bg-blue-100 text-blue-800 dark:bg-blue-900 dark:text-blue-200",
  "Strings": "bg-green-100 text-green-800 dark:bg-green-900 dark:text-green-200",
  "Data/Hora": "bg-purple-100 text-purple-800 dark:bg-purple-900 dark:text-purple-200",
  "Condicionais": "bg-orange-100 text-orange-800 dark:bg-orange-900 dark:text-orange-200",
  "Agregação": "bg-pink-100 text-pink-800 dark:bg-pink-900 dark:text-pink-200",
  "Geral": "bg-gray-100 text-gray-800 dark:bg-gray-800 dark:text-gray-200"
};

export function FunctionsCatalog() {
  const [searchQuery, setSearchQuery] = useState("");
  const [selectedCategory, setSelectedCategory] = useState<string | null>(null);

  // Agrupar por categoria
  const groupedFunctions = useMemo(() => {
    const groups: Record<string, typeof EXPRESSION_FUNCTIONS> = {};
    EXPRESSION_FUNCTIONS.forEach((fn) => {
      const cat = fn.category || "Geral";
      if (!groups[cat]) groups[cat] = [];
      groups[cat].push(fn);
    });
    return groups;
  }, []);

  const categories = Object.keys(groupedFunctions);

  // Filtrar
  const filteredFunctions = useMemo(() => {
    return EXPRESSION_FUNCTIONS.filter((fn) => {
      if (selectedCategory && fn.category !== selectedCategory) return false;
      if (!searchQuery.trim()) return true;
      const q = searchQuery.toLowerCase();
      return (
        fn.name.toLowerCase().includes(q) ||
        fn.alias?.toLowerCase().includes(q) ||
        fn.description?.toLowerCase().includes(q) ||
        fn.example?.toLowerCase().includes(q)
      );
    });
  }, [searchQuery, selectedCategory]);

  // Verificar se função está na allowlist do AST
  const isInAllowlist = (name: string) => AST_FUNC_ALLOWLIST.includes(name);

  return (
    <div className="space-y-6">
      {/* Header */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <FunctionSquare className="h-5 w-5" />
            Catálogo de Funções de Expressão
          </CardTitle>
          <CardDescription>
            {EXPRESSION_FUNCTIONS.length} funções disponíveis para uso em expressões e fórmulas
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-4">
          {/* Busca */}
          <div className="relative max-w-md">
            <Search className="absolute left-3 top-1/2 -translate-y-1/2 h-4 w-4 text-muted-foreground" />
            <Input
              placeholder="Buscar funções..."
              className="pl-10"
              value={searchQuery}
              onChange={(e) => setSearchQuery(e.target.value)}
            />
          </div>

          {/* Filtros por categoria */}
          <div className="flex flex-wrap gap-2">
            <Badge
              variant={selectedCategory === null ? "default" : "outline"}
              className="cursor-pointer"
              onClick={() => setSelectedCategory(null)}
            >
              Todas ({EXPRESSION_FUNCTIONS.length})
            </Badge>
            {categories.map((cat) => (
              <Badge
                key={cat}
                variant={selectedCategory === cat ? "default" : "outline"}
                className={`cursor-pointer ${selectedCategory === cat ? "" : CATEGORY_COLORS[cat]}`}
                onClick={() => setSelectedCategory(cat === selectedCategory ? null : cat)}
              >
                {CATEGORY_ICONS[cat]} {cat} ({groupedFunctions[cat].length})
              </Badge>
            ))}
          </div>
        </CardContent>
      </Card>

      {/* AST Allowlist Info */}
      <Card className="border-blue-500 bg-blue-50 dark:bg-blue-950">
        <CardContent className="pt-4">
          <div className="flex items-start gap-3">
            <FunctionSquare className="h-5 w-5 text-blue-600 flex-shrink-0 mt-0.5" />
            <div>
              <p className="font-semibold text-blue-800 dark:text-blue-200">
                Allowlist do Validador AST (V31)
              </p>
              <p className="text-sm text-blue-700 dark:text-blue-300 mt-1">
                Apenas {AST_FUNC_ALLOWLIST.length} funções são permitidas no validador AST V31:
                <code className="ml-2 bg-blue-100 dark:bg-blue-800 px-1 rounded">
                  {AST_FUNC_ALLOWLIST.join(", ")}
                </code>
              </p>
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Lista de funções por categoria */}
      {selectedCategory === null ? (
        // Exibir agrupado por categoria
        <div className="space-y-6">
          {categories.map((cat) => (
            <Card key={cat}>
              <CardHeader>
                <CardTitle className="flex items-center gap-2">
                  {CATEGORY_ICONS[cat]}
                  Funções {cat}
                </CardTitle>
                <CardDescription>
                  {groupedFunctions[cat].length} funções nesta categoria
                </CardDescription>
              </CardHeader>
              <CardContent>
                <div className="rounded-md border">
                  <Table>
                    <TableHeader>
                      <TableRow>
                        <TableHead>Função</TableHead>
                        <TableHead>Alias</TableHead>
                        <TableHead>Descrição</TableHead>
                        <TableHead>Exemplo</TableHead>
                        <TableHead>Retorno</TableHead>
                      </TableRow>
                    </TableHeader>
                    <TableBody>
                      {groupedFunctions[cat].map((fn) => (
                        <TableRow key={fn.name} id={`manual-function-${fn.name}`}>
                          <TableCell>
                            <code className="font-mono font-semibold">{fn.name}</code>
                            {isInAllowlist(fn.name) && (
                              <Badge variant="outline" className="ml-2 text-xs">
                                AST ✓
                              </Badge>
                            )}
                          </TableCell>
                          <TableCell className="text-muted-foreground">
                            {fn.alias || "-"}
                          </TableCell>
                          <TableCell className="text-sm">{fn.description}</TableCell>
                          <TableCell>
                            <code className="text-xs bg-muted px-1 rounded">{fn.example}</code>
                          </TableCell>
                          <TableCell>
                            <Badge variant="secondary">{fn.returnType}</Badge>
                          </TableCell>
                        </TableRow>
                      ))}
                    </TableBody>
                  </Table>
                </div>
              </CardContent>
            </Card>
          ))}
        </div>
      ) : (
        // Exibir apenas categoria selecionada
        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2">
              {CATEGORY_ICONS[selectedCategory]}
              Funções {selectedCategory}
            </CardTitle>
          </CardHeader>
          <CardContent>
            <Accordion type="multiple" className="w-full">
              {filteredFunctions.map((fn) => (
                <AccordionItem key={fn.name} value={fn.name} id={`manual-function-${fn.name}`}>
                  <AccordionTrigger className="hover:no-underline">
                    <div className="flex items-center gap-3 text-left">
                      <code className="font-mono font-semibold">{fn.name}</code>
                      {fn.alias && (
                        <span className="text-sm text-muted-foreground">
                          (alias: {fn.alias})
                        </span>
                      )}
                      {isInAllowlist(fn.name) && (
                        <Badge variant="outline" className="text-xs">AST ✓</Badge>
                      )}
                    </div>
                  </AccordionTrigger>
                  <AccordionContent>
                    <div className="space-y-3 pt-2">
                      <div>
                        <h4 className="text-sm font-medium">Descrição</h4>
                        <p className="text-sm text-muted-foreground">{fn.description}</p>
                      </div>
                      <div>
                        <h4 className="text-sm font-medium">Exemplo</h4>
                        <pre className="bg-muted rounded p-2 text-sm">{fn.example}</pre>
                      </div>
                      <div>
                        <h4 className="text-sm font-medium">Tipo de Retorno</h4>
                        <Badge variant="secondary">{fn.returnType}</Badge>
                      </div>
                    </div>
                  </AccordionContent>
                </AccordionItem>
              ))}
            </Accordion>
          </CardContent>
        </Card>
      )}

      {/* Exemplos práticos */}
      <Card>
        <CardHeader>
          <CardTitle>Exemplos de Uso em Regras</CardTitle>
          <CardDescription>
            Como combinar funções para criar condições poderosas
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="grid gap-4 md:grid-cols-2">
            <div className="bg-muted rounded-lg p-4">
              <h4 className="font-semibold mb-2">Verificar valor arredondado</h4>
              <pre className="text-sm bg-background rounded p-2">
{`ROUND(transactionAmount, 2) > 1000`}
              </pre>
              <p className="text-sm text-muted-foreground mt-2">
                Arredonda o valor para 2 casas e compara
              </p>
            </div>

            <div className="bg-muted rounded-lg p-4">
              <h4 className="font-semibold mb-2">Verificar dias desde última atividade</h4>
              <pre className="text-sm bg-background rounded p-2">
{`DAYS_BETWEEN(lastActivityDate, TODAY()) > 90`}
              </pre>
              <p className="text-sm text-muted-foreground mt-2">
                Detecta contas inativas há mais de 90 dias
              </p>
            </div>

            <div className="bg-muted rounded-lg p-4">
              <h4 className="font-semibold mb-2">Valor com fallback</h4>
              <pre className="text-sm bg-background rounded p-2">
{`COALESCE(customLimit, 5000) < transactionAmount`}
              </pre>
              <p className="text-sm text-muted-foreground mt-2">
                Usa limite customizado ou 5000 como padrão
              </p>
            </div>

            <div className="bg-muted rounded-lg p-4">
              <h4 className="font-semibold mb-2">Condicional em expressão</h4>
              <pre className="text-sm bg-background rounded p-2">
{`IF(customerType == "VIP", 
  transactionAmount > 50000, 
  transactionAmount > 10000)`}
              </pre>
              <p className="text-sm text-muted-foreground mt-2">
                Limites diferentes por tipo de cliente
              </p>
            </div>
          </div>
        </CardContent>
      </Card>
    </div>
  );
}
