import { useMemo, useState } from "react";

import { MANUAL_DATA } from "@/manual/manualData";

import { Badge } from "@/components/ui/badge";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Input } from "@/components/ui/input";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "@/components/ui/table";

export default function Manual() {
  const [searchSimple, setSearchSimple] = useState("");
  const [searchComplex, setSearchComplex] = useState("");

  const simple = MANUAL_DATA.generatedFrom.ruleFormDialog;
  const complex = MANUAL_DATA.generatedFrom.complexRuleBuilder;

  const filteredSimpleOperators = useMemo(() => {
    const q = searchSimple.trim().toLowerCase();
    if (!q) return simple.operators;

    return simple.operators.filter((op) => {
      const hay = `${op.value} ${op.label} ${op.description ?? ""}`.toLowerCase();
      return hay.includes(q);
    });
  }, [searchSimple, simple.operators]);

  const filteredComplexOperators = useMemo(() => {
    const q = searchComplex.trim().toLowerCase();
    if (!q) return complex.comparisonOperators;

    return complex.comparisonOperators.filter((op) => {
      const hay = `${op.value} ${op.label} ${op.description}`.toLowerCase();
      return hay.includes(q);
    });
  }, [searchComplex, complex.comparisonOperators]);

  return (
    <div className="space-y-6">
      <div>
        <h1 className="text-3xl font-bold tracking-tight">Manual</h1>
        <p className="text-muted-foreground">
          Documentacao gerada a partir do codigo do frontend (sem conteudo inventado).
        </p>
      </div>

      <Tabs defaultValue="regras-complexas" className="space-y-4">
        <TabsList>
          <TabsTrigger value="regras-complexas">Regras complexas</TabsTrigger>
          <TabsTrigger value="regras-form">Regras (formulario)</TabsTrigger>
        </TabsList>

        <TabsContent value="regras-complexas" className="space-y-4">
          <div className="grid gap-4 md:grid-cols-3">
            <Card>
              <CardHeader className="pb-2">
                <CardTitle className="text-sm font-medium">Operadores logicos</CardTitle>
                <CardDescription>ComplexRuleBuilder</CardDescription>
              </CardHeader>
              <CardContent>
                <div className="text-2xl font-bold">{complex.logicOperators.length}</div>
              </CardContent>
            </Card>

            <Card>
              <CardHeader className="pb-2">
                <CardTitle className="text-sm font-medium">Operadores de comparacao</CardTitle>
                <CardDescription>ComplexRuleBuilder</CardDescription>
              </CardHeader>
              <CardContent>
                <div className="text-2xl font-bold">{complex.comparisonOperators.length}</div>
              </CardContent>
            </Card>

            <Card>
              <CardHeader className="pb-2">
                <CardTitle className="text-sm font-medium">Tipos de valor</CardTitle>
                <CardDescription>ComplexRuleBuilder</CardDescription>
              </CardHeader>
              <CardContent>
                <div className="text-2xl font-bold">{complex.valueTypes.length}</div>
              </CardContent>
            </Card>
          </div>

          <Card>
            <CardHeader>
              <CardTitle>Operadores de comparacao</CardTitle>
              <CardDescription>
                Lista carregada de <span className="font-mono">ComplexRuleBuilder/types.ts</span>.
              </CardDescription>
            </CardHeader>
            <CardContent className="space-y-3">
              <div className="flex flex-col gap-2 md:flex-row md:items-center md:justify-between">
                <Input
                  value={searchComplex}
                  onChange={(e) => setSearchComplex(e.target.value)}
                  placeholder="Buscar por codigo, label ou descricao..."
                />
                <div className="text-sm text-muted-foreground shrink-0">
                  {filteredComplexOperators.length} de {complex.comparisonOperators.length}
                </div>
              </div>

              <div className="rounded-md border overflow-auto">
                <Table>
                  <TableHeader>
                    <TableRow>
                      <TableHead>Codigo</TableHead>
                      <TableHead>Label</TableHead>
                      <TableHead>Descricao</TableHead>
                      <TableHead className="text-right">Tipo</TableHead>
                    </TableRow>
                  </TableHeader>
                  <TableBody>
                    {filteredComplexOperators.map((op) => (
                      <TableRow key={op.value}>
                        <TableCell className="font-mono">{op.value}</TableCell>
                        <TableCell>{op.label}</TableCell>
                        <TableCell>{op.description}</TableCell>
                        <TableCell className="text-right">
                          <Badge variant="secondary">{op.category}</Badge>
                        </TableCell>
                      </TableRow>
                    ))}
                  </TableBody>
                </Table>
              </div>
            </CardContent>
          </Card>

          <Card>
            <CardHeader>
              <CardTitle>Operadores logicos</CardTitle>
              <CardDescription>
                Lista carregada de <span className="font-mono">ComplexRuleBuilder/types.ts</span>.
              </CardDescription>
            </CardHeader>
            <CardContent>
              <div className="rounded-md border overflow-auto">
                <Table>
                  <TableHeader>
                    <TableRow>
                      <TableHead>Codigo</TableHead>
                      <TableHead>Label</TableHead>
                      <TableHead>Descricao</TableHead>
                    </TableRow>
                  </TableHeader>
                  <TableBody>
                    {complex.logicOperators.map((op) => (
                      <TableRow key={op.value}>
                        <TableCell className="font-mono">{op.value}</TableCell>
                        <TableCell>{op.label}</TableCell>
                        <TableCell>{op.description}</TableCell>
                      </TableRow>
                    ))}
                  </TableBody>
                </Table>
              </div>
            </CardContent>
          </Card>

          <Card>
            <CardHeader>
              <CardTitle>Tipos de valor</CardTitle>
              <CardDescription>
                Lista carregada de <span className="font-mono">ComplexRuleBuilder/types.ts</span>.
              </CardDescription>
            </CardHeader>
            <CardContent>
              <div className="rounded-md border overflow-auto">
                <Table>
                  <TableHeader>
                    <TableRow>
                      <TableHead>Codigo</TableHead>
                      <TableHead>Label</TableHead>
                    </TableRow>
                  </TableHeader>
                  <TableBody>
                    {complex.valueTypes.map((t) => (
                      <TableRow key={t.value}>
                        <TableCell className="font-mono">{t.value}</TableCell>
                        <TableCell>{t.label}</TableCell>
                      </TableRow>
                    ))}
                  </TableBody>
                </Table>
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        <TabsContent value="regras-form" className="space-y-4">
          <div className="grid gap-4 md:grid-cols-4">
            <Card>
              <CardHeader className="pb-2">
                <CardTitle className="text-sm font-medium">Tipos de regra</CardTitle>
                <CardDescription>RuleFormDialog</CardDescription>
              </CardHeader>
              <CardContent>
                <div className="text-2xl font-bold">{simple.ruleTypes.length}</div>
              </CardContent>
            </Card>

            <Card>
              <CardHeader className="pb-2">
                <CardTitle className="text-sm font-medium">Classificacoes</CardTitle>
                <CardDescription>RuleFormDialog</CardDescription>
              </CardHeader>
              <CardContent>
                <div className="text-2xl font-bold">{simple.classifications.length}</div>
              </CardContent>
            </Card>

            <Card>
              <CardHeader className="pb-2">
                <CardTitle className="text-sm font-medium">Operadores logicos</CardTitle>
                <CardDescription>RuleFormDialog</CardDescription>
              </CardHeader>
              <CardContent>
                <div className="text-2xl font-bold">{simple.logicOperators.length}</div>
              </CardContent>
            </Card>

            <Card>
              <CardHeader className="pb-2">
                <CardTitle className="text-sm font-medium">Operadores</CardTitle>
                <CardDescription>RuleFormDialog</CardDescription>
              </CardHeader>
              <CardContent>
                <div className="text-2xl font-bold">{simple.operators.length}</div>
              </CardContent>
            </Card>
          </div>

          <Card>
            <CardHeader>
              <CardTitle>Operadores</CardTitle>
              <CardDescription>
                Lista carregada de <span className="font-mono">RuleFormDialog/types.ts</span>.
              </CardDescription>
            </CardHeader>
            <CardContent className="space-y-3">
              <div className="flex flex-col gap-2 md:flex-row md:items-center md:justify-between">
                <Input
                  value={searchSimple}
                  onChange={(e) => setSearchSimple(e.target.value)}
                  placeholder="Buscar por codigo, label ou descricao..."
                />
                <div className="text-sm text-muted-foreground shrink-0">
                  {filteredSimpleOperators.length} de {simple.operators.length}
                </div>
              </div>

              <div className="rounded-md border overflow-auto">
                <Table>
                  <TableHeader>
                    <TableRow>
                      <TableHead>Codigo</TableHead>
                      <TableHead>Label</TableHead>
                      <TableHead>Descricao</TableHead>
                      <TableHead className="text-right">Flags</TableHead>
                    </TableRow>
                  </TableHeader>
                  <TableBody>
                    {filteredSimpleOperators.map((op) => {
                      const flags: string[] = [];
                      if (simple.unaryOperators.includes(op.value)) flags.push("unario");
                      if (simple.fieldRefOperators.includes(op.value)) flags.push("ref-campo");
                      if ((op.label ?? "").toLowerCase().includes("legacy")) flags.push("legacy");

                      return (
                        <TableRow key={op.value}>
                          <TableCell className="font-mono">{op.value}</TableCell>
                          <TableCell>{op.label}</TableCell>
                          <TableCell>{op.description ?? ""}</TableCell>
                          <TableCell className="text-right space-x-1">
                            {flags.length ? (
                              flags.map((f) => (
                                <Badge key={f} variant="secondary">
                                  {f}
                                </Badge>
                              ))
                            ) : (
                              <span className="text-muted-foreground text-sm">-</span>
                            )}
                          </TableCell>
                        </TableRow>
                      );
                    })}
                  </TableBody>
                </Table>
              </div>
            </CardContent>
          </Card>

          <Card>
            <CardHeader>
              <CardTitle>Operadores por tipo</CardTitle>
              <CardDescription>
                Mapa carregado de <span className="font-mono">RuleFormDialog/types.ts</span>.
              </CardDescription>
            </CardHeader>
            <CardContent>
              <div className="rounded-md border overflow-auto">
                <Table>
                  <TableHeader>
                    <TableRow>
                      <TableHead>Tipo</TableHead>
                      <TableHead className="text-right">Quantidade</TableHead>
                    </TableRow>
                  </TableHeader>
                  <TableBody>
                    {Object.entries(simple.operatorsByType).map(([typeKey, ops]) => (
                      <TableRow key={typeKey}>
                        <TableCell className="font-mono">{typeKey}</TableCell>
                        <TableCell className="text-right">{ops.length}</TableCell>
                      </TableRow>
                    ))}
                  </TableBody>
                </Table>
              </div>
            </CardContent>
          </Card>
        </TabsContent>
      </Tabs>
    </div>
  );
}
