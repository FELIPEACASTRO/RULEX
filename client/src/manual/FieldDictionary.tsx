/**
 * FieldDictionary.tsx - Dicionário de TODOS os 102 campos do payload do RULEX
 *
 * Features:
 * - Accordion por categoria de campo
 * - Busca por nome do campo ou label
 * - Labels em português brasileiro
 */
import { useEffect, useMemo, useState } from "react";

import {
  Accordion,
  AccordionContent,
  AccordionItem,
  AccordionTrigger,
} from "@/components/ui/accordion";
import { Badge } from "@/components/ui/badge";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Input } from "@/components/ui/input";
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "@/components/ui/table";

import { FIELD_LABELS, FIELD_CATEGORIES, getFieldLabel } from "./manualData";

// Ícones por categoria
const CATEGORY_ICONS: Record<string, string> = {
  transaction: "💳",
  customer: "👤",
  card: "💳",
  token: "🔑",
  merchant: "🏪",
  terminal: "🖥️",
  authentication: "🔐",
  cvv: "🔢",
  avs: "📍",
  pin: "🔒",
  security: "🛡️",
  emv: "📟",
  acquirer: "🏦",
  network: "🌐",
  scores: "📊",
  workflow: "⚙️",
  credit: "💰",
  user: "📝",
  indicators: "🚦",
  other: "📁",
};

export interface FieldDictionaryProps {
  highlightField?: string;
}

export function FieldDictionary({ highlightField }: FieldDictionaryProps) {
  const [search, setSearch] = useState("");
  const [openCategories, setOpenCategories] = useState<string[]>([]);

  useEffect(() => {
    if (!highlightField) return;
    setSearch(highlightField);
    const category = FIELD_CATEGORIES.find((cat) => cat.fields.includes(highlightField));
    if (category) {
      setOpenCategories((prev) => (prev.includes(category.id) ? prev : [category.id, ...prev]));
    }
  }, [highlightField]);

  // Contar total de campos definidos
  const totalFields = Object.keys(FIELD_LABELS).length;

  // Filtrar campos por busca
  const filteredCategories = useMemo(() => {
    if (!search.trim()) return FIELD_CATEGORIES;

    const searchLower = search.toLowerCase();
    return FIELD_CATEGORIES.map((cat) => ({
      ...cat,
      fields: cat.fields.filter((field) => {
        const label = getFieldLabel(field);
        return (
          field.toLowerCase().includes(searchLower) ||
          label.toLowerCase().includes(searchLower)
        );
      }),
    })).filter((cat) => cat.fields.length > 0);
  }, [search]);

  // Contar campos filtrados
  const filteredCount = useMemo(() => {
    return filteredCategories.reduce((sum, cat) => sum + cat.fields.length, 0);
  }, [filteredCategories]);

  return (
    <div className="space-y-6">
      {/* Header com estatísticas */}
      <Card>
        <CardHeader className="pb-3">
          <div className="flex items-center justify-between">
            <div>
              <CardTitle className="flex items-center gap-2">
                📋 Dicionário de Campos do Payload
                <Badge variant="secondary" className="text-lg">
                  {totalFields}
                </Badge>
              </CardTitle>
              <CardDescription>
                Todos os campos disponíveis para uso em condições de regras
              </CardDescription>
            </div>
          </div>
        </CardHeader>
        <CardContent>
          {/* Busca */}
          <div className="space-y-2">
            <Input
              placeholder="Buscar por nome do campo ou label..."
              value={search}
              onChange={(e) => setSearch(e.target.value)}
            />
            <div className="text-sm text-muted-foreground">
              Exibindo{" "}
              <span className="font-semibold text-foreground">{filteredCount}</span> de{" "}
              <span className="font-semibold text-foreground">{totalFields}</span>{" "}
              campos em{" "}
              <span className="font-semibold text-foreground">
                {filteredCategories.length}
              </span>{" "}
              categorias
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Accordion por categoria */}
      <Accordion
        type="multiple"
        value={openCategories}
        onValueChange={setOpenCategories}
        className="space-y-2"
      >
        {filteredCategories.map((category) => {
          const icon = CATEGORY_ICONS[category.id] || "📁";

          return (
            <AccordionItem
              key={category.id}
              value={category.id}
              className="border rounded-lg px-4"
            >
              <AccordionTrigger className="hover:no-underline">
                <div className="flex items-center gap-3">
                  <span className="text-2xl">{icon}</span>
                  <span className="font-semibold">{category.label}</span>
                  <Badge variant="outline">{category.fields.length}</Badge>
                </div>
              </AccordionTrigger>
              <AccordionContent>
                <div className="rounded-md border overflow-auto">
                  <Table>
                    <TableHeader>
                      <TableRow>
                        <TableHead className="w-[300px]">Nome do Campo (API)</TableHead>
                        <TableHead>Label (PT-BR)</TableHead>
                        <TableHead className="w-[150px] text-right">Tipo</TableHead>
                      </TableRow>
                    </TableHeader>
                    <TableBody>
                      {category.fields.map((field) => {
                        const label = getFieldLabel(field);
                        // Inferir tipo pelo nome do campo
                        let type = "string";
                        if (
                          field.toLowerCase().includes("amount") ||
                          field.toLowerCase().includes("score") ||
                          field.toLowerCase().includes("count") ||
                          field.toLowerCase().includes("limit") ||
                          field.toLowerCase().includes("atc") ||
                          field.toLowerCase().includes("installments")
                        ) {
                          type = "number";
                        } else if (
                          field.toLowerCase().includes("date") ||
                          field.toLowerCase().includes("time")
                        ) {
                          type = "date/time";
                        } else if (
                          field.toLowerCase().includes("present") ||
                          field.toLowerCase().includes("indicator") ||
                          field.toLowerCase().includes("recurring")
                        ) {
                          type = "boolean";
                        } else if (
                          field.toLowerCase().includes("latitude") ||
                          field.toLowerCase().includes("longitude")
                        ) {
                          type = "geo";
                        }

                        const isHighlighted = highlightField === field;

                        return (
                          <TableRow
                            key={field}
                            id={`manual-field-${field}`}
                            className={
                              isHighlighted
                                ? "bg-primary/10 ring-2 ring-primary ring-inset"
                                : undefined
                            }
                          >
                            <TableCell className="font-mono text-sm">{field}</TableCell>
                            <TableCell>{label}</TableCell>
                            <TableCell className="text-right">
                              <Badge variant="outline" className="text-xs">
                                {type}
                              </Badge>
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

      {/* Mensagem quando nenhum resultado */}
      {filteredCount === 0 && (
        <Card className="py-8">
          <CardContent className="flex flex-col items-center justify-center text-center">
            <span className="text-4xl mb-4">🔍</span>
            <p className="text-lg font-medium">Nenhum campo encontrado</p>
            <p className="text-muted-foreground">
              Tente ajustar o termo de busca
            </p>
            <button
              onClick={() => setSearch("")}
              className="mt-4 text-sm text-primary hover:underline"
            >
              Limpar busca
            </button>
          </CardContent>
        </Card>
      )}

      {/* Legenda de tipos */}
      <Card>
        <CardHeader className="pb-2">
          <CardTitle className="text-sm">Legenda de Tipos</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="flex flex-wrap gap-4 text-sm">
            <div className="flex items-center gap-2">
              <Badge variant="outline">string</Badge>
              <span className="text-muted-foreground">Texto</span>
            </div>
            <div className="flex items-center gap-2">
              <Badge variant="outline">number</Badge>
              <span className="text-muted-foreground">Número</span>
            </div>
            <div className="flex items-center gap-2">
              <Badge variant="outline">date/time</Badge>
              <span className="text-muted-foreground">Data ou Hora</span>
            </div>
            <div className="flex items-center gap-2">
              <Badge variant="outline">boolean</Badge>
              <span className="text-muted-foreground">Verdadeiro/Falso</span>
            </div>
            <div className="flex items-center gap-2">
              <Badge variant="outline">geo</Badge>
              <span className="text-muted-foreground">Coordenada Geográfica</span>
            </div>
          </div>
        </CardContent>
      </Card>
    </div>
  );
}

export default FieldDictionary;
